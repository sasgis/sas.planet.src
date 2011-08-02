unit u_TileDownloaderBaseThread;

interface

uses
  Windows,
  SysUtils,
  Classes,
  SyncObjs,
  ALHTTPCommon,
  ALHttpClient,
  ALWinInetHttpClient,
  i_JclNotify,
  i_InetConfig,
  i_ProxySettings,
  i_TileRequestBuilder,
  i_TileDownloader,
  i_TileDownloaderConfig;

type
  TTileDownloaderBaseThread = class(TThread)
  private
    FCancelListener: IJclListener;
    FCancelEvent: TEvent;
    FHttpClient: TALWinInetHTTPClient;
    FResponseHeader: TALHTTPResponseHeader;
    FTileRequestBuilder: ITileRequestBuilder;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FTileDownloaderConfigStatic: ITileDownloaderConfigStatic;
    FRawResponseHeader: string;
    FEvent: ITileDownloaderEvent;
    FSemaphore: THandle;
    FParentSemaphore: THandle;
    FBusy: Boolean;
    FWasConnectError: Boolean;
    FLastDownloadTime: Cardinal;
    FIsCanceled: Boolean;
    FSessionCS: TCriticalSection;
    procedure PrepareHttpClientConfig(const AAcceptEncoding, ARawHeaders: string);
    procedure PreProcess;
    procedure PostProcess;
    procedure DoRequest;
    function IsCanceled: Boolean;
    procedure SetIsCanceled;
    procedure SetNotCanceled;
    procedure SleepCancelable(ATime: Cardinal);
    function IsConnectError(ALastError: Cardinal): Boolean;
    function IsDownloadError(ALastError: Cardinal): Boolean;
    function IsOkStatus(AStatusCode: Cardinal): Boolean;
    function IsDownloadErrorStatus(AStatusCode: Cardinal): Boolean;
    function IsTileNotExistStatus(AStatusCode: Cardinal): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEvent(AEvent: ITileDownloaderEvent);
    procedure OnCancelEvent(Sender: TObject);
    property Busy: Boolean read FBusy default False;
    property TileRequestBuilder: ITileRequestBuilder write FTileRequestBuilder default nil;
    property TileDownloaderConfig: ITileDownloaderConfig write FTileDownloaderConfig default nil;
    property Semaphore: THandle read FParentSemaphore write FParentSemaphore;
    property RawResponseHeader: string write FRawResponseHeader;
  end;

implementation

uses
  WinInet,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_DownloadChecker,
  u_NotifyEventListener;

{ TTileDownloaderBaseThread }

constructor TTileDownloaderBaseThread.Create;
begin
  FRawResponseHeader := '';
  FCancelEvent := TEvent.Create;
  FCancelListener := TNotifyEventListener.Create(Self.OnCancelEvent);
  FIsCanceled := False;
  FSessionCS := TCriticalSection.Create;
  FSemaphore := CreateSemaphore(nil, 0, 1, nil);
  FHttpClient := TALWinInetHTTPClient.Create(nil);
  FResponseHeader := TALHTTPResponseHeader.Create;
  FWasConnectError := False;
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TTileDownloaderBaseThread.Destroy;
begin
  try
    SetIsCanceled;
    FResponseHeader.Free;
    FHttpClient.Free;
    CloseHandle(FSemaphore);
    FreeAndNil(FSessionCS);
    FreeAndNil(FCancelEvent);
  finally
    inherited Destroy;
  end;
end;

procedure TTileDownloaderBaseThread.AddEvent(AEvent: ITileDownloaderEvent);
begin
  FBusy := True;
  FEvent := AEvent;
  ReleaseSemaphore(FSemaphore, 1, nil);
end;

procedure TTileDownloaderBaseThread.PreProcess;

  procedure SleepIfConnectErrorOrWaitInterval;
  var
    VNow: Cardinal;
    VTimeFromLastDownload: Cardinal;
    VSleepTime: Cardinal;
    VInetConfig: IInetConfigStatic;
  begin
    VInetConfig := FTileDownloaderConfigStatic.InetConfigStatic;
    VNow := GetTickCount;
    if VNow >= FLastDownloadTime then begin
      VTimeFromLastDownload := VNow - FLastDownloadTime;
    end else begin
      VTimeFromLastDownload := MaxInt;
    end;
    if FWasConnectError then begin
      if VTimeFromLastDownload < VInetConfig.SleepOnResetConnection then begin
        VSleepTime := VInetConfig.SleepOnResetConnection - VTimeFromLastDownload;
        SleepCancelable(VSleepTime);
      end;
    end else begin
      if VTimeFromLastDownload < FTileDownloaderConfigStatic.WaitInterval then begin
        VSleepTime := FTileDownloaderConfigStatic.WaitInterval - VTimeFromLastDownload;
        SleepCancelable(VSleepTime);
      end;
    end;
  end;

var
  VUrl: string;
  VRawRequestHeader: string;
begin
  SleepIfConnectErrorOrWaitInterval;
  FTileRequestBuilder.BuildRequest(
    FEvent.TileXY,
    FEvent.TileZoom,
    nil,
    nil,
    VUrl,
    VRawRequestHeader
  );
  FEvent.Url := VUrl;
  FEvent.RawRequestHeader := VRawRequestHeader;
  PrepareHttpClientConfig(FTileDownloaderConfigStatic.DefaultMIMEType, FEvent.RawRequestHeader);
  FEvent.OnBeforeRequest(FTileDownloaderConfigStatic);
end;

procedure TTileDownloaderBaseThread.PostProcess;
var
  VStatusCode: Cardinal;
begin
  FRawResponseHeader := FResponseHeader.RawHeaderText;
  FEvent.RawResponseHeader := FRawResponseHeader;
  FEvent.TileMIME := FResponseHeader.ContentType;
  VStatusCode := StrToIntDef(FResponseHeader.StatusCode, 0);
  FEvent.HttpStatusCode := VStatusCode;
  if IsOkStatus(VStatusCode) then
    FEvent.OnAfterResponse
  else
    if IsDownloadErrorStatus(VStatusCode) then
      FEvent.DownloadResult := FEvent.ResultFactory.BuildLoadErrorByStatusCode(VStatusCode)
    else
      if IsTileNotExistStatus(VStatusCode) then
        FEvent.DownloadResult := FEvent.ResultFactory.BuildDataNotExistsByStatusCode(FEvent.RawResponseHeader, VStatusCode)
      else
        FEvent.DownloadResult := FEvent.ResultFactory.BuildLoadErrorByUnknownStatusCode(VStatusCode);
end;

procedure TTileDownloaderBaseThread.DoRequest;
var
  VCount: Integer;
  VTryCount: Integer;
begin
  FTileDownloaderConfigStatic := FTileDownloaderConfig.GetStatic;
  SetNotCanceled;
  try
    try
      if (FTileDownloaderConfigStatic <> nil) and (FTileRequestBuilder <> nil) then
      begin
        try
          if (FEvent <> nil) and (FEvent.CancelNotifier <> nil) then begin
            FEvent.CancelNotifier.Add(FCancelListener);
          end;

          if FEvent.DownloadResult = nil then
          begin
            VCount := 0;
            VTryCount := FTileDownloaderConfigStatic.InetConfigStatic.DownloadTryCount;
            FWasConnectError := False;
            FLastDownloadTime := MaxInt;
            repeat
              PreProcess;

              if IsCanceled then
              begin
                FEvent.DownloadResult := FEvent.ResultFactory.BuildCanceled;
                Exit;
              end;

              try
                FResponseHeader.Clear;
                FHttpClient.Get(FEvent.Url, FEvent.TileStream, FResponseHeader);
              except
                on E: EALHTTPClientException do
                begin
                  if E.StatusCode = 0 then
                    FEvent.DownloadResult := FEvent.ResultFactory.BuildNotNecessary(E.Message, FResponseHeader.RawHeaderText)
                  else
                    FEvent.DownloadResult := FEvent.ResultFactory.BuildLoadErrorByStatusCode(E.StatusCode);
                end;
                on E: EOSError do
                begin
                  if IsConnectError(E.ErrorCode) then
                    FEvent.DownloadResult := FEvent.ResultFactory.BuildNoConnetctToServerByErrorCode(E.ErrorCode)
                  else
                    if IsDownloadError(E.ErrorCode) then
                      FEvent.DownloadResult := FEvent.ResultFactory.BuildLoadErrorByErrorCode(E.ErrorCode)
                    else
                      FEvent.DownloadResult := FEvent.ResultFactory.BuildNoConnetctToServerByErrorCode(E.ErrorCode)
                end;
              end;

              if IsCanceled then begin
                FEvent.DownloadResult := FEvent.ResultFactory.BuildCanceled;
                Exit;
              end;

              Inc(VCount);
              FLastDownloadTime := GetTickCount;
              PostProcess;

              if FEvent.DownloadResult <> nil then begin
                FWasConnectError := not FEvent.DownloadResult.IsServerExists;
              end;

            until (not FWasConnectError) or (VCount >= VTryCount);
          end;
        finally
          if (FEvent <> nil) and (FEvent.CancelNotifier <> nil) then begin
            FEvent.CancelNotifier.Remove(FCancelListener);
          end;
        end;
      end;
    finally
      FEvent.ProcessEvent;
    end;
  finally
    FBusy := False;
    if FParentSemaphore <> 0 then    
      ReleaseSemaphore(FParentSemaphore, 1, nil);
  end;
end;

procedure TTileDownloaderBaseThread.Execute;
begin
  repeat
    if Terminated then
      Break;

    repeat
      if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0  then
        Break
      else
        if Terminated then
           Break;
    until False;

    if Assigned(FEvent) then
      DoRequest;

  until False;
end;

procedure TTileDownloaderBaseThread.OnCancelEvent(Sender: TObject);
begin
  SetIsCanceled;
  if Assigned(FHttpClient) then
    FHttpClient.Disconnect;
end;

function TTileDownloaderBaseThread.IsCanceled: Boolean;
begin
  FSessionCS.Acquire;
  try
    Result := FIsCanceled;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBaseThread.SetIsCanceled;
begin
  FSessionCS.Acquire;
  try
    FCancelEvent.SetEvent;
    FIsCanceled := True;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBaseThread.SetNotCanceled;
begin
  FSessionCS.Acquire;
  try
    FCancelEvent.ResetEvent;
    FIsCanceled := False;
  finally
    FSessionCS.Release;
  end;
end;  

procedure TTileDownloaderBaseThread.SleepCancelable(ATime: Cardinal);
begin
  if ATime > 0 then
    FCancelEvent.WaitFor(ATime);
end;

procedure TTileDownloaderBaseThread.PrepareHttpClientConfig(const AAcceptEncoding, ARawHeaders: string);
var
  VInetConfig: IInetConfigStatic;
  VProxyConfig: IProxyConfigStatic;
begin
  VInetConfig := FTileDownloaderConfig.InetConfigStatic;

  FHttpClient.RequestHeader.UserAgent := VInetConfig.UserAgentString;

  if AAcceptEncoding <> '' then
    FHttpClient.RequestHeader.Accept := AAcceptEncoding
  else
    FHttpClient.RequestHeader.Accept := '*/*';

  if ARawHeaders <> '' then
    FHttpClient.RequestHeader.RawHeaderText := ARawHeaders;

  FHttpClient.ConnectTimeout := VInetConfig.TimeOut;
  FHttpClient.SendTimeout := VInetConfig.TimeOut;
  FHttpClient.ReceiveTimeout := VInetConfig.TimeOut;

  FHttpClient.InternetOptions := [  wHttpIo_No_cache_write,
                                    wHttpIo_Pragma_nocache,
                                    wHttpIo_No_cookies,
                                    wHttpIo_Keep_connection
                                 ];

  VProxyConfig := VInetConfig.ProxyConfigStatic;
  if Assigned(VProxyConfig) then
  begin
    if VProxyConfig.UseIESettings then
      FHttpClient.AccessType := wHttpAt_Preconfig
    else
      if VProxyConfig.UseProxy then
      begin
        FHttpClient.AccessType := wHttpAt_Proxy;
        FHttpClient.ProxyParams.ProxyServer := Copy(VProxyConfig.Host, 0, Pos(':', VProxyConfig.Host) - 1);
        FHttpClient.ProxyParams.ProxyPort := StrToInt(Copy(VProxyConfig.Host, Pos(':', VProxyConfig.Host) + 1));
        if VProxyConfig.UseLogin then
        begin
          FHttpClient.ProxyParams.ProxyUserName := VProxyConfig.Login;
          FHttpClient.ProxyParams.ProxyPassword := VProxyConfig.Password;
        end;
      end
      else
        FHttpClient.AccessType := wHttpAt_Direct;
  end;
end;

function TTileDownloaderBaseThread.IsConnectError(ALastError: Cardinal): Boolean;
begin
  case ALastError of
    ERROR_INTERNET_OUT_OF_HANDLES,
    ERROR_INTERNET_TIMEOUT,
    ERROR_INTERNET_INTERNAL_ERROR,
    ERROR_INTERNET_INVALID_URL,
    ERROR_INTERNET_UNRECOGNIZED_SCHEME,
    ERROR_INTERNET_NAME_NOT_RESOLVED,
    ERROR_INTERNET_PROTOCOL_NOT_FOUND,
    ERROR_INTERNET_SHUTDOWN,
    ERROR_INTERNET_INVALID_OPERATION,
    ERROR_INTERNET_OPERATION_CANCELLED,
    ERROR_INTERNET_NOT_PROXY_REQUEST,
    ERROR_INTERNET_NO_DIRECT_ACCESS,
    ERROR_INTERNET_INCORRECT_FORMAT,
    ERROR_INTERNET_ITEM_NOT_FOUND,
    ERROR_INTERNET_CANNOT_CONNECT,
    ERROR_INTERNET_INVALID_PROXY_REQUEST,
    ERROR_HTTP_INVALID_HEADER,
    ERROR_INTERNET_TCPIP_NOT_INSTALLED,
    ERROR_INTERNET_SERVER_UNREACHABLE,
    ERROR_INTERNET_PROXY_SERVER_UNREACHABLE,
    ERROR_INTERNET_BAD_AUTO_PROXY_SCRIPT,
    ERROR_INTERNET_UNABLE_TO_DOWNLOAD_SCRIPT:
    begin
      Result := True;
    end;
  else
    begin
      Result := False;
    end;
  end;
end;

function TTileDownloaderBaseThread.IsDownloadError(ALastError: Cardinal): Boolean;
begin
  case ALastError of
    ERROR_INTERNET_EXTENDED_ERROR,
    ERROR_INTERNET_CONNECTION_ABORTED,
    ERROR_INTERNET_CONNECTION_RESET,
    ERROR_INTERNET_SEC_CERT_CN_INVALID,
    ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR,
    ERROR_INTERNET_HTTPS_TO_HTTP_ON_REDIR,
    ERROR_HTTP_DOWNLEVEL_SERVER,
    ERROR_HTTP_INVALID_SERVER_RESPONSE,
    ERROR_HTTP_INVALID_HEADER,
    ERROR_HTTP_REDIRECT_FAILED:
    begin
      Result := True;
    end;
  else
    begin
      Result := False;
    end;
  end;
end;

function TTileDownloaderBaseThread.IsDownloadErrorStatus(AStatusCode: Cardinal): Boolean;
begin
  case AStatusCode of
    HTTP_STATUS_SERVER_ERROR,
    HTTP_STATUS_NOT_SUPPORTED,
    HTTP_STATUS_BAD_GATEWAY,
    HTTP_STATUS_SERVICE_UNAVAIL,
    HTTP_STATUS_GATEWAY_TIMEOUT:
    begin
      Result := True;
    end;
  else
    begin
      Result := False;
    end;
  end;
end;

function TTileDownloaderBaseThread.IsOkStatus(AStatusCode: Cardinal): Boolean;
begin
  case AStatusCode of
    HTTP_STATUS_OK,
    HTTP_STATUS_CREATED,
    HTTP_STATUS_ACCEPTED,
    HTTP_STATUS_PARTIAL,
    HTTP_STATUS_RESET_CONTENT,
    HTTP_STATUS_PARTIAL_CONTENT:
    begin
      Result := True;
    end;
  else
    begin
      Result := False;
    end;
  end;
end;

function TTileDownloaderBaseThread.IsTileNotExistStatus(AStatusCode: Cardinal): Boolean;
begin
  case AStatusCode of
    HTTP_STATUS_NO_CONTENT,
    HTTP_STATUS_BAD_REQUEST,
    HTTP_STATUS_NOT_FOUND:
    begin
      Result := True;
    end;
  else
    begin
      Result := False;
    end;
  end;
end;

end.

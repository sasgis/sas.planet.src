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
  i_OperationNotifier,
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

begin
  SleepIfConnectErrorOrWaitInterval;
  FEvent.Request := FTileRequestBuilder.BuildRequest(
    FEvent.TileXY,
    FEvent.TileZoom,
    FEvent.VersionInfo,
    FEvent.LastResponseInfo
  );
  PrepareHttpClientConfig(
    FTileDownloaderConfigStatic.DefaultMIMEType,
    FEvent.Request.RequestHeader
  );
  FEvent.OnBeforeRequest(FTileDownloaderConfigStatic);
end;

procedure TTileDownloaderBaseThread.PostProcess;
var
  VStatusCode: Cardinal;
begin
  VStatusCode := StrToIntDef(FResponseHeader.StatusCode, 0);
  FEvent.HttpStatusCode := VStatusCode;
  FEvent.LastResponseInfo.ResponseHead := FResponseHeader.RawHeaderText;
  FEvent.TileMIME := FResponseHeader.ContentType;
  if IsOkStatus(VStatusCode) then begin
    FEvent.OnAfterResponse(FResponseHeader.RawHeaderText)
  end else if IsDownloadErrorStatus(VStatusCode) then begin
    FEvent.DownloadResult :=
      FEvent.ResultFactory.BuildLoadErrorByStatusCode(
        FEvent.Request,
        VStatusCode
      );
  end else if IsTileNotExistStatus(VStatusCode) then begin
    FEvent.DownloadResult :=
      FEvent.ResultFactory.BuildDataNotExistsByStatusCode(
        FEvent.Request,
        FResponseHeader.RawHeaderText,
        VStatusCode
      );
  end else begin
    FEvent.DownloadResult :=
      FEvent.ResultFactory.BuildLoadErrorByUnknownStatusCode(
        FEvent.Request,
        VStatusCode
      );
  end;
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
      if (FTileDownloaderConfigStatic <> nil) and (FTileRequestBuilder <> nil) then begin
        try
          if (FEvent <> nil) and (FEvent.CancelNotifier <> nil) then begin
            FEvent.CancelNotifier.AddListener(FCancelListener);
          end;
          if FEvent.DownloadResult = nil then begin
            VCount := 0;
            VTryCount := FTileDownloaderConfigStatic.InetConfigStatic.DownloadTryCount;
            FWasConnectError := False;
            FLastDownloadTime := MaxInt;
            repeat
              PreProcess;
              if IsCanceled then begin
                if FEvent.ResultFactory <> nil then begin
                  FEvent.DownloadResult := FEvent.ResultFactory.BuildCanceled(FEvent.Request);
                end;
                Exit;
              end;
              try
                FResponseHeader.Clear;
                FHttpClient.Get(
                  FEvent.Request.Url,
                  FEvent.TileStream,
                  FResponseHeader
                );
              except
                on E: EALHTTPClientException do begin
                  if FEvent.ResultFactory <> nil then begin
                    if E.StatusCode = 0 then begin
                      FEvent.DownloadResult :=
                        FEvent.ResultFactory.BuildNotNecessary(
                          FEvent.Request,
                          E.Message,
                          FResponseHeader.RawHeaderText
                        );
                    end else begin
                      FEvent.DownloadResult :=
                        FEvent.ResultFactory.BuildLoadErrorByStatusCode(
                          FEvent.Request,
                          E.StatusCode
                        );
                    end;
                  end;
                end;
                on E: EOSError do begin
                  if FEvent.ResultFactory <> nil then begin
                    if IsConnectError(E.ErrorCode) then begin
                      FEvent.DownloadResult :=
                        FEvent.ResultFactory.BuildNoConnetctToServerByErrorCode(
                          FEvent.Request,
                          E.ErrorCode
                        );
                    end else if IsDownloadError(E.ErrorCode) then begin
                      FEvent.DownloadResult :=
                        FEvent.ResultFactory.BuildLoadErrorByErrorCode(
                          FEvent.Request,
                          E.ErrorCode
                        );
                    end else begin
                      FEvent.DownloadResult :=
                        FEvent.ResultFactory.BuildNoConnetctToServerByErrorCode(
                          FEvent.Request,
                          E.ErrorCode
                        );
                    end;
                  end;
                end; 
              end;
              if IsCanceled then begin
                if FEvent.ResultFactory <> nil then begin
                  FEvent.DownloadResult := FEvent.ResultFactory.BuildCanceled(FEvent.Request);
                end;
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
            FEvent.CancelNotifier.RemoveListener(FCancelListener);
          end;
        end;
      end;
    finally
      FEvent.ProcessEvent;
    end;
  finally
    FBusy := False;
    if FParentSemaphore <> 0 then begin
      ReleaseSemaphore(FParentSemaphore, 1, nil);
    end;
  end;
end;

procedure TTileDownloaderBaseThread.Execute;
begin
  repeat
    if Terminated then begin
      Break;
    end;
    repeat
      if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0  then begin
        Break
      end else if Terminated then begin
        Break;
      end;
    until False;
    if Assigned(FEvent) then begin
      DoRequest;
    end;
  until False;
end;

procedure TTileDownloaderBaseThread.OnCancelEvent(Sender: TObject);
begin
  SetIsCanceled;
  if Assigned(FHttpClient) then begin
    FHttpClient.Disconnect;
  end;
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
  if ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

procedure TTileDownloaderBaseThread.PrepareHttpClientConfig(const AAcceptEncoding, ARawHeaders: string);
var
  VInetConfig: IInetConfigStatic;
  VProxyConfig: IProxyConfigStatic;
begin
  VInetConfig := FTileDownloaderConfig.InetConfigStatic;
  FHttpClient.RequestHeader.UserAgent := VInetConfig.UserAgentString;
  if AAcceptEncoding <> '' then begin
    FHttpClient.RequestHeader.Accept := AAcceptEncoding
  end else begin
    FHttpClient.RequestHeader.Accept := '*/*';
  end;
  if ARawHeaders <> '' then begin
    FHttpClient.RequestHeader.RawHeaderText := ARawHeaders;
  end;
  FHttpClient.ConnectTimeout := VInetConfig.TimeOut;
  FHttpClient.SendTimeout := VInetConfig.TimeOut;
  FHttpClient.ReceiveTimeout := VInetConfig.TimeOut;
  FHttpClient.InternetOptions := [  wHttpIo_No_cache_write,
                                    wHttpIo_Pragma_nocache,
                                    wHttpIo_No_cookies,
                                    wHttpIo_Keep_connection
                                 ];
  VProxyConfig := VInetConfig.ProxyConfigStatic;
  if Assigned(VProxyConfig) then begin
    if VProxyConfig.UseIESettings then begin
      FHttpClient.AccessType := wHttpAt_Preconfig
    end else if VProxyConfig.UseProxy then begin
      FHttpClient.AccessType := wHttpAt_Proxy;
      FHttpClient.ProxyParams.ProxyServer :=
        Copy(VProxyConfig.Host, 0, Pos(':', VProxyConfig.Host) - 1);
      FHttpClient.ProxyParams.ProxyPort :=
        StrToInt(Copy(VProxyConfig.Host, Pos(':', VProxyConfig.Host) + 1));
      if VProxyConfig.UseLogin then begin
        FHttpClient.ProxyParams.ProxyUserName := VProxyConfig.Login;
        FHttpClient.ProxyParams.ProxyPassword := VProxyConfig.Password;
      end;
    end else begin
      FHttpClient.AccessType := wHttpAt_Direct;
    end;
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

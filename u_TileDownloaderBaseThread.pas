unit u_TileDownloaderBaseThread;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ALHTTPCommon,
  ALHttpClient,
  ALWinInetHttpClient,
  i_ProxySettings,
  i_RequestBuilderScript,
  i_TileDownloader,
  i_TileDownloaderConfig;

type
  TTileDownloaderBaseThread = class(TThread)
  private
    FHttpClient: TALWinInetHTTPClient;
    FResponseHeader: TALHTTPResponseHeader;
    FRequestBuilderScript: IRequestBuilderScript;
    FTileDownloaderConfig: ITileDownloaderConfigStatic;
    FRawResponseHeader: string;
    FEvent: ITileDownloaderEvent;
    FSemaphore: THandle;
    FParentSemaphore: THandle;
    FBusy: Boolean;
    procedure PrepareHttpClientConfig(const AAcceptEncoding, ARawHeaders: string);
    procedure PreProcess;
    procedure PostProcess;
    procedure DoRequest;
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
    property Busy: Boolean read FBusy default False;
    property RequestBuilderScript: IRequestBuilderScript write FRequestBuilderScript default nil;
    property TileDownloaderConfig: ITileDownloaderConfigStatic write FTileDownloaderConfig default nil;
    property Semaphore: THandle read FParentSemaphore write FParentSemaphore;
    property RawResponseHeader: string write FRawResponseHeader;
  end;

implementation

uses
  WinInet,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_DownloadChecker;

{ TTileDownloaderBaseThread }

constructor TTileDownloaderBaseThread.Create;
begin
  FRawResponseHeader := '';
  FSemaphore := CreateSemaphore(nil, 0, 1, nil);
  FHttpClient := TALWinInetHTTPClient.Create(nil);
  FHttpClient.RequestHeader.UserAgent := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)';
  FResponseHeader := TALHTTPResponseHeader.Create;
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TTileDownloaderBaseThread.Destroy;
begin
  FResponseHeader.Free;
  FHttpClient.Free;
  CloseHandle(FSemaphore);
  inherited Destroy;
end;

procedure TTileDownloaderBaseThread.AddEvent(AEvent: ITileDownloaderEvent);
begin
  FBusy := True;
  FEvent := AEvent;
  ReleaseSemaphore(FSemaphore, 1, nil);
end;

procedure TTileDownloaderBaseThread.PreProcess;
var
  VUrl: string;
  VRawRequestHeader: string;
begin
  FRequestBuilderScript.GenRequest(FEvent.TileXY, FEvent.TileZoom, FRawResponseHeader, VUrl, VRawRequestHeader);
  FEvent.Url := VUrl;
  FEvent.RawRequestHeader := VRawRequestHeader;
  PrepareHttpClientConfig(FTileDownloaderConfig.DefaultMIMEType, FEvent.RawRequestHeader);
  FEvent.OnBeforeRequest(FTileDownloaderConfig);
end;

procedure TTileDownloaderBaseThread.PostProcess;
var
  VStatusCode: Cardinal;
begin
  FEvent.RawResponseHeader := FResponseHeader.RawHeaderText;
  FEvent.TileMIME := FResponseHeader.ContentType;
  VStatusCode := StrToInt(FResponseHeader.StatusCode);
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
begin
  try
    try
      if (FTileDownloaderConfig <> nil) and (FRequestBuilderScript <> nil) then
      begin
        PreProcess;
        if FEvent.DownloadResult = nil then
        begin
          VCount := 0;
          repeat
            try
              FResponseHeader.Clear;
              FHttpClient.Get(FEvent.Url, FEvent.TileStream, FResponseHeader);
              Inc(VCount);
            except
              on E: EALHTTPClientException do
              begin
                FEvent.DownloadResult := FEvent.ResultFactory.BuildLoadErrorByStatusCode(E.StatusCode);
                FEvent.ErrorString := IntToStr(E.StatusCode) + ' ' + FResponseHeader.ReasonPhrase;
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
            PostProcess;
          until Supports(FEvent.DownloadResult, IDownloadResultOk) or (VCount >= FTileDownloaderConfig.DownloadTryCount);
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

procedure TTileDownloaderBaseThread.PrepareHttpClientConfig(const AAcceptEncoding, ARawHeaders: string);
var
  VProxyConfig: IProxyConfigStatic;
begin
  FHttpClient.RequestHeader.Accept := AAcceptEncoding;

  if ARawHeaders <> '' then
    FHttpClient.RequestHeader.RawHeaderText := ARawHeaders;

  FHttpClient.ConnectTimeout := FTileDownloaderConfig.TimeOut;
  FHttpClient.SendTimeout := FTileDownloaderConfig.TimeOut;
  FHttpClient.ReceiveTimeout := FTileDownloaderConfig.TimeOut;

  FHttpClient.InternetOptions := [  wHttpIo_No_cache_write,
                                    wHttpIo_Pragma_nocache,
                                    wHttpIo_No_cookies,
                                    wHttpIo_Keep_connection
                                 ];

  VProxyConfig := FTileDownloaderConfig.ProxyConfigStatic;
  if Assigned(VProxyConfig) then
  begin
    if VProxyConfig.UseIESettings then
      FHttpClient.AccessType := wHttpAt_Preconfig
    else
      if VProxyConfig.UseProxy then
      begin
        FHttpClient.AccessType := wHttpAt_Proxy;
        FHttpClient.ProxyParams.ProxyServer := Copy(VProxyConfig.Host, 0, Pos(':', VProxyConfig.Host));
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

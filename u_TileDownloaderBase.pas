unit u_TileDownloaderBase;

interface

uses
  Windows,
  WinInet,
  SyncObjs,
  Classes,
  i_JclNotify,
  i_ProxySettings,
  i_TileDownloaderConfig,
  i_TileDownlodSession;

type
  TTileDownloaderBase = class(TInterfacedObject, ITileDownlodSession)
  private
    FConfig: ITileDownloaderConfig;
    FConfigStatic: ITileDownloaderConfigStatic;
    FConfigListener: IJclListener;
    FUserAgentString: string;
    procedure OnConfigChange(Sender: TObject);
  protected
    FSessionHandle: HInternet;
    FSessionOpenError: Cardinal;
    FSessionCS: TCriticalSection;
    FDownloadCS: TCriticalSection;
    FLastDownloadTime: Cardinal;
    FLastDownloadResult: TDownloadTileResult;
    function IsDownloadError(ALastError: Cardinal): Boolean; virtual;
    function IsOkStatus(AStatusCode: Cardinal): Boolean; virtual;
    function IsDownloadErrorStatus(AStatusCode: Cardinal): Boolean; virtual;
    function IsTileNotExistStatus(AStatusCode: Cardinal): Boolean; virtual;
    procedure ResetConnetction; virtual;
    procedure OpenSession; virtual;
    procedure CloseSession; virtual;
    function BuildHeader(AUrl, AHead: string): string; virtual;
    function TryDownload(
      AConfig: ITileDownloaderConfigStatic;
      AUrl, ARequestHead: string;
      ACheckTileSize: Boolean;
      AExistsFileSize: Cardinal;
      fileBuf: TMemoryStream;
      out AServerExists: Boolean;
      out AStatusCode: Cardinal;
      out AContentType, AResponseHead: string
    ): TDownloadTileResult; virtual;
    procedure ProcessDataRequest(AFileHandle: HInternet; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AContentType: string; var AResponseHead: string); virtual;
    procedure GetData(AFileHandle: HInternet; fileBuf: TMemoryStream); virtual;
    function IsGlobalOffline(ASessionHandle: HInternet): Boolean;
    procedure ResetGlobalOffline(ASessionHandle: HInternet);
    procedure CheckTargetSize(
      AFileHandle: HInternet;
      ACheckTileSize: Boolean;
      AExistsFileSize: Cardinal
    );
    procedure CheckContentType(
      AFileHandle: HInternet;
      var AContentType: string
    );
    procedure ProxyAuth(
      AProxyConfig: IProxyConfigStatic;
      AFileHandle: HInternet;
      out AStatusCode: Cardinal
    );
    function GetStatusCode(AFileHandle: HInternet): Cardinal;
    procedure GetResponsHead(AFileHandle: HInternet; var AResponseHead: string);
  protected
    function DownloadTile(AUrl, ARequestHead: string; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AStatusCode: Cardinal; out AContentType, AResponseHead: string): TDownloadTileResult; virtual;
  public
    constructor Create(AConfig: ITileDownloaderConfig);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener;

{ TTileDownloaderBase }

constructor TTileDownloaderBase.Create(AConfig: ITileDownloaderConfig);
begin
  FConfig := AConfig;
  FDownloadCS := TCriticalSection.Create;
  FSessionCS := TCriticalSection.Create;

  FConfigListener := TNotifyEventListener.Create(Self.OnConfigChange);
  FConfig.GetChangeNotifier.Add(FConfigListener);
  OnConfigChange(nil);

  FUserAgentString := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)';
  FLastDownloadResult := dtrOK;
  OpenSession;
end;

destructor TTileDownloaderBase.Destroy;
begin
  FConfig.GetChangeNotifier.Remove(FConfigListener);
  FConfigListener := nil;
  FConfig := nil;
  FConfigStatic := nil;
  CloseSession;
  FreeAndNil(FDownloadCS);
  FreeAndNil(FSessionCS);
  inherited;
end;

function TTileDownloaderBase.BuildHeader(AUrl, AHead: string): string;
begin
  Result := AHead;
end;

procedure TTileDownloaderBase.CloseSession;
begin
  FSessionCS.Acquire;
  try
    if Assigned(FSessionHandle) then begin
      InternetCloseHandle(FSessionHandle);
    end;
  finally
    FSessionCS.Release;
  end;
end;

function TTileDownloaderBase.DownloadTile(AUrl, ARequestHead: string;
  ACheckTileSize: Boolean; AExistsFileSize: Cardinal;
  fileBuf: TMemoryStream; out AStatusCode: Cardinal;
  out AContentType, AResponseHead: string): TDownloadTileResult;
var
  VTryCount: Integer;
  VDownloadTryCount: Integer;
  VConfig: ITileDownloaderConfigStatic;
  VServerExists: Boolean;
  VNow: Cardinal;
begin
  FSessionCS.Acquire;
  try
    if not Assigned(FSessionHandle) then begin
      Result := dtrErrorInternetOpen;
      exit;
    end;
  finally
    FSessionCS.Release;
  end;
  VConfig := FConfigStatic;
  VDownloadTryCount := VConfig.DownloadTryCount;
  FDownloadCS.Acquire;
  try
    VTryCount := 0;
    Result := FLastDownloadResult;
    repeat
      if Result = dtrDownloadError then begin
        ResetConnetction;
      end;
      VNow := GetTickCount;
      if VNow < FLastDownloadTime + VConfig.WaitInterval then begin
        Sleep(VConfig.WaitInterval);
      end;
      Result := TryDownload(VConfig, AUrl, ARequestHead, ACheckTileSize, AExistsFileSize, fileBuf, VServerExists, AStatusCode, AContentType, AResponseHead);
      Inc(VTryCount);
      if VServerExists then begin
        FLastDownloadTime := GetTickCount;
      end;
    until (Result <> dtrDownloadError) or (VTryCount >= VDownloadTryCount);
    FLastDownloadResult := Result;
  finally
    FDownloadCS.Release;
  end;
end;

procedure TTileDownloaderBase.GetData(AFileHandle: HInternet;
  fileBuf: TMemoryStream);
var
  VBuffer: array [1..64535] of Byte;
  VBufferLen: LongWord;
begin
  repeat
    if InternetReadFile(AFileHandle, @VBuffer, SizeOf(VBuffer), VBufferLen) then begin
      filebuf.Write(VBuffer, VBufferLen);
    end else begin
      RaiseLastOSError;
    end;
  until (VBufferLen = 0);
end;

function TTileDownloaderBase.IsDownloadError(
  ALastError: Cardinal): Boolean;
begin
  case ALastError of
    ERROR_INTERNET_CONNECTION_RESET,
    ERROR_INTERNET_CANNOT_CONNECT,
    ERROR_HTTP_INVALID_SERVER_RESPONSE,
    ERROR_INTERNET_DISCONNECTED,
    ERROR_INTERNET_FORCE_RETRY,
    ERROR_INTERNET_OPERATION_CANCELLED,
    ERROR_INTERNET_PROXY_SERVER_UNREACHABLE,
    ERROR_INTERNET_SERVER_UNREACHABLE,
    ERROR_INTERNET_SHUTDOWN,
    ERROR_INTERNET_NAME_NOT_RESOLVED,
    ERROR_INTERNET_TIMEOUT:
    begin
      Result := true;
    end;
  else begin
    Result := false;
  end;
  end;
end;

function TTileDownloaderBase.IsDownloadErrorStatus(
  AStatusCode: Cardinal): Boolean;
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
  else begin
    Result := False;
  end;
  end;
end;

function TTileDownloaderBase.IsOkStatus(AStatusCode: Cardinal): Boolean;
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
  else begin
    Result := False;
  end;
  end;
end;

function TTileDownloaderBase.IsTileNotExistStatus(
  AStatusCode: Cardinal): Boolean;
begin
  case AStatusCode of
    HTTP_STATUS_NO_CONTENT,
    HTTP_STATUS_BAD_REQUEST,
    HTTP_STATUS_NOT_FOUND:
    begin
      Result := True;
    end;
  else begin
    Result := False;
  end;
  end;
end;

procedure TTileDownloaderBase.OnConfigChange(Sender: TObject);
begin
  FConfigStatic := FConfig.GetStatic;
  FSessionCS.Acquire;
  try
    CloseSession;
    OpenSession;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBase.OpenSession;
var
  VTimeOut: DWORD;
begin
  FSessionCS.Acquire;
  try
    FSessionHandle := InternetOpen(pChar(FUserAgentString), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    if Assigned(FSessionHandle) then begin
      FSessionOpenError := 0;
      VTimeOut := FConfigStatic.TimeOut;
      if not InternetSetOption(FSessionHandle, INTERNET_OPTION_CONNECT_TIMEOUT, @VTimeOut, sizeof(VTimeOut)) then begin
        FSessionOpenError := GetLastError;
      end;
      if not InternetSetOption(FSessionHandle, INTERNET_OPTION_DATA_RECEIVE_TIMEOUT, @VTimeOut, sizeof(VTimeOut)) then begin
        FSessionOpenError := GetLastError;
      end;
      if not InternetSetOption(FSessionHandle, INTERNET_OPTION_DATA_SEND_TIMEOUT, @VTimeOut, sizeof(VTimeOut)) then begin
        FSessionOpenError := GetLastError;
      end;
      if not InternetSetOption(FSessionHandle, INTERNET_OPTION_SEND_TIMEOUT, @VTimeOut, sizeof(VTimeOut)) then begin
        FSessionOpenError := GetLastError;
      end;
      if not InternetSetOption(FSessionHandle, INTERNET_OPTION_RECEIVE_TIMEOUT, @VTimeOut, sizeof(VTimeOut)) then begin
        FSessionOpenError := GetLastError;
      end;
      ResetGlobalOffline(FSessionHandle);
    end else begin
      FSessionOpenError := GetLastError;
    end;
  finally
    FSessionCS.Release;
  end;
end;

type
  EMimeTypeError = class(Exception);

procedure TTileDownloaderBase.CheckContentType(AFileHandle: HInternet;
  var AContentType: string);
var
  VConfig: ITileDownloaderConfigStatic;
  VBufSize: Cardinal;
  dwIndex: Cardinal;
  VLastError: Cardinal;
begin
  VConfig := FConfigStatic;
  if VConfig.IgnoreMIMEType then begin
    AContentType := VConfig.DefaultMIMEType;
  end else begin
    VBufSize := Length(AContentType);
    if VBufSize = 0 then begin
      SetLength(AContentType, 20);
      VBufSize := Length(AContentType);
    end;
    FillChar(AContentType[1], VBufSize, 0);
    dwIndex := 0;
    if not HttpQueryInfo(AFileHandle, HTTP_QUERY_CONTENT_TYPE, @AContentType[1], VBufSize, dwIndex) then begin
      VLastError := GetLastError;
      if VLastError = ERROR_INSUFFICIENT_BUFFER then begin
        SetLength(AContentType, VBufSize);
        if not HttpQueryInfo(AFileHandle, HTTP_QUERY_CONTENT_TYPE, @AContentType[1], VBufSize, dwIndex) then begin
          RaiseLastOSError;
        end;
      end else if VLastError = ERROR_HTTP_HEADER_NOT_FOUND then begin
        AContentType := '';
      end else begin
        RaiseLastOSError(VLastError);
      end;
    end;
    AContentType := trim(AContentType);
    if (AContentType = '') then begin
      AContentType := VConfig.DefaultMIMEType;
    end else if (Pos(AContentType, VConfig.ExpectedMIMETypes) <= 0) then begin
      raise EMimeTypeError.CreateFmt('Неожиданный тип %s', [AContentType]);
    end;
  end;
end;

type
  ESameTileSize = class(Exception);

procedure TTileDownloaderBase.CheckTargetSize(
  AFileHandle: HInternet;
  ACheckTileSize: Boolean;
  AExistsFileSize: Cardinal
);
var
  VBufSize: Cardinal;
  dwIndex: Cardinal;
  VLastError: Cardinal;
  VContentLen: Cardinal;
begin
  if ACheckTileSize then begin
    dwIndex := 0;
    VBufSize := sizeof(VContentLen);
    if HttpQueryInfo(AFileHandle, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @VContentLen, VBufSize, dwIndex) then begin
      if VContentLen = AExistsFileSize then begin
        raise ESameTileSize.Create('Одинаковый размер тайла');
      end;
    end else begin
      VLastError := GetLastError;
      if VLastError <> ERROR_HTTP_HEADER_NOT_FOUND then begin
        RaiseLastOSError(VLastError);
      end;
    end;
  end;
end;

procedure TTileDownloaderBase.GetResponsHead(AFileHandle: HInternet;
  var AResponseHead: string);
var
  VBufSize: Cardinal;
  dwIndex: Cardinal;
  VLastError: Cardinal;
begin
  try
    VBufSize := 1024;
    SetLength(AResponseHead, VBufSize);
    FillChar(AResponseHead[1], VBufSize, 0);
    dwIndex := 0;
    if not HttpQueryInfo(AFileHandle, HTTP_QUERY_RAW_HEADERS_CRLF, @AResponseHead[1], VBufSize, dwIndex) then begin
      VLastError := GetLastError;
      if VLastError = ERROR_INSUFFICIENT_BUFFER then begin
        SetLength(AResponseHead, VBufSize);
        FillChar(AResponseHead[1], VBufSize, 0);
        dwIndex := 0;
        if not HttpQueryInfo(AFileHandle, HTTP_QUERY_RAW_HEADERS_CRLF, @AResponseHead[1], VBufSize, dwIndex) then begin
          AResponseHead := ''
        end else begin
          SetLength(AResponseHead, VBufSize);
        end;
      end else begin
        AResponseHead := '';
      end;
    end else begin
      SetLength(AResponseHead, VBufSize);
    end;
  except
    AResponseHead := '';
  end;
end;

procedure TTileDownloaderBase.ProcessDataRequest(AFileHandle: HInternet;
  ACheckTileSize: Boolean; AExistsFileSize: Cardinal;
  fileBuf: TMemoryStream; out AContentType: string; var AResponseHead: string);
begin
  GetResponsHead(AFileHandle, AResponseHead);
  CheckContentType(AFileHandle, AContentType);
  CheckTargetSize(AFileHandle, ACheckTileSize, AExistsFileSize);
  GetData(AFileHandle, fileBuf);
end;

procedure TTileDownloaderBase.ResetConnetction;
begin
  CloseSession;
  Sleep(FConfigStatic.SleepOnResetConnection);
  OpenSession;
end;

procedure TTileDownloaderBase.ResetGlobalOffline(ASessionHandle: HInternet);
var
  ci: INTERNET_CONNECTED_INFO;
begin
  if IsGlobalOffline(ASessionHandle) then begin
    ci.dwConnectedState := INTERNET_STATE_CONNECTED;
    InternetSetOption(ASessionHandle, INTERNET_OPTION_CONNECTED_STATE, @ci, SizeOf(ci));
  end;
end;

function TTileDownloaderBase.IsGlobalOffline(ASessionHandle: HInternet): Boolean;
var
  State, Size: DWORD;
begin
  Result := False;
  State := 0;
  Size := SizeOf(DWORD);
  if InternetQueryOption(ASessionHandle, INTERNET_OPTION_CONNECTED_STATE, @State, Size) then begin
    if (State and INTERNET_STATE_DISCONNECTED_BY_USER) <> 0 then begin
      Result := True;
    end;
  end;
end;

type
  EProxyAuthError = class(Exception);

function TTileDownloaderBase.GetStatusCode(AFileHandle: HInternet): Cardinal;
var
  dwIndex: Cardinal;
  VBufSize: Cardinal;
begin
  VBufSize := sizeof(Result);
  dwIndex := 0;
  if not HttpQueryInfo(AFileHandle, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @Result, VBufSize, dwIndex) then begin
    RaiseLastOSError;
  end;
end;

procedure TTileDownloaderBase.ProxyAuth(
  AProxyConfig: IProxyConfigStatic;
  AFileHandle: HInternet;
  out AStatusCode: Cardinal
);
var
  VLogin, VPassword: string;
begin
  if AProxyConfig.UseIESettings or
    not AProxyConfig.UseProxy or
    not AProxyConfig.UseLogin
  then begin
    raise EProxyAuthError.Create('Настройки не предусматривают авторизацию на прокси');
  end;
  VLogin := AProxyConfig.Login;
  VPassword := AProxyConfig.Password;
  InternetSetOption(AFileHandle, INTERNET_OPTION_PROXY_USERNAME, PChar(VLogin), length(VLogin));
  InternetSetOption(AFileHandle, INTERNET_OPTION_PROXY_PASSWORD, PChar(VPassword), length(VPassword));
  HttpSendRequest(AFileHandle, nil, 0, Nil, 0);

  AStatusCode := GetStatusCode(AFileHandle);
  if AStatusCode = HTTP_STATUS_PROXY_AUTH_REQ then begin
    raise EProxyAuthError.Create('Ошибка авторизации на прокси');
  end;
end;

function TTileDownloaderBase.TryDownload(
  AConfig: ITileDownloaderConfigStatic;
  AUrl, ARequestHead: string;
  ACheckTileSize: Boolean;
  AExistsFileSize: Cardinal;
  fileBuf: TMemoryStream;
  out AServerExists: Boolean;
  out AStatusCode: Cardinal;
  out AContentType, AResponseHead: string
): TDownloadTileResult;
var
  VFileHandle: HInternet;
  VHeader: String;
  VProxyConfig: IProxyConfigStatic;
  VSessionHandle: HInternet;
begin
  AServerExists := False;
  VProxyConfig := AConfig.ProxyConfigStatic;
  FSessionCS.Acquire;
  try
    VSessionHandle := FSessionHandle;
  finally
    FSessionCS.Release;
  end;

  VHeader := BuildHeader(AUrl, ARequestHead);
  try
    VFileHandle := InternetOpenURL(VSessionHandle, PChar(AURL), PChar(VHeader), length(VHeader),
      INTERNET_FLAG_NO_CACHE_WRITE or
      INTERNET_FLAG_RELOAD or
      INTERNET_FLAG_IGNORE_CERT_CN_INVALID or
      INTERNET_FLAG_IGNORE_CERT_DATE_INVALID or
      INTERNET_FLAG_NO_COOKIES, { no automatic cookie handling }
      0);
    if not Assigned(VFileHandle) then begin
      RaiseLastOSError;
    end;
    try
      try
        AStatusCode := GetStatusCode(VFileHandle);
        if AStatusCode = HTTP_STATUS_PROXY_AUTH_REQ then begin
          ProxyAuth(VProxyConfig, VFileHandle, AStatusCode);
        end;
        if IsOkStatus(AStatusCode) then begin
          AServerExists := True;
          try
            ProcessDataRequest(VFileHandle, ACheckTileSize, AExistsFileSize, fileBuf, AContentType, AResponseHead);
          except
            on E: EMimeTypeError do begin
              Result := dtrErrorMIMEType;
            end;
            on E: ESameTileSize do begin
              Result := dtrSameTileSize;
            end;
          end;
          if fileBuf.Size = 0 then begin
            Result := dtrTileNotExists;
          end else begin
            Result := dtrOK;
          end;
        end else if IsDownloadErrorStatus(AStatusCode) then begin
          Result := dtrDownloadError;
        end else if IsTileNotExistStatus(AStatusCode) then begin
          Result := dtrTileNotExists;
        end else begin
          Result := dtrUnknownError;
        end;
      except
        on E: EOSError do begin
          if IsDownloadError(E.ErrorCode) then begin
            Result := dtrDownloadError;
          end else begin
            Result := dtrUnknownError;
          end;
        end;
        on E: EProxyAuthError do begin
          Result := dtrProxyAuthError;
        end;
      end;
    finally
      InternetCloseHandle(VFileHandle);
    end;
  except
    on E: EOSError do begin
      if IsDownloadError(E.ErrorCode) then begin
        Result := dtrDownloadError;
      end else begin
        Result := dtrErrorInternetOpenURL;
      end;
    end;
  end;
end;

end.

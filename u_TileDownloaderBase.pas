unit u_TileDownloaderBase;

interface

uses
  Windows,
  WinInet,
  SyncObjs,
  Classes,
  i_ProxySettings,
  i_TileDownlodSession;

type
  TTileDownloaderBase = class(TInterfacedObject, ITileDownlodSession)
  private
    FExpectedMIMETypes: string;
    FDefaultMIMEType: string;
    FIgnoreMIMEType: Boolean;
    FDownloadTryCount: Integer;
    FConnectionSettings: IInetConfig;
    FSleepOnResetConnection: Cardinal;
    FWaitInterval: Cardinal;
    FUserAgentString: string;
  protected
    FSessionHandle: HInternet;
    FSessionOpenError: Cardinal;
    FCS: TCriticalSection;
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
    function TryDownload(AUrl: string; var AHead: string; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AStatusCode: Cardinal; out AContentType: string): TDownloadTileResult; virtual;
    function ProcessDataRequest(AFileHandle: HInternet; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AContentType: string; out AResponseHead: string): TDownloadTileResult; virtual;
    function GetData(AFileHandle: HInternet; fileBuf: TMemoryStream): TDownloadTileResult; virtual;
    function IsGlobalOffline: Boolean;
  public
    constructor Create(AIgnoreMIMEType: Boolean; AExpectedMIMETypes, ADefaultMIMEType: string; ADownloadTryCount: Integer; AConnectionSettings: IInetConfig);
    destructor Destroy; override;
    function DownloadTile(AUrl: string; var AHead: string; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AStatusCode: Cardinal; out AContentType: string): TDownloadTileResult; virtual;
    property SleepOnResetConnection: Cardinal read FSleepOnResetConnection write FSleepOnResetConnection;
    property ExpectedMIMETypes: string read FExpectedMIMETypes write FExpectedMIMETypes;
    property WaitInterval: Cardinal read FWaitInterval write FWaitInterval;
  end;

implementation

uses
  SysUtils;

{ TTileDownloaderBase }

function TTileDownloaderBase.BuildHeader(AUrl, AHead: string): string;
begin
  Result := AHead;
end;

procedure TTileDownloaderBase.CloseSession;
begin
  if Assigned(FSessionHandle) then begin
    InternetCloseHandle(FSessionHandle);
  end;
end;

constructor TTileDownloaderBase.Create(AIgnoreMIMEType: Boolean;
  AExpectedMIMETypes, ADefaultMIMEType: string;
  ADownloadTryCount: Integer; AConnectionSettings: IInetConfig);
begin
  FIgnoreMIMEType := AIgnoreMIMEType;
  FDefaultMIMEType := ADefaultMIMEType;
  FExpectedMIMETypes := AExpectedMIMETypes;
  FDownloadTryCount := ADownloadTryCount;
  FConnectionSettings := AConnectionSettings;
  FCS := TCriticalSection.Create;
  FUserAgentString := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)';
  FSleepOnResetConnection := 200;
  FLastDownloadResult := dtrOK;
  OpenSession;
end;

destructor TTileDownloaderBase.Destroy;
begin
  FreeAndNil(FCS);
  CloseSession;
  inherited;
end;

function TTileDownloaderBase.DownloadTile(AUrl: string; var AHead: string;
  ACheckTileSize: Boolean; AExistsFileSize: Cardinal;
  fileBuf: TMemoryStream; out AStatusCode: Cardinal;
  out AContentType: string): TDownloadTileResult;
var
  VTryCount: Integer;
  VHead: string;
begin
  if not Assigned(FSessionHandle) then begin
    Result := dtrErrorInternetOpen;
    exit;
  end;
  FCS.Acquire;
  try
    VHead := AHead;
    VTryCount := 0;
    Result := FLastDownloadResult;
    repeat
      if Result = dtrDownloadError then begin
        ResetConnetction;
      end;
      Result := TryDownload(AUrl, VHead, ACheckTileSize, AExistsFileSize, fileBuf, AStatusCode, AContentType);
      Inc(VTryCount);
    until (Result <> dtrDownloadError) or (VTryCount >= FDownloadTryCount);
    FLastDownloadResult := Result;
    AHead := VHead;
  finally
    FCS.Release;
  end;
end;

function TTileDownloaderBase.GetData(AFileHandle: HInternet;
  fileBuf: TMemoryStream): TDownloadTileResult;
var
  VBuffer: array [1..64535] of Byte;
  VBufferLen: LongWord;
  VLastError: Cardinal;
begin
  repeat
    if InternetReadFile(AFileHandle, @VBuffer, SizeOf(VBuffer), VBufferLen) then begin
      filebuf.Write(VBuffer, VBufferLen);
    end else begin
      VLastError := GetLastError;
      if IsDownloadError(VLastError) then begin
        Result := dtrDownloadError;
      end else begin
        Result := dtrUnknownError;
        Assert(False, 'Неизвестная ошибка при получении данных. Код ошибки ' + IntToStr(VLastError));
      end;
      Exit;
    end;
  until (VBufferLen = 0);
  Result := dtrOK;
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

procedure TTileDownloaderBase.OpenSession;
var
  VTimeOut: DWORD;
begin
  FSessionHandle := InternetOpen(pChar(FUserAgentString), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if Assigned(FSessionHandle) then begin
    FSessionOpenError := 0;
    VTimeOut := FConnectionSettings.GetTimeOut;
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
  end else begin
    FSessionOpenError := GetLastError;
  end;
end;

function TTileDownloaderBase.ProcessDataRequest(AFileHandle: HInternet;
  ACheckTileSize: Boolean; AExistsFileSize: Cardinal;
  fileBuf: TMemoryStream; out AContentType: string; out AResponseHead: string): TDownloadTileResult;
var
  VBufSize: Cardinal;
  dwIndex: Cardinal;
  VContentLen: Cardinal;
  VLastError: Cardinal;
begin
  try
    AResponseHead := '';
    VBufSize := 10;
    SetLength(AResponseHead, VBufSize);
    FillChar(AResponseHead[1], VBufSize, 0);
    dwIndex := 0;
    if not HttpQueryInfo(AFileHandle, HTTP_QUERY_RAW_HEADERS_CRLF, @AResponseHead[1], VBufSize, dwIndex) then begin
      VLastError := GetLastError;
      if VLastError = ERROR_INSUFFICIENT_BUFFER then begin
        SetLength(AResponseHead, VBufSize);
        if not HttpQueryInfo(AFileHandle, HTTP_QUERY_RAW_HEADERS_CRLF, @AResponseHead[1], VBufSize, dwIndex) then
          AResponseHead := ''
        else SetLength(AResponseHead, VBufSize);
      end else AResponseHead := '';
    end;
  except
  end;
  if FIgnoreMIMEType then begin
    AContentType := FDefaultMIMEType;
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
          VLastError := GetLastError;
          Result := dtrUnknownError;
          Assert(False, 'Неизвестная ошибка при закачке. Код ошибки ' + IntToStr(VLastError));
          exit;
        end;
      end else if VLastError = ERROR_HTTP_HEADER_NOT_FOUND then begin
        AContentType := '';
      end else begin
        Result := dtrUnknownError;
        Assert(False, 'Неизвестная ошибка при закачке. Код ошибки ' + IntToStr(VLastError));
        exit;
      end;
    end;
    AContentType := trim(AContentType);
    if (AContentType = '') then begin
      AContentType := FDefaultMIMEType;
    end else if (Pos(AContentType, FExpectedMIMETypes) <= 0) then begin
      Result := dtrErrorMIMEType;
      exit;
    end;
  end;
  if ACheckTileSize then begin
    dwIndex := 0;
    VBufSize := sizeof(VContentLen);
    if HttpQueryInfo(AFileHandle, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @VContentLen, VBufSize, dwIndex) then begin
      if VContentLen = AExistsFileSize then begin
        Result := dtrSameTileSize;
        Exit;
      end;
    end else begin
      VLastError := GetLastError;
      Assert(False, 'Неизвестная ошибка при получении размера. Код ошибки ' + IntToStr(VLastError));
    end;
  end;
  Result := GetData(AFileHandle, fileBuf);
  if (Result = dtrOK) and (fileBuf.Size = 0) then begin
    Result := dtrTileNotExists;
  end;
end;

procedure TTileDownloaderBase.ResetConnetction;
begin
  CloseSession;
  Sleep(FSleepOnResetConnection);
  OpenSession;
end;

function TTileDownloaderBase.IsGlobalOffline: Boolean;
var
  State, Size: DWORD;
begin
  Result := False;
  State := 0;
  Size := SizeOf(DWORD);
  if InternetQueryOption(nil, INTERNET_OPTION_CONNECTED_STATE, @State, Size) then begin
    if (State and INTERNET_STATE_DISCONNECTED_BY_USER) <> 0 then begin
      Result := True;
    end;
  end;
end;

function TTileDownloaderBase.TryDownload(AUrl: string; var AHead: string;
  ACheckTileSize: Boolean; AExistsFileSize: Cardinal;
  fileBuf: TMemoryStream; out AStatusCode: Cardinal;
  out AContentType: string): TDownloadTileResult;
var
  VFileHandle: HInternet;
  VHeader: String;
  VBufSize: Cardinal;
  dwIndex: Cardinal;
  VLastError: Cardinal;
  ci: INTERNET_CONNECTED_INFO;
  VNow: Cardinal;
  VProxyConfig: IProxyConfig;
  VUselogin: Boolean;
  VLogin: string;
  VPassword: string;
begin
  VProxyConfig := FConnectionSettings.ProxyConfig;
  VProxyConfig.LockRead;
  try
    VUselogin := (not VProxyConfig.GetUseIESettings) and VProxyConfig.GetUseProxy and VProxyConfig.GetUseLogin;
    VLogin := VProxyConfig.GetLogin;
    VPassword := VProxyConfig.GetPassword;
  finally
    VProxyConfig.UnlockRead;
  end;
  VHeader := BuildHeader(AUrl, AHead);
  if IsGlobalOffline then begin
    ci.dwConnectedState := INTERNET_STATE_CONNECTED;
    InternetSetOption(FSessionHandle, INTERNET_OPTION_CONNECTED_STATE, @ci, SizeOf(ci));
  end;
  VNow := GetTickCount;
  if VNow < FLastDownloadTime + FWaitInterval then begin
    Sleep(FWaitInterval);
  end;
  VFileHandle := InternetOpenURL(FSessionHandle, PChar(AURL), PChar(VHeader), length(VHeader),
    INTERNET_FLAG_NO_CACHE_WRITE or
    INTERNET_FLAG_RELOAD or
    INTERNET_FLAG_IGNORE_CERT_CN_INVALID or
    INTERNET_FLAG_IGNORE_CERT_DATE_INVALID,
    0);
  if not Assigned(VFileHandle) then begin
    VLastError := GetLastError;
    if IsDownloadError(VLastError) then begin
      Result := dtrDownloadError;
    end else begin
      Result := dtrErrorInternetOpenURL;
      if VLastError <> ERROR_INTERNET_INVALID_CA then begin
        Assert(False, 'Неизвестная ошибка при открытии соединения. Код ошибки ' + IntToStr(VLastError));
      end else begin
        //Что бы нормально обрабатывать ситуацию нужно полностью переделать закачку.
      end;
    end;
    exit;
  end;
  try
    VBufSize := sizeof(AStatusCode);
    dwIndex := 0;
    if not HttpQueryInfo(VFileHandle, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @AStatusCode, VBufSize, dwIndex) then begin
      VLastError := GetLastError;
      if IsDownloadError(VLastError) then begin
        Result := dtrDownloadError;
      end else begin
        Result := dtrUnknownError;
        Assert(False, 'Неизвестная ошибка при закачке. Код ошибки ' + IntToStr(VLastError));
      end;
      Exit;
    end;
    if AStatusCode = HTTP_STATUS_PROXY_AUTH_REQ then begin
      if VUselogin then begin
        InternetSetOption(VFileHandle, INTERNET_OPTION_PROXY_USERNAME, PChar(VLogin), length(VLogin));
        InternetSetOption(VFileHandle, INTERNET_OPTION_PROXY_PASSWORD, PChar(VPassword), length(VPassword));
        HttpSendRequest(VFileHandle, nil, 0, Nil, 0);

        dwIndex := 0;
        VBufSize := sizeof(AStatusCode);
        if not HttpQueryInfo(VFileHandle, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @AStatusCode, VBufSize, dwIndex) then begin
          VLastError := GetLastError;
          if IsDownloadError(VLastError) then begin
            Result := dtrDownloadError;
          end else begin
            Result := dtrUnknownError;
            Assert(False, 'Неизвестная ошибка при закачке. Код ошибки ' + IntToStr(VLastError));
          end;
          exit;
        end;
        if AStatusCode = HTTP_STATUS_PROXY_AUTH_REQ then begin
          Result := dtrProxyAuthError;
          exit;
        end;
      end else begin
        Result := dtrProxyAuthError;
        Exit;
      end;
    end;
    if IsOkStatus(AStatusCode) then begin
      Result := ProcessDataRequest(VFileHandle, ACheckTileSize, AExistsFileSize, fileBuf, AContentType, AHead);
    end else if IsDownloadErrorStatus(AStatusCode) then begin
      Result := dtrDownloadError;
    end else if IsTileNotExistStatus(AStatusCode) then begin
      Result := dtrTileNotExists;
    end else begin
      Result := dtrUnknownError;
    end;
    FLastDownloadTime := GetTickCount;
  finally
    InternetCloseHandle(VFileHandle);
  end;
end;

end.

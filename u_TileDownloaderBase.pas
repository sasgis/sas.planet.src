unit u_TileDownloaderBase;

interface

uses
  Windows,
  WinInet,
  SyncObjs,
  Classes,
  t_CommonTypes,
  urlMon;

type
  TDownloadTileResult = (dtrOK, dtrSameTileSize, dtrErrorInternetOpen, dtrErrorInternetOpenURL, dtrProxyAuthError, dtrErrorMIMEType, dtrDownloadError, dtrTileNotExists, dtrBanError, dtrUnknownError);

  TTileDownloaderBase = class
  protected
    FExpectedMIMETypes: string;
    FDownloadTryCount: Integer;
    FConnectionSettings: TInetConnect;
    FUserAgentString: string;
    FSessionHandle: HInternet;
    FSessionOpenError: Cardinal;
    FCS: TCriticalSection;
    FSleepOnResetConnection: Cardinal;
    function IsDownloadError(ALastError: Cardinal): Boolean; virtual;
    function IsOkStatus(AStatusCode: Cardinal): Boolean; virtual;
    function IsDownloadErrorStatus(AStatusCode: Cardinal): Boolean; virtual;
    function IsTileNotExistStatus(AStatusCode: Cardinal): Boolean; virtual;
    procedure ResetConnetction; virtual;
    procedure OpenSession; virtual;
    procedure CloseSession; virtual;
    function BuildHeader(AUrl: string): string; virtual;
    function TryDownload(AUrl: string; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AStatusCode: Cardinal; out AContentType: string): TDownloadTileResult; virtual;
    function ProcessDataRequest(AFileHandle: HInternet; ACheckTileSize: Boolean; AExistsFileSize: Cardinal;  fileBuf: TMemoryStream; out AContentType: string): TDownloadTileResult; virtual;
    function GetData(AFileHandle: HInternet; fileBuf: TMemoryStream): TDownloadTileResult; virtual;
    function IsGlobalOffline: Boolean;
  public
    constructor Create(AExpectedMIMETypes: string; ADownloadTryCount: Integer; AConnectionSettings: TInetConnect);
    destructor Destroy; override;
    function DownloadTile(AUrl: string; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AStatusCode: Cardinal; out AContentType: string): TDownloadTileResult; virtual;
    property SleepOnResetConnection: Cardinal read FSleepOnResetConnection write FSleepOnResetConnection;
    property ExpectedMIMETypes: string read FExpectedMIMETypes write FExpectedMIMETypes; 
  end;

implementation

uses
  u_GlobalState,
  StrUtils,
  SysUtils;

{ TTileDownloaderBase }

function TTileDownloaderBase.BuildHeader(AUrl: string): string;
begin
  Result := '';
end;

procedure TTileDownloaderBase.CloseSession;
begin
  if Assigned(FSessionHandle) then begin
    InternetCloseHandle(FSessionHandle);
  end;
end;

constructor TTileDownloaderBase.Create(AExpectedMIMETypes: string;
  ADownloadTryCount: Integer; AConnectionSettings: TInetConnect);
begin
  FExpectedMIMETypes := AExpectedMIMETypes;
  FDownloadTryCount := ADownloadTryCount;
  FConnectionSettings := AConnectionSettings;
  FCS := TCriticalSection.Create;
  FUserAgentString := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)';
  FSleepOnResetConnection := 200;
  OpenSession;
end;

destructor TTileDownloaderBase.Destroy;
begin
  FreeAndNil(FCS);
  CloseSession;
  inherited;
end;

function TTileDownloaderBase.DownloadTile(AUrl: string;
  ACheckTileSize: Boolean; AExistsFileSize: Cardinal;
  fileBuf: TMemoryStream; out AStatusCode: Cardinal;
  out AContentType: string): TDownloadTileResult;
var
  VTryCount: Integer;
begin
  if not Assigned(FSessionHandle) then begin
    Result := dtrErrorInternetOpen;
    exit;
  end;
  Result := dtrOK;
  FCS.Acquire;
  try
    VTryCount := 0;
    repeat
      if Result = dtrDownloadError then begin
        ResetConnetction;
      end;
      Result := TryDownload(AUrl, ACheckTileSize, AExistsFileSize, fileBuf, AStatusCode, AContentType);
      Inc(VTryCount);
    until (Result <> dtrDownloadError) or (VTryCount >= FDownloadTryCount);
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
      end;
      Exit;
    end;
  until (VBufferLen=0);
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
    ERROR_INTERNET_TIMEOUT: begin
      Result := true;
    end;
    else
      Result := false;
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
    HTTP_STATUS_GATEWAY_TIMEOUT: begin
      Result := True;
    end;
    else
      Result := False;
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
    HTTP_STATUS_PARTIAL_CONTENT: begin
      Result := True;
    end;
    else
      Result := False;
  end;
end;

function TTileDownloaderBase.IsTileNotExistStatus(
  AStatusCode: Cardinal): Boolean;
begin
  case AStatusCode of
    HTTP_STATUS_NO_CONTENT,
    HTTP_STATUS_NOT_FOUND: begin
      Result := True;
    end;
    else
      Result := False;
  end;
end;

procedure TTileDownloaderBase.OpenSession;
var VTimeOut: DWORD;
begin
  FSessionHandle := InternetOpen(pChar(FUserAgentString), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if Assigned(FSessionHandle) then begin
    FSessionOpenError := 0;
    VTimeOut:=GState.InetConnect.TimeOut;
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
  fileBuf: TMemoryStream; out AContentType: string): TDownloadTileResult;
var
  VBufSize: Cardinal;
  dwIndex: Cardinal;
  VContentLen: Cardinal;
begin
  VBufSize := Length(AContentType);
  if VBufSize = 0 then begin
    SetLength(AContentType, 20);
    VBufSize := Length(AContentType);
  end;
  FillChar(AContentType[1], VBufSize, 0);
  dwIndex := 0;
  if not HttpQueryInfo(AFileHandle, HTTP_QUERY_CONTENT_TYPE, @AContentType[1], VBufSize, dwIndex) then begin
    if GetLastError() = ERROR_INSUFFICIENT_BUFFER then begin
      SetLength(AContentType, VBufSize);
      if not HttpQueryInfo(AFileHandle, HTTP_QUERY_CONTENT_TYPE, @AContentType[1], VBufSize, dwIndex) then begin
        Result := dtrUnknownError;
        exit;
      end;
    end else begin
      Result := dtrUnknownError;
      exit;
    end;
  end;
  AContentType := trim(AContentType);
  if (AContentType = '') or (PosEx(AContentType, FExpectedMIMETypes, 0)<=0) then begin
    Result := dtrErrorMIMEType;
    exit;
  end;
  if ACheckTileSize then begin
    dwIndex := 0;
    VBufSize := sizeof(VContentLen);
    if HttpQueryInfo(AFileHandle, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @VContentLen, VBufSize, dwIndex) then begin
      if VContentLen = AExistsFileSize then begin
        Result := dtrSameTileSize;
        Exit;
      end;
    end;
  end;
  Result := GetData(AFileHandle, fileBuf);
end;

procedure TTileDownloaderBase.ResetConnetction;
begin
  CloseSession;
  Sleep(FSleepOnResetConnection);
  OpenSession;
end;

function TTileDownloaderBase.IsGlobalOffline: Boolean;
var State, Size: DWORD;
begin
  Result:=False;
  State:=0;
  Size:=SizeOf(DWORD);
  if InternetQueryOption(nil, INTERNET_OPTION_CONNECTED_STATE, @State, Size) then
    if (State and INTERNET_STATE_DISCONNECTED_BY_USER) <> 0 then Result := True;
end;

function TTileDownloaderBase.TryDownload(AUrl: string;
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
begin
  VHeader := BuildHeader(AUrl);
  if IsGlobalOffline then begin
    ci.dwConnectedState := INTERNET_STATE_CONNECTED;
    InternetSetOption(FSessionHandle, INTERNET_OPTION_CONNECTED_STATE, @ci, SizeOf(ci));
  end;
  VFileHandle := InternetOpenURL(FSessionHandle, PChar(AURL), PChar(VHeader), length(VHeader), INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_RELOAD , 0);
  if not Assigned(VFileHandle) then begin
    Result := dtrErrorInternetOpenURL;
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
      end;
      Exit;
    end;
    if AStatusCode = HTTP_STATUS_PROXY_AUTH_REQ then begin
      if (not FConnectionSettings.userwinset)and(FConnectionSettings.uselogin) then begin
        InternetSetOption (VFileHandle, INTERNET_OPTION_PROXY_USERNAME,PChar(FConnectionSettings.loginstr), length(FConnectionSettings.loginstr));
        InternetSetOption (VFileHandle, INTERNET_OPTION_PROXY_PASSWORD,PChar(FConnectionSettings.passstr), length(FConnectionSettings.Passstr));
        HttpSendRequest(VFileHandle, nil, 0,Nil, 0);

        dwIndex := 0;
        VBufSize := sizeof(AStatusCode);
        if not HttpQueryInfo(VFileHandle, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @AStatusCode, VBufSize, dwIndex) then begin
          Result := dtrUnknownError;
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
      Result := ProcessDataRequest(VFileHandle, ACheckTileSize, AExistsFileSize, fileBuf, AContentType);
    end else if IsDownloadErrorStatus(AStatusCode) then begin
      Result := dtrDownloadError;
    end else if IsTileNotExistStatus(AStatusCode) then begin
      Result := dtrTileNotExists;
    end else begin
      Result := dtrUnknownError;
    end;
  finally
    InternetCloseHandle(VFileHandle);
  end;
end;

end.

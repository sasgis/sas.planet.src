unit u_TileDownloaderBase;

interface

uses
  Windows,
  WinInet,
  SyncObjs,
  Classes,
  u_GlobalState;

type
  TDownloadTileResult = (dtrOK, dtrSameTileSize, dtrErrorInternetOpen, dtrErrorInternetOpenURL, dtrProxyAuthError, dtrErrorMIMEType, dtrDownloadError, dtrUnknownError);

  TTileDownloaderBase = class
  protected
    FExpectedMIMETypes: string;
    FDownloadTryCount: Integer;
    FConnectionSettings: TInetConnect;
    FSessionHandle: HInternet;
    FSessionOpenError: Cardinal;
    FCS: TCriticalSection;
    function BuildHeader(AUrl: string): string; virtual;
    function TryDownload(AUrl: string; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AStatusCode: Cardinal; out AContentType: string): TDownloadTileResult; virtual;
    function ProcessDataRequest(AFileHandle: HInternet; ACheckTileSize: Boolean; AExistsFileSize: Cardinal;  fileBuf: TMemoryStream; out AContentType: string): TDownloadTileResult; virtual;
    function GetData(AFileHandle: HInternet; fileBuf: TMemoryStream): TDownloadTileResult; virtual;
  public
    constructor Create(AExpectedMIMETypes: string; ADownloadTryCount: Integer; AConnectionSettings: TInetConnect);
    destructor Destroy; override;
    function DownloadTile(AUrl: string; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; fileBuf: TMemoryStream; out AStatusCode: Cardinal; out AContentType: string): TDownloadTileResult; virtual;
  end;

implementation

uses
  StrUtils,
  SysUtils;

{ TTileDownloaderBase }

function TTileDownloaderBase.BuildHeader(AUrl: string): string;
begin
  Result := '';
end;

constructor TTileDownloaderBase.Create(AExpectedMIMETypes: string;
  ADownloadTryCount: Integer; AConnectionSettings: TInetConnect);
begin
  FExpectedMIMETypes := AExpectedMIMETypes;
  FDownloadTryCount := ADownloadTryCount;
  FConnectionSettings := AConnectionSettings;
  FCS := TCriticalSection.Create;
  FSessionHandle := InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if Assigned(FSessionHandle) then begin
    FSessionOpenError := 0;
  end else begin
    FSessionOpenError := GetLastError;
  end;
end;

destructor TTileDownloaderBase.Destroy;
begin
  FreeAndNil(FCS);
  if Assigned(FSessionHandle) then begin
    InternetCloseHandle(FSessionHandle);
  end;
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
  FCS.Acquire;
  try
    VTryCount := 0;
    repeat
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
  VBuffer:array [1..64535] of Byte;
  VBufferLen:LongWord;
begin
  repeat
    if InternetReadFile(AFileHandle, @VBuffer, SizeOf(VBuffer), VBufferLen) then begin
      filebuf.Write(VBuffer, VBufferLen);
    end else begin
      Result := dtrDownloadError;
      Exit;
    end;
  until (VBufferLen=0);
  Result := dtrOK;
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
  if (AContentType = '') or (PosEx(AContentType, FExpectedMIMETypes, 0)>0) then begin
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

function TTileDownloaderBase.TryDownload(AUrl: string;
  ACheckTileSize: Boolean; AExistsFileSize: Cardinal;
  fileBuf: TMemoryStream; out AStatusCode: Cardinal;
  out AContentType: string): TDownloadTileResult;
var
  VFileHandle: HInternet;
  VHeader: String;
  VBufSize: Cardinal;
  dwIndex: Cardinal;
begin
  VHeader := BuildHeader(AUrl);
  VFileHandle := InternetOpenURL(FSessionHandle, PChar(AURL), PChar(VHeader), length(VHeader), INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_RELOAD, 0);
  if  not Assigned(VFileHandle) then begin
    Result := dtrErrorInternetOpenURL;
    exit;
  end;
  try
    VBufSize := sizeof(AStatusCode);
    dwIndex := 0;
    if not HttpQueryInfo(VFileHandle, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @AStatusCode, VBufSize, dwIndex) then begin
      Result := dtrUnknownError;
      exit;
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
    case AStatusCode of
      200, 201, 202, 203, 205, 206: begin
        Result := ProcessDataRequest(VFileHandle, ACheckTileSize, AExistsFileSize, fileBuf, AContentType);
      end;
      500, 501, 502, 503, 504: begin
        Result := dtrDownloadError;
      end;
      else
        Result := dtrUnknownError;
    end;
  finally
    InternetCloseHandle(VFileHandle);
  end;
end;

end.

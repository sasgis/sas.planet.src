unit u_TileDownloaderBase;

interface

uses
  Windows,
  WinInet,
  SyncObjs,
  Classes,
  i_JclNotify,
  i_OperationNotifier,
  i_ProxySettings,
  i_DownloadChecker,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_InetConfig,
  i_TileDownloaderConfig,
  i_TileDownlodSession;

type
  TTileDownloaderBase = class(TInterfacedObject, ITileDownlodSession)
  private
    FConfig: ITileDownloaderConfig;
    FConfigStatic: ITileDownloaderConfigStatic;
    FConfigListener: IJclListener;
    FDownloadCancelListener: IJclListener;
    FWaitCancelListener: IJclListener;
    FCancelEvent: TEvent;
    procedure OnConfigChange(Sender: TObject);
    procedure OnDownloadCanceled(Sender: TObject);
    procedure OnWaitCanceled(Sender: TObject);
    procedure SleepCancelable(ATime: Cardinal);
  protected
    FSessionHandle: HInternet;
    FSessionCS: TCriticalSection;
    FDownloadMutex: TMutex;
    FLastDownloadTime: Cardinal;
    FWasConnectError: Boolean;
    function GetSleepTime(AConfig: ITileDownloaderConfigStatic): Cardinal; virtual;
    function IsConnectError(ALastError: Cardinal): Boolean; virtual;
    function IsDownloadError(ALastError: Cardinal): Boolean; virtual;
    function IsOkStatus(AStatusCode: Cardinal): Boolean; virtual;
    function IsDownloadErrorStatus(AStatusCode: Cardinal): Boolean; virtual;
    function IsTileNotExistStatus(AStatusCode: Cardinal): Boolean; virtual;
    function OpenSession(AConfig: IInetConfigStatic): HInternet; virtual;
    procedure CloseSession; virtual;
    function TryDownload(
      AConfig: IInetConfigStatic;
      AResultFactory: IDownloadResultFactory;
      ARequest: IDownloadRequest;
      ADownloadChecker: IDownloadChecker
    ): IDownloadResult;
    procedure GetData(AFileHandle: HInternet; fileBuf: TMemoryStream); virtual;
    procedure ResetGlobalOffline(ASessionHandle: HInternet);
    procedure GetContentType(
      AFileHandle: HInternet;
      var AContentType: string
    );
    function ProxyAuth(
      AResultFactory: IDownloadResultFactory;
      AProxyConfig: IProxyConfigStatic;
      AFileHandle: HInternet;
      out AStatusCode: Cardinal
    ): IDownloadResult;
    function GetStatusCode(AFileHandle: HInternet): Cardinal;
    procedure GetResponsHead(AFileHandle: HInternet; var AResponseHead: string);
  protected
    function DownloadTile(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      AResultFactory: IDownloadResultFactory;
      ARequest: IDownloadRequest;
      ADownloadChecker: IDownloadChecker
    ): IDownloadResult;
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
  FDownloadMutex := TMutex.Create;
  FSessionCS := TCriticalSection.Create;
  FCancelEvent := TEvent.Create;
  FWaitCancelListener := TNotifyEventListener.Create(Self.OnWaitCanceled);
  FDownloadCancelListener := TNotifyEventListener.Create(Self.OnDownloadCanceled);

  FConfigListener := TNotifyEventListener.Create(Self.OnConfigChange);
  FConfig.GetChangeNotifier.Add(FConfigListener);
  OnConfigChange(nil);

  FWasConnectError := False;
  FCancelEvent.ResetEvent;
end;

destructor TTileDownloaderBase.Destroy;
begin
  FCancelEvent.SetEvent;
  FConfig.GetChangeNotifier.Remove(FConfigListener);
  FConfigListener := nil;
  FConfig := nil;
  FDownloadCancelListener := nil;
  FWaitCancelListener := nil;
  FConfigStatic := nil;
  CloseSession;
  FreeAndNil(FDownloadMutex);
  FreeAndNil(FSessionCS);
  FreeAndNil(FCancelEvent);
  inherited;
end;

procedure TTileDownloaderBase.CloseSession;
begin
  FSessionCS.Acquire;
  try
    if Assigned(FSessionHandle) then begin
      InternetCloseHandle(FSessionHandle);
      FSessionHandle := nil;
    end;
  finally
    FSessionCS.Release;
  end;
end;

function TTileDownloaderBase.DownloadTile(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  AResultFactory: IDownloadResultFactory;
  ARequest: IDownloadRequest;
  ADownloadChecker: IDownloadChecker
): IDownloadResult;
var
  VTryNo: Integer;
  VDownloadTryCount: Integer;
  VConfig: ITileDownloaderConfigStatic;
  VSleepTime: Cardinal;

  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  Result := nil;
  VConfig := FConfigStatic;
  VDownloadTryCount := VConfig.InetConfigStatic.DownloadTryCount;

  if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    ACancelNotifier.AddListener(FWaitCancelListener);
    try
      VHandles[0] := FDownloadMutex.Handle;
      VHandles[1] := FCancelEvent.Handle;
      while Result = nil do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          FCancelEvent.ResetEvent;
          Result := AResultFactory.BuildCanceled;
        end else begin
          VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);
          case VWaitResult of
            WAIT_OBJECT_0: begin
              try
                VTryNo := 0;
                repeat
                  VSleepTime := GetSleepTime(VConfig);
                  if VSleepTime > 0 then begin
                    SleepCancelable(VSleepTime);
                  end;
                  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
                    Result := AResultFactory.BuildCanceled;
                    Exit;
                  end;
                  ACancelNotifier.AddListener(FDownloadCancelListener);
                  try
                    Result := TryDownload(VConfig.InetConfigStatic, AResultFactory, ARequest, ADownloadChecker);
                  finally
                    ACancelNotifier.RemoveListener(FDownloadCancelListener);
                  end;
                  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
                    Result := AResultFactory.BuildCanceled;
                    Exit;
                  end;
                  Inc(VTryNo);
                  FWasConnectError := not Result.IsServerExists;
                  FLastDownloadTime := GetTickCount;
                  if FWasConnectError then begin
                    CloseSession;
                  end;
                until (not FWasConnectError) or (VTryNo >= VDownloadTryCount);
              finally
                FDownloadMutex.Release;
              end;
            end;
          end;
        end;
      end;
    finally
      ACancelNotifier.RemoveListener(FWaitCancelListener);
    end;
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

function TTileDownloaderBase.IsConnectError(ALastError: Cardinal): Boolean;
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
      Result := true;
    end;
  else begin
    Result := false;
  end;
  end;
end;

function TTileDownloaderBase.IsDownloadError(
  ALastError: Cardinal): Boolean;
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
    FCancelEvent.SetEvent;
    CloseSession;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBase.OnDownloadCanceled(Sender: TObject);
begin
  FSessionCS.Acquire;
  try
    FCancelEvent.SetEvent;
    CloseSession;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBase.OnWaitCanceled(Sender: TObject);
begin
  FCancelEvent.SetEvent;
end;

function TTileDownloaderBase.OpenSession(AConfig: IInetConfigStatic): HInternet;
var
  VTimeOut: DWORD;
begin
  FSessionCS.Acquire;
  try
    if not Assigned(FSessionHandle) then begin
      FSessionHandle := InternetOpen(pChar(AConfig.UserAgentString), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
      if Assigned(FSessionHandle) then begin
        VTimeOut := AConfig.TimeOut;
        if not InternetSetOption(FSessionHandle, INTERNET_OPTION_CONNECT_TIMEOUT, @VTimeOut, sizeof(VTimeOut)) then begin
          RaiseLastOSError;
        end;
        if not InternetSetOption(FSessionHandle, INTERNET_OPTION_DATA_RECEIVE_TIMEOUT, @VTimeOut, sizeof(VTimeOut)) then begin
          RaiseLastOSError;
        end;
        if not InternetSetOption(FSessionHandle, INTERNET_OPTION_DATA_SEND_TIMEOUT, @VTimeOut, sizeof(VTimeOut)) then begin
          RaiseLastOSError;
        end;
        if not InternetSetOption(FSessionHandle, INTERNET_OPTION_SEND_TIMEOUT, @VTimeOut, sizeof(VTimeOut)) then begin
          RaiseLastOSError;
        end;
        if not InternetSetOption(FSessionHandle, INTERNET_OPTION_RECEIVE_TIMEOUT, @VTimeOut, sizeof(VTimeOut)) then begin
          RaiseLastOSError;
        end;
        ResetGlobalOffline(FSessionHandle);
      end else begin
        RaiseLastOSError;
      end;
    end;
    Result := FSessionHandle;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBase.GetContentType(AFileHandle: HInternet;
  var AContentType: string);
var
  VBufSize: Cardinal;
  dwIndex: Cardinal;
  VLastError: Cardinal;
begin
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

procedure TTileDownloaderBase.ResetGlobalOffline(ASessionHandle: HInternet);
var
  State, Size: DWORD;
  ci: INTERNET_CONNECTED_INFO;
begin
  State := 0;
  Size := SizeOf(DWORD);
  if InternetQueryOption(ASessionHandle, INTERNET_OPTION_CONNECTED_STATE, @State, Size) then begin
    if (State and INTERNET_STATE_DISCONNECTED_BY_USER) <> 0 then begin
      ci.dwConnectedState := INTERNET_STATE_CONNECTED;
      InternetSetOption(ASessionHandle, INTERNET_OPTION_CONNECTED_STATE, @ci, SizeOf(ci));
    end;
  end;
end;

procedure TTileDownloaderBase.SleepCancelable(ATime: Cardinal);
begin
  if  ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

function TTileDownloaderBase.GetSleepTime(AConfig: ITileDownloaderConfigStatic): Cardinal;
var
  VNow: Cardinal;
  VTimeFromLastDownload: Cardinal;
begin
  VNow := GetTickCount;
  if VNow >= FLastDownloadTime then begin
    VTimeFromLastDownload := VNow - FLastDownloadTime;
  end else begin
    VTimeFromLastDownload := MaxInt;
  end;
  Result := 0;
  if FWasConnectError then begin
    if VTimeFromLastDownload < AConfig.InetConfigStatic.SleepOnResetConnection then begin
      Result := AConfig.InetConfigStatic.SleepOnResetConnection - VTimeFromLastDownload;
    end;
  end else begin
    if VTimeFromLastDownload < AConfig.WaitInterval then begin
      Result := AConfig.WaitInterval - VTimeFromLastDownload;
    end;
  end;
end;

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

function TTileDownloaderBase.ProxyAuth(
  AResultFactory: IDownloadResultFactory;
  AProxyConfig: IProxyConfigStatic;
  AFileHandle: HInternet;
  out AStatusCode: Cardinal
): IDownloadResult;
var
  VLogin, VPassword: string;
begin
  if AProxyConfig.UseIESettings or
    not AProxyConfig.UseProxy or
    not AProxyConfig.UseLogin
  then begin
    Result := AResultFactory.BuildUnexpectedProxyAuth;
    Exit;
  end;
  VLogin := AProxyConfig.Login;
  VPassword := AProxyConfig.Password;
  InternetSetOption(AFileHandle, INTERNET_OPTION_PROXY_USERNAME, PChar(VLogin), length(VLogin));
  InternetSetOption(AFileHandle, INTERNET_OPTION_PROXY_PASSWORD, PChar(VPassword), length(VPassword));
  HttpSendRequest(AFileHandle, nil, 0, Nil, 0);

  AStatusCode := GetStatusCode(AFileHandle);
  if AStatusCode = HTTP_STATUS_PROXY_AUTH_REQ then begin
    Result := AResultFactory.BuildBadProxyAuth;
    Exit;
  end;
end;

function TTileDownloaderBase.TryDownload(
  AConfig: IInetConfigStatic;
  AResultFactory: IDownloadResultFactory;
  ARequest: IDownloadRequest;
  ADownloadChecker: IDownloadChecker
): IDownloadResult;
var
  VFileHandle: HInternet;
  VProxyConfig: IProxyConfigStatic;
  VSessionHandle: HInternet;
  VRecivedData: TMemoryStream;
  VStatusCode: Cardinal;
  VContentType, VResponseHead: string;
  VHeader: string;
begin
  VProxyConfig := AConfig.ProxyConfigStatic;
  try
    VSessionHandle := OpenSession(AConfig);
  except
    on E: EOSError do begin
      Result := AResultFactory.BuildNoConnetctToServerByErrorCode(E.ErrorCode);
      Exit;
    end;
  end;
  if Result <> nil then Exit;
  VHeader := ARequest.RequestHeader;
  if ADownloadChecker <> nil then begin
    Result := ADownloadChecker.BeforeRequest(ARequest.Url, VHeader);
    if Result <> nil then Exit;
  end;
  try
    VFileHandle :=
      InternetOpenURL(
        VSessionHandle,
        PChar(ARequest.URL),
        PChar(VHeader),
        Length(VHeader),
        INTERNET_FLAG_NO_CACHE_WRITE or
        INTERNET_FLAG_IGNORE_CERT_CN_INVALID or
        INTERNET_FLAG_IGNORE_CERT_DATE_INVALID or
        INTERNET_FLAG_NO_COOKIES, { no automatic cookie handling }
        0
      );
    if not Assigned(VFileHandle) then begin
      RaiseLastOSError;
    end;
    try
      VStatusCode := GetStatusCode(VFileHandle);
      if VStatusCode = HTTP_STATUS_PROXY_AUTH_REQ then begin
        Result := ProxyAuth(AResultFactory, VProxyConfig, VFileHandle, VStatusCode);
        if Result <> nil then Exit;
      end;
      GetResponsHead(VFileHandle, VResponseHead);
      if IsOkStatus(VStatusCode) then begin
        GetContentType(VFileHandle, VContentType);
        VRecivedData := TMemoryStream.Create;
        try
          if ADownloadChecker <> nil then begin
            Result := ADownloadChecker.AfterResponse(VStatusCode, VContentType, VResponseHead);
            if Result <> nil then Exit;
          end;
          GetData(VFileHandle, VRecivedData);
          if ADownloadChecker <> nil then begin
            Result := ADownloadChecker.AfterReciveData(VRecivedData.Size, VRecivedData.Memory, VStatusCode, VResponseHead);
            if Result <> nil then Exit;
          end;
          if VRecivedData.Size = 0 then begin
            Result := AResultFactory.BuildDataNotExistsZeroSize(VResponseHead);
            Exit;
          end;
          Result := AResultFactory.BuildOk(VStatusCode, VResponseHead, VContentType, VRecivedData.Size, VRecivedData.Memory);
        finally
          VRecivedData.Free;
        end;
      end else if IsDownloadErrorStatus(VStatusCode) then begin
        Result := AResultFactory.BuildLoadErrorByStatusCode(VStatusCode);
        Exit;
      end else if IsTileNotExistStatus(VStatusCode) then begin
        Result := AResultFactory.BuildDataNotExistsByStatusCode(VResponseHead, VStatusCode);
        Exit;
      end else begin
        Result := AResultFactory.BuildLoadErrorByUnknownStatusCode(VStatusCode);
        Exit;
      end;
    finally
      InternetCloseHandle(VFileHandle);
    end;
  except
    on E: EOSError do begin
      if IsConnectError(E.ErrorCode) then begin
        Result := AResultFactory.BuildNoConnetctToServerByErrorCode(E.ErrorCode)
      end else if IsDownloadError(E.ErrorCode) then begin
        Result := AResultFactory.BuildLoadErrorByErrorCode(E.ErrorCode)
      end else begin
        Result := AResultFactory.BuildNoConnetctToServerByErrorCode(E.ErrorCode)
      end;
    end;
  end;
end;

end.

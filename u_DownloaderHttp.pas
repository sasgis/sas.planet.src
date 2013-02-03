{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_DownloaderHttp;

interface

uses
  Windows,
  Classes,
  SysUtils,
  WinInet,
  ALHTTPCommon,
  ALHttpClient,
  ALWinInetHttpClient,
  i_Listener,
  i_NotifierOperation,
  i_Downloader,
  i_InetConfig,
  i_ProxySettings,
  i_DownloadResult,
  i_DownloadRequest,
  i_DownloadResultFactory,
  i_DownloadChecker,
  u_BaseInterfacedObject;

type
  THttpClientConfigRec = record
    HttpTimeOut: Cardinal;
    HeaderUserAgent: AnsiString;
    HeaderRawText: AnsiString;
    ProxyUseIESettings: Boolean;
    ProxyUseCustomSettings: Boolean;
    ProxyHost: AnsiString;
    ProxyUseLogin: Boolean;
    ProxyUserName: AnsiString;
    ProxyPassword: AnsiString;
  end;

  TDownloaderHttp = class(TBaseInterfacedObject, IDownloader)
  private
    FCS: IReadWriteSync;
    FCancelListener: IListener;
    FHttpClient: TALWinInetHTTPClient;
    FHttpResponseHeader: TALHTTPResponseHeader;
    FHttpResponseBody: TMemoryStream;
    FHttpClientLastConfig: THttpClientConfigRec;
    FResultFactory: IDownloadResultFactory;
    FDisconnectByServer: Byte;
    FDisconnectByUser: Byte;
    FOpeningHandle: HINTERNET;
    function InternalMakeResponse(
      const AResultFactory: IDownloadResultFactory;
      const ARequest: IDownloadRequest;
      var AStatusCode: Cardinal;
      var AContentType, ARawHeaderText: AnsiString;
      const ABody: TMemoryStream;
      const AHasOwned: PBoolean = nil
    ): IDownloadResult;
    function ProcessFileSystemRequest(
      const ARequest: IDownloadRequest;
      const AResultFactory: IDownloadResultFactory
    ): IDownloadResult;
    function OnBeforeRequest(
      const ARequest: IDownloadRequest;
      const AResultFactory: IDownloadResultFactory
    ): IDownloadResult;
    function OnOSError(
      const ARequest: IDownloadRequest;
      const AResultFactory: IDownloadResultFactory;
      AErrorCode: Cardinal
    ): IDownloadResult;
    function OnAfterResponse(
      const ARequest: IDownloadRequest;
      const AResultFactory: IDownloadResultFactory
    ): IDownloadResult;
    procedure PreConfigHttpClient(
      const ARawHttpRequestHeader: AnsiString;
      const AInetConfig: IInetConfigStatic
    );
    function IsConnectError(ALastError: Cardinal): Boolean;
    function IsDownloadError(ALastError: Cardinal): Boolean;
    function IsOkStatus(AStatusCode: Cardinal): Boolean;
    function IsDownloadErrorStatus(AStatusCode: Cardinal): Boolean;
    function IsContentNotExistStatus(AStatusCode: Cardinal): Boolean;
    procedure Disconnect;
    procedure OnCancelEvent;
    procedure DoGetRequest(const ARequest: IDownloadRequest);
    procedure DoHeadRequest(const ARequest: IDownloadHeadRequest);
    procedure DoPostRequest(const ARequest: IDownloadPostRequest);
    procedure CheckGraceOff(Sender: Tobject);
  private
    procedure DoOnALStatusChange(
      sender: Tobject;
      InternetStatus: DWord;
      StatusInformation: Pointer;
      StatusInformationLength: DWord
    );
    function DoRequest(
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer
    ): IDownloadResult;
  public
    constructor Create(
      const AResultFactory: IDownloadResultFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  ALfcnString,
  i_BinaryData,
  u_ListenerByEvent,
  u_Synchronizer,
  u_StreamReadOnlyByBinaryData,
  u_TileRequestBuilderHelpers,
  u_BinaryDataByMemStream;

{ TDownloaderHttp }

procedure TDownloaderHttp.CheckGraceOff(Sender: Tobject);
begin
  if (0 <> FDisconnectByServer) then begin
    try
      //FDisconnectByServer:=0;
      Disconnect;
    finally
      FDisconnectByServer := 0;
    end;
  end;
end;

constructor TDownloaderHttp.Create(
  const AResultFactory: IDownloadResultFactory
);
begin
  inherited Create;
  FOpeningHandle := nil;
  FDisconnectByUser := 0;
  FDisconnectByServer := 0;
  FCS := MakeSyncRW_Big(Self, FALSE);
  FHttpClient := TALWinInetHTTPClient.Create(nil);
  FHttpClient.OnStatusChange := DoOnALStatusChange;
  FHttpResponseHeader := TALHTTPResponseHeader.Create;
  FHttpResponseBody := TMemoryStream.Create;
  FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancelEvent);
  FResultFactory := AResultFactory;

  FHttpClientLastConfig.HttpTimeOut := 0;
  FHttpClientLastConfig.HeaderUserAgent := '';
  FHttpClientLastConfig.HeaderRawText := '';
  FHttpClientLastConfig.ProxyUseIESettings := True;
  FHttpClientLastConfig.ProxyUseCustomSettings := False;
  FHttpClientLastConfig.ProxyHost := '';
  FHttpClientLastConfig.ProxyUseLogin := False;
  FHttpClientLastConfig.ProxyUserName := '';
  FHttpClientLastConfig.ProxyPassword := '';
end;

destructor TDownloaderHttp.Destroy;
begin
  Disconnect;
  FreeAndNil(FHttpResponseHeader);
  FreeAndNil(FHttpResponseBody);
  FreeAndNil(FHttpClient);
  FResultFactory := nil;
  FCS := nil;
  inherited;
end;

procedure TDownloaderHttp.DoGetRequest(const ARequest: IDownloadRequest);
begin
  FHttpClient.Get(
    ARequest.Url,
    FHttpResponseBody,
    FHttpResponseHeader
  );
end;

procedure TDownloaderHttp.DoHeadRequest(const ARequest: IDownloadHeadRequest);
begin
  FHttpClient.Head(
    ARequest.Url,
    FHttpResponseBody,
    FHttpResponseHeader
  );
end;

procedure TDownloaderHttp.DoOnALStatusChange(
  sender: Tobject;
  InternetStatus: DWord;
  StatusInformation: Pointer;
  StatusInformationLength: DWord
);
begin
  if (0 = FDisconnectByUser) then begin
    // if disconnected - raise flag
    case InternetStatus of
      (*
      INTERNET_STATUS_HANDLE_CREATED: begin // 60
        FOpeningHandle:=PHINTERNET(StatusInformation)^;
      end;
      INTERNET_STATUS_RESOLVING_NAME: begin // 10
      end;
      INTERNET_STATUS_NAME_RESOLVED: begin // 11
      end;
      INTERNET_STATUS_CONNECTING_TO_SERVER: begin // 20
        FOpeningHandle:=nil;
      end;
      INTERNET_STATUS_CONNECTED_TO_SERVER: begin // 21
        FOpeningHandle:=nil;
      end;
      INTERNET_STATUS_SENDING_REQUEST: begin // 30
      end;
      INTERNET_STATUS_REQUEST_SENT: begin // 31
      end;
      INTERNET_STATUS_RECEIVING_RESPONSE: begin // 40
      end;
      INTERNET_STATUS_RESPONSE_RECEIVED: begin // 41
      end;
      INTERNET_STATUS_HANDLE_CLOSING: begin // 70
        FOpeningHandle:=nil;
      end;
      *)
      INTERNET_STATUS_CLOSING_CONNECTION: begin // 50
        if (0 = FDisconnectByServer) then begin
          Inc(FDisconnectByServer);
        end;
        (*
        if (nil<>FOpeningHandle) then begin
          InternetCloseHandle(FOpeningHandle);
          FOpeningHandle:=nil;
        end;
        *)
      end;
      INTERNET_STATUS_CONNECTION_CLOSED: begin // 51
        if (0 = FDisconnectByServer) then begin
          Inc(FDisconnectByServer);
        end;
        (*
        if (nil<>FOpeningHandle) then begin
          InternetCloseHandle(FOpeningHandle);
          FOpeningHandle:=nil;
        end;
        *)
      end;
    end;
  end;
end;

procedure TDownloaderHttp.DoPostRequest(const ARequest: IDownloadPostRequest);
var
  VData: IBinaryData;
  VStream: TStream;
begin
  VData := ARequest.PostData;
  if (VData <> nil) and (VData.Size > 0) then begin
    VStream := TStreamReadOnlyByBinaryData.Create(VData);
    try
      FHttpClient.Post(
        ARequest.Url,
        VStream,
        FHttpResponseBody,
        FHttpResponseHeader
      );
    finally
      VStream.Free;
    end;
  end else begin
    FHttpClient.Post(
      ARequest.Url,
      FHttpResponseBody,
      FHttpResponseHeader
    );
  end;
end;

function TDownloaderHttp.DoRequest(
  const ARequest: IDownloadRequest;
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer
): IDownloadResult;
var
  VPostRequest: IDownloadPostRequest;
  VHeadRequest: IDownloadHeadRequest;
begin
  // обычная работа
  Result := nil;
  FCS.BeginWrite;
  try
    // check gracefully off
    CheckGraceOff(nil);

    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Result := FResultFactory.BuildCanceled(ARequest);
    end;
    if Result = nil then begin
      ACancelNotifier.AddListener(FCancelListener);
      try
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Result := FResultFactory.BuildCanceled(ARequest);
        end;
        if Result = nil then begin
          Result := ProcessFileSystemRequest(
            ARequest,
            FResultFactory
          );
        end;
        if Result = nil then begin
          Result := OnBeforeRequest(
            ARequest,
            FResultFactory
          );
        end;
        if Result = nil then begin
          try
            if Supports(ARequest, IDownloadHeadRequest, VHeadRequest) then begin
              DoHeadRequest(VHeadRequest);
            end else if Supports(ARequest, IDownloadPostRequest, VPostRequest) then begin
              DoPostRequest(VPostRequest);
            end else begin
              DoGetRequest(ARequest);
            end;
          except
            on E: EALHTTPClientException do begin
              if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
                Result := FResultFactory.BuildCanceled(ARequest);
              end else begin
                if E.StatusCode = 0 then begin
                  Result := FResultFactory.BuildLoadErrorByUnknownReason(ARequest, '%s', [e.Message]);
                end;
              end;
            end;
            on E: EOSError do begin
              Result := OnOSError(
                ARequest,
                FResultFactory,
                E.ErrorCode
              );
            end;
          end;
        end;
        if Result = nil then begin
          Result := OnAfterResponse(
            ARequest,
            FResultFactory
          );
        end;
      finally
        FOpeningHandle := nil;
        ACancelNotifier.RemoveListener(FCancelListener);
      end;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TDownloaderHttp.Disconnect;
begin
  if Assigned(FHttpClient) then begin
    Inc(FDisconnectByUser);
    try
      FHttpClient.Disconnect;
    finally
      Dec(FDisconnectByUser);
    end;
  end;
end;

function TDownloaderHttp.OnBeforeRequest(
  const ARequest: IDownloadRequest;
  const AResultFactory: IDownloadResultFactory
): IDownloadResult;
var
  VRequestWithChecker: IRequestWithChecker;
begin
  FHttpResponseHeader.Clear;
  FHttpResponseBody.Clear;

  if Supports(ARequest, IRequestWithChecker, VRequestWithChecker) then begin
    VRequestWithChecker.Checker.BeforeRequest(AResultFactory, ARequest);
  end;

  if ARequest <> nil then begin
    PreConfigHttpClient(
      ARequest.RequestHeader,
      ARequest.InetConfig
    );
  end;

  Result := nil; // successful
end;

procedure TDownloaderHttp.OnCancelEvent;
begin
  Disconnect;
end;

function TDownloaderHttp.OnOSError(
  const ARequest: IDownloadRequest;
  const AResultFactory: IDownloadResultFactory;
  AErrorCode: Cardinal
): IDownloadResult;
begin
  Result := nil;
  if AResultFactory <> nil then begin
    if IsConnectError(AErrorCode) then begin
      Result := AResultFactory.BuildNoConnetctToServerByErrorCode(
        ARequest,
        AErrorCode
      );
    end else if IsDownloadError(AErrorCode) then begin
      Result := AResultFactory.BuildLoadErrorByErrorCode(
        ARequest,
        AErrorCode
      );
    end else begin
      Result := AResultFactory.BuildNoConnetctToServerByErrorCode(
        ARequest,
        AErrorCode
      );
    end;
  end;
end;

function TDownloaderHttp.OnAfterResponse(
  const ARequest: IDownloadRequest;
  const AResultFactory: IDownloadResultFactory
): IDownloadResult;
var
  VRawHeaderText: AnsiString;
  VStatusCode: Cardinal;
  VContentType: AnsiString;
begin
  Result := nil;
  if AResultFactory <> nil then begin
    VRawHeaderText := FHttpResponseHeader.RawHeaderText;
    VStatusCode := ALStrToIntDef(FHttpResponseHeader.StatusCode, 0);
    if IsOkStatus(VStatusCode) then begin
      VContentType := FHttpResponseHeader.ContentType;
      Result := InternalMakeResponse(
        AResultFactory,
        ARequest,
        VStatusCode,
        VContentType,
        VRawHeaderText,
        FHttpResponseBody
      );
    end else if IsDownloadErrorStatus(VStatusCode) then begin
      Result := AResultFactory.BuildLoadErrorByStatusCode(
        ARequest,
        VStatusCode
      );
    end else if IsContentNotExistStatus(VStatusCode) then begin
      Result := AResultFactory.BuildDataNotExistsByStatusCode(
        ARequest,
        VRawHeaderText,
        VStatusCode
      );
    end else begin
      Result := AResultFactory.BuildLoadErrorByUnknownStatusCode(
        ARequest,
        VStatusCode
      );
    end;
  end;
end;

procedure TDownloaderHttp.PreConfigHttpClient(
  const ARawHttpRequestHeader: AnsiString;
  const AInetConfig: IInetConfigStatic
);
var
  VProxyConfig: IProxyConfigStatic;
  VUserAgent: AnsiString;
  VProxyHost: AnsiString;
begin
  if (ARawHttpRequestHeader <> '') and
    (FHttpClientLastConfig.HeaderRawText <> ARawHttpRequestHeader) then begin
    FHttpClientLastConfig.HeaderRawText := ARawHttpRequestHeader;
    FHttpClient.RequestHeader.RawHeaderText := FHttpClientLastConfig.HeaderRawText;
  end;
  VUserAgent := GetHeaderValue(ARawHttpRequestHeader, 'User-Agent');
  if VUserAgent = '' then begin
    VUserAgent := AInetConfig.UserAgentString;
  end;
  if FHttpClientLastConfig.HeaderUserAgent <> VUserAgent then begin
    FHttpClientLastConfig.HeaderUserAgent := VUserAgent;
    FHttpClient.RequestHeader.UserAgent := VUserAgent;
  end;
  if FHttpClient.RequestHeader.Accept = '' then begin
    FHttpClient.RequestHeader.Accept := '*/*';
  end;
  if FHttpClientLastConfig.HttpTimeOut <> AInetConfig.TimeOut then begin
    FHttpClientLastConfig.HttpTimeOut := AInetConfig.TimeOut;
    FHttpClient.ConnectTimeout := FHttpClientLastConfig.HttpTimeOut;
    FHttpClient.SendTimeout := FHttpClientLastConfig.HttpTimeOut;
    FHttpClient.ReceiveTimeout := FHttpClientLastConfig.HttpTimeOut;
  end;
  FHttpClient.InternetOptions :=
    [
      wHttpIo_No_cache_write,
      wHttpIo_Pragma_nocache,
      wHttpIo_No_cookies,
      wHttpIo_Keep_connection
    ];
  VProxyConfig := AInetConfig.ProxyConfigStatic;
  if Assigned(VProxyConfig) then begin
    if (FHttpClientLastConfig.ProxyUseIESettings <> VProxyConfig.UseIESettings) or
      (FHttpClientLastConfig.ProxyUseCustomSettings <> VProxyConfig.UseProxy) or
      (FHttpClientLastConfig.ProxyUseLogin <> VProxyConfig.UseLogin) or
      (FHttpClientLastConfig.ProxyHost <> VProxyConfig.Host) or
      (FHttpClientLastConfig.ProxyUserName <> VProxyConfig.Login) or
      (FHttpClientLastConfig.ProxyPassword <> VProxyConfig.Password) then begin
      FHttpClientLastConfig.ProxyUseIESettings := VProxyConfig.UseIESettings;
      FHttpClientLastConfig.ProxyUseCustomSettings := VProxyConfig.UseProxy;
      FHttpClientLastConfig.ProxyUseLogin := VProxyConfig.UseLogin;
      FHttpClientLastConfig.ProxyHost := VProxyConfig.Host;
      FHttpClientLastConfig.ProxyUserName := VProxyConfig.Login;
      FHttpClientLastConfig.ProxyPassword := VProxyConfig.Password;
      if FHttpClientLastConfig.ProxyUseIESettings then begin
        FHttpClient.AccessType := wHttpAt_Preconfig;
      end else if FHttpClientLastConfig.ProxyUseCustomSettings then begin
        FHttpClient.AccessType := wHttpAt_Proxy;
        VProxyHost := FHttpClientLastConfig.ProxyHost;
        FHttpClient.ProxyParams.ProxyServer :=
          ALCopyStr(VProxyHost, 0, ALPos(':', VProxyHost) - 1);
        FHttpClient.ProxyParams.ProxyPort :=
          ALStrToInt(ALCopyStr(VProxyHost, ALPos(':', VProxyHost) + 1, Length(VProxyHost)));
        if FHttpClientLastConfig.ProxyUseLogin then begin
          FHttpClient.ProxyParams.ProxyUserName := FHttpClientLastConfig.ProxyUserName;
          FHttpClient.ProxyParams.ProxyPassword := FHttpClientLastConfig.ProxyPassword;
        end;
      end else begin
        FHttpClient.AccessType := wHttpAt_Direct;
      end;
    end;
  end;
end;

function TDownloaderHttp.ProcessFileSystemRequest(
  const ARequest: IDownloadRequest;
  const AResultFactory: IDownloadResultFactory
): IDownloadResult;
var
  VUrl: String;
  VStatusCode: Cardinal;
  VContentType, VRawResponseHeader: AnsiString;
  VMemStream: TMemoryStream;
  VOwned: Boolean;
begin
  Result := nil;
  if (nil=AResultFactory) then
    Exit;

  // check filename
  VUrl := ARequest.Url;
  if (Length(VUrl) < 4) then
    Exit;

  // very simple checks
  if (VUrl[2] in ['t','T']) then begin
    // fast detect ftp & http(s)
    // skip file, \\ & C:
    Exit;
  end else if (VUrl[1]='\') and (VUrl[2]='\') then begin
    // in case of \\servername\sharename\folder\..
  end else if (VUrl[2]=':') and (VUrl[3]='\') then begin
    // in case of C:\folder\...
  end else if (VUrl[1] in ['f','F']) then begin
    // check for
    // file:///C:/folder/...
    // file://///servername/sharename/folder/...
    if not SameText(System.Copy(VUrl, 1, 8), 'file:///') then
      Exit;
    // bingo!
    System.Delete(VUrl, 1, 8);
    if (Length(VUrl) <= 2) then
      Exit;
    // replace slashes
    VUrl := StringReplace(VUrl, '/', '\', [rfReplaceAll]);
  end else begin
    // noway
    Exit;
  end;

  // just empty headers
  VRawResponseHeader := '';

  // check
  if FileExists(VUrl) then begin
    // found
    VOwned := FALSE;
    VMemStream:=TMemoryStream.Create;
    try
      // read file
      VMemStream.LoadFromFile(VUrl);
      // autodetect file type
      VContentType := '';
      VStatusCode := HTTP_STATUS_OK;
      Result := InternalMakeResponse(
        AResultFactory,
        ARequest,
        VStatusCode,
        VContentType,
        VRawResponseHeader,
        VMemStream,
        @VOwned
      );
    finally
      if (not VOwned) then begin
        VMemStream.Free;
      end;
    end;
  end;

  // no file
  if (nil=Result) then begin
    Result := AResultFactory.BuildDataNotExistsByStatusCode(
      ARequest,
      VRawResponseHeader,
      HTTP_STATUS_NOT_FOUND
    );
  end;
end;

function TDownloaderHttp.InternalMakeResponse(
  const AResultFactory: IDownloadResultFactory;
  const ARequest: IDownloadRequest;
  var AStatusCode: Cardinal;
  var AContentType, ARawHeaderText: AnsiString;
  const ABody: TMemoryStream;
  const AHasOwned: PBoolean
): IDownloadResult;
var
  VSize: Int64;
  VRequestWithChecker: IRequestWithChecker;
  VResponseBody: IBinaryData;
begin
  Result := nil;
  VSize := ABody.Size;

  if (AHasOwned<>nil) then begin
    // can own stream without copying memory
    VResponseBody := TBinaryDataByMemStream.CreateWithOwn(ABody);
    AHasOwned^ := TRUE;
  end else begin
    // cannot own - just copy
    VResponseBody := TBinaryDataByMemStream.CreateFromMem(
      ABody.Size,
      ABody.Memory
    );
  end;

  // if has checker
  if Supports(ARequest, IRequestWithChecker, VRequestWithChecker) then begin
    Result := VRequestWithChecker.Checker.AfterReciveData(
      AResultFactory,
      ARequest,
      VResponseBody,
      AStatusCode,
      AContentType,
      ARawHeaderText
    );
  end;

  if Result = nil then begin
    if VSize = 0 then begin
      Result := AResultFactory.BuildDataNotExistsZeroSize(
        ARequest,
        AStatusCode,
        ARawHeaderText
      );
    end else begin
      Result := AResultFactory.BuildOk(
        ARequest,
        AStatusCode,
        ARawHeaderText,
        AContentType,
        VResponseBody
      );
    end;
  end;
end;

function TDownloaderHttp.IsConnectError(ALastError: Cardinal): Boolean;
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
  else begin
    Result := False;
  end;
  end;
end;

function TDownloaderHttp.IsDownloadError(ALastError: Cardinal): Boolean;
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
  else begin
    Result := False;
  end;
  end;
end;

function TDownloaderHttp.IsDownloadErrorStatus(AStatusCode: Cardinal): Boolean;
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

function TDownloaderHttp.IsOkStatus(AStatusCode: Cardinal): Boolean;
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

function TDownloaderHttp.IsContentNotExistStatus(AStatusCode: Cardinal): Boolean;
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

end.



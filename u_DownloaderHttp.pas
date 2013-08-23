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
  i_BinaryData,
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
    FAllowUseCookie: Boolean;
    function InternalMakeResponse(
      const AResultFactory: IDownloadResultFactory;
      const ARequest: IDownloadRequest;
      const AResponseBody: IBinaryData;
      var AStatusCode: Cardinal;
      var AContentType, ARawHeaderText: AnsiString
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
      const AResultFactory: IDownloadResultFactory;
      const AAllowUseCookie: Boolean = FALSE
    );
    destructor Destroy; override;
  end;

implementation

uses
  ALfcnString,
  u_ListenerByEvent,
  u_Synchronizer,
  u_HttpStatusChecker,
  u_StreamReadOnlyByBinaryData,
  u_TileRequestBuilderHelpers,
  u_BinaryData,
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
  const AResultFactory: IDownloadResultFactory;
  const AAllowUseCookie: Boolean
);
begin
  inherited Create;
  FOpeningHandle := nil;
  FDisconnectByUser := 0;
  FDisconnectByServer := 0;
  FAllowUseCookie := AAllowUseCookie;
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
  VResponseBody: IBinaryData;
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
      VResponseBody :=
        TBinaryData.Create(
          FHttpResponseBody.Size,
          FHttpResponseBody.Memory
        );
      Result := InternalMakeResponse(
        AResultFactory,
        ARequest,
        VResponseBody,
        VStatusCode,
        VContentType,
        VRawHeaderText
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
  VPos: Integer;
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

  // cookie
  if FAllowUseCookie then begin
    // allow cookie
    FHttpClient.InternetOptions :=
    [
      wHttpIo_No_cache_write,
      wHttpIo_Pragma_nocache,
      wHttpIo_Keep_connection
    ]
  end else begin
    // no cookie
    FHttpClient.InternetOptions :=
    [
      wHttpIo_No_cache_write,
      wHttpIo_Pragma_nocache,
      wHttpIo_No_cookies,
      wHttpIo_Keep_connection
    ];
  end;
    
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
        VPos := ALPos(':', VProxyHost);
        if VPos > 0 then begin
          FHttpClient.ProxyParams.ProxyServer :=
            ALCopyStr(VProxyHost, 0, VPos - 1);
          FHttpClient.ProxyParams.ProxyPort :=
            ALStrToInt(ALCopyStr(VProxyHost, VPos + 1, Length(VProxyHost)));
        end else begin
          FHttpClient.ProxyParams.ProxyServer := VProxyHost;
          FHttpClient.ProxyParams.ProxyPort := 0;
        end;

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
  VResponseBody: IBinaryData;
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
    VMemStream:=TMemoryStream.Create;
    try
      // read file
      VMemStream.LoadFromFile(VUrl);
      VResponseBody := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
      VMemStream := nil;
    finally
      VMemStream.Free;
    end;
    // autodetect file type
    VContentType := '';
    VStatusCode := HTTP_STATUS_OK;
    Result := InternalMakeResponse(
      AResultFactory,
      ARequest,
      VResponseBody,
      VStatusCode,
      VContentType,
      VRawResponseHeader
    );
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
  const AResponseBody: IBinaryData;
  var AStatusCode: Cardinal;
  var AContentType, ARawHeaderText: AnsiString
): IDownloadResult;
var
  VRequestWithChecker: IRequestWithChecker;
begin
  Result := nil;

  // if has checker
  if Supports(ARequest, IRequestWithChecker, VRequestWithChecker) then begin
    Result := VRequestWithChecker.Checker.AfterReciveData(
      AResultFactory,
      ARequest,
      AResponseBody,
      AStatusCode,
      AContentType,
      ARawHeaderText
    );
  end;

  if Result = nil then begin
    if AResponseBody.Size = 0 then begin
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
        AResponseBody
      );
    end;
  end;
end;

end.



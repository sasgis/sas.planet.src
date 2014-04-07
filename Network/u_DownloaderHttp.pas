{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_DownloaderHttp;

interface

uses
  Windows,
  Classes,
  SysUtils,
  WinInet,
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

  TDownloaderHttp = class(TBaseInterfacedObject, IDownloader, IDownloaderAsync)
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
    FAllowRedirect: Boolean;
    FTryDetectContentType: Boolean;
    FOnDownloadProgress: TOnDownloadProgress;
    function InternalMakeResponse(
      const ARequest: IDownloadRequest;
      const AResponseBody: IBinaryData;
      var AStatusCode: Cardinal;
      var AContentType, ARawHeaderText: AnsiString
    ): IDownloadResult;
    function ProcessFileSystemRequest(
      const ARequest: IDownloadRequest
    ): IDownloadResult;
    function OnBeforeRequest(
      const ARequest: IDownloadRequest
    ): IDownloadResult;
    function OnOSError(
      const ARequest: IDownloadRequest;
      AErrorCode: Cardinal
    ): IDownloadResult;
    function OnAfterResponse(
      const ARequest: IDownloadRequest
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
    { ALHttpClient CallBack's }
    procedure DoOnALStatusChange(
      sender: Tobject;
      InternetStatus: DWord;
      StatusInformation: Pointer;
      StatusInformationLength: DWord
    );
    procedure DoOnALDownloadProgress(
      sender: Tobject;
      Read: Integer;
      Total: Integer
    );
  private
    { IDownloader }
    function DoRequest(
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer
    ): IDownloadResult;
    { IDownloaderAsync }
    procedure DoRequestAsync(
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const AOnResultCallBack: TRequestAsyncCallBack
    );
  public
    constructor Create(
      const AResultFactory: IDownloadResultFactory;
      const AAllowUseCookie: Boolean = False;
      const AAllowRedirect: Boolean = True;
      const ATryDetectContentType: Boolean = False;
      const AOnDownloadProgress: TOnDownloadProgress = nil
    );
    destructor Destroy; override;
  end;

implementation

uses
  UrlMon,
  ALString,
  u_ListenerByEvent,
  u_Synchronizer,
  u_HttpStatusChecker,
  u_StreamReadOnlyByBinaryData,
  u_TileRequestBuilderHelpers,
  u_ReadableThreadNames,
  u_BinaryData,
  u_BinaryDataByMemStream;

type
  TAsyncRequestHelperThread = class(TThread)
  private
    FDownloader: IDownloader;
    FRequest: IDownloadRequest;
    FCancelNotifier: INotifierOperation;
    FOperationID: Integer;
    FOnResultCallBack: TRequestAsyncCallBack;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const ADownloader: IDownloader;
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const AOnResultCallBack: TRequestAsyncCallBack
    );
  end;

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
  const AAllowUseCookie: Boolean;
  const AAllowRedirect: Boolean;
  const ATryDetectContentType: Boolean;
  const AOnDownloadProgress: TOnDownloadProgress
);
begin
  inherited Create;

  FResultFactory := AResultFactory;
  FAllowUseCookie := AAllowUseCookie;
  FAllowRedirect := AAllowRedirect;
  FTryDetectContentType := ATryDetectContentType;
  FOnDownloadProgress := AOnDownloadProgress;

  FOpeningHandle := nil;
  FDisconnectByUser := 0;
  FDisconnectByServer := 0;

  FCS := MakeSyncRW_Big(Self, FALSE);

  FHttpClient := TALWinInetHTTPClient.Create;
  FHttpClient.OnStatusChange := Self.DoOnALStatusChange;
  if Assigned(FOnDownloadProgress) then begin
    FHttpClient.OnDownloadProgress := Self.DoOnALDownloadProgress;
  end;
  FHttpClient.DisconnectOnError := True;

  FHttpResponseHeader := TALHTTPResponseHeader.Create;
  FHttpResponseBody := TMemoryStream.Create;

  FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancelEvent);

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

procedure TDownloaderHttp.DoOnALDownloadProgress(
  sender: Tobject;
  Read: Integer;
  Total: Integer
);
begin
  FOnDownloadProgress(Read, Total);
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
  const AOperationID: Integer
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
          Result := ProcessFileSystemRequest(ARequest);
        end;
        if Result = nil then begin
          Result := OnBeforeRequest(ARequest);
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
              Result := OnOSError(ARequest, E.ErrorCode);
            end;
          end;
        end;
        if Result = nil then begin
          Result := OnAfterResponse(ARequest);
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

procedure TDownloaderHttp.DoRequestAsync(
  const ARequest: IDownloadRequest;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const AOnResultCallBack: TRequestAsyncCallBack
);
begin
  TAsyncRequestHelperThread.Create(
    (Self as IDownloader),
    ARequest,
    ACancelNotifier,
    AOperationID,
    AOnResultCallBack
  );
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
  const ARequest: IDownloadRequest
): IDownloadResult;
var
  VRequestWithChecker: IRequestWithChecker;
begin
  FHttpResponseHeader.Clear;
  FHttpResponseBody.Clear;

  if Supports(ARequest, IRequestWithChecker, VRequestWithChecker) then begin
    VRequestWithChecker.Checker.BeforeRequest(FResultFactory, ARequest);
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
  AErrorCode: Cardinal
): IDownloadResult;
begin
  Result := nil;
  if FResultFactory <> nil then begin
    if IsConnectError(AErrorCode) then begin
      Result := FResultFactory.BuildNoConnetctToServerByErrorCode(
        ARequest,
        AErrorCode
      );
    end else if IsDownloadError(AErrorCode) then begin
      Result := FResultFactory.BuildLoadErrorByErrorCode(
        ARequest,
        AErrorCode
      );
    end else begin
      Result := FResultFactory.BuildNoConnetctToServerByErrorCode(
        ARequest,
        AErrorCode
      );
    end;
  end;
end;

function TDownloaderHttp.OnAfterResponse(
  const ARequest: IDownloadRequest
): IDownloadResult;

  function DetectContentType(const AData: Pointer; const ASize: Int64): AnsiString;
  const
    // IE9. Returns image/png and image/jpeg instead of image/x-png and image/pjpeg
    FMFD_RETURNUPDATEDIMGMIMES = $20;
  var
    VResult: HRESULT;
    VContentType: PWideChar;
  begin
    Assert(AData <> nil);
    Assert(ASize > 0);

    Result := '';

    VResult :=
      UrlMon.FindMimeFromData(
        nil,
        nil,
        AData,
        ASize,
        nil,
        FMFD_RETURNUPDATEDIMGMIMES,
        VContentType,
        0
      );

    if VResult = S_OK then begin
      Result := VContentType;
    end;
  end;

var
  VResponseBody: IBinaryData;
  VRawHeaderText: AnsiString;
  VStatusCode: Cardinal;
  VContentType: AnsiString;
  VRealContentType: AnsiString;
begin
  Result := nil;
  if FResultFactory <> nil then begin
    VRawHeaderText := FHttpResponseHeader.RawHeaderText;
    VStatusCode := ALStrToIntDef(FHttpResponseHeader.StatusCode, 0);
    if IsOkStatus(VStatusCode) then begin
      VContentType := FHttpResponseHeader.ContentType;

      if FTryDetectContentType then begin
        VRealContentType := DetectContentType(FHttpResponseBody.Memory, FHttpResponseBody.Size);
        if (VRealContentType <> '') and (AlLowerCase(VRealContentType) <> AlLowerCase(VContentType)) then begin
          VRawHeaderText := ALStringReplace(VRawHeaderText, VContentType, VRealContentType, [rfIgnoreCase, rfReplaceAll]);
          VContentType := VRealContentType;
        end;
      end;

      VResponseBody :=
        TBinaryData.Create(
          FHttpResponseBody.Size,
          FHttpResponseBody.Memory
        );
      Result := InternalMakeResponse(
        ARequest,
        VResponseBody,
        VStatusCode,
        VContentType,
        VRawHeaderText
      );
    end else if IsDownloadErrorStatus(VStatusCode) then begin
      Result := FResultFactory.BuildLoadErrorByStatusCode(
        ARequest,
        VStatusCode
      );
    end else if IsContentNotExistStatus(VStatusCode) then begin
      Result := FResultFactory.BuildDataNotExistsByStatusCode(
        ARequest,
        VRawHeaderText,
        VStatusCode
      );
    end else begin
      Result := FResultFactory.BuildLoadErrorByUnknownStatusCode(
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
  VOptions: TALWininetHttpClientInternetOptionSet;
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

  VOptions :=
    [
      wHttpIo_No_cache_write,
      wHttpIo_Pragma_nocache,
      wHttpIo_Keep_connection,
      wHttpIo_Ignore_cert_cn_invalid,
      wHttpIo_Ignore_cert_date_invalid,
      wHttpIo_Ignore_redirect_to_http,
      wHttpIo_Ignore_redirect_to_https
    ];

  if not FAllowUseCookie then begin
    Include(VOptions, wHttpIo_No_cookies);
  end;

  if not FAllowRedirect then begin
    Include(VOptions, wHttpIo_No_auto_redirect);
  end;

  FHttpClient.InternetOptions := VOptions;

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
  const ARequest: IDownloadRequest
): IDownloadResult;
var
  VUrl: String;
  VStatusCode: Cardinal;
  VContentType, VRawResponseHeader: AnsiString;
  VMemStream: TMemoryStream;
  VResponseBody: IBinaryData;
begin
  Result := nil;
  if (nil = FResultFactory) then
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
    VMemStream := TMemoryStream.Create;
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
      ARequest,
      VResponseBody,
      VStatusCode,
      VContentType,
      VRawResponseHeader
    );
  end;

  // no file
  if (nil=Result) then begin
    Result := FResultFactory.BuildDataNotExistsByStatusCode(
      ARequest,
      VRawResponseHeader,
      HTTP_STATUS_NOT_FOUND
    );
  end;
end;

function TDownloaderHttp.InternalMakeResponse(
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
      FResultFactory,
      ARequest,
      AResponseBody,
      AStatusCode,
      AContentType,
      ARawHeaderText
    );
  end;

  if Result = nil then begin
    if AResponseBody.Size = 0 then begin
      Result := FResultFactory.BuildDataNotExistsZeroSize(
        ARequest,
        AStatusCode,
        ARawHeaderText
      );
    end else begin
      Result := FResultFactory.BuildOk(
        ARequest,
        AStatusCode,
        ARawHeaderText,
        AContentType,
        AResponseBody
      );
    end;
  end;
end;

{ TAsyncRequestHelperThread }

constructor TAsyncRequestHelperThread.Create(
  const ADownloader: IDownloader;
  const ARequest: IDownloadRequest;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const AOnResultCallBack: TRequestAsyncCallBack
);
begin
  FDownloader := ADownloader;
  FRequest := ARequest;
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
  FOnResultCallBack := AOnResultCallBack;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TAsyncRequestHelperThread.Execute;
var
  VResult: IDownloadResult;
begin
  SetCurrentThreadName(Self.ClassName);
  try
    VResult := FDownloader.DoRequest(FRequest, FCancelNotifier, FOperationID);
    FOnResultCallBack(VResult, FOperationID);
  except
    //
  end;
end;

end.



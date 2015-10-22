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

{$IFDEF DEBUG}
  {.$DEFINE VerboseHttpClient}
{$ENDIF}

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
  i_SimpleFlag,
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
    FDisconnectByServer: ISimpleFlag;
    FDisconnectByUser: ISimpleFlag;
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
  u_StrFunc,
  u_ListenerByEvent,
  u_Synchronizer,
  u_HttpStatusChecker,
  u_SimpleFlagWithInterlock,
  u_StreamReadOnlyByBinaryData,
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

{$IFDEF VerboseHttpClient}
procedure VerboseStatusChange(
  InternetStatus: DWORD;
  StatusInformation: Pointer;
  StatusInformationLength: DWORD
); forward;
{$ENDIF}

{ TDownloaderHttp }

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

  FDisconnectByUser := TSimpleFlagWithInterlock.Create;
  FDisconnectByServer := TSimpleFlagWithInterlock.Create;

  FCS := GSync.SyncBig.Make(Self.ClassName);

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
  FDisconnectByUser := nil;
  FDisconnectByServer := nil;
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
  {$IFDEF VerboseHttpClient}
  VerboseStatusChange(
    InternetStatus,
    StatusInformation,
    StatusInformationLength
  );
  {$ENDIF}
  case InternetStatus of
    INTERNET_STATUS_CLOSING_CONNECTION, INTERNET_STATUS_CONNECTION_CLOSED: begin
      if not FDisconnectByUser.CheckFlag then begin
        FDisconnectByServer.SetFlag;
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
  Assert(ARequest <> nil);
  Assert(ARequest.InetConfig <> nil);
  Assert(ARequest.InetConfig.ProxyConfigStatic <> nil);

  Result := nil;

  FCS.BeginWrite;
  try
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
            // check gracefully off
            if FDisconnectByServer.CheckFlag then begin
              try
                Disconnect;
              finally
                FDisconnectByServer.CheckFlagAndReset;
              end;
            end;
            {$IFDEF VerboseHttpClient}
            OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <I> DoRequest: ' + ARequest.Url));
            {$ENDIF}
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
              {$IFDEF VerboseHttpClient}
              OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <E> ' + E.ClassName + ':' + E.Message));
              {$ENDIF}
            end;
            on E: EOSError do begin
              Result := OnOSError(ARequest, E.ErrorCode);
              {$IFDEF VerboseHttpClient}
              OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <E> ' + E.ClassName + ':' + SysErrorMessage(E.ErrorCode)));
              {$ENDIF}
            end;
            on E: Exception do begin
              {$IFDEF VerboseHttpClient}
              OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <E> ' + E.ClassName + ':' + E.Message));
              {$ENDIF}
            end;
          end;
        end;
        if Result = nil then begin
          Result := OnAfterResponse(ARequest);
        end;
      finally
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
    {$IFDEF VerboseHttpClient}
    if FDisconnectByServer.CheckFlag then begin
      OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <W> Detect disconnection by server.'));
    end else begin
      OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <W> Init disconnection by user.'));
    end;
    {$ENDIF}
    FDisconnectByUser.SetFlag;
    try
      FHttpClient.Disconnect;
    finally
      FDisconnectByUser.CheckFlagAndReset;
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
      Result := AnsiString(VContentType);
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
    (FHttpClientLastConfig.HeaderRawText <> ARawHttpRequestHeader) then
  begin
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
      (FHttpClientLastConfig.ProxyPassword <> VProxyConfig.Password) then
    begin
      FHttpClientLastConfig.ProxyUseIESettings := VProxyConfig.UseIESettings;
      FHttpClientLastConfig.ProxyUseCustomSettings := VProxyConfig.UseProxy;
      FHttpClientLastConfig.ProxyUseLogin := VProxyConfig.UseLogin;
      FHttpClientLastConfig.ProxyHost := VProxyConfig.Host;
      FHttpClientLastConfig.ProxyUserName := UTF8Encode(VProxyConfig.Login);
      FHttpClientLastConfig.ProxyPassword := UTF8Encode(VProxyConfig.Password);

      if FHttpClientLastConfig.ProxyUseIESettings then begin
        FHttpClient.AccessType := wHttpAt_Preconfig;
        {$IFDEF VerboseHttpClient}
        OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <P> Set INTERNET_OPEN_TYPE_PRECONFIG'));
        {$ENDIF}
      end else if FHttpClientLastConfig.ProxyUseCustomSettings then begin
        VProxyHost := FHttpClientLastConfig.ProxyHost;
        Assert(VProxyHost <> '');
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
        FHttpClient.AccessType := wHttpAt_Proxy;
        {$IFDEF VerboseHttpClient}
        OutputDebugString(
          PChar(
            IntToStr(GetCurrentThreadId) + ' <P> Set INTERNET_OPEN_TYPE_PROXY ' +
            FHttpClient.ProxyParams.ProxyServer + ':' +
            IntToStr(FHttpClient.ProxyParams.ProxyPort)
          )
        );
        {$ENDIF}
      end else begin
        FHttpClient.AccessType := wHttpAt_Direct;
        {$IFDEF VerboseHttpClient}
        OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <P> Set INTERNET_OPEN_TYPE_DIRECT'));
        {$ENDIF}
      end;
    end;
  end else begin
    {$IFDEF VerboseHttpClient}
    OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <E> Fail ProxyConfig read.'));
    {$ENDIF}
  end;
end;

function TDownloaderHttp.ProcessFileSystemRequest(
  const ARequest: IDownloadRequest
): IDownloadResult;
var
  VUrl: AnsiString;
  VUrlLen: Integer;
  VStatusCode: Cardinal;
  VContentType, VRawResponseHeader: AnsiString;
  VMemStream: TMemoryStream;
  VResponseBody: IBinaryData;
  VFileName: string;
begin
  Result := nil;
  if (nil = FResultFactory) then begin
    Exit;
  end;

  // check filename
  VUrl := ARequest.Url;
  VUrlLen := Length(VUrl);
  if (VUrlLen < 4) then begin
    Exit;
  end;

  // very simple checks
  if (VUrl[2] in ['t', 'T']) then begin
    // fast detect ftp & http(s)
    // skip file, \\ & C:
    Exit;
  end else if (VUrl[1] = '\') and (VUrl[2] = '\') then begin
    // in case of \\servername\sharename\folder\..
  end else if (VUrl[2] = ':') and (VUrl[3] = '\') then begin
    // in case of C:\folder\...
  end else if (VUrl[1] in ['f', 'F']) then begin
    // check for
    // file:///C:/folder/...
    // file://///servername/sharename/folder/...
    if (VUrlLen <= 10) then begin
      Exit;
    end;
    if not ALSameText(ALCopyStr(VUrl, 1, 8), 'file:///') then begin
      Exit;
    end;
    // bingo!
    VUrl := ALCopyStr(VUrl, 9, VUrlLen);
    // replace slashes
    VUrl := ALStringReplace(VUrl, '/', '\', [rfReplaceAll]);
  end else begin
    // noway
    Exit;
  end;
  VFileName := string(VUrl);

  // just empty headers
  VRawResponseHeader := '';

  // check
  if FileExists(VFileName) then begin
    // found
    VMemStream := TMemoryStream.Create;
    try
      // read file
      VMemStream.LoadFromFile(VFileName);
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
  if (nil = Result) then begin
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
    {$IFDEF VerboseHttpClient}
    on E: Exception do begin
      OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <E> ' + E.ClassName + ':' + E.Message));
    end;
    {$ENDIF}
  end;
end;

{$IFDEF VerboseHttpClient}
procedure VerboseStatusChange(
  InternetStatus: DWORD;
  StatusInformation: Pointer;
  StatusInformationLength: DWORD
);

const
  INTERNET_STATUS_DETECTING_PROXY = 80;

  {
      #define INTERNET_STATUS_RESOLVING_NAME          10
      #define INTERNET_STATUS_NAME_RESOLVED           11
      #define INTERNET_STATUS_CONNECTING_TO_SERVER    20
      #define INTERNET_STATUS_CONNECTED_TO_SERVER     21
      #define INTERNET_STATUS_SENDING_REQUEST         30
      #define INTERNET_STATUS_REQUEST_SENT            31
      #define INTERNET_STATUS_RECEIVING_RESPONSE      40
      #define INTERNET_STATUS_RESPONSE_RECEIVED       41
      #define INTERNET_STATUS_CTL_RESPONSE_RECEIVED   42
      #define INTERNET_STATUS_PREFETCH                43
      #define INTERNET_STATUS_CLOSING_CONNECTION      50
      #define INTERNET_STATUS_CONNECTION_CLOSED       51
      #define INTERNET_STATUS_HANDLE_CREATED          60
      #define INTERNET_STATUS_HANDLE_CLOSING          70
      #define INTERNET_STATUS_DETECTING_PROXY         80
      #define INTERNET_STATUS_REQUEST_COMPLETE        100
      #define INTERNET_STATUS_REDIRECT                110
      #define INTERNET_STATUS_INTERMEDIATE_RESPONSE   120
      #define INTERNET_STATUS_USER_INPUT_REQUIRED     140
      #define INTERNET_STATUS_STATE_CHANGE            200
      #define INTERNET_STATUS_COOKIE_SENT             320
      #define INTERNET_STATUS_COOKIE_RECEIVED         321
      #define INTERNET_STATUS_PRIVACY_IMPACTED        324
      #define INTERNET_STATUS_P3P_HEADER              325
      #define INTERNET_STATUS_P3P_POLICYREF           326
      #define INTERNET_STATUS_COOKIE_HISTORY          327
  }

  function StatInfoAsStr: string;
  begin
    if StatusInformationLength > 0 then begin
      SetLength(Result, StatusInformationLength);
      Move(StatusInformation^, Result[1], StatusInformationLength);
    end else begin
      Result := '<EMPTY>';
    end;
  end;

var
  VInfoStr: string;
begin
  case InternetStatus of

    INTERNET_STATUS_CLOSING_CONNECTION:
    begin
      VInfoStr := 'INTERNET_STATUS_CLOSING_CONNECTION - Closing the connection to the server.';
    end;

    INTERNET_STATUS_CONNECTED_TO_SERVER:
    begin
      VInfoStr := 'INTERNET_STATUS_CONNECTED_TO_SERVER - Successfully connected to the socket address: ' + StatInfoAsStr;
    end;

    INTERNET_STATUS_CONNECTING_TO_SERVER:
    begin
      VInfoStr := 'INTERNET_STATUS_CONNECTING_TO_SERVER - Connecting to the socket address: ' + StatInfoAsStr;
    end;

    INTERNET_STATUS_CONNECTION_CLOSED:
    begin
      VInfoStr := 'INTERNET_STATUS_CONNECTION_CLOSED - Successfully closed the connection to the server.';
    end;

    INTERNET_STATUS_CTL_RESPONSE_RECEIVED:
    begin
      VInfoStr := 'INTERNET_STATUS_CTL_RESPONSE_RECEIVED - Not implemented';
    end;

    INTERNET_STATUS_HANDLE_CLOSING:
    begin
      VInfoStr := 'INTERNET_STATUS_HANDLE_CLOSING - This handle value has been terminated: ' + IntToHex(DWORD(StatusInformation^), 8);
    end;

    INTERNET_STATUS_HANDLE_CREATED:
    begin
      VInfoStr := 'INTERNET_STATUS_HANDLE_CREATED - InternetConnect has created the new handle: ' + IntToHex(DWORD(StatusInformation^), 8);
    end;

    INTERNET_STATUS_INTERMEDIATE_RESPONSE:
    begin
      VInfoStr := 'INTERNET_STATUS_INTERMEDIATE_RESPONSE - Received an intermediate (100 level) status code message from the server';
    end;

    INTERNET_STATUS_NAME_RESOLVED:
    begin
      VInfoStr := 'INTERNET_STATUS_NAME_RESOLVED - Successfully found the IP address of the name: ' + StatInfoAsStr;
    end;

    INTERNET_STATUS_PREFETCH:
    begin
      VInfoStr := 'INTERNET_STATUS_PREFETCH - Not implemented.';
    end;

    INTERNET_STATUS_RECEIVING_RESPONSE:
    begin
      VInfoStr := 'INTERNET_STATUS_RECEIVING_RESPONSE - Waiting for the server to respond to a request.';
    end;

    INTERNET_STATUS_REDIRECT:
    begin
      VInfoStr := 'INTERNET_STATUS_REDIRECT - HTTP request is about to automatically redirect the request. The new URL: ' + StatInfoAsStr;
    end;

    INTERNET_STATUS_REQUEST_COMPLETE:
    begin
      VInfoStr := 'INTERNET_STATUS_REQUEST_COMPLETE - An asynchronous operation has been completed.';
    end;

    INTERNET_STATUS_REQUEST_SENT:
    begin
      VInfoStr := 'INTERNET_STATUS_REQUEST_SENT - Successfully sent the information request to the server: ' + IntToStr(Integer(StatusInformation^)) + ' Byte';
    end;

    INTERNET_STATUS_RESOLVING_NAME:
    begin
      VInfoStr := 'INTERNET_STATUS_RESOLVING_NAME - Looking up the IP address of the name: ' + StatInfoAsStr;
    end;

    INTERNET_STATUS_RESPONSE_RECEIVED:
    begin
      VInfoStr := 'INTERNET_STATUS_RESPONSE_RECEIVED - Successfully received a response from the server: ' + IntToStr(Integer(StatusInformation^)) + ' Byte';
    end;

    INTERNET_STATUS_SENDING_REQUEST:
    begin
      VInfoStr := 'INTERNET_STATUS_SENDING_REQUEST - Sending the information request to the server.';
    end;

    INTERNET_STATUS_DETECTING_PROXY:
    begin
      VInfoStr := 'INTERNET_STATUS_DETECTING_PROXY - Proxy has been detected.';
    end;

    INTERNET_STATUS_STATE_CHANGE: begin
      VInfoStr := 'INTERNET_STATUS_STATE_CHANGE - Moved between a secure (HTTPS) and a nonsecure (HTTP) site.';

      case DWORD(StatusInformation^) of
        INTERNET_STATE_CONNECTED:
        begin
          VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATE_CONNECTED - Connected state. Mutually exclusive with disconnected state.';
        end;

        INTERNET_STATE_DISCONNECTED:
        begin
          VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATE_DISCONNECTED - Disconnected state. No network connection could be established.';
        end;

        INTERNET_STATE_DISCONNECTED_BY_USER:
        begin
          VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATE_DISCONNECTED_BY_USER - Disconnected by user request.';
        end;

        INTERNET_STATE_IDLE:
        begin
          VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATE_IDLE - No network requests are being made by Windows Internet.';
        end;

        INTERNET_STATE_BUSY:
        begin
          VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATE_BUSY - Network requests are being made by Windows Internet.';
        end;

        //INTERNET_STATUS_USER_INPUT_REQUIRED:
          //VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATUS_USER_INPUT_REQUIRED - The request requires user input to be completed.';
      else begin
        VInfoStr := '<W> Unknown StatusInformation: ' + IntToStr(DWORD(StatusInformation^));
      end;
      end;
    end;
  else begin
    VInfoStr := '<W> Unknown InternetStatus: ' + IntToStr(InternetStatus);
  end;
  end;
  OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <WinInet> ' + VInfoStr));
end;

{$ENDIF}

end.

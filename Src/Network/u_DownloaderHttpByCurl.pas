{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_DownloaderHttpByCurl;

interface

{$IFDEF DEBUG}
  {.$DEFINE DO_HTTP_LOG}
{$ENDIF}

uses
  Windows,
  Classes,
  SysUtils,
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
  u_CurlHttpClient,
  u_BaseInterfacedObject;

type
  TDownloaderHttpByCurl = class(TBaseInterfacedObject, IDownloader, IDownloaderAsync)
  private
    FLock: IReadWriteSync;
    FCancelListener: IListener;

    FHttpOptions: TCurlOptions;
    FHttpRequest: TCurlRequest;
    FHttpResponse: TCurlResponse;
    FHttpClient: TCurlHttpClient;

    FResultFactory: IDownloadResultFactory;
    FTryDetectContentType: Boolean;
    FOnDownloadProgress: TOnDownloadProgress;

    function InternalMakeResponse(
      const ARequest: IDownloadRequest;
      const AResponseBody: IBinaryData;
      var AStatusCode: Cardinal;
      var AContentType, ARawHeaderText: AnsiString
    ): IDownloadResult;

    function OnBeforeRequest(
      const ARequest: IDownloadRequest
    ): IDownloadResult;

    function OnAfterResponse(
      const ARequest: IDownloadRequest
    ): IDownloadResult;

    procedure DoDisconnect;

  {$IFDEF DO_HTTP_LOG}
  private
    FLogStream: TFileStream;
    procedure InitLog;
    procedure FinLog;
    procedure WriteLogMsg(const AMsg: string);
  {$ENDIF}
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
      const AAllowUseCookie: Boolean;
      const AAllowRedirect: Boolean;
      const ATryDetectContentType: Boolean;
      const AOnDownloadProgress: TOnDownloadProgress
    );
    destructor Destroy; override;
  end;

implementation

uses
  UrlMon,
  ALString,
  u_StrFunc,
  u_AsyncRequestHelperThread,
  u_ListenerByEvent,
  u_HttpStatusChecker,
  u_BinaryData,
  u_Synchronizer;

procedure OnCurlProgress(const ATotal: Integer; const ADownload: Integer;
  const AUserData: Pointer);
var
  VSelf: TDownloaderHttpByCurl absolute AUserData;
begin
  VSelf.FOnDownloadProgress(ADownload, ATotal);
end;

{$IFDEF DO_HTTP_LOG}
var
  GLogIdCounter: Integer = 0;

procedure OnCurlDebug(const ADebugMsg: RawByteString; const AUserData: Pointer);
var
  VSelf: TDownloaderHttpByCurl absolute AUserData;
begin
  VSelf.WriteLogMsg(string(ADebugMsg));
end;
{$ENDIF}

{ TDownloaderHttpByCurl }

constructor TDownloaderHttpByCurl.Create(
  const AResultFactory: IDownloadResultFactory;
  const AAllowUseCookie: Boolean;
  const AAllowRedirect: Boolean;
  const ATryDetectContentType: Boolean;
  const AOnDownloadProgress: TOnDownloadProgress
);
var
  VDebugCallBack: TCurlDebugCallBack;
  VProgressCallBack: TCurlProgressCallBack;
begin
  inherited Create;

  FResultFactory := AResultFactory;

  FHttpOptions.StoreCookie := AAllowUseCookie;
  FHttpOptions.FollowLocation := AAllowRedirect;
  FHttpOptions.IgnoreSSLCertificateErrors := True;

  FHttpRequest.Options := @FHttpOptions;
  FHttpResponse.Data := TMemoryStream.Create;

  FTryDetectContentType := ATryDetectContentType;
  FOnDownloadProgress := AOnDownloadProgress;

  VProgressCallBack := nil;
  if Assigned(FOnDownloadProgress) then begin
    VProgressCallBack := OnCurlProgress;
  end;

  {$IFDEF DO_HTTP_LOG}
  InitLog;
  VDebugCallBack := OnCurlDebug;
  {$ELSE}
  VDebugCallBack := nil;
  {$ENDIF}

  FHttpClient :=
    TCurlHttpClient.Create(
      StringToAnsiSafe(ExtractFilePath(ParamStr(0)) + cCurlDefaultCertFileName),
      VProgressCallBack,
      VDebugCallBack,
      Self
    );

  FCancelListener :=
    TNotifyNoMmgEventListener.Create(
      Self.DoDisconnect
    );

  FLock := GSync.SyncBig.Make(Self.ClassName);
end;

destructor TDownloaderHttpByCurl.Destroy;
begin
  FreeAndNil(FHttpClient);
  FreeAndNil(FHttpResponse.Data);
  {$IFDEF DO_HTTP_LOG}
  FinLog;
  {$ENDIF}
  inherited Destroy;
end;

function TDownloaderHttpByCurl.DoRequest(
  const ARequest: IDownloadRequest;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer
): IDownloadResult;
begin
  Assert(ARequest <> nil);
  Assert(ARequest.InetConfig <> nil);
  Assert(ARequest.InetConfig.ProxyConfigStatic <> nil);

  FLock.BeginWrite;
  try
    {$IFDEF DO_HTTP_LOG}
    WriteLogMsg(
      Format(
        '[INF] <start at %s> TreadId=%d; Url=%s',
        [FormatDateTime('hh:nn:ss.zzz', Now), GetCurrentThreadId, ARequest.Url]
        )
      );
    {$ENDIF}

    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Result := FResultFactory.BuildCanceled(ARequest);
      Exit;
    end;

    ACancelNotifier.AddListener(FCancelListener);
    try
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Result := FResultFactory.BuildCanceled(ARequest);
        Exit;
      end;

      try
        Result := OnBeforeRequest(ARequest);
        if Result <> nil then begin
          Exit;
        end;

        if FHttpClient.DoRequest(@FHttpRequest, @FHttpResponse) then begin
          Result := OnAfterResponse(ARequest);
        end else begin
          Result :=
            FResultFactory.BuildLoadErrorByUnknownReason(
              ARequest,
              FHttpResponse.ErrorReason,
              []
            );
        end;
      except
        on E: Exception do begin
          Result :=
            FResultFactory.BuildLoadErrorByUnknownReason(
              ARequest,
              E.Message,
              []
            );
          {$IFDEF DO_HTTP_LOG}
          WriteLogMsg(Format('[ERR] TreadID=%d; %s', [E.ClassName + ': ' + E.Message]));
          {$ENDIF}
        end;
      end;
    finally
      ACancelNotifier.RemoveListener(FCancelListener);
    end;

    {$IFDEF DO_HTTP_LOG}
    WriteLogMsg(
      Format(
        '[INF] <finish at %s> TreadId=%d; Url=%s',
        [FormatDateTime('hh:nn:ss.zzz', Now), GetCurrentThreadId, ARequest.Url]
      )
    );
    {$ENDIF}
  finally
    FLock.EndWrite;
  end;
end;

procedure TDownloaderHttpByCurl.DoRequestAsync(
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

procedure TDownloaderHttpByCurl.DoDisconnect;
begin
  if Assigned(FHttpClient) then begin
    FHttpClient.Disconnect;
  end;
end;

function TDownloaderHttpByCurl.OnBeforeRequest(
  const ARequest: IDownloadRequest
): IDownloadResult;
var
  VPostData: IBinaryData;
  VInetConfig: IInetConfigStatic;
  VHeadRequest: IDownloadHeadRequest;
  VPostRequest: IDownloadPostRequest;
  VRequestWithChecker: IRequestWithChecker;
begin
  Result := nil;

  if Supports(ARequest, IRequestWithChecker, VRequestWithChecker) then begin
    VRequestWithChecker.Checker.BeforeRequest(FResultFactory, ARequest);
  end;

  FHttpRequest.Url := ARequest.Url;
  FHttpRequest.Headers := ARequest.RequestHeader;
  FHttpRequest.PostData := nil;
  FHttpRequest.PostDataSize := 0;

  if Supports(ARequest, IDownloadHeadRequest, VHeadRequest) then begin
    FHttpRequest.Method := rmHead;
  end else
  if Supports(ARequest, IDownloadPostRequest, VPostRequest) then begin
    VPostData := VPostRequest.PostData;
    if VPostData <> nil then begin
      FHttpRequest.PostData := VPostData.Buffer;
      FHttpRequest.PostDataSize := VPostData.Size;
    end;
  end else begin
    FHttpRequest.Method := rmGet;
  end;

  FHttpResponse.Data.Clear;

  VInetConfig := ARequest.InetConfig;

  FHttpOptions.TimeOut := VInetConfig.TimeOut div 1000;
  FHttpOptions.ConnectionTimeOut := FHttpOptions.TimeOut;

  if GetHeaderValue(FHttpRequest.Headers, 'User-Agent') = '' then begin
    FHttpRequest.Headers :=
      SetHeaderValue(FHttpRequest.Headers, 'User-Agent', VInetConfig.UserAgentString);
  end;

  // accept and auto-decompress all known encodings
  FHttpOptions.AcceptEncoding := True;
  FHttpRequest.Headers := DeleteHeaderEntry(FHttpRequest.Headers, 'Accept-Encoding');

  // ToDo: Configure Proxy
end;

function TDownloaderHttpByCurl.OnAfterResponse(
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

      // fix detected mime types for IE versions prior IE 9
      if AlLowerCase(Result) = 'image/x-png' then begin
        Result := 'image/png';
      end else if AlLowerCase(Result) = 'image/pjpeg' then begin
        Result := 'image/jpeg';
      end;
    end;
  end;

var
  VBody: TMemoryStream;
  VHeaders: AnsiString;
  VStatusCode: Cardinal;
  VContentType: AnsiString;
  VDetectedContentType: AnsiString;
begin
  Result := nil;

  if FResultFactory <> nil then begin
    VHeaders := FHttpResponse.Headers;
    VStatusCode := FHttpResponse.Code;

    if IsOkStatus(VStatusCode) then begin
      VBody := FHttpResponse.Data;
      VContentType := GetHeaderValue(VHeaders, 'Content-Type');
      if FTryDetectContentType and (VBody.Size > 0) then begin
        VDetectedContentType := DetectContentType(VBody.Memory, VBody.Size);
        if (VDetectedContentType <> '') and (AlLowerCase(VDetectedContentType) <> AlLowerCase(VContentType)) then begin
          VHeaders := SetHeaderValue(VHeaders, 'Content-Type', VDetectedContentType);
          VContentType := VDetectedContentType;
        end;
      end;

      Result :=
        InternalMakeResponse(
          ARequest,
          TBinaryData.Create(VBody.Size, VBody.Memory) as IBinaryData,
          VStatusCode,
          VContentType,
          VHeaders
        );
    end else
    if IsDownloadErrorStatus(VStatusCode) then begin
      Result :=
        FResultFactory.BuildLoadErrorByStatusCode(
          ARequest,
          VStatusCode
        );
    end else
    if IsContentNotExistStatus(VStatusCode) then begin
      Result :=
        FResultFactory.BuildDataNotExistsByStatusCode(
          ARequest,
          VHeaders,
          VStatusCode
        );
    end else begin
      Result :=
        FResultFactory.BuildLoadErrorByUnknownStatusCode(
          ARequest,
          VStatusCode
        );
    end;
  end;
end;

function TDownloaderHttpByCurl.InternalMakeResponse(
  const ARequest: IDownloadRequest;
  const AResponseBody: IBinaryData;
  var AStatusCode: Cardinal;
  var AContentType, ARawHeaderText: AnsiString
): IDownloadResult;
var
  VRequestWithChecker: IRequestWithChecker;
begin
  if Supports(ARequest, IRequestWithChecker, VRequestWithChecker) then begin
    Result :=
      VRequestWithChecker.Checker.AfterReciveData(
        FResultFactory,
        ARequest,
        AResponseBody,
        AStatusCode,
        AContentType,
        ARawHeaderText
      );
    if Result <> nil then begin
      Exit;
    end;
  end;

  if AResponseBody.Size > 0 then begin
    Result :=
      FResultFactory.BuildOk(
        ARequest,
        AStatusCode,
        ARawHeaderText,
        AContentType,
        AResponseBody
      );
  end else begin
    Result :=
      FResultFactory.BuildDataNotExistsZeroSize(
        ARequest,
        AStatusCode,
        ARawHeaderText
      );
  end;
end;

{$IFDEF DO_HTTP_LOG}
procedure TDownloaderHttpByCurl.InitLog;
var
  VLogId: Integer;
  VLogFileName: string;
begin
  VLogId := InterlockedIncrement(GLogIdCounter);
  VLogFileName := Format('%s\HttpLog\%.4d.curl.log', [ExtractFileDir(ParamStr(0)), VLogId]);
  ForceDirectories(ExtractFileDir(VLogFileName));
  FLogStream := TFileStream.Create(VLogFileName, fmCreate or fmShareDenyWrite);
end;

procedure TDownloaderHttpByCurl.FinLog;
begin
  FreeAndNil(FLogStream);
end;

procedure TDownloaderHttpByCurl.WriteLogMsg(const AMsg: string);
var
  VText: UTF8String;
begin
  VText := UTF8Encode(AMsg) + #13#10;
  FLogStream.WriteBuffer(PAnsiChar(VText)^, Length(VText));
end;
{$ENDIF}

end.

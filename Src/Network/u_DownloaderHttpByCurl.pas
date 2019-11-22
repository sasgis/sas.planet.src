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
  t_CurlHttpClient,
  u_CurlHttpClient,
  u_CurlProxyResolver,
  u_DownloaderHttpBase;

type
  TDownloaderHttpByCurl = class(TDownloaderHttpBase, IDownloader)
  private
    FLock: IReadWriteSync;
    FCancelListener: IListener;

    FHttpProxy: TCurlProxy;
    FHttpOptions: TCurlOptions;
    FHttpRequest: TCurlRequest;
    FHttpResponse: TCurlResponse;
    FHttpClient: TCurlHttpClient;

    FProxyResolver: TCurlProxyResolver;

    FTryDetectContentType: Boolean;
    FOnDownloadProgress: TOnDownloadProgress;

    function OnBeforeRequest(
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
  public
    constructor Create(
      const AResultFactory: IDownloadResultFactory;
      const AAllowUseCookie: Boolean;
      const AAllowRedirect: Boolean;
      const AAcceptEncoding: Boolean;
      const ATryDetectContentType: Boolean;
      const AOnDownloadProgress: TOnDownloadProgress
    );
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  u_StrFunc,
  u_NetworkStrFunc,
  u_ListenerByEvent,
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
  const AAcceptEncoding: Boolean;
  const ATryDetectContentType: Boolean;
  const AOnDownloadProgress: TOnDownloadProgress
);
var
  VCertFileName: AnsiString;
  VDebugCallBack: TCurlDebugCallBack;
  VProgressCallBack: TCurlProgressCallBack;
begin
  inherited Create(AResultFactory);

  FHttpOptions.StoreCookie := AAllowUseCookie;
  FHttpOptions.FollowLocation := AAllowRedirect;
  FHttpOptions.AcceptEncoding := AAcceptEncoding;
  FHttpOptions.IgnoreSSLCertificateErrors := True; // ToDo

  FillChar(FHttpProxy, SizeOf(FHttpProxy), 0);

  FHttpRequest.Options := @FHttpOptions;
  FHttpRequest.Proxy := @FHttpProxy;
  FHttpResponse.Data := TMemoryStream.Create;

  FTryDetectContentType := ATryDetectContentType;
  FOnDownloadProgress := AOnDownloadProgress;

  VProgressCallBack := nil;
  if Assigned(FOnDownloadProgress) then begin
    VProgressCallBack := OnCurlProgress;
  end;

  FProxyResolver := TCurlProxyResolver.Create;

  {$IFDEF DO_HTTP_LOG}
  InitLog;
  VDebugCallBack := OnCurlDebug;
  {$ELSE}
  VDebugCallBack := nil;
  {$ENDIF}

  VCertFileName := '';
  if not FHttpOptions.IgnoreSSLCertificateErrors then begin
    VCertFileName := StringToAnsiSafe(ExtractFilePath(ParamStr(0)) + cCurlDefaultCertFileName)
  end;

  FHttpClient :=
    TCurlHttpClient.Create(
      VCertFileName,
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
  FreeAndNil(FProxyResolver);
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
var
  VResult: Boolean;
  VDescription: string;
  VProxyAuthType: string;
  VProxyRealm: string;
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

        VResult := FHttpClient.DoRequest(@FHttpRequest, @FHttpResponse);

        if (FHttpResponse.Code = 407) and (FHttpRequest.Proxy <> nil) then begin

          VResult :=
            FProxyResolver.GetProxyAuthenticateInfo(
              FHttpResponse.Headers,
              FHttpProxy.AuthType,
              VProxyAuthType,
              VProxyRealm
            );

          if not VResult then begin
            Assert(
              False,
              'Proxy-Authenticate parsing failed:' + #13#10 + FHttpResponse.Headers
            );
          end;

          if VResult and (VProxyRealm <> '') then begin
            VDescription := Format('%s "%s"', [FHttpProxy.Address, VProxyRealm]);
          end else begin
            VDescription := string(FHttpProxy.Address);
          end;

          VDescription :=
            Format(
              _('The proxy server %s is requesting a username and password.'),
              [VDescription]
            );

          FProxyResolver.DoResolveProxyAuth(
            ExtractFileName(ParamStr(0)),
            VDescription,
            FHttpProxy
          );
          VResult := FHttpProxy.UserName <> '';

          if VResult then begin
            VResult := FHttpClient.DoRequest(@FHttpRequest, @FHttpResponse);
          end;
        end;

        if VResult then begin
          Result :=
            OnAfterResponse(
              False,
              FTryDetectContentType,
              ARequest,
              FHttpResponse.Code,
              FHttpResponse.Headers,
              FHttpResponse.Data
            );
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
  VProxyConfig: IProxyConfigStatic;
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

  VInetConfig := ARequest.InetConfig;

  FHttpOptions.TimeOutMS := VInetConfig.TimeOut;
  FHttpOptions.ConnectionTimeOutMS := FHttpOptions.TimeOutMS;

  if GetHeaderValueUp(FHttpRequest.Headers, 'USER-AGENT') = '' then begin
    FHttpRequest.Headers := 'User-Agent: ' + VInetConfig.UserAgentString + #13#10 + FHttpRequest.Headers;
  end;

  if FHttpOptions.AcceptEncoding then begin
    DeleteHeaderValueUp(FHttpRequest.Headers, 'ACCEPT-ENCODING');
  end;

  VProxyConfig := VInetConfig.ProxyConfigStatic;

  if (VProxyConfig = nil) or
     (not VProxyConfig.UseProxy and not VProxyConfig.UseIESettings)
  then begin
    FHttpRequest.Proxy := nil;
  end else begin
    FillChar(FHttpProxy, SizeOf(FHttpProxy), 0);
    FHttpProxy.AuthType := atAny;

    if VProxyConfig.UseProxy then begin
      FHttpProxy.Address := VProxyConfig.Host;
      if VProxyConfig.UseLogin then begin
        FHttpProxy.UserName := StringToAnsiSafe(VProxyConfig.Login);
        FHttpProxy.UserPass := StringToAnsiSafe(VProxyConfig.Password);
      end;
    end else
    if VProxyConfig.UseIESettings then begin
      FProxyResolver.DoResolveProxy(string(FHttpRequest.Url), FHttpProxy);
    end;

    FHttpRequest.Proxy := @FHttpProxy;
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

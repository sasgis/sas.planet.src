{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_DownloaderHttpByCurl;

interface

{$IFDEF DEBUG}
  {$I SASPlanet.inc}
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
  i_ContentTypeManager,
  t_CurlHttpClient,
  u_CurlHttpClient,
  u_CurlProxyResolver,
  {$IFDEF WRITE_HTTP_LOG}
  u_DownloaderHttpLog,
  {$ENDIF}
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

    FAcceptEncoding: Boolean;
    FTryDetectContentType: Boolean;
    FOnDownloadProgress: TOnDownloadProgress;

    {$IFDEF WRITE_HTTP_LOG}
    FLog: TDownloaderHttpLog;
    {$ENDIF}

    function OnBeforeRequest(
      const ARequest: IDownloadRequest
    ): IDownloadResult;

    procedure DoDisconnect;
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
      const AContentTypeManager: IContentTypeManager;
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
  u_AnsiStr,
  u_ContentDecoder,
  u_NetworkStrFunc,
  u_ListenerByEvent,
  u_Synchronizer;

procedure OnCurlProgress(const ATotal, ADownload: Integer; const AUserData: Pointer);
var
  VSelf: TDownloaderHttpByCurl absolute AUserData;
begin
  VSelf.FOnDownloadProgress(ADownload, ATotal);
end;

{$IFDEF WRITE_HTTP_LOG}
procedure OnCurlDebug(const ADebugMsg: RawByteString; const AUserData: Pointer);
var
  VSelf: TDownloaderHttpByCurl absolute AUserData;
begin
  VSelf.FLog.Write(hmtRaw, string(ADebugMsg));
end;
{$ENDIF}

{ TDownloaderHttpByCurl }

constructor TDownloaderHttpByCurl.Create(
  const AResultFactory: IDownloadResultFactory;
  const AContentTypeManager: IContentTypeManager;
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
  inherited Create(AResultFactory, AContentTypeManager);

  FHttpOptions.StoreCookie := AAllowUseCookie;
  FHttpOptions.FollowLocation := AAllowRedirect;

  // libcurl doesn't fix header values Content-Length
  // and Content-Encoding in HTTP response when it automatically
  // decompress received content. In other hand, we rely on the
  // values of these headers, so we will deal with content
  // decompression ourselves.
  FAcceptEncoding := AAcceptEncoding;
  FHttpOptions.AcceptEncoding := False; // disable it for libcurl

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

  {$IFDEF WRITE_HTTP_LOG}
  FLog := TDownloaderHttpLog.Create('curl');
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
  {$IFDEF WRITE_HTTP_LOG}
  FreeAndNil(FLog);
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

  {$IFDEF WRITE_HTTP_LOG}
  FLog.Write(hmtInfo, 'BEGIN url=' + ARequest.Url);
  {$ENDIF}

  FLock.BeginWrite;
  try
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

      Result := ProcessFileSystemRequest(ARequest);
      if Result <> nil then begin
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
              FAcceptEncoding,
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
        {$IFDEF WRITE_HTTP_LOG}
        FLog.Write(FHttpResponse.Data);
        {$ENDIF}
      except
        on E: Exception do begin
          Result := FResultFactory.BuildLoadErrorByUnknownReason(ARequest, E.Message, []);
          {$IFDEF WRITE_HTTP_LOG}
          FLog.Write(hmtError, E.ClassName + ': ' + E.Message);
          {$ENDIF}
        end;
      end;
    finally
      ACancelNotifier.RemoveListener(FCancelListener);
    end;
  finally
    FLock.EndWrite;
    {$IFDEF WRITE_HTTP_LOG}
    FLog.Write(hmtInfo, 'END url=' + ARequest.Url);
    {$ENDIF}
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

  function GetProxyProtocol(const AProxyType: TProxyServerType): RawByteString;
  begin
    case AProxyType of
      ptHttp    : Result := '';
      ptHttps   : Result := 'https://';
      ptSocks4  : Result := 'socks4://';
      ptSocks4a : Result := 'socks4a://';
      ptSocks5  : Result := 'socks5://';
      ptSocks5h : Result := 'socks5h://';
    else
      raise Exception.CreateFmt('Unexpected ProxyType: %d', [Integer(AProxyType)]);
    end;
  end;

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
    FHttpRequest.Method := rmPost;
    VPostData := VPostRequest.PostData;
    if VPostData <> nil then begin
      FHttpRequest.PostData := VPostData.Buffer;
      FHttpRequest.PostDataSize := VPostData.Size;
    end;
  end else begin
    FHttpRequest.Method := rmGet;
  end;

  VInetConfig := ARequest.InetConfig;

  if not Assigned(FOnDownloadProgress) then begin
    // The maximum time in milliseconds that you allow the libcurl transfer operation to take.

    // Since this puts a hard limit for how long time a request is allowed to take, it has limited
    // use in dynamic use cases with varying transfer times. You are then advised to explore
    // CURLOPT_LOW_SPEED_LIMIT, CURLOPT_LOW_SPEED_TIME or using CURLOPT_PROGRESSFUNCTION to
    // implement your own timeout logic.
    // https://curl.se/libcurl/c/CURLOPT_TIMEOUT_MS.html

    FHttpOptions.TimeOutMS := VInetConfig.TimeOut;
  end else begin
    FHttpOptions.TimeOutMS := 0;
  end;

  FHttpOptions.ConnectionTimeOutMS := VInetConfig.TimeOut;

  if GetHeaderValueUp(FHttpRequest.Headers, 'USER-AGENT') = '' then begin
    FHttpRequest.Headers := 'User-Agent: ' + VInetConfig.UserAgentString + #13#10 + FHttpRequest.Headers;
  end;

  if FAcceptEncoding then begin
    DeleteHeaderValueUp(FHttpRequest.Headers, 'ACCEPT-ENCODING');
    AddHeaderValue(FHttpRequest.Headers, 'Accept-Encoding', TContentDecoder.GetDecodersStr);
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
      FHttpProxy.Address := GetProxyProtocol(VProxyConfig.ProxyType) + VProxyConfig.Host;
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

end.

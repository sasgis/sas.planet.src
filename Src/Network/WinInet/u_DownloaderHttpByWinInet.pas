{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_DownloaderHttpByWinInet;

interface

{$IFDEF DEBUG}
  {$I ..\Network.inc}
{$ENDIF}

uses
  Windows,
  Classes,
  SysUtils,
  WinInet,
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
  t_WinInetHttpClient,
  u_WinInetHttpClient,
  {$IFDEF WRITE_HTTP_LOG}
  u_DownloaderHttpLog,
  {$ENDIF}
  u_DownloaderHttpBase;

type
  TDownloaderHttpByWinInet = class(TDownloaderHttpBase, IDownloader)
  private
    FLock: IReadWriteSync;
    FCancelListener: IListener;

    FHttpProxy: TWinInetProxy;
    FHttpOptions: TWinInetOptions;
    FHttpRequest: TWinInetRequest;
    FHttpResponse: TWinInetResponse;
    FHttpClient: TWinInetHttpClient;

    FAllowUseCookie: Boolean;
    FAllowRedirect: Boolean;
    FAcceptEncoding: Boolean;
    FTryDetectContentType: Boolean;

    FOnDownloadProgress: TOnDownloadProgress;

    {$IFDEF WRITE_HTTP_LOG}
    FLog: TDownloaderHttpLog;
    procedure OnWinInetStatusChange(
      const AStatus: DWORD;
      const AInfo: Pointer;
      const AInfoLen: DWORD
    );
    {$ENDIF}

    procedure OnWinInetProgress(
      const ATotal: Integer;
      const ADownload: Integer
    );

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
  u_AnsiStr,
  u_ContentDecoder,
  u_NetworkStrFunc,
  u_ListenerByEvent,
  u_Synchronizer;

{ TDownloaderHttpByWinInet }

constructor TDownloaderHttpByWinInet.Create(
  const AResultFactory: IDownloadResultFactory;
  const AContentTypeManager: IContentTypeManager;
  const AAllowUseCookie: Boolean;
  const AAllowRedirect: Boolean;
  const AAcceptEncoding: Boolean;
  const ATryDetectContentType: Boolean;
  const AOnDownloadProgress: TOnDownloadProgress
);
var
  VProgressCallBack: TWinInetProgressCallBack;
  VStatusCallBack: TWinInetStatusCallBack;
begin
  inherited Create(AResultFactory, AContentTypeManager);

  FAllowUseCookie := AAllowUseCookie;
  FAllowRedirect := AAllowRedirect;
  FAcceptEncoding := AAcceptEncoding;
  FTryDetectContentType := ATryDetectContentType;
  FOnDownloadProgress := AOnDownloadProgress;

  FHttpRequest.Options := @FHttpOptions;
  FHttpRequest.Proxy := @FHttpProxy;
  FHttpResponse.Data := TMemoryStream.Create;

  VProgressCallBack := nil;
  if Assigned(FOnDownloadProgress) then begin
    VProgressCallBack := Self.OnWinInetProgress;
  end;

  {$IFDEF WRITE_HTTP_LOG}
  FLog := TDownloaderHttpLog.Create('wininet');
  VStatusCallBack := Self.OnWinInetStatusChange;
  {$ELSE}
  VStatusCallBack := nil;
  {$ENDIF}

  FHttpClient := TWinInetHttpClient.Create(VProgressCallBack, VStatusCallBack);
  FCancelListener := TNotifyNoMmgEventListener.Create(Self.DoDisconnect);
  FLock := GSync.SyncBig.Make(Self.ClassName);
end;

destructor TDownloaderHttpByWinInet.Destroy;
begin
  FreeAndNil(FHttpClient);
  FreeAndNil(FHttpResponse.Data);
  {$IFDEF WRITE_HTTP_LOG}
  FreeAndNil(FLog);
  {$ENDIF}
  inherited Destroy;
end;

function TDownloaderHttpByWinInet.DoRequest(
  const ARequest: IDownloadRequest;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer
): IDownloadResult;
var
  VResult: Boolean;
  VErrorReason: string;
begin
  Assert(ARequest <> nil);
  Assert(ARequest.InetConfig <> nil);
  Assert(ARequest.InetConfig.ProxyConfigStatic <> nil);

  {$IFDEF WRITE_HTTP_LOG}
  FLog.Write(hmtInfo, 'BEGIN ' + ARequest.Url);
  {$ENDIF}

  FLock.BeginWrite;
  try
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Result := FResultFactory.BuildCanceled(ARequest);
      Exit;
    end;

    FHttpClient.ResetDisconnectFlag;

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

        if VResult then begin
          {$IFDEF WRITE_HTTP_LOG}
          FLog.Write(hmtRaw, FHttpResponse.Headers);
          FLog.Write(TMemoryStream(FHttpResponse.Data));
          {$ENDIF}
          Result :=
            OnAfterResponse(
              FAcceptEncoding,
              FTryDetectContentType,
              ARequest,
              FHttpResponse.Code,
              FHttpResponse.Headers,
              TMemoryStream(FHttpResponse.Data)
            );
        end else begin
          DoDisconnect;
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            Result := FResultFactory.BuildCanceled(ARequest);
            {$IFDEF WRITE_HTTP_LOG}
            FLog.Write(hmtInfo, FHttpResponse.ErrorReason);
            {$ENDIF}
          end else begin
            VErrorReason := FHttpClient.ClassName + ': ' + FHttpResponse.ErrorReason;
            Result := FResultFactory.BuildLoadErrorByUnknownReason(ARequest, VErrorReason, []);
            {$IFDEF WRITE_HTTP_LOG}
            FLog.Write(hmtError, VErrorReason);
            {$ENDIF}
          end;
        end;
      except
        on E: Exception do begin
          DoDisconnect;
          VErrorReason := E.ClassName + ': ' + E.Message;
          Result := FResultFactory.BuildLoadErrorByUnknownReason(ARequest, VErrorReason, []);
          {$IFDEF WRITE_HTTP_LOG}
          FLog.Write(hmtError, VErrorReason);
          {$ENDIF}
        end;
      end;
    finally
      ACancelNotifier.RemoveListener(FCancelListener);
    end;
  finally
    FLock.EndWrite;
    {$IFDEF WRITE_HTTP_LOG}
    FLog.Write(hmtInfo, 'END ' + ARequest.Url);
    {$ENDIF}
  end;
end;

procedure TDownloaderHttpByWinInet.DoDisconnect;
begin
  if Assigned(FHttpClient) then begin
    FHttpClient.Disconnect;
  end;
end;

function TDownloaderHttpByWinInet.OnBeforeRequest(
  const ARequest: IDownloadRequest
): IDownloadResult;
var
  VEncodingCur: RawByteString;
  VEncodingOld: RawByteString;
  VPostData: IBinaryData;
  VInetConfig: IInetConfigStatic;
  VProxyConfig: IProxyConfigStatic;
  VHeadRequest: IDownloadHeadRequest;
  VPostRequest: IDownloadPostRequest;
  VRequestWithChecker: IRequestWithChecker;
  VProxyHost: RawByteString;
begin
  Result := nil;

  if Supports(ARequest, IRequestWithChecker, VRequestWithChecker) then begin
    Result := VRequestWithChecker.Checker.BeforeRequest(FResultFactory, ARequest);
  end;

  if Result <> nil then begin
    Exit;
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

  FHttpOptions.TimeOutMS := VInetConfig.TimeOut;
  FHttpOptions.IgnoreSecurityErrors := True;
  FHttpOptions.AllowHttp2Protocol := True;

  FHttpOptions.Flags :=
    INTERNET_FLAG_NO_CACHE_WRITE or
    INTERNET_FLAG_PRAGMA_NOCACHE or
    INTERNET_FLAG_KEEP_CONNECTION or
    INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP or
    INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS;

  if not FAllowUseCookie then begin
    FHttpOptions.Flags := FHttpOptions.Flags or INTERNET_FLAG_NO_COOKIES;
  end;
  if not FAllowRedirect then begin
    FHttpOptions.Flags := FHttpOptions.Flags or INTERNET_FLAG_NO_AUTO_REDIRECT;
  end;

  FHttpRequest.UserAgent := GetHeaderValueUp(FHttpRequest.Headers, 'USER-AGENT');
  if FHttpRequest.UserAgent = '' then begin
    FHttpRequest.UserAgent := VInetConfig.UserAgentString;
    FHttpRequest.Headers := 'User-Agent: ' + FHttpRequest.UserAgent + #13#10 + FHttpRequest.Headers;
  end;

  if GetHeaderValueUp(FHttpRequest.Headers, 'ACCEPT') = '' then begin
    AddHeaderValue(FHttpRequest.Headers, 'Accept', '*/*');
  end;

  if FAcceptEncoding then begin
    VEncodingOld := GetHeaderValueUp(FHttpRequest.Headers, 'ACCEPT-ENCODING');
    VEncodingCur := VEncodingOld;
    if VEncodingCur = '' then begin
      AddHeaderValue(FHttpRequest.Headers, 'Accept-Encoding', TContentDecoder.GetDecodersStr);
    end else begin
      TContentDecoder.RemoveUnsupportedDecoders(VEncodingCur);
      if VEncodingCur <> '' then begin
        if VEncodingCur <> VEncodingOld then begin
          ReplaceHeaderValueUp(FHttpRequest.Headers, 'ACCEPT-ENCODING', VEncodingCur);
        end;
      end else begin
        DeleteHeaderValueUp(FHttpRequest.Headers, 'ACCEPT-ENCODING');
      end;
    end;
  end;

  {$IFDEF WRITE_HTTP_LOG}
  FLog.Write(hmtRaw, FHttpRequest.Headers);
  {$ENDIF}

  VProxyConfig := VInetConfig.ProxyConfigStatic;

  FillChar(FHttpProxy, SizeOf(FHttpProxy), 0);

  if (VProxyConfig = nil) or
     (not VProxyConfig.UseProxy and not VProxyConfig.UseIESettings)
  then begin
    FHttpProxy.AccessType := INTERNET_OPEN_TYPE_DIRECT;
  end else begin
    if VProxyConfig.UseIESettings then begin
      FHttpProxy.AccessType := INTERNET_OPEN_TYPE_PRECONFIG;
    end else begin
      FHttpProxy.AccessType := INTERNET_OPEN_TYPE_PROXY;
      VProxyHost := VProxyConfig.Host;
      
      if VProxyConfig.ProxyType = ptSocks4 then begin
        VProxyHost := 'socks=' + VProxyHost;
      end else if VProxyConfig.ProxyType <> ptHttp then begin
        Assert(False, Format('Unsupported proxy type: %d', [Integer(VProxyConfig.ProxyType)]));
      end;
      
      FHttpProxy.Host := VProxyHost;
      
      if VProxyConfig.UseLogin then begin
        FHttpProxy.UserName := StringToAnsiSafe(VProxyConfig.Login);
        FHttpProxy.Password := StringToAnsiSafe(VProxyConfig.Password);
      end;
    end;
  end;
end;

procedure TDownloaderHttpByWinInet.OnWinInetProgress(const ATotal, ADownload: Integer);
begin
  FOnDownloadProgress(ATotal, ADownload);
end;

{$IFDEF WRITE_HTTP_LOG}
procedure TDownloaderHttpByWinInet.OnWinInetStatusChange(
  const AStatus: DWORD;
  const AInfo: Pointer;
  const AInfoLen: DWORD
);
const
  INTERNET_STATUS_DETECTING_PROXY = 80;
  INTERNET_STATUS_USER_INPUT_REQUIRED = 140;

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

  function StatInfoAsStr: AnsiString;
  begin
    if AInfoLen > 0 then begin
      SetLength(Result, AInfoLen);
      Move(AInfo^, Result[1], AInfoLen);
    end else begin
      Result := '<EMPTY>';
    end;
  end;

var
  VInfoStr: string;
begin
  case AStatus of
    INTERNET_STATUS_CLOSING_CONNECTION:
      VInfoStr := 'INTERNET_STATUS_CLOSING_CONNECTION - Closing the connection to the server.';

    INTERNET_STATUS_CONNECTED_TO_SERVER:
      VInfoStr := 'INTERNET_STATUS_CONNECTED_TO_SERVER - Successfully connected to the socket address: ' + StatInfoAsStr;

    INTERNET_STATUS_CONNECTING_TO_SERVER:
      VInfoStr := 'INTERNET_STATUS_CONNECTING_TO_SERVER - Connecting to the socket address: ' + StatInfoAsStr;

    INTERNET_STATUS_CONNECTION_CLOSED:
      VInfoStr := 'INTERNET_STATUS_CONNECTION_CLOSED - Successfully closed the connection to the server.';

    INTERNET_STATUS_CTL_RESPONSE_RECEIVED:
      VInfoStr := 'INTERNET_STATUS_CTL_RESPONSE_RECEIVED - Not implemented';

    INTERNET_STATUS_HANDLE_CLOSING:
      VInfoStr := 'INTERNET_STATUS_HANDLE_CLOSING - This handle value has been terminated: ' + IntToHex(DWORD(AInfo^), 8);

    INTERNET_STATUS_HANDLE_CREATED:
      VInfoStr := 'INTERNET_STATUS_HANDLE_CREATED - InternetConnect has created the new handle: ' + IntToHex(DWORD(AInfo^), 8);

    INTERNET_STATUS_INTERMEDIATE_RESPONSE:
       VInfoStr := 'INTERNET_STATUS_INTERMEDIATE_RESPONSE - Received an intermediate (100 level) status code message from the server';

    INTERNET_STATUS_NAME_RESOLVED:
      VInfoStr := 'INTERNET_STATUS_NAME_RESOLVED - Successfully found the IP address of the name: ' + StatInfoAsStr;

    INTERNET_STATUS_PREFETCH:
      VInfoStr := 'INTERNET_STATUS_PREFETCH - Not implemented.';

    INTERNET_STATUS_RECEIVING_RESPONSE:
      VInfoStr := 'INTERNET_STATUS_RECEIVING_RESPONSE - Waiting for the server to respond to a request.';

    INTERNET_STATUS_REDIRECT:
      VInfoStr := 'INTERNET_STATUS_REDIRECT - HTTP request is about to automatically redirect the request. The new URL: ' + StatInfoAsStr;

    INTERNET_STATUS_REQUEST_COMPLETE:
      VInfoStr := 'INTERNET_STATUS_REQUEST_COMPLETE - An asynchronous operation has been completed.';

    INTERNET_STATUS_REQUEST_SENT:
       VInfoStr := 'INTERNET_STATUS_REQUEST_SENT - Successfully sent the information request to the server: ' + IntToStr(Integer(AInfo^)) + ' Byte';

    INTERNET_STATUS_RESOLVING_NAME:
      VInfoStr := 'INTERNET_STATUS_RESOLVING_NAME - Looking up the IP address of the name: ' + StatInfoAsStr;

    INTERNET_STATUS_RESPONSE_RECEIVED:
      VInfoStr := 'INTERNET_STATUS_RESPONSE_RECEIVED - Successfully received a response from the server: ' + IntToStr(Integer(AInfo^)) + ' Byte';

    INTERNET_STATUS_SENDING_REQUEST:
      VInfoStr := 'INTERNET_STATUS_SENDING_REQUEST - Sending the information request to the server.';

    INTERNET_STATUS_DETECTING_PROXY:
      VInfoStr := 'INTERNET_STATUS_DETECTING_PROXY - Proxy has been detected.';

    INTERNET_STATUS_STATE_CHANGE: begin
      VInfoStr := 'INTERNET_STATUS_STATE_CHANGE - Moved between a secure (HTTPS) and a nonsecure (HTTP) site.';

      case DWORD(AInfo^) of
        INTERNET_STATE_CONNECTED:
          VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATE_CONNECTED - Connected state. Mutually exclusive with disconnected state.';

        INTERNET_STATE_DISCONNECTED:
          VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATE_DISCONNECTED - Disconnected state. No network connection could be established.';

        INTERNET_STATE_DISCONNECTED_BY_USER:
          VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATE_DISCONNECTED_BY_USER - Disconnected by user request.';

        INTERNET_STATE_IDLE:
          VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATE_IDLE - No network requests are being made by Windows Internet.';

        INTERNET_STATE_BUSY:
          VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATE_BUSY - Network requests are being made by Windows Internet.';

        INTERNET_STATUS_USER_INPUT_REQUIRED:
          VInfoStr := VInfoStr + #13#10 + 'INTERNET_STATUS_USER_INPUT_REQUIRED - The request requires user input to be completed.';
      else
        VInfoStr := 'INTERNET_STATUS_STATE_CHANGE: StatusInformation = ' + IntToStr(DWORD(AInfo^));
      end;
    end;
  else
    VInfoStr := 'INTERNET_STATUS: ' + IntToStr(AStatus);
  end;

  FLog.Write(hmtInfo, VInfoStr);
end;
{$ENDIF}

end.
{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_TileDownloaderHttp;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  SysUtils,
  ALHTTPCommon,
  ALHttpClient,
  ALWinInetHttpClient,
  i_JclNotify,
  i_OperationNotifier,
  i_SimpleDownloader,
  i_InetConfig,
  i_ProxySettings,
  i_DownloadResult,
  i_DownloadRequest,
  i_DownloadResultFactory,
  i_DownloadChecker;

type
  TTileDownloaderHttp = class(TInterfacedObject, ISimpleDownloader)
  private
    FCS: TCriticalSection;
    FCancelListener: IJclListener;
    FHttpClient: TALWinInetHTTPClient;
    FHttpResponseHeader: TALHTTPResponseHeader;
    FHttpResponseBody: TMemoryStream;
    FResultFactory: IDownloadResultFactory;
    FDownloadChecker: IDownloadChecker;
    function OnBeforeRequest(
      ARequest: IDownloadRequest;
      AResultFactory: IDownloadResultFactory
    ): IDownloadResult;
    function OnHttpError(
      ARequest: IDownloadRequest;
      AResultFactory: IDownloadResultFactory;
      AStatusCode: Cardinal;
      const AMessage: string
    ): IDownloadResult;
    function OnOSError(
      ARequest: IDownloadRequest;
      AResultFactory: IDownloadResultFactory;
      AErrorCode: Cardinal
    ): IDownloadResult;
    function OnAfterResponse(
      ARequest: IDownloadRequest;
      AResultFactory: IDownloadResultFactory
    ): IDownloadResult;
    procedure PreConfigHttpClient(
      const ARawHttpRequestHeader: string;
      AInetConfig: IInetConfigStatic
    );
    function IsConnectError(ALastError: Cardinal): Boolean;
    function IsDownloadError(ALastError: Cardinal): Boolean;
    function IsOkStatus(AStatusCode: Cardinal): Boolean;
    function IsDownloadErrorStatus(AStatusCode: Cardinal): Boolean;
    function IsTileNotExistStatus(AStatusCode: Cardinal): Boolean;
    procedure Disconnect;
    procedure OnCancelEvent(Sender: TObject);
    procedure DoGetRequest(ARequest: IDownloadRequest);
    procedure DoHeadRequest(ARequest: IDownloadHeadRequest);
    procedure DoPostRequest(ARequest: IDownloadPostRequest);
  protected
    function DoRequest(
      ARequest: IDownloadRequest;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    ): IDownloadResult;
  public
    constructor Create(
      ADownloadChecker: IDownloadChecker;
      AResultFactory: IDownloadResultFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  WinInet,
  u_NotifyEventListener;

{ TTileDownloaderHttp }

constructor TTileDownloaderHttp.Create(
  ADownloadChecker: IDownloadChecker;
  AResultFactory: IDownloadResultFactory
);
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FHttpClient := TALWinInetHTTPClient.Create(nil);
  FHttpResponseHeader := TALHTTPResponseHeader.Create;
  FHttpResponseBody := TMemoryStream.Create;
  FCancelListener := TNotifyEventListener.Create(Self.OnCancelEvent);
  FResultFactory := AResultFactory;
  FDownloadChecker := ADownloadChecker;
end;

destructor TTileDownloaderHttp.Destroy;
begin
  Disconnect;
  FreeAndNil(FCS);
  FreeAndNil(FHttpResponseHeader);
  FreeAndNil(FHttpResponseBody);
  FreeAndNil(FHttpClient);
  FResultFactory := nil;
  FDownloadChecker := nil;
  inherited;
end;

procedure TTileDownloaderHttp.DoGetRequest(ARequest: IDownloadRequest);
begin
  FHttpClient.Get(
    ARequest.Url,
    FHttpResponseBody,
    FHttpResponseHeader
  );
end;

procedure TTileDownloaderHttp.DoHeadRequest(ARequest: IDownloadHeadRequest);
begin
  FHttpClient.Head(
    ARequest.Url,
    FHttpResponseBody,
    FHttpResponseHeader
  );
end;

procedure TTileDownloaderHttp.DoPostRequest(ARequest: IDownloadPostRequest);
var
  VStream: TMemoryStream;
begin
  if ARequest.PostDataSize > 0 then begin
    VStream := TMemoryStream.Create;
    try
      VStream.Write(ARequest.PostData^, ARequest.PostDataSize);
      VStream.Position := 0;
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

function TTileDownloaderHttp.DoRequest(
  ARequest: IDownloadRequest;
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
): IDownloadResult;
var
  VPostRequest: IDownloadPostRequest;
  VHeadRequest: IDownloadHeadRequest;
begin
  Result := nil;
  FCS.Acquire;
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
          Result := OnBeforeRequest(
            ARequest,
            FResultFactory
          );
        end;
        if Result = nil then
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
            Result := OnHttpError(
              ARequest,
              FResultFactory,
              E.StatusCode,
              E.Message
            );
          end;
          on E: EOSError do begin
            Result := OnOSError(
              ARequest,
              FResultFactory,
              E.ErrorCode
            );
          end;
        end;
        if Result = nil then begin
          Result := OnAfterResponse(
            ARequest,
            FResultFactory
          );
        end;
      finally
        ACancelNotifier.RemoveListener(FCancelListener);
      end;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TTileDownloaderHttp.Disconnect;
begin
  if Assigned(FHttpClient) then begin
    FHttpClient.Disconnect;
  end;
end;

function TTileDownloaderHttp.OnBeforeRequest(
  ARequest: IDownloadRequest;
  AResultFactory: IDownloadResultFactory
): IDownloadResult;
begin
  FHttpResponseHeader.Clear;
  FHttpResponseBody.Clear;

  Result := FDownloadChecker.BeforeRequest(AResultFactory, ARequest);

  if Result = nil then begin
    PreConfigHttpClient(
      ARequest.RequestHeader,
      ARequest.InetConfig
    );
  end;
end;

procedure TTileDownloaderHttp.OnCancelEvent(Sender: TObject);
begin
  Disconnect;
end;

function TTileDownloaderHttp.OnHttpError(
  ARequest: IDownloadRequest;
  AResultFactory: IDownloadResultFactory;
  AStatusCode: Cardinal;
  const AMessage: string
): IDownloadResult;
begin
  if AResultFactory <> nil then begin
    if AStatusCode = 0 then begin
      Result := AResultFactory.BuildNotNecessary(
        ARequest,
        AMessage,
        FHttpResponseHeader.RawHeaderText
      );
    end else begin
      Result := AResultFactory.BuildLoadErrorByStatusCode(
        ARequest,
        AStatusCode
      );
    end;
  end;
end;

function TTileDownloaderHttp.OnOSError(
  ARequest: IDownloadRequest;
  AResultFactory: IDownloadResultFactory;
  AErrorCode: Cardinal
): IDownloadResult;
begin
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

function TTileDownloaderHttp.OnAfterResponse(
  ARequest: IDownloadRequest;
  AResultFactory: IDownloadResultFactory
): IDownloadResult;
var
  VRawHeaderText: string;
  VStatusCode: Cardinal;
  VContentType: string;
begin
  if AResultFactory <> nil then begin
    VRawHeaderText := FHttpResponseHeader.RawHeaderText;
    VContentType := FHttpResponseHeader.ContentType;
    VStatusCode := StrToIntDef(FHttpResponseHeader.StatusCode, 0);
    if IsOkStatus(VStatusCode) then begin
      Result := FDownloadChecker.AfterReciveData(
        AResultFactory,
        ARequest,
        FHttpResponseBody.Size,
        FHttpResponseBody.Memory,
        VStatusCode,
        VContentType,
        VRawHeaderText
      );
      if Result = nil then begin
        if FHttpResponseBody.Size = 0 then begin
          Result := AResultFactory.BuildDataNotExistsZeroSize(
            ARequest,
            VRawHeaderText
          );
        end else begin
          Result := AResultFactory.BuildOk(
            ARequest,
            VStatusCode,
            VRawHeaderText,
            VContentType,
            FHttpResponseBody.Size,
            FHttpResponseBody.Memory
          );
        end;
      end;
    end else if IsDownloadErrorStatus(VStatusCode) then begin
      Result := AResultFactory.BuildLoadErrorByStatusCode(
        ARequest,
        VStatusCode
      );
    end else if IsTileNotExistStatus(VStatusCode) then begin
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

procedure TTileDownloaderHttp.PreConfigHttpClient(
  const ARawHttpRequestHeader: string;
  AInetConfig: IInetConfigStatic
);
var
  VProxyConfig: IProxyConfigStatic;
begin
  FHttpClient.RequestHeader.Clear;
  if ARawHttpRequestHeader <> '' then begin
    FHttpClient.RequestHeader.RawHeaderText := ARawHttpRequestHeader;
  end;
  if FHttpClient.RequestHeader.UserAgent = '' then begin
    FHttpClient.RequestHeader.UserAgent := AInetConfig.UserAgentString;
  end;
  if FHttpClient.RequestHeader.Accept = '' then begin
    FHttpClient.RequestHeader.Accept := '*/*';
  end;   
  FHttpClient.ConnectTimeout := AInetConfig.TimeOut;
  FHttpClient.SendTimeout := AInetConfig.TimeOut;
  FHttpClient.ReceiveTimeout := AInetConfig.TimeOut;
  FHttpClient.InternetOptions := [  wHttpIo_No_cache_write,
                                    wHttpIo_Pragma_nocache,
                                    wHttpIo_No_cookies,
                                    wHttpIo_Keep_connection
                                 ];
  VProxyConfig := AInetConfig.ProxyConfigStatic;
  if Assigned(VProxyConfig) then begin
    if VProxyConfig.UseIESettings then begin
      FHttpClient.AccessType := wHttpAt_Preconfig
    end else if VProxyConfig.UseProxy then begin
      FHttpClient.AccessType := wHttpAt_Proxy;
      FHttpClient.ProxyParams.ProxyServer :=
        Copy(VProxyConfig.Host, 0, Pos(':', VProxyConfig.Host) - 1);
      FHttpClient.ProxyParams.ProxyPort :=
        StrToInt(Copy(VProxyConfig.Host, Pos(':', VProxyConfig.Host) + 1));
      if VProxyConfig.UseLogin then begin
        FHttpClient.ProxyParams.ProxyUserName := VProxyConfig.Login;
        FHttpClient.ProxyParams.ProxyPassword := VProxyConfig.Password;
      end;
    end else begin
      FHttpClient.AccessType := wHttpAt_Direct;
    end;
  end;
end;

function TTileDownloaderHttp.IsConnectError(ALastError: Cardinal): Boolean;
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
  else
    begin
      Result := False;
    end;
  end;
end;

function TTileDownloaderHttp.IsDownloadError(ALastError: Cardinal): Boolean;
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
  else
    begin
      Result := False;
    end;
  end;
end;

function TTileDownloaderHttp.IsDownloadErrorStatus(AStatusCode: Cardinal): Boolean;
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
  else
    begin
      Result := False;
    end;
  end;
end;

function TTileDownloaderHttp.IsOkStatus(AStatusCode: Cardinal): Boolean;
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
  else
    begin
      Result := False;
    end;
  end;
end;

function TTileDownloaderHttp.IsTileNotExistStatus(AStatusCode: Cardinal): Boolean;
begin
  case AStatusCode of
    HTTP_STATUS_NO_CONTENT,
    HTTP_STATUS_BAD_REQUEST,
    HTTP_STATUS_NOT_FOUND:
    begin
      Result := True;
    end;
  else
    begin
      Result := False;
    end;
  end;
end;

end.

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

unit u_WinInetHttpClient;

interface

{$DEFINE ALLOW_CLIENT_REUSE_BUFFERS}

uses
  Windows,
  Classes,
  SysUtils,
  WinInet,
  i_SimpleFlag,
  t_WinInetHttpClient;

type
  TWinInetHttpClient = class
  private type
    THostInfo = record
      HostName : AnsiString;
      Port     : INTERNET_PORT;
      Scheme   : TInternetScheme;
    end;
  private
    FLock: IReadWriteSync;

    FInetRoot: HINTERNET;
    FInetConnect: HINTERNET;
    FInetRequest: HINTERNET;

    FReq: PWinInetRequest;
    FResp: PWinInetResponse;
    FOptions: TWinInetOptions;
    FProxy: TWinInetProxy;

    FUserAgent: RawByteString;
    FHostInfo: THostInfo;
    FIsHttps: Boolean;

    FDoDisconnect: ISimpleFlag;

    FProgressCallBack: TWinInetProgressCallBack;
    FStatusCallBack: TWinInetStatusCallBack;

    function CrackUrl(const AUrl: AnsiString; out AInfo: THostInfo; out AUrlPath: AnsiString): Boolean;
    function CanReuseConnection(const AReq: PWinInetRequest; const AUrlInfo: THostInfo): Boolean; inline;

    function WinInetSetup: Boolean;

    procedure WinInetCloseRequest; inline;
    procedure WinInetCloseConnect; inline;
    procedure WinInetCloseRoot; inline;

    function SetSecurityFlags(const ARequest: HINTERNET): Boolean;
    function TryHandleSecurityError(const ARequest: HINTERNET; const AError: Cardinal): Boolean;

    procedure SetErrorReason(const AFuncName: string); overload; inline;
    procedure SetErrorReason(const AFuncName: string; const AError: Cardinal); overload;
  public
    function DoRequest(const AReq: PWinInetRequest; const AResp: PWinInetResponse): Boolean;
    procedure Disconnect;
    procedure ResetDisconnectFlag;
  public
    constructor Create(
      const AProgressCallBack: TWinInetProgressCallBack = nil;
      const AStatusCallBack: TWinInetStatusCallBack = nil
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_HttpStatusChecker,
  u_Synchronizer,
  u_SimpleFlagWithInterlock;

procedure WinInetStatusCallback(
  hInternet: HINTERNET;
  dwContext: DWORD_PTR;
  dwInternetStatus: DWORD;
  lpvStatusInformation: LPVOID;
  dwStatusInformationLength: DWORD
); stdcall;
var
  VSelf: TWinInetHttpClient;
begin
  if dwContext <> 0 then begin
    VSelf := TWinInetHttpClient(dwContext);
    if Assigned(VSelf.FStatusCallBack) then begin
      VSelf.FStatusCallBack(dwInternetStatus, lpvStatusInformation, dwStatusInformationLength);
    end;
  end;
end;

{ TWinInetHttpClient }

constructor TWinInetHttpClient.Create(
  const AProgressCallBack: TWinInetProgressCallBack;
  const AStatusCallBack: TWinInetStatusCallBack
);
begin
  inherited Create;

  FInetRoot := nil;
  FInetConnect := nil;
  FInetRequest := nil;

  FProgressCallBack := AProgressCallBack;
  FStatusCallBack := AStatusCallBack;

  FillChar(FHostInfo, SizeOf(FHostInfo), 0);
  FillChar(FOptions, SizeOf(FOptions), 0);
  FillChar(FProxy, SizeOf(FProxy), 0);

  FLock := GSync.SyncStd.Make(Self.ClassName);
  FDoDisconnect := TSimpleFlagWithInterlock.Create;
end;

destructor TWinInetHttpClient.Destroy;
begin
  Disconnect; // Ensures all handles are closed gracefully
  FLock := nil;
  FDoDisconnect := nil;
  inherited Destroy;
end;

procedure TWinInetHttpClient.WinInetCloseRequest;
begin
  if FInetRequest <> nil then begin
    InternetCloseHandle(FInetRequest);
    FInetRequest := nil;
  end;
end;

procedure TWinInetHttpClient.WinInetCloseConnect;
begin
  if FInetConnect <> nil then begin
    InternetCloseHandle(FInetConnect);
    FInetConnect := nil;
  end;
end;

procedure TWinInetHttpClient.WinInetCloseRoot;
begin
  if FInetRoot <> nil then begin
    InternetCloseHandle(FInetRoot);
    FInetRoot := nil;
  end;
end;

function TWinInetHttpClient.CrackUrl(const AUrl: AnsiString; out AInfo: THostInfo; out AUrlPath: AnsiString): Boolean;
var
  VHostName: array[0..INTERNET_MAX_HOST_NAME_LENGTH - 1] of Byte;
  VUrlPath: array[0..INTERNET_MAX_URL_LENGTH - 1] of Byte;
var
  VHostNamePtr, VUrlPathPtr: PAnsiChar;
  VUrlComponents: TURLComponentsA;
begin
  VHostNamePtr := @VHostName[0];
  VUrlPathPtr := @VUrlPath[0];

  FillChar(VUrlComponents, SizeOf(VUrlComponents), 0);
  VUrlComponents.dwStructSize := SizeOf(VUrlComponents);

  VUrlComponents.lpszHostName := VHostNamePtr;
  VUrlComponents.dwHostNameLength := INTERNET_MAX_HOST_NAME_LENGTH;

  VUrlComponents.lpszUrlPath := VUrlPathPtr;
  VUrlComponents.dwUrlPathLength := INTERNET_MAX_URL_LENGTH;

  if not InternetCrackUrlA(Pointer(AUrl), Length(AUrl), 0, VUrlComponents) then begin
    SetErrorReason('InternetCrackUrl');
    Exit(False);
  end;

  SetString(AInfo.HostName, VHostNamePtr, VUrlComponents.dwHostNameLength);
  SetString(AUrlPath, VUrlPathPtr, VUrlComponents.dwUrlPathLength);

  AInfo.Port := VUrlComponents.nPort;
  AInfo.Scheme := VUrlComponents.nScheme;

  FIsHttps := AInfo.Scheme = INTERNET_SCHEME_HTTPS;

  Result := True;
end;

function TWinInetHttpClient.CanReuseConnection(const AReq: PWinInetRequest; const AUrlInfo: THostInfo): Boolean;
begin
  Result :=
    // host
    (FHostInfo.HostName = AUrlInfo.HostName) and
    (FHostInfo.Port     = AUrlInfo.Port) and
    (FHostInfo.Scheme   = AUrlInfo.Scheme) and
    // user-agent
    (FUserAgent = AReq.UserAgent) and
    // options
    (FOptions = AReq.Options^) and
    // proxy
    (FProxy = AReq.Proxy^);
end;

procedure TWinInetHttpClient.SetErrorReason(const AFuncName: string);
begin
  SetErrorReason(AFuncName, GetLastError);
end;

procedure TWinInetHttpClient.SetErrorReason(const AFuncName: string; const AError: Cardinal);
var
  VMsg: string;
  VLen: Cardinal;
begin
  if FDoDisconnect.CheckFlag then begin
    FResp.ErrorReason := 'Disconnected by user';
  end else begin
    if AError <> 0 then begin
      SetLength(VMsg, 256);
      VLen := Windows.FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_IGNORE_INSERTS,
        Pointer(GetModuleHandle('wininet.dll')), AError, 0, Pointer(VMsg), Length(VMsg), nil);
      if VLen > 0 then begin
        SetLength(VMsg, VLen);
      end else begin
        VMsg := SysErrorMessage(AError);
      end;
    end else begin
      VMsg := '';
    end;
    FResp.ErrorReason := AFuncName + ' failed with error ' + IntToStr(AError) + ' - ' + Trim(VMsg);
  end;
end;

function TWinInetHttpClient.WinInetSetup: Boolean;

  procedure SetOptionA(hInet: HINTERNET; dwOption: DWORD; lpBuffer: Pointer; dwBufferLength: DWORD);
  begin
    if not InternetSetOptionA(hInet, dwOption, lpBuffer, dwBufferLength) then
      SetErrorReason('InternetSetOption');
  end;

const
  HTTP_PROTOCOL_FLAG_HTTP2 = 2;
  INTERNET_OPTION_ENABLE_HTTP_PROTOCOL = 148;
var
  VProxyPtr: PAnsiChar;
  VOption: DWORD;
  VTimeout: DWORD;
begin
  Assert(FInetRoot = nil);
  Assert(FInetConnect = nil);

  Result := False;

  VProxyPtr := nil;
  if FProxy.AccessType = INTERNET_OPEN_TYPE_PROXY then begin
    VProxyPtr := Pointer(FProxy.Host);
  end;

  FInetRoot :=
    InternetOpenA(
      nil, // User-Agent
      FProxy.AccessType,
      VProxyPtr,
      nil, // Proxy Bypass
      0    // Flags
    );

  if FInetRoot = nil then begin
    SetErrorReason('InternetOpen');
    Exit;
  end;

  if Assigned(FStatusCallBack) then begin
    InternetSetStatusCallback(FInetRoot, @WinInetStatusCallback);
  end;

  if FOptions.AllowHttp2Protocol then begin
    VOption := HTTP_PROTOCOL_FLAG_HTTP2;
    InternetSetOptionA(FInetRoot, INTERNET_OPTION_ENABLE_HTTP_PROTOCOL, @VOption, SizeOf(VOption));
  end;

  if FOptions.TimeOutMS > 0 then begin
    VTimeout := FOptions.TimeOutMS;
    SetOptionA(FInetRoot, INTERNET_OPTION_CONNECT_TIMEOUT, @VTimeout, SizeOf(VTimeout));
    SetOptionA(FInetRoot, INTERNET_OPTION_SEND_TIMEOUT,    @VTimeout, SizeOf(VTimeout));
    SetOptionA(FInetRoot, INTERNET_OPTION_RECEIVE_TIMEOUT, @VTimeout, SizeOf(VTimeout));
  end;

  FInetConnect :=
    InternetConnectA(
      FInetRoot,
      Pointer(FHostInfo.HostName),
      FHostInfo.Port,
      nil, // FTP UserName
      nil, // FTP Password
      INTERNET_SERVICE_HTTP,
      0,   // Flags
      DWORD_PTR(Self)
    );

  if FInetConnect = nil then begin
    SetErrorReason('InternetConnect');
    WinInetCloseRoot;
    Exit;
  end;

  if FProxy.UserName <> '' then begin
    SetOptionA(FInetConnect, INTERNET_OPTION_PROXY_USERNAME, Pointer(FProxy.UserName), Length(FProxy.UserName));
  end;
  if FProxy.Password <> '' then begin
    SetOptionA(FInetConnect, INTERNET_OPTION_PROXY_PASSWORD, Pointer(FProxy.Password), Length(FProxy.Password));
  end;

  Result := True;
end;

function TWinInetHttpClient.SetSecurityFlags(const ARequest: HINTERNET): Boolean;
var
  VFlags, VLen: DWORD;
begin
  VLen := SizeOf(VFlags);
  if InternetQueryOptionA(ARequest, INTERNET_OPTION_SECURITY_FLAGS, @VFlags, VLen) then begin
    VFlags := VFlags or SECURITY_SET_MASK;
    Result := InternetSetOptionA(ARequest, INTERNET_OPTION_SECURITY_FLAGS, @VFlags, SizeOf(VFlags));
  end else begin
    Result := False;
  end;
end;

function TWinInetHttpClient.TryHandleSecurityError(const ARequest: HINTERNET; const AError: Cardinal): Boolean;
begin
  Result := IsSecurityError(AError) and SetSecurityFlags(ARequest);
end;

function TWinInetHttpClient.DoRequest(const AReq: PWinInetRequest; const AResp: PWinInetResponse): Boolean;
const
  CMethod: array[TWinInetReqMethod] of PAnsiChar = ('GET', 'POST', 'HEAD');
  CHttpVersion: PAnsiChar = 'HTTP/1.1';
  CBufferSize = 64 * 1024; // 64 KB
var
  VUrlInfo: THostInfo;
  VUrlPath: AnsiString;
  VFlags, VLen, VIndex, VBytesRead, VContentLen: DWORD;
  VBuffer: array[0..CBufferSize-1] of Byte;
  VStream: TMemoryStream;
  VDataSize: NativeInt;
  VRequest: HINTERNET;
  VResult: Boolean;
  VError: Cardinal;
begin
  Assert(AReq.Options <> nil);
  Assert(AReq.Proxy <> nil);
  Assert(AResp.Data <> nil);

  Result := False;

  FReq := AReq;
  FResp := AResp;

  FResp.Code := 0;
  FResp.ErrorReason := '';
  VStream := FResp.Data;

  {$IFNDEF ALLOW_CLIENT_REUSE_BUFFERS}
  FResp.Headers := '';
  VStream.Size := 0;
  {$ENDIF}

  if not CrackUrl(FReq.Url, VUrlInfo, VUrlPath) then begin
    Exit;
  end;

  FLock.BeginWrite;
  try
    if FDoDisconnect.CheckFlag then begin
      Exit;
    end;

    if (FInetRoot = nil) or (FInetConnect = nil) or not CanReuseConnection(FReq, VUrlInfo) then begin
      FHostInfo := VUrlInfo;
      FUserAgent := FReq.UserAgent;
      FOptions := FReq.Options^;
      FProxy := FReq.Proxy^;

      Assert(FInetRequest = nil);
      WinInetCloseConnect;
      WinInetCloseRoot;

      if not WinInetSetup then begin
        Exit;
      end;
    end;

    VFlags := FOptions.Flags;
    if FIsHttps then begin
      VFlags := VFlags or INTERNET_FLAG_SECURE;
    end;

    Assert(FInetRequest = nil);

    FInetRequest :=
      HttpOpenRequestA(
        FInetConnect,
        CMethod[FReq.Method],
        Pointer(VUrlPath),
        CHttpVersion,
        nil, // Referer
        nil, // Accept
        VFlags,
        DWORD_PTR(Self)
      );

    if FInetRequest = nil then begin
      SetErrorReason('HttpOpenRequest');
      Exit;
    end;

    VRequest := FInetRequest;
  finally
    FLock.EndWrite;
  end;

  try
    if FReq.Headers <> '' then begin
      if not HttpAddRequestHeadersA(VRequest, Pointer(FReq.Headers), Length(FReq.Headers), HTTP_ADDREQ_FLAG_ADD or HTTP_ADDREQ_FLAG_REPLACE) then begin
        SetErrorReason('HttpAddRequestHeaders');
        Exit;
      end;
    end;

    if FDoDisconnect.CheckFlag then begin
      Exit;
    end;

    // Send request
    VResult := HttpSendRequestA(VRequest, nil, 0, FReq.PostData, FReq.PostDataSize);

    if not VResult then begin
      VError := GetLastError;
      if FIsHttps and FOptions.IgnoreSecurityErrors and TryHandleSecurityError(VRequest, VError) then begin
        // retry with disabled security flags
        VResult := HttpSendRequestA(VRequest, nil, 0, FReq.PostData, FReq.PostDataSize);
        if not VResult then VError := GetLastError;
      end;
      if not VResult then begin
        SetErrorReason('HttpSendRequest', VError);
        Exit;
      end;
    end;

    if FDoDisconnect.CheckFlag then begin
      Exit;
    end;

    // Response status code
    VLen := SizeOf(FResp.Code);
    VIndex := 0;
    if not HttpQueryInfoA(VRequest, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @FResp.Code, VLen, VIndex) then begin
      SetErrorReason('HttpQueryInfo HTTP_QUERY_STATUS_CODE');
      Exit;
    end;

    // Response raw headers
    VLen := 0;
    VIndex := 0;
    HttpQueryInfoA(VRequest, HTTP_QUERY_RAW_HEADERS_CRLF, nil, VLen, VIndex);
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then begin
      VIndex := 0;
      SetLength(FResp.Headers, VLen);
      if HttpQueryInfoA(VRequest, HTTP_QUERY_RAW_HEADERS_CRLF, Pointer(FResp.Headers), VLen, VIndex) then begin
        SetLength(FResp.Headers, VLen);
      end else begin
        FResp.Headers := '';
        SetErrorReason('HttpQueryInfo HTTP_QUERY_RAW_HEADERS #2');
        Exit;
      end;
    end else begin
      SetErrorReason('HttpQueryInfo HTTP_QUERY_RAW_HEADERS #1');
      Exit;
    end;

    // Response Content-Length
    VLen := SizeOf(VContentLen);
    VIndex := 0;
    if not HttpQueryInfoA(VRequest, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @VContentLen, VLen, VIndex) then begin
      // not an error
      VContentLen := 0;
    end;

    // Response body
    if (VContentLen > 0) and (VContentLen <= 16 * SizeOf(VBuffer)) then begin
      VStream.Size := VContentLen;
    end;
    VStream.Position := 0;
    VDataSize := 0;

    repeat
      if FDoDisconnect.CheckFlag then begin
        Exit;
      end;

      if not InternetReadFile(VRequest, @VBuffer[0], SizeOf(VBuffer), VBytesRead) then begin
        SetErrorReason('InternetReadFile');
        Exit;
      end;

      if VBytesRead = 0 then begin
        Break;
      end;

      VStream.WriteBuffer(VBuffer[0], VBytesRead);
      Inc(VDataSize, VBytesRead);

      if Assigned(FProgressCallBack) then begin
        FProgressCallBack(VContentLen, VDataSize);
      end;
    until False;

    if VDataSize <> VStream.Size then begin
      VStream.Size := VDataSize;
    end;

    Result := True;
  finally
    FLock.BeginWrite;
    try
      WinInetCloseRequest;
    finally
      FLock.EndWrite;
    end;
  end;
end;

procedure TWinInetHttpClient.Disconnect;
begin
  FLock.BeginWrite;
  try
    FDoDisconnect.SetFlag;
    WinInetCloseRequest;
    WinInetCloseConnect;
    WinInetCloseRoot;
  finally
    FLock.EndWrite;
  end;
end;

procedure TWinInetHttpClient.ResetDisconnectFlag;
begin
  FDoDisconnect.CheckFlagAndReset;
end;

end.

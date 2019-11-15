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

unit u_CurlHttpClient;

interface

uses
  Classes,
  SynCurl,
  t_CurlHttpClient;

type
  TCurlHttpClient = class
  private
    const
      coTimeoutMS = TCurlOption(155);
      coConnectTimeoutMS = TCurlOption(156);
      coNoProxy = TCurlOption(10177);
  private
    FCurl: TCurl;
    FCurlSlist: Pointer;
    FCertFileName: AnsiString;
    FDoDisconnect: Boolean;

    FReq: PCurlRequest;
    FResp: PCurlResponse;
    FOptions: TCurlOptions;
    FProxy: TCurlProxy;

    FReqMethod: TReqMethod;
    FReqHeaders: RawByteString;

    FCurlProgressCallBack: TCurlProgressCallBack;
    FCurlDebugCallBack: TCurlDebugCallBack;
    FCurlUserData: Pointer;

    procedure CurlSetup;
    procedure CurlClose; inline;
    procedure CurlCheck(const AResult: TCurlResult); {$IFNDEF DEBUG} inline; {$ENDIF}

    class function CurlAuthType(const AType: TCurlAuthType): Cardinal;
    class procedure CurlClearSlist(var ASlist: Pointer);
    class procedure CurlUpdateSlist(const ARawHeaders: RawByteString; var ASlist: Pointer);

    function IsOptionsChanged(const AOptions: PCurlOptions): Boolean; inline;
    function IsProxyChanged(const AProxy: PCurlProxy): Boolean; inline;
  public
    function DoRequest(
      const AReq: PCurlRequest;
      const AResp: PCurlResponse
    ): Boolean;
    procedure Disconnect;
    class function GetCurlVersionInfo: string;
  public
    constructor Create(
      const ACertFileName: AnsiString = '';
      const AProgressCallBack: TCurlProgressCallBack = nil;
      const ADebugCallBack: TCurlDebugCallBack = nil;
      const AUserData: Pointer = nil
    );
    destructor Destroy; override;
  end;

const
  // https://curl.haxx.se/docs/caextract.html
  cCurlDefaultCertFileName = 'curl-ca-bundle.crt';

const
  cCurlDefaultOptions: TCurlOptions = (
    StoreCookie: True;
    FollowLocation: True;
    AcceptEncoding: True;
    IgnoreSSLCertificateErrors: False;
    TimeOutMS: 30000;
    ConnectionTimeOutMS: 15000;
  );

  cCurlDefaultProxy: TCurlProxy = (
    Address: '';
    NoProxy: '';
    UserName: '';
    UserPass: '';
    AuthType: atAny;
  );

implementation

uses
  WinSock,
  SysUtils,
  u_NetworkStrFunc;

function CurlWriteHeaderCallBack(
  const AData: PAnsiChar;
  const ASize: Integer;
  const ANmemb: Integer;
  const AUserData: Pointer
): Integer; cdecl;
var
  VSelf: TCurlHttpClient absolute AUserData;
  VHeader: ^RawByteString;
  VHeaderLen: Integer;
begin
  if VSelf.FDoDisconnect then begin
    Result := 0;
    Exit;
  end;
  VHeader := @VSelf.FResp.Headers;
  VHeaderLen := Length(VHeader^);
  Result := ASize * ANmemb;
  SetLength(VHeader^, VHeaderLen + Result);
  Move(AData^, PPAnsiChar(VHeader)^[VHeaderLen], Result);
end;

function CurlWriteDataCallBack(
  const AData: PAnsiChar;
  const ASize: Integer;
  const ANmemb: Integer;
  const AUserData: Pointer
): Integer; cdecl;
var
  VSelf: TCurlHttpClient absolute AUserData;
  VStream: TMemoryStream;
begin
  Result := 0;
  if VSelf.FDoDisconnect then begin
    Exit;
  end;
  VStream := VSelf.FResp.Data;
  if VStream <> nil then begin
    Result := VStream.Write(AData^, ASize * ANmemb);
  end;
end;

function CurlProgressCallBack(
  const AClientP: Pointer;
  const ADlTotal: Int64;
  const ADlNow: Int64;
  const AUlTotal: Int64;
  const AUlNow: Int64
): Integer; cdecl;
var
  VSelf: TCurlHttpClient absolute AClientP;
begin
  if not VSelf.FDoDisconnect then begin
    VSelf.FCurlProgressCallBack(ADlTotal, ADlNow, VSelf.FCurlUserData);
    Result := 0;
  end else begin
    Result := 1;
  end;
end;

function CurlProgressCallBack2(const P: Pointer; const ADlTotal, ADlNow, AUlTotal,
  AUlNow: Double): Integer; cdecl;
begin
  Result := CurlProgressCallBack(P, Trunc(ADlTotal), Trunc(ADlNow),
    Trunc(AUlTotal), Trunc(AUlNow));
end;

{$MINENUMSIZE 4}
type
  TCurlInfoType = (
    citText = 0,
    citHeaderIn,
    citHeaderOut,
    citDataIn,
    citDataOut,
    citSSLDataIn,
    citSSLDataOut,
    citEnd
  );
{$MINENUMSIZE 1}

function CurlDebugCallBack(
  const AHandle: TCurl;
  const AInfoType: TCurlInfoType;
  const AData: PAnsiChar;
  const ASize: Integer;
  const AUserPtr: Pointer
): Integer; cdecl;
const
  cIdent: array [citText..citHeaderOut] of RawByteString = ('* ', '< ', '> ');
var
  VSelf: TCurlHttpClient absolute AUserPtr;
  VStr: RawByteString;
  VPtr: PAnsiChar;
  VData: AnsiString;
begin
  Result := 0;
  if AInfoType in [citText, citHeaderIn, citHeaderOut] then begin
    SetString(VData, AData, ASize);
    VPtr := Pointer(VData);
    while VPtr <> nil do begin
      GetNextLine(VPtr, VStr);
      VSelf.FCurlDebugCallBack(
        cIdent[AInfoType] + VStr,
        VSelf.FCurlUserData
      );
    end;
  end;
end;

{ TCurlHttpClient }

constructor TCurlHttpClient.Create(
  const ACertFileName: AnsiString;
  const AProgressCallBack: TCurlProgressCallBack;
  const ADebugCallBack: TCurlDebugCallBack;
  const AUserData: Pointer
);
begin
  inherited Create;

  FCurlProgressCallBack := AProgressCallBack;
  FCurlDebugCallBack := ADebugCallBack;
  FCurlUserData := AUserData;

  FCurl := nil;
  FCurlSlist := nil;

  FReqMethod := rmGet;
  FReqHeaders := '';

  FCertFileName := ACertFileName;
  if (FCertFileName <> '') and not FileExists(string(FCertFileName)) then begin
    raise Exception.CreateFmt(
      'Can''t find curl certificate: %s', [FCertFileName]
    );
  end;

  FOptions := cCurlDefaultOptions;
  FProxy := cCurlDefaultProxy;

  if curl.Module = 0 then begin
    LibCurlInitialize;
  end;
end;

destructor TCurlHttpClient.Destroy;
begin
  CurlClose;
  CurlClearSlist(FCurlSlist);
  inherited Destroy;
end;

class function TCurlHttpClient.CurlAuthType(const AType: TCurlAuthType): Cardinal;
begin
  // https://curl.haxx.se/libcurl/c/CURLOPT_HTTPAUTH.html
  case AType of
    atAny       : Result := $FFFFFFFF {xor 16};
    atBasic     : Result := 1;
    atDigest    : Result := 2;
    atNegotiate : Result := 4;
    atNtlm      : Result := 8;
    atDigestIE  : Result := 16;
    atNtlmWB    : Result := 32;
    atBearer    : Result := 64;
  else
    raise ECurl.CreateFmt('Unexpected auth type value: %d', [Integer(AType)]);
  end;
end;

procedure TCurlHttpClient.CurlCheck(const AResult: TCurlResult);
begin
  // https://curl.haxx.se/libcurl/c/libcurl-errors.html
  {$IFDEF DEBUG}
  if AResult <> crOK then begin
    raise ECurl.CreateFmt(
      'libcurl error %d (%s)', [Ord(AResult), curl.easy_strerror(AResult)]
    );
  end;
  {$ENDIF}
end;

procedure TCurlHttpClient.CurlClose;
begin
  if FCurl <> nil then begin
    curl.easy_cleanup(FCurl);
    FCurl := nil;
  end;
end;

procedure TCurlHttpClient.CurlSetup;

  function _GetCurlUserPwd: RawByteString;
  begin
    Result := URLEncode(FProxy.UserName);
    if (Result <> '') and (FProxy.UserPass <> '') then begin
      Result := Result + ':' + URLEncode(FProxy.UserPass);
    end;
  end;

const
  cBoolToLong: array [Boolean] of Integer = (0, 1);
begin
  CurlCheck( curl.easy_setopt(FCurl, coTimeoutMS, FOptions.TimeOutMS) );
  CurlCheck( curl.easy_setopt(FCurl, coConnectTimeoutMS, FOptions.ConnectionTimeOutMS) );
  CurlCheck( curl.easy_setopt(FCurl, coFollowLocation, cBoolToLong[FOptions.FollowLocation]) );

  if FOptions.IgnoreSSLCertificateErrors then begin
    CurlCheck( curl.easy_setopt(FCurl, coSSLVerifyPeer, 0) );
    CurlCheck( curl.easy_setopt(FCurl, coSSLVerifyHost, 0) );
  end else
  if FCertFileName <> '' then begin
    CurlCheck( curl.easy_setopt(FCurl, coCAInfo, Pointer(FCertFileName)) );
  end;

  if FOptions.StoreCookie then begin
    CurlCheck( curl.easy_setopt(FCurl, coCookieFile, PAnsiChar('')) );
  end;

  if FOptions.AcceptEncoding then begin
    CurlCheck( curl.easy_setopt(FCurl, coAcceptEncoding, PAnsiChar('')) );
  end else begin
    CurlCheck( curl.easy_setopt(FCurl, coAcceptEncoding, nil) );
  end;

  CurlCheck( curl.easy_setopt(FCurl, coProxy, PAnsiChar(FProxy.Address)) );
  CurlCheck( curl.easy_setopt(FCurl, coNoProxy, PAnsiChar(FProxy.NoProxy)) );
  CurlCheck( curl.easy_setopt(FCurl, coProxyAuth, CurlAuthType(FProxy.AuthType)) );
  CurlCheck( curl.easy_setopt(FCurl, coProxyUserPwd, PAnsiChar(_GetCurlUserPwd)) );

  CurlCheck( curl.easy_setopt(FCurl, coHeaderFunction, @CurlWriteHeaderCallBack) );
  CurlCheck( curl.easy_setopt(FCurl, coWriteHeader, Self) );

  CurlCheck( curl.easy_setopt(FCurl, coWriteFunction, @CurlWriteDataCallBack) );
  CurlCheck( curl.easy_setopt(FCurl, coWriteData, Self) );

  if Assigned(FCurlProgressCallBack) then begin
    if curl.info.version_num > $072000 then begin
      // for libcurl newer than 7.32.0
      CurlCheck( curl.easy_setopt(FCurl, coXferInfoFunction, @CurlProgressCallBack) );
      CurlCheck( curl.easy_setopt(FCurl, coXferInfoData, Self) );
    end else begin
      CurlCheck( curl.easy_setopt(FCurl, coProgressFunction, @CurlProgressCallBack2) );
      CurlCheck( curl.easy_setopt(FCurl, coProgressData, Self) );
    end;
    CurlCheck( curl.easy_setopt(FCurl, coNoProgress, 0) );
  end;

  if Assigned(FCurlDebugCallBack) then begin
    CurlCheck( curl.easy_setopt(FCurl, coDebugFunction, @CurlDebugCallBack) );
    CurlCheck( curl.easy_setopt(FCurl, coDebugData, Self) );
    CurlCheck( curl.easy_setopt(FCurl, coVerbose, 1) );
  end;
end;

class procedure TCurlHttpClient.CurlUpdateSlist(
  const ARawHeaders: RawByteString;
  var ASlist: Pointer
);
var
  VPtr: PAnsiChar;
  VLine: RawByteString;
begin
  VPtr := Pointer(ARawHeaders);
  while VPtr <> nil do begin
    GetNextLine(VPtr, VLine);
    if VLine <> '' then begin
      ASlist := curl.slist_append(ASlist, Pointer(VLine));
    end;
  end;
end;

class procedure TCurlHttpClient.CurlClearSlist(var ASlist: Pointer);
begin
  if ASlist <> nil then begin
    curl.slist_free_all(ASlist);
    ASlist := nil;
  end;
end;

function TCurlHttpClient.IsOptionsChanged(const AOptions: PCurlOptions): Boolean;
begin
  Result := (AOptions <> nil) and (FOptions <> AOptions^);
end;

function TCurlHttpClient.IsProxyChanged(const AProxy: PCurlProxy): Boolean;
begin
  Result := (AProxy <> nil) and (FProxy <> AProxy^);
end;

function TCurlHttpClient.DoRequest(
  const AReq: PCurlRequest;
  const AResp: PCurlResponse
): Boolean;
const
  cMethod: array [TReqMethod] of RawByteString = ('HEAD', 'GET', 'POST');
var
  VMethod: RawByteString;
  VCurlResult: TCurlResult;
  VDoCurlSetup: Boolean;
begin
  Result := False;
  FDoDisconnect := False;

  FReq := AReq;
  FResp := AResp;

  FResp.Code := 0;
  FResp.Headers := '';
  if FResp.Data <> nil then begin
    FResp.Data.Clear;
  end;
  FResp.ErrorReason := '';

  if FCurl = nil then begin
    FCurl := curl.easy_init;
    VDoCurlSetup := True;
    if FCurl = nil then begin
      FResp.ErrorReason := 'Unexpected result of curl_easy_init()';
      Exit;
    end;
  end else begin
    VDoCurlSetup := IsOptionsChanged(FReq.Options) or IsProxyChanged(FReq.Proxy);
    if VDoCurlSetup then begin
      curl.easy_reset(FCurl);
    end;
  end;

  if VDoCurlSetup then begin
    if FReq.Options <> nil then begin
      FOptions := FReq.Options^;
    end else begin
      FOptions := cCurlDefaultOptions;
    end;
    if FReq.Proxy <> nil then begin
      FProxy := FReq.Proxy^;
    end else begin
      FProxy := cCurlDefaultProxy;
    end;
    CurlSetup;
  end;

  if FReq.Method = rmHead then begin
    CurlCheck( curl.easy_setopt(FCurl, coNoBody, 1) );
  end else begin
    CurlCheck( curl.easy_setopt(FCurl, coNoBody, 0) );
  end;

  VMethod := cMethod[FReq.Method];
  CurlCheck( curl.easy_setopt(FCurl, coCustomRequest, Pointer(VMethod)) );

  CurlCheck( curl.easy_setopt(FCurl, coUrl, Pointer(FReq.Url)) );

  if FReq.Method = rmPOST then begin
    CurlCheck( curl.easy_setopt(FCurl, coPostFields, FReq.PostData) );
    CurlCheck( curl.easy_setopt(FCurl, coPostFieldSize, FReq.PostDataSize) );
  end;

  if (FReq.Headers <> FReqHeaders) or (FReq.Method <> FReqMethod) then begin
    CurlClearSlist(FCurlSlist);

    if FReq.Method = rmPOST then begin
      FCurlSlist := curl.slist_append(FCurlSlist, 'Expect:');
    end;

    CurlUpdateSlist(FReq.Headers, FCurlSlist);

    CurlCheck( curl.easy_setopt(FCurl, coHTTPHeader, FCurlSlist) );

    FReqMethod := FReq.Method;
    FReqHeaders := FReq.Headers;
  end;

  VCurlResult := curl.easy_perform(FCurl);

  if (VCurlResult <> crOK) and not FDoDisconnect then begin
    if (FResp.Code = 0) and (FResp.Headers <> '') then begin
      FResp.Code := GetResponseCode(FResp.Headers);
    end;
    FResp.ErrorReason := Format(
      'curl error #%d (%s) on %s %s',
      [Ord(VCurlResult), curl.easy_strerror(VCurlResult), VMethod, FReq.Url]
    );
    Exit;
  end;

  CurlCheck( curl.easy_getinfo(FCurl, ciResponseCode, FResp.Code) );

  if not FDoDisconnect then begin
    Result := True;
  end else begin
    FResp.ErrorReason := 'Disconnected by user';
    CurlClose;
  end;
end;

procedure TCurlHttpClient.Disconnect;
begin
  FDoDisconnect := True;
end;

class function TCurlHttpClient.GetCurlVersionInfo: string;
begin
  if curl.Module = 0 then begin
    LibCurlInitialize;
  end;
  Result := curl.infoText;
end;

procedure WinSockInitialize;
var
  VRet: Integer;
  VData: WSAData;
begin
  FillChar(VData, SizeOf(VData), 0);
  VRet := WSAStartup($0202, VData);
  if VRet <> 0 then begin
    raise Exception.CreateFmt('WSAStartup failed with error: %d', [VRet]);
  end;
end;

initialization
  WinSockInitialize;

finalization
  WSACleanup;

end.

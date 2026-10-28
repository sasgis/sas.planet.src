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

unit u_CurlHttpClient;

interface

uses
  Classes,
  mormot.lib.curl,
  t_CurlHttpClient,
  u_CurlLibrary;

type
  TCurlHttpClient = class
  private
    FCurl: TCurl;
    FCurlSlist: Pointer;
    FCertFileName: AnsiString;
    FDoDisconnect: Boolean;
    FImpersonateTarget: RawByteString;
    FCurlLibraryProvider: ICurlLibraryProvider;
    FCurlLib: TCurlLibrary;

    FReq: PCurlRequest;
    FResp: PCurlResponse;
    FOptions: TCurlOptions;
    FProxy: TCurlProxy;

    FReqMethod: TReqMethod;
    FReqHeaders: RawByteString;
    FReqUserAgent: RawByteString;

    FCurlProgressCallBack: TCurlProgressCallBack;
    FCurlDebugCallBack: TCurlDebugCallBack;
    FCurlUserData: Pointer;

    procedure CurlSetup;
    procedure CurlImpersonateSetup;
    procedure CurlClose; inline;
    procedure CurlCheck(const AResult: TCurlResult); {$IFNDEF DEBUG} inline; {$ENDIF}

    class function CurlAuthType(const AType: TCurlAuthType): Cardinal;
    procedure CurlClearSlist(var ASlist: Pointer);
    procedure CurlUpdateSlist(const ARawHeaders: RawByteString; var ASlist: Pointer);

    function IsOptionsChanged(const AOptions: PCurlOptions): Boolean; inline;
    function IsProxyChanged(const AProxy: PCurlProxy): Boolean; inline;
  public
    function DoRequest(
      const AReq: PCurlRequest;
      const AResp: PCurlResponse
    ): Boolean;
    procedure Disconnect;
  public
    constructor Create(
      const ACertFileName: AnsiString;
      const AProgressCallBack: TCurlProgressCallBack;
      const ADebugCallBack: TCurlDebugCallBack;
      const AUserData: Pointer;
      const ACurlLibraryProvider: ICurlLibraryProvider;
      const AImpersonateTarget: RawByteString
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
  const ASize: NativeUInt;
  const ANitems: NativeUInt;
  const AUserData: Pointer
): NativeUInt; cdecl;
var
  VSelf: TCurlHttpClient absolute AUserData;
  VHeader: PRawByteString;
  VHeaderLen: Integer;
  VLen: NativeUInt;
begin
  // https://curl.se/libcurl/c/CURLOPT_HEADERFUNCTION.html
  Result := 0;
  try
    if VSelf.FDoDisconnect then begin
      Exit;
    end;

    VLen := ASize * ANitems;

    // the buffer is NOT null-terminated, so check the length first
    if (VLen >= 5) and IdemPChar(AData, 'HTTP/') then begin
      // collect the headers of only the last response
      VSelf.FResp.Headers := '';
    end;

    if VLen > 0 then begin
      VHeader := @VSelf.FResp.Headers;
      VHeaderLen := Length(VHeader^);
      SetLength(VHeader^, VHeaderLen + Integer(VLen));
      Move(AData^, PPAnsiChar(VHeader)^[VHeaderLen], VLen);
    end;
    Result := VLen;
  except
    Result := 0; // signals an error to libcurl (CURLE_WRITE_ERROR)
  end;
end;

function CurlWriteDataCallBack(
  const AData: PAnsiChar;
  const ASize: NativeUInt;
  const ANmemb: NativeUInt;
  const AUserData: Pointer
): NativeUInt; cdecl;
var
  VSelf: TCurlHttpClient absolute AUserData;
  VStream: TMemoryStream;
  VLen: NativeUInt;
begin
  // https://curl.se/libcurl/c/CURLOPT_WRITEFUNCTION.html
  Result := 0;
  try
    if VSelf.FDoDisconnect then begin
      Exit;
    end;
    VLen := ASize * ANmemb;
    VStream := VSelf.FResp.Data;
    if VStream <> nil then begin
      Result := NativeUInt(VStream.Write(AData^, Integer(VLen)));
    end else begin
      // no destination stream: just discard the body
      // (returning 0 would abort the transfer with CURLE_WRITE_ERROR)
      Result := VLen;
    end;
  except
    Result := 0;
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
  // https://curl.se/libcurl/c/CURLOPT_XFERINFOFUNCTION.html
  try
    if not VSelf.FDoDisconnect then begin
      VSelf.FCurlProgressCallBack(ADlTotal, ADlNow, VSelf.FCurlUserData);
      Result := 0;
    end else begin
      Result := 1;
    end;
  except
    Result := 1; // abort the transfer
  end;
end;

function CurlProgressCallBack2(const P: Pointer; const ADlTotal, ADlNow, AUlTotal,
  AUlNow: Double): Integer; cdecl;
begin
  // https://curl.se/libcurl/c/CURLOPT_PROGRESSFUNCTION.html
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
  const ASize: NativeUInt;
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
  try
    if AInfoType in [citText, citHeaderIn, citHeaderOut] then begin
      SetString(VData, AData, Integer(ASize));
      VPtr := Pointer(VData);
      while VPtr <> nil do begin
        GetNextLine(VPtr, VStr);
        VSelf.FCurlDebugCallBack(
          cIdent[AInfoType] + VStr,
          VSelf.FCurlUserData
        );
      end;
    end;
  except
    // never let an exception go through libcurl
  end;
end;

{ TCurlHttpClient }

constructor TCurlHttpClient.Create(
  const ACertFileName: AnsiString;
  const AProgressCallBack: TCurlProgressCallBack;
  const ADebugCallBack: TCurlDebugCallBack;
  const AUserData: Pointer;
  const ACurlLibraryProvider: ICurlLibraryProvider;
  const AImpersonateTarget: RawByteString
);
begin
  Assert(ACurlLibraryProvider <> nil);

  inherited Create;

  FCurlProgressCallBack := AProgressCallBack;
  FCurlDebugCallBack := ADebugCallBack;
  FCurlUserData := AUserData;

  FCurl := nil;
  FCurlSlist := nil;

  FReqMethod := rmGet;

  FImpersonateTarget := AImpersonateTarget;

  // keep the provider alive for the whole client lifetime; the shared
  // library instance is owned (and freed) by the provider
  FCurlLibraryProvider := ACurlLibraryProvider;
  FCurlLib := FCurlLibraryProvider.GetLibrary(FImpersonateTarget <> '');

  FCertFileName := ACertFileName;
  if (FCertFileName <> '') and not FileExists(string(FCertFileName)) then begin
    raise ECurl.CreateFmt('Can''t find curl certificate: %s', [FCertFileName]);
  end;

  FOptions := cCurlDefaultOptions;
  FProxy := cCurlDefaultProxy;
end;

destructor TCurlHttpClient.Destroy;
begin
  CurlClose;
  CurlClearSlist(FCurlSlist);
  FCurlLib := nil;
  FCurlLibraryProvider := nil;
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
    raise ECurl.CreateFmt('libcurl error %d (%s)', [Ord(AResult), FCurlLib.easy_strerror(AResult)]);
  end;
  {$ENDIF}
end;

procedure TCurlHttpClient.CurlClose;
begin
  if FCurl <> nil then begin
    FCurlLib.easy_cleanup(FCurl);
    FCurl := nil;
  end;
end;

procedure TCurlHttpClient.CurlImpersonateSetup;
var
  VCurlResult: TCurlResult;
begin
  if not Assigned(FCurlLib.easy_impersonate) then begin
    raise ECurl.Create('curl_easy_impersonate is not available!');
  end;

  VCurlResult := FCurlLib.easy_impersonate(FCurl, PAnsiChar(FImpersonateTarget), 1);
  if VCurlResult <> crOK then begin
    raise ECurl.CreateFmt(
      'curl_easy_impersonate(''%s'') error #%d (%s)',
      [string(FImpersonateTarget), Ord(VCurlResult), FCurlLib.easy_strerror(VCurlResult)]
    );
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
  if FImpersonateTarget <> '' then begin
    CurlImpersonateSetup;
  end;

  CurlCheck( FCurlLib.easy_setopt(FCurl, coTimeoutMS, FOptions.TimeOutMS) );
  CurlCheck( FCurlLib.easy_setopt(FCurl, coConnectTimeoutMS, FOptions.ConnectionTimeOutMS) );
  CurlCheck( FCurlLib.easy_setopt(FCurl, coFollowLocation, cBoolToLong[FOptions.FollowLocation]) );

  if FCertFileName <> '' then begin
    CurlCheck( FCurlLib.easy_setopt(FCurl, coCAInfo, PAnsiChar(FCertFileName)) );
  end;

  if FOptions.IgnoreSSLCertificateErrors then begin
    CurlCheck( FCurlLib.easy_setopt(FCurl, coSSLVerifyPeer, 0) );
    CurlCheck( FCurlLib.easy_setopt(FCurl, coSSLVerifyHost, 0) );
  end;

  if FOptions.StoreCookie then begin
    CurlCheck( FCurlLib.easy_setopt(FCurl, coCookieFile, PAnsiChar('')) );
  end;

  if FOptions.AcceptEncoding then begin
    CurlCheck( FCurlLib.easy_setopt(FCurl, coAcceptEncoding, PAnsiChar('')) );
  end else begin
    CurlCheck( FCurlLib.easy_setopt(FCurl, coAcceptEncoding, nil) );
  end;

  CurlCheck( FCurlLib.easy_setopt(FCurl, coProxy, PAnsiChar(FProxy.Address)) );
  CurlCheck( FCurlLib.easy_setopt(FCurl, coNoProxy, PAnsiChar(FProxy.NoProxy)) );
  CurlCheck( FCurlLib.easy_setopt(FCurl, coProxyAuth, CurlAuthType(FProxy.AuthType)) );
  CurlCheck( FCurlLib.easy_setopt(FCurl, coProxyUserPwd, PAnsiChar(_GetCurlUserPwd)) );

  CurlCheck( FCurlLib.easy_setopt(FCurl, coHeaderFunction, @CurlWriteHeaderCallBack) );
  CurlCheck( FCurlLib.easy_setopt(FCurl, coWriteHeader, Self) );

  CurlCheck( FCurlLib.easy_setopt(FCurl, coWriteFunction, @CurlWriteDataCallBack) );
  CurlCheck( FCurlLib.easy_setopt(FCurl, coWriteData, Self) );

  if Assigned(FCurlProgressCallBack) then begin
    if FCurlLib.VersionNum >= $072000 then begin
      // for libcurl newer than 7.32.0
      CurlCheck( FCurlLib.easy_setopt(FCurl, coXferInfoFunction, @CurlProgressCallBack) );
      CurlCheck( FCurlLib.easy_setopt(FCurl, coXferInfoData, Self) );
    end else begin
      // deprecated since 7.32.0
      CurlCheck( FCurlLib.easy_setopt(FCurl, coProgressFunction, @CurlProgressCallBack2) );
      CurlCheck( FCurlLib.easy_setopt(FCurl, coProgressData, Self) );
    end;
    CurlCheck( FCurlLib.easy_setopt(FCurl, coNoProgress, 0) );
  end;

  if Assigned(FCurlDebugCallBack) then begin
    CurlCheck( FCurlLib.easy_setopt(FCurl, coDebugFunction, @CurlDebugCallBack) );
    CurlCheck( FCurlLib.easy_setopt(FCurl, coDebugData, Self) );
    CurlCheck( FCurlLib.easy_setopt(FCurl, coVerbose, 1) );
  end;
end;

procedure TCurlHttpClient.CurlUpdateSlist(
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
      ASlist := FCurlLib.slist_append(ASlist, Pointer(VLine));
    end;
  end;
end;

procedure TCurlHttpClient.CurlClearSlist(var ASlist: Pointer);
begin
  if ASlist <> nil then begin
    FCurlLib.slist_free_all(ASlist);
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
  if not FCurlLib.Available then begin
    raise ECurl.CreateFmt('Curl library is not available: %s', [FCurlLib.LoadError]);
  end;

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
    FCurl := FCurlLib.easy_init;
    VDoCurlSetup := True;
    if FCurl = nil then begin
      FResp.ErrorReason := 'Unexpected result of curl_easy_init()';
      Exit;
    end;
  end else begin
    VDoCurlSetup :=
      IsOptionsChanged(FReq.Options) or
      IsProxyChanged(FReq.Proxy) or
      (FReq.UserAgent <> FReqUserAgent) or
      (FReq.Method <> FReqMethod);
    if VDoCurlSetup then begin
      FCurlLib.easy_reset(FCurl);
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
    CurlCheck( FCurlLib.easy_setopt(FCurl, coNoBody, 1) );
  end else begin
    CurlCheck( FCurlLib.easy_setopt(FCurl, coNoBody, 0) );
  end;

  if FReq.Method = rmPOST then begin
    if (FReq.PostData = nil) and (FReq.PostDataSize > 0) then begin
      FResp.ErrorReason := 'POST data is nil, but size > 0';
      Exit;
    end;
    if FReq.PostData <> nil then begin
      CurlCheck( FCurlLib.easy_setopt(FCurl, coPostFields, FReq.PostData) );
    end else begin
      CurlCheck( FCurlLib.easy_setopt(FCurl, coPostFields, PAnsiChar('')) );
    end;
    CurlCheck( FCurlLib.easy_setopt(FCurl, coPostFieldSize, FReq.PostDataSize) );
  end;

  VMethod := cMethod[FReq.Method];
  CurlCheck( FCurlLib.easy_setopt(FCurl, coCustomRequest, Pointer(VMethod)) );

  CurlCheck( FCurlLib.easy_setopt(FCurl, coUrl, Pointer(FReq.Url)) );

  if (FReq.Method <> FReqMethod) or (FReq.Headers <> FReqHeaders) then begin
    CurlClearSlist(FCurlSlist);

    if FReq.Method = rmPOST then begin
      FCurlSlist := FCurlLib.slist_append(FCurlSlist, 'Expect:');
    end;

    CurlUpdateSlist(FReq.Headers, FCurlSlist);

    CurlCheck( FCurlLib.easy_setopt(FCurl, coHTTPHeader, FCurlSlist) );

    FReqMethod := FReq.Method;
    FReqHeaders := FReq.Headers;
    FReqUserAgent := FReq.UserAgent;
  end;

  VCurlResult := FCurlLib.easy_perform(FCurl);

  if (VCurlResult <> crOK) and not FDoDisconnect then begin
    if (FResp.Code = 0) and (FResp.Headers <> '') then begin
      FResp.Code := GetResponseCode(FResp.Headers);
    end;
    FResp.ErrorReason := Format(
      'curl error #%d (%s) on %s %s',
      [Ord(VCurlResult), FCurlLib.easy_strerror(VCurlResult), VMethod, FReq.Url]
    );
    Exit;
  end;

  CurlCheck( FCurlLib.easy_getinfo(FCurl, ciResponseCode, FResp.Code) );

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

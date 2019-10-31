unit u_CurlHttpClient;

interface

uses
  Classes,
  SynCurl;

type
  {$IFNDEF UNICODE}
  RawByteString = type AnsiString;
  {$ENDIF}

  TReqMethod = (rmHead, rmGet, rmPost);

  TCurlOptions = record
    StoreCookie: Boolean;
    FollowLocation: Boolean;
    AcceptEncoding: Boolean;
    IgnoreSSLCertificateErrors: Boolean;
    TimeOut: Integer;
    ConnectionTimeOut: Integer;
  end;
  PCurlOptions = ^TCurlOptions;

  TCurlRequest = record
    Method: TReqMethod;
    Url: RawByteString;
    Headers: RawByteString;
    PostData: Pointer;
    PostDataSize: Integer;
    Options: PCurlOptions;
  end;
  PCurlRequest = ^TCurlRequest;

  TCurlResponse = record
    Code: Integer;
    Headers: RawByteString;
    Data: TMemoryStream;
    ErrorReason: string;
  end;
  PCurlResponse = ^TCurlResponse;

  TCurlDebugCallBack = procedure(
    const ADebugMsg: RawByteString;
    const AUserData: Pointer
  );

  TCurlProgressCallBack = procedure(
    const ATotal: Integer;
    const ADownload: Integer;
    const AUserData: Pointer
  );

  TCurlHttpClient = class
  private
    FCurl: TCurl;
    FCurlSlist: Pointer;
    FCertFileName: AnsiString;
    FDoDisconnect: Boolean;

    FReq: PCurlRequest;
    FResp: PCurlResponse;
    FOptions: TCurlOptions;

    FReqMethod: TReqMethod;
    FReqHeaders: RawByteString;

    FCurlProgressCallBack: TCurlProgressCallBack;
    FCurlDebugCallBack: TCurlDebugCallBack;
    FCurlUserData: Pointer;

    procedure CurlSetup;
    procedure CurlClose; inline;
    procedure CurlCheck(const AResult: TCurlResult); inline;

    class procedure CurlClearSlist(var ASlist: Pointer);
    class procedure CurlUpdateSlist(const ARawHeaders: RawByteString; var ASlist: Pointer);

    function IsOptionsChanged(const AOptions: PCurlOptions): Boolean; inline;
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

implementation

uses
  WinSock,
  SysUtils;

procedure GetNextLine(var P: PAnsiChar; var AResult: RawByteString);
var
  S: PAnsiChar;
begin
  if P = nil then begin
    AResult := '';
  end else begin
    S := P;
    while S^ >= ' ' do begin // break on any control char
      Inc(S);
    end;
    SetString(AResult, P, S-P);
    while (S^ <> #0) and (S^ < ' ') do begin // ignore e.g. #13 or #10
      Inc(S);
    end;
    if S^ <> #0 then begin
      P := S;
    end else begin
      P := nil;
    end;
  end;
end;

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

  FOptions.StoreCookie := True;
  FOptions.FollowLocation := True;
  FOptions.AcceptEncoding := True;
  FOptions.IgnoreSSLCertificateErrors := False;
  FOptions.TimeOut := 30; // seconds
  FOptions.ConnectionTimeOut := 30; // seconds

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

procedure TCurlHttpClient.CurlCheck(const AResult: TCurlResult);
begin
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

function TCurlHttpClient.IsOptionsChanged(const AOptions: PCurlOptions): Boolean;
begin
  if AOptions <> nil then begin
    Result :=
      (FOptions.StoreCookie <> AOptions.StoreCookie) or
      (FOptions.FollowLocation <> AOptions.FollowLocation) or
      (FOptions.IgnoreSSLCertificateErrors <> AOptions.IgnoreSSLCertificateErrors) or
      (FOptions.TimeOut <> AOptions.TimeOut) or
      (FOptions.ConnectionTimeOut <> AOptions.ConnectionTimeOut);
  end else begin
    Result := False;
  end;
end;

procedure TCurlHttpClient.CurlSetup;
const
  cBoolToLong: array [Boolean] of Integer = (0, 1);
begin
  CurlCheck( curl.easy_setopt(FCurl, coTimeout, FOptions.TimeOut) );
  CurlCheck( curl.easy_setopt(FCurl, coConnectTimeout, FOptions.ConnectionTimeOut) );
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

  CurlCheck( curl.easy_setopt(FCurl, coHeaderFunction, @CurlWriteHeaderCallBack) );
  CurlCheck( curl.easy_setopt(FCurl, coWriteHeader, Self) );

  CurlCheck( curl.easy_setopt(FCurl, coWriteFunction, @CurlWriteDataCallBack) );
  CurlCheck( curl.easy_setopt(FCurl, coWriteData, Self) );

  if Assigned(FCurlProgressCallBack) then begin
    CurlCheck( curl.easy_setopt(FCurl, TCurlOption(coXferInfoFunction), @CurlProgressCallBack) );
    CurlCheck( curl.easy_setopt(FCurl, coXferInfoData, Self) );
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

function TCurlHttpClient.DoRequest(
  const AReq: PCurlRequest;
  const AResp: PCurlResponse
): Boolean;
const
  cMethod: array [TReqMethod] of RawByteString = ('HEAD', 'GET', 'POST');
var
  VMethod: RawByteString;
  VCurlResult: TCurlResult;
begin
  Result := False;
  FDoDisconnect := False;

  FReq := AReq;
  FResp := AResp;

  FResp.Code := 0;
  FResp.Headers := '';
  FResp.ErrorReason := '';

  if FCurl = nil then begin
    FCurl := curl.easy_init;
    if FCurl = nil then begin
      FResp.ErrorReason := 'Unexpected result of curl_easy_init()';
      Exit;
    end;
    if FReq.Options <> nil then begin
      FOptions := FReq.Options^;
    end;
    CurlSetup;
  end else begin
    if IsOptionsChanged(FReq.Options) then begin
      FOptions := FReq.Options^;
      CurlSetup;
    end;
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
    FResp.ErrorReason := Format(
      'curl_easy_perform() error %d (%s) on %s %s',
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

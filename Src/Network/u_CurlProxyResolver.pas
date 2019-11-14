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

unit u_CurlProxyResolver;

interface

uses
  t_CurlHttpClient,
  u_WindowsCredentials,
  u_InternetExplorerProxy;

type
  TCurlProxyResolver = class
  private
    type
      TRecState = (rsEmpty, rsOk, rsFail);

      TInfoRec = record
        Scheme: string;
        Host: string;
        Info: TInternetExplorerProxyInfo;
        State: TRecState;
      end;
      PInfoRec = ^TInfoRec;

      TAuthRec = record
        ProxyAddress: RawByteString;
        UserName: string;
        UserPass: string;
        State: TRecState;
      end;
      PAuthRec = ^TAuthRec;
  private
    FInfo: array of TInfoRec;
    FCredentials: TWindowsCredentials;
  private
    class procedure SetCurlProxy(
      const ARequestScheme: string;
      const ARequestHost: string;
      const AProxyList: string;
      const AProxyBypassList: string;
      out ACurlProxy: RawByteString;
      out ACurlNoProxy: RawByteString
    );
  public
    procedure Reset;

    procedure DoResolveProxy(const AUrl: string; var ACurlProxy: TCurlProxy);

    procedure DoResolveProxyAuth(
      const ACaption: string;
      const ADescription: string;
      var ACurlProxy: TCurlProxy
    );

    class function GetProxyAuthenticateInfo(
      const AResponseHeaders: RawByteString;
      out ACurlAuthType: TCurlAuthType;
      out AShemeName: string;
      out ARealm: string
    ): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SyncObjs,
  StrUtils,
  SysUtils,
  u_NetworkStrFunc;

var
  GAuth: TCurlProxyResolver.TAuthRec;
  GLock: TCriticalSection;

type
  TStringArray = array of string;

  TProxyItem = record
    Scheme: string;
    Address: string;
  end;

function StringArrayToString(
  const AArr: TStringArray;
  const ASep: RawByteString = ','
): RawByteString;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(AArr) - 1 do begin
    if Result <> '' then begin
      Result := Result + ASep + RawByteString(AArr[I]);
    end else begin
      Result := RawByteString(AArr[I]);
    end;
  end;
end;

procedure ParseRequest(const AUrl: string; out AScheme: string; out AHost: string);
var
  I, J: Integer;
begin
  I := Pos('://', AUrl);
  if I > 0 then begin
    AScheme := LowerCase(Copy(AUrl, 1, I-1));
    Inc(I, 3);
    J := PosEx('/', AUrl, I);
    if J > 0 then begin
      AHost := LowerCase(Copy(AUrl, I, J-I));
    end else begin
      AHost := LowerCase(Copy(AUrl, I));
    end;
  end else begin
    raise Exception.Create('Error parsing request scheme for url: ' + AUrl);
  end;
end;

function SplitSpaceSemicolon(const ASource: string): TStringArray;
var
  I: Integer;
  P, S: PChar;
begin
  Result := nil;

  if ASource = '' then begin
    Exit;
  end;

  I := 0;
  S := PChar(ASource);

  while S^ <> #0 do begin
    P := S;

    while (S^ > ' ') and (S^ <> ';') do begin
      Inc(S);
    end;

    SetLength(Result, I+1);
    SetString(Result[I], P, S-P);
    Result[I] := LowerCase(Result[I]);
    Inc(I);

    while (S^ <> #0) and ((S^ <= ' ') or (S^ = ';')) do begin
      Inc(S);
    end;
  end;
end;

function ParseProxyItem(const AStr: string; out AItem: TProxyItem): Boolean;
var
  I: Integer;
begin
  I := Pos('=', AStr);
  Result := (I > 0);
  if Result then begin
    AItem.Scheme := LowerCase(Copy(AStr, 1, I-1));
    AItem.Address := LowerCase(Copy(AStr, I+1));
  end;
end;

function MatchPattern(AStr, APattern: PChar): Boolean;
begin
  while True do begin

    case APattern[0] of
      #0: begin
        // end of pattern reached
        Result := AStr[0] = #0; // True if end of Str
        Exit;
      end;

      '*': begin
        // Match zero or more occurances of any char
        if APattern[1] = #0 then begin
          // Match any number of trailing chars
          Result := True;
          Exit;
        end else begin
          Inc(APattern);
        end;

        while AStr[0] <> #0 do begin
          // Try to match any substring of Str
          if MatchPattern(AStr, APattern) then begin
            Result := True;
            Exit;
          end;
          // Continue testing next char
          Inc(AStr);
        end;
      end;

    else
      // Match given single char
      begin
        if AStr[0] <> APattern[0] then begin
          Result := False;
          Exit;
        end;
        // Continue testing next char
        Inc(AStr);
        Inc(APattern);
      end;
    end;
  end;
end;

function IsBypassed(const AHost: string; const ABypass: string): Boolean; inline;
begin
  if Pos('*', ABypass) > 0 then begin
    Result := MatchPattern(PChar(AHost), PChar(ABypass));
  end else begin
    Result := False;
  end;
end;

function IsLocalIntranet(const AHost: string): Boolean; inline;
begin
  Result := Pos('.', AHost) = 0;
end;

{ TCurlProxyResolver }

constructor TCurlProxyResolver.Create;
begin
  inherited Create;
  FCredentials := TWindowsCredentials.Create;
  Reset;
end;

destructor TCurlProxyResolver.Destroy;
begin
  FreeAndNil(FCredentials);
  inherited Destroy;
end;

class procedure TCurlProxyResolver.SetCurlProxy(
  const ARequestScheme: string;
  const ARequestHost: string;
  const AProxyList: string;
  const AProxyBypassList: string;
  out ACurlProxy: RawByteString;
  out ACurlNoProxy: RawByteString
);
var
  I, J, K: Integer;
  VProxy: TStringArray;
  VBypass: TStringArray;
  VProxyItems: array of TProxyItem;
  VCurlProxy: string;
  VCurlBypass: TStringArray;
begin
  VCurlProxy := '';
  VCurlBypass := nil;

  if AProxyList <> '' then begin

    // https://docs.microsoft.com/en-us/windows/win32/api/winhttp/ns-winhttp-winhttp_proxy_info
    // The proxy server list contains one or more of the following strings
    // separated by semicolons or whitespace.
    // ([<scheme>=][<scheme>"://"]<server>[":"<port>])

    VProxy := SplitSpaceSemicolon(AProxyList);
    K := Length(VProxy);

    J := 0;
    SetLength(VProxyItems, K);

    for I := 0 to K - 1 do begin
      if ParseProxyItem(VProxy[I], VProxyItems[J]) then begin
        Inc(J);
      end;
    end;

    SetLength(VProxyItems, J);

    if (J = 0) and (K > 0) then begin
      VCurlProxy := VProxy[0];
    end else begin
      for I := 0 to Length(VProxyItems) - 1 do begin
        if VProxyItems[I].Scheme = ARequestScheme then begin
          VCurlProxy := VProxyItems[I].Address;
          Break;
        end;
      end;
    end;

    if (VCurlProxy <> '') and (AProxyBypassList <> '') then begin

      // The proxy bypass list contains one or more server names separated by
      // semicolons or whitespace. The proxy bypass list can also contain the
      // string "<local>" to indicate that all local intranet sites are bypassed.
      // Local intranet sites are considered to be all servers that do not
      // contain a period in their name.

      J := 0;
      VBypass := SplitSpaceSemicolon(AProxyBypassList);
      SetLength(VCurlBypass, Length(VBypass));

      for I := 0 to Length(VBypass) - 1 do begin
        if (VBypass[I] = '<local>') or (VBypass[I] = '<-loopback>') then begin
          if IsLocalIntranet(ARequestHost) then begin
            VCurlProxy := '';
            Break;
          end else begin
            VCurlBypass[J] := 'localhost,127.0.0.1,::1';
            Inc(J);
          end;
        end else if IsBypassed(ARequestHost, VBypass[I]) then begin
          VCurlProxy := '';
          Break;
        end else begin
          VCurlBypass[J] := VBypass[I];
          Inc(J);
        end;
      end;

      SetLength(VCurlBypass, J);
    end;
  end;

  ACurlProxy := RawByteString(VCurlProxy);
  if VCurlProxy <> '' then begin
    ACurlNoProxy := StringArrayToString(VCurlBypass);
  end else begin
    ACurlNoProxy := '';
  end;
end;

procedure TCurlProxyResolver.DoResolveProxy(
  const AUrl: string;
  var ACurlProxy: TCurlProxy
);
var
  I: Integer;
  VInfoRec: PInfoRec;
  VScheme, VHost: string;
  VIsInfoRecFound: Boolean;
begin
  ParseRequest(AUrl, VScheme, VHost);

  FillChar(ACurlProxy, SizeOf(TCurlProxy), 0);

  VInfoRec := @FInfo[0];

  if VInfoRec.State = rsEmpty then begin
    if TInternetExplorerProxy.GetInfo(VInfoRec.Info) then begin
      VInfoRec.State := rsOk;
    end else begin
      VInfoRec.State := rsFail;
    end;
  end;

  if VInfoRec.Info.AutoDetect or (VInfoRec.Info.AutoConfigUrl <> '') then begin

    VIsInfoRecFound := False;
    for I := 1 to Length(FInfo) - 1 do begin
      VInfoRec := @FInfo[I];
      if (VInfoRec.Scheme = VScheme) and (VInfoRec.Host = VHost) then begin
        VIsInfoRecFound := True;
        Break;
      end;
    end;

    if not VIsInfoRecFound then begin
      I := Length(FInfo);
      SetLength(FInfo, I+1);

      VInfoRec := @FInfo[I];

      VInfoRec.Scheme := VScheme;
      VInfoRec.Host := VHost;
      VInfoRec.State := rsFail;

      if TInternetExplorerProxy.GetInfoForUrl(AUrl, VInfoRec.Info) then begin
        VInfoRec.State := rsOk;
      end;
    end;
  end;

  if VInfoRec.State = rsOk then begin
    SetCurlProxy(
      VScheme,
      VHost,
      VInfoRec.Info.Proxy,
      VInfoRec.Info.ProxyBypass,
      ACurlProxy.Address,
      ACurlProxy.NoProxy
    );
  end;
end;

procedure TCurlProxyResolver.DoResolveProxyAuth(
  const ACaption: string;
  const ADescription: string;
  var ACurlProxy: TCurlProxy
);

  procedure SetAuth;
  begin
    ACurlProxy.UserName := AnsiString(GAuth.UserName);
    ACurlProxy.UserPass := AnsiString(GAuth.UserPass);
  end;

var
  VResult: Boolean;
  VSaveResult: Boolean;
begin
  ACurlProxy.UserName := '';
  ACurlProxy.UserPass := '';

  GLock.Acquire;
  try
    if (GAuth.State = rsOk) and (GAuth.ProxyAddress <> ACurlProxy.Address) then begin
      GAuth.State := rsEmpty;
    end;

    if GAuth.State = rsEmpty then begin
      GAuth.UserName := '';
      GAuth.UserPass := '';
      VSaveResult := True;
      try
        VResult :=
          FCredentials.RequestCredentials(
            ACaption,
            ADescription,
            GAuth.UserName,
            GAuth.UserPass,
            VSaveResult
          );
        if not VResult then begin
          // Cancelled by user
          Exit;
        end;
        if VSaveResult then begin
          GAuth.State := rsOk;
          GAuth.ProxyAddress := ACurlProxy.Address;
        end;
        SetAuth;
      except
        GAuth.State := rsFail;
        raise;
      end;
    end else
    if GAuth.State = rsOk then begin
      Assert(GAuth.ProxyAddress = ACurlProxy.Address);
      SetAuth;
    end;
  finally
    GLock.Release;
  end;
end;

class function TCurlProxyResolver.GetProxyAuthenticateInfo(
  const AResponseHeaders: RawByteString;
  out ACurlAuthType: TCurlAuthType;
  out AShemeName, ARealm: string
): Boolean;

  function GetAuthTypeByName(const AName: string): TCurlAuthType;
  begin
    if AName = 'basic' then begin
      Result := atBasic;
    end else
    if AName = 'digest' then begin
      Result := atDigest;
    end else
    if AName = 'bearer' then begin
      Result := atBearer;
    end else
    if AName = 'negotiate' then begin
      Result := atNegotiate;
    end else begin
      Result := atAny;
    end;
  end;

var
  I, J, K: Integer;
  VHeader: RawByteString;
begin
  // Proxy-Authenticate: <type> realm=<realm>

  // Proxy-Authenticate: Basic
  // Proxy-Authenticate: Basic realm="Access to the internal site"

  AShemeName := '';
  ARealm := '';

  VHeader := GetHeaderValue(AResponseHeaders, 'PROXY-AUTHENTICATE');

  if VHeader <> '' then begin
    I := 1;
    K := Length(VHeader) + 1;

    for J := I to K - 1 do begin
      if VHeader[I] <> ' ' then begin
        Inc(I);
      end else begin
        Break;
      end;
    end;

    if I > 1 then begin
      AShemeName := string(Copy(VHeader, 1, I-1));
      while (I < K) and (VHeader[I] = ' ') do begin
        Inc(I);
      end;
      if IdemPChar(@VHeader[I], 'REALM="') then begin
        Inc(I, 7);
        J := I;
        while (I < K) and (VHeader[I] <> '"') do begin
          Inc(I);
        end;
        if VHeader[I] = '"' then begin
          ARealm := string(Copy(VHeader, J, I-J));
        end;
      end;
    end else begin
      AShemeName := string(Copy(VHeader, 1));
    end;
  end;

  ACurlAuthType := GetAuthTypeByName(LowerCase(AShemeName));
  Result := AShemeName <> '';
end;

procedure TCurlProxyResolver.Reset;
begin
  SetLength(FInfo, 1);
  FillChar(FInfo[0], SizeOf(TInfoRec), 0);
end;

initialization
  FillChar(GAuth, SizeOf(GAuth), 0);
  GLock := TCriticalSection.Create;

finalization
  FreeAndNil(GLock);

end.

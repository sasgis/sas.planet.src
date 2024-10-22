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

unit u_NetworkStrFunc;

interface

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

{$IFNDEF UNICODE}
type
  RawByteString = AnsiString;
{$ENDIF}

// https://github.com/synopse/mORMot2/blob/master/src/core/mormot.core.unicode.pas
// returns TRUE if the beginning of p^ is the same as up^
// - ignore case - up^ must be already Upper
// - chars are compared as 7-bit Ansi only (no accentuated characters)
// - if p is nil, will return FALSE
// - if up is nil, will return TRUE
function IdemPChar(p, up: PAnsiChar): Boolean; inline;

procedure GetNextLine(var P: PAnsiChar; var AResult: RawByteString);

function GetHeaderValueUp(const AHeaders: RawByteString; const AUpName: RawByteString): RawByteString;
function ReplaceHeaderValueUp(var AHeaders: RawByteString; const AUpName, AValue: RawByteString): Boolean;
procedure DeleteHeaderValueUp(var AHeaders: RawByteString; const AUpName: RawByteString);
procedure AddHeaderValue(var AHeaders: RawByteString; const AName, AValue: RawByteString);

function GetResponseCode(const AHeaders: RawByteString): Cardinal;

function UrlEncode(const S: AnsiString): AnsiString;
function UrlDecode(const AUrl: string): string;

type
  TNormTable = packed array[AnsiChar] of AnsiChar;
  PNormTable = ^TNormTable;

var
  GNormToUpperAnsi7: TNormTable;
  GNormToUpperAnsi7Ptr: PNormTable;

implementation

uses
  Windows,
  ShLwApi,
  SysUtils;

function IdemPChar(p, up: PAnsiChar): Boolean;
type
  {$IF CompilerVersion <= 18.5}
  NativeUInt = Cardinal;
  {$IFEND}
  PtrUInt = NativeUInt;
var
  {$IFDEF CPUX86}
  table: TNormTable absolute GNormToUpperAnsi7;
  {$ELSE}
  table: PNormTable absolute GNormToUpperAnsi7Ptr; // faster on x86_64
  {$ENDIF CPUX86}
begin
  Result := False;

  if p = nil then begin
    Exit;
  end;

  if up <> nil then begin
    Dec(PtrUInt(p), PtrUInt(up));
    repeat
      if up^ = #0 then begin
        Break;
      end;
      if table[up[PtrUInt(p)]] <> up^ then begin
        Exit;
      end;
      Inc(up);
    until False;
  end;

  Result := True;
end;

procedure GetNextLine(var P: PAnsiChar; var AResult: RawByteString);
var
  S: PAnsiChar;
begin
  if P = nil then begin
    AResult := '';
    Exit;
  end;

  S := P;
  while S^ >= ' ' do begin // break on any control char
    Inc(S);
  end;

  SetString(AResult, P, S-P);

  while (S^ <> #0) and (S^ < ' ') do begin // ignore e.g. #13 or #10
    Inc(S);
  end;

  if S^ <> #0 then begin
    P := S
  end else begin
    P := nil;
  end;
end;

function GetHeaderValueUp(const AHeaders: RawByteString; const AUpName: RawByteString): RawByteString;
var
  I, J, K: Integer;
begin
  Result := '';

  if (AHeaders = '') or (AUpName = '') then begin
    Exit;
  end;

  I := 1;
  repeat
    K := Length(AHeaders) + 1;
    for J := I to K - 1 do begin
      if AHeaders[J] < ' ' then begin
        K := J;
        Break;
      end;
    end;

    if IdemPChar(@AHeaders[I], Pointer(AUpName)) then begin
      Inc(I, Length(AUpName));
      while (I < K) and ((AHeaders[I] = ' ') or (AHeaders[I] = ':')) do begin
        Inc(I);
      end;
      Result := Copy(AHeaders, I, K-I);
      Exit;
    end;

    I := K;
    while AHeaders[I] < ' ' do begin
      if AHeaders[I] = #0 then begin
        Exit
      end else begin
        Inc(I);
      end;
    end;
  until False;
end;

function ReplaceHeaderValueUp(var AHeaders: RawByteString; const AUpName, AValue: RawByteString): Boolean;
var
  I, J, K: Integer;
begin
  Result := False;

  if AHeaders = '' then begin
    Exit;
  end;

  if (AUpName = '') or (AValue = '') then begin
    Assert(False);
    Exit;
  end;

  I := 1;
  repeat
    K := Length(AHeaders) + 1;
    for J := I to K - 1 do begin
      if AHeaders[J] < ' ' then begin
        K := J;
        Break;
      end;
    end;

    if IdemPChar(@AHeaders[I], Pointer(AUpName)) then begin
      Inc(I, Length(AUpName));
      while (I < K) and ((AHeaders[I] = ' ') or (AHeaders[I] = ':')) do begin
        Inc(I);
      end;
      Delete(AHeaders, I, K-I);
      Insert(AValue, AHeaders, I);
      Result := True;
      Exit;
    end;

    I := K;
    while AHeaders[I] < ' ' do begin
      if AHeaders[I] = #0 then begin
        Exit
      end else begin
        Inc(I);
      end;
    end;
  until False;
end;

procedure DeleteHeaderValueUp(var AHeaders: RawByteString; const AUpName: RawByteString);
var
  I, J, K: Integer;
begin
  if (AHeaders = '') or (AUpName = '') then begin
    Exit;
  end;

  I := 1;
  repeat
    K := Length(AHeaders) + 1;
    for J := I to K - 1 do begin
      if AHeaders[J] < ' ' then begin
        K := J;
        Break;
      end;
    end;

    if IdemPChar(@AHeaders[I], Pointer(AUpName)) then begin
      while True do begin
        // delete also ending #13#10
        if (AHeaders[K] = #0) or (AHeaders[K] >= ' ') then begin
          Break
        end else begin
          Inc(K);
        end;
      end;
      Delete(AHeaders, I, K-I);
      Exit;
    end;

    I := K;
    while AHeaders[I] < ' ' do begin
      if AHeaders[I] = #0 then begin
        Exit
      end else begin
        Inc(I);
      end;
    end;
  until False;
end;

procedure AddHeaderValue(var AHeaders: RawByteString; const AName, AValue: RawByteString);
var
  I, J, K: Integer;
begin
  if AHeaders = '' then begin
    AHeaders := AName + ': ' + AValue + #13#10;
  end else begin
    K := Length(AHeaders) + 1;
    J := K;
    for I := K - 1 downto 0 do begin
      if AHeaders[I] < ' ' then begin
        Dec(J);
      end else begin
        Break;
      end;
    end;
    if J = K then begin
      AHeaders := AHeaders + #13#10 + AName + ': ' + AValue + #13#10;
    end else
    if J > 0 then begin
      Insert(#13#10 + AName + ': ' + AValue, AHeaders, J);
    end else begin
      AHeaders := AName + ': ' + AValue + AHeaders;
    end;
  end;
end;

function GetResponseCode(const AHeaders: RawByteString): Cardinal;
var
  P, S: PAnsiChar;
  VCode: RawByteString;
begin
  // HTTP/1.1 200 OK

  Result := 0;

  P := Pointer(AHeaders);
  if not IdemPChar(P, 'HTTP/') then begin
    Exit;
  end;

  S := nil;
  while P^ <> #0 do begin
    if P^ = ' '  then begin
      if S = nil then begin
        S := P;
        Inc(S);
      end else begin
        Break;
      end;
    end;
    Inc(P);
  end;

  if (S <> nil) and (S^ <> #0) then begin
    SetString(VCode, S, P-S);
    Result := StrToIntDef(string(VCode), 0);
  end;
end;

function UrlEncode(const S: AnsiString): AnsiString;

  function DigitToHex(const ADigit: Integer): AnsiChar;
  begin
    case ADigit of
      0..9: begin
        Result := AnsiChar(ADigit + Ord('0'));
      end;
      10..15: begin
        Result := AnsiChar(ADigit - 10 + Ord('A'));
      end;
    else
      Result := '0';
    end;
  end;

var
  I, J: Integer;
  VLen: Integer;
begin
  VLen := 0;
  for I := 1 to Length(S) do begin
    if ((S[I] >= '0') and (S[I] <= '9')) or
      ((S[I] >= 'A') and (S[I] <= 'Z')) or
      ((S[I] >= 'a') and (S[I] <= 'z')) or (S[I] = ' ') or
      (S[I] = '_') or (S[I] = '*') or (S[I] = '-') or (S[I] = '.') then
    begin
      VLen := VLen + 1;
    end else begin
      VLen := VLen + 3;
    end;
  end;
  SetLength(Result, VLen);
  J := 1;
  for I := 1 to Length(S) do begin
    if S[I] = ' ' then begin
      Result[J] := '+';
      Inc(J);
    end else
    if ((S[I] >= '0') and (S[I] <= '9')) or
       ((S[I] >= 'A') and (S[I] <= 'Z')) or
       ((S[I] >= 'a') and (S[I] <= 'z')) or
       (S[I] = '_') or
       (S[I] = '*') or
       (S[I] = '-') or
       (S[I] = '.')
    then begin
      Result[J] := S[I];
      Inc(J);
    end else begin
      Result[J] := '%';
      Result[J + 1] := DigitToHex(Ord(S[I]) div 16);
      Result[J + 2] := DigitToHex(Ord(S[I]) mod 16);
      Inc(J, 3);
    end;
  end;
end;

function UrlDecode(const AUrl: string): string;
var
  VLen: DWORD;
  VRet: Integer;
begin
  Assert(AUrl <> '');

  VLen := Length(AUrl);
  SetLength(Result, VLen);

  VRet := UrlUnescape(PChar(AUrl), PChar(Result), @VLen, 0);

  if VRet = S_OK then begin
    SetLength(Result, VLen);
  end else if VRet = E_POINTER then begin
    SetLength(Result, VLen);
    VRet := UrlUnescape(PChar(AUrl), PChar(Result), @VLen, 0);
    if VRet = S_OK then begin
      SetLength(Result, VLen);
    end else begin
      RaiseLastOSError;
    end;
  end else begin
    RaiseLastOSError;
  end;
end;

procedure Initialize;
var
  I: Integer;
begin
  // initialize internal lookup table
  for I := 0 to 255 do begin
    GNormToUpperAnsi7[AnsiChar(I)] := AnsiChar(I);
  end;
  for I := ord('a') to ord('z') do begin
    Dec(GNormToUpperAnsi7[AnsiChar(I)], 32);
  end;
  GNormToUpperAnsi7Ptr := @GNormToUpperAnsi7;
end;

initialization
  Initialize;

end.

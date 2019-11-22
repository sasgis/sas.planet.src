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

// The code for some functions in this unit is based on the freeware Synopse
// mORMot framework, licensed under a MPL/GPL/LGPL tri-license

unit u_NetworkStrFunc;

interface

{$IFNDEF UNICODE}
type
  RawByteString = AnsiString;
{$ENDIF}

function IdemPChar(const AStr, ASubStr: PAnsiChar): Boolean;

procedure GetNextLine(var P: PAnsiChar; var AResult: RawByteString);

function GetHeaderValueUp(const AHeaders: RawByteString; const AUpName: RawByteString): RawByteString;
function ReplaceHeaderValueUp(var AHeaders: RawByteString; const AUpName, AValue: RawByteString): Boolean;
procedure DeleteHeaderValueUp(var AHeaders: RawByteString; const AUpName: RawByteString);
procedure AddHeaderValue(var AHeaders: RawByteString; const AName, AValue: RawByteString);

function GetResponseCode(const AHeaders: RawByteString): Cardinal;
function URLEncode(const S: AnsiString): AnsiString;

implementation

uses
  SysUtils;

type
  {$IF CompilerVersion <= 18.5}
  NativeUInt = Cardinal;
  {$IFEND}
  TNormToUpper = array [Byte] of Byte;

var
  GNormToUpper: TNormToUpper;

function IdemPChar(const AStr, ASubStr: PAnsiChar): Boolean;
// if the beginning of AStr is same as ASubStr (ignore case - ASubStr must be already Upper)
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt-1] of Byte;
var
  u: Byte;
  p: PByteArray;
  up: PByte;
begin
  Result := False;

  p := Pointer(AStr);
  up := Pointer(ASubStr);

  if p = nil then begin
    Exit;
  end;

  if up = nil then begin
    Result := True;
    Exit;
  end;

  Dec(NativeUInt(p), NativeUInt(up));
  repeat
    u := up^;
    if u = 0 then begin
      Break;
    end;
    if GNormToUpper[p[NativeUInt(up)]] <> u then begin
      Exit;
    end;
    Inc(up);
  until False;

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

function URLEncode(const S: AnsiString): AnsiString;

  function DigitToHex(Digit: Integer): AnsiChar;
  begin
    case Digit of
      0..9:
      begin
        Result := AnsiChar(Digit + Ord('0'));
      end;
      10..15:
      begin
        Result := AnsiChar(Digit - 10 + Ord('A'));
      end;
    else
      Result := '0';
    end;
  end;

var
  i, idx, len: Integer;
begin
  len := 0;
  for i := 1 to Length(S) do begin
    if ((S[i] >= '0') and (S[i] <= '9')) or
      ((S[i] >= 'A') and (S[i] <= 'Z')) or
      ((S[i] >= 'a') and (S[i] <= 'z')) or (S[i] = ' ') or
      (S[i] = '_') or (S[i] = '*') or (S[i] = '-') or (S[i] = '.') then
    begin
      len := len + 1;
    end else begin
      len := len + 3;
    end;
  end;
  SetLength(Result, len);
  idx := 1;
  for i := 1 to Length(S) do begin
    if S[i] = ' ' then begin
      Result[idx] := '+';
      idx := idx + 1;
    end else if ((S[i] >= '0') and (S[i] <= '9')) or
      ((S[i] >= 'A') and (S[i] <= 'Z')) or
      ((S[i] >= 'a') and (S[i] <= 'z')) or
      (S[i] = '_') or (S[i] = '*') or (S[i] = '-') or (S[i] = '.') then begin
      Result[idx] := S[i];
      idx := idx + 1;
    end else begin
      Result[idx] := '%';
      Result[idx + 1] := DigitToHex(Ord(S[i]) div 16);
      Result[idx + 2] := DigitToHex(Ord(S[i]) mod 16);
      idx := idx + 3;
    end;
  end;
end;

procedure Initialize;
var
  I: Integer;
begin
  for I := 0 to High(GNormToUpper) do begin
    GNormToUpper[I] := I;
  end;
  for I := ord('a') to ord('z') do begin
    Dec(GNormToUpper[I], 32);
  end;
end;

initialization
  Initialize;

end.

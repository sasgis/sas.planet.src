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

unit u_StrFunc;

interface

function GetAfter(const SubStr, Str: AnsiString): AnsiString; inline;
function GetBefore(const SubStr, Str: AnsiString): AnsiString; inline;
function GetBetween(const Str, After, Before: AnsiString): AnsiString; inline;

procedure SwapStr(var A, B: string); inline;
procedure SwapStrA(var A, B: AnsiString); inline;

function Utf8DataToUnicodeString(const AData: Pointer; const ASize: Integer): UnicodeString;

implementation

function GetAfter(const SubStr, Str: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := Pos(SubStr, Str);
  if I > 0 then begin
    Result := Copy(Str, I + Length(SubStr), Length(Str));
  end else begin
    Result := '';
  end;
end;

function GetBefore(const SubStr, Str: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := Pos(SubStr, Str);
  if I > 0 then begin
    Result := Copy(Str, 1, I - 1);
  end else begin
    Result := '';
  end;
end;

function GetBetween(const Str, After, Before: AnsiString): AnsiString;
begin
  Result := GetBefore(Before, GetAfter(After, Str));
end;

procedure SwapStr(var A, B: string);
var
  P: Pointer;
begin
  P := Pointer(A);
  Pointer(A) := Pointer(B);
  Pointer(B) := P;
end;

procedure SwapStrA(var A, B: AnsiString);
var
  P: Pointer;
begin
  P := Pointer(A);
  Pointer(A) := Pointer(B);
  Pointer(B) := P;
end;

function Utf8DataToUnicodeString(const AData: Pointer; const ASize: Integer): UnicodeString;
var
  VLen: Integer;
begin
  if (AData = nil) or (ASize <= 0) then begin
    Result := '';
    Exit;
  end;

  SetLength(Result, ASize);

  VLen := Utf8ToUnicode(PWideChar(Result), ASize + 1, AData, ASize);

  if VLen > 0 then begin
    SetLength(Result, VLen - 1);
  end else begin
    Result := '';
  end;
end;

end.

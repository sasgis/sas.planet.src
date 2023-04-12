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

function SetHeaderValue(const AHeaders, AName, AValue: AnsiString): AnsiString;
function GetHeaderValue(const AHeaders, AName: AnsiString): AnsiString;
function DeleteHeaderEntry(const AHeaders, AName: AnsiString): AnsiString;

implementation

uses
  SysUtils,
  RegExpr,
  u_AnsiStr;

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

function SetHeaderValue(const AHeaders, AName, AValue: AnsiString): AnsiString;
var
  VRegExpr: TRegExpr;
begin
  if AHeaders <> '' then begin
    VRegExpr := TRegExpr.Create;
    try
      VRegExpr.Expression := '(?i)' + AName + ':(\s+|)(.*?)(\r\n|$)';
      if VRegExpr.Exec(AHeaders) then begin
        Result := StringReplaceA(AHeaders, VRegExpr.Match[2], AValue, [rfIgnoreCase]);
      end else begin
        Result := AName + ': ' + AValue + #13#10 + AHeaders;
      end;
    finally
      VRegExpr.Free;
    end;
  end else begin
    Result := AName + ': ' + AValue + #13#10;
  end;
end;

function GetHeaderValue(const AHeaders, AName: AnsiString): AnsiString;
var
  VRegExpr: TRegExpr;
begin
  if AHeaders <> '' then begin
    VRegExpr := TRegExpr.Create;
    try
      VRegExpr.Expression := '(?i)' + AName + ':(\s+|)(.*?)(\r\n|$)';
      if VRegExpr.Exec(AHeaders) then begin
        Result := VRegExpr.Match[2];
      end else begin
        Result := '';
      end;
    finally
      VRegExpr.Free;
    end;
  end else begin
    Result := '';
  end;
end;

function DeleteHeaderEntry(const AHeaders, AName: AnsiString): AnsiString;
var
  VRegExpr: TRegExpr;
begin
  if AHeaders <> '' then begin
    VRegExpr := TRegExpr.Create;
    try
      VRegExpr.Expression := '(?i)' + AName + ':(\s+|)(.*?)(\r\n|$)';
      if VRegExpr.Exec(AHeaders) then begin
        Result := StringReplaceA(AHeaders, VRegExpr.Match[0], '', []);
      end else begin
        Result := AHeaders;
      end;
    finally
      VRegExpr.Free;
    end;
  end else begin
    Result := '';
  end;
end;

end.

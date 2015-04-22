{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_StrFunc;

interface

uses
  SysUtils;

{$IF CompilerVersion < 23}
function CharInSet(AChar: AnsiChar; const ASet: TSysCharSet): Boolean; inline;
{$IFEND}

function GetAfter(const SubStr, Str: AnsiString): AnsiString;
function GetBefore(const SubStr, Str: AnsiString): AnsiString;
function GetBetween(const Str, After, Before: AnsiString): AnsiString;

function SetHeaderValue(const AHeaders, AName, AValue: AnsiString): AnsiString;
function GetHeaderValue(const AHeaders, AName: AnsiString): AnsiString;

implementation

uses
  ALString,
  RegExpr;

{$IF CompilerVersion < 23}
function CharInSet(AChar: AnsiChar; const ASet: TSysCharSet): Boolean; inline;
begin
  Result := AChar in ASet;
end;
{$IFEND}

function GetAfter(const SubStr, Str: AnsiString): AnsiString;
begin
  if ALPos(SubStr, Str) > 0 then begin
    Result := ALCopyStr(Str, ALPos(SubStr, Str) + Length(SubStr), Length(Str));
  end else begin
    Result := '';
  end;
end;

function GetBefore(const SubStr, Str: AnsiString): AnsiString;
begin
  if ALPos(SubStr, Str) > 0 then begin
    Result := ALCopyStr(Str, 1, ALPos(SubStr, Str) - 1);
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
        Result := ALStringReplace(AHeaders, VRegExpr.Match[2], AValue, [rfIgnoreCase]);
      end else begin
        Result := AName + ': ' + AValue + #13#10 + AHeaders;
      end;
    finally
      FreeAndNil(VRegExpr);
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
      FreeAndNil(VRegExpr);
    end;
  end else begin
    Result := '';
  end;
end;

end.

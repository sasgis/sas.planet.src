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

end.

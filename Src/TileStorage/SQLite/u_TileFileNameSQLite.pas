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

unit u_TileFileNameSQLite;

interface

uses
  Types,
  u_TileFileNameBase;

type
  TTileFileNameSQLite = class(TTileFileNameBase)
  protected
    function GetTileFileName(
      AXY: TPoint;
      AZoom: Byte
    ): string; override;

    function GetTilePoint(
      const ATileFileName: AnsiString;
      out ATileXY: TPoint;
      out ATileZoom: Byte
    ): Boolean; override;
  end;

implementation

uses
  RegExpr,
  SysUtils;

const
  cSQLiteRegExpr  = '^(.+\\)?[zZ](\d\d?)\\\d+\\\d+\\(\d+)\.(\d+)(\..+)?$';

{ TTileFileNameSQLite }

function TTileFileNameSQLite.GetTileFileName(
  AXY: TPoint;
  AZoom: Byte
): string;
begin
  Result := Format(
    'z%d' + PathDelim +
    '%d' + PathDelim +
    '%d' + PathDelim +
    '%d' + '.' + '%d',
    [
      AZoom + 1,
      AXY.x shr 10,
      AXY.y shr 10,
      AXY.x shr 8,
      AXY.y shr 8
    ]
  );
end;

function TTileFileNameSQLite.GetTilePoint(
  const ATileFileName: AnsiString;
  out ATileXY: TPoint;
  out ATileZoom: Byte
): Boolean;
var
  VRegExpr: TRegExpr;
begin
  VRegExpr := TRegExpr.Create;
  try
    VRegExpr.Expression := cSQLiteRegExpr;
    if VRegExpr.Exec(ATileFileName) then begin
      ATileZoom := StrToInt(VRegExpr.Match[2]) - 1;
      ATileXY.X := StrToInt(VRegExpr.Match[3]) shl 8;
      ATileXY.Y := StrToInt(VRegExpr.Match[4]) shl 8;
      Result := True;
    end else begin
      Result := False;
    end;
  finally
    VRegExpr.Free;
  end;
end;

end.

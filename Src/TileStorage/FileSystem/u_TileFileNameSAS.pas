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

unit u_TileFileNameSAS;

interface

uses
  Types,
  i_TileFileNameParser,
  i_TileFileNameGenerator,
  u_TileFileNameBase;

type
  TTileFileNameSAS = class(TTileFileNameBase)
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
  SysUtils,
  u_AnsiStr;

const
  c_SAS_Expr = '^(.+\\)?[zZ](\d\d?)\\\d+\\[xX](\d+)\\\d+\\[yY](\d+)(\..+)?$';

{ TTileFileNameSAS }

function TTileFileNameSAS.GetTileFileName(
  AXY: TPoint;
  AZoom: Byte
): string;
begin
  result := format('z%d' + PathDelim + '%d' + PathDelim + 'x%d' + PathDelim + '%d' + PathDelim + 'y%d', [
    AZoom + 1,
    AXY.x shr 10,
    AXY.x,
    AXY.y shr 10,
    AXY.y
    ]);
end;

function TTileFileNameSAS.GetTilePoint(
  const ATileFileName: AnsiString;
  out ATileXY: TPoint;
  out ATileZoom: Byte
): Boolean;
var
  VRegExpr: TRegExpr;
begin
  VRegExpr := TRegExpr.Create;
  try
    VRegExpr.Expression := c_SAS_Expr;
    if VRegExpr.Exec(ATileFileName) then begin
      ATileZoom := StrToIntA(VRegExpr.Match[2]) - 1;
      ATileXY.X := StrToIntA(VRegExpr.Match[3]);
      ATileXY.Y := StrToIntA(VRegExpr.Match[4]);
      Result := True;
    end else begin
      Result := False;
    end;
  finally
    VRegExpr.Free;
  end;
end;

end.

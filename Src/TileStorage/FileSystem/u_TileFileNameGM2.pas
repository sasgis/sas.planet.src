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

unit u_TileFileNameGM2;

interface

uses
  Types,
  i_TileFileNameParser,
  i_TileFileNameGenerator,
  u_TileFileNameBase;

type
  TTileFileNameGM2 = class(TTileFileNameBase)
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
  c_GM2_Expr = '^(.+\\)?[zZ](\d\d?)\\(\d+)_(\d+)(\..+)?$';

{ TTileFileNameGM2 }

function TTileFileNameGM2.GetTileFileName(
  AXY: TPoint;
  AZoom: Byte
): string;
begin
  Result := Format('z%d' + PathDelim + '%d_%d', [
    AZoom,
    AXY.Y,
    AXY.X
    ]);
end;

function TTileFileNameGM2.GetTilePoint(
  const ATileFileName: AnsiString;
  out ATileXY: TPoint;
  out ATileZoom: Byte
): Boolean;
var
  VRegExpr: TRegExpr;
begin
  VRegExpr := TRegExpr.Create;
  try
    VRegExpr.Expression := c_GM2_Expr;
    if VRegExpr.Exec(ATileFileName) then begin
      ATileZoom := StrToIntA(VRegExpr.Match[2]);
      ATileXY.Y := StrToIntA(VRegExpr.Match[3]); // (!) Y - first, X - last
      ATileXY.X := StrToIntA(VRegExpr.Match[4]);
      Result := True;
    end else begin
      Result := False;
    end;
  finally
    VRegExpr.Free;
  end;
end;

end.

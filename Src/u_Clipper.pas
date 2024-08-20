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

unit u_Clipper;

interface

uses
  Clipper.Core,
  Clipper.Engine,
  t_GeoTypes;

type
  TClipper = Clipper.Engine.TClipper64;

  TClipperPoint = Clipper.Core.TPoint64;
  PClipperPoint = ^TClipperPoint;

  TClipperPath = Clipper.Core.TPath64;
  TClipperPaths = Clipper.Core.TPaths64;

  TClipperPolyTree = Clipper.Engine.TPolyTree64;
  TClipperPolyNode = Clipper.Engine.TPolyPath64;

  TFillRule = Clipper.Core.TFillRule;
  TClipType = Clipper.Core.TClipType;

const
  ReversePath: function(const path: TPath64): TPath64 = Clipper.Core.ReversePath;
  IsPositive: function(const path: TPath64): Boolean = Clipper.Core.IsPositive;

const
  CClipperIntToCoordCoeff = Int64(100000000); // 8 digits after dot = 1 cm accuracy

function ClipprIntToDouble(const AValue: Int64): Double; inline;
function DoubleToClipperInt(const AValue: Double): Int64; inline;

function ClipperPointToDoublePoint(const APoint: TClipperPoint): TDoublePoint; inline;
function DoublePointToClipperPoint(const APoint: TDoublePoint): TClipperPoint; inline;

implementation

function ClipprIntToDouble(const AValue: Int64): Double;
begin
  Result := AValue / CClipperIntToCoordCoeff;
end;

function DoubleToClipperInt(const AValue: Double): Int64;
begin
  Result := Round(AValue * CClipperIntToCoordCoeff);
end;

function DoublePointToClipperPoint(const APoint: TDoublePoint): TClipperPoint;
begin
  Result.X := DoubleToClipperInt(APoint.X);
  Result.Y := DoubleToClipperInt(APoint.Y);
end;

function ClipperPointToDoublePoint(const APoint: TClipperPoint): TDoublePoint;
begin
  Result.X := ClipprIntToDouble(APoint.X);
  Result.Y := ClipprIntToDouble(APoint.Y);
end;

initialization
  Assert(CClipperIntToCoordCoeff * 180 < Clipper.Core.MaxCoord);
  Assert(CClipperIntToCoordCoeff * -180 > Clipper.Core.MinCoord);

end.

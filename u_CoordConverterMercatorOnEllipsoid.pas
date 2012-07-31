{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_CoordConverterMercatorOnEllipsoid;

interface

uses
  t_GeoTypes,
  u_CoordConverterBasic;

type
  TCoordConverterMercatorOnEllipsoid = class(TCoordConverterBasic)
  private
    FExct: Double;
  protected
    function LonLat2MetrInternal(const ALL: TDoublePoint): TDoublePoint; override;
    function Metr2LonLatInternal(const AMm: TDoublePoint): TDoublePoint; override;
    function LonLat2RelativeInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
    function Relative2LonLatInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
  public
    constructor Create(
      const ARadiusA, ARadiusB: Double
    );
  end;

implementation

uses
  Math,
  u_CoordConverterRoutines,
  u_Datum;

const
  MerkElipsK = 0.000000001;

{ TCoordConverterMercatorOnEllipsoid }

constructor TCoordConverterMercatorOnEllipsoid.Create(
  const ARadiusA, ARadiusB: Double
);
begin
  FExct := sqrt(ARadiusA * ARadiusA - ARadiusB * ARadiusB) / ARadiusA;
  if (Abs(ARadiusA - 6378137) < 1) and (Abs(ARadiusB - 6356752) < 1) then begin
    inherited Create(TDatum.Create(3395, ARadiusA, ARadiusB), 3395, CELL_UNITS_METERS);
  end else begin
    inherited Create(TDatum.Create(0, ARadiusA, ARadiusB), 0, CELL_UNITS_UNKNOWN);
  end;
end;

function TCoordConverterMercatorOnEllipsoid.LonLat2MetrInternal(const ALL: TDoublePoint): TDoublePoint;
begin
  Result := Ellipsoid_LonLat2Metr(Datum.GetSpheroidRadiusA, FExct, ALL);
end;

function TCoordConverterMercatorOnEllipsoid.LonLat2RelativeInternal(
  const XY: TDoublePoint): TDoublePoint;
var
  z, c: Extended;
  VLL: TDoublePoint;
begin
  VLL := XY;
  Result.x := (0.5 + VLl.x / 360);
  z := sin(VLl.y * Pi / 180);
  c := (1 / (2 * Pi));
  Result.y := (0.5 - c * (ArcTanh(z) - FExct * ArcTanh(FExct * z)));
end;

function TCoordConverterMercatorOnEllipsoid.Metr2LonLatInternal(const AMm: TDoublePoint): TDoublePoint;
begin
  Result := Ellipsoid_Metr2LonLat(Datum.GetSpheroidRadiusA, FExct, AMm);
end;

function TCoordConverterMercatorOnEllipsoid.Relative2LonLatInternal(
  const XY: TDoublePoint): TDoublePoint;
var
  Zu, Zum1, yy: extended;
  VXY: TDoublePoint;
  VSin: Extended;
  e_y: Extended;
begin
  VXY := XY;
  Result.X := (VXY.x - 0.5) * 360;

  if (VXY.y > 0.5) then begin
    yy := (VXY.y - 0.5);
  end else begin
    yy := (0.5 - VXY.y);
  end;
  yy := yy * (2 * PI);
  Zu := 2 * arctan(exp(yy)) - PI / 2;
  e_y := exp(2 * yy);
  Result.Y := Zu * (180 / Pi);
  repeat
    Zum1 := Zu;
    VSin := Sin(Zum1);
    Zu := arcsin(1 - (1 + VSin) * power((1 - FExct * VSin) / (1 + FExct * VSin), FExct) / e_y);
  until (abs(Zum1 - Zu) < MerkElipsK) or (isNAN(Zu));
  if not (isNAN(Zu)) then begin
    if VXY.y > 0.5 then begin
      result.Y := -zu * 180 / Pi;
    end else begin
      result.Y := zu * 180 / Pi;
    end;
  end;
end;

end.

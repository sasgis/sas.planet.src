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

unit u_CoordConverterMercatorOnEllipsoid;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Datum,
  u_CoordConverterBasic;

type
  TCoordConverterMercatorOnEllipsoid = class(TCoordConverterBasic)
  private
    FExct: Double;
  protected
    function LonLat2RelativeInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
    function Relative2LonLatInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
  public
    constructor Create(
      const AHash: THashValue;
      const ADatum: IDatum;
      const AProjEPSG: integer
    );
  end;

implementation

uses
  Math,
  i_ProjectionType,
  u_ProjectionTypeMercatorOnEllipsoid;

const
  MerkElipsK = 0.000000001;

{ TCoordConverterMercatorOnEllipsoid }

constructor TCoordConverterMercatorOnEllipsoid.Create(
  const AHash: THashValue;
  const ADatum: IDatum;
  const AProjEPSG: integer
);
var
  VProjectionType: IProjectionType;
  VRadiusA, VRadiusB: Double;
begin
  Assert(ADatum <> nil);
  VProjectionType := TProjectionTypeMercatorOnEllipsoid.Create(AHash, ADatum, AProjEPSG);
  inherited Create(AHash, ADatum, VProjectionType, AProjEPSG);
  VRadiusA := ADatum.GetSpheroidRadiusA;
  VRadiusB := ADatum.GetSpheroidRadiusB;
  FExct := sqrt(VRadiusA * VRadiusA - VRadiusB * VRadiusB) / VRadiusA;
end;

function TCoordConverterMercatorOnEllipsoid.LonLat2RelativeInternal(
  const XY: TDoublePoint): TDoublePoint;
var
  z, c: Extended;
  VLonLat: TDoublePoint;
begin
  VLonLat := XY;
  Result.x := (0.5 + VLonLat.x / 360);
  z := sin(VLonLat.y * Pi / 180);
  c := (1 / (2 * Pi));
  Result.y := (0.5 - c * (ArcTanh(z) - FExct * ArcTanh(FExct * z)));
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
      result.Y := -Zu * 180 / Pi;
    end else begin
      result.Y := Zu * 180 / Pi;
    end;
  end;
end;

end.

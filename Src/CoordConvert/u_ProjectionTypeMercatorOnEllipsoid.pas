{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_ProjectionTypeMercatorOnEllipsoid;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Datum,
  u_ProjectionTypeBase;

type
  TProjectionTypeMercatorOnEllipsoid = class(TProjectionTypeBase)
  private
    FExct: Double;
  protected
    function Relative2LonLatInternal(const APoint: TDoublePoint): TDoublePoint; override;
    function LonLat2RelativeInternal(const APoint: TDoublePoint): TDoublePoint; override;
  public
    constructor Create(
      const AHash: THashValue;
      const ADatum: IDatum;
      const AProjEPSG: integer
    );
  end;

implementation

uses
  Math;

const
  MerkElipsK = 0.000000001;

{ TProjectionTypeMercatorOnEllipsoid }

constructor TProjectionTypeMercatorOnEllipsoid.Create(
  const AHash: THashValue;
  const ADatum: IDatum;
  const AProjEPSG: integer
);
var
  VRadiusA, VRadiusB: Double;
begin
  inherited Create(AHash, ADatum, AProjEPSG);
  VRadiusA := ADatum.GetSpheroidRadiusA;
  VRadiusB := ADatum.GetSpheroidRadiusB;
  FExct := sqrt(VRadiusA * VRadiusA - VRadiusB * VRadiusB) / VRadiusA;
end;

function TProjectionTypeMercatorOnEllipsoid.LonLat2RelativeInternal(
  const APoint: TDoublePoint
): TDoublePoint;
var
  z, c: Extended;
begin
  Result.x := (0.5 + APoint.x / 360);
  z := sin(APoint.y * Pi / 180);
  c := (1 / (2 * Pi));
  Result.y := (0.5 - c * (ArcTanh(z) - FExct * ArcTanh(FExct * z)));
end;

function TProjectionTypeMercatorOnEllipsoid.Relative2LonLatInternal(
  const APoint: TDoublePoint
): TDoublePoint;
var
  Zu, Zum1, yy: extended;
  VSin: Extended;
  e_y: Extended;
begin
  Result.X := (APoint.x - 0.5) * 360;

  if (APoint.y > 0.5) then begin
    yy := (APoint.y - 0.5);
  end else begin
    yy := (0.5 - APoint.y);
  end;
  yy := yy * (2 * PI);
  Zu := 2 * ArcTan(exp(yy)) - PI / 2;
  e_y := Exp(2 * yy);
  Result.Y := Zu * (180 / Pi);
  repeat
    Zum1 := Zu;
    VSin := Sin(Zum1);
    Zu := ArcSin(1 - (1 + VSin) * Power((1 - FExct * VSin) / (1 + FExct * VSin), FExct) / e_y);
  until (abs(Zum1 - Zu) < MerkElipsK) or (IsNan(Zu));
  if not (IsNan(Zu)) then begin
    if APoint.y > 0.5 then begin
      Result.Y := -Zu * 180 / Pi;
    end else begin
      Result.Y := Zu * 180 / Pi;
    end;
  end;
end;

end.

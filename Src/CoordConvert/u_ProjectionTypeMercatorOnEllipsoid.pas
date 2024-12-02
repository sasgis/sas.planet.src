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
      const AProjEPSG: Integer
    );
  end;

implementation

uses
  Math;

{ TProjectionTypeMercatorOnEllipsoid }

constructor TProjectionTypeMercatorOnEllipsoid.Create(
  const AHash: THashValue;
  const ADatum: IDatum;
  const AProjEPSG: Integer
);
var
  VRadiusA, VRadiusB: Double;
begin
  inherited Create(AHash, ADatum, AProjEPSG);
  VRadiusA := ADatum.GetSpheroidRadiusA;
  VRadiusB := ADatum.GetSpheroidRadiusB;
  FExct := Sqrt(VRadiusA * VRadiusA - VRadiusB * VRadiusB) / VRadiusA;
end;

function TProjectionTypeMercatorOnEllipsoid.LonLat2RelativeInternal(
  const APoint: TDoublePoint
): TDoublePoint;
var
  z, c: Extended;
begin
  Result.X := 0.5 + APoint.X / 360;
  z := Sin(APoint.Y * PI / 180);
  c := 1 / (2 * PI);
  Result.Y := 0.5 - c * (ArcTanh(z) - FExct * ArcTanh(FExct * z));
end;

function TProjectionTypeMercatorOnEllipsoid.Relative2LonLatInternal(
  const APoint: TDoublePoint
): TDoublePoint;
const
  MerkElipsK = 0.000000001;
var
  Zu, Zum1, yy: Extended;
  VSin: Extended;
  e_y: Extended;
begin
  Result.X := (APoint.X - 0.5) * 360;

  if APoint.Y > 0.5 then begin
    yy := APoint.Y - 0.5;
  end else begin
    yy := 0.5 - APoint.Y;
  end;

  yy := yy * (2 * PI);
  Zu := 2 * ArcTan(Exp(yy)) - PI / 2;
  e_y := Exp(2 * yy);

  Result.Y := Zu * (180 / PI);

  repeat
    Zum1 := Zu;
    VSin := Sin(Zum1);
    Zu := ArcSin(1 - (1 + VSin) * Power((1 - FExct * VSin) / (1 + FExct * VSin), FExct) / e_y);
  until (Abs(Zum1 - Zu) < MerkElipsK) or IsNan(Zu);

  if not IsNan(Zu) then begin
    if APoint.Y > 0.5 then begin
      Result.Y := -Zu * 180 / PI;
    end else begin
      Result.Y := Zu * 180 / PI;
    end;
  end;
end;

end.

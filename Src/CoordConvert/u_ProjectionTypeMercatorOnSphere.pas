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

unit u_ProjectionTypeMercatorOnSphere;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Datum,
  u_ProjectionTypeBase;

type
  TProjectionTypeMercatorOnSphere = class(TProjectionTypeBase)
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

{ TProjectionTypeMercatorOnSphere }

constructor TProjectionTypeMercatorOnSphere.Create(
  const AHash: THashValue;
  const ADatum: IDatum;
  const AProjEPSG: Integer
);
begin
  Assert(Abs(ADatum.GetSpheroidRadiusA - ADatum.GetSpheroidRadiusB) < 1);
  inherited Create(AHash, ADatum, AProjEPSG);
end;

function TProjectionTypeMercatorOnSphere.LonLat2RelativeInternal(
  const APoint: TDoublePoint
): TDoublePoint;
const
  cEpsilon = 1E-12;
var
  z, c: Extended;
begin
  Result.X := 0.5 + APoint.X / 360;

  z := Sin(APoint.Y * PI / 180);
  c := Ln((1 + z) / (1 - z)) / (2 * PI);

  Result.Y := 0.5 - 0.5 * c;

  if SameValue(Result.Y, 0, cEpsilon) then begin
    Result.Y := 0;
  end else
  if SameValue(Result.Y, 1, cEpsilon) then begin
    Result.Y := 1;
  end;
end;

function TProjectionTypeMercatorOnSphere.Relative2LonLatInternal(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result.X := (APoint.X - 0.5) * 360;
  Result.Y := -(APoint.Y - 0.5) * (2 * Pi);
  Result.Y := (2 * ArcTan(Exp(Result.Y)) - PI / 2) * 180 / PI;
end;

end.

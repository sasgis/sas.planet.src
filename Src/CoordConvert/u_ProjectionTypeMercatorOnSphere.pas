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
      const AProjEPSG: integer
    );
  end;

implementation

uses
  Math;

{ TProjectionTypeMercatorOnSphere }

constructor TProjectionTypeMercatorOnSphere.Create(
  const AHash: THashValue;
  const ADatum: IDatum;
  const AProjEPSG: integer
);
begin
  Assert(Abs(ADatum.GetSpheroidRadiusA - ADatum.GetSpheroidRadiusB) < 1);
  inherited Create(AHash, ADatum, AProjEPSG);
end;

function TProjectionTypeMercatorOnSphere.LonLat2RelativeInternal(
  const APoint: TDoublePoint
): TDoublePoint;
var
  z, c: Extended;
begin
  Result.x := 0.5 + APoint.x / 360;
  z := Sin(APoint.y * Pi / 180);
  c := 1 / (2 * Pi);
  Result.y := 0.5 - 0.5 * Ln((1 + z) / (1 - z)) * c;
end;

function TProjectionTypeMercatorOnSphere.Relative2LonLatInternal(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result.X := (APoint.x - 0.5) * 360;
  Result.Y := -(APoint.y - 0.5) * (2 * PI);
  Result.Y := (2 * ArcTan(Exp(Result.Y)) - PI / 2) * 180 / PI;
end;

end.

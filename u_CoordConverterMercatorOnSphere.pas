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

unit u_CoordConverterMercatorOnSphere;

interface

uses
  Math,
  t_GeoTypes,
  i_Datum,
  u_CoordConverterBasic;

type
  TCoordConverterMercatorOnSphere = class(TCoordConverterBasic)
  protected
    function LonLat2MetrInternal(const ALl: TDoublePoint): TDoublePoint; override;
    function Metr2LonLatInternal(const AMm: TDoublePoint): TDoublePoint; override;
    function LonLat2RelativeInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
    function Relative2LonLatInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
  public
    constructor Create(
      const ADatum: IDatum;
      AProjEPSG: integer;
      ACellSizeUnits: TCellSizeUnits
    );
  end;

implementation

{ TCoordConverterMercatorOnSphere }

constructor TCoordConverterMercatorOnSphere.Create(
  const ADatum: IDatum;
  AProjEPSG: integer;
  ACellSizeUnits: TCellSizeUnits
);
begin
  Assert(ADatum <> nil);
  Assert(Abs(ADatum.GetSpheroidRadiusA - ADatum.GetSpheroidRadiusB) < 1);
  inherited;
end;

function TCoordConverterMercatorOnSphere.LonLat2MetrInternal(const ALl: TDoublePoint): TDoublePoint;
var
  VLonLat: TDoublePoint;
begin
  VLonLat := ALl;
  VLonLat.x := VLonLat.x * (Pi / 180);
  VLonLat.y := VLonLat.y * (Pi / 180);
  result.x := Datum.GetSpheroidRadiusA * VLonLat.x;
  result.y := Datum.GetSpheroidRadiusA * Ln(Tan(PI / 4 + VLonLat.y / 2));
end;

function TCoordConverterMercatorOnSphere.LonLat2RelativeInternal(const XY: TDoublePoint): TDoublePoint;
var
  z, c: Extended;
begin
  Result.x := 0.5 + XY.x / 360;
  z := sin(XY.y * Pi / 180);
  c := 1 / (2 * Pi);
  Result.y := 0.5 - 0.5 * ln((1 + z) / (1 - z)) * c;
end;

function TCoordConverterMercatorOnSphere.Metr2LonLatInternal(const AMm: TDoublePoint): TDoublePoint;
begin
  result.X := (AMm.X / Datum.GetSpheroidRadiusA) * (180 / Pi);

  Result.Y := (AMm.Y / Datum.GetSpheroidRadiusA);
  Result.Y := exp(Result.Y);
  Result.Y := ArcTan(Result.Y);
  Result.Y := (2 * (180 / Pi) * Result.Y) - 90;
end;

function TCoordConverterMercatorOnSphere.Relative2LonLatInternal(
  const XY: TDoublePoint): TDoublePoint;
begin
  Result.X := (XY.x - 0.5) * 360;
  Result.Y := -(XY.y - 0.5) * (2 * PI);
  Result.Y := (2 * arctan(exp(Result.Y)) - PI / 2) * 180 / PI;
end;

end.

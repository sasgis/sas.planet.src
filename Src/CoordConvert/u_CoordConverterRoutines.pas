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

unit u_CoordConverterRoutines;

interface

uses
  t_GeoTypes;

function Ellipsoid_LonLat2Metr(
    const ARadiusa, AExct: Double;
    const ALl: TDoublePoint
  ): TDoublePoint;
function Ellipsoid_Metr2LonLat(
    const ARadiusa, AExct: Double;
    const AMm: TDoublePoint
  ): TDoublePoint;

implementation

uses
  Math;

const
  Ellipsoid_Metr2LonLat_Grad_Error = 0.00000001;

function Ellipsoid_LonLat2Metr(
  const ARadiusa, AExct: Double;
  const ALl: TDoublePoint
): TDoublePoint;
var
  VLonLat: TDoublePoint;
  b, bs: extended;
begin
  VLonLat := ALl;
  VLonLat.x := VLonLat.x * (Pi / 180);
  VLonLat.y := VLonLat.y * (Pi / 180);
  result.x := ARadiusa * VLonLat.x;

  bs := AExct * sin(VLonLat.y);
  b := Tan((VLonLat.y + PI / 2) / 2) * power((1 - bs) / (1 + bs), (AExct / 2));
  if b <= 0 then begin
    b := 0.00000000000001;
  end;
  result.y := ARadiusa * Ln(b);
end;

function Ellipsoid_Metr2LonLat(
  const ARadiusa, AExct: Double;
  const AMm: TDoublePoint
): TDoublePoint;
var
  VSinN, VCommon, VNext, VResult: Extended;
begin
  result.X := (AMm.X / ARadiusa) * (180 / Pi);

  // get initial value as on spheroid
  Result.Y := (AMm.Y / ARadiusa);
  VCommon := Exp(2 * Result.Y); // for iterations
  Result.Y := exp(Result.Y);
  Result.Y := ArcTan(Result.Y);
  Result.Y := (2 * (180 / Pi) * Result.Y) - 90;

  // iterations
  VSinN := Result.Y / (180 / Pi);
  repeat
    // get sinus of next iteration
    VNext := (1 - AExct * VSinN) / (1 + AExct * VSinN);
    VNext := Power(VNext, AExct);
    VNext := 1 - ((1 + VSinN) * VNext / VCommon);
    // get result
    VResult := ArcSin(VNext) * (180 / Pi);
    // check result
    if Abs(VResult - Result.Y) < Ellipsoid_Metr2LonLat_Grad_Error then begin
      // done
      Result.Y := VResult;
      break;
    end else begin
      // one more time
      Result.Y := VResult;
      VSinN := VNext;
    end;
  until FALSE;
end;


end.

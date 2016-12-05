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

unit u_DistanceCalculatorByGeodesicLib;

interface

uses
  Windows,
  GeographicLib,
  i_DistanceCalculator,
  u_BaseInterfacedObject;

type
  TDistanceCalculatorByGeographicLib = class(TBaseInterfacedObject, IDistanceCalculator)
  private
    VGeod: geod_geodesic;
  private
    { IDistanceCalculator }
    procedure ComputeFinishPosition(
      const ALat1, ALon1: Double;
      const AInitialBearing: Double;
      const ADistance: Double;
      out ALat2, ALon2: Double
    );
    function ComputeDistance(
      const ALat1, ALon1: Double;
      const ALat2, ALon2: Double;
      out AInitialBearing: Double;
      out AFinalBearing: Double
    ): Double;
  public
    constructor Create(
      const ARadiusA: Double;
      const ARadiusB: Double
    );
  end;

implementation

uses
  SysUtils,
  Math;

{ TDistanceCalculatorByGeographicLib }

constructor TDistanceCalculatorByGeographicLib.Create(
  const ARadiusA: Double;
  const ARadiusB: Double
);
begin
  inherited Create;
  if init_geodesic_dll(geodesic_dll, True) then begin
    geod_init(@VGeod, ARadiusA, ((ARadiusA - ARadiusB) / ARadiusA));
  end;
end;

procedure TDistanceCalculatorByGeographicLib.ComputeFinishPosition(
  const ALat1, ALon1: Double;
  const AInitialBearing: Double;
  const ADistance: Double;
  out ALat2, ALon2: Double
);
var
  VAzi2: Double;
begin
  ALat2 := NAN;
  ALon2 := NAN;
  VAzi2 := NAN;
  geod_direct(@VGeod, ALat1, ALon1, AInitialBearing, ADistance, ALat2, ALon2, VAzi2);
end;

function TDistanceCalculatorByGeographicLib.ComputeDistance(
  const ALat1, ALon1: Double;
  const ALat2, ALon2: Double;
  out AInitialBearing: Double;
  out AFinalBearing: Double
): Double;
const
  DEG2RAD: Double = 0.017453292519943295769236907684886;
begin
  geod_inverse(@VGeod, ALat1, ALon1, ALat2, ALon2, Result, AInitialBearing, AFinalBearing);

  if AInitialBearing < 0 then begin
    AInitialBearing := 2 * Pi / DEG2RAD + AInitialBearing;
  end;

  AFinalBearing := AFinalBearing - Pi / DEG2RAD;
  if AFinalBearing < 0 then begin
    AFinalBearing := 2 * Pi / DEG2RAD + AFinalBearing;
  end;
end;

end.

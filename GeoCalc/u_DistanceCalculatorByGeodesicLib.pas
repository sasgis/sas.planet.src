{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit u_DistanceCalculatorByGeodesicLib;

interface

uses
  Windows,
  i_DistanceCalculator,
  u_BaseInterfacedObject;

type
  TDistanceCalculatorByGeodesicLib = class(TBaseInterfacedObject, IDistanceCalculator)
  private
    type
      geod_geodesic = record
        a: Double;
        f: Double;
        f1, e2, ep2, n, b, c2, etol2: Double;
        A3x: array [0..5] of Double;
        C3x: array [0..14] of Double;
        C4x: array [0..20] of Double;
      end;
      geod_geodesic_ptr = ^geod_geodesic;
      geod_init_t = procedure(
        const g: geod_geodesic_ptr;
        const a, f: Double
      ); cdecl;
      geod_direct_t = procedure(
        const g: geod_geodesic_ptr;
        const lat1, lon1, azi1, s12: Double;
        out lat2, lon2, azi2: Double
      ); cdecl;
      geod_inverse_t = procedure(
        const g: geod_geodesic_ptr;
        const lat1, lon1, lat2, lon2: Double;
        out s12, azi1, azi2: Double
      ); cdecl;
  private
    geodesic_dll: THandle;
    g: geod_geodesic;
    geod_init: geod_init_t;
    geod_direct: geod_direct_t;
    geod_inverse: geod_inverse_t;
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
    constructor Create(const ARadiusA: Double; const ARadiusB: Double);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

const
  geodesic_lib = 'geodesic.dll';

{ TDistanceCalculatorByGeodesicLib }

constructor TDistanceCalculatorByGeodesicLib.Create(
  const ARadiusA: Double;
  const ARadiusB: Double
);
begin
  inherited Create;
  geodesic_dll := LoadLibrary(PChar(geodesic_lib));
  if (geodesic_dll <> 0) then begin
    try
      geod_init := GetProcAddress(geodesic_dll, 'geod_init');
      if Addr(geod_init) = nil then begin
        RaiseLastOSError;
      end;
      geod_direct := GetProcAddress(geodesic_dll, 'geod_direct');
      if Addr(geod_direct) = nil then begin
        RaiseLastOSError;
      end;
      geod_inverse := GetProcAddress(geodesic_dll, 'geod_inverse');
      if Addr(geod_inverse) = nil then begin
        RaiseLastOSError;
      end;

      geod_init(@g, ARadiusA, ((ARadiusA - ARadiusB) / ARadiusA) );
    except
      FreeLibrary(geodesic_dll);
      geodesic_dll := 0;
      raise;
    end;
  end else begin
    RaiseLastOSError;
  end;
end;

destructor TDistanceCalculatorByGeodesicLib.Destroy;
begin
  if (geodesic_dll <> 0) then begin
    FreeLibrary(geodesic_dll);
    geodesic_dll := 0;
  end;
  inherited;
end;

procedure TDistanceCalculatorByGeodesicLib.ComputeFinishPosition(
  const ALat1, ALon1: Double;
  const AInitialBearing: Double;
  const ADistance: Double;
  out ALat2, ALon2: Double
);
var
  azi2: Double;
begin
  geod_direct(@g, ALat1, ALon1, AInitialBearing, ADistance, ALat2, ALon2, azi2);
end;

function TDistanceCalculatorByGeodesicLib.ComputeDistance(
  const ALat1, ALon1: Double;
  const ALat2, ALon2: Double;
  out AInitialBearing: Double;
  out AFinalBearing: Double
): Double;
begin
  geod_inverse(@g, ALat1, ALon1, ALat2, ALon2, Result, AInitialBearing, AFinalBearing);
end;

end.

{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit u_ProjConverterByDll;

interface

uses
  Proj4,
  t_GeoTypes,
  i_ProjConverter,
  u_BaseInterfacedObject;

type
  // 1. This class is not thread safe (because of projCtx), so use one instance
  // per thread (https://trac.osgeo.org/proj/wiki/ThreadSafety).
  // 2. You must init pro4 library BEFORE creating instances of this class
  // (see TProjConverterFactory).
  TProjConverterByDll = class(TBaseInterfacedObject, IProjConverter)
  private
    FCtx: projCtx;
    FGeoPJ: projPJ;
    FProjPJ: projPJ;
    FProj4InitStr: AnsiString;
  private
    function LonLat2XY(const ALonLat: TDoublePoint): TDoublePoint;
    function XY2LonLat(const AXY: TDoublePoint): TDoublePoint;
  public
    constructor Create(const AProj4InitStr: AnsiString);
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

implementation

uses
  SysUtils,
  Proj4Defs;

type
  EProjConverterByDllError = class(Exception);

const
  cProjCtxInitError = 'Can''t initialize proj4 context!';
  cProjectionInitError = 'Can''t initialize proj4 with string: %s';

{ TProjConverterByDll }

constructor TProjConverterByDll.Create(const AProj4InitStr: AnsiString);
begin
  Assert(AProj4InitStr <> '');
  inherited Create;
  FProj4InitStr := AProj4InitStr;
  FCtx := nil;
  FProjPJ := nil;
  FGeoPJ := nil;
end;

procedure TProjConverterByDll.AfterConstruction;
begin
  inherited;

  FCtx := pj_ctx_alloc();
  if FCtx = nil then begin
    raise EProjConverterByDllError.Create(cProjCtxInitError);
  end;

  FProjPJ := pj_init_plus_ctx(FCtx, PAnsiChar(FProj4InitStr));
  if FProjPJ = nil then begin
    raise EProjConverterByDllError.CreateFmt(cProjectionInitError, [FProj4InitStr]);
  end;

  FGeoPJ := pj_init_plus_ctx(FCtx, PAnsiChar(wgs_84));
  if FGeoPJ = nil then begin
    raise EProjConverterByDllError.CreateFmt(cProjectionInitError, [wgs_84]);
  end;
end;

destructor TProjConverterByDll.Destroy;
begin
  if FProjPJ <> nil then begin
    pj_free(FProjPJ);
    FProjPJ := nil;
  end;

  if FGeoPJ <> nil then begin
    pj_free(FGeoPJ);
    FGeoPJ := nil;
  end;

  if FCtx <> nil then begin
    pj_ctx_free(FCtx);
    FCtx := nil;
  end;

  inherited;
end;

function TProjConverterByDll.LonLat2XY(
  const ALonLat: TDoublePoint
): TDoublePoint;
var
  err: Integer;
  x, y, z: Double;
begin
  Assert(FProjPJ <> nil);
  Assert(FGeoPJ <> nil);

  x := ALonLat.X * DEG_TO_RAD;
  y := ALonLat.Y * DEG_TO_RAD;
  z := 0;

  err := pj_transform(FGeoPJ, FProjPJ, 1, 0, x, y, z); // Geo -> Proj

  if err <> 0 then begin
    raise EProjConverterByDllError.CreateFmt(
      'LonLat2XY(%.6f; %.6f) failed: %s', [ALonLat.X, ALonLat.Y, pj_strerrno(err)]
    );
  end;

  Result.X := x;
  Result.Y := y;
end;

function TProjConverterByDll.XY2LonLat(
  const AXY: TDoublePoint
): TDoublePoint;
var
  err: Integer;
  x, y, z: Double;
begin
  Assert(FProjPJ <> nil);
  Assert(FGeoPJ <> nil);

  x := AXY.X;
  y := AXY.Y;
  z := 0;

  err := pj_transform(FProjPJ, FGeoPJ, 1, 0, x, y, z); // Proj -> Geo

  if err <> 0 then begin
    raise EProjConverterByDllError.CreateFmt(
      'XY2LonLat(%.6f; %.6f) failed: %s', [AXY.X, AXY.Y, pj_strerrno(err)]
    );
  end;

  Result.X := x * RAD_TO_DEG;
  Result.Y := y * RAD_TO_DEG;
end;

end.

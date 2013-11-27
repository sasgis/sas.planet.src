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

unit u_PolygonAreaCalculator;

interface

uses
  t_GeoTypes,
  i_NotifierOperation,
  i_PolygonAreaCalculator,
  u_BaseInterfacedObject;

type
  TPolygonAreaCalculator = class(TBaseInterfacedObject, IPolygonAreaCalculator)
  private
    m_QA, m_QB, m_QC: Double;
    m_QbarA, m_QbarB, m_QbarC, m_QbarD: Double;
    m_TwoPI, m_AE, m_Qp, m_E: Double;
  private
    FRadiusA: Double;
    FRadiusB: Double;
    FIsEllipsoid: Boolean;
    function GetQ(const X: Double): Double;
    function GetQbar(const X: Double): Double;
    procedure ComputeAreaInit;
    function ComputePolygonFlatArea(
      const APoints: PDoublePointArray;
      const ACount: Integer;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;
  private
    { IPolygonAreaCalculator }
    function ComputePolygonArea(
      const APoints: PDoublePointArray;
      const ACount: Integer;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;
  public
    constructor Create; overload;
    constructor Create(const ARadiusA: Double; const ARadiusB: Double); overload;
  end;

implementation

uses
  Math;

const
  DEG2RAD: Double = 0.017453292519943295769236907684886;

{ TPolygonAreaCalculator }

constructor TPolygonAreaCalculator.Create;
begin
  inherited Create;
  FIsEllipsoid := False;
  FRadiusA := 0;
  FRadiusB := 0;
end;

constructor TPolygonAreaCalculator.Create(const ARadiusA: Double; const ARadiusB: Double);
begin
  inherited Create;
  FIsEllipsoid := True;
  FRadiusA := ARadiusA;
  FRadiusB := ARadiusB;
  ComputeAreaInit;
end;

function TPolygonAreaCalculator.GetQ(const X: Double): Double;
var
  sinx, sinx2: Double;
begin
  sinx := sin(x);
  sinx2 := sinx * sinx;

  Result := sinx * (1 + sinx2 * (m_QA + sinx2 * (m_QB + sinx2 * m_QC)));
end;

function TPolygonAreaCalculator.GetQbar(const X: Double): Double;
var
  cosx, cosx2: Double;
begin
  cosx := cos(x);
  cosx2 := cosx * cosx;

  Result := cosx * (m_QbarA + cosx2 * (m_QbarB + cosx2 * (m_QbarC + cosx2 * m_QbarD)));
end;

procedure TPolygonAreaCalculator.ComputeAreaInit;
var
  a2, e2, e4, e6: Double;
begin
  a2 := FRadiusA * FRadiusA;
  e2 := 1 - (a2 / (FRadiusB * FRadiusB));

  m_TwoPI := Pi + Pi;

  e4 := e2 * e2;
  e6 := e4 * e2;

  m_AE := a2 * ( 1 - e2 );

  m_QA := ( 2.0 / 3.0 ) * e2;
  m_QB := ( 3.0 / 5.0 ) * e4;
  m_QC := ( 4.0 / 7.0 ) * e6;

  m_QbarA := -1.0 - ( 2.0 / 3.0 ) * e2 - ( 3.0 / 5.0 ) * e4  - ( 4.0 / 7.0 ) * e6;
  m_QbarB := ( 2.0 / 9.0 ) * e2 + ( 2.0 / 5.0 ) * e4  + ( 4.0 / 7.0 ) * e6;
  m_QbarC := - ( 3.0 / 25.0 ) * e4 - ( 12.0 / 35.0 ) * e6;
  m_QbarD := ( 4.0 / 49.0 ) * e6;

  m_Qp := GetQ(Pi/2);
  m_E := 4 * Pi * m_Qp * m_AE;
  if m_E < 0.0 then begin
    m_E := -m_E;
  end;
end;

// stuff for measuring areas - copied from GRASS
// don't know how does it work, but it's working .)
// see G_begin_ellipsoid_polygon_area() in area_poly1.c

function TPolygonAreaCalculator.ComputePolygonArea(
  const APoints: PDoublePointArray;
  const ACount: Integer;
  const ANotifier: INotifierOperation = nil;
  const AOperationID: Integer = 0
): Double;
var
  x1, y1, x2, y2, dx, dy: Double;
  Qbar1, Qbar2: Double;
  area: Double;
  n, i: Integer;
  VCount: Integer;
  VDoAbortCheck: Boolean;
begin

  if not FIsEllipsoid then begin
    Result := ComputePolygonFlatArea(APoints, ACount, ANotifier, AOperationID);
    Exit;
  end;

  VCount := 0;
  VDoAbortCheck := Assigned(ANotifier);

  n := ACount;

  x2 := DEG2RAD * APoints[n-1].x;
  y2 := DEG2RAD * APoints[n-1].y;

  Qbar2 := GetQbar( y2 );

  area := 0;

  for i := 0 to n - 1 do begin
    x1 := x2;
    y1 := y2;
    Qbar1 := Qbar2;

    x2 := DEG2RAD * APoints[i].x;
    y2 := DEG2RAD * APoints[i].y;

    Qbar2 := GetQbar( y2 );

    if ( x1 > x2 ) then begin
      while ( x1 - x2 > Pi ) do begin
        x2 := x2 + m_TwoPI;
      end;
    end else if ( x2 > x1 ) then begin
      while ( x2 - x1 > Pi ) do begin
        x1 := x1 + m_TwoPI;
      end;
    end;

    dx := x2 - x1;
    area := area + dx * ( m_Qp - GetQ( y2 ) );

    dy := y2 - y1;
    if not SameValue(dy, 0) then begin
      area := area + dx * GetQ( y2 ) - ( dx / dy ) * ( Qbar2 - Qbar1 );
    end;

    Inc(VCount);
    if VDoAbortCheck and (VCount mod 32 = 0) then begin
      if ANotifier.IsOperationCanceled(AOperationID) then begin
        Result := NAN;
        Exit;
      end;
    end;

  end;

  area := area * m_AE;
  if area < 0 then begin
    area := -area;
  end;

  (* kludge - if polygon circles the south pole the area will be
  * computed as if it cirlced the north pole. The correction is
  * the difference between total surface area of the earth and
  * the "north pole" area.
  *)

  if ( area > m_E ) then
    area := m_E;
  if ( area > m_E / 2 ) then
    area := m_E - area;

  Result := area;
end;

function TPolygonAreaCalculator.ComputePolygonFlatArea(
  const APoints: PDoublePointArray;
  const ACount: Integer;
  const ANotifier: INotifierOperation = nil;
  const AOperationID: Integer = 0
): Double;
var
  area: Double;
  i: Integer;
  VCount: Integer;
  VDoAbortCheck: Boolean;
begin
  VCount := 0;
  VDoAbortCheck := Assigned(ANotifier);

  // Normal plane area calculations.
  area := 0;

  for i := 0 to ACount - 1 do begin
    area := area + APoints[i].x * APoints[(i+1) mod ACount].y - APoints[(i+1) mod ACount].x * APoints[i].y;

    Inc(VCount);
    if VDoAbortCheck and (VCount mod 32 = 0) then begin
      if ANotifier.IsOperationCanceled(AOperationID) then begin
        Result := NAN;
        Exit;
      end;
    end;
  end;

  area := area / 2.0;

  Result := Abs(area); // All areas are positive!
end;

end.

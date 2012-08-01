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

unit u_Datum;

interface

uses
  t_GeoTypes,
  i_Datum;

type
  TDatum = class(TInterfacedObject, IDatum)
  private
    FEPSG: Integer;
    FRadiusA: Double;
    FRadiusB: Double;
    FExct: Double;
  private
    function GetEPSG: integer; stdcall;
    function GetSpheroidRadiusA: Double; stdcall;
    function GetSpheroidRadiusB: Double; stdcall;
    function IsSameDatum(const ADatum: IDatum): Boolean; stdcall;
    function SphericalTriangleSquare(points: array of TDoublePoint): Double;
    function CalcPoligonArea(
      const APoints: PDoublePointArray;
      const ACount: Integer
    ): Double;
    function CalcDist(const AStart, AFinish: TDoublePoint): Double;
  public
    constructor Create(
      AEPSG: Integer;
      const ARadiusA: Double;
      const ARadiusB: Double
    ); overload;
    constructor Create(
      AEPSG: Integer;
      const ARadiusA: Double
    ); overload;
  end;

implementation

uses
  Math;

const
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы

{ TDatum }

constructor TDatum.Create(
  AEPSG: Integer;
  const ARadiusA, ARadiusB: Double
);
begin
  inherited Create;
  FEPSG := AEPSG;
  FRadiusA := ARadiusA;
  FRadiusB := ARadiusB;
  FExct := sqrt(FRadiusA * FRadiusA - FRadiusB * FRadiusB) / FRadiusA;
end;

constructor TDatum.Create(
  AEPSG: Integer;
  const ARadiusA: Double
);
begin
  Create(AEPSG, ARadiusA, ARadiusA);
end;

function TDatum.CalcDist(const AStart, AFinish: TDoublePoint): Double;
var
  fSinPhimean, fdLambda, fdPhi, fAlpha, fRho, fNu, fR, fz, fTemp, a, e2: Double;
  VStart, VFinish: TDoublePoint; // Координаты в радианах
begin
  result := 0;
  if (AStart.X = AFinish.X) and (AStart.Y = AFinish.Y) then begin
    exit;
  end;
  e2 := FExct * FExct;
  a := FRadiusA;

  VStart.X := AStart.X * D2R;
  VStart.Y := AStart.Y * D2R;
  VFinish.X := AFinish.X * D2R;
  VFinish.Y := AFinish.Y * D2R;

  fdLambda := VStart.X - VFinish.X;
  fdPhi := VStart.Y - VFinish.Y;
  fz := Sqrt(intPower(Sin(fdPhi / 2), 2) + Cos(VFinish.Y) * Cos(VStart.Y) * intPower(Sin(fdLambda / 2), 2));
  fz := 2 * ArcSin(fz);
  fSinPhimean := Sin((VStart.Y + VFinish.Y) / 2.0);
  fTemp := 1 - e2 * (fSinPhimean * fSinPhimean);
  fRho := (a * (1 - e2)) / Power(fTemp, 1.5);
  fNu := a / (Sqrt(1 - e2 * (fSinPhimean * fSinPhimean)));
  fAlpha := Cos(VFinish.Y) * Sin(fdLambda) * 1 / Sin(fz);
  if fAlpha > 1 then begin
    fAlpha := 1;
  end else if fAlpha < -1 then begin
    fAlpha := -1;
  end;
  fAlpha := ArcSin(fAlpha);
  fR := (fRho * fNu) / ((fRho * intPower(Sin(fAlpha), 2)) + (fNu * intPower(Cos(fAlpha), 2)));

  if abs(fdLambda) <= Pi then begin
    result := (fz * fR);
  end else begin
    result := (Pi * fR) + ((Pi * fR) - (fz * fR));
  end;
end;

function TDatum.SphericalTriangleSquare(points: array of TDoublePoint): Double;
var
  x, y, z: array [0..2] of double;
  a12, a23, a13, s, eps: double;
  i: integer;
begin
  for i := 0 to 2 do begin
    points[i].Y := Pi / 2 - points[i].Y * D2R;
    points[i].X := Pi + points[i].X * D2R;
    x[i] := Sin(points[i].Y) * Cos(points[i].X);
    y[i] := Sin(points[i].Y) * Sin(points[i].X);
    z[i] := Cos(points[i].Y);
  end;
  a12 := ArcCos(1 - (Sqr(x[0] - x[1]) + Sqr(y[0] - y[1]) + Sqr(z[0] - z[1])) / 2);   //Стороны сферического треугольника
  a23 := ArcCos(1 - (Sqr(x[1] - x[2]) + Sqr(y[1] - y[2]) + Sqr(z[1] - z[2])) / 2);
  a13 := ArcCos(1 - (Sqr(x[0] - x[2]) + Sqr(y[0] - y[2]) + Sqr(z[0] - z[2])) / 2);
  s := (a12 + a23 + a13) / 2;
  s := Tan(s / 2) * Tan((s - a12) / 2) * Tan((s - a23) / 2) * Tan((s - a13) / 2);
  if s >= 0 then begin
    eps := 4 * ArcTan(Sqrt(s));  //сферический эксцесс
  end else begin
    eps := 0;
  end;
  Result := Sqr(FRadiusA) * eps;  //Площадь
end;

function TDatum.CalcPoligonArea(
  const APoints: PDoublePointArray;
  const ACount: Integer
): Double;

  function Sign(AValue: double): integer;
  begin
    if AValue < 0 then begin
      Result := -1;
    end else begin
      Result := 1;
    end;
  end;

  function Orientation(
  const APoints: PDoublePointArray;
  const ACount: Integer
  ): extended;
  var
    i: integer;
    s: double;
  begin
    s := 0;
    for i := 0 to ACount - 1 do begin
      s := s + (APoints[(i + 1) mod ACount].x - APoints[i].x) * (APoints[(i + 1) mod ACount].y + APoints[i].y);
    end;
    result := -s / 2;
  end;

  function Det(point1, point2, point3: TDoublePoint): Extended;
  begin
    Result := ((point2.X - point1.X) * (point3.Y - point1.Y) - (point2.Y - point1.Y) * (point3.X - point1.X)) / 2;
  end;

  function InTriangle(point, point1, point2, point3: TDoublePoint): Boolean;
  var
    a, b, c: double;
  begin
    a := Det(point, point1, point2);
    b := Det(point, point2, point3);
    c := Det(point, point3, point1);
    Result := ((a >= 0) and (b >= 0) and (c >= 0)) or ((a < 0) and (b < 0) and (c < 0));
  end;

var
  Node: TDoublePoint;
  p: array [0..2] of TDoublePoint;  //вершины треугольника
  pn: array [0..2] of integer;
  NodeN: integer;
  PointsNum: integer;
  Orient: Integer;
  inPoint: Boolean;
  s: double;
  i: integer;
  PointsA: Array of integer;
  ErrNum: integer;
begin
  s := 0;
  if ACount < 3 then begin
    result := s;
    exit;
  end;
  Orient := Sign(Orientation(APoints, ACount)); //ориентация многоугольника
  PointsNum := ACount;
  ErrNum := 0;
  SetLength(PointsA, ACount);
  For NodeN := 0 to ACount - 1 do begin
    PointsA[NodeN] := 1;
  end;
  for i := 0 to 2 do begin
    pn[i] := i;
    p[i] := APoints[i];
  end;
  while (PointsNum > 3) and (ErrNum <= ACount) do begin
    if Sign(Det(p[0], p[1], p[2])) = Orient then begin//Проверка ориентации треугольника
      inPoint := false;
      NodeN := (pn[2] + 1) mod ACount;
      Node := APoints[Noden];
      while NodeN <> pn[0] do begin
        if (PointsA[NodeN] = 1) and (InTriangle(Node, p[0], p[1], p[2])) then begin
          inPoint := true;  // Проверка не попала ли вершина в отсекаемый треугольник
        end;
        Noden := (NodeN + 1) mod ACount;
        Node := APoints[NodeN];
      end;
    end else begin
      inPoint := true;
    end;
    if (not InPoint) then begin
      s := s + SphericalTriangleSquare(p);
      PointsA[pn[1]] := 0;    //Удаление вершины из рассмотрения
      dec(PointsNum);
      ErrNum := 0;
    end else begin
      pn[0] := pn[1];          //  Переход к следущему треугольнику
      p[0] := APoints[pn[0]];
      inc(ErrNum);
    end;
    pn[1] := pn[2];  //  Переход к следущему треугольнику
    p[1] := APoints[pn[1]];
    pn[2] := (pn[2] + 1) mod ACount;
    while (PointsA[pn[2]] = 0) and (pn[2] <> pn[0]) do begin
      pn[2] := (pn[2] + 1) mod ACount;
    end;
    p[2] := APoints[pn[2]];
  end;
  if ErrNum <= ACount then begin
    s := s + SphericalTriangleSquare(p);
    result := s;
  end else begin
    result := NAN;
  end;
end;

function TDatum.GetEPSG: integer;
begin
  Result := FEPSG;
end;

function TDatum.GetSpheroidRadiusA: Double;
begin
  Result := FRadiusA;
end;

function TDatum.GetSpheroidRadiusB: Double;
begin
  Result := FRadiusB;
end;

function TDatum.IsSameDatum(const ADatum: IDatum): Boolean;
var
  VSelf: IDatum;
begin
  VSelf := Self;
  if VSelf = ADatum then begin
    Result := True;
  end else begin
    Result := (ADatum.EPSG <> 0) and (FEPSG <> 0) and (FEPSG = ADatum.EPSG);
  end;
end;

end.

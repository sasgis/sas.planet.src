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
  i_Datum,
  u_BaseInterfacedObject;

type
  TDatum = class(TBaseInterfacedObject, IDatum)
  private
    FEPSG: Integer;
    FRadiusA: Double;
    FRadiusB: Double;
    FExct: Double;
    FFlattening: Double;
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
    function CalcDist(const AStart, AFinish: TDoublePoint): Double; overload;

    function CalcDist(
      const AStart: TDoublePoint;
      const AFinish: TDoublePoint;
      out AInitialBearing: Double;
      out AFinalBearing: Double
    ): Double; overload;

    function CalcFinishPosition(
      const AStart: TDoublePoint;
      const AInitialBearing: Double;
      const ADistance: Double
    ): TDoublePoint;
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
  SysUtils,
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
  FFlattening := (FRadiusA - FRadiusB) / FRadiusA;
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
  VInitialBearing: Double;
  VFinalBearing: Double;
begin
  Result := CalcDist(AStart, AFinish, VInitialBearing, VFinalBearing);
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

  function Det(const point1, point2, point3: TDoublePoint): Extended;
  begin
    Result := ((point2.X - point1.X) * (point3.Y - point1.Y) - (point2.Y - point1.Y) * (point3.X - point1.X)) / 2;
  end;

  function InTriangle(const point, point1, point2, point3: TDoublePoint): Boolean;
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

function TDatum.CalcFinishPosition(
  const AStart: TDoublePoint;
  const AInitialBearing: Double;
  const ADistance: Double
): TDoublePoint;
var
  SinTc, CosTc, U1, TanU1, SinU1, CosU1, SinAlpha, CosAlpha, Usqr, A, B, C,
  Term1, Term2, Term3, Sigma, SinSigma, CosSigma, TanSigma1, DeltaSigma,
  Sigma1, TwoSigmaM, c2sm, LastSigma, Lambda, Omega, aa, bb, r0: Extended;
  Lat, Lon, Lat1, Lon1, Azimuth: Double;
begin
  if ADistance = 0.0 then begin  // Coincident points
    Result := AStart;
    Exit;
  end;

  Lat1 := AStart.Y * D2R;
  Lon1 := AStart.X * D2R;

  Azimuth := AInitialBearing * D2R;

  aa := FRadiusA * FRadiusA;
  bb := FRadiusB * FRadiusB;
  r0 := 1 - FFlattening;

  // === Thaddeus Vincenty's direct algorithm for ellipsoids: ==================
  TanU1 := r0 * Tan(Lat1);
  TanSigma1 := TanU1 / Cos(Azimuth);                                       //eq 1
  U1 := ArcTan(TanU1);
  SinCos(U1, SinU1, CosU1);
  SinAlpha := CosU1 * Sin(Azimuth);                                      //eq 2
  CosAlpha := Sqrt(1 - Sqr(SinAlpha));
  Usqr := Sqr(CosAlpha) * (aa - bb) / bb;

  Term1 := Usqr / 16384;
  Term2 := 4096 + Usqr * (-768 + Usqr * (320 - 175 * Usqr));

  A := 1 + Term1 * Term2;
  B := Usqr / 1024 * (256 + Usqr * (-128 + Usqr * (74 - 47 * Usqr)));    //eq 4

  Sigma := ADistance / (FRadiusB * A);
  Sigma1 := ArcTan(TanSigma1);

  repeat
    LastSigma := Sigma;
    TwoSigmaM := 2 * Sigma1 + Sigma;                                     //eq 5
    SinCos(Sigma, SinSigma, CosSigma);
    SinCos(Azimuth, SinTc, CosTc);
    c2sm := Cos(TwoSigmaM);

    DeltaSigma :=
      B * SinSigma * (c2sm + b / 4 * (CosSigma * (-1 + 2 * Sqr(c2sm)) -
      B / 6 * c2sm * (-3 + 4 * Sqr(SinSigma)) * (-3 + 4 * Sqr(c2sm))));  //eq 6

    Sigma := ADistance / (FRadiusB * A) + DeltaSigma;                    //eq 7
  until (Abs(Sigma - LastSigma) <= 1E-12);

  TwoSigmaM := 2 * Sigma1 + Sigma;                                       //eq 5
  c2sm := Cos(TwoSigmaM);
  SinCos(Sigma, SinSigma, CosSigma);
  Term1 := SinU1 * CosSigma + CosU1 * SinSigma * CosTc;
  Term2 := Sqr(SinAlpha) + Sqr(SinU1 * SinSigma - CosU1 * CosSigma * CosTc);
  Term3 := r0 * Sqrt(Term2);
  Lat := ArcTan2(Term1, Term3);
  Term1 := SinSigma * Sin(Azimuth);
  Term2 := CosU1 * CosSigma - SinU1 * SinSigma * CosTc;
  Lambda := ArcTan2(Term1, Term2);

  C := FFlattening / 16 * Sqr(CosAlpha) * (4 + FFlattening * (4 - 3 * Sqr(CosAlpha)));

  Omega := Lambda - (1 - c) * FFlattening * SinAlpha *
    (Sigma + C * SinSigma * (c2sm + C * CosSigma * (-1 + 2 * Sqr(c2sm))));

  Lon := Lon1 + Omega;
  //============================================================================

  Result.Y := Lat / D2R;
  Result.X := Lon / D2R;
end;

function TDatum.CalcDist(
  const AStart: TDoublePoint;
  const AFinish: TDoublePoint;
  out AInitialBearing: Double;
  out AFinalBearing: Double
): Double;
var
  IterLimit: Integer;
  L, U1, U2, SinU1, CosU1, SinU2, CosU2, Lambda, LambdaP, SinLambda, CosLambda,
  Sigma, SinSigma, CosSigma, Cos2SigmaM, SinAlpha, CosSqAlpha, uSq, A, B, C,
  DeltaSigma, Lat1, Lon1, Lat2, Lon2, aa, bb, r0: Extended;
begin
  Result := 0;
  AInitialBearing := 0;
  AFinalBearing := 0;

  Lat1 := AStart.Y * D2R;
  Lon1 := AStart.X * D2R;

  Lat2 := AFinish.Y * D2R;
  Lon2 := AFinish.X * D2R;

  aa := FRadiusA * FRadiusA;
  bb := FRadiusB * FRadiusB;
  r0 := 1 - FFlattening;

  L := Lon2 - Lon1;

  // === Thaddeus Vincenty's inverse algorithm: ================================
  U1 := ArcTan(r0 * Tan(Lat1));
  U2 := ArcTan(r0 * Tan(Lat2));
  SinCos(U1, SinU1, CosU1);
  SinCos(U2, SinU2, CosU2);
  Lambda := L;
  IterLimit := 100;

  repeat
    SinCos(Lambda, SinLambda, CosLambda);
    SinSigma :=
      Sqrt((Sqr(CosU2 * SinLambda)) + Sqr(CosU1 * SinU2 - SinU1 * CosU2 * CosLambda));
    if SinSigma = 0 then begin // co-incident points
      Exit;
    end;
    CosSigma := SinU1 * SinU2 + CosU1 * CosU2 * CosLambda;
    Sigma := ArcTan2(SinSigma, CosSigma);
    SinAlpha := CosU1 * CosU2 * SinLambda / SinSigma;
    CosSqAlpha := 1 - Sqr(SinAlpha);
    if CosSqAlpha <> 0 then begin
      Cos2SigmaM := CosSigma - 2 * SinU1 * SinU2 / CosSqAlpha;
    end else begin
      Cos2SigmaM := 0; // equatorial line: cosSqAlpha=0
    end;
    C := FFlattening / 16 * CosSqAlpha * (4 + FFlattening * (4 - 3 * CosSqAlpha));
    LambdaP := Lambda;
    Lambda := L + (1 - C) * FFlattening * SinAlpha *
      (Sigma + C * SinSigma * (Cos2SigmaM + C * CosSigma * (-1 + 2 * Sqr(Cos2SigmaM))));
    Dec(IterLimit);
    if (IterLimit <= 0) then begin
      raise Exception.CreateFmt(
        'Vincenty''s inverse algorithm failed to converge!' + #13#10 +
        'Start point (lat; lon): %.6f; %.6f' + #13#10 +
        'Finish point (lat; lon): %.6f; %.6f',
        [AStart.Y, AStart.X, AFinish.Y, AFinish.X]
      );
    end;
  until not (Abs(Lambda - LambdaP) > 1E-12);

  uSq := CosSqAlpha * (aa - bb) / bb;
  A := 1 + uSq / 16384 * (4096 + uSq * (-768 + uSq * (320 - 175 * uSq)));
  B := uSq / 1024 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)));
  DeltaSigma :=
    B * SinSigma * (Cos2SigmaM + B / 4 * (CosSigma * (-1 + 2 * Sqr(Cos2SigmaM)) -
    B / 6 * Cos2SigmaM * (-3 + 4 * Sqr(SinSigma)) * (-3 + 4 * Sqr(Cos2SigmaM))));

  AInitialBearing := ArcTan2(CosU2 * SinLambda, CosU1 * SinU2 - SinU1 * CosU2 * CosLambda);
  if (AInitialBearing < 0) then begin
    AInitialBearing := 2 * Pi + AInitialBearing;
  end;

  AFinalBearing := ArcTan2(CosU1 * SinLambda, (-SinU1 * CosU2 + CosU1 * SinU2 * CosLambda)) - Pi; // ?? (- Pi)
  if (AFinalBearing < 0) then begin
    AFinalBearing := 2 * Pi + AFinalBearing;
  end;
  //============================================================================

  AInitialBearing := AInitialBearing / D2R;
  AFinalBearing := AFinalBearing / D2R;
  Result := FRadiusB * A * (Sigma - DeltaSigma);
end;

end.

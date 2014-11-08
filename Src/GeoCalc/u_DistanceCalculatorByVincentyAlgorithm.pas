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

unit u_DistanceCalculatorByVincentyAlgorithm;

interface

uses
  i_DistanceCalculator,
  u_BaseInterfacedObject;

type
  TDistanceCalculatorByVincentyAlgorithm = class(TBaseInterfacedObject, IDistanceCalculator)
  private
    aa, bb, r0: Extended;
  private
    FRadiusA: Double;
    FRadiusB: Double;
    FFlattening: Double;
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
  end;

implementation

uses
  SysUtils,
  Math;

const
  DEG2RAD: Double = 0.017453292519943295769236907684886;

{ TDistanceCalculatorByVincentyAlgorithm }

constructor TDistanceCalculatorByVincentyAlgorithm.Create(
  const ARadiusA: Double;
  const ARadiusB: Double
);
begin
  inherited Create;
  FRadiusA := ARadiusA;
  FRadiusB := ARadiusB;
  FFlattening := (FRadiusA - FRadiusB) / FRadiusA;

  aa := FRadiusA * FRadiusA;
  bb := FRadiusB * FRadiusB;
  r0 := 1 - FFlattening;
end;

procedure TDistanceCalculatorByVincentyAlgorithm.ComputeFinishPosition(
  const ALat1, ALon1: Double;
  const AInitialBearing: Double;
  const ADistance: Double;
  out ALat2, ALon2: Double
);
var
  SinTc, CosTc, U1, TanU1, SinU1, CosU1, SinAlpha, CosAlpha, Usqr, A, B, C,
  Term1, Term2, Term3, Sigma, SinSigma, CosSigma, TanSigma1, DeltaSigma,
  Sigma1, TwoSigmaM, c2sm, LastSigma, Lambda, Omega: Extended;
  Lat, Lon, Lat1, Lon1, Azimuth: Double;
begin
  if SameValue(ADistance, 0) then begin  // Coincident points
    ALat2 := ALat1;
    ALon2 := ALon1;
    Exit;
  end;

  Lat1 := ALat1 * DEG2RAD;
  Lon1 := ALon1 * DEG2RAD;

  Azimuth := AInitialBearing * DEG2RAD;

  // === Thaddeus Vincenty's direct algorithm for ellipsoids: ==================

  TanU1 := r0 * Tan(Lat1);
  TanSigma1 := TanU1 / Cos(Azimuth);                                     //eq 1
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

  ALat2 := Lat / DEG2RAD;
  ALon2 := Lon / DEG2RAD;
end;

function TDistanceCalculatorByVincentyAlgorithm.ComputeDistance(
  const ALat1, ALon1: Double;
  const ALat2, ALon2: Double;
  out AInitialBearing: Double;
  out AFinalBearing: Double
): Double;
var
  VIterLimit: Integer;
  L, U1, U2, SinU1, CosU1, SinU2, CosU2, Lambda, LambdaP, SinLambda, CosLambda,
  Sigma, SinSigma, CosSigma, Cos2SigmaM, SinAlpha, CosSqAlpha, uSq, A, B, C,
  DeltaSigma, Lat1, Lon1, Lat2, Lon2: Extended;
begin
  Result := 0;

  AInitialBearing := 0;
  AFinalBearing := 0;

  Lat1 := ALat1 * DEG2RAD;
  Lon1 := ALon1 * DEG2RAD;

  Lat2 := ALat2 * DEG2RAD;
  Lon2 := ALon2 * DEG2RAD;

  L := Lon2 - Lon1;

  // === Thaddeus Vincenty's inverse algorithm: ================================

  U1 := ArcTan(r0 * Tan(Lat1));
  U2 := ArcTan(r0 * Tan(Lat2));
  SinCos(U1, SinU1, CosU1);
  SinCos(U2, SinU2, CosU2);

  Lambda := L;
  LambdaP := 2 * Pi;

  SinLambda := 0;
  CosLambda := 0;
  SinSigma := 0;
  CosSigma := 0;
  Sigma := 0;
  CosSqAlpha := 0;
  Cos2SigmaM := 0;
  VIterLimit := 100;

  while (Abs(Lambda - LambdaP) > 1E-12) and (VIterLimit > 0) do begin
    Dec(VIterLimit);

    SinCos(Lambda, SinLambda, CosLambda);
    SinSigma :=
      Sqrt((Sqr(CosU2 * SinLambda)) + Sqr(CosU1 * SinU2 - SinU1 * CosU2 * CosLambda));
    if SameValue(SinSigma, 0) then begin // co-incident points
      Exit;
    end;
    CosSigma := SinU1 * SinU2 + CosU1 * CosU2 * CosLambda;
    Sigma := ArcTan2(SinSigma, CosSigma);
    SinAlpha := CosU1 * CosU2 * SinLambda / SinSigma;
    CosSqAlpha := 1 - Sqr(SinAlpha);
    if not SameValue(CosSqAlpha, 0) then begin
      Cos2SigmaM := CosSigma - 2 * SinU1 * SinU2 / CosSqAlpha;
    end else begin
      Cos2SigmaM := 0; // equatorial line: cosSqAlpha=0
    end;
    C := FFlattening / 16 * CosSqAlpha * (4 + FFlattening * (4 - 3 * CosSqAlpha));
    LambdaP := Lambda;
    Lambda := L + (1 - C) * FFlattening * SinAlpha *
      (Sigma + C * SinSigma * (Cos2SigmaM + C * CosSigma * (-1 + 2 * Sqr(Cos2SigmaM))));
  end;

  if VIterLimit <= 0 then begin // formula failed to converge
    raise Exception.CreateFmt(
      'Vincenty''s inverse algorithm failed to converge!' + #13#10 +
      'Start point (lat; lon): %.6f; %.6f' + #13#10 +
      'Finish point (lat; lon): %.6f; %.6f',
      [ALat1, ALon1, ALat2, ALon2]
    );
  end;

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

  AFinalBearing := ArcTan2(CosU1 * SinLambda, (-SinU1 * CosU2 + CosU1 * SinU2 * CosLambda)) - Pi;
  if (AFinalBearing < 0) then begin
    AFinalBearing := 2 * Pi + AFinalBearing;
  end;

  //============================================================================

  AInitialBearing := AInitialBearing / DEG2RAD;
  AFinalBearing := AFinalBearing / DEG2RAD;

  Result := FRadiusB * A * (Sigma - DeltaSigma);
end;

end.

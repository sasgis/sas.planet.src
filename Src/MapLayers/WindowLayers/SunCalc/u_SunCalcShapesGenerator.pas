{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_SunCalcShapesGenerator;

interface

uses
  Types,
  GR32,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_SunCalcShapesGenerator,
  u_GeoFunc,
  u_BaseInterfacedObject;

type
  TSunCalcShapesGenerator = class(TBaseInterfacedObject, ISunCalcShapesGenerator)
  private
    FRadius: Integer;
    FCenter: TFloatPoint;
    FLocation: TDoublePoint;
    FDateTime: TDateTime;

    FCirclePoints: TArrayOfFixedPoint;
    FIsCirclePointsValid: Boolean;

    FLongestDayPoints: TArrayOfFixedPoint;
    FShortestDayPoints: TArrayOfFixedPoint;
    FSunlightFillPoints: TArrayOfFixedPoint;
    FIsYearInfoPointsValid: Boolean;

    FDayPoints: TArrayOfFixedPoint;
    FSunrisePoint: TFixedPoint;
    FSunsetPoint: TFixedPoint;
    FIsDayInfoPointsValid: Boolean;

    FSunPos: TFixedPoint;
    FIsTimeInfoPointsValid: Boolean;

    FSolstice: array [0..1] of TDateTime;

    FLocalCoordConverter: ILocalCoordConverterChangeable;
  private
    function CalcCenter: TFloatPoint;
    function IsValidSate: Boolean; inline;
  private
    { ISunCalcShapesGenerator }
    procedure ValidateCache;

    function IsIntersectScreenRect: Boolean;

    procedure SetLocation(const AValue: TDoublePoint);
    procedure SetDateTime(const AValue: TDateTime);

    procedure GetCirclePoints(
      out ACirclePoints: TArrayOfFixedPoint
    );

    procedure GetYearInfoPoints(
      out ALongestDayPoints: TArrayOfFixedPoint;
      out AShortestDayPoints: TArrayOfFixedPoint;
      out ASunlightFillPoints: TArrayOfFixedPoint
    );

    procedure GetDayInfoPoints(
      out ADayPoints: TArrayOfFixedPoint;
      out ASunrise: TFixedPoint;
      out ASunset: TFixedPoint;
      out ACenter: TFixedPoint
    );

    procedure GetTimeInfoPoints(
      out ASunPos: TFixedPoint;
      out ACenter: TFixedPoint
    );
  public
    constructor Create(
      const ALocalCoordConverter: ILocalCoordConverterChangeable;
      const ARadius: Integer
    );
  end;

implementation

uses
  Math,
  DateUtils,
  GR32_Math,
  Solstice,
  SunCalc;

function IsSameFloatPoint(const A, B: TFloatPoint): Boolean; inline;
begin
  Result := SameValue(A.X, B.X) and SameValue(A.Y, B.Y);
end;

{ TSunCalcShapesGenerator }

constructor TSunCalcShapesGenerator.Create(
  const ALocalCoordConverter: ILocalCoordConverterChangeable;
  const ARadius: Integer
);
begin
  Assert(ALocalCoordConverter <> nil);
  Assert(ARadius > 0);

  inherited Create;

  FLocalCoordConverter := ALocalCoordConverter;
  FRadius := ARadius;

  FDateTime := 0;
  FLocation := CEmptyDoublePoint;

  FCenter.X := NaN;
  FCenter.Y := NaN;

  FIsCirclePointsValid := False;
  FIsYearInfoPointsValid := False;
  FIsDayInfoPointsValid := False;
  FIsTimeInfoPointsValid := False;

  FSolstice[0] := 0;
  FSolstice[1] := 0;
end;

function GenerateCircle(
  const ACenter: TFloatPoint;
  const ARadius: TFloat
): TArrayOfFixedPoint;
const
  cSteps: Integer = 72;
var
  I: Integer;
  M: TFloat;
  C, D: TFloatPoint;
begin
  SetLength(Result, cSteps);
  M := 2 * System.Pi / cSteps;

  // first item
  Result[0].X := Fixed(ARadius + ACenter.X);
  Result[0].Y := Fixed(ACenter.Y);

  // calculate complex offset
  GR32_Math.SinCos(M, C.Y, C.X);
  D.X := ARadius * C.X;
  D.Y := ARadius * C.Y;

  // second item
  Result[1].X := Fixed(D.X + ACenter.X);
  Result[1].Y := Fixed(D.Y + ACenter.Y);

  // other items
  for I := 2 to cSteps - 1 do begin
    D := FloatPoint(D.X * C.X - D.Y * C.Y, D.Y * C.X + D.X * C.Y);

    Result[I].X := Fixed(D.X + ACenter.X);
    Result[I].Y := Fixed(D.Y + ACenter.Y);
  end;
end;

procedure TSunCalcShapesGenerator.GetCirclePoints(
  out ACirclePoints: TArrayOfFixedPoint
);
begin
  if not IsValidSate then begin
    SetLength(ACirclePoints, 0);
    Exit;
  end;

  Assert(IsIntersectScreenRect, 'SunCalc: Shape will be out of screen rect!');

  if not FIsCirclePointsValid then begin
    FCirclePoints := GenerateCircle(FCenter, FRadius);
    FIsCirclePointsValid := True;
  end;
  ACirclePoints := FCirclePoints;
end;

procedure GetSunPosPoint(
  const ADate: TDateTime;
  const ALocation: TDoublePoint;
  const ACenter: TFloatPoint;
  const ARadius: Integer;
  out APoint: TFixedPoint;
  out AIsBelowHorizont: Boolean
); inline;
var
  VSunPos: TSunPos;
  VAngle: Double;
  R: Double;
begin
  VSunPos := SunCalc.GetPosition(ADate, ALocation.Y, ALocation.X);
  VAngle := Pi / 2 + VSunPos.Azimuth;
  R := ARadius * Cos(VSunPos.Altitude);
  APoint.X := Fixed(ACenter.X + R * Cos(VAngle));
  APoint.Y := Fixed(ACenter.Y + R * Sin(VAngle));
  AIsBelowHorizont := VSunPos.Altitude < 0;
end;

function GenerateCurvePoints(
  const ADate: TDateTime;
  const ALocation: TDoublePoint;
  const ACenter: TFloatPoint;
  const ARadius: Integer;
  const AIgnoreInvisiblePos: Boolean
): TArrayOfFixedPoint;
const
  cStep = 20; // minutes
var
  I, J: Integer;
  VCount: Integer;
  VTime: TSunCalcTimesInfo;
  VSunTimes: TSunCalcTimes;
  VStart, VEnd: TDateTime;
  VIsCurveBelowHorizont: Boolean;
  VIsPointBelowHorizont: Boolean;
  VTimePoints: array of TDateTime;
begin
  VSunTimes := SunCalc.GetTimes(ADate, ALocation.Y, ALocation.X);

  // sunrise start
  VTime := VSunTimes[sunrise];
  if VTime.Value <> 0 then begin
    VStart := VTime.Value;
  end else begin
    VStart := ADate;
  end;

  // sunset end
  VTime := VSunTimes[sunset];
  if VTime.Value <> 0 then begin
    VEnd := VTime.Value;
  end else begin
    VEnd := IncDay(ADate, 1);
  end;

  // make time points
  VCount := MinutesBetween(VStart, VEnd) div cStep;
  SetLength(VTimePoints, VCount + 1);
  for I := 0 to VCount - 1 do begin
    VTimePoints[I] := IncMinute(VStart, I * cStep);
  end;
  VTimePoints[VCount] := VEnd;

  // calc points posions
  J := 0;
  VIsCurveBelowHorizont := True;
  SetLength(Result, Length(VTimePoints));
  for I := 0 to Length(VTimePoints) - 1 do begin
    GetSunPosPoint(
      VTimePoints[I],
      ALocation,
      ACenter,
      ARadius,
      Result[J],
      VIsPointBelowHorizont
    );
    if VIsPointBelowHorizont then begin
      if not AIgnoreInvisiblePos then begin
        Inc(J);
      end;
    end else begin
      VIsCurveBelowHorizont := False;
      Inc(J);
    end;
  end;
  if VIsCurveBelowHorizont then begin
    SetLength(Result, 0);
  end else begin
    SetLength(Result, J);
  end;
end;

function GenerateCircleSegment(
  const ACenter: TFloatPoint;
  const ARadius: Integer;
  const AStart: TFixedPoint;
  const AEnd: TFixedPoint
): TArrayOfFixedPoint;
const
  cStep: Double = 0.0873; // 5 degree
var
  I: Integer;
  A, A1, A2: Double;
  P, P1, P2: TFloatPoint;
  VSteps: Integer;
  C, D: TFloatPoint;
begin
  P := ACenter;

  P1 := FloatPoint(AStart);
  P2 := FloatPoint(AEnd);

  A1 := ArcTan2(-(P1.Y - P.Y), P1.X - P.X);
  A2 := ArcTan2(-(P2.Y - P.Y), P2.X - P.X);

  A := A1 - A2;
  if A < 0 then begin
    A := 2 * Pi + A;
  end;

  VSteps := Ceil(A / cStep) + 1;
  SetLength(Result, VSteps);

  GR32_Math.SinCos(cStep, C.Y, C.X);
  D.X := P1.X - P.X;
  D.Y := P1.Y - P.Y;

  Result[0].X := Fixed(D.X + P.X);
  Result[0].Y := Fixed(D.Y + P.Y);

  for I := 1 to VSteps - 1 do begin
    D := FloatPoint(D.X * C.X - D.Y * C.Y, D.Y * C.X + D.X * C.Y);

    Result[I].X := Fixed(D.X + P.X);
    Result[I].Y := Fixed(D.Y + P.Y);
  end;
end;

procedure TSunCalcShapesGenerator.GetYearInfoPoints(
  out ALongestDayPoints: TArrayOfFixedPoint;
  out AShortestDayPoints: TArrayOfFixedPoint;
  out ASunlightFillPoints: TArrayOfFixedPoint
);
var
  I, J: Integer;
  VYear: Word;
  VLen1, VLen2, VLen3, VLen4: Integer;
  VSeg1, VSeg2: TArrayOfFixedPoint;
begin
  if not IsValidSate then begin
    SetLength(ALongestDayPoints, 0);
    SetLength(AShortestDayPoints, 0);
    SetLength(ASunlightFillPoints, 0);
    Exit;
  end;

  Assert(IsIntersectScreenRect, 'SunCalc: Shape will be out of screen rect!');

  if not FIsYearInfoPointsValid then begin

    VYear := YearOf(FDateTime);
    if (FSolstice[0] = 0) or (VYear <> YearOf(FSolstice[0])) then begin
      FSolstice[0] := Solstice.June(VYear);
      FSolstice[1] := Solstice.December(VYear);
    end;

    if FLocation.Y > 0 then begin
      I := 0;
      J := 1;
    end else begin
      I := 1;
      J := 0;
    end;

    // Longest day
    FLongestDayPoints := GenerateCurvePoints(FSolstice[I], FLocation, FCenter, FRadius, False);

    // Shortest day
    FShortestDayPoints := GenerateCurvePoints(FSolstice[J], FLocation, FCenter, FRadius, False);

    // Polygon between longest and shortest day
    VLen1 := Length(FLongestDayPoints);
    VLen2 := Length(FShortestDayPoints);
    if (VLen1 > 0) and (VLen2 > 0) then begin
      VSeg1 :=
        GenerateCircleSegment(
          FCenter,
          FRadius,
          FLongestDayPoints[0],
          FShortestDayPoints[0]
        );
      VLen3 := Length(VSeg1);

      VSeg2 :=
        GenerateCircleSegment(
          FCenter,
          FRadius,
          FShortestDayPoints[VLen2 - 1],
          FLongestDayPoints[VLen1 - 1]
        );
      VLen4 := Length(VSeg2);

      SetLength(FSunlightFillPoints, VLen1 + VLen2 + VLen3 + VLen4);

      Move(FLongestDayPoints[0], FSunlightFillPoints[0], VLen1 * SizeOf(TFixedPoint));

      J := VLen1;

      for I := VLen4 - 1 downto 0 do begin
        FSunlightFillPoints[J] := VSeg2[I];
        Inc(J);
      end;

      for I := VLen2 - 1 downto 0 do begin
        FSunlightFillPoints[J] := FShortestDayPoints[I];
        Inc(J);
      end;

      for I := VLen3 - 1 downto 0 do begin
        FSunlightFillPoints[J] := VSeg1[I];
        Inc(J);
      end;
    end else begin
      SetLength(FSunlightFillPoints, 0);
    end;

    FIsYearInfoPointsValid := True;
  end;

  ALongestDayPoints := FLongestDayPoints;
  AShortestDayPoints := FShortestDayPoints;
  ASunlightFillPoints := FSunlightFillPoints;
end;

procedure TSunCalcShapesGenerator.GetDayInfoPoints(
  out ADayPoints: TArrayOfFixedPoint;
  out ASunrise, ASunset, ACenter: TFixedPoint
);
var
  VSunTimes: TSunCalcTimes;
  VIsPointBelowHorizont: Boolean;
begin
  if not IsValidSate then begin
    SetLength(ADayPoints, 0);
    ASunrise := FixedPoint(0, 0);
    ASunset := FixedPoint(0, 0);
    ACenter := FixedPoint(0, 0);
    Exit;
  end;

  Assert(IsIntersectScreenRect, 'SunCalc: Shape will be out of screen rect!');

  if not FIsDayInfoPointsValid then begin
    FDayPoints := GenerateCurvePoints(FDateTime, FLocation, FCenter, FRadius, False);

    VSunTimes := SunCalc.GetTimes(FDateTime, FLocation.Y, FLocation.X);

    // sunrise
    GetSunPosPoint(
      VSunTimes[sunrise].Value,
      FLocation,
      FCenter,
      FRadius,
      FSunrisePoint,
      VIsPointBelowHorizont
    );

    // sunset
    GetSunPosPoint(
      VSunTimes[sunset].Value,
      FLocation,
      FCenter,
      FRadius,
      FSunsetPoint,
      VIsPointBelowHorizont
    );

    FIsDayInfoPointsValid := True;
  end;

  ADayPoints := FDayPoints;
  ASunrise := FSunrisePoint;
  ASunset := FSunsetPoint;
  ACenter := FixedPoint(FCenter);
end;

procedure TSunCalcShapesGenerator.GetTimeInfoPoints(
  out ASunPos, ACenter: TFixedPoint
);
var
  VIsPointBelowHorizont: Boolean;
begin
  if not IsValidSate then begin
    ASunPos := FixedPoint(0, 0);
    ACenter := FixedPoint(0, 0);
    Exit;
  end;

  Assert(IsIntersectScreenRect, 'SunCalc: Shape will be out of screen rect!');

  if not FIsTimeInfoPointsValid then begin
    GetSunPosPoint(
      FDateTime,
      FLocation,
      FCenter,
      FRadius,
      FSunPos,
      VIsPointBelowHorizont
    );

    if VIsPointBelowHorizont then begin
      FSunPos := FixedPoint(0, 0);
    end;

    FIsTimeInfoPointsValid := True;
  end;

  ASunPos := FSunPos;
  ACenter := FixedPoint(FCenter);
end;

procedure TSunCalcShapesGenerator.SetDateTime(const AValue: TDateTime);
begin
  if FDateTime <> AValue then begin
    FIsTimeInfoPointsValid := False;
    FIsDayInfoPointsValid := FIsDayInfoPointsValid and IsSameDay(FDateTime, AValue);
    FDateTime := AValue;
  end;
end;

procedure TSunCalcShapesGenerator.SetLocation(const AValue: TDoublePoint);
begin
  if not DoublePointsEqual(FLocation, AValue) then begin
    FLocation := AValue;
    FCenter := CalcCenter;

    FIsCirclePointsValid := False;
    FIsYearInfoPointsValid := False;
    FIsDayInfoPointsValid := False;
    FIsTimeInfoPointsValid := False;
  end;
end;

procedure TSunCalcShapesGenerator.ValidateCache;
var
  VCenter: TFloatPoint;
begin
  if not IsValidSate then begin
    Exit;
  end;

  VCenter := CalcCenter;
  if not IsSameFloatPoint(FCenter, VCenter) then begin
    FCenter := VCenter;
    FIsCirclePointsValid := False;
    FIsYearInfoPointsValid := False;
    FIsDayInfoPointsValid := False;
    FIsTimeInfoPointsValid := False;
  end;
end;

function TSunCalcShapesGenerator.IsIntersectScreenRect: Boolean;
var
  VPoint: TDoublePoint;
  VScreenRect, VShapesRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
begin
  if PointIsEmpty(FLocation) then begin
    Result := False;
  end else begin
    VLocalConverter := FLocalCoordConverter.GetStatic;
    VPoint := VLocalConverter.LonLat2LocalPixelFloat(FLocation);
    VShapesRect :=
      DoubleRect(
        VPoint.X - FRadius,
        VPoint.Y - FRadius,
        VPoint.X + FRadius,
        VPoint.Y + FRadius
      );
    VScreenRect := DoubleRect(VLocalConverter.GetLocalRect);
    Result := IsIntersecProjectedRect(VShapesRect, VScreenRect);
  end;
end;

function TSunCalcShapesGenerator.IsValidSate: Boolean;
begin
  Result :=
    (FDateTime <> 0) and
    not PointIsEmpty(FLocation) and
    not IsNan(FCenter.X) and not IsNan(FCenter.Y);
end;

function TSunCalcShapesGenerator.CalcCenter: TFloatPoint;
var
  VPoint: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
begin
  Assert(not PointIsEmpty(FLocation));
  VLocalConverter := FLocalCoordConverter.GetStatic;
  VPoint := VLocalConverter.LonLat2LocalPixelFloat(FLocation);
  Result := FloatPoint(VPoint.X, VPoint.Y);
end;

end.

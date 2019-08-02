{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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
  Math,
  GR32,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_SunCalcDataProvider,
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
    FTimeZone: Double;
    FStartOfTheDay: TDateTime;
    FEndOfTheDay: TDateTime;
    FDataProvider: ISunCalcDataProvider;

    FCirclePoints: TArrayOfFixedPoint;
    FIsCirclePointsValid: Boolean;

    FMinAltitude: Double;

    FMaxAltitudeDayPoints: TArrayOfFixedPoint;
    FMinAltitudeDayPoints: TArrayOfFixedPoint;
    FMinMaxAltitudePoly: TArrayOfFixedPoint;
    FIsAltitudePointsValid: Boolean;

    FDayPoints: TArrayOfArrayOfFixedPoint;
    FRisePoint: TFixedPoint;
    FSetPoint: TFixedPoint;
    FIsDayInfoPointsValid: Boolean;

    FCurrentPos: TFixedPoint;
    FIsCurrentPosValid: Boolean;

    FLocalCoordConverter: ILocalCoordConverterChangeable;
  private
    function CalcCenter: TFloatPoint;
    function IsValidSate: Boolean; inline;

    function GetPointPosition(
      const ADate: TDateTime;
      out AAltitude: Double
    ): TFixedPoint; inline;

    function GenerateCurvePoints(
      const AStartOfTheDay: TDateTime;
      const AEndOfTheDay: TDateTime
    ): TArrayOfArrayOfFixedPoint;

    procedure UpdateMinAltitude(
      const AStartOfTheDay: TDateTime;
      const AEndOfTheDay: TDateTime
    );
  private
    { ISunCalcShapesGenerator }
    procedure ValidateCache;

    function IsIntersectScreenRect: Boolean;

    procedure SetLocation(
      const ALonLat: TDoublePoint
    );
    procedure SetDateTime(
      const AUtcDateTime: TDateTime;
      const AUtcOffset: Double
    );

    procedure SetDataProvider(
      const AProvider: ISunCalcDataProvider
    );

    procedure GetCirclePoints(
      out ACirclePoints: TArrayOfFixedPoint
    );

    procedure GetMinMaxAltitudePoints(
      out AMinAltitudePoints: TArrayOfFixedPoint;
      out AMaxAltitudePoints: TArrayOfFixedPoint;
      out AMinMaxAltitudePolygon: TArrayOfFixedPoint
    );

    procedure GetDayInfoPoints(
      out ADayPoints: TArrayOfArrayOfFixedPoint;
      out ARise: TFixedPoint;
      out ASet: TFixedPoint;
      out ACenter: TFixedPoint
    );

    procedure GetTimeInfoPoints(
      out APos: TFixedPoint;
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
  DateUtils,
  GR32_Math,
  t_SunCalcDataProvider,
  u_TimeZoneInfo;

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

  FMinAltitude := 0;

  FDateTime := 0;
  FStartOfTheDay := 0;
  FEndOfTheDay := 0;
  FLocation := CEmptyDoublePoint;
  FDataProvider := nil;

  FCenter.X := NaN;
  FCenter.Y := NaN;

  FIsCirclePointsValid := False;
  FIsAltitudePointsValid := False;
  FIsDayInfoPointsValid := False;
  FIsCurrentPosValid := False;
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

  Assert(IsIntersectScreenRect, 'SunCalc: Shape is out of screen rect!');

  if not FIsCirclePointsValid then begin
    FCirclePoints := GenerateCircle(FCenter, FRadius);
    FIsCirclePointsValid := True;
  end;
  ACirclePoints := FCirclePoints;
end;

function TSunCalcShapesGenerator.GetPointPosition(
  const ADate: TDateTime;
  out AAltitude: Double
): TFixedPoint;
var
  VPos: TSunCalcProviderPosition;
  VAngle: Double;
  R: Double;
begin
  if ADate = 0 then begin
    AAltitude := 0;
    Result := FixedPoint(0, 0);
    Exit;
  end;
  VPos := FDataProvider.GetPosition(ADate, FLocation);
  VAngle := Pi / 2 + VPos.Azimuth;
  R := FRadius * Cos(VPos.Altitude);
  AAltitude := VPos.Altitude;
  Result.X := Fixed(FCenter.X + R * Cos(VAngle));
  Result.Y := Fixed(FCenter.Y + R * Sin(VAngle));
end;

function TSunCalcShapesGenerator.GenerateCurvePoints(
  const AStartOfTheDay: TDateTime;
  const AEndOfTheDay: TDateTime
): TArrayOfArrayOfFixedPoint;

  function GetInterval(const AStart, AEnd: TDateTime): TArrayOfFixedPoint;
  const
    cStep = 20; // minutes
  var
    I, J: Integer;
    VCount: Integer;
    VAltitude: Double;
    VTimePoints: array of TDateTime;
  begin
    // make times
    VCount := Floor(MinutesBetween(AStart, AEnd) / cStep) + 1;
    SetLength(VTimePoints, VCount);

    for I := 0 to VCount - 2 {!} do begin
      VTimePoints[I] := IncMinute(AStart, I * cStep);
    end;
    VTimePoints[VCount-1] := AEnd;

    SetLength(Result, VCount);

    // calc positions
    J := 0;
    for I := 0 to VCount - 1 do begin
      Result[J] := GetPointPosition(VTimePoints[I], VAltitude);
      if VAltitude >= FMinAltitude then begin
        Inc(J);
      end;
    end;

    if J = 1 then begin
      Dec(J);
    end;

    SetLength(Result, J);
  end;

var
  I: Integer;
  VTimes: TSunCalcProviderTimes;
begin
  UpdateMinAltitude(AStartOfTheDay, AEndOfTheDay);

  SetLength(Result, 0);

  if (AStartOfTheDay = 0) or (AEndOfTheDay = 0) then begin
    Exit;
  end;

  VTimes := FDataProvider.GetTimes(AStartOfTheDay, AEndOfTheDay, FLocation);

  if VTimes.RiseTime = 0 then begin
    VTimes.RiseTime := AStartOfTheDay;
  end;

  if VTimes.SetTime = 0 then begin
    VTimes.SetTime := AEndOfTheDay;
  end;

  if SameValue(VTimes.RiseTime, VTimes.SetTime) then begin
    Exit;
  end;

  I := 0;

  if VTimes.RiseTime < VTimes.SetTime then begin
    SetLength(Result, 1);

    Result[I] := GetInterval(VTimes.RiseTime, VTimes.SetTime);
    if Length(Result[I]) > 0 then begin
      Inc(I);
    end;
  end else begin
    SetLength(Result, 2);

    Result[I] := GetInterval(AStartOfTheDay, VTimes.SetTime);
    if Length(Result[I]) > 0 then begin
      Inc(I)
    end;

    Result[I] := GetInterval(VTimes.RiseTime, AEndOfTheDay);
    if Length(Result[I]) > 0 then begin
      Inc(I)
    end;
  end;

  SetLength(Result, I);
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

procedure TSunCalcShapesGenerator.GetMinMaxAltitudePoints(
  out AMinAltitudePoints: TArrayOfFixedPoint;
  out AMaxAltitudePoints: TArrayOfFixedPoint;
  out AMinMaxAltitudePolygon: TArrayOfFixedPoint
);
var
  I, J: Integer;
  VDay: TDateTime;
  VPoints: TArrayOfArrayOfFixedPoint;
  VLen1, VLen2, VLen3, VLen4: Integer;
  VSeg1, VSeg2: TArrayOfFixedPoint;
begin
  if not IsValidSate then begin
    SetLength(AMinAltitudePoints, 0);
    SetLength(AMaxAltitudePoints, 0);
    SetLength(AMinMaxAltitudePolygon, 0);
    Exit;
  end;

  Assert(IsIntersectScreenRect, 'SunCalc: Shape is out of screen rect!');

  if not FIsAltitudePointsValid then begin
    // min altitude curve
    VDay := FDataProvider.GetMinAltitudeDay(FStartOfTheDay, FLocation);
    VPoints := GenerateCurvePoints(StartOfTheDay(VDay), EndOfTheDay(VDay));
    if Length(VPoints) > 0 then begin
      FMinAltitudeDayPoints := VPoints[0];
    end else begin
      FMinAltitudeDayPoints := nil;
    end;

    // max altitude curve
    VDay := FDataProvider.GetMaxAltitudeDay(FStartOfTheDay, FLocation);
    VPoints := GenerateCurvePoints(StartOfTheDay(VDay), EndOfTheDay(VDay));
    if Length(VPoints) > 0 then begin
      FMaxAltitudeDayPoints := VPoints[0];
    end else begin
      FMaxAltitudeDayPoints := nil;
    end;

    // Polygon between min/max curves
    VLen1 := Length(FMaxAltitudeDayPoints);
    VLen2 := Length(FMinAltitudeDayPoints);
    if (VLen1 > 0) and (VLen2 > 0) then begin
      VSeg1 :=
        GenerateCircleSegment(
          FCenter,
          FRadius,
          FMaxAltitudeDayPoints[0],
          FMinAltitudeDayPoints[0]
        );
      VLen3 := Length(VSeg1);

      VSeg2 :=
        GenerateCircleSegment(
          FCenter,
          FRadius,
          FMinAltitudeDayPoints[VLen2 - 1],
          FMaxAltitudeDayPoints[VLen1 - 1]
        );
      VLen4 := Length(VSeg2);

      SetLength(FMinMaxAltitudePoly, VLen1 + VLen2 + VLen3 + VLen4);

      Move(FMaxAltitudeDayPoints[0], FMinMaxAltitudePoly[0], VLen1 * SizeOf(TFixedPoint));

      J := VLen1;

      for I := VLen4 - 1 downto 0 do begin
        FMinMaxAltitudePoly[J] := VSeg2[I];
        Inc(J);
      end;

      for I := VLen2 - 1 downto 0 do begin
        FMinMaxAltitudePoly[J] := FMinAltitudeDayPoints[I];
        Inc(J);
      end;

      for I := VLen3 - 1 downto 0 do begin
        FMinMaxAltitudePoly[J] := VSeg1[I];
        Inc(J);
      end;
    end else begin
      SetLength(FMinMaxAltitudePoly, 0);
    end;

    FIsAltitudePointsValid := True;
  end;

  AMinAltitudePoints := FMinAltitudeDayPoints;
  AMaxAltitudePoints := FMaxAltitudeDayPoints;
  AMinMaxAltitudePolygon := FMinMaxAltitudePoly;
end;

procedure TSunCalcShapesGenerator.GetDayInfoPoints(
  out ADayPoints: TArrayOfArrayOfFixedPoint;
  out ARise, ASet, ACenter: TFixedPoint
);
var
  VTimes: TSunCalcProviderTimes;
  VAltitude: Double;
begin
  ACenter := FixedPoint(FCenter);

  if not IsValidSate then begin
    SetLength(ADayPoints, 0);
    ARise := FixedPoint(0, 0);
    ASet := FixedPoint(0, 0);
    Exit;
  end;

  Assert(IsIntersectScreenRect, 'SunCalc: Shape is out of screen rect!');

  if not FIsDayInfoPointsValid then begin
    VTimes := FDataProvider.GetTimes(FStartOfTheDay, FEndOfTheDay, FLocation);
    FDayPoints := GenerateCurvePoints(FStartOfTheDay, FEndOfTheDay);
    FRisePoint := GetPointPosition(VTimes.RiseTime, VAltitude);
    FSetPoint := GetPointPosition(VTimes.SetTime, VAltitude);
    FIsDayInfoPointsValid := True;
  end;

  ADayPoints := FDayPoints;
  ARise := FRisePoint;
  ASet := FSetPoint;
end;

procedure TSunCalcShapesGenerator.GetTimeInfoPoints(
  out APos, ACenter: TFixedPoint
);
var
  VAltitude: Double;
begin
  if not IsValidSate then begin
    APos := FixedPoint(0, 0);
    ACenter := FixedPoint(0, 0);
    Exit;
  end;

  Assert(IsIntersectScreenRect, 'SunCalc: Shape is out of screen rect!');

  if not FIsCurrentPosValid then begin
    FCurrentPos := GetPointPosition(FDateTime, VAltitude);

    if VAltitude < FMinAltitude then begin
      UpdateMinAltitude(FStartOfTheDay, FEndOfTheDay);
      if VAltitude < FMinAltitude then begin
        FCurrentPos := FixedPoint(0, 0);
      end;
    end;

    FIsCurrentPosValid := True;
  end;

  APos := FCurrentPos;
  ACenter := FixedPoint(FCenter);
end;

procedure TSunCalcShapesGenerator.UpdateMinAltitude(
  const AStartOfTheDay: TDateTime;
  const AEndOfTheDay: TDateTime
);
var
  VAlt: Double;
  VPoint: TFixedPoint;
  VTimes: TSunCalcProviderTimes;
begin
  FMinAltitude := 0;

  VTimes := FDataProvider.GetTimes(AStartOfTheDay, AEndOfTheDay, FLocation);

  VPoint := GetPointPosition(VTimes.RiseTime, VAlt);
  if VPoint.X > 0 then begin
    FMinAltitude := Min(VAlt, FMinAltitude);
  end;

  VPoint := GetPointPosition(VTimes.SetTime, VAlt);
  if VPoint.X > 0 then begin
    FMinAltitude := Min(VAlt, FMinAltitude);
  end;
end;

procedure TSunCalcShapesGenerator.SetDataProvider(const AProvider: ISunCalcDataProvider);
begin
  FDataProvider := AProvider;

  FIsCirclePointsValid := False;
  FIsAltitudePointsValid := False;
  FIsDayInfoPointsValid := False;
  FIsCurrentPosValid := False;
end;

procedure TSunCalcShapesGenerator.SetDateTime(
  const AUtcDateTime: TDateTime;
  const AUtcOffset: Double
);
var
  VLocalTime: TDateTime;
begin
  if (AUtcDateTime <> FDateTime) or (AUtcOffset <> FTimeZone) then begin
    FIsCurrentPosValid := False;

    FIsDayInfoPointsValid :=
      FIsDayInfoPointsValid and
      (AUtcOffset = FTimeZone) and
      IsSameDay(AUtcDateTime, FDateTime);

    FDateTime := AUtcDateTime;
    FTimeZone := AUtcOffset;

    VLocalTime := TTimeZoneInfo.UTCToTzLocalTime(FDateTime, FTimeZone);

    FStartOfTheDay := TTimeZoneInfo.TzLocalTimeToUTC(StartOfTheDay(VLocalTime), FTimeZone);
    FEndOfTheDay := TTimeZoneInfo.TzLocalTimeToUTC(EndOfTheDay(VLocalTime), FTimeZone);
  end;
end;

procedure TSunCalcShapesGenerator.SetLocation(const ALonLat: TDoublePoint);
begin
  if not DoublePointsEqual(FLocation, ALonLat) then begin
    FLocation := ALonLat;
    FCenter := CalcCenter;

    FIsCirclePointsValid := False;
    FIsAltitudePointsValid := False;
    FIsDayInfoPointsValid := False;
    FIsCurrentPosValid := False;
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
    FIsAltitudePointsValid := False;
    FIsDayInfoPointsValid := False;
    FIsCurrentPosValid := False;
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
    (FDataProvider <> nil) and
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

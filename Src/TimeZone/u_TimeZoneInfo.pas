{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_TimeZoneInfo;

interface

uses
  Types,
  SysUtils,
  libtz,
  t_GeoTypes;

type
  TTimeZoneRec = record
    TzName: string;
    TzTime: TDateTime;
    TzOffset: TDateTime;
  end;

  TTimeZoneInfo = class(TObject)
  private
    FCtx: Pointer;

    FPrevPoint: TPoint;
    FPrevTzInfo: TTzInfo;
    FPrevUpdateTime: Cardinal;

    FIsAvailable: Boolean;
    FIsInitialized: Boolean;

    procedure LazyInit; inline;
    function CanUsePrevTzInfo(const ALonLat: TDoublePoint): Boolean; inline;
  public
    class function GetSystemTzOffset(const AUtcTime: TDateTime): TDateTime;
    class function LocalTimeToUtc(const AValue: TDateTime): TDateTime;
    class function UtcToLocalTime(const AValue: TDateTime): TDateTime; inline;
    class function UtcToTzLocalTime(const AUtcTime: TDateTime; const ATzOffset: TDateTime): TDateTime; inline;
    class function TzLocalTimeToUtc(const ALocalTime: TDateTime; const ATzOffset: TDateTime): TDateTime; inline;
    class function UtcOffsetToString(const AOffset: TDateTime): string;

    function GetTzInfo(const ALonLat: TDoublePoint; const AUtcTime: TDateTime): TTimeZoneRec;
    function GetStatusBarTzInfo(const ALonLat: TDoublePoint): string;

    property Available: Boolean read FIsAvailable;
  public
    constructor Create;
    destructor Destroy; override;
  end;

const
  cTimeZoneDllName = libtz_dll;

implementation

uses
  Windows;

const
  cTimeZoneInfoUpdateInterval = 200; // ms

{ TTimeZoneInfo }

constructor TTimeZoneInfo.Create;
begin
  inherited Create;

  FCtx := nil;

  FPrevPoint.X := MaxInt;
  FPrevPoint.Y := MaxInt;
  FPrevUpdateTime := 0;

  FIsAvailable := FileExists(ExtractFilePath(ParamStr(0)) + cTimeZoneDllName);
  FIsInitialized := False;
end;

destructor TTimeZoneInfo.Destroy;
begin
  if FCtx <> nil then begin
    tz_ctx_free(FCtx);
  end;
  inherited Destroy;
end;

procedure TTimeZoneInfo.LazyInit;
begin
  if not FIsAvailable then begin
    raise Exception.Create(cTimeZoneDllName +  ' is not available!');
  end;
  if not FIsInitialized then
  try
    FIsInitialized := True;
    FIsAvailable := LibTzInitialize;
    FCtx := tz_ctx_new();
  except
    FIsAvailable := False;
    raise;
  end;
end;

class function TTimeZoneInfo.GetSystemTzOffset(const AUtcTime: TDateTime): TDateTime;
var
  ST1, ST2: TSystemTime;
  VTzLocalTime: TDateTime;
begin
  DateTimeToSystemTime(AUtcTime, ST1);
  SystemTimeToTzSpecificLocalTime(nil, ST1, ST2);
  VTzLocalTime := SystemTimeToDateTime(ST2);
  Result := VTzLocalTime - AUtcTime;
end;

class function TTimeZoneInfo.LocalTimeToUtc(const AValue: TDateTime): TDateTime;
var
  ST1, ST2: TSystemTime;
  TZ: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TZ);

  TZ.Bias := -TZ.Bias;
  TZ.StandardBias := -TZ.StandardBias;
  TZ.DaylightBias := -TZ.DaylightBias;

  DateTimeToSystemTime(AValue, ST1);
  SystemTimeToTzSpecificLocalTime(@TZ, ST1, ST2);
  Result := SystemTimeToDateTime(ST2);
end;

class function TTimeZoneInfo.UtcToLocalTime(const AValue: TDateTime): TDateTime;
begin
  Result := UtcToTzLocalTime(AValue, GetSystemTzOffset(AValue));
end;

class function TTimeZoneInfo.TzLocalTimeToUtc(
  const ALocalTime: TDateTime;
  const ATzOffset: TDateTime
): TDateTime;
begin
  Result := ALocalTime - ATzOffset;
end;

class function TTimeZoneInfo.UtcToTzLocalTime(
  const AUtcTime: TDateTime;
  const ATzOffset: TDateTime
): TDateTime;
begin
  Result := AUtcTime + ATzOffset;
end;

class function TTimeZoneInfo.UtcOffsetToString(const AOffset: TDateTime): string;
const
  cSign: array[Boolean] of string = ('-', '+');
var
  H, M: Double;
  VHours, VMinutes: string;
begin
  if AOffset = 0 then begin
    Result := '(UTC 0)';
    Exit;
  end;

  H := Frac(Abs(AOffset)) * 24;
  VHours := IntToStr(Trunc(H));

  M := Frac(H) * 60;
  if M <> 0 then begin
    VMinutes := ':' + IntToStr(Trunc(M));
  end else begin
    VMinutes := '';
  end;

  Result := Format('(UTC%s%s%s)', [cSign[AOffset >= 0], VHours, VMinutes]);
end;

function _FloatToFixed(const AValue: Double): Integer; inline;
begin
  Result := Round(AValue * 10000);
end;

function TTimeZoneInfo.CanUsePrevTzInfo(const ALonLat: TDoublePoint): Boolean;
begin
  Result :=
    (FPrevPoint.X <> MaxInt) and
    (FPrevPoint.Y <> MaxInt) and
    (FPrevPoint.X = _FloatToFixed(ALonLat.X)) and
    (FPrevPoint.Y = _FloatToFixed(ALonLat.Y)) and
    (GetTickCount < FPrevUpdateTime + cTimeZoneInfoUpdateInterval);
end;

function TTimeZoneInfo.GetStatusBarTzInfo(const ALonLat: TDoublePoint): string;
var
  VTzInfo: PTzInfo;
  VResult: Boolean;
  VUtcTime: TDateTime;
  VTzLocalTime: TDateTime;
begin
  LazyInit;

  VTzInfo := @FPrevTzInfo;
  VUtcTime := LocalTimeToUtc(Now);

  if not CanUsePrevTzInfo(ALonLat) then begin
    VResult := tz_get_info(FCtx, ALonLat.X, ALonLat.Y, VUtcTime, VTzInfo);
    tz_check_result(FCtx, VResult);

    FPrevPoint.X := _FloatToFixed(ALonLat.X);
    FPrevPoint.Y := _FloatToFixed(ALonLat.Y);

    FPrevUpdateTime := GetTickCount;
  end;

  VTzLocalTime := UtcToTzLocalTime(VUtcTime, VTzInfo.Offset);
  Result := TimeToStr(VTzLocalTime) + ' ' + UtcOffsetToString(VTzInfo.Offset);
end;

function TTimeZoneInfo.GetTzInfo(
  const ALonLat: TDoublePoint;
  const AUtcTime: TDateTime
): TTimeZoneRec;
var
  VTzInfo: TTzInfo;
  VResult: Boolean;
begin
  LazyInit;

  VResult := tz_get_info(FCtx, ALonLat.X, ALonLat.Y, AUtcTime, @VTzInfo);
  tz_check_result(FCtx, VResult);

  Result.TzName := string(VTzInfo.Name);
  Result.TzTime := UtcToTzLocalTime(AUtcTime, VTzInfo.Offset);
  Result.TzOffset := VTzInfo.Offset;
end;

end.

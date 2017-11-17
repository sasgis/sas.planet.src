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

unit u_TimeZoneInfo;

interface

uses
  Types,
  SysUtils,
  DateUtils,
  t_GeoTypes;

type
  PLonLatToTimeZoneTime = procedure (
    const Lon: Double;
    const Lat: Double;
    const AUtcTime: Extended;
    const ALastTimeZoneIDBuf: PAnsiChar;
    const ALastTimeZoneIDBufLen: Integer;
    var ALastTimeZoneIDStrLen: Integer;
    var ALastTimeZoneIndex: Integer;
    var ALastPolygonIndex: Integer;
    out ATimeZoneTime: Extended;
    out ATimeZoneUtcOffset: Extended
  ); cdecl;

  TTimeZoneRec = record
    TzName: string;
    TzTime: Extended;
    TzOffset: Extended;
  end;

  TTimeZoneInfo = class(TObject)
  private
    FLastTimeZoneTime: Extended;
    FLastTimeZoneOffset: Extended;
    FLastPoint: TPoint;
    FLastTZID: AnsiString;
    FLastUpdateTime: Cardinal;
    FLastTimeZoneIndex: Integer;
    FLastPolygonIndex: Integer;
    FTimeZoneDll: THandle;
    FLonLatToTimeZoneTime: PLonLatToTimeZoneTime;
    FAvailable: Boolean;
    FStrBuf: PAnsiChar;
    FStrBufSize: Integer;
    procedure GetLonLatToTimeZoneID;
  public
    class function GetSystemTzOffset(const AUTCTime: TDateTime): Extended;
    class function LocalTimeToUTC(const AValue: TDateTime): TDateTime;
    class function UTCToTzLocalTime(const AUTCTime: TDateTime; const ATzOffset: Extended): TDateTime; inline;
    class function TzLocalTimeToUTC(const ALocalTime: TDateTime; const ATzOffset: Extended): TDateTime; inline;
    class function UTCOffsetToString(const AOffset: Extended): string;
    function GetTzInfo(const ALonLat: TDoublePoint; const AUTCTime: TDateTime): TTimeZoneRec;
    function GetStatusBarTzInfo(const ALonLat: TDoublePoint): string;
    property Available: Boolean read FAvailable;
  public
    constructor Create;
    destructor Destroy; override;
  end;

const
  cTimeZoneDllName = 'TimeZone.dll';

implementation

uses
  Windows;

const
  cTimeZoneInfoUpdateInterval = 200; // ms
  cTimeZoneLonLatToTimeZoneTimeFuncName = 'LonLatToTimeZoneTime';

{ TTimeZoneInfo }

constructor TTimeZoneInfo.Create;
begin
  inherited Create;
  FLastUpdateTime := 0;
  FLastTZID := '';
  FStrBufSize := 255;
  GetMem(FStrBuf, FStrBufSize);
  FAvailable := False;
  FLonLatToTimeZoneTime := nil;
  FLastTimeZoneIndex := -1;
  FLastPolygonIndex := -1;
  FLastTimeZoneTime := 0;
  FLastTimeZoneOffset := 0;
  GetLonLatToTimeZoneID;
end;

destructor TTimeZoneInfo.Destroy;
begin
  FLonLatToTimeZoneTime := nil;
  if FTimeZoneDll <> 0 then begin
    FreeLibrary(FTimeZoneDll);
    FTimeZoneDll := 0;
  end;
  FreeMem(FStrBuf);
  inherited;
end;

procedure TTimeZoneInfo.GetLonLatToTimeZoneID;
begin
  FTimeZoneDll := LoadLibrary(cTimeZoneDllName);
  if FTimeZoneDll <> 0 then begin
    FLonLatToTimeZoneTime := GetProcAddress(FTimeZoneDll, cTimeZoneLonLatToTimeZoneTimeFuncName);
    FAvailable := (Addr(FLonLatToTimeZoneTime) <> nil);
  end;
end;

class function TTimeZoneInfo.GetSystemTzOffset(const AUTCTime: TDateTime): Extended;
var
  VTmpDate: TDateTime;
  ST1, ST2: TSystemTime;
  TZ: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TZ);
  DateTimeToSystemTime(AUTCTime, ST1);
  SystemTimeToTzSpecificLocalTime(@TZ, ST1, ST2);
  VTmpDate := SystemTimeToDateTime(ST2);
  Result := MinutesBetween(VTmpDate, AUTCTime) / 60;
  if VTmpDate < AUTCTime then begin
    Result := -Result;
  end;
end;

class function TTimeZoneInfo.LocalTimeToUTC(const AValue: TDateTime): TDateTime;
var
  ST1, ST2: TSystemTime;
  TZ: TTimeZoneInformation;
begin
  // TZ - локальные (Windows) настройки
  GetTimeZoneInformation(TZ);
  // т.к. надо будет делать обратное преобразование - инвертируем bias
  TZ.Bias := -TZ.Bias;
  TZ.StandardBias := -TZ.StandardBias;
  TZ.DaylightBias := -TZ.DaylightBias;

  DateTimeToSystemTime(AValue, ST1);

  // Применение локальных настроек ко времени
  SystemTimeToTzSpecificLocalTime(@TZ, ST1, ST2);

  // Приведение WindowsSystemTime к TDateTime
  Result := SystemTimeToDateTime(ST2);
end;

class function TTimeZoneInfo.TzLocalTimeToUTC(
  const ALocalTime: TDateTime;
  const ATzOffset: Extended
): TDateTime;
begin
  Result := UTCToTzLocalTime(ALocalTime, -ATzOffset);
end;

class function TTimeZoneInfo.UTCToTzLocalTime(
  const AUTCTime: TDateTime;
  const ATzOffset: Extended
): TDateTime;
begin
  if ATzOffset = 0 then begin
    Result := AUTCTime;
  end else begin
    Result := IncHour(AUTCTime, Trunc(ATzOffset));
    Result := IncMinute(Result, Round(Frac(ATzOffset) * 60));
  end;
end;

class function TTimeZoneInfo.UTCOffsetToString(const AOffset: Extended): string;
const
  cUTCFormatStr = ' (UTC%s%s)';
  cUTCFormatStrWithPlus = ' (UTC+%s%s)';
var
  VFormatStr: string;
  VFloor, VFrac: string;
  VOffsetFrac: Extended;
begin
  if AOffset > 0 then begin
    VFormatStr := cUTCFormatStrWithPlus;
  end else begin
    VFormatStr := cUTCFormatStr;
  end;

  VFloor := IntToStr(Trunc(AOffset));
  if VFloor = '0' then begin
    VFloor := ' ' + VFloor;
  end;

  VOffsetFrac := Frac(AOffset) * 60;

  if VOffsetFrac <> 0 then begin
    if VOffsetFrac < 0 then begin
      VOffsetFrac := -VOffsetFrac;
    end;
    VFrac := Format(':%.0f', [VOffsetFrac]);
  end else begin
    VFrac := '';
  end;

  Result := Format(VFormatStr, [VFloor, VFrac]);
end;

function TTimeZoneInfo.GetStatusBarTzInfo(const ALonLat: TDoublePoint): string;
var
  VLen: Integer;
  VNeedDetectTZID: Boolean;
  VUTCTime: Extended;
begin
  if (FLastTZID = '') or
     (GetTickCount > FLastUpdateTime + cTimeZoneInfoUpdateInterval)
  then begin

    VNeedDetectTZID := not (
      (FLastPoint.X = Round(ALonLat.X * 10000)) and
      (FLastPoint.Y = Round(ALonLat.Y * 10000)) and
      (FLastTZID <> '')
    );
    if VNeedDetectTZID then begin
      VLen := 0;
    end else begin
      VLen := Length(FLastTZID);
      if VLen > FStrBufSize then begin
        ReallocMem(FStrBuf, VLen);
        FStrBufSize := VLen;
      end;
      StrLCopy(FStrBuf, PAnsiChar(FLastTZID), FStrBufSize);
    end;

    VUTCTime := LocalTimeToUTC(Now);

    try
      FLonLatToTimeZoneTime(
        ALonLat.X,
        ALonLat.Y,
        VUTCTime,
        FStrBuf,
        FStrBufSize,
        VLen,
        FLastTimeZoneIndex,
        FLastPolygonIndex,
        FLastTimeZoneTime,
        FLastTimeZoneOffset
      );
    except
      Result := Format('Error (%.1f; %.1f)', [ALonLat.X, ALonLat.Y]);
      Exit;
    end;

    FLastPoint.X := Round(ALonLat.X * 10000);
    FLastPoint.Y := Round(ALonLat.Y * 10000);
    FLastUpdateTime := GetTickCount;
    if VLen > 0 then begin
      SetLength(FLastTZID, VLen);
      StrLCopy(PAnsiChar(FLastTZID), FStrBuf, VLen);
    end else begin
      FLastTZID := '';
    end;
  end;

  Result := TimeToStr(FLastTimeZoneTime) + UTCOffsetToString(FLastTimeZoneOffset);
end;

function TTimeZoneInfo.GetTzInfo(
  const ALonLat: TDoublePoint;
  const AUTCTime: TDateTime
): TTimeZoneRec;
var
  VLen: Integer;
begin
  if not FAvailable then begin
    raise Exception.Create('TimeZone library not available!');
  end;

  VLen := 0;

  FLonLatToTimeZoneTime(
    ALonLat.X,
    ALonLat.Y,
    AUTCTime,
    FStrBuf,
    FStrBufSize,
    VLen,
    FLastTimeZoneIndex,
    FLastPolygonIndex,
    FLastTimeZoneTime,
    FLastTimeZoneOffset
  );

  if VLen > 0 then begin
    SetLength(FLastTZID, VLen);
    StrLCopy(PAnsiChar(FLastTZID), FStrBuf, VLen);
  end else begin
    FLastTZID := '';
  end;

  Result.TzName := FLastTZID;
  Result.TzTime := FLastTimeZoneTime;
  Result.TzOffset := FLastTimeZoneOffset;
end;

end.

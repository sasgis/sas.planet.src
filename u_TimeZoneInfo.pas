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

unit u_TimeZoneInfo;

interface

uses
  Types,
  SysUtils,
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
    function LocalTimeToUTC(AValue: TDateTime): TDateTime;
    function UTCOffsetToString(const AOffset: Extended): string;
    procedure GetLonLatToTimeZoneID;
    function GetStatusBarTzInfoNew(const ALonLat: TDoublePoint): string; inline;
  public
    constructor Create;
    destructor Destroy; override;
    function GetStatusBarTzInfo(const ALonLat: TDoublePoint): string;
    property Available: Boolean read FAvailable;
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
  inherited Destroy;
end;

procedure TTimeZoneInfo.GetLonLatToTimeZoneID;
begin
  FTimeZoneDll := LoadLibrary(cTimeZoneDllName);
  if FTimeZoneDll <> 0 then begin
    FLonLatToTimeZoneTime := GetProcAddress(FTimeZoneDll, cTimeZoneLonLatToTimeZoneTimeFuncName);
    FAvailable := (Addr(FLonLatToTimeZoneTime) <> nil);
  end;
end;

function TTimeZoneInfo.GetStatusBarTzInfo(const ALonLat: TDoublePoint): string;
begin
  Result := GetStatusBarTzInfoNew(ALonLat);
end;

function TTimeZoneInfo.LocalTimeToUTC(AValue: TDateTime): TDateTime;
// AValue - локальное время
// Result - время UTC
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

function TTimeZoneInfo.UTCOffsetToString(const AOffset: Extended): string;
const
  cUTCFormatStr = ' (UTC%s%s)';
  cUTCFormatStrWithPlus = ' (UTC+%s%s)';
var
  VFormatStr: string;
  VFloor, VFrac: string;
  VOffsetFrac: Extended;
begin
  VOffsetFrac := Frac(AOffset) * 60;

  if AOffset > 0 then begin
    VFormatStr := cUTCFormatStrWithPlus;
  end else begin
    VFormatStr := cUTCFormatStr;
  end;

  VFloor := IntToStr(Trunc(AOffset));
  if VFloor = '0' then begin
    VFloor := ' ' + VFloor;
  end;

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

function TTimeZoneInfo.GetStatusBarTzInfoNew(const ALonLat: TDoublePoint): string;
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

end.

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
  t_GeoTypes,
  i_TimeZoneDiffByLonLat;

type
  PLonLatToTimeZoneID = function (
    const Lon: Double;
    const Lat: Double;
    const ABuffer: PAnsiChar;
    const ABufferSize: Integer;
    var ALastTimeZoneIndex: Integer;
    var ALastPolygonIndex: Integer
  ): Integer; cdecl;

  TTimeZoneInfo = class(TObject)
  private
    FLastPoint: TPoint;
    FLastTZID: AnsiString;
    FLastUpdateTime: Cardinal;
    FLastTimeZoneIndex: Integer;
    FLastPolygonIndex: Integer;
    FTimeZoneDll: THandle;
    FLonLatToTimeZoneID: PLonLatToTimeZoneID;
    FTimeZoneDiff: ITimeZoneDiffByLonLat;
    procedure GetLonLatToTimeZoneID;
    function GetStatusBarTzInfoNew(const ALonLat: TDoublePoint): string; inline;
    function GetStatusBarTzInfoOld(const ALonLat: TDoublePoint): string; inline;
  public
    constructor Create(
      const AOldTzInterface: ITimeZoneDiffByLonLat;
      const ATryUseNewMethod: Boolean = True
    );
    destructor Destroy; override;
    function GetStatusBarTzInfo(const ALonLat: TDoublePoint): string;
  end;

implementation

uses
  Windows,
  Math,
  TZDB;

const
  cTimeZoneInfoUpdateInterval = 500; // ms
  cTimeZoneDllName = 'TimeZone.dll';
  cTimeZoneLonLatToTimeZoneIDFuncName = 'LonLatToTimeZoneID';

{ TTimeZoneInfo }

constructor TTimeZoneInfo.Create(
  const AOldTzInterface: ITimeZoneDiffByLonLat;
  const ATryUseNewMethod: Boolean = True
);
begin
  inherited Create;
  FTimeZoneDiff := AOldTzInterface;
  FLastUpdateTime := 0;
  FLastTZID := '';
  FLonLatToTimeZoneID := nil;
  FLastTimeZoneIndex := -1;
  FLastPolygonIndex := -1;
  if ATryUseNewMethod then begin
    GetLonLatToTimeZoneID;
  end;
end;

destructor TTimeZoneInfo.Destroy;
begin
  FLonLatToTimeZoneID := nil;
  if FTimeZoneDll <> 0 then begin
    FreeLibrary(FTimeZoneDll);
    FTimeZoneDll := 0;
  end;
  inherited Destroy;
end;

procedure TTimeZoneInfo.GetLonLatToTimeZoneID;
begin
  FTimeZoneDll := LoadLibrary(cTimeZoneDllName);
  if FTimeZoneDll <> 0 then begin
    FLonLatToTimeZoneID := GetProcAddress(FTimeZoneDll, cTimeZoneLonLatToTimeZoneIDFuncName);
  end;
end;

function TTimeZoneInfo.GetStatusBarTzInfo(const ALonLat: TDoublePoint): string;
begin
  Result := '';
  if Addr(FLonLatToTimeZoneID) <> nil then begin
    Result := GetStatusBarTzInfoNew(ALonLat);
  end;
  if Result = '' then begin
    Result := GetStatusBarTzInfoOld(ALonLat);
  end;
end;

function TTimeZoneInfo.GetStatusBarTzInfoNew(const ALonLat: TDoublePoint): string;
const
  cUTCFormatStr = ' (UTC %.0f)';
  cUTCFormatStrWithPlus = ' (UTC +%.0f)';
var
  VLen: Integer;
  VNeedDetectTZID: Boolean;
  VFormatStr: string;
  VTZID: AnsiString;
  VTZDB: TBundledTimeZone;
  VUTCTime: Extended;
  VSysTime: TSystemTime;
begin
  Result := '';
  if (FLastTZID = '') or
     (GetTickCount > FLastUpdateTime + cTimeZoneInfoUpdateInterval)
  then begin
    VNeedDetectTZID := not (
      (FLastPoint.X = Round(ALonLat.X * 100)) and
      (FLastPoint.Y = Round(ALonLat.Y * 100)) and
      (FLastTZID <> '')
    );
    if VNeedDetectTZID then begin 
      SetLength(VTZID, 255);
      VLen := FLonLatToTimeZoneID(
        ALonLat.X,
        ALonLat.Y,
        PAnsiChar(VTZID),
        Length(VTZID),
        FLastTimeZoneIndex,
        FLastPolygonIndex
      );
      if VLen > 0 then begin // ok, we found it
        SetLength(VTZID, VLen);
      end else if VLen = 0 then begin // not found, but maybe we on border between
        VLen := FLonLatToTimeZoneID(  // two time zones, so try decres accuracy
          RoundTo(ALonLat.X, -1),
          RoundTo(ALonLat.Y, -1),
          PAnsiChar(VTZID),
          Length(VTZID),
          FLastTimeZoneIndex,
          FLastPolygonIndex
        );
        if VLen >= 0 then begin
          SetLength(VTZID, VLen);
        end;
      end else if VLen < 0 then begin
        raise Exception.Create(
          'LonLatToTimeZoneID: It''s strange, but buffer is too small!'
        );
      end;  
      FLastTZID := VTZID;
      FLastPoint.X := Round(ALonLat.X * 100);
      FLastPoint.Y := Round(ALonLat.Y * 100);
      FLastUpdateTime := GetTickCount;
    end;
  end;  
  if (FLastTZID <> '') and (FLastTZID <> 'uninhabited') then begin
    VTZDB := TBundledTimeZone.GetTimeZone(FLastTZID);
    if VTZDB.UtcOffset > 0 then begin
      VFormatStr := cUTCFormatStrWithPlus;
    end else begin
      VFormatStr := cUTCFormatStr;
    end;
    GetSystemTime(VSysTime);
    VUTCTime :=
      EncodeDate(VSysTime.wYear, VSysTime.wMonth, VSysTime.wDay) +
      EncodeTime(VSysTime.wHour, VSysTime.wMinute, VSysTime.wSecond, VSysTime.wMilliseconds);
    Result := TimeToStr(VTZDB.ToLocalTime(VUTCTime)) + Format(VFormatStr, [VTZDB.UtcOffset/(60*60)]);
  end;
end;

function TTimeZoneInfo.GetStatusBarTzInfoOld(const ALonLat: TDoublePoint): string;
var
  tz: TDateTime;
  st: TSystemTime;
  VTime: Extended;
begin
  tz := FTimeZoneDiff.GetTimeDiff(ALonLat);
  GetSystemTime(st);
  VTime := EncodeTime(st.wHour, st.wMinute, st.wSecond, st.wMilliseconds);
  VTime := VTime + tz;
  VTime := Frac(VTime);
  if VTime < 0 then begin
    VTime := 1 + VTime;
  end;
  Result := TimeToStr(VTime);
end;

end.

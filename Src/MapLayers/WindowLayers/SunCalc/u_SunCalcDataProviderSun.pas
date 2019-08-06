{******************************************************************************}
{* SAS.Planet (SAS.œÎ‡ÌÂÚ‡)                                                   *}
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

unit u_SunCalcDataProviderSun;

interface

uses
  SunCalc,
  t_GeoTypes,
  t_SunCalcDataProvider,
  i_MarkerDrawable,
  i_SunCalcDataProvider,
  u_BaseInterfacedObject;

type
  TSunCalcDataProviderSun = class(TBaseInterfacedObject, ISunCalcDataProvider)
  private
    FDayMarker: IMarkerDrawable;
    FYearMarker: IMarkerDrawable;

    FYearEvents: TSunCalcYearEvents;

    FTimes: TSunCalcTimes;
    FTimesDate: TDateTime;
    FTimesLocation: TDoublePoint;
    FIsTimesPrepared: Boolean;

    FDayEvents: TSunCalcDayEvents;
    FDayEventsIsFullDetails: Boolean;
    FIsDayEventsPrepared: Boolean;

    FDayEventNames: array [TSunCalcTimesID] of string;

    procedure PrepareDayEventsNames;
    procedure YearEventsValidate(const AUtcDate: TDateTime);
  private
    { ISunCalcDataProvider }
    function GetTimes(
      const AStartOfTheDay: TDateTime;
      const AEndOfTheDay: TDateTime;
      const ALonLat: TDoublePoint
    ): TSunCalcProviderTimes;

    function GetPosition(
      const AUtcDate: TDateTime;
      const ALonLat: TDoublePoint
    ): TSunCalcProviderPosition;

    function GetMaxAltitudeDay(
      const AUtcDate: TDateTime;
      const ALonLat: TDoublePoint
    ): TDateTime;

    function GetMinAltitudeDay(
      const AUtcDate: TDateTime;
      const ALonLat: TDoublePoint
    ): TDateTime;

    function GetDayEvents(
      const AParams: TSunCalcParams
    ): TSunCalcDayEvents;

    function GetDayInfo(
      const AStartOfTheDay: TDateTime;
      const AEndOfTheDay: TDateTime;
      const ACurrentTime: TDateTime;
      const ALonLat: TDoublePoint
    ): TSunCalcDayInfo;

    function GetYearEvents(
      const AUtcDate: TDateTime;
      const ALonLat: TDoublePoint
    ): TSunCalcYearEvents;

    function GetDayMarker: IMarkerDrawable;
    function GetYearMarker: IMarkerDrawable;
  public
    constructor Create;
  end;

implementation

uses
  Math,
  SysUtils,
  DateUtils,
  GR32,
  gnugettext,
  Solstice,
  u_GeoFunc,
  u_MarkerSimpleConfigStatic,
  u_MarkerDrawableSimpleCircle,
  u_MarkerDrawableSimpleSquare;

resourcestring
  rsSolstice = 'Solstice at';
  rsEquinox = 'Equinox at';

  rsAstroStart = 'Astro. twilight start';
  rsNauticalStart = 'Nautical twilight start';
  rsCivilStart = 'Civil twilight start';
  rsRise = 'Rise';
  rsGoldenHrEnd = 'Golden hour end';
  rsNoon = 'Noon';
  rsGoldenHrStart = 'Golden hour start';
  rsSet = 'Set';
  rsCivilEnd = 'Civil twilight end';
  rsNauticalEnd = 'Nautical twilight end';
  rsAstroEnd = 'Astro. twilight end';
  rsMidnight = 'Midnight';

{ TSunCalcDataProviderSun }

constructor TSunCalcDataProviderSun.Create;
begin
  inherited Create;

  SetLength(FYearEvents, 0);

  FDayMarker :=
    TMarkerDrawableSimpleCircle.Create(
      TMarkerSimpleConfigStatic.Create(14, clYellow32, clRed32)
    );

  FYearMarker :=
    TMarkerDrawableSimpleSquare.Create(
      TMarkerSimpleConfigStatic.Create(8, clYellow32, clRed32)
    );

  PrepareDayEventsNames;

  FIsTimesPrepared := False;
  FIsDayEventsPrepared := False;
end;

function TSunCalcDataProviderSun.GetPosition(
  const AUtcDate: TDateTime;
  const ALonLat: TDoublePoint
): TSunCalcProviderPosition;
var
  VPos: TSunPos;
begin
  VPos := SunCalc.GetPosition(AUtcDate, ALonLat.Y, ALonLat.X);

  Result.Azimuth := VPos.Azimuth;
  Result.Altitude := VPos.Altitude;
end;

function TSunCalcDataProviderSun.GetTimes(
  const AStartOfTheDay: TDateTime;
  const AEndOfTheDay: TDateTime;
  const ALonLat: TDoublePoint
): TSunCalcProviderTimes;
var
  VIsTimesValid: Boolean;
begin
  VIsTimesValid :=
    FIsTimesPrepared and
    SameDate(AStartOfTheDay, FTimesDate) and
    DoublePointsEqual(ALonLat, FTimesLocation);

  if not VIsTimesValid then begin
    FTimes := SunCalc.GetTimes(AStartOfTheDay, ALonLat.Y, ALonLat.X);
    FTimesDate := AStartOfTheDay;
    FTimesLocation := ALonLat;
    FIsTimesPrepared := True;
  end;

  Result.RiseTime := FTimes[sunrise].Value;
  Result.SetTime := FTimes[sunset].Value;
end;

procedure TSunCalcDataProviderSun.YearEventsValidate(const AUtcDate: TDateTime);
type
  TDateFunc = function(const AYear: Word): TDateTime;
const
  cDateFunc: array [0..3] of TDateFunc = (
    Solstice.March,
    Solstice.June,
    Solstice.September,
    Solstice.December
  );
var
  I: Integer;
  VYear: Word;
  VNames: array [0..1] of string;
begin
  VYear := YearOf(AUtcDate);
  if (Length(FYearEvents) = 0) or (VYear <> YearOf(FYearEvents[0].Date)) then begin
    SetLength(FYearEvents, 4);
    VNames[0] := rsEquinox;
    VNames[1] := rsSolstice;
    for I := 0 to 3 do begin
      FYearEvents[I].Date := cDateFunc[I](VYear);
      FYearEvents[I].Name := VNames[I mod 2];
    end;
  end;
end;

function TSunCalcDataProviderSun.GetMaxAltitudeDay(
  const AUtcDate: TDateTime;
  const ALonLat: TDoublePoint
): TDateTime;
begin
  YearEventsValidate(AUtcDate);
  if ALonLat.Y > 0 then begin
    Result := FYearEvents[1].Date; // June
  end else begin
    Result := FYearEvents[3].Date; // December
  end;
end;

function TSunCalcDataProviderSun.GetMinAltitudeDay(
  const AUtcDate: TDateTime;
  const ALonLat: TDoublePoint
): TDateTime;
begin
  YearEventsValidate(AUtcDate);
  if ALonLat.Y > 0 then begin
    Result := FYearEvents[3].Date; // December
  end else begin
    Result := FYearEvents[1].Date; // June
  end;
end;

function TSunCalcDataProviderSun.GetYearEvents(
  const AUtcDate: TDateTime;
  const ALonLat: TDoublePoint
): TSunCalcYearEvents;
begin
  YearEventsValidate(AUtcDate);
  Result := FYearEvents;
end;

type
  TDayEventItem = record
    Id  : TSunCalcTimesID;
    Idx : Integer;
  end;
  TDayEventItems = array of TDayEventItem;

function GetDayEventItems: TDayEventItems;
const
  cItems: array [0..11] of TDayEventItem = (
    (Id: nightEnd;      Idx: 0),
    (Id: nauticalDawn;  Idx: 1),
    (Id: dawn;          Idx: 2),
    (Id: sunrise;       Idx: 3),
    (Id: goldenHourEnd; Idx: 4),
    (Id: solarNoon;     Idx: 5),
    (Id: goldenHour;    Idx: 5),
    (Id: sunset;        Idx: 4),
    (Id: dusk;          Idx: 3),
    (Id: nauticalDusk;  Idx: 2),
    (Id: night;         Idx: 1),
    (Id: nadir;         Idx: 0)
  );
var
  I: Integer;
begin
  SetLength(Result, Length(cItems));
  for I := 0 to Length(cItems) - 1 do begin
    Result[I] := cItems[I];
  end;
end;

function GetDayEventItemsShort: TDayEventItems;
const
  cItems: array [0..4] of TDayEventItem = (
    (Id: dawn;      Idx: 0),
    (Id: sunrise;   Idx: 3),
    (Id: solarNoon; Idx: 5),
    (Id: sunset;    Idx: 5),
    (Id: dusk;      Idx: 3)
  );
var
  I: Integer;
begin
  SetLength(Result, Length(cItems));
  for I := 0 to Length(cItems) - 1 do begin
    Result[I] := cItems[I];
  end;
end;

function TSunCalcDataProviderSun.GetDayEvents(
  const AParams: TSunCalcParams
): TSunCalcDayEvents;
var
  I: Integer;
  VTimeID: TSunCalcTimesID;
  VItems: TDayEventItems;
  VCount: Integer;
  VIsTimesValid: Boolean;
  VIsDayEventsValid: Boolean;
begin
  VIsTimesValid :=
    FIsTimesPrepared and
    SameDate(AParams.StartOfTheDay, FTimesDate) and
    DoublePointsEqual(AParams.LonLat, FTimesLocation);

  VIsDayEventsValid :=
    VIsTimesValid and
    FIsDayEventsPrepared and
    (AParams.IsFullDetails = FDayEventsIsFullDetails);

  if VIsDayEventsValid then begin
    Result := FDayEvents;
    Exit;
  end;

  if AParams.IsFullDetails then begin
    VItems := GetDayEventItems;
  end else begin
    VItems := GetDayEventItemsShort;
  end;

  if not VIsTimesValid then begin
    FTimes := SunCalc.GetTimes(AParams.StartOfTheDay, AParams.LonLat.Y, AParams.LonLat.X);
    FTimesDate := AParams.StartOfTheDay;
    FTimesLocation := AParams.LonLat;
    FIsTimesPrepared := True;
  end;

  VCount := Length(VItems);
  SetLength(FDayEvents, VCount);

  for I := 0 to VCount - 1 do begin
    VTimeID := VItems[I].Id;
    FDayEvents[I].Date := FTimes[VTimeID].Value;
    FDayEvents[I].Name := FDayEventNames[VTimeID];
    FDayEvents[I].IsCulmination := (VTimeID = solarNoon);

    if VTimeID in [solarNoon, nadir] then begin
      FDayEvents[I].ColorIndex := -1;
      FDayEvents[I].NextColorIndex := -1;
    end else begin
      FDayEvents[I].ColorIndex := VItems[I].Idx;
      FDayEvents[I].NextColorIndex := VItems[(I + 1) mod VCount].Idx;
    end;
  end;

  Result := FDayEvents;
  FDayEventsIsFullDetails := AParams.IsFullDetails;
  FIsDayEventsPrepared := True;
end;

function TSunCalcDataProviderSun.GetDayInfo(
  const AStartOfTheDay: TDateTime;
  const AEndOfTheDay: TDateTime;
  const ACurrentTime: TDateTime;
  const ALonLat: TDoublePoint
): TSunCalcDayInfo;

  function MinutesToStr(const AMinutes: Int64): string;
  var
    VHour, VMin: Int64;
  begin
    if AMinutes > 0 then begin
      VHour := AMinutes div 60;
      VMin := AMinutes - VHour * 60;
      Result := Format('%.2d:%.2d', [VHour, VMin]);
    end else begin
      Result := '';
    end;
  end;

  function ShadowToStr(const AAltitude: Double): string;
  var
    VMeters: Double;
  begin
    if AAltitude > 0 then begin
      VMeters := 1/Tan(AAltitude);
      if VMeters < 10 then begin
        Result := Format('%.1f', [VMeters]);
      end else
      if VMeters < 500 then begin
        Result := Format('%.0f', [VMeters]);
      end else begin
        Result := '';
      end;
    end else begin
      Result := '';
    end;
  end;

  procedure AddRow(var ARow: Integer; const AStr1, AStr2: string);
  begin
    with Result[ARow] do begin
      Text[0] := AStr1;
      Text[1] := AStr2;
    end;
    Inc(ARow);
  end;

var
  VRow: Integer;
  VPos: TSunPos;
  VUptime: Integer;
  VIsUnderHorizont: Boolean;
  VTimes: TSunCalcProviderTimes;
begin
  // day length
  // azimuth
  // altitude
  // shadow

  VRow := 0;
  SetLength(Result, 4);

  VTimes := GetTimes(AStartOfTheDay, AEndOfTheDay, ALonLat);
  VPos := SunCalc.GetPosition(ACurrentTime, ALonLat.Y, ALonLat.X);

  VUptime := 0;
  VIsUnderHorizont := RadToDeg(VPos.Altitude) < 0;

  if (VTimes.RiseTime = 0) and (VTimes.SetTime = 0) then begin
    if not VIsUnderHorizont then begin
      VUptime := 24 * 60;
    end;
  end else
  if (VTimes.RiseTime > 0) and (VTimes.SetTime > 0) then begin
    VUptime := MinutesBetween(VTimes.RiseTime, VTimes.SetTime);
  end else
  if VTimes.RiseTime = 0 then begin
    VUptime := MinutesBetween(AStartOfTheDay, VTimes.SetTime);
  end else
  if VTimes.SetTime = 0 then begin
    VUptime := MinutesBetween(VTimes.RiseTime, AEndOfTheDay);
  end;

  AddRow(VRow, _('Day Length'), MinutesToStr(VUptime) );
  AddRow(VRow, _('Azimuth'), Format('%.0f∞', [RadToDeg(VPos.Azimuth + Pi)]) );
  AddRow(VRow, _('Altitude'), Format('%.1f∞', [RadToDeg(VPos.Altitude)]) );
  AddRow(VRow, _('Shadow, meters'), ShadowToStr(VPos.Altitude) );

  SetLength(Result, VRow);
end;

function TSunCalcDataProviderSun.GetDayMarker: IMarkerDrawable;
begin
  Result := FDayMarker;
end;

function TSunCalcDataProviderSun.GetYearMarker: IMarkerDrawable;
begin
  Result := FYearMarker;
end;

procedure TSunCalcDataProviderSun.PrepareDayEventsNames;
begin
  FDayEventNames[solarNoon]     := rsNoon;
  FDayEventNames[nadir]         := rsMidnight;
  FDayEventNames[sunrise]       := rsRise;
  FDayEventNames[sunset]        := rsSet;
  FDayEventNames[sunriseEnd]    := rsRise;
  FDayEventNames[sunsetStart]   := rsSet;
  FDayEventNames[Dawn]          := rsCivilStart;
  FDayEventNames[Dusk]          := rsCivilEnd;
  FDayEventNames[nauticalDawn]  := rsNauticalStart;
  FDayEventNames[nauticalDusk]  := rsNauticalEnd;
  FDayEventNames[nightEnd]      := rsAstroStart;
  FDayEventNames[night]         := rsAstroEnd;
  FDayEventNames[goldenHourEnd] := rsGoldenHrEnd;
  FDayEventNames[goldenHour]    := rsGoldenHrStart;
end;

end.

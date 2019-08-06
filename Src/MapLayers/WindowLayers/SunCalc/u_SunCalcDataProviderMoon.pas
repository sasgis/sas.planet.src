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

unit u_SunCalcDataProviderMoon;

interface

uses
  SunCalc,
  t_GeoTypes,
  t_SunCalcDataProvider,
  i_MarkerDrawable,
  i_SunCalcDataProvider,
  u_BaseInterfacedObject;

type
  TSunCalcDataProviderMoon = class(TBaseInterfacedObject, ISunCalcDataProvider)
  private
    FDayMarker: IMarkerDrawable;
    FYearMarker: IMarkerDrawable;

    FTimes: array[0..1] of TMoonTimes;
    FTimesParams: TSunCalcTimesParams;
    FIsTimesPrepared: Boolean;

    FDayEvents: TSunCalcDayEvents;
    FDayEventsParams: TSunCalcParams;
    FIsDayEventsPrepared: Boolean;
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
  u_GeoFunc,
  u_MarkerSimpleConfigStatic,
  u_MarkerDrawableSimpleCircle,
  u_MarkerDrawableSimpleSquare;

resourcestring
  rsRise = 'Rise';
  rsSet = 'Set';

{ TSunCalcDataProviderMoon }

constructor TSunCalcDataProviderMoon.Create;
begin
  inherited Create;

  FDayMarker :=
    TMarkerDrawableSimpleCircle.Create(
      TMarkerSimpleConfigStatic.Create(14, clSilver32, clWhite32)
    );

  FYearMarker :=
    TMarkerDrawableSimpleSquare.Create(
      TMarkerSimpleConfigStatic.Create(8, clSilver32, clWhite32)
    );

  FIsTimesPrepared := False;
  FIsDayEventsPrepared := False;
end;

function TSunCalcDataProviderMoon.GetPosition(
  const AUtcDate: TDateTime;
  const ALonLat: TDoublePoint
): TSunCalcProviderPosition;
var
  VPos: TMoonPos;
begin
  VPos := SunCalc.GetMoonPosition(AUtcDate, ALonLat.Y, ALonLat.X);

  Result.Azimuth := VPos.Azimuth;
  Result.Altitude := VPos.Altitude;
end;

function TSunCalcDataProviderMoon.GetTimes(
  const AStartOfTheDay: TDateTime;
  const AEndOfTheDay: TDateTime;
  const ALonLat: TDoublePoint
): TSunCalcProviderTimes;

  function TrySetTime(const ATime: TDateTime; var AResultTime: TDateTime): Boolean;
  begin
    Result := (ATime >= AStartOfTheDay) and (ATime <= AEndOfTheDay);
    if Result then begin
      AResultTime := ATime;
    end;
  end;

var
  VIsPrepared: Boolean;
  VIsRiseFound: Boolean;
  VIsSetFound: Boolean;
  VParams: TSunCalcTimesParams;
begin
  VParams := SunCalcTimesParams(AStartOfTheDay, AEndOfTheDay, ALonLat);
  VIsPrepared := FIsTimesPrepared and (FTimesParams = VParams);

  if not VIsPrepared then begin
    FTimes[0] := SunCalc.GetMoonTimes(AStartOfTheDay, ALonLat.Y, ALonLat.X);
  end;
  VIsRiseFound := TrySetTime(FTimes[0].MoonRise, Result.RiseTime);
  VIsSetFound := TrySetTime(FTimes[0].MoonSet, Result.SetTime);

  if not VIsRiseFound or not VIsSetFound then begin
    if not VIsPrepared then begin
      FTimes[1] := SunCalc.GetMoonTimes(AEndOfTheDay, ALonLat.Y, ALonLat.X);
    end;
    if not VIsRiseFound then begin
      VIsRiseFound := TrySetTime(FTimes[1].MoonRise, Result.RiseTime);
    end;
    if not VIsSetFound then begin
      VIsSetFound := TrySetTime(FTimes[1].MoonSet, Result.SetTime);
    end;
  end;

  if not VIsRiseFound then begin
    Result.RiseTime := 0;
  end;

  if not VIsSetFound then begin
    Result.SetTime := 0;
  end;

  if not VIsPrepared then begin
    FIsTimesPrepared := True;
    FTimesParams := VParams;
  end;
end;

function TSunCalcDataProviderMoon.GetMaxAltitudeDay(
  const AUtcDate: TDateTime;
  const ALonLat: TDoublePoint
): TDateTime;
begin
  //ToDo: Full Moon (or plus one/two days) in December has max altitude
  Result := 0;
end;

function TSunCalcDataProviderMoon.GetMinAltitudeDay(
  const AUtcDate: TDateTime;
  const ALonLat: TDoublePoint
): TDateTime;
begin
  //ToDo: New Moon (or plus one/two days) in December has min altitude
  Result := 0;
end;

function TSunCalcDataProviderMoon.GetYearEvents(
  const AUtcDate: TDateTime;
  const ALonLat: TDoublePoint
): TSunCalcYearEvents;
begin
  Result := nil;
end;

procedure SortEventsByTime(var AEvents: TSunCalcDayEvents);
var
  I, J, K: Integer;
  VItem: TSunCalcDayEvent;
begin
  // selection sort
  for I := Low(AEvents) to High(AEvents) - 1 do begin
    K := I;
    for J := I + 1 to High(AEvents) do begin
      if AEvents[J].Date < AEvents[K].Date then begin
        K := J;
      end;
    end;
    if K <> I then begin
      // swap
      VItem := AEvents[I];
      AEvents[I] := AEvents[K];
      AEvents[K] := VItem;
    end;
  end;
end;

function TSunCalcDataProviderMoon.GetDayEvents(
  const AParams: TSunCalcParams
): TSunCalcDayEvents;
var
  I: Integer;
  VIsRiseFound: Boolean;
  VIsSetFound: Boolean;
begin
  if FIsDayEventsPrepared and (FDayEventsParams = AParams) then begin
    Result := FDayEvents;
    Exit;
  end;

  VIsRiseFound := False;
  VIsSetFound := False;

  I := 0;
  SetLength(Result, 2);

  FTimes[0] :=
    SunCalc.GetMoonTimes(
      AParams.StartOfTheDay,
      AParams.LonLat.Y,
      AParams.LonLat.X
    );

  if
    (FTimes[0].MoonRise <> 0) and
    (FTimes[0].MoonRise >= AParams.StartOfTheDay) then
  begin
    Result[I] := SunCalcDayEvent(FTimes[0].MoonRise, rsRise, False, 0, 5);
    Inc(I);
    VIsRiseFound := True;
  end;

  if
    (FTimes[0].MoonSet <> 0) and
    (FTimes[0].MoonSet >= AParams.StartOfTheDay) then
  begin
    Result[I] := SunCalcDayEvent(FTimes[0].MoonSet, rsSet, False, 5, 0);
    Inc(I);
    VIsSetFound := True;
  end;

  if not VIsRiseFound or not VIsSetFound then begin

    FTimes[1] :=
      SunCalc.GetMoonTimes(
        AParams.EndOfTheDay,
        AParams.LonLat.Y,
        AParams.LonLat.X
      );

    if
      not VIsRiseFound and
      (FTimes[1].MoonRise <> 0) and
      (FTimes[1].MoonRise <= AParams.EndOfTheDay) then
    begin
      Result[I] := SunCalcDayEvent(FTimes[1].MoonRise, rsRise, False, 0, 5);
      Inc(I);
    end;

    if
      not VIsSetFound and
      (FTimes[1].MoonSet <> 0) and
      (FTimes[1].MoonSet <= AParams.EndOfTheDay) then
    begin
      Result[I] := SunCalcDayEvent(FTimes[1].MoonSet, rsSet, False, 5, 0);
      Inc(I);
    end;
  end;

  SetLength(Result, I);

  SortEventsByTime(Result);

  FDayEvents := Result;
  FDayEventsParams := AParams;
  FIsDayEventsPrepared := True;
end;

function TSunCalcDataProviderMoon.GetDayInfo(
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
      Result := Format(_('%.2d:%.2d'), [VHour, VMin]);
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
        Result := Format(_('%.1f'), [VMeters]);
      end else
      if VMeters < 500 then begin
        Result := Format(_('%.0f'), [VMeters]);
      end else begin
        Result := '';
      end;
    end else begin
      Result := '';
    end;
  end;

  function PhaseToStr(const APhase: Double): string;
  var
    VDays: Double;
  begin
    VDays := APhase * 29.53;
    if VDays < 1.84566 then begin
      Result := _('New Moon')
    end else
    if VDays < 5.53699 then begin
      Result := _('Waxing Crescent');
    end else
    if VDays < 9.22831 then begin
      Result := _('First Quarter');
    end else
    if VDays < 12.91963 then begin
      Result := _('Waxing Gibbous')
    end else
    if VDays < 16.61096 then begin
      Result := _('Full Moon');
    end else
    if VDays < 20.30228 then begin
      Result := _('Waning Gibbous');
    end else
    if VDays < 23.99361 then begin
      Result := _('Last Quarter');
    end else
    if VDays < 27.68493 then begin
      Result := _('Waning Crescent');
    end else begin
      Result := _('New Moon');
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
  VPos: TMoonPos;
  VUptime: Integer;
  VIsUnderHorizont: Boolean;
  VTimes: TSunCalcProviderTimes;
  VIllumination: TMoonIllumination;
begin
  // uptime
  // azimuth
  // altitude
  // shadow
  // illumination
  // phase

  VRow := 0;
  SetLength(Result, 6);

  VTimes := GetTimes(AStartOfTheDay, AEndOfTheDay, ALonLat);
  VPos := SunCalc.GetMoonPosition(ACurrentTime, ALonLat.Y, ALonLat.X);
  VIllumination := SunCalc.GetMoonIllumination(ACurrentTime);

  VUptime := 0;
  VIsUnderHorizont := RadToDeg(VPos.Altitude) < 0;

  if (VTimes.RiseTime = 0) and (VTimes.SetTime = 0) then begin
    if not VIsUnderHorizont then begin
      VUptime := 24 * 60;
    end;
  end else
  if (VTimes.RiseTime > 0) and (VTimes.SetTime > 0) then begin
    if VTimes.RiseTime < VTimes.SetTime then begin
      VUptime := MinutesBetween(VTimes.RiseTime, VTimes.SetTime);
    end else begin
      VUptime :=
        MinutesBetween(AStartOfTheDay, VTimes.SetTime) +
        MinutesBetween(VTimes.RiseTime, AEndOfTheDay);
    end;
  end else
  if VTimes.RiseTime = 0 then begin
    VUptime := MinutesBetween(AStartOfTheDay, VTimes.SetTime);
  end else
  if VTimes.SetTime = 0 then begin
    VUptime := MinutesBetween(VTimes.RiseTime, AEndOfTheDay);
  end;

  AddRow(VRow, _('Up Time'), MinutesToStr(VUptime) );
  AddRow(VRow, _('Azimuth'), Format('%.0f∞', [RadToDeg(VPos.Azimuth)]) );
  AddRow(VRow, _('Altitude'), Format('%.1f∞', [RadToDeg(VPos.Altitude)]) );
  AddRow(VRow, _('Shadow, meters'), ShadowToStr(VPos.Altitude) );
  AddRow(VRow, _('Illumination'), Format('%.0f%%', [VIllumination.Fraction * 100]) );
  AddRow(VRow, PhaseToStr(VIllumination.Phase), '');

  SetLength(Result, VRow);
end;

function TSunCalcDataProviderMoon.GetDayMarker: IMarkerDrawable;
begin
  Result := FDayMarker;
end;

function TSunCalcDataProviderMoon.GetYearMarker: IMarkerDrawable;
begin
  Result := FYearMarker;
end;

end.

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

unit t_SunCalcDataProvider;

interface

uses
  Math,
  DateUtils,
  t_GeoTypes,
  u_GeoFunc;

type
  TSunCalcProviderTimes = record
    RiseTime: TDateTime;
    SetTime: TDateTime;
  end;

  TSunCalcProviderPosition = record
    Azimuth: Double;
    Altitude: Double;
  end;

  TSunCalcDayEvent = record
    Date: TDateTime;
    Name: string;
    IsCulmination: Boolean;
    ColorIndex: Integer;
    NextColorIndex: Integer;
  end;
  TSunCalcDayEvents = array of TSunCalcDayEvent;

  TSunCalcDayInfoRec = record
    Text: array [0..1] of string;
  end;
  TSunCalcDayInfo = array of TSunCalcDayInfoRec;

  TSunCalcYearEvent = record
    Date: TDateTime;
    Name: string;
  end;
  TSunCalcYearEvents = array of TSunCalcYearEvent;

  TSunCalcTimesParams = record
    StartOfTheDay: TDateTime;
    EndOfTheDay: TDateTime;
    LonLat: TDoublePoint;
    class operator Equal(const A, B: TSunCalcTimesParams): Boolean; inline;
  end;

  TSunCalcParams = record
    StartOfTheDay: TDateTime;
    EndOfTheDay: TDateTime;
    LonLat: TDoublePoint;
    IsFullDetails: Boolean;
    class operator Equal(const A, B: TSunCalcParams): Boolean; inline;
  end;

function SunCalcTimesParams(
  AStartOfTheDay: TDateTime;
  AEndOfTheDay: TDateTime;
  ALonLat: TDoublePoint
): TSunCalcTimesParams; inline;

function SunCalcParams(
  const AStartOfTheDay: TDateTime;
  const AEndOfTheDay: TDateTime;
  const ALonLat: TDoublePoint;
  const AIsFullDetails: Boolean
): TSunCalcParams; inline;

function SunCalcDayEvent(
  const ADate: TDateTime;
  const AName: string;
  const AIsCulmination: Boolean;
  const AColorIndex: Integer;
  const ANextColorIndex: Integer
): TSunCalcDayEvent; inline;

implementation

function SunCalcTimesParams(
  AStartOfTheDay: TDateTime;
  AEndOfTheDay: TDateTime;
  ALonLat: TDoublePoint
): TSunCalcTimesParams;
begin
  Result.StartOfTheDay := AStartOfTheDay;
  Result.EndOfTheDay := AEndOfTheDay;
  Result.LonLat := ALonLat;
end;

function SunCalcParams(
  const AStartOfTheDay: TDateTime;
  const AEndOfTheDay: TDateTime;
  const ALonLat: TDoublePoint;
  const AIsFullDetails: Boolean
): TSunCalcParams;
begin
  Result.StartOfTheDay := AStartOfTheDay;
  Result.EndOfTheDay := AEndOfTheDay;
  Result.LonLat := ALonLat;
  Result.IsFullDetails := AIsFullDetails;
end;

function SunCalcDayEvent(
  const ADate: TDateTime;
  const AName: string;
  const AIsCulmination: Boolean;
  const AColorIndex: Integer;
  const ANextColorIndex: Integer
): TSunCalcDayEvent;
begin
  Result.Date := ADate;
  Result.Name := AName;
  Result.IsCulmination := AIsCulmination;
  Result.ColorIndex := AColorIndex;
  Result.NextColorIndex := ANextColorIndex;
end;

{ TSunCalcTimesParams }

class operator TSunCalcTimesParams.Equal(
  const A, B: TSunCalcTimesParams
): Boolean;
begin
  Result :=
    SameDate(A.StartOfTheDay, B.StartOfTheDay) and
    SameDate(A.EndOfTheDay, B.EndOfTheDay) and
    DoublePointsEqual(A.LonLat, B.LonLat);
end;

{ TSunCalcParams }

class operator TSunCalcParams.Equal(
  const A, B: TSunCalcParams
): Boolean;
begin
  Result :=
    SameDate(A.StartOfTheDay, B.StartOfTheDay) and
    SameDate(A.EndOfTheDay, B.EndOfTheDay) and
    DoublePointsEqual(A.LonLat, B.LonLat) and
    (A.IsFullDetails = B.IsFullDetails);
end;

end.

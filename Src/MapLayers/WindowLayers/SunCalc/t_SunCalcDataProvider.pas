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

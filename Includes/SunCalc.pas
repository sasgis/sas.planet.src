(*
     Copyright (c) 2011-2013, Vladimir Agafonkin
     SunCalc is a JavaScript library for calculating sun position, sunlight phases, and moon position.
     https://github.com/mourner/suncalc
*)

unit SunCalc;

interface

type
  TSunPos = record
    Azimuth  : Double;
    Altitude : Double;
  end;

  TMoonPos = record
    Azimuth  : Double;
    Altitude : Double;
    Distance : Double;
  end;

  TSunCalcTimesID = (
    solarNoon,
    nadir,
    sunrise,
    sunset,
    sunriseEnd,
    sunsetStart,
    dawn,
    dusk,
    nauticalDawn,
    nauticalDusk,
    nightEnd,
    night,
    goldenHourEnd,
    goldenHour
  );

  TSunCalcTimesInfo = record
    Angle      : Double;
    Value      : TDateTime;
    IsRiseInfo : Boolean;
  end;

  TSunCalcTimes = array [solarNoon..goldenHour] of TSunCalcTimesInfo;

function GetPosition(const ADate: TDateTime; const ALat, ALon: Double): TSunPos;
function GetTimes(const ADate: TDateTime; const ALat, ALon: Double): TSunCalcTimes;

function GetMoonPosition(const ADate: TDateTime; const ALat, ALon: Double): TMoonPos;
function GetMoonFraction(const ADate: TDateTime): Double;

procedure NewSunCalcTimes(var ASunCalcTimes: TSunCalcTimes);

implementation

uses
  Math,
  DateUtils;

type
  TSunCoords = record
    dec  : Double;
    ra   : Double;
  end;

  TMoonCoords = record
    dec  : Double;
    ra   : Double;
    dist : Double;
  end;

const
  cSunCalcTimesInfo: TSunCalcTimes = (
    (Angle: 0;     Value: 0; IsRiseInfo: False), // solarNoon
    (Angle: 0;     Value: 0; IsRiseInfo: False), // nadir

    (Angle: -0.83; Value: 0; IsRiseInfo: True ),
    (Angle: -0.83; Value: 0; IsRiseInfo: False),

    (Angle: -0.3;  Value: 0; IsRiseInfo: True ),
    (Angle: -0.3;  Value: 0; IsRiseInfo: False),

    (Angle: -6;    Value: 0; IsRiseInfo: True ),
    (Angle: -6;    Value: 0; IsRiseInfo: False),

    (Angle: -12;   Value: 0; IsRiseInfo: True ),
    (Angle: -12;   Value: 0; IsRiseInfo: False),

    (Angle: -18;   Value: 0; IsRiseInfo: True ),
    (Angle: -18;   Value: 0; IsRiseInfo: False),

    (Angle: 6;     Value: 0; IsRiseInfo: True ), // goldenHourEnd
    (Angle: 6;     Value: 0; IsRiseInfo: False)  // goldenHour
  );

const
  cRad   = Pi / 180;
  cDayMs = 1000 * 60 * 60 * 24;
  J1970  = 2440588;
  J2000  = 2451545;
  e      = cRad * 23.4397; // obliquity of the Earth
  J0     = 0.0009;

procedure NewSunCalcTimes(var ASunCalcTimes: TSunCalcTimes);
begin
  ASunCalcTimes := cSunCalcTimesInfo;
end;

// sun calculations are based on http://aa.quae.nl/en/reken/zonpositie.html formulas

// date/time constants and conversions

function toJulian(const AValue: TDateTime): Double;
begin
  Result :=  DateTimeToJulianDate(AValue);
end;

function fromJulian(const AValue: Double): TDateTime;
begin
  Result := JulianDateToDateTime(AValue);
end;

function toDays(const AValue: TDateTime): Double;
begin
  Result := toJulian(AValue) - J2000;
end;

// general calculations for position

function getRightAscension(const l, b: Double): Double;
begin
  result := ArcTan2(sin(l) * cos(e) - tan(b) * sin(e), cos(l));
end;

function getDeclination(const l, b: Double): Double;
begin
  Result := ArcSin(sin(b) * cos(e) + cos(b) * sin(e) * sin(l));
end;

function getAzimuth(const H, phi, dec: Double): Double;
begin
  Result := ArcTan2(sin(H), cos(H) * sin(phi) - tan(dec) * cos(phi));
end;

function getAltitude(const H, phi, dec: Double): Double;
begin
  Result := ArcSin(sin(phi) * sin(dec) + cos(phi) * cos(dec) * cos(H));
end;

function getSiderealTime(const d, lw: Double): Double;
begin
  Result := cRad * (280.16 + 360.9856235 * d) - lw;
end;

// general sun calculations

function getSolarMeanAnomaly(const d: Double): Double;
begin
  Result := cRad * (357.5291 + 0.98560028 * d);
end;

function getEquationOfCenter(const M: Double): Double;
begin
  Result := cRad * (1.9148 * sin(M) + 0.02 * sin(2 * M) + 0.0003 * sin(3 * M));
end;

function getEclipticLongitude(const M, C: Double): Double;
var
  P: Double;
begin
  P := cRad * 102.9372; // perihelion of the Earth
  Result := M + C + P + PI;
end;

function getSunCoords(const d: Double): TSunCoords;
var
  M, C, L: Double;
begin
  M := getSolarMeanAnomaly(d);
  C := getEquationOfCenter(M);
  L := getEclipticLongitude(M, C);

  Result.dec := getDeclination(L, 0);
  Result.ra  := getRightAscension(L, 0);
end;

// calculates sun position for a given date and latitude/longitude

function GetPosition(const ADate: TDateTime; const ALat, ALon: Double): TSunPos;
var
  lw, phi, d: Double;
  c: TSunCoords;
  H: Double;
begin
  lw  := cRad * (-ALon);
  phi := cRad * ALat;
  d   := toDays(ADate);

  c := getSunCoords(d);
  H := getSiderealTime(d, lw) - c.ra;

  Result.Azimuth := getAzimuth(H, phi, c.dec);
  Result.Altitude := getAltitude(H, phi, c.dec);
end;

// calculations for sun times

function getJulianCycle(const d, lw: Double): Double;
begin
  Result := Round(d - J0 - lw / (2 * PI));
end;
function getApproxTransit(const Ht, lw: Double; const n: Double): Double;
begin
  Result := J0 + (Ht + lw) / (2 * PI) + n;
end;
function getSolarTransitJ(const ds, M, L: Double): Double;
begin
  Result := J2000 + ds + 0.0053 * sin(M) - 0.0069 * sin(2 * L);
end;
function getHourAngle(const h, phi, d: Double): Double;
begin
  Result := ArcCos((sin(h) - sin(phi) * sin(d)) / (cos(phi) * cos(d)));
end;

// calculates sun times for a given date and latitude/longitude

function GetTimes(const ADate: TDateTime; const ALat, ALon: Double): TSunCalcTimes;
var
  lw, phi, d, ds, M, C, L, dec, Jnoon: Double;
  n: Double;

  // returns set time for the given sun altitude
  function getSetJ(const h: Double): Double;
  var
    w, a: Double;
  begin
    w := getHourAngle(h, phi, dec);
    a := getApproxTransit(w, lw, n);
    Result := getSolarTransitJ(a, M, L);
  end;

var
  I: TSunCalcTimesID;
  Jset, Jrise: Double;
begin
  lw  := cRad * (-ALon);
  phi := cRad * ALat;
  d   := toDays(ADate);

  n  := getJulianCycle(d, lw);
  ds := getApproxTransit(0, lw, n);

  M := getSolarMeanAnomaly(ds);
  C := getEquationOfCenter(M);
  L := getEclipticLongitude(M, C);

  dec := getDeclination(L, 0);

  Jnoon := getSolarTransitJ(ds, M, L);

  NewSunCalcTimes(Result);

  Result[solarNoon].Value := fromJulian(Jnoon);
  Result[nadir].Value     := fromJulian(Jnoon - 0.5);

  for I := Low(Result) to High(Result) do begin
    if (I <> solarNoon) and (I <> nadir) then begin
      Jset := getSetJ(Result[I].Angle * cRad);      
      if Result[I].IsRiseInfo then begin
        Jrise := Jnoon - (Jset - Jnoon);
        Result[I].Value := fromJulian(Jrise);
      end else begin
        Result[I].Value := fromJulian(Jset);
      end;
    end;
  end;     
end;

// moon calculations, based on http://aa.quae.nl/en/reken/hemelpositie.html formulas

function getMoonCoords(const d: Double): TMoonCoords; // geocentric ecliptic coordinates of the moon
var
  L, M, F, l1, b, dt: Double;
begin
  L := cRad * (218.316 + 13.176396 * d); // ecliptic longitude
  M := cRad * (134.963 + 13.064993 * d); // mean anomaly
  F := cRad * (93.272 + 13.229350 * d);  // mean distance

  l1  := L + cRad * 6.289 * sin(M); // longitude
  b  := cRad * 5.128 * sin(F);     // latitude
  dt := 385001 - 20905 * cos(M);   // distance to the moon in km

  Result.ra   := getRightAscension(l1, b);
  Result.dec  := getDeclination(l1, b);
  Result.dist := dt;
end;

function GetMoonPosition(const ADate: TDateTime; const ALat, ALon: Double): TMoonPos;
var
  lw, phi, d, H, h1: Double;
  c: TMoonCoords;
begin
  lw  := cRad * (-ALon);
  phi := cRad * ALat;
  d   := toDays(ADate);

  c  := getMoonCoords(d);
  H  := getSiderealTime(d, lw) - c.ra;
  h1 := getAltitude(H, phi, c.dec);

  // altitude correction for refraction
  h1 := h1 + cRad * 0.017 / tan(h1 + cRad * 10.26 / (h1 + cRad * 5.10));

  Result.Azimuth  := getAzimuth(H, phi, c.dec);
  Result.Altitude := h1;
  Result.Distance := c.dist;
end;

// calculations for illuminated fraction of the moon,
// based on http://idlastro.gsfc.nasa.gov/ftp/pro/astro/mphase.pro formulas

function GetMoonFraction(const ADate: TDateTime): Double;
var
  d, phi, inc1: Double;
  s: TSunCoords;
  m: TMoonCoords;
  sdist: Double;
begin
  d := toDays(ADate);
  s := getSunCoords(d);
  m := getMoonCoords(d);

  sdist := 149598000; // distance from Earth to Sun in km

  phi := ArcCos(sin(s.dec) * sin(m.dec) + cos(s.dec) * cos(m.dec) * cos(s.ra - m.ra));
  inc1 := ArcTan2(sdist * sin(phi), m.dist - sdist * cos(phi));

  Result := (1 + cos(inc1)) / 2;
end;

end.

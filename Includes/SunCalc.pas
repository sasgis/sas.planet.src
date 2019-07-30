(*

Copyright (c) 2014, Vladimir Agafonkin
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are
permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of
      conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list
      of conditions and the following disclaimer in the documentation and/or other materials
      provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(c) 2011-2015, Vladimir Agafonkin
SunCalc is a JavaScript library for calculating sun/moon position and light phases.
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
    Azimuth          : Double;
    Altitude         : Double;
    Distance         : Double;
    ParallacticAngle : Double;
  end;

  TMoonIllumination = record
    Fraction : Double;  // illuminated fraction of the moon;
                        // varies from 0.0 (new moon) to 1.0 (full moon)

    Phase    : Double;  // moon phase; varies from 0.0 to 1.0, described below
                        // Moon phase value should be interpreted like this:
                        //  ---------------------------
                        // | Phase |  Name            |
                        // ----------------------------
                        // | 0     |  New Moon        |
                        // |       |  Waxing Crescent |
                        // | 0.25  |  First Quarter   |
                        // |       |  Waxing Gibbous  |
                        // | 0.5   |  Full Moon       |
                        // |       |  Waning Gibbous  |
                        // | 0.75  |  Last Quarter    |
                        // |       |  Waning Crescent |
                        // ----------------------------

    Angle    : Double;  // midpoint angle in radians of the illuminated limb of
                        // the moon reckoned eastward from the north point of the disk;
                        // the moon is waxing if the angle is negative, and waning if positive
  end;

  TMoonTimes = record
    MoonRise : TDateTime;
    MoonSet  : TDateTime;
  end;

  TSunCalcTimesID = (
    solarNoon,      // solar noon (sun is in the highest position)
    nadir,          // nadir (darkest moment of the night, sun is in the lowest position)
    sunrise,        // sunrise (top edge of the sun appears on the horizon)
    sunset,         // sunset (sun disappears below the horizon, evening civil twilight starts)
    sunriseEnd,     // sunrise ends (bottom edge of the sun touches the horizon)
    sunsetStart,    // sunset starts (bottom edge of the sun touches the horizon)
    dawn,           // dawn (morning nautical twilight ends, morning civil twilight starts)
    dusk,           // dusk (evening nautical twilight starts)
    nauticalDawn,   // nautical dawn (morning nautical twilight starts)
    nauticalDusk,   // nautical dusk (evening astronomical twilight starts)
    nightEnd,       // night ends (morning astronomical twilight starts)
    night,          // night starts (dark enough for astronomical observations)
    goldenHourEnd,  // morning golden hour (soft light, best time for photography) ends
    goldenHour      // evening golden hour starts
  );

  TSunCalcTimesInfo = record
    Angle      : Double;
    Value      : TDateTime;
    IsRiseInfo : Boolean;
  end;

  TSunCalcTimes = array [solarNoon..goldenHour] of TSunCalcTimesInfo;

function GetPosition(const ADate: TDateTime; const ALat, ALon: Double): TSunPos;
function GetTimes(const ADate: TDateTime; const ALat, ALon: Double): TSunCalcTimes;

function GetMoonTimes(const ADate: TDateTime; const ALat, ALon: Double): TMoonTimes;
function GetMoonPosition(const ADate: TDateTime; const ALat, ALon: Double): TMoonPos;
function GetMoonIllumination(const ADate: TDateTime): TMoonIllumination;

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
    (Angle: 0;      Value: 0; IsRiseInfo: False), // solarNoon
    (Angle: 0;      Value: 0; IsRiseInfo: False), // nadir

    (Angle: -0.833; Value: 0; IsRiseInfo: True ), // sunrise
    (Angle: -0.833; Value: 0; IsRiseInfo: False), // sunset

    (Angle: -0.3;   Value: 0; IsRiseInfo: True ), // sunriseEnd
    (Angle: -0.3;   Value: 0; IsRiseInfo: False), // sunsetStart

    (Angle: -6;     Value: 0; IsRiseInfo: True ), // dawn
    (Angle: -6;     Value: 0; IsRiseInfo: False), // dusk

    (Angle: -12;    Value: 0; IsRiseInfo: True ), // nauticalDawn
    (Angle: -12;    Value: 0; IsRiseInfo: False), // nauticalDusk

    (Angle: -18;    Value: 0; IsRiseInfo: True ), // nightEnd
    (Angle: -18;    Value: 0; IsRiseInfo: False), // night

    (Angle: 6;      Value: 0; IsRiseInfo: True ), // goldenHourEnd
    (Angle: 6;      Value: 0; IsRiseInfo: False)  // goldenHour
  );

const
  cRad   = Pi / 180;
  J2000  = 2451545;
  e      = cRad * 23.4397; // obliquity of the Earth
  J0     = 0.0009;

procedure NewSunCalcTimes(var ASunCalcTimes: TSunCalcTimes);
begin
  ASunCalcTimes := cSunCalcTimesInfo;
end;

// sun calculations are based on http://aa.quae.nl/en/reken/zonpositie.html formulas

// date/time constants and conversions

function toJulian(const AValue: TDateTime): Double; inline;
begin
  Result := DateTimeToJulianDate(AValue);
end;

function fromJulian(const AValue: Double): TDateTime; inline;
begin
  Result := JulianDateToDateTime(AValue);
end;

function toDays(const AValue: TDateTime): Double; inline;
begin
  Result := toJulian(AValue) - J2000;
end;

// general calculations for position

function getRightAscension(const l, b: Double): Double; inline;
begin
  result := ArcTan2(sin(l) * cos(e) - tan(b) * sin(e), cos(l));
end;

function getDeclination(const l, b: Double): Double; inline;
begin
  Result := ArcSin(sin(b) * cos(e) + cos(b) * sin(e) * sin(l));
end;

function getAzimuth(const H, phi, dec: Double): Double; inline;
begin
  Result := ArcTan2(sin(H), cos(H) * sin(phi) - tan(dec) * cos(phi));
end;

function getAltitude(const H, phi, dec: Double): Double; inline;
begin
  Result := ArcSin(sin(phi) * sin(dec) + cos(phi) * cos(dec) * cos(H));
end;

function getSiderealTime(const d, lw: Double): Double; inline;
begin
  Result := cRad * (280.16 + 360.9856235 * d) - lw;
end;

function astroRefraction(h: Double): Double; inline;
begin
  if h < 0 then begin // the following formula works for positive altitudes only.
    h := 0; // if h = -0.08901179 a div/0 would occur.
  end;

  // formula 16.4 of "Astronomical Algorithms" 2nd edition by Jean Meeus (Willmann-Bell, Richmond) 1998.
  // 1.02 / tan(h + 10.26 / (h + 5.10)) h in degrees, result in arc minutes -> converted to rad:
  Result := 0.0002967 / tan(h + 0.00312536 / (h + 0.08901179));
end;

// general sun calculations

function getSolarMeanAnomaly(const d: Double): Double; inline;
begin
  Result := cRad * (357.5291 + 0.98560028 * d);
end;

function getEclipticLongitude(const M: Double): Double; inline;
var
  P, C: Double;
begin
  C := cRad * (1.9148 * sin(M) + 0.02 * sin(2 * M) + 0.0003 * sin(3 * M)); // equation of center
  P := cRad * 102.9372; // perihelion of the Earth
  Result := M + C + P + PI;
end;

function getSunCoords(const d: Double): TSunCoords; inline;
var
  M, L: Double;
begin
  M := getSolarMeanAnomaly(d);
  L := getEclipticLongitude(M);

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

function getJulianCycle(const d, lw: Double): Double; inline;
begin
  Result := Round(d - J0 - lw / (2 * PI));
end;

function getApproxTransit(const Ht, lw: Double; const n: Double): Double; inline;
begin
  Result := J0 + (Ht + lw) / (2 * PI) + n;
end;

function getSolarTransitJ(const ds, M, L: Double): Double; inline;
begin
  Result := J2000 + ds + 0.0053 * sin(M) - 0.0069 * sin(2 * L);
end;

function getHourAngle(const h, phi, d: Double): Double; inline;
var
  X: Double;
begin
  X := (sin(h) - sin(phi) * sin(d)) / (cos(phi) * cos(d));
  if (X >= -1) and (X <= 1) then begin
    Result := ArcCos(X);
  end else begin
    Result := NaN;
  end;
end;

// calculates sun times for a given date and latitude/longitude

function GetTimes(const ADate: TDateTime; const ALat, ALon: Double): TSunCalcTimes;
var
  lw, phi, d, ds, M, L, dec, Jnoon: Double;
  n, h, w, a: Double;
  i: TSunCalcTimesID;
  Jset, Jrise: Double;
begin

  NewSunCalcTimes(Result);

  lw  := cRad * (-ALon);
  phi := cRad * ALat;
  d   := toDays(ADate);

  n  := getJulianCycle(d, lw);
  ds := getApproxTransit(0, lw, n);

  M := getSolarMeanAnomaly(ds);
  L := getEclipticLongitude(M);

  dec := getDeclination(L, 0);

  Jnoon := getSolarTransitJ(ds, M, L);

  if not IsNan(Jnoon) then begin
    Result[solarNoon].Value := fromJulian(Jnoon);
    Result[nadir].Value     := fromJulian(Jnoon - 0.5);
  end;

  for i := Low(Result) to High(Result) do begin
    if (i <> solarNoon) and (i <> nadir) then begin

      h := Result[i].Angle * cRad;
      w := getHourAngle(h, phi, dec);
      a := getApproxTransit(w, lw, n);
      Jset := getSolarTransitJ(a, M, L);

      if not IsNan(Jset) then begin
        if Result[i].IsRiseInfo then begin
          Jrise := Jnoon - (Jset - Jnoon);
          Result[i].Value := fromJulian(Jrise);
        end else begin
          Result[i].Value := fromJulian(Jset);
        end;
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
  lw, phi, d, H, h1, pa: Double;
  c: TMoonCoords;
begin
  lw  := cRad * (-ALon);
  phi := cRad * ALat;
  d   := toDays(ADate);

  c  := getMoonCoords(d);
  H  := getSiderealTime(d, lw) - c.ra;
  h1 := getAltitude(H, phi, c.dec);

  // formula 14.1 of "Astronomical Algorithms" 2nd edition by Jean Meeus (Willmann-Bell, Richmond) 1998.
  pa := ArcTan2(sin(H), tan(phi) * cos(c.dec) - sin(c.dec) * cos(H));

  h1 := h1 + astroRefraction(h1); // altitude correction for refraction

  Result.Azimuth  := getAzimuth(H, phi, c.dec);
  Result.Altitude := h1;
  Result.Distance := c.dist;
  Result.ParallacticAngle := pa;
end;

// calculations for illumination parameters of the moon,
// based on http://idlastro.gsfc.nasa.gov/ftp/pro/astro/mphase.pro formulas and
// Chapter 48 of "Astronomical Algorithms" 2nd edition by Jean Meeus (Willmann-Bell, Richmond) 1998.

function GetMoonIllumination(const ADate: TDateTime): TMoonIllumination;
var
  d, phi, inc1, angle: Double;
  s: TSunCoords;
  m: TMoonCoords;
  sdist: Double;
  angle_sign: Integer;
begin
  d := toDays(ADate);
  s := getSunCoords(d);
  m := getMoonCoords(d);

  sdist := 149598000; // distance from Earth to Sun in km

  phi := ArcCos(sin(s.dec) * sin(m.dec) + cos(s.dec) * cos(m.dec) * cos(s.ra - m.ra));
  inc1 := ArcTan2(sdist * sin(phi), m.dist - sdist * cos(phi));
  angle := ArcTan2(cos(s.dec) * sin(s.ra - m.ra), sin(s.dec) * cos(m.dec) -
                   cos(s.dec) * sin(m.dec) * cos(s.ra - m.ra));

  if angle < 0 then
    angle_sign := -1
  else
    angle_sign := 1;

  Result.Fraction := (1 + cos(inc1)) / 2;
  Result.Phase := 0.5 + 0.5 * inc1 * angle_sign / Pi;
  Result.Angle := angle;
end;

function hoursLater(const ADate: TDateTime; const h: Double): TDateTime; inline;
begin
  Result := IncMilliSecond(ADate, Round(h * 60 * 60 * 1000));
end;

// calculations for moon rise/set times are based on http://www.stargazing.net/kepler/moonrise.html article

function GetMoonTimes(const ADate: TDateTime; const ALat, ALon: Double): TMoonTimes;
var
  t: TDateTime;
  i, roots: Integer;
  hc, h0, h1, h2, rise, set_, a, b, xe, ye, d, x1, x2, dx: Double;
begin
  Result.MoonRise := 0;
  Result.MoonSet := 0;

  rise := 0;
  set_ := 0;

  t := DateOf(ADate);

  hc := 0.133 * cRad;
  h0 := GetMoonPosition(t, ALat, ALon).altitude - hc;

  // go in 2-hour chunks, each time seeing if a 3-point quadratic curve crosses
  // zero (which means rise or set)
  i := 1;
  while (i <= 24) do begin
    h1 := GetMoonPosition(hoursLater(t, i), ALat, ALon).altitude - hc;
    h2 := GetMoonPosition(hoursLater(t, i + 1), ALat, ALon).altitude - hc;

    a := (h0 + h2) / 2 - h1;
    b := (h2 - h0) / 2;
    xe := -b / (2 * a);
    ye := (a * xe + b) * xe + h1;
    d := b * b - 4 * a * h1;

    x1 := 0;
    x2 := 0;
    roots := 0;

    if (d >= 0) then begin
      dx := sqrt(d) / (abs(a) * 2);
      x1 := xe - dx;
      x2 := xe + dx;
      if abs(x1) <= 1 then Inc(roots);
      if abs(x2) <= 1 then Inc(roots);
      if x1 < -1 then x1 := x2;
    end;

    if roots = 1 then begin
      if h0 < 0 then begin
        rise := i + x1;
      end else begin
        set_ := i + x1;
      end;
    end else if roots = 2 then begin
      if ye < 0 then begin
        rise := i + x2;
        set_ := i + x1;
      end else begin
        rise := i + x1;
        set_ := i + x2;
      end;
    end;

    if (rise > 0) and (set_ > 0) then
      Break;

    h0 := h2;

    Inc(i, 2);
  end;

  if (rise > 0) then Result.MoonRise := hoursLater(t, rise);
  if (set_ > 0) then Result.MoonSet := hoursLater(t, set_);
end;

end.

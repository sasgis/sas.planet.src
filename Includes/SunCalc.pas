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

--------------------------------------------------------------------------------------------
Delphi port.

Ported from suncalc.js (https://github.com/mourner/suncalc), which computes Sun and Moon
position using full Meeus series for the Sun (ch. 25) and Moon (ch. 47), including a
Terrestrial-Time correction (deltaT) for the position series, and an iterative
(Newton-refined) rise/set solver. See suncalc.js source comments for the detailed
derivations; this file mirrors its structure and naming closely so the two can be diffed
against each other.

TSunPos.Azimuth/Altitude, TMoonPos.Azimuth/Altitude/ParallacticAngle, and
TMoonIllumination.Angle are all returned in DEGREES.

GetTimes and GetMoonTimes each have two overloads: one anchoring the scanned solar
day/night to the transit or nadir nearest the ADate instant itself, and one taking an
additional AUtcOffset parameter (minutes ahead of UTC) that instead anchors to the
nearest civil midnight in that zone - useful when ADate is "some time during the user's
calendar day" in a specific time zone rather than a precise UTC instant.

*)

unit SunCalc;

interface

type
  TSunPos = record
    Azimuth  : Double; // degrees, 0 = N, 90 = E, 180 = S, 270 = W
    Altitude : Double; // degrees, apparent (refraction-corrected)
  end;

  TMoonPos = record
    Azimuth          : Double; // degrees
    Altitude         : Double; // degrees, apparent (refraction-corrected)
    Distance         : Double; // km
    ParallacticAngle : Double; // degrees
  end;

  TMoonIllumination = record
    Fraction : Double;  // illuminated fraction of the moon;
                        // varies from 0.0 (new moon) to 1.0 (full moon)

    Phase    : Double;  // moon phase; varies from 0.0 to 1.0, described below. Computed from
                        // the ecliptic-longitude elongation between the Sun and Moon
                        // (Meeus ch. 49 / USNO).
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

    Angle    : Double;  // degrees: midpoint angle of the illuminated limb of the moon
                        // reckoned eastward from the north point of the disk;
                        // subtract TMoonPos.ParallacticAngle to get the zenith-relative
                        // tilt angle (useful for drawing the limb/terminator).

    Waxing   : Boolean; // True while the Moon is waxing (new -> full), False while waning
                        // (full -> new). Equivalent to (Phase < 0.5).
  end;

  TMoonTimes = record
    MoonRise      : TDateTime;
    MoonSet       : TDateTime;
    Transit       : TDateTime; // upper meridian crossing (moon culmination), reported regardless
                               // of whether the moon is above the horizon at the time
    LowerTransit  : TDateTime; // lower meridian crossing (anti-culmination)
    HasRise       : Boolean;   // False if the moon does not rise during the scanned day
    HasSet        : Boolean;   // False if the moon does not set during the scanned day
    HasTransit    : Boolean;   // False if the upper transit does not fall within the scanned day
    HasLowerTransit: Boolean;  // False if the lower transit does not fall within the scanned day
    AlwaysUp      : Boolean;   // True if the moon never sets on this day (stays above the horizon)
    AlwaysDown    : Boolean;   // True if the moon never rises on this day (stays below the horizon)
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
    HasValue   : Boolean;   // False if the sun never reaches this angle on this day
    IsRiseInfo : Boolean;
  end;

  TSunCalcTimesArr = array [solarNoon..goldenHour] of TSunCalcTimesInfo;

  TSunCalcTimes = record
    Times      : TSunCalcTimesArr;
    AlwaysUp   : Boolean; // True: sun never goes below the sunrise/sunset threshold (polar day)
    AlwaysDown : Boolean; // True: sun never goes above the sunrise/sunset threshold (polar night)
  end;

function GetPosition(const ADate: TDateTime; const ALat, ALon: Double): TSunPos;

// Anchors the scanned solar day to the transit nearest the ADate instant itself - the original
// (and still default) behaviour. AHeight: observer height in meters above the horizon.
function GetTimes(const ADate: TDateTime; const ALat, ALon: Double;
  const AHeight: Double = 0): TSunCalcTimes; overload;

// As above, but anchors the scanned solar day to the nearest civil midnight in a time zone
// AUtcOffset minutes ahead of UTC (e.g. 180 for MSK, -300 for EST) instead of to the ADate
// instant itself. Useful when ADate is "some time during the user's calendar day" in a specific
// time zone rather than a precise UTC instant.
function GetTimes(const ADate: TDateTime; const ALat, ALon: Double; const AHeight: Double;
  const AUtcOffset: Double): TSunCalcTimes; overload;

// Scans the 24 hours from the solar nadir nearest ADate for rise/set/transit - the original
// (and still default) behaviour.
function GetMoonTimes(const ADate: TDateTime; const ALat, ALon: Double): TMoonTimes; overload;

// As above, but scans the 24 hours from civil midnight in a time zone AUtcOffset minutes ahead
// of UTC instead of from the solar nadir nearest ADate. See GetTimes' AUtcOffset overload.
function GetMoonTimes(const ADate: TDateTime; const ALat, ALon: Double;
  const AUtcOffset: Double): TMoonTimes; overload;

function GetMoonPosition(const ADate: TDateTime; const ALat, ALon: Double): TMoonPos;
function GetMoonIllumination(const ADate: TDateTime): TMoonIllumination;

procedure NewSunCalcTimes(var ASunCalcTimes: TSunCalcTimes);

implementation

uses
  Math,
  DateUtils,
  SysUtils;

type
  TSunCoords = record
    dec  : Double;
    ra   : Double;
    lon  : Double;
  end;

  TMoonCoords = record
    dec  : Double;
    ra   : Double;
    dist : Double;
    lon  : Double;
  end;

const
  cSunCalcTimesInfo: TSunCalcTimesArr = (
    (Angle: 0;      Value: 0; HasValue: False; IsRiseInfo: False), // solarNoon
    (Angle: 0;      Value: 0; HasValue: False; IsRiseInfo: False), // nadir

    (Angle: -0.833; Value: 0; HasValue: False; IsRiseInfo: True ), // sunrise
    (Angle: -0.833; Value: 0; HasValue: False; IsRiseInfo: False), // sunset

    (Angle: -0.3;   Value: 0; HasValue: False; IsRiseInfo: True ), // sunriseEnd
    (Angle: -0.3;   Value: 0; HasValue: False; IsRiseInfo: False), // sunsetStart

    (Angle: -6;     Value: 0; HasValue: False; IsRiseInfo: True ), // dawn
    (Angle: -6;     Value: 0; HasValue: False; IsRiseInfo: False), // dusk

    (Angle: -12;    Value: 0; HasValue: False; IsRiseInfo: True ), // nauticalDawn
    (Angle: -12;    Value: 0; HasValue: False; IsRiseInfo: False), // nauticalDusk

    (Angle: -18;    Value: 0; HasValue: False; IsRiseInfo: True ), // nightEnd
    (Angle: -18;    Value: 0; HasValue: False; IsRiseInfo: False), // night

    (Angle: 6;      Value: 0; HasValue: False; IsRiseInfo: True ), // goldenHourEnd
    (Angle: 6;      Value: 0; HasValue: False; IsRiseInfo: False)  // goldenHour
  );

const
  cRad         = Pi / 180;
  J2000        = 2451545;
  J0           = 0.0009;
  cEarthRadius = 6378.14;   // equatorial radius in km, for the Moon's topocentric parallax
  cSunDist     = 149598000; // distance from Earth to Sun in km

procedure NewSunCalcTimes(var ASunCalcTimes: TSunCalcTimes);
begin
  ASunCalcTimes.Times := cSunCalcTimesInfo;
  ASunCalcTimes.AlwaysUp := False;
  ASunCalcTimes.AlwaysDown := False;
end;

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

// Delta T = TT - UT in seconds (Espenak & Meeus polynomial fits, good ~1900-2150). The Meeus
// position series are defined in Terrestrial Time, but SunCalc's input dates are UT - so
// the position math runs on days-since-J2000 shifted by deltaT, while sidereal time stays
// on UT. ~69 s today; negligible for the Sun (<0.001 deg), real for the Moon. d only needs
// ~month accuracy here (deltaT changes <1 s/yr), so the decimal year is derived
// arithmetically from d rather than from the original date.
function deltaT(const d: Double): Double;
var
  y, t: Double;
begin
  y := 2000 + d / 365.2425;
  if y < 1920 then begin
    t := y - 1900;
    Result := -2.79 + t * (1.494119 + t * (-0.0598939 + t * (0.0061966 - t * 0.000197)));
  end else if y < 1941 then begin
    t := y - 1920;
    Result := 21.20 + t * (0.84493 + t * (-0.076100 + t * 0.0020936));
  end else if y < 1961 then begin
    t := y - 1950;
    Result := 29.07 + t * (0.407 + t * (-1 / 233 + t / 2547));
  end else if y < 1986 then begin
    t := y - 1975;
    Result := 45.45 + t * (1.067 + t * (-1 / 260 - t / 718));
  end else if y < 2005 then begin
    t := y - 2000;
    Result := 63.86 + t * (0.3345 + t * (-0.060374 + t * (0.0017275 + t * (0.000651814 + t * 0.00002373599))));
  end else if y < 2050 then begin
    t := y - 2000;
    Result := 62.92 + t * (0.32217 + t * 0.005589);
  end else begin
    t := (y - 1820) / 100;
    Result := -20 + 32 * t * t - 0.5628 * (2150 - y);
  end;
end;

function toDaysTT(const d: Double): Double; inline;
begin
  Result := d + deltaT(d) / 86400;
end;

// general calculations for position

// north-based clockwise azimuth in degrees (0 = N, 90 = E, 180 = S, 270 = W)
function azimuthDeg(const H, phi, dec: Double): Double; inline;
begin
  Result := (ArcTan2(sin(H), cos(H) * sin(phi) - tan(dec) * cos(phi)) / cRad + 540);
  Result := Result - 360 * Floor(Result / 360); // normalize to [0, 360)
end;

function getAltitude(const H, phi, dec: Double): Double; inline;
begin
  Result := ArcSin(sin(phi) * sin(dec) + cos(phi) * cos(dec) * cos(H));
end;

// Greenwich mean sidereal time, formula 12.4 of Meeus (linear term; sub-arcsec T^2/T^3 dropped)
function getSiderealTime(const d, lw: Double): Double; inline;
begin
  Result := cRad * (280.46061837 + 360.98564736629 * d) - lw;
end;

function astroRefraction(h: Double): Double; inline;
begin
  if h < 0 then h := 0; // formula valid for positive altitudes only

  // Meeus 16.4: 1.02 / tan(h + 10.26 / (h + 5.10)), h in degrees, arcmin result -> folded into rad
  Result := 0.0002967 / tan(h + 0.00312536 / (h + 0.08901179));
end;

// general sun calculations

// Sun's apparent equatorial coordinates, Meeus ch. 25. d = days since J2000 (TT); t = Julian centuries.
function getSunCoords(const d: Double): TSunCoords;
var
  t, L0, M, sinM, cosM, C, Om, L, e: Double;
begin
  t := d / 36525;
  L0 := cRad * (280.46646 + t * (36000.76983 + t * 0.0003032)); // 25.2 geometric mean longitude
  M  := cRad * (357.52911 + t * (35999.05029 - t * 0.0001537)); // 25.3 mean anomaly
  sinM := sin(M);
  cosM := cos(M);
  C := cRad * ((1.914602 - t * (0.004817 + t * 0.000014)) * sinM +  // equation of center
       (0.019993 - 0.000101 * t) * 2 * sinM * cosM + 0.000289 * sinM * (3 - 4 * sinM * sinM));
  Om := cRad * (125.04 - 1934.136 * t); // longitude of the ascending node
  L := L0 + C - cRad * (0.00569 + 0.00478 * sin(Om)); // apparent longitude (nutation + aberration)
  // 22.2 mean obliquity + 25.8 correction for apparent position
  e := cRad * (23.439291 - t * (0.0130042 + t * (0.00000016 - t * 0.000000504))) + cRad * 0.00256 * cos(Om);

  Result.ra  := ArcTan2(cos(e) * sin(L), cos(L)); // 25.6
  Result.dec := ArcSin(sin(e) * sin(L));          // 25.7
  Result.lon := L;
end;

// calculates sun position for a given date and latitude/longitude

function GetPosition(const ADate: TDateTime; const ALat, ALon: Double): TSunPos;
var
  lw, phi, d, hourAngle, alt: Double;
  c: TSunCoords;
begin
  lw  := cRad * (-ALon);
  phi := cRad * ALat;
  d   := toDays(ADate);

  c := getSunCoords(toDaysTT(d)); // position series run on Terrestrial Time
  hourAngle := getSiderealTime(d, lw) - c.ra; // sidereal time stays on UT
  alt := getAltitude(hourAngle, phi, c.dec);

  Result.Azimuth := azimuthDeg(hourAngle, phi, c.dec);
  Result.Altitude := (alt + astroRefraction(alt)) / cRad; // apparent (refraction-corrected) altitude
end;

// calculations for sun times - Meeus ch.15 (rising, transit, setting), solving the Sun's
// local hour angle directly off the same apparent coordinates (getSunCoords) and sidereal
// time used by GetPosition. Day offsets d are in UT days since J2000; the position series
// run on TT.

function observerAngle(const AHeight: Double): Double; inline;
begin
  Result := -2.076 * sqrt(AHeight) / 60;
end;

// wrap an angle to (-PI, PI]
function wrapPi(const a: Double): Double; inline;
begin
  Result := a - 2 * PI * Round(a / (2 * PI));
end;

// refines a transit time so the Sun's local hour angle is zero (Meeus 15.2; dH/dd ~= 2*PI/day,
// the sidereal excess and the Sun's own motion cancelling to one solar day).
function solarTransit(dt: Double; const lw: Double): Double;
var
  i: Integer;
  hourAngle: Double;
begin
  for i := 0 to 2 do begin
    hourAngle := wrapPi(getSiderealTime(dt, lw) - getSunCoords(toDaysTT(dt)).ra);
    dt := dt - hourAngle / (2 * PI);
  end;
  Result := dt;
end;

// time the Sun reaches altitude h0 on the given side of transit (sign -1 = rise, +1 = set);
// starts from the hour angle at transit and converges with Meeus' altitude correction (15.2).
function getSetJ(const h0, dt: Double; const sign: Integer; const lw, phi, decT: Double): Double;
var
  cosH0, d, hourAngle, alt, sinH: Double;
  c: TSunCoords;
  i: Integer;
begin
  cosH0 := (sin(h0) - sin(phi) * sin(decT)) / (cos(phi) * cos(decT));
  if (cosH0 < -1) or (cosH0 > 1) then begin
    Result := NaN; // sun stays above / below this altitude all day
    Exit;
  end;

  d := dt + sign * ArcCos(cosH0) / (2 * PI);
  for i := 0 to 1 do begin
    c := getSunCoords(toDaysTT(d));
    hourAngle := wrapPi(getSiderealTime(d, lw) - c.ra);
    alt := getAltitude(hourAngle, phi, c.dec);
    sinH := cos(phi) * cos(c.dec) * sin(hourAngle);
    if abs(sinH) < 1e-6 then Break; // grazing the horizon - correction is ill-conditioned
    d := d + (alt - h0) / (2 * PI * sinH);
  end;
  Result := d;
end;

// start of the civil day containing ADate in a zone AUtcOffset minutes ahead of UTC, as a TDateTime
function civilMidnight(const ADate: TDateTime; const AUtcOffset: Double): TDateTime;
var
  offDays: Double;
begin
  offDays := AUtcOffset / (24 * 60);
  Result := Floor(ADate + offDays) - offDays; // floor to a whole UTC day, then undo the shift
end;

// solar transit (days since J2000) anchoring the day shared by GetTimes and GetMoonTimes: the one
// nearest the instant (the solar day containing it, when AHasUtcOffset is False), or nearest
// civil noon in the AUtcOffset zone (when AHasUtcOffset is True)
function solarDayTransit(const ADate: TDateTime; const lw: Double; const AHasUtcOffset: Boolean;
  const AUtcOffset: Double): Double;
var
  anchor, lon: Double;
begin
  if not AHasUtcOffset then
    anchor := toDays(ADate)
  else
    anchor := toDays(civilMidnight(ADate, AUtcOffset) + 0.5); // civil midnight + 12h = civil noon
  lon := J0 + lw / (2 * PI);
  Result := solarTransit(Round(anchor - lon) + lon, lw);
end;

// calculates sun times for a given date, latitude/longitude, and, optionally, the observer
// height (in meters) relative to the horizon and a UTC-offset anchor for the scanned day
function GetTimesImpl(const ADate: TDateTime; const ALat, ALon, AHeight: Double;
  const AHasUtcOffset: Boolean; const AUtcOffset: Double): TSunCalcTimes;
var
  lw, phi, dh, dt, dec, h0, jrise, jset, noonAlt, riseSetAlt: Double;
  i: TSunCalcTimesID;
begin
  NewSunCalcTimes(Result);

  lw  := cRad * (-ALon);
  phi := cRad * ALat;
  dh  := observerAngle(AHeight);

  // Anchor to the solar day containing ADate: the transit nearest the instant itself (no
  // UTC offset given), or the transit nearest civil noon in the given zone.
  dt := solarDayTransit(ADate, lw, AHasUtcOffset, AUtcOffset);
  dec := getSunCoords(toDaysTT(dt)).dec; // declination at transit, shared by every rise/set solve

  Result.Times[solarNoon].Value := fromJulian(dt + J2000);
  Result.Times[solarNoon].HasValue := True;
  Result.Times[nadir].Value := fromJulian(dt + J2000 - 0.5);
  Result.Times[nadir].HasValue := True;

  for i := Low(Result.Times) to High(Result.Times) do begin
    if (i <> solarNoon) and (i <> nadir) and Result.Times[i].IsRiseInfo then begin
      h0 := (Result.Times[i].Angle + dh) * cRad;
      jrise := getSetJ(h0, dt, -1, lw, phi, dec);
      jset  := getSetJ(h0, dt, 1, lw, phi, dec);

      // find the matching set entry for this rise angle (they share the same Angle value
      // and sit at consecutive indices, rise then set - see cSunCalcTimesInfo above)
      if not IsNaN(jrise) then begin
        Result.Times[i].Value := fromJulian(jrise + J2000);
        Result.Times[i].HasValue := True;
      end;
      if not IsNaN(jset) then begin
        Result.Times[Succ(i)].Value := fromJulian(jset + J2000);
        Result.Times[Succ(i)].HasValue := True;
      end;
    end;
  end;

  // polar day/night: when the Sun never crosses the standard rise/set altitude, flag which
  // side it stays on by comparing its altitude at solar noon (its daily maximum) against
  // that threshold.
  if not Result.Times[sunrise].HasValue then begin
    noonAlt := getAltitude(0, phi, dec);
    riseSetAlt := (Result.Times[sunrise].Angle + dh) * cRad;
    Result.AlwaysUp := noonAlt > riseSetAlt;
    Result.AlwaysDown := noonAlt <= riseSetAlt;
  end;
end;

function GetTimes(const ADate: TDateTime; const ALat, ALon: Double;
  const AHeight: Double): TSunCalcTimes;
begin
  Result := GetTimesImpl(ADate, ALat, ALon, AHeight, False, 0);
end;

function GetTimes(const ADate: TDateTime; const ALat, ALon: Double; const AHeight: Double;
  const AUtcOffset: Double): TSunCalcTimes;
begin
  Result := GetTimesImpl(ADate, ALat, ALon, AHeight, True, AUtcOffset);
end;

// moon calculations, based on Meeus ch. 47 (truncated ELP-2000/82 series)

// Nutation in longitude (dpsi) and true obliquity of the ecliptic, both in degrees,
// from the abridged series of Meeus ch. 22 (sub-arcsecond, ample for our needs).
procedure nutationObliquity(const t: Double; out dpsi, eps: Double);
var
  om, ls, lm, deps, eps0: Double;
begin
  om := cRad * (125.04452 - 1934.136261 * t); // longitude of the Moon's ascending node
  ls := cRad * (280.4665 + 36000.7698 * t);   // mean longitude of the Sun
  lm := cRad * (218.3165 + 481267.8813 * t);  // mean longitude of the Moon
  dpsi := (-17.20 * sin(om) - 1.32 * sin(2 * ls) - 0.23 * sin(2 * lm) + 0.21 * sin(2 * om)) / 3600;
  deps := (9.20 * cos(om) + 0.57 * cos(2 * ls) + 0.10 * cos(2 * lm) - 0.09 * cos(2 * om)) / 3600;
  eps0 := 23.439291 - t * (0.0130042 + t * (0.00000016 - t * 0.000000504)); // 22.2 mean obliquity
  eps := cRad * (eps0 + deps);
end;

// Meeus table 47.A - periodic terms for the Moon's longitude (Sl, x1e-6 deg) and
// distance (Sr, x1e-3 km). Flat rows of 6: D, M, M', F, Sl, Sr.
const
  moonLon: array [0..359] of Integer = (
    0, 0, 1, 0, 6288774, -20905355,
    2, 0, -1, 0, 1274027, -3699111,
    2, 0, 0, 0, 658314, -2955968,
    0, 0, 2, 0, 213618, -569925,
    0, 1, 0, 0, -185116, 48888,
    0, 0, 0, 2, -114332, -3149,
    2, 0, -2, 0, 58793, 246158,
    2, -1, -1, 0, 57066, -152138,
    2, 0, 1, 0, 53322, -170733,
    2, -1, 0, 0, 45758, -204586,
    0, 1, -1, 0, -40923, -129620,
    1, 0, 0, 0, -34720, 108743,
    0, 1, 1, 0, -30383, 104755,
    2, 0, 0, -2, 15327, 10321,
    0, 0, 1, 2, -12528, 0,
    0, 0, 1, -2, 10980, 79661,
    4, 0, -1, 0, 10675, -34782,
    0, 0, 3, 0, 10034, -23210,
    4, 0, -2, 0, 8548, -21636,
    2, 1, -1, 0, -7888, 24208,
    2, 1, 0, 0, -6766, 30824,
    1, 0, -1, 0, -5163, -8379,
    1, 1, 0, 0, 4987, -16675,
    2, -1, 1, 0, 4036, -12831,
    2, 0, 2, 0, 3994, -10445,
    4, 0, 0, 0, 3861, -11650,
    2, 0, -3, 0, 3665, 14403,
    0, 1, -2, 0, -2689, -7003,
    2, 0, -1, 2, -2602, 0,
    2, -1, -2, 0, 2390, 10056,
    1, 0, 1, 0, -2348, 6322,
    2, -2, 0, 0, 2236, -9884,
    0, 1, 2, 0, -2120, 5751,
    0, 2, 0, 0, -2069, 0,
    2, -2, -1, 0, 2048, -4950,
    2, 0, 1, -2, -1773, 4130,
    2, 0, 0, 2, -1595, 0,
    4, -1, -1, 0, 1215, -3958,
    0, 0, 2, 2, -1110, 0,
    3, 0, -1, 0, -892, 3258,
    2, 1, 1, 0, -810, 2616,
    4, -1, -2, 0, 759, -1897,
    0, 2, -1, 0, -713, -2117,
    2, 2, -1, 0, -700, 2354,
    2, 1, -2, 0, 691, 0,
    2, -1, 0, -2, 596, 0,
    4, 0, 1, 0, 549, -1423,
    0, 0, 4, 0, 537, -1117,
    4, -1, 0, 0, 520, -1571,
    1, 0, -2, 0, -487, -1739,
    2, 1, 0, -2, -399, 0,
    0, 0, 2, -2, -381, -4421,
    1, 1, 1, 0, 351, 0,
    3, 0, -2, 0, -340, 0,
    4, 0, -3, 0, 330, 0,
    2, -1, 2, 0, 327, 0,
    0, 2, 1, 0, -323, 1165,
    1, 1, -1, 0, 299, 0,
    2, 0, 3, 0, 294, 0,
    2, 0, -1, -2, 0, 8752
  );

  // Meeus table 47.B - periodic terms for the Moon's latitude (Sb, x1e-6 deg).
  // Flat rows of 5: D, M, M', F, Sb.
  moonLat: array [0..299] of Integer = (
    0, 0, 0, 1, 5128122,
    0, 0, 1, 1, 280602,
    0, 0, 1, -1, 277693,
    2, 0, 0, -1, 173237,
    2, 0, -1, 1, 55413,
    2, 0, -1, -1, 46271,
    2, 0, 0, 1, 32573,
    0, 0, 2, 1, 17198,
    2, 0, 1, -1, 9266,
    0, 0, 2, -1, 8822,
    2, -1, 0, -1, 8216,
    2, 0, -2, -1, 4324,
    2, 0, 1, 1, 4200,
    2, 1, 0, -1, -3359,
    2, -1, -1, 1, 2463,
    2, -1, 0, 1, 2211,
    2, -1, -1, -1, 2065,
    0, 1, -1, -1, -1870,
    4, 0, -1, -1, 1828,
    0, 1, 0, 1, -1794,
    0, 0, 0, 3, -1749,
    0, 1, -1, 1, -1565,
    1, 0, 0, 1, -1491,
    0, 1, 1, 1, -1475,
    0, 1, 1, -1, -1410,
    0, 1, 0, -1, -1344,
    1, 0, 0, -1, -1335,
    0, 0, 3, 1, 1107,
    4, 0, 0, -1, 1021,
    4, 0, -1, 1, 833,
    0, 0, 1, -3, 777,
    4, 0, -2, 1, 671,
    2, 0, 0, -3, 607,
    2, 0, 2, -1, 596,
    2, -1, 1, -1, 491,
    2, 0, -2, 1, -451,
    0, 0, 3, -1, 439,
    2, 0, 2, 1, 422,
    2, 0, -3, -1, 421,
    2, 1, -1, 1, -366,
    2, 1, 0, 1, -351,
    4, 0, 0, 1, 331,
    2, -1, 1, 1, 315,
    2, -2, 0, -1, 302,
    0, 0, 1, 3, -283,
    2, 1, 1, -1, -229,
    1, 1, 0, -1, 223,
    1, 1, 0, 1, 223,
    0, 1, -2, -1, -220,
    2, 1, -1, -1, -220,
    1, 0, 1, 1, -185,
    2, -1, -2, -1, 181,
    0, 1, 2, 1, -177,
    4, 0, -2, -1, 176,
    4, -1, -1, -1, 166,
    1, 0, 1, -1, -164,
    4, 0, 1, -1, 132,
    1, 0, -1, -1, -119,
    4, -1, 0, -1, 115,
    2, -2, 0, 1, 107
  );

// geocentric apparent equatorial coordinates of the Moon, Meeus ch. 47. d = days since J2000 (TT).
function getMoonCoords(const dJ: Double): TMoonCoords;
var
  t, Lp, D, M, Mp, F, A1, A2, A3, E: Double;
  Dr, Mr, Mpr, Fr: Double;
  sl, sr, sb: Double;
  i, mCoef: Integer;
  arg, eFactor: Double;
  A1r, Lpr: Double;
  dpsi, eps: Double;
  l, b: Double;
begin
  t := dJ / 36525;

  // fundamental arguments (degrees), 47.1-47.6
  Lp := 218.3164477 + t * (481267.88123421 + t * (-0.0015786 + t * (1 / 538841 - t / 65194000)));
  D  := 297.8501921 + t * (445267.1114034 + t * (-0.0018819 + t * (1 / 545868 - t / 113065000)));
  M  := 357.5291092 + t * (35999.0502909 + t * (-0.0001536 + t / 24490000));
  Mp := 134.9633964 + t * (477198.8675055 + t * (0.0087414 + t * (1 / 69699 - t / 14712000)));
  F  := 93.2720950 + t * (483202.0175233 + t * (-0.0036539 + t * (-1 / 3526000 + t / 863310000)));
  A1 := 119.75 + 131.849 * t;
  A2 := 53.09 + 479264.290 * t;
  A3 := 313.45 + 481266.484 * t;
  E  := 1 - t * (0.002516 + t * 0.0000074); // eccentricity factor for solar-anomaly terms

  Dr := cRad * D;
  Mr := cRad * M;
  Mpr := cRad * Mp;
  Fr := cRad * F;
  sl := 0; sr := 0; sb := 0;

  i := 0;
  while i < Length(moonLon) do begin
    mCoef := moonLon[i + 1];
    arg := moonLon[i] * Dr + mCoef * Mr + moonLon[i + 2] * Mpr + moonLon[i + 3] * Fr;
    if (mCoef = 1) or (mCoef = -1) then eFactor := E
    else if (mCoef = 2) or (mCoef = -2) then eFactor := E * E
    else eFactor := 1;
    sl := sl + moonLon[i + 4] * eFactor * sin(arg);
    sr := sr + moonLon[i + 5] * eFactor * cos(arg);
    Inc(i, 6);
  end;

  i := 0;
  while i < Length(moonLat) do begin
    mCoef := moonLat[i + 1];
    arg := moonLat[i] * Dr + mCoef * Mr + moonLat[i + 2] * Mpr + moonLat[i + 3] * Fr;
    if (mCoef = 1) or (mCoef = -1) then eFactor := E
    else if (mCoef = 2) or (mCoef = -2) then eFactor := E * E
    else eFactor := 1;
    sb := sb + moonLat[i + 4] * eFactor * sin(arg);
    Inc(i, 5);
  end;

  // additive terms (Venus, Jupiter and flattening of the Earth), 47 p.342
  A1r := cRad * A1;
  Lpr := cRad * Lp;
  sl := sl + 3958 * sin(A1r) + 1962 * sin(Lpr - Fr) + 318 * sin(cRad * A2);
  sb := sb - 2235 * sin(Lpr) + 382 * sin(cRad * A3) + 175 * sin(A1r - Fr) + 175 * sin(A1r + Fr) +
        127 * sin(Lpr - Mpr) - 115 * sin(Lpr + Mpr);

  nutationObliquity(t, dpsi, eps);
  l := cRad * (Lp + sl / 1e6 + dpsi); // apparent ecliptic longitude
  b := cRad * (sb / 1e6);            // ecliptic latitude

  Result.ra   := ArcTan2(sin(l) * cos(eps) - tan(b) * sin(eps), cos(l)); // 13.3
  Result.dec  := ArcSin(sin(b) * cos(eps) + cos(b) * sin(eps) * sin(l)); // 13.4
  Result.dist := 385000.56 + sr / 1000; // distance to the Moon in km
  Result.lon  := l;
end;

function GetMoonPosition(const ADate: TDateTime; const ALat, ALon: Double): TMoonPos;
var
  lw, phi, d, hourAngle, hGeo, alt, pa: Double;
  c: TMoonCoords;
begin
  lw  := cRad * (-ALon);
  phi := cRad * ALat;
  d   := toDays(ADate);

  c := getMoonCoords(toDaysTT(d)); // position series run on Terrestrial Time
  hourAngle := getSiderealTime(d, lw) - c.ra; // sidereal time stays on UT

  // geocentric parallax (Meeus ch.40) lowers the moon along its vertical circle, leaving
  // azimuth unchanged; sin(parallax) = (Earth radius / distance) * cos(geocentric altitude).
  hGeo := getAltitude(hourAngle, phi, c.dec);
  alt := hGeo - ArcSin(cEarthRadius / c.dist * cos(hGeo));

  // parallactic angle, Meeus 14.1
  pa := ArcTan2(sin(hourAngle), tan(phi) * cos(c.dec) - sin(c.dec) * cos(hourAngle));

  Result.Azimuth := azimuthDeg(hourAngle, phi, c.dec);
  Result.Altitude := (alt + astroRefraction(alt)) / cRad; // apparent (refraction-corrected) altitude
  Result.Distance := c.dist;
  Result.ParallacticAngle := pa / cRad;
end;

// moon illumination parameters, Meeus ch. 48 (and idlastro's mphase.pro)

function GetMoonIllumination(const ADate: TDateTime): TMoonIllumination;
var
  d, phi, inc1, angle: Double;
  s: TSunCoords;
  m: TMoonCoords;
begin
  d := toDaysTT(toDays(ADate));
  s := getSunCoords(d);
  m := getMoonCoords(d);

  phi := ArcCos(sin(s.dec) * sin(m.dec) + cos(s.dec) * cos(m.dec) * cos(s.ra - m.ra));
  inc1 := ArcTan2(cSunDist * sin(phi), m.dist - cSunDist * cos(phi));
  angle := ArcTan2(cos(s.dec) * sin(s.ra - m.ra), sin(s.dec) * cos(m.dec) -
                   cos(s.dec) * sin(m.dec) * cos(s.ra - m.ra));

  // elongation in ecliptic longitude, which defines the named phases (Meeus ch. 49, USNO); the RA
  // difference behind `angle` crosses 0/180 deg hours off conjunction/opposition near the nodes
  Result.Phase := FMod(FMod((m.lon - s.lon) / (2 * PI), 1) + 1, 1);
  Result.Waxing := Result.Phase < 0.5; // true while the Moon is waxing (new -> full)

  // illuminated fraction, 0 (new) -> 1 (full); reaches the exact extrema only at perfect
  // syzygy (eclipses), so a "full" moon typically peaks a hair under 1 - this is correct
  Result.Fraction := (1 + cos(inc1)) / 2;
  // position angle of the bright limb, degrees - subtract GetMoonPosition().ParallacticAngle,
  // also degrees, to get the zenith-relative tilt
  Result.Angle := angle / cRad;
end;

function hoursLater(const ADate: TDateTime; const h: Double): TDateTime; inline;
begin
  Result := ADate + h / 24;
end;

// height of the moon's upper limb above the rise/set horizon (deg): topocentric centre
// altitude plus the moon's semidiameter (0.2725 * equatorial horizontal parallax, so it
// tracks distance: ~0.25 deg at apogee, ~0.28 deg at perigee) plus the residual horizon
// refraction our model under-bends (~0.09 deg, tuned vs USNO). Crossing zero == upper-limb
// rise/set, the USNO convention.
function moonHeight(const ADate: TDateTime; const ALat, ALon: Double): Double;
var
  p: TMoonPos;
begin
  p := GetMoonPosition(ADate, ALat, ALon);
  Result := p.Altitude + 0.2725 * ArcSin(cEarthRadius / p.Distance) / cRad + 0.09;
end;

// polish a crossing time (ms): the quadratic sampler's parabola root sits up to ~0.2 deg off
// the true altitude curve, so Newton-refine against the real moonHeight. Two central-
// difference steps drop the mean error from ~0.65 to ~0.28 min; more don't help (near
// grazing dh/dt is small either way).
function refineMoonCross(tDate: TDateTime; const ALat, ALon: Double): TDateTime;
var
  i: Integer;
  h, dh: Double;
  msStep: Double; // 30 seconds, expressed in TDateTime days
const
  cMsStepDays = 30 / (24 * 60 * 60); // 30 seconds as a fraction of a day
begin
  msStep := cMsStepDays;
  for i := 0 to 1 do begin
    h := moonHeight(tDate, ALat, ALon);
    dh := (moonHeight(tDate + msStep, ALat, ALon) - moonHeight(tDate - msStep, ALat, ALon)) /
          (2 * msStep * 24 * 60); // degrees per minute
    tDate := tDate - (h / dh) / (24 * 60); // h/dh is minutes -> convert to days
  end;
  Result := tDate;
end;

// the moon's local hour angle (radians, unwrapped) at d (days since J2000, UT)
function moonHourAngle(const d, lw: Double): Double; inline;
begin
  Result := getSiderealTime(d, lw) - getMoonCoords(toDaysTT(d)).ra;
end;

// time (days since J2000) in [d0, d0 + 1) when the moon's hour angle reaches H0 (0 = upper
// transit, PI = lower), or NaN when the window misses it, about once a month since the lunar
// day lasts ~24.8 hours
function moonTransit(const d0, lw, H0: Double): Double;
const
  cRate = 2 * PI * 0.96614; // mean hour angle rate (347.8 deg per day) in radians per day
var
  a, d: Double;
  i: Integer;
begin
  a := H0 - moonHourAngle(d0, lw);
  d := d0 + (a - 2 * PI * Floor(a / (2 * PI))) / cRate; // first crossing after d0 at the mean rate
  for i := 0 to 1 do
    d := d - wrapPi(moonHourAngle(d, lw) - H0) / cRate;
  if d < d0 + 1 then
    Result := d
  else
    Result := NaN;
end;

function GetMoonTimesImpl(const ADate: TDateTime; const ALat, ALon: Double;
  const AHasUtcOffset: Boolean; const AUtcOffset: Double): TMoonTimes;
var
  t, lw, transit, lowerTransit: Double;
  i, roots: Integer;
  h0, h1, h2, hMax, rise, set_, a, b, xe, ye, d, x1, x2, dx: Double;
  hasRise, hasSet: Boolean;
begin
  Result.MoonRise := 0;
  Result.MoonSet := 0;
  Result.Transit := 0;
  Result.LowerTransit := 0;
  Result.HasRise := False;
  Result.HasSet := False;
  Result.HasTransit := False;
  Result.HasLowerTransit := False;
  Result.AlwaysUp := False;
  Result.AlwaysDown := False;

  hasRise := False;
  hasSet := False;
  rise := 0;
  set_ := 0;

  lw := cRad * (-ALon);

  // scan the same day GetTimes resolves: the 24 hours from the solar nadir nearest ADate (no
  // UTC offset given), or from civil midnight when the observer's UTC offset is given
  if not AHasUtcOffset then
    t := fromJulian(solarDayTransit(ADate, lw, AHasUtcOffset, AUtcOffset) + J2000 - 0.5)
  else
    t := civilMidnight(ADate, AUtcOffset);

  h0 := moonHeight(t, ALat, ALon);
  hMax := h0;

  // go in 2-hour chunks, each time seeing if a 3-point quadratic curve crosses zero (which
  // means rise or set)
  i := 1;
  while i <= 24 do begin
    h1 := moonHeight(hoursLater(t, i), ALat, ALon);
    h2 := moonHeight(hoursLater(t, i + 1), ALat, ALon);
    hMax := Max(hMax, Max(h1, h2));

    a := (h0 + h2) / 2 - h1;
    b := (h2 - h0) / 2;
    xe := -b / (2 * a);
    d := b * b - 4 * a * h1;

    x1 := 0;
    x2 := 0;
    roots := 0;
    ye := (a * xe + b) * xe + h1;

    if d >= 0 then begin
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
        hasRise := True;
      end else begin
        set_ := i + x1;
        hasSet := True;
      end;
    end else if roots = 2 then begin
      if ye < 0 then begin
        rise := i + x2;
        set_ := i + x1;
      end else begin
        rise := i + x1;
        set_ := i + x2;
      end;
      hasRise := True;
      hasSet := True;
    end;

    if hasRise and hasSet then Break;

    h0 := h2;
    Inc(i, 2);
  end;

  // compare against "has" flags, not truthiness of the numeric value: a crossing at exactly
  // hour 0 (midnight) is a real event whose numeric value is 0.
  if hasRise then begin
    Result.MoonRise := refineMoonCross(hoursLater(t, rise), ALat, ALon);
    Result.HasRise := True;
  end;
  if hasSet then begin
    Result.MoonSet := refineMoonCross(hoursLater(t, set_), ALat, ALon);
    Result.HasSet := True;
  end;

  // meridian crossings, reported whether or not the moon is above the horizon at the time
  transit := moonTransit(toDays(t), lw, 0);
  lowerTransit := moonTransit(toDays(t), lw, PI);
  if not IsNaN(transit) then begin
    Result.Transit := fromJulian(transit + J2000);
    Result.HasTransit := True;
  end;
  if not IsNaN(lowerTransit) then begin
    Result.LowerTransit := fromJulian(lowerTransit + J2000);
    Result.HasLowerTransit := True;
  end;

  // no crossing all day: flag which side the moon stays on by testing its highest sampled
  // height against the rise/set threshold (already baked into moonHeight), setting both like
  // GetTimes does
  if (not hasRise) and (not hasSet) then begin
    Result.AlwaysUp := hMax > 0;
    Result.AlwaysDown := hMax <= 0;
  end;
end;

function GetMoonTimes(const ADate: TDateTime; const ALat, ALon: Double): TMoonTimes;
begin
  Result := GetMoonTimesImpl(ADate, ALat, ALon, False, 0);
end;

function GetMoonTimes(const ADate: TDateTime; const ALat, ALon: Double;
  const AUtcOffset: Double): TMoonTimes;
begin
  Result := GetMoonTimesImpl(ADate, ALat, ALon, True, AUtcOffset);
end;

end.

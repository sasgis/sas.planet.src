unit Solstice;

interface

// Methods for calculations of the equinixes and solstice of the sun accroding to
// Chapter 27 "Astronomical Algorithms", Jean Meeus.

// Accuracy is within one minute of time for the years 1951-2050
// Results are valid for the years -1000 to +3000. But also quite good for before -1000

// returns the March equinox for the given year
function March(const AYear: Word): TDateTime;

// returns the June solstice for the given year
function June(const AYear: Word): TDateTime;

// returns the September equinox for the given year
function September(const AYear: Word): TDateTime;

// returns the December solstice for a given year
function December(const AYear: Word): TDateTime;

implementation

uses
  Math,
  DateUtils;

type
  TArrayOf5Double = array [0..4] of Double;

const
  mc0: TArrayOf5Double = (1721139.29189, 365242.13740, 0.06134, 0.00111, -0.00071);
	jc0: TArrayOf5Double = (1721233.25401, 365241.72562, -0.05232, 0.00907, 0.00025);
	sc0: TArrayOf5Double = (1721325.70455, 365242.49558, -0.11677, -0.00297, 0.00074);
	dc0: TArrayOf5Double = (1721414.39987, 365242.88257, -0.00769, -0.00933, -0.00006);

	mc2: TArrayOf5Double = (2451623.80984, 365242.37404, 0.05169, -0.00411, -0.00057);
	jc2: TArrayOf5Double = (2451716.56767, 365241.62603, 0.00325, 0.00888, -0.00030);
	sc2: TArrayOf5Double = (2451810.21715, 365242.01767, -0.11575, 0.00337, 0.00078);
	dc2: TArrayOf5Double = (2451900.05952, 365242.74049, -0.06223, -0.00823, 0.00032);

type
  TTermsArray = array [0..23] of array [0..2] of Double;

const
  terms: TTermsArray = (
    (485, 324.96, 1934.136),
		(203, 337.23, 32964.467),
		(199, 342.08, 20.186),
		(182, 27.85, 445267.112),
		(156, 73.14, 45036.886),
		(136, 171.52, 22518.443),
		(77, 222.54, 65928.934),
		(74, 296.72, 3034.906),
		(70, 243.58, 9037.513),
		(58, 119.81, 33718.147),
		(52, 297.17, 150.678),
		(50, 21.02, 2281.226),

		(45, 247.54, 29929.562),
		(44, 325.15, 31555.956),
		(29, 60.93, 4443.417),
		(18, 155.12, 67555.328),
		(17, 288.79, 4562.452),
		(16, 198.04, 62894.029),
		(14, 199.76, 31436.921),
		(12, 95.39, 14577.848),
		(12, 287.11, 31931.756),
		(12, 320.81, 34777.259),
		(9, 227.73, 1222.114),
		(8, 15.45, 16859.074)
  );

// J2000Century returns the number of Julian centuries since J2000
function J2000Century(const JDE: Double): Double; inline;
const
  J2000 = 2451545.0; // Julian date corresponding to January 1.5, year 2000
  JulianCentury = 36525; // days
begin
	Result := (JDE - J2000) / JulianCentury
end;

function Horner(const x: Double; const c: TArrayOf5Double): Double; inline;
var
  I: Integer;
begin
	I := Length(c) - 1;
	Result := c[I];
	while I > 0 do begin
		Dec(I);
		Result := Result * x + c[I];
  end;
end;

function eq(const y: Integer; const c: TArrayOf5Double): Double;
const
  k1 = 35999.373 * Pi / 180;
  k2 = 2.47 * Pi / 180;
  cDegToRad = Pi / 180;
var
  I: Integer;
  J0, T, W, S: Double;
  dL: Double;
begin
	J0 := Horner(y * 0.001, c);
	T := J2000Century(J0);
	W := k1 * T - k2;
	dL := 1 + 0.0334 * Cos(W) + 0.0007 * Cos(2 * W);
	S := 0;
	for I := Length(terms) - 1 downto 0 do begin
		S := S + terms[I][0] * Cos( (terms[I][1] + terms[I][2] * T) * cDegToRad );
	end;
  Result := J0 + 0.00001 * S / dL; // This is the answer in Julian Emphemeris Days
end;

procedure ModF(v: Double; out zf: Integer; out f: Double); inline;
begin
  if v < 0 then begin
    v := -v;
		zf := -Floor(v);
    f := -Frac(v);
	end else begin
		zf := Floor(v);
    f := Frac(v);
  end;
end;

procedure JdToCalendar(const jd: Double; out year, month: Integer; out day: Double); inline;
var
  zf: Integer;
  f: Double;
  a, b, c, d, e, z, alpha: Integer;
begin
	ModF(jd + 0.5, zf, f);
	z := zf;
	a := z;
	if z >= 2299151 then begin
		alpha := Floor((z * 100 - 186721625) / 3652425);
		a := z + 1 + alpha - Floor(alpha / 4);
  end;

	b := a + 1524;
	c := Floor((b * 100 - 12210) / 36525);
	d := Floor(36525 * c / 100);
	e := Floor((b - d) * 1e4 / 306001);

	// compute return values
	day := ((b - d) - Floor(306001 * e / 1e4)) + f;

	if (e = 14) or (e = 15) then begin
		month := e - 13;
  end else begin
		month := e - 1;
  end;

	if (month = 1) or (month = 2) then begin
		year := c - 4715;
  end else begin
		year := c - 4716;
  end;
end;

// Estimate Delta T for the given Calendar. This is based on Espenak and Meeus,
// "Five Millennium Canon of Solar Eclipses: -1999 to +3000" (NASA/TP-2006-214141)
// see http://eclipse.gsfc.nasa.gov/SEcat5/deltatpoly.html
function EstimateDeltaT(const Jde: Double): Double;
var
  y, m: Integer;
  year, t, u, d: Double;
begin
  JdToCalendar(Jde, y, m, d);
  year := y + (m - 0.5) / 12;

  if (y >= 2005) and (y < 2050)  then begin
    t := year - 2000;
    Result := 62.92 + 0.32217 * t + 0.005589 * Power(t, 2);
    Exit;
  end;

  case y of
    -500..499: begin
      u := year / 100;
      Result := 10583.6 - 1014.41 * u + 33.78311 * Power(u, 2) - 5.952053 * Power(u, 3) -
        0.1798452 * Power(u, 4) + 0.022174192 * Power(u, 5) + 0.0090316521 * Power(u, 6);
    end;

    500..1599: begin
			u := (year - 1000) / 100;
			Result := 1574.2 - 556.01 * u + 71.23472 * Power(u, 2) + 0.319781 * Power(u, 3) -
					0.8503463 * Power(u, 4) - 0.005050998 * Power(u, 5) + 0.0083572073 * Power(u, 6);
    end;

    1600..1699: begin
			t := year - 1600;
			Result := 120 - 0.9808 * t - 0.01532 * Power(t, 2) + Power(t, 3) / 7129;
    end;

    1700..1799: begin
			t := year - 1700;
			Result := 8.83 + 0.1603 * t - 0.0059285 * Power(t, 2) + 0.00013336 * Power(t, 3) - Power(t, 4) / 1174000;
    end;

    1800..1859: begin
			t := year - 1800;
			Result := 13.72 - 0.332447 * t + 0.0068612 * Power(t, 2) + 0.0041116 * Power(t, 3) - 0.00037436 * Power(t, 4) +
					0.0000121272 * Power(t, 5) - 0.0000001699 * Power(t, 6) + 0.000000000875 * Power(t, 7);
    end;

    1860..1899: begin
			t := year - 1860;
			Result := 7.62 + 0.5737 * t - 0.251754 * Power(t, 2) + 0.01680668 * Power(t, 3) -
					0.0004473624 * Power(t, 4) + Power(t, 5) / 233174;
    end;

    1900..1919: begin
      t := year - 1900;
			Result := -2.79 + 1.494119 * t - 0.0598939 * Power(t, 2) + 0.0061966 * Power(t, 3) - 0.000197 * Power(t, 4);
    end;

    1920..1940: begin
      t := year - 1920;
			Result := 21.20 + 0.84493 * t - 0.076100 * Power(t, 2) + 0.0020936 * Power(t, 3);
    end;

    1941..1960: begin
      t := year - 1950;
			Result := 29.07 + 0.407 * t - Power(t, 2) / 233 + Power(t, 3) / 2547;
    end;

    1961..1985: begin
      t := year - 1975;
			Result := 45.45 + 1.067 * t - Power(t, 2) / 260 - Power(t, 3) / 718;
    end;

    1986..2004: begin
      t := year - 2000;
		  Result := 63.86 + 0.3345 * t - 0.060374 * Power(t, 2) + 0.0017275 * Power(t, 3) +
        0.000651814 * Power(t, 4) + 0.00002373599 * Power(t, 5);
    end;

    2005..2049: begin
      t := year - 2000;
      Result := 62.92 + 0.32217 * t + 0.005589 * Power(t, 2);
    end;

    2050..2150: begin
      Result := -20 + 32 * Power(((year - 1820) / 100), 2) - 0.5628 * (2150 - year);
	  end;
  else
    begin
      u := (year - 1820) / 100;
      Result := -20 + 32 * Power(u, 2);
    end;
  end;
end;

function JdeToJd(const Jde: Double): Double; inline;
var
  deltaT: Double;
begin
  deltaT := EstimateDeltaT(Jde);
	Result := Jde - deltaT / 86400; // Julian day
end;

function March(const AYear: Word): TDateTime;
var
  JDE, JD: Double;
begin
  if AYear < 1000 then begin
    JDE := eq(AYear, mc0);
  end else begin
    JDE := eq(AYear - 2000, mc2);
  end;
  JD := JdeToJd(JDE);
  Result := JulianDateToDateTime(JD);
end;

function June(const AYear: Word): TDateTime;
var
  JDE, JD: Double;
begin
  if AYear < 1000 then begin
    JDE := eq(AYear, jc0);
  end else begin
    JDE := eq(AYear - 2000, jc2);
  end;
  JD := JdeToJd(JDE);
  Result := JulianDateToDateTime(JD);
end;

function September(const AYear: Word): TDateTime;
var
  JDE, JD: Double;
begin
  if AYear < 1000 then begin
    JDE := eq(AYear, sc0);
  end else begin
    JDE := eq(AYear - 2000, sc2);
  end;
  JD := JdeToJd(JDE);
  Result := JulianDateToDateTime(JD);
end;

function December(const AYear: Word): TDateTime;
var
  JDE, JD: Double;
begin
  if AYear < 1000 then begin
    JDE := eq(AYear, dc0);
  end else begin
    JDE := eq(AYear - 2000, dc2);
  end;
  JD := JdeToJd(JDE);
  Result := JulianDateToDateTime(JD);
end;

end.

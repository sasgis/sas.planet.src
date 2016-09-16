unit Proj4SK42;

interface

function long_to_gauss_kruger_zone(const ALon: Double): Integer; inline;

function get_sk42_gauss_kruger_init(const ALon, ALat: Double): AnsiString; overload;
function get_sk42_gauss_kruger_init(const AZone: Integer; const AIsNorth: Boolean): AnsiString; overload;

function geodetic_wgs84_to_sk42(var ALon, ALat: Double): Boolean;
function geodetic_sk42_to_wgs84(var ALon, ALat: Double): Boolean;
function geodetic_sk42_to_gauss_kruger(const ALon, ALat: Double; out AX, AY: Double): Boolean;
function geodetic_wgs84_to_gauss_kruger(const ALon, ALat: Double; out AX, AY: Double): Boolean;

function gauss_kruger_to_wgs84(
  const AX, AY: Double;
  const AZone: Integer;
  const AIsNorth: Boolean;
  out ALon, ALat: Double
): Boolean;

function gauss_kruger_to_sk42(
  const AX, AY: Double;
  const AZone: Integer;
  const AIsNorth: Boolean;
  out ALon, ALat: Double
): Boolean;

implementation

uses
  Math,
  SysUtils,
  Proj4Defs,
  Proj4Utils;

function long_to_gauss_kruger_zone(const ALon: Double): Integer; inline;
begin
  if ALon > 0 then begin
    Result := Floor(ALon / 6) + 1;
  end else begin
    Result := Floor((180 + ALon) / 6) + 30 + 1;
  end;
end;

function get_sk42_gauss_kruger_init(const ALon, ALat: Double): AnsiString;
var
  zone: Integer;
begin
  zone := long_to_gauss_kruger_zone(ALon);
  Result := get_sk42_gauss_kruger_init(zone, (ALat > 0));
end;

function get_sk42_gauss_kruger_init(const AZone: Integer; const AIsNorth: Boolean): AnsiString;
var
  lon_0: Integer;
  x_0, y_0: Integer;
begin
  lon_0 := AZone * 6 - 3;
  if AZone > 30 then begin
    lon_0 := lon_0 - 360;
  end;
  x_0 := AZone * 1000000 + 500000;
  if AIsNorth then begin
    y_0 := 0;
  end else begin
    y_0 := 10000000;
  end;
  Result := AnsiString(Format(gauss_kruger_fmt, [lon_0, x_0, y_0]));
end;

function geodetic_wgs84_to_sk42(var ALon, ALat: Double): Boolean;
begin
  Result := geodetic_cs_to_cs(wgs_84, sk_42, ALon, ALat);
end;

function geodetic_sk42_to_wgs84(var ALon, ALat: Double): Boolean;
begin
  Result := geodetic_cs_to_cs(sk_42, wgs_84, ALon, ALat);
end;

function geodetic_sk42_to_gauss_kruger(const ALon, ALat: Double; out AX, AY: Double): Boolean;
var
  gk_sk42: AnsiString;
begin
  gk_sk42 := get_sk42_gauss_kruger_init(ALon, ALat);
  Result := geodetic_cs_to_projected_cs(sk_42, gk_sk42, ALon, ALat, AX, AY);
end;

function geodetic_wgs84_to_gauss_kruger(const ALon, ALat: Double; out AX, AY: Double): Boolean;
var
  long_sk42, lat_sk42: Double;
  gk_sk42: AnsiString;
begin
  long_sk42 := ALon;
  lat_sk42 := ALat;
  Result := geodetic_cs_to_cs(wgs_84, sk_42, long_sk42, lat_sk42);
  if Result then begin
    gk_sk42 := get_sk42_gauss_kruger_init(long_sk42, lat_sk42);
    Result := geodetic_cs_to_projected_cs(sk_42, gk_sk42, long_sk42, lat_sk42, AX, AY);
  end;
end;

function gauss_kruger_to_wgs84(
  const AX, AY: Double;
  const AZone: Integer;
  const AIsNorth: Boolean;
  out ALon, ALat: Double
): Boolean;
begin
  Result := gauss_kruger_to_sk42(AX, AY, AZone, AIsNorth, ALon, ALat);
  if Result then begin
    Result := geodetic_cs_to_cs(sk_42, wgs_84, ALon, ALat);
  end;
end;

function gauss_kruger_to_sk42(
  const AX, AY: Double;
  const AZone: Integer;
  const AIsNorth: Boolean;
  out ALon, ALat: Double
): Boolean;
var
  gk_sk42: AnsiString;
begin
  gk_sk42 := get_sk42_gauss_kruger_init(AZone, AIsNorth);
  Result := projected_cs_to_geodetic_cs(gk_sk42, sk_42, AX, AY, ALon, ALat);
end;

end.

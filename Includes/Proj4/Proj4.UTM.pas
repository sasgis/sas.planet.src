unit Proj4.UTM;

interface

function wgs84_long_to_utm_zone(const ALon: Double): Integer; inline;
function wgs84_longlat_to_utm_zone(const ALon, ALat: Double; out AZone: Integer; out ALatBand: Char): Boolean;

function utm_zone_to_wgs84_long(const AZone: Integer): Double; inline; overload;
function utm_zone_to_wgs84_long(const AZone: Integer; const ALatBand: Char): Double; overload;

function get_utm_init(const AZone: Integer; const ALatBand: Char): string; inline; overload;
function get_utm_init(const AZone: Integer; const AIsNorth: Boolean): string; inline; overload;

function geodetic_wgs84_to_utm(const ALon, ALat: Double; out AX, AY: Double): Boolean;

function utm_to_wgs84(const AX, AY: Double; const AZone: Integer; const AIsNorth: Boolean;
  out ALon, ALat: Double): Boolean;

function geodetic_wgs84_to_ups(const ALon, ALat: Double; out AX, AY: Double): Boolean;
function ups_to_wgs84(const AX, AY: Double; const AIsNorth: Boolean; out ALon, ALat: Double): Boolean;

const
  CMgrsSouthLatBands = 'CDEFGHJKLM';
  CMgrsNorthLatBands = 'NPQRSTUVWXX'; // X is repeated for 80-84N

  CMgrsLatBands = CMgrsSouthLatBands + CMgrsNorthLatBands;

implementation

uses
  Math,
  SysUtils,
  Proj4.Utils,
  Proj4.Defines;

function wgs84_long_to_utm_zone(const ALon: Double): Integer;
begin
  Result := Floor( (ALon + 180) / 6 ) + 1;
  if Result > 60 { ALon = 180 } then begin
    Result := 60;
  end;
end;

function wgs84_longlat_to_utm_zone(const ALon, ALat: Double; out AZone: Integer; out ALatBand: Char): Boolean;
begin
  Result := (ALat < 84) and (ALat > -80);

  if not Result then begin
    // latitude outside UTM limits
    Exit;
  end;

  AZone := wgs84_long_to_utm_zone(ALon);
  ALatBand := CMgrsLatBands[1 + Floor(ALat / 8 + 10)];

  // adjust zone for Norway
  if ALatBand = 'V' then begin
    if ( (AZone = 31) and (ALon >= 3) ) then Inc(AZone);
  end;

  // adjust zone for Svalbard
  if ALatBand = 'X' then begin
    if ( (AZone = 32) and (ALon <  9)  ) then Dec(AZone);
    if ( (AZone = 32) and (ALon >= 9)  ) then Inc(AZone);
    if ( (AZone = 34) and (ALon <  21) ) then Dec(AZone);
    if ( (AZone = 34) and (ALon >= 21) ) then Inc(AZone);
    if ( (AZone = 36) and (ALon <  33) ) then Dec(AZone);
    if ( (AZone = 36) and (ALon >= 33) ) then Inc(AZone);
  end;
end;

function utm_zone_to_wgs84_long(const AZone: Integer): Double;
begin
  Result := -180 + (AZone - 1) * 6;
end;

function utm_zone_to_wgs84_long(const AZone: Integer; const ALatBand: Char): Double;
begin
  Result := utm_zone_to_wgs84_long(AZone);

  if (ALatBand = 'V') and (AZone = 32) then begin
    Result := Result - 3;
  end;

  if ALatBand = 'X' then begin
    // todo
  end;
end;

function get_utm_init(const AZone: Integer; const ALatBand: Char): string;
begin
  Assert(Pos(UpperCase(ALatBand), CMgrsLatBands) > 0);

  Result := get_utm_init(AZone, Pos(UpperCase(ALatBand), CMgrsNorthLatBands) > 0);
end;

function get_utm_init(const AZone: Integer; const AIsNorth: Boolean): string;
begin
  if AIsNorth then begin
    Result := Format(utm_north_fmt, [AZone]);
  end else begin
    Result := Format(utm_south_fmt, [AZone]);
  end;
end;

function geodetic_wgs84_to_utm(const ALon, ALat: Double; out AX, AY: Double): Boolean;
var
  VZone: Integer;
  VLatBand: Char;
  VInitStr: AnsiString;
begin
  Result := wgs84_longlat_to_utm_zone(ALon, ALat, VZone, VLatBand);
  if Result then begin
    VInitStr := AnsiString(get_utm_init(VZone, VLatBand));
    Result := geodetic_cs_to_projected_cs(wgs_84, VInitStr, ALon, ALat, AX, AY);
  end;
end;

function utm_to_wgs84(const AX, AY: Double; const AZone: Integer; const AIsNorth: Boolean;
  out ALon, ALat: Double): Boolean;
var
  VInitStr: AnsiString;
begin
  VInitStr := AnsiString(get_utm_init(AZone, AIsNorth));
  Result := projected_cs_to_geodetic_cs(VInitStr, wgs_84, AX, AY, ALon, ALat);
end;

function geodetic_wgs84_to_ups(const ALon, ALat: Double; out AX, AY: Double): Boolean;
var
  VInitStr: AnsiString;
begin
  if ALat >= 84 then begin
    VInitStr := ups_north;
  end else
  if ALat <= 80 then begin
    VInitStr := ups_south;
  end else begin
    Result := False;
    Exit;
  end;
  Result := geodetic_cs_to_projected_cs(wgs_84, VInitStr, ALon, ALat, AX, AY);
end;

function ups_to_wgs84(const AX, AY: Double; const AIsNorth: Boolean; out ALon, ALat: Double): Boolean;
var
  VInitStr: AnsiString;
begin
  if AIsNorth then begin
    VInitStr := ups_north;
  end else begin
    VInitStr := ups_south;
  end;
  Result := projected_cs_to_geodetic_cs(VInitStr, wgs_84, AX, AY, ALon, ALat);
end;

end.

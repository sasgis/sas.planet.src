unit Proj4.UTM;

interface

type
  TUtmCoord = record
    Zone: Integer;
    Band: Char;
    IsNorth: Boolean;
    X: Double; // Easting
    Y: Double; // Northing
  end;

  TMgrsCoord = record
    Zone: Integer;
    Band: Char;
    Digraph: array[0..1] of Char;
    X: Integer; // Easting
    Y: Integer; // Northing
  end;

function wgs84_long_to_utm_zone(const ALon: Double): Integer; inline;
function wgs84_longlat_to_utm_zone(const ALon, ALat: Double; out AZone: Integer; out ALatBand: Char): Boolean;

function utm_zone_to_wgs84_long(const AZone: Integer): Double; inline; overload;
function utm_zone_to_wgs84_long(const AZone: Integer; const ALatBand: Char): Double; overload;

function get_utm_init(const AZone: Integer; const ALatBand: Char): AnsiString; inline; overload;
function get_utm_init(const AZone: Integer; const AIsNorth: Boolean): AnsiString; inline; overload;

function geodetic_wgs84_to_utm(const ALon, ALat: Double; out AUtm: TUtmCoord): Boolean;
function utm_to_wgs84(const AUtm: TUtmCoord; out ALon, ALat: Double): Boolean;

function geodetic_wgs84_to_ups(const ALon, ALat: Double; out AUtm: TUtmCoord): Boolean;
function ups_to_wgs84(const AUtm: TUtmCoord; out ALon, ALat: Double): Boolean;

function geodetic_wgs84_to_mgrs(const ALon, ALat: Double; out AMgrs: TMgrsCoord): Boolean;

function mgrs_to_utm(const AMgrs: TMgrsCoord; out AUtm: TUtmCoord): Boolean;
function mgrs_to_wgs84(const AMgrs: TMgrsCoord; out ALon, ALat: Double): Boolean; inline;

function str_to_mgrs(const AStr: string; out AMgrs: TMgrsCoord): Boolean;

const
  CMgrsLatBands = 'CDEFGHJKLMNPQRSTUVWXX'; // X is repeated for 80-84N

  CMgrsUtmCols: array [0..2] of string = (
    'ABCDEFGH', 'JKLMNPQR', 'STUVWXYZ'
  );

  CMgrsUtmRows: array [0..1] of string = (
    'ABCDEFGHJKLMNPQRSTUV', 'FGHJKLMNPQRSTUVABCDE'
  );

  CMgrsUpsCols: array [0..3] of string = (
    'JKLPQRSTUXYZZ', 'ABCFGHJKLPQR', 'JKLPQRSTUXYZZ', 'ABCFGHJ'
  );

  CMgrsUpsRows: array [0..3] of string = (
    'ABCDEFGHJKLMNPQRSTUVWXYZ', 'ABCDEFGHJKLMNPQRSTUVWXYZ', 'ABCDEFGHJKLMNP', 'ABCDEFGHJKLMNP'
  );

implementation

uses
  Math,
  SysUtils,
  Proj4.Utils,
  Proj4.Defines;

const
  CFeUps: array[0..3] of Integer = (8, 20, 8, 20);
  CFnUps: array[0..3] of Integer = (8, 8, 13, 13);

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

function get_utm_init(const AZone: Integer; const ALatBand: Char): AnsiString;
begin
  Assert(Pos(UpperCase(ALatBand), CMgrsLatBands) > 0);

  Result := get_utm_init(AZone, UpperCase(ALatBand) >= 'N');
end;

function get_utm_init(const AZone: Integer; const AIsNorth: Boolean): AnsiString;
begin
  if AIsNorth then begin
    Result := AnsiString(Format(utm_north_fmt, [AZone]));
  end else begin
    Result := AnsiString(Format(utm_south_fmt, [AZone]));
  end;
end;

function geodetic_wgs84_to_utm(const ALon, ALat: Double; out AUtm: TUtmCoord): Boolean;
var
  VInitStr: AnsiString;
begin
  Result := wgs84_longlat_to_utm_zone(ALon, ALat, AUtm.Zone, AUtm.Band);
  if Result then begin
    AUtm.IsNorth := AUtm.Band >= 'N';
    VInitStr := get_utm_init(AUtm.Zone, AUtm.IsNorth);
    Result := geodetic_cs_to_projected_cs(wgs_84, VInitStr, ALon, ALat, AUtm.X, AUtm.Y);
  end;
end;

function utm_to_wgs84(const AUtm: TUtmCoord; out ALon, ALat: Double): Boolean;
var
  VInitStr: AnsiString;
begin
  if AUtm.Zone in [1..60] then begin
    VInitStr := get_utm_init(AUtm.Zone, AUtm.IsNorth);
    Result := projected_cs_to_geodetic_cs(VInitStr, wgs_84, AUtm.X, AUtm.Y, ALon, ALat);
  end else
  if AUtm.Zone = 0 then begin
    Result := ups_to_wgs84(AUtm, ALon, ALat);
  end else begin
    Result := False;
  end;
end;

function geodetic_wgs84_to_ups(const ALon, ALat: Double; out AUtm: TUtmCoord): Boolean;
var
  VInitStr: AnsiString;
begin
  AUtm.Zone := 0;

  if ALat >= 84 then begin
    VInitStr := ups_north;
    AUtm.IsNorth := True;
  end else
  if ALat <= 80 then begin
    VInitStr := ups_south;
    AUtm.IsNorth := False;
  end else begin
    Result := False;
    Exit;
  end;

  Result := geodetic_cs_to_projected_cs(wgs_84, VInitStr, ALon, ALat, AUtm.X, AUtm.Y);

  if Result then begin
    if AUtm.IsNorth then begin
      if ALon < 0 then begin
        AUtm.Band := 'Y';
      end else begin
        AUtm.Band := 'Z';
      end;
    end else begin
      if ALon < 0 then begin
        AUtm.Band := 'A';
      end else begin
        AUtm.Band := 'B';
      end;
    end;
  end;
end;

function ups_to_wgs84(const AUtm: TUtmCoord; out ALon, ALat: Double): Boolean;
var
  VInitStr: AnsiString;
begin
  if AUtm.IsNorth then begin
    VInitStr := ups_north;
  end else begin
    VInitStr := ups_south;
  end;
  Result := projected_cs_to_geodetic_cs(VInitStr, wgs_84, AUtm.X, AUtm.Y, ALon, ALat);
end;

function geodetic_wgs84_to_mgrs(const ALon, ALat: Double; out AMgrs: TMgrsCoord): Boolean;
var
  I: Integer;
  VUtm: TUtmCoord;
  VCol, VRow: Integer;
begin
  Result :=
    geodetic_wgs84_to_utm(ALon, ALat, VUtm) or
    geodetic_wgs84_to_ups(ALon, ALat, VUtm);

  if not Result then begin
    Exit;
  end;

  AMgrs.Zone := VUtm.Zone;
  AMgrs.Band := VUtm.Band;

  VCol := Floor(VUtm.X / 100000);
  VRow := Floor(VUtm.Y / 100000);

  if AMgrs.Zone > 0 then begin
    I := AMgrs.Zone - 1;

    AMgrs.Digraph[0] := CMgrsUtmCols[I mod 3][1 + (VCol-1)];
    AMgrs.Digraph[1] := CMgrsUtmRows[I mod 2][1 + (VRow mod Length(CMgrsUtmRows[0]))];
  end else begin
    case AMgrs.Band of
      'A': I := 0;
      'B': I := 1;
      'Y': I := 2;
      'Z': I := 3;
    else
      Result := False;
      Exit;
    end;

    AMgrs.Digraph[0] := CMgrsUpsCols[I][1 + (VCol - CFeUps[I])];
    AMgrs.Digraph[1] := CMgrsUpsRows[I][1 + (VRow - CFnUps[I])];
  end;

  AMgrs.X := Trunc(VUtm.X) mod 100000;
  AMgrs.Y := Trunc(VUtm.Y) mod 100000;
end;

function mgrs_to_utm(const AMgrs: TMgrsCoord; out AUtm: TUtmCoord): Boolean;
var
  I: Integer;
  VPos: Integer;
  VUtmTmp: TUtmCoord;
  VLatMax: Integer;
  e, n, nBand, n2M: Integer;
begin
  Result := False;

  AUtm.Zone := AMgrs.Zone;
  AUtm.Band := AMgrs.Band;

  n2M := 0;

  if AUtm.Zone > 0 then begin
    AUtm.IsNorth := AMgrs.Band >= 'N';

    I := AMgrs.Zone - 1;

    VPos := Pos(AMgrs.Digraph[0], CMgrsUtmCols[I mod 3]);
    if VPos > 0 then begin
      Dec(VPos);
      e := (VPos + 1) * 100000;
    end else begin
      Exit;
    end;

    VPos := Pos(AMgrs.Digraph[1], CMgrsUtmRows[I mod 2]);
    if VPos > 0 then begin
      Dec(VPos);
      n := VPos * 100000;
    end else begin
      Exit;
    end;

    VPos := Pos(AMgrs.Band, CMgrsLatBands);
    if VPos > 0 then begin
      Dec(VPos);
      VLatMax := (VPos - 10) * 8;
    end else begin
      Exit;
    end;

    if not geodetic_wgs84_to_utm(3, VLatMax, VUtmTmp) then begin
      Exit;
    end;

    nBand := Trunc(VUtmTmp.Y / 100000) * 100000;

    while n2M + n + AMgrs.Y < nBand do begin
      Inc(n2M, 2000000);
    end;

  end else begin
    AUtm.IsNorth := AnsiChar(AMgrs.Band) in ['Y', 'Z'];

    case AMgrs.Band of
      'A': I := 0;
      'B': I := 1;
      'Y': I := 2;
      'Z': I := 3;
    else
      Exit;
    end;

    VPos := Pos(AMgrs.Digraph[0], CMgrsUpsCols[I]);
    if VPos > 0 then begin
      Dec(VPos);
      e := (VPos + CFeUps[I]) * 100000;
    end else begin
      Exit;
    end;

    VPos := Pos(AMgrs.Digraph[1], CMgrsUpsRows[I]);
    if VPos > 0 then begin
      Dec(VPos);
      n := (VPos + CFnUps[I]) * 100000;
    end else begin
      Exit;
    end;
  end;

  AUtm.X := e + AMgrs.X;
  AUtm.Y := n2M + n + AMgrs.Y;

  Result := True;
end;

function mgrs_to_wgs84(const AMgrs: TMgrsCoord; out ALon, ALat: Double): Boolean;
var
  VUtm: TUtmCoord;
begin
  Result := mgrs_to_utm(AMgrs, VUtm);
  if Result then begin
    if VUtm.Zone > 0 then begin
      Result := utm_to_wgs84(VUtm, ALon, ALat);
    end else begin
      Result := ups_to_wgs84(VUtm, ALon, ALat);
    end;
  end;
end;

function str_to_mgrs(const AStr: string; out AMgrs: TMgrsCoord): Boolean;
var
  P: PChar;
  I: Integer;
  VStr: string;
  VTmp: string;
  VLen: Integer;
begin
  Result := False;

  VStr := StringReplace(UpperCase(AStr), ' ', '', [rfReplaceAll]);
  VLen := Length(VStr);

  if VLen < 13 then begin
    Exit;
  end;

  I := 0;
  P := Pointer(VStr);
  while AnsiChar(P^) in ['0'..'9'] do begin
    Inc(P);
    Inc(I);
  end;

  if I > 2 then begin
    Exit;
  end;

  if I > 0 then begin
    AMgrs.Zone := StrToInt( Copy(VStr, 1, I) );
    if not (AMgrs.Zone in [1..60]) then begin
      Exit;
    end;
  end else begin
    AMgrs.Zone := 0;
  end;

  AMgrs.Band := P^;
  Inc(P);

  AMgrs.Digraph[0] := P^;
  Inc(P);

  AMgrs.Digraph[1] := P^;
  Inc(P);

  while P^ <> #0 do begin
    if AnsiChar(P^) in ['0'..'9'] then begin
      Inc(P);
    end else begin
      Exit;
    end;
  end;

  Inc(I, 3);
  VTmp := Copy(VStr, 1+I, (VLen-I) div 2);
  AMgrs.X := StrToInt(VTmp);

  Inc(I, Length(VTmp));
  VTmp := Copy(VStr, 1+I);
  AMgrs.Y := StrToInt(VTmp);

  Inc(I, Length(VTmp));
  Result := I = VLen;
end;

end.

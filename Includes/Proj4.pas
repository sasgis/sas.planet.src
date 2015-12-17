unit Proj4;

interface

{.$DEFINE STATIC_PROJ4}

const
  proj4_dll = 'proj480.dll';

  DEG_TO_RAD = 0.0174532925199432958;
  RAD_TO_DEG = 57.29577951308232;

type
  projUV = record
    U: Double;
    V: Double;
  end;

  projXY = projUV;
  projLP = projUV;
  projPJ = Pointer;

{$IFNDEF STATIC_PROJ4}
var
  pj_init_plus: function(const Args: PAnsiChar): projPJ; cdecl;
  pj_transform: function(const src, dst: projPJ; point_count: LongInt;
    point_offset: Integer; var X, Y, Z: Double): Integer; cdecl;
  pj_fwd: function(AProjLP: projLP; const AProjPJ: projPJ): projXY; cdecl;
  pj_inv: function(AProjXY: projXY; const AProjPJ: projPJ): projLP; cdecl;
  pj_free: function(AProjPJ: projPJ): Integer; cdecl;
  pj_strerrno: function(err_no: integer): PAnsiChar; cdecl;
{$ELSE}
  function pj_init_plus(const Args: PAnsiChar): projPJ; cdecl; external proj4_dll;
  function pj_transform(const src, dst: projPJ; point_count: LongInt;
    point_offset: Integer; var X, Y, Z: Double): Integer; cdecl; external proj4_dll;
  function pj_fwd(AProjLP: projLP; const AProjPJ: projPJ): projXY; cdecl; external proj4_dll;
  function pj_inv(AProjXY: projXY; const AProjPJ: projPJ): projLP; cdecl; external proj4_dll;
  function pj_free(AProjPJ: projPJ): Integer; cdecl; external proj4_dll;
  function pj_strerrno(err_no: integer): PAnsiChar; cdecl; external proj4_dll;
{$ENDIF}

function init_proj4_dll(
  const ALibName: string = proj4_dll;
  const ARaiseExceptions: Boolean = True
): Boolean;

const
  wgs84 = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs';

  sk42_towgs84 = '+towgs84=23.92,-141.27,-80.9,-0,0.35,0.82,-0.12';
  sk42 = '+proj=longlat +ellps=krass ' + sk42_towgs84 + ' +no_defs';

function get_sk42_gauss_kruger_init(const ALon, ALat: Double): AnsiString; overload;
function get_sk42_gauss_kruger_init(const AZone: Integer; const AIsNorth: Boolean): AnsiString; overload;

function long_to_gauss_kruger_zone(const ALon: Double): Integer; inline;

function geodetic_cs_to_cs(const ASrc, ADst: AnsiString; var ALon, ALat: Double): Boolean;

function geodetic_cs_to_projected_cs(
  const ASrc, ADst: AnsiString;
  const ALon, ALat: Double;
  out AX, AY: Double
): Boolean;

function projected_cs_to_geodetic_cs(
  const ASrc, ADst: AnsiString;
  const AX, AY: Double;
  out ALon, ALat: Double
): Boolean;

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
  Windows,
  {$IFNDEF STATIC_PROJ4}
  SyncObjs,
  {$ENDIF}
  Math,
  SysUtils;

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
  Result :=
    AnsiString(Format(
      '+proj=tmerc +lat_0=0 +lon_0=%d +k=1 +x_0=%d +y_0=%d +ellps=krass +units=m +no_defs',
      [lon_0, x_0, y_0]
    ));
end;

function geodetic_cs_to_cs(const ASrc, ADst: AnsiString; var ALon, ALat: Double): Boolean;
var
  ret: Integer;
  x, y, z: Double;
  src, dst: projPJ;
begin
  Result := False;
  if not init_proj4_dll then begin
    Exit;
  end;
  src := pj_init_plus(PAnsiChar(ASrc));
  if (src <> nil) then begin
    try
      dst := pj_init_plus(PAnsiChar(ADst));
      if dst <> nil then begin
        try
          x := ALon * DEG_TO_RAD;
          y := ALat * DEG_TO_RAD;
          z := 0;
          ret := pj_transform(src, dst, 1, 0, x, y, z);
          if ret = 0 then begin
            ALon := x * RAD_TO_DEG;
            ALat := y * RAD_TO_DEG;
            Result := True;
          end else begin
            OutputDebugStringA(pj_strerrno(ret));
          end;
        finally
          pj_free(dst);
        end;
      end;
    finally
      pj_free(src);
    end;
  end;
end;

function geodetic_wgs84_to_sk42(var ALon, ALat: Double): Boolean;
begin
  Result := geodetic_cs_to_cs(wgs84, sk42, ALon, ALat);
end;

function geodetic_sk42_to_wgs84(var ALon, ALat: Double): Boolean;
begin
  Result := geodetic_cs_to_cs(sk42, wgs84, ALon, ALat);
end;

function geodetic_cs_to_projected_cs(
  const ASrc, ADst: AnsiString;
  const ALon, ALat: Double;
  out AX, AY: Double
): Boolean;
var
  ret: Integer;
  x, y, z: Double;
  src, dst: projPJ;
begin
  Result := False;
  if not init_proj4_dll then begin
    Exit;
  end;
  src := pj_init_plus(PAnsiChar(ASrc));
  if (src <> nil) then begin
    try
      dst := pj_init_plus(PAnsiChar(ADst));
      if dst <> nil then begin
        try
          x := ALon * DEG_TO_RAD;
          y := ALat * DEG_TO_RAD;
          z := 0;
          ret := pj_transform(src, dst, 1, 0, x, y, z);
          if ret = 0 then begin
            AX := x;
            AY := y;
            Result := True;
          end else begin
            OutputDebugStringA(pj_strerrno(ret));
          end;
        finally
          pj_free(dst);
        end;
      end;
    finally
      pj_free(src);
    end;
  end;
end;

function geodetic_sk42_to_gauss_kruger(const ALon, ALat: Double; out AX, AY: Double): Boolean;
var
  gk_sk42: AnsiString;
begin
  gk_sk42 := get_sk42_gauss_kruger_init(ALon, ALat);
  Result := geodetic_cs_to_projected_cs(sk42, gk_sk42, ALon, ALat, AX, AY);
end;

function geodetic_wgs84_to_gauss_kruger(const ALon, ALat: Double; out AX, AY: Double): Boolean;
var
  long_sk42, lat_sk42: Double;
  gk_sk42: AnsiString;
begin
  long_sk42 := ALon;
  lat_sk42 := ALat;
  Result := geodetic_cs_to_cs(wgs84, sk42, long_sk42, lat_sk42);
  if Result then begin
    gk_sk42 := get_sk42_gauss_kruger_init(long_sk42, lat_sk42);
    Result := geodetic_cs_to_projected_cs(sk42, gk_sk42, long_sk42, lat_sk42, AX, AY);
  end;
end;

function projected_cs_to_geodetic_cs(
  const ASrc, ADst: AnsiString;
  const AX, AY: Double;
  out ALon, ALat: Double
): Boolean;
var
  ret: Integer;
  x, y, z: Double;
  src, dst: projPJ;
begin
  Result := False;
  if not init_proj4_dll then begin
    Exit;
  end;
  src := pj_init_plus(PAnsiChar(ASrc));
  if (src <> nil) then begin
    try
      dst := pj_init_plus(PAnsiChar(ADst));
      if dst <> nil then begin
        try
          x := AX;
          y := AY;
          z := 0;
          ret := pj_transform(src, dst, 1, 0, x, y, z);
          if ret = 0 then begin
            ALon := x * RAD_TO_DEG;
            ALat := y * RAD_TO_DEG;
            Result := True;
          end else begin
            OutputDebugStringA(pj_strerrno(ret));
          end;
        finally
          pj_free(dst);
        end;
      end;
    finally
      pj_free(src);
    end;
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
    Result := geodetic_cs_to_cs(sk42, wgs84, ALon, ALat);
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
  Result := projected_cs_to_geodetic_cs(gk_sk42, sk42, AX, AY, ALon, ALat);
end;

{$IFDEF STATIC_PROJ4}

function init_proj4_dll(const ALibName: string; const ARaiseExceptions: Boolean): Boolean;
begin
  Result := True;
end;

{$ELSE}

var
  gHandle: THandle = 0;
  gLock: TCriticalSection = nil;
  gIsInitialized: Boolean = False;

function init_proj4_dll(const ALibName: string; const ARaiseExceptions: Boolean): Boolean;

  function GetProcAddr(Name: PAnsiChar): Pointer;
  begin
    Result := GetProcAddress(gHandle, Name);
    if ARaiseExceptions and (Result = nil) then begin
      RaiseLastOSError;
    end;
  end;

begin
  if gIsInitialized then begin
    Result := True;
    Exit;
  end;

  gLock.Acquire;
  try
    if gIsInitialized then begin
      Result := True;
      Exit;
    end;

    if gHandle = 0 then begin
      gHandle := LoadLibrary(PChar(ALibName));
      if ARaiseExceptions and (gHandle = 0) then begin
        RaiseLastOSError;
      end;
    end;

    if gHandle <> 0 then begin
      pj_init_plus := GetProcAddr('pj_init_plus');
      pj_transform := GetProcAddr('pj_transform');
      pj_fwd := GetProcAddr('pj_fwd');
      pj_inv := GetProcAddr('pj_inv');
      pj_free := GetProcAddr('pj_free');
      pj_strerrno := GetProcAddr('pj_strerrno');
    end;

    gIsInitialized :=
      (gHandle <> 0) and
      (Addr(pj_init_plus) <> nil) and
      (Addr(pj_transform) <> nil) and
      (Addr(pj_fwd) <> nil) and
      (Addr(pj_inv) <> nil) and
      (Addr(pj_strerrno) <> nil) and
      (Addr(pj_free) <> nil);

    Result := gIsInitialized;
  finally
    gLock.Release;
  end;
end;

procedure free_proj4_dll;
begin
  gLock.Acquire;
  try
    gIsInitialized := False;

    if gHandle <> 0 then begin
      FreeLibrary(gHandle);
      gHandle := 0;
    end;

    pj_init_plus := nil;
    pj_transform := nil;
    pj_fwd := nil;
    pj_inv := nil;
    pj_free := nil;
    pj_strerrno := nil;
  finally
    gLock.Release;
  end;
end;

initialization
  gLock := TCriticalSection.Create;

finalization
  free_proj4_dll;
  FreeAndNil(gLock);

{$ENDIF}

end.

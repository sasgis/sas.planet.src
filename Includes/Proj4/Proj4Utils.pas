unit Proj4Utils;

interface

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

implementation

uses
  Windows,
  Math,
  SysUtils,
  Proj4;

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

end.

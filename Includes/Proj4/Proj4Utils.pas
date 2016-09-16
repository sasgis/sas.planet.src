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
  ctx: projCtx;
  err: Integer;
  x, y, z: Double;
  src, dst: projPJ;
begin
  Result := False;

  if not init_proj4_dll then begin
    Exit;
  end;

  ctx := pj_ctx_alloc();
  if ctx = nil then begin
    Exit;
  end;

  try
    src := pj_init_plus_ctx(ctx, PAnsiChar(ASrc));
    if src = nil then begin
      Exit;
    end;

    try
      dst := pj_init_plus_ctx(ctx, PAnsiChar(ADst));
      if dst = nil then begin
        Exit;
      end;

      try
        x := ALon * DEG_TO_RAD;
        y := ALat * DEG_TO_RAD;
        z := 0;

        err := pj_transform(src, dst, 1, 0, x, y, z);
        if err <> 0 then begin
          OutputDebugStringA(pj_strerrno(err));
          Exit;
        end;

        ALon := x * RAD_TO_DEG;
        ALat := y * RAD_TO_DEG;
        Result := True;
      finally
        pj_free(dst);
      end;
    finally
      pj_free(src);
    end;
  finally
    pj_ctx_free(ctx);
  end;
end;

function geodetic_cs_to_projected_cs(
  const ASrc, ADst: AnsiString;
  const ALon, ALat: Double;
  out AX, AY: Double
): Boolean;
var
  ctx: projCtx;
  err: Integer;
  x, y, z: Double;
  src, dst: projPJ;
begin
  Result := False;

  if not init_proj4_dll then begin
    Exit;
  end;

  ctx := pj_ctx_alloc();
  if ctx = nil then begin
    Exit;
  end;

  try
    src := pj_init_plus_ctx(ctx, PAnsiChar(ASrc));
    if src = nil then begin
      Exit;
    end;

    try
      dst := pj_init_plus_ctx(ctx, PAnsiChar(ADst));
      if dst = nil then begin
        Exit;
      end;

      try
        x := ALon * DEG_TO_RAD;
        y := ALat * DEG_TO_RAD;
        z := 0;

        err := pj_transform(src, dst, 1, 0, x, y, z);
        if err <> 0 then begin
          OutputDebugStringA(pj_strerrno(err));
          Exit;
        end;

        AX := x;
        AY := y;
        Result := True;
      finally
        pj_free(dst);
      end;
    finally
      pj_free(src);
    end;
  finally
    pj_ctx_free(ctx);
  end;
end;

function projected_cs_to_geodetic_cs(
  const ASrc, ADst: AnsiString;
  const AX, AY: Double;
  out ALon, ALat: Double
): Boolean;
var
  ctx: projCtx;
  err: Integer;
  x, y, z: Double;
  src, dst: projPJ;
begin
  Result := False;

  if not init_proj4_dll then begin
    Exit;
  end;

  ctx := pj_ctx_alloc();
  if ctx = nil then begin
    Exit;
  end;

  try
    src := pj_init_plus_ctx(ctx, PAnsiChar(ASrc));
    if src = nil then begin
      Exit;
    end;

    try
      dst := pj_init_plus_ctx(ctx, PAnsiChar(ADst));
      if dst = nil then begin
        Exit;
      end;

      try
        x := AX;
        y := AY;
        z := 0;

        err := pj_transform(src, dst, 1, 0, x, y, z);
        if err <> 0 then begin
          OutputDebugStringA(pj_strerrno(err));
          Exit;
        end;

        ALon := x * RAD_TO_DEG;
        ALat := y * RAD_TO_DEG;
        Result := True;
      finally
        pj_free(dst);
      end;
    finally
      pj_free(src);
    end;
  finally
    pj_ctx_free(ctx);
  end;
end;

end.

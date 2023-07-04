unit Proj4.Utils;

interface

uses
  Proj4.API;

function geodetic_cs_to_cs(
  const ASrc, ADst: AnsiString;
  var ALon, ALat: Double;
  const AErrNo: PInteger = nil
): Boolean; overload;

function geodetic_cs_to_cs(
  const ASrc, ADst: projPJ;
  var ALon, ALat: Double;
  const AErrNo: PInteger = nil
): Boolean; overload; inline;

function geodetic_cs_to_projected_cs(
  const ASrc, ADst: AnsiString;
  const ALon, ALat: Double;
  out AX, AY: Double;
  const AErrNo: PInteger = nil
): Boolean; overload;

function geodetic_cs_to_projected_cs(
  const ASrc, ADst: projPJ;
  const ALon, ALat: Double;
  out AX, AY: Double;
  const AErrNo: PInteger = nil
): Boolean; overload; inline;

function projected_cs_to_geodetic_cs(
  const ASrc, ADst: AnsiString;
  const AX, AY: Double;
  out ALon, ALat: Double;
  const AErrNo: PInteger = nil
): Boolean; overload;

function projected_cs_to_geodetic_cs(
  const ASrc, ADst: projPJ;
  const AX, AY: Double;
  out ALon, ALat: Double;
  const AErrNo: PInteger = nil
): Boolean; overload; inline;

implementation

function geodetic_cs_to_cs(
  const ASrc, ADst: AnsiString;
  var ALon, ALat: Double;
  const AErrNo: PInteger
): Boolean;
var
  ctx: projCtx;
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
        Result := geodetic_cs_to_cs(src, dst, ALon, ALat, AErrNo);
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

function geodetic_cs_to_cs(
  const ASrc, ADst: projPJ;
  var ALon, ALat: Double;
  const AErrNo: PInteger
): Boolean;
var
  err: Integer;
  x, y, z: Double;
begin
  x := ALon * DEG_TO_RAD;
  y := ALat * DEG_TO_RAD;
  z := 0;

  err := pj_transform(ASrc, ADst, 1, 0, x, y, z);

  Result := (err = 0);

  if Result then begin
    ALon := x * RAD_TO_DEG;
    ALat := y * RAD_TO_DEG;
  end else
  if AErrNo <> nil then begin
    AErrNo^ := err;
  end;
end;

function geodetic_cs_to_projected_cs(
  const ASrc, ADst: AnsiString;
  const ALon, ALat: Double;
  out AX, AY: Double;
  const AErrNo: PInteger
): Boolean;
var
  ctx: projCtx;
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
        Result := geodetic_cs_to_projected_cs(src, dst, ALon, ALat, AX, AY, AErrNo);
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
  const ASrc, ADst: projPJ;
  const ALon, ALat: Double;
  out AX, AY: Double;
  const AErrNo: PInteger
): Boolean;
var
  err: Integer;
  x, y, z: Double;
begin
  x := ALon * DEG_TO_RAD;
  y := ALat * DEG_TO_RAD;
  z := 0;

  err := pj_transform(ASrc, ADst, 1, 0, x, y, z);

  Result := (err = 0);

  if Result then begin
    AX := x;
    AY := y;
  end else
  if AErrNo <> nil then begin
    AErrNo^ := err;
  end;
end;

function projected_cs_to_geodetic_cs(
  const ASrc, ADst: AnsiString;
  const AX, AY: Double;
  out ALon, ALat: Double;
  const AErrNo: PInteger
): Boolean;
var
  ctx: projCtx;
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
        Result := projected_cs_to_geodetic_cs(src, dst, AX, AY, ALon, ALat, AErrNo);
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
  const ASrc, ADst: projPJ;
  const AX, AY: Double;
  out ALon, ALat: Double;
  const AErrNo: PInteger
): Boolean;
var
  err: Integer;
  x, y, z: Double;
begin
  x := AX;
  y := AY;
  z := 0;

  err := pj_transform(ASrc, ADst, 1, 0, x, y, z);

  Result := (err = 0);

  if Result then begin
    ALon := x * RAD_TO_DEG;
    ALat := y * RAD_TO_DEG;
  end else
  if AErrNo <> nil then begin
    AErrNo^ := err;
  end;
end;

end.

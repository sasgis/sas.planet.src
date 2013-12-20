unit LibPngWriter;

interface

uses
  Classes,
  LibPng,
  LibPngIO;

type
  TGetLineCallBack = function(
    const ARowNumber: Integer;
    const ALineSize: Integer;
    const AUserInfo: Pointer
  ): Pointer of object;

  TLibPngWriter = class(TObject)
  private
    png_ptr: png_structp;
    info_ptr: png_infop;
    rw_io: png_rw_io_stream;
  private
    FWidth: Integer;
    FHeight: Integer;
    FBitsPerPixel: Integer;
    FPngColorType: Integer;
    FCompression: Integer;
    FPalette: Pointer;
    FPaletteSize: Integer;
    FGamma: Double;
    FGetLineCallBack: TGetLineCallBack;
    FUserInfo: Pointer;
  private
    procedure InitWriter(const AOutStream: TStream);
    procedure FinWriter;
    procedure SetGamma;
    procedure WriteRows;
    procedure WriteTrueColor;
    procedure WriteIndexedColor;
  public
    procedure Write(
      const AOutputStream: TStream;
      const AWidth, AHeight: Integer;           // max 65536x65536 pix
      const ABitsPerPixel: Integer;             // 8, 24, 32 bits
      const AGetLineCallBack: TGetLineCallBack;
      const AUserInfo: Pointer = nil;
      const ACompression: Integer = 2;          // 0..9 (0 = no compression)
      const APalette: Pointer = nil;
      const APaletteSize: Integer = 0;          // 1..256 (0 = no palette)
      const AGamma: Double = 0.45455            // 0..1
    );
    constructor Create;
  end;

implementation

uses
  SysUtils;

type
  ELibPngWriterError = class(Exception);

{ TLibPngWriter }

constructor TLibPngWriter.Create;
begin
  inherited Create;
  if not InitLibPng then begin
    raise ELibPngWriterError.CreateFmt('Initialization of %s failed', [libpng_dll]);
  end;
end;

procedure TLibPngWriter.InitWriter(const AOutStream: TStream);
begin
  png_ptr := png_create_write_struct(PNG_LIBPNG_VER_STRING, nil, nil, nil);
  if not Assigned(png_ptr) then begin
    raise ELibPngWriterError.Create('LIBPNG_INIT_ERROR'); // out of memory
  end;

  info_ptr := png_create_info_struct(png_ptr);
  if not Assigned(info_ptr) then begin
    png_destroy_write_struct(@png_ptr, nil);
    raise ELibPngWriterError.Create('LIBPNG_INIT_ERROR'); // out of memory
  end;

  rw_io.DestStream := AOutStream;
  rw_io.DestBufferSize := cIOBufferSize;
  GetMem(rw_io.DestBuffer, rw_io.DestBufferSize);
  rw_io.BufferedDataSize := 0;

  png_set_write_fn(png_ptr, @rw_io, @lib_png_write_data_callback, nil);

  png_set_compression_level(@png_ptr, FCompression);
end;

procedure TLibPngWriter.FinWriter;
begin
  png_free_data(png_ptr, info_ptr, PNG_FREE_ALL);
  png_destroy_write_struct(@png_ptr, @info_ptr);

  png_ptr := nil;
  info_ptr := nil;

  FlashBuffer(@rw_io);
  FreeMem(rw_io.DestBuffer);

  FillChar(rw_io, SizeOf(rw_io), 0);
end;

procedure TLibPngWriter.SetGamma;
begin
  if FGamma > 0.0 then begin
    png_set_gAMA(png_ptr, info_ptr, FGamma);
    if (FGamma > 0.45454) and (FGamma < 0.45456) then begin
      png_set_sRGB(png_ptr, info_ptr, 0); // 0 = Perceptual
    end;
  end;
end;

procedure TLibPngWriter.WriteRows;
var
  I: Integer;
  VLine: Pointer;
  VLineSize: Integer;
begin
  png_write_info(png_ptr, info_ptr);
  try
    png_set_packing(png_ptr);

    VLineSize := Integer(info_ptr.width) * FBitsPerPixel;
    Assert(VLineSize > 0);

    for I := 0 to info_ptr.height - 1 do begin
      VLine := FGetLineCallBack(I, VLineSize, FUserInfo);
      if VLine <> nil then begin
        png_write_row(png_ptr, VLine);
      end else begin
        Break;
      end;
    end;
  finally
    png_write_end(png_ptr, info_ptr);
  end;
end;

procedure TLibPngWriter.WriteTrueColor;
begin
  png_set_IHDR(png_ptr, info_ptr, FWidth, FHeight, 8, FPngColorType,
    PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_BASE);

  SetGamma;

  WriteRows;
end;

procedure TLibPngWriter.WriteIndexedColor;
var
  I: Integer;
  VNumTrans: Integer;
  VSampleDepth: Integer;
  VPtr: PByte;
  VTrans: array [0..255] of Byte;
  VPal: array [0..255] of png_color;
begin
  // Palette images generally don't gain anything from filtering
  png_set_filter(png_ptr, PNG_FILTER_TYPE_BASE, PNG_FILTER_VALUE_NONE);

  SetGamma;

  // set the image parameters appropriately
  if FPaletteSize <= 2 then begin
    VSampleDepth := 1;
  end else if FPaletteSize <= 4 then begin
    VSampleDepth := 2;
  end else if FPaletteSize <= 16 then begin
    VSampleDepth := 4;
  end else begin
    VSampleDepth := 8;
  end;

  png_set_IHDR(png_ptr, info_ptr, FWidth, FHeight, VSampleDepth, FPngColorType,
    PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_BASE);

  VPtr := FPalette;
  VNumTrans := 0;

  for I := 0 to FPaletteSize - 1 do begin
    VPal[I].blue := VPtr^;
    Inc(VPtr);

    VPal[I].green := VPtr^;
    Inc(VPtr);

    VPal[I].red := VPtr^;
    Inc(VPtr);

    VTrans[VNumTrans] := VPtr^;
    if VTrans[VNumTrans] < $FF then begin
      Inc(VNumTrans);
    end;
    Inc(VPtr);
  end;

  png_set_PLTE(png_ptr, info_ptr, @VPal[0], FPaletteSize);

  if VNumTrans > 0 then begin
    png_set_tRNS(png_ptr, info_ptr, @VTrans[0], VNumTrans, nil);
  end;

  WriteRows;
end;

procedure TLibPngWriter.Write(
  const AOutputStream: TStream;
  const AWidth, AHeight: Integer;
  const ABitsPerPixel: Integer;
  const AGetLineCallBack: TGetLineCallBack;
  const AUserInfo: Pointer;
  const ACompression: Integer;
  const APalette: Pointer;
  const APaletteSize: Integer;
  const AGamma: Double
);
begin
  Assert(Assigned(AOutputStream));
  Assert(Assigned(AGetLineCallBack));
  Assert(ABitsPerPixel in [8, 24, 32]);
  Assert( (ACompression >= 0) and (ACompression <= 9) );
  Assert( (AGamma >= 0.0) and (AGamma <= 1.0) );

  FWidth := AWidth;
  FHeight := AHeight;
  FBitsPerPixel := ABitsPerPixel;
  FGetLineCallBack := AGetLineCallBack;
  FUserInfo := AUserInfo;
  FCompression := ACompression;
  FPalette := APalette;
  FPaletteSize := APaletteSize;
  FGamma := AGamma;

  case FBitsPerPixel of
    8: begin
        Assert(FPalette <> nil);
        Assert(FPaletteSize > 0);
        FPngColorType := PNG_COLOR_TYPE_PALETTE;
    end;
    24: FPngColorType := PNG_COLOR_TYPE_RGB;
  else
    FPngColorType := PNG_COLOR_TYPE_RGB_ALPHA;
  end;

  InitWriter(AOutputStream);
  try
    if FPngColorType in [PNG_COLOR_TYPE_RGB, PNG_COLOR_TYPE_RGB_ALPHA] then begin
      WriteTrueColor;
    end else if FPngColorType in [PNG_COLOR_TYPE_PALETTE] then begin
      WriteIndexedColor;
    end;
  finally
    FinWriter;
  end;
end;

end.

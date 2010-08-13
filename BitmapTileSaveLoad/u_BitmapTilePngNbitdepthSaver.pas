unit u_BitmapTilePngNBitdepthSaver;

interface

uses
  Classes,
  GR32,
  i_BitmapTileSaveLoad,
  Windows;

type
  TPngBitmapTileNBitdepthSaver = class(TInterfacedObject, IBitmapTileSaver)
  private
    FCompressionQuality: byte;
    Fbitdepth: byte;
  public
    constructor create(ACompressionQuality, Abitdepth: byte); overload;
    procedure SaveToFile(ABtm: TCustomBitmap32; AFileName: string);
    procedure SaveToStream(ABtm: TCustomBitmap32; AStream: TStream);
  end;

implementation

uses
  Graphics,
  GIFImage,
  pngimage,
  u_GlobalState;

{ TPngBitmapTileSaver }

constructor TPngBitmapTileNBitdepthSaver.create(ACompressionQuality, Abitdepth: byte);
begin
  FCompressionQuality := ACompressionQuality;
  Fbitdepth := Abitdepth;
end;

procedure TPngBitmapTileNBitdepthSaver.SaveToFile(ABtm: TCustomBitmap32; AFileName: string);
var
  VPng_ex: TPNGObject;
  VBtm_ex, bmp8b: TBitmap;
  BtmRGB, PngRGB: PByte;
  j: integer;
begin
  VBtm_ex := TBitmap.Create;
  bmp8b := TBitmap.Create;
  try
    VBtm_ex.Assign(Abtm as TCustomBitmap32);
    VPng_ex := tpngobject.createblank(COLOR_PALETTE, Fbitdepth, VBtm_ex.Width, VBtm_ex.Height);
    VPng_ex.CompressionLevel := FCompressionQuality;
    bmp8b := ReduceColors(VBtm_ex, rmQuantize, dmNearest, Fbitdepth, VBtm_ex.Palette);
    try
      BtmRGB := bmp8b.Scanline[0];
      PngRGB := VPng_ex.Scanline[0];
      for j := 0 to VBtm_ex.height - 1 do begin
        CopyMemory(PngRGB, BtmRGB, bmp8b.Width);
        DEC(BtmRGB, bmp8b.Width);
        DEC(PngRGB, VPng_ex.Width);
      end;
      VPng_ex.Palette := bmp8b.Palette;
      VPng_ex.SaveToFile(AFileName);
    finally
      VPng_ex.Free;
    end;
  finally
    VBtm_ex.Free;
    bmp8b.Free;
  end;
end;

procedure TPngBitmapTileNBitdepthSaver.SaveToStream(ABtm: TCustomBitmap32; AStream: TStream);
var
  VPng_ex: TPNGObject;
  VBtm_ex, bmp8b: TBitmap;
  BtmRGB, PngRGB: PByte;
  j: integer;
begin
  VBtm_ex := TBitmap.Create;
  bmp8b := TBitmap.Create;
  try
    VBtm_ex.Assign(Abtm as TCustomBitmap32);
    VPng_ex := tpngobject.createblank(COLOR_PALETTE, Fbitdepth, VBtm_ex.Width, VBtm_ex.Height);
    VPng_ex.CompressionLevel := FCompressionQuality;
    VPng_ex.AddtEXt('src', 'SAS.Planet' + SASVersion);
    bmp8b := ReduceColors(VBtm_ex, rmQuantize, dmNearest, Fbitdepth, VBtm_ex.Palette);
    try
      BtmRGB := bmp8b.Scanline[0];
      PngRGB := VPng_ex.Scanline[0];
      for j := 0 to VBtm_ex.height - 1 do begin
        CopyMemory(PngRGB, BtmRGB, bmp8b.Width);
        DEC(BtmRGB, bmp8b.Width);
        DEC(PngRGB, VPng_ex.Width);
      end;
      VPng_ex.Palette := bmp8b.Palette;
      VPng_ex.SaveToStream(AStream);
    finally
      VPng_ex.Free;
    end;
  finally
    VBtm_ex.Free;
    bmp8b.Free;
  end;
end;

end.

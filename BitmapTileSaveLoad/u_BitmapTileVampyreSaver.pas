unit u_BitmapTileVampyreSaver;

interface

uses
  Classes,
  Imaging,
  ImagingTypes,
  GR32,
  i_BinaryData,
  i_Bitmap32Static,
  i_ARGBToPaletteConverter,
  i_BitmapTileSaveLoad;

type
  TVampyreBasicBitmapTileSaver = class(TInterfacedObject, IBitmapTileSaver)
  private
    FFormat: TImageFileFormat;
  protected
    procedure PrepareData(var AImage: TImageData); virtual;
  protected
    procedure SaveToStream(ABtm: TCustomBitmap32; AStream: TStream);
    function Save(ABitmap: IBitmap32Static): IBinaryData;
  public
    constructor Create(AFormat: TImageFileFormat);
    destructor Destroy; override;
  end;

  TVampyreBasicBitmapTileSaverPNG = class(TVampyreBasicBitmapTileSaver)
  public
    constructor Create(ACompressLevel: LongInt); overload;
    constructor Create; overload;
  end;

  TVampyreBasicBitmapTileSaverPNGRGB = class(TVampyreBasicBitmapTileSaverPNG)
  protected
    procedure PrepareData(var AImage: TImageData); override;
  end;

  TVampyreBasicBitmapTileSaverPNGPalette = class(TVampyreBasicBitmapTileSaverPNG)
  private
    FConverter: IARGBToPaletteConverter;
  protected
    procedure PrepareData(var AImage: TImageData); override;
  public
    constructor Create(
      AConverter: IARGBToPaletteConverter;
      ACompressLevel: LongInt
    );
  end;

  TVampyreBasicBitmapTileSaverGIF = class(TVampyreBasicBitmapTileSaver)
  public
    constructor Create;
  end;

  TVampyreBasicBitmapTileSaverBMP = class(TVampyreBasicBitmapTileSaver)
  public
    constructor Create;
  end;

  TVampyreBasicBitmapTileSaverJPG = class(TVampyreBasicBitmapTileSaver)
  public
    constructor Create(ACompressionQuality: Byte);
  end;

implementation

uses
  SysUtils,
  ImagingGraphics32,
  ImagingNetworkGraphics,
  ImagingJpeg,
  ImagingGif,
  ImagingBitmap,
  u_BinaryDataByMemStream;

{ TVampyreBasicBitmapTileSaver }

constructor TVampyreBasicBitmapTileSaver.Create(AFormat: TImageFileFormat);
begin
  FFormat := AFormat;
end;

destructor TVampyreBasicBitmapTileSaver.Destroy;
begin
  FreeAndNil(FFormat);
  inherited;
end;

procedure TVampyreBasicBitmapTileSaver.PrepareData(var AImage: TImageData);
begin
end;

function TVampyreBasicBitmapTileSaver.Save(
  ABitmap: IBitmap32Static
): IBinaryData;
var
  VFormat: TImageFileFormat;
  VImage: TImageData;
  IArray: TDynImageDataArray;
  VMemStream: TMemoryStream;
begin
  if FFormat <> nil then begin
    VFormat := FFormat;
  end else begin
    VFormat := FindImageFileFormatByExt('.bmp');
  end;
  if VFormat = nil then begin
    raise Exception.Create('Неизвестный формат файла');
  end;
  InitImage(VImage);
  try
    ConvertBitmap32ToImageData(ABitmap.Bitmap, VImage);
    PrepareData(VImage);
    SetLength(IArray, 1);
    IArray[0] := VImage;
    VMemStream := TMemoryStream.Create;
    try
      if not VFormat.SaveToStream(VMemStream, IArray, True) then begin
        raise Exception.Create('Ошибка записи файла');
      end;
      Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
      VMemStream := nil;
    finally
      VMemStream.Free;
    end;
  finally
    FreeImage(VImage);
  end;
end;

procedure TVampyreBasicBitmapTileSaver.SaveToStream(ABtm: TCustomBitmap32;
  AStream: TStream);
var
  VFormat: TImageFileFormat;
  VImage: TImageData;
  IArray: TDynImageDataArray;
begin
  if FFormat <> nil then begin
    VFormat := FFormat;
  end else begin
    VFormat := FindImageFileFormatByExt('.bmp');
  end;
  if VFormat = nil then begin
    raise Exception.Create('Неизвестный формат файла');
  end;
  InitImage(VImage);
  try
    ConvertBitmap32ToImageData(ABtm, VImage);
    PrepareData(VImage);
    SetLength(IArray, 1);
    IArray[0] := VImage;
    if not VFormat.SaveToStream(AStream, IArray, True) then begin
      raise Exception.Create('Ошибка записи файла');
    end;
  finally
    FreeImage(VImage);
  end;
end;

{ TVampyreBasicBitmapTileSaverPNG }

constructor TVampyreBasicBitmapTileSaverPNG.Create(ACompressLevel: LongInt);
var
  VFormat: TPNGFileFormat;
begin
  VFormat := TPNGFileFormat.Create();
  VFormat.CompressLevel := ACompressLevel;
  inherited Create(VFormat);
end;

constructor TVampyreBasicBitmapTileSaverPNG.Create;
var
  VFormat: TPNGFileFormat;
begin
  VFormat := TPNGFileFormat.Create();
  inherited Create(VFormat);
end;

{ TVampyreBasicBitmapTileSaverGIF }

constructor TVampyreBasicBitmapTileSaverGIF.Create;
var
  VFormat: TGIFFileFormat;
begin
  VFormat := TGIFFileFormat.Create();
  inherited Create(VFormat);
end;

{ TVampyreBasicBitmapTileSaverBMP }

constructor TVampyreBasicBitmapTileSaverBMP.Create;
var
  VFormat: TBitmapFileFormat;
begin
  VFormat := TBitmapFileFormat.Create();
  inherited Create(VFormat);
end;

{ TVampyreBasicBitmapTileSaverPNGRGB }

procedure TVampyreBasicBitmapTileSaverPNGRGB.PrepareData(var AImage: TImageData);
begin
  ConvertImage(AImage, ifR8G8B8);
end;

{ TVampyreBasicBitmapTileSaverPNGPalette }

constructor TVampyreBasicBitmapTileSaverPNGPalette.Create(
  AConverter: IARGBToPaletteConverter; ACompressLevel: Integer);
begin
  inherited Create(ACompressLevel);
  FConverter := AConverter;
end;

procedure TVampyreBasicBitmapTileSaverPNGPalette.PrepareData(
  var AImage: TImageData
);
begin
  FConverter.Convert(AImage);
end;

{ TVampyreBasicBitmapTileSaverJPG }

constructor TVampyreBasicBitmapTileSaverJPG.Create(ACompressionQuality: byte);
var
  VFormat: TJpegFileFormat;
begin
  VFormat := TJpegFileFormat.Create();
  VFormat.Quality := ACompressionQuality;
  inherited Create(VFormat);
end;

end.


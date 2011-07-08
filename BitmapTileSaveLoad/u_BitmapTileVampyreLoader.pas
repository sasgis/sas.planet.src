unit u_BitmapTileVampyreLoader;

interface

uses
  Classes,
  Imaging,
  GR32,
  i_BitmapTileSaveLoad;

type
  TVampyreBasicBitmapTileLoader = class(TInterfacedObject, IBitmapTileLoader)
  private
    FFormat: TImageFileFormat;
  public
    constructor Create(AFormat: TImageFileFormat);
    procedure LoadFromFile(const AFileName: string; ABtm: TCustomBitmap32);
    procedure LoadFromStream(AStream: TStream; ABtm: TCustomBitmap32);
  end;

  TVampyreBasicBitmapTileLoaderPNG = class(TVampyreBasicBitmapTileLoader)
  public
    constructor Create;
  end;

  TVampyreBasicBitmapTileLoaderGIF = class(TVampyreBasicBitmapTileLoader)
  public
    constructor Create;
  end;

  TVampyreBasicBitmapTileLoaderBMP = class(TVampyreBasicBitmapTileLoader)
  public
    constructor Create;
  end;

  TVampyreBasicBitmapTileLoaderJPEG = class(TVampyreBasicBitmapTileLoader)
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  ImagingTypes,
  ImagingGraphics32,
  ImagingJpeg,
  ImagingNetworkGraphics,
  ImagingGif,
  ImagingBitmap;

{ TVampyreBasicBitmapTileLoader }

constructor TVampyreBasicBitmapTileLoader.Create(AFormat: TImageFileFormat);
begin
  FFormat := AFormat;
end;

procedure TVampyreBasicBitmapTileLoader.LoadFromFile(
  const AFileName: string;
  ABtm: TCustomBitmap32
);
var
  VFormat: TImageFileFormat;
  VImage: TImageData;
  IArray: TDynImageDataArray;
  I: LongInt;
begin
  if FFormat <> nil then begin
    VFormat := FFormat;
  end else begin
    VFormat := FindImageFileFormatByExt(DetermineFileFormat(AFileName));
  end;
  if VFormat = nil then begin
    raise Exception.Create('Неизвестный формат файла');
  end;
  InitImage(VImage);
  try
    if not VFormat.LoadFromFile(AFileName, IArray, True) then begin
      raise Exception.Create('Ошибка загрузки файла');
    end;
    if Length(IArray) = 0 then begin
      raise Exception.Create('В файле не найдено изображений');
    end;
    VImage := IArray[0];
    for I := 1 to Length(IArray) - 1 do begin
      FreeImage(IArray[I]);
    end;
    ConvertImageDataToBitmap32(VImage, ABtm);
  finally
    FreeImage(VImage);
  end;
end;

procedure TVampyreBasicBitmapTileLoader.LoadFromStream(AStream: TStream;
  ABtm: TCustomBitmap32);
var
  VFormat: TImageFileFormat;
  VImage: TImageData;
  IArray: TDynImageDataArray;
  I: LongInt;
begin
  if FFormat <> nil then begin
    VFormat := FFormat;
  end else begin
    VFormat := FindImageFileFormatByExt(DetermineStreamFormat(AStream));
  end;
  if VFormat = nil then begin
    raise Exception.Create('Неизвестный формат файла');
  end;
  InitImage(VImage);
  try
    if not VFormat.LoadFromStream(AStream, IArray, True) then begin
      raise Exception.Create('Ошибка загрузки файла');
    end;
    if Length(IArray) = 0 then begin
      raise Exception.Create('В файле не найдено изображений');
    end;
    VImage := IArray[0];
    for I := 1 to Length(IArray) - 1 do begin
      FreeImage(IArray[I]);
    end;
    ConvertImageDataToBitmap32(VImage, ABtm);
  finally
    FreeImage(VImage);
  end;
end;

{ TVampyreBasicBitmapTileLoaderPNG }

constructor TVampyreBasicBitmapTileLoaderPNG.Create;
begin
  inherited Create(FindImageFileFormatByClass(TPNGFileFormat))
end;

{ TVampyreBasicBitmapTileLoaderGIF }

constructor TVampyreBasicBitmapTileLoaderGIF.Create;
begin
  inherited Create(FindImageFileFormatByClass(TGIFFileFormat))
end;

{ TVampyreBasicBitmapTileLoaderBMP }

constructor TVampyreBasicBitmapTileLoaderBMP.Create;
begin
  inherited Create(FindImageFileFormatByClass(TBitmapFileFormat))
end;

{ TVampyreBasicBitmapTileLoaderJPEG }

constructor TVampyreBasicBitmapTileLoaderJPEG.Create;
begin
  inherited Create(FindImageFileFormatByClass(TJpegFileFormat))
end;

end.


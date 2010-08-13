unit u_BitmapTileVampyreSaver;

interface

uses
  Classes,
  Imaging,
  GR32,
  i_BitmapTileSaveLoad;

type
  TVampyreBasicBitmapTileSaver = class(TInterfacedObject, IBitmapTileSaver)
  private
    FFormat: TImageFileFormat;
  public
    constructor Create(AFormat: TImageFileFormat);
    destructor Destroy; override;
    procedure SaveToFile(ABtm: TCustomBitmap32; AFileName: string);
    procedure SaveToStream(ABtm: TCustomBitmap32; AStream: TStream);
  end;

  TVampyreBasicBitmapTileSaverPNG = class(TVampyreBasicBitmapTileSaver)
  public
    constructor Create;
  end;

  TVampyreBasicBitmapTileSaverGIF = class(TVampyreBasicBitmapTileSaver)
  public
    constructor Create;
  end;

  TVampyreBasicBitmapTileSaverBMP = class(TVampyreBasicBitmapTileSaver)
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  ImagingTypes,
  ImagingGraphics32,
  ImagingNetworkGraphics,
  ImagingGif,
  ImagingBitmap;

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

procedure TVampyreBasicBitmapTileSaver.SaveToFile(ABtm: TCustomBitmap32;
  AFileName: string);
var
  VFormat: TImageFileFormat;
  VImage: TImageData;
  IArray: TDynImageDataArray;
begin
  if FFormat <> nil then begin
    VFormat := FFormat;
  end else begin
    VFormat := FindImageFileFormatByName(AFileName);
  end;
  if VFormat = nil then begin
    raise Exception.Create('Неизвестный формат файла');
  end;
  InitImage(VImage);
  try
    ConvertBitmap32ToImageData(ABtm, VImage);
    SetLength(IArray, 1);
    IArray[0] := VImage;
    if not VFormat.SaveToFile(AFileName, IArray, True) then begin
      raise Exception.Create('Ошибка записи файла');
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

end.


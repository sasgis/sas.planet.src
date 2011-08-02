unit u_BitmapTileVampyreLoader;

interface

uses
  Classes,
  Imaging,
  GR32,
  i_InternalPerformanceCounter,
  i_BitmapTileSaveLoad;

type
  TVampyreBasicBitmapTileLoader = class(TInterfacedObject, IBitmapTileLoader)
  private
    FFormat: TImageFileFormat;
    FLoadStreamCounter: IInternalPerformanceCounter;
    FLoadFileCounter: IInternalPerformanceCounter;
  public
    constructor Create(
      AFormat: TImageFileFormat;
      APerfCounterList: IInternalPerformanceCounterList
    );
    procedure LoadFromFile(const AFileName: string; ABtm: TCustomBitmap32);
    procedure LoadFromStream(AStream: TStream; ABtm: TCustomBitmap32);
  end;

  TVampyreBasicBitmapTileLoaderPNG = class(TVampyreBasicBitmapTileLoader)
  public
    constructor Create(
      APerfCounterList: IInternalPerformanceCounterList
    );
  end;

  TVampyreBasicBitmapTileLoaderGIF = class(TVampyreBasicBitmapTileLoader)
  public
    constructor Create(
      APerfCounterList: IInternalPerformanceCounterList
    );
  end;

  TVampyreBasicBitmapTileLoaderBMP = class(TVampyreBasicBitmapTileLoader)
  public
    constructor Create(
      APerfCounterList: IInternalPerformanceCounterList
    );
  end;

  TVampyreBasicBitmapTileLoaderJPEG = class(TVampyreBasicBitmapTileLoader)
  public
    constructor Create(
      APerfCounterList: IInternalPerformanceCounterList
    );
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

constructor TVampyreBasicBitmapTileLoader.Create(
  AFormat: TImageFileFormat;
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  FFormat := AFormat;
  FLoadStreamCounter := APerfCounterList.CreateAndAddNewCounter('LoadStream');
  FLoadFileCounter := APerfCounterList.CreateAndAddNewCounter('LoadFile');
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
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FLoadFileCounter.StartOperation;
  try
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
  finally
    FLoadFileCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TVampyreBasicBitmapTileLoader.LoadFromStream(AStream: TStream;
  ABtm: TCustomBitmap32);
var
  VFormat: TImageFileFormat;
  VImage: TImageData;
  IArray: TDynImageDataArray;
  I: LongInt;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FLoadStreamCounter.StartOperation;
  try
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
  finally
    FLoadStreamCounter.FinishOperation(VCounterContext);
  end;
end;

{ TVampyreBasicBitmapTileLoaderPNG }

constructor TVampyreBasicBitmapTileLoaderPNG.Create(
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(FindImageFileFormatByClass(TPNGFileFormat), APerfCounterList.CreateAndAddNewSubList('VampyrePNG'))
end;

{ TVampyreBasicBitmapTileLoaderGIF }

constructor TVampyreBasicBitmapTileLoaderGIF.Create(
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(FindImageFileFormatByClass(TGIFFileFormat), APerfCounterList.CreateAndAddNewSubList('VampyreGIF'))
end;

{ TVampyreBasicBitmapTileLoaderBMP }

constructor TVampyreBasicBitmapTileLoaderBMP.Create(
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(FindImageFileFormatByClass(TBitmapFileFormat), APerfCounterList.CreateAndAddNewSubList('VampyreBMP'))
end;

{ TVampyreBasicBitmapTileLoaderJPEG }

constructor TVampyreBasicBitmapTileLoaderJPEG.Create(
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(FindImageFileFormatByClass(TJpegFileFormat), APerfCounterList.CreateAndAddNewSubList('VampyreJPEG'))
end;

end.


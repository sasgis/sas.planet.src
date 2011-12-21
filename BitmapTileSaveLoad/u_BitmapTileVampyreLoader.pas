unit u_BitmapTileVampyreLoader;

interface

uses
  Classes,
  SyncObjs,
  Imaging,
  GR32,
  i_InternalPerformanceCounter,
  i_BitmapTileSaveLoad;

type
  TVampyreBasicBitmapTileLoader = class(TInterfacedObject, IBitmapTileLoader)
  private
    FCS: TCriticalSection;
    FMetadata: TMetadata;
    FFormat: TImageFileFormat;
    FLoadStreamCounter: IInternalPerformanceCounter;
  public
    constructor Create(
      AFormatClass: TImageFileFormatClass;
      APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
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
  AFormatClass: TImageFileFormatClass;
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  FCS := TCriticalSection.Create;
  FMetadata := TMetadata.Create;
  FFormat := AFormatClass.Create(FMetadata);
  FLoadStreamCounter := APerfCounterList.CreateAndAddNewCounter('LoadStream');
end;

destructor TVampyreBasicBitmapTileLoader.Destroy;
begin
  FreeAndNil(FCS);
  FreeAndNil(FFormat);
  FreeAndNil(FMetadata);
  inherited;
end;

procedure TVampyreBasicBitmapTileLoader.LoadFromStream(AStream: TStream;
  ABtm: TCustomBitmap32);
var
  VImage: TImageData;
  IArray: TDynImageDataArray;
  I: LongInt;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FLoadStreamCounter.StartOperation;
  try
    InitImage(VImage);
    try
      FCS.Acquire;
      try
        if not FFormat.LoadFromStream(AStream, IArray, True) then begin
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
        FCS.Release;
      end;
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
  inherited Create(TPNGFileFormat, APerfCounterList.CreateAndAddNewSubList('VampyrePNG'))
end;

{ TVampyreBasicBitmapTileLoaderGIF }

constructor TVampyreBasicBitmapTileLoaderGIF.Create(
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(TGIFFileFormat, APerfCounterList.CreateAndAddNewSubList('VampyreGIF'))
end;

{ TVampyreBasicBitmapTileLoaderBMP }

constructor TVampyreBasicBitmapTileLoaderBMP.Create(
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(TBitmapFileFormat, APerfCounterList.CreateAndAddNewSubList('VampyreBMP'))
end;

{ TVampyreBasicBitmapTileLoaderJPEG }

constructor TVampyreBasicBitmapTileLoaderJPEG.Create(
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(TJpegFileFormat, APerfCounterList.CreateAndAddNewSubList('VampyreJPEG'))
end;

end.


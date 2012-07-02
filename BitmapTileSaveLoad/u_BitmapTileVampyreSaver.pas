unit u_BitmapTileVampyreSaver;

interface

uses
  Classes,
  SysUtils,
  Imaging,
  ImagingTypes,
  GR32,
  i_BinaryData,
  i_Bitmap32Static,
  i_ARGBToPaletteConverter,
  i_InternalPerformanceCounter,
  i_BitmapTileSaveLoad;

type
  TVampyreBasicBitmapTileSaver = class(TInterfacedObject, IBitmapTileSaver)
  private
    FCS: IReadWriteSync;
    FMetadata: TMetadata;
    FFormat: TImageFileFormat;
    FPerfCounter: IInternalPerformanceCounter;
  protected
    procedure PrepareData(var AImage: TImageData); virtual;
  private
    procedure SaveToStream(
      ABtm: TCustomBitmap32;
      AStream: TStream
    );
    function Save(const ABitmap: IBitmap32Static): IBinaryData;
  public
    constructor CreateWithMeta(
      AFormat: TImageFileFormat;
      AMeta: TMetadata;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    constructor Create(
      AFormatClass: TImageFileFormatClass;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

  TVampyreBasicBitmapTileSaverPNG = class(TVampyreBasicBitmapTileSaver)
  public
    constructor Create(
      ACompressLevel: LongInt;
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ); overload;
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ); overload;
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
      const AConverter: IARGBToPaletteConverter;
      ACompressLevel: LongInt;
      const APerfCounterList: IInternalPerformanceCounterList = nil
    );
  end;

  TVampyreBasicBitmapTileSaverGIF = class(TVampyreBasicBitmapTileSaver)
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    );
  end;

  TVampyreBasicBitmapTileSaverBMP = class(TVampyreBasicBitmapTileSaver)
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    );
  end;

  TVampyreBasicBitmapTileSaverJPG = class(TVampyreBasicBitmapTileSaver)
  public
    constructor Create(
      ACompressionQuality: Byte;
      const APerfCounterList: IInternalPerformanceCounterList = nil
    );
  end;

function GetVampireGlobalLock: IReadWriteSync;

implementation

uses
  ImagingGraphics32,
  ImagingNetworkGraphics,
  ImagingJpeg,
  ImagingGif,
  ImagingBitmap,
  u_BinaryDataByMemStream;

var
  GVampireGlobalLock: IReadWriteSync;

function GetVampireGlobalLock: IReadWriteSync;
begin
  Result := GVampireGlobalLock;
end;

{ TVampyreBasicBitmapTileSaver }

constructor TVampyreBasicBitmapTileSaver.CreateWithMeta(
  AFormat: TImageFileFormat;
  AMeta: TMetadata;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FCS := GetVampireGlobalLock;
  FMetadata := AMeta;
  FFormat := AFormat;
  if APerfCounterList = nil then begin
    FPerfCounter := nil;
  end else begin
    FPerfCounter := APerfCounterList.CreateAndAddNewCounter('SaveStream');
  end;
end;

constructor TVampyreBasicBitmapTileSaver.Create(
  AFormatClass: TImageFileFormatClass;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VMeta: TMetadata;
begin
  VMeta := TMetadata.Create;
  try
    CreateWithMeta(AFormatClass.Create(VMeta), VMeta, APerfCounterList);
    VMeta := nil;
  finally
    VMeta.Free;
  end;
end;

destructor TVampyreBasicBitmapTileSaver.Destroy;
begin
  FreeAndNil(FFormat);
  FreeAndNil(FMetadata);
  FCS := nil;
  inherited;
end;

procedure TVampyreBasicBitmapTileSaver.PrepareData(var AImage: TImageData);
begin
end;

function TVampyreBasicBitmapTileSaver.Save(
  const ABitmap: IBitmap32Static
): IBinaryData;
var
  VImage: TImageData;
  IArray: TDynImageDataArray;
  VMemStream: TMemoryStream;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if FPerfCounter <> nil then begin
    VCounterContext := FPerfCounter.StartOperation;
  end else begin
    VCounterContext := 0;
  end;
  try
    InitImage(VImage);
    try
      ConvertBitmap32ToImageData(ABitmap.Bitmap, VImage);
      PrepareData(VImage);
      SetLength(IArray, 1);
      IArray[0] := VImage;
      VMemStream := TMemoryStream.Create;
      try
        FCS.BeginWrite;
        try
          if not FFormat.SaveToStream(VMemStream, IArray, True) then begin
            raise Exception.Create('Ошибка записи файла');
          end;
        finally
          FCS.EndWrite;
        end;
        Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
        VMemStream := nil;
      finally
        VMemStream.Free;
      end;
    finally
      FreeImage(VImage);
    end;
  finally
    if FPerfCounter <> nil then begin
      FPerfCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

procedure TVampyreBasicBitmapTileSaver.SaveToStream(
  ABtm: TCustomBitmap32;
  AStream: TStream
);
var
  VImage: TImageData;
  IArray: TDynImageDataArray;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if FPerfCounter <> nil then begin
    VCounterContext := FPerfCounter.StartOperation;
  end else begin
    VCounterContext := 0;
  end;
  try
    InitImage(VImage);
    try
      ConvertBitmap32ToImageData(ABtm, VImage);
      PrepareData(VImage);
      SetLength(IArray, 1);
      IArray[0] := VImage;
      FCS.BeginWrite;
      try
        if not FFormat.SaveToStream(AStream, IArray, True) then begin
          raise Exception.Create('Ошибка записи файла');
        end;
      finally
        FCS.EndWrite;
      end;
    finally
      FreeImage(VImage);
    end;
  finally
    if FPerfCounter <> nil then begin
      FPerfCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

{ TVampyreBasicBitmapTileSaverPNG }

constructor TVampyreBasicBitmapTileSaverPNG.Create(
  ACompressLevel: LongInt;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VFormat: TPNGFileFormat;
  VMeta: TMetadata;
begin
  VMeta := TMetadata.Create;
  VFormat := TPNGFileFormat.Create(VMeta);
  VFormat.CompressLevel := ACompressLevel;
  inherited CreateWithMeta(VFormat, VMeta, APerfCounterList);
end;

constructor TVampyreBasicBitmapTileSaverPNG.Create(
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(TPNGFileFormat, APerfCounterList);
end;

{ TVampyreBasicBitmapTileSaverGIF }

constructor TVampyreBasicBitmapTileSaverGIF.Create(
  const APerfCounterList: IInternalPerformanceCounterList = nil
);
begin
  inherited Create(TGIFFileFormat, APerfCounterList);
end;

{ TVampyreBasicBitmapTileSaverBMP }

constructor TVampyreBasicBitmapTileSaverBMP.Create(
  const APerfCounterList: IInternalPerformanceCounterList = nil
);
begin
  inherited Create(TBitmapFileFormat, APerfCounterList);
end;

{ TVampyreBasicBitmapTileSaverPNGRGB }

procedure TVampyreBasicBitmapTileSaverPNGRGB.PrepareData(var AImage: TImageData);
begin
  ConvertImage(AImage, ifR8G8B8);
end;

{ TVampyreBasicBitmapTileSaverPNGPalette }

constructor TVampyreBasicBitmapTileSaverPNGPalette.Create(
  const AConverter: IARGBToPaletteConverter;
  ACompressLevel: Integer;
  const APerfCounterList: IInternalPerformanceCounterList = nil
);
begin
  inherited Create(ACompressLevel, APerfCounterList);
  FConverter := AConverter;
end;

procedure TVampyreBasicBitmapTileSaverPNGPalette.PrepareData(
  var AImage: TImageData
);
begin
  FConverter.Convert(AImage);
end;

{ TVampyreBasicBitmapTileSaverJPG }

constructor TVampyreBasicBitmapTileSaverJPG.Create(
  ACompressionQuality: byte;
  const APerfCounterList: IInternalPerformanceCounterList = nil
);
var
  VFormat: TJpegFileFormat;
  VMeta: TMetadata;
begin
  VMeta := TMetadata.Create;
  VFormat := TJpegFileFormat.Create(VMeta);
  VFormat.Quality := ACompressionQuality;
  inherited CreateWithMeta(VFormat, VMeta, APerfCounterList);
end;

initialization
  GVampireGlobalLock := TSimpleRWSync.Create;

finalization
  GVampireGlobalLock := nil;
end.

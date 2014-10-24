unit u_BitmapTileLibPng_Test;

interface

uses
  TestFramework,
  Classes,
  SysUtils,
  i_InternalPerformanceCounter,
  i_Bitmap32To8Converter,
  i_BitmapTileSaveLoad,
  i_Bitmap32Static,
  i_BinaryData,
  i_BitmapTileSaveLoadFactory,
  u_BitmapTileLibPng;

type
  TestTLibPngTileSaver = class(TTestCase)
  strict private
    FLibPng8bppTileSaverWithFreeImageConverter: IBitmapTileSaver;
    FLibPng8bppTileSaverWithLibImageQuantConverter: IBitmapTileSaver;
    FLibPng24bppTileSaver: IBitmapTileSaver;
    FLibPng32bppTileSaver: IBitmapTileSaver;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  private
    procedure SaveBinData(const AData: IBinaryData; const AFileName: string);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSave;
  end;

implementation

uses
  u_HashFunctionByImpl,
  u_HashFunctionCRC64,
  u_BinaryDataByMemStream,
  u_BitmapTileSaveLoadFactory,
  u_Bitmap32BufferFactorySimple,
  u_Bitmap32StaticFactory,
  u_InternalPerformanceCounterFake,
  u_Bitmap32To8ConverterByFreeImage,
  u_Bitmap32To8ConverterByLibImageQuant;

const
  cCompression = 9; // 0..9 (0 - no compression; 9 - max compression)

  cPngTestFile = './../Test/bitmaps/pngtest.png';

procedure TestTLibPngTileSaver.SetUp;
var
  VCounter: IInternalPerformanceCounterList;
  VConverter: IBitmap32To8Converter;
begin
  VCounter := TInternalPerformanceCounterFake.Create;

  try
    VConverter := TBitmap32To8ConverterByFreeImage.Create;
    FLibPng8bppTileSaverWithFreeImageConverter :=
      TLibPngTileSaver.Create(VCounter, cCompression, 8, VConverter);
  except // converter create error
    FLibPng8bppTileSaverWithFreeImageConverter := nil;
  end;

  try
    VConverter := TBitmap32To8ConverterByLibImageQuant.Create;
    FLibPng8bppTileSaverWithLibImageQuantConverter :=
      TLibPngTileSaver.Create(VCounter, cCompression, 8, VConverter);
  except // converter create error
    FLibPng8bppTileSaverWithLibImageQuantConverter := nil;
  end;

  FLibPng24bppTileSaver := TLibPngTileSaver.Create(VCounter, cCompression, 24);
  FLibPng32bppTileSaver := TLibPngTileSaver.Create(VCounter, cCompression, 32);

  FBitmapTileSaveLoadFactory :=
    TBitmapTileSaveLoadFactory.Create(
      TBitmap32StaticFactory.Create(
        THashFunctionByImpl.Create(THashFunctionCRC64.Create),
        TBitmap32BufferFactorySimple.Create
      )
    );
end;

procedure TestTLibPngTileSaver.TearDown;
begin
  //
end;

procedure TestTLibPngTileSaver.SaveBinData(const AData: IBinaryData; const AFileName: string);
var
  VMemStream: TMemoryStream;
begin
  Assert(AData <> nil);
  
  VMemStream := TMemoryStream.Create;
  try
    VMemStream.WriteBuffer(AData.Buffer^, AData.Size);
    VMemStream.SaveToFile(AFileName);
  finally
    VMemStream.Free;
  end;
end;

procedure TestTLibPngTileSaver.TestSave;
var
  VMemStream: TMemoryStream;
  VBinData: IBinaryData;
  VBitmap: IBitmap32Static;
begin
  VMemStream := TMemoryStream.Create;
  try
    VMemStream.LoadFromFile(cPngTestFile);
    VBinData := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
  finally
    VMemStream.Free;
  end;

  VBitmap := FBitmapTileSaveLoadFactory.CreatePngLoader.Load(VBinData);

  if Assigned(FLibPng8bppTileSaverWithFreeImageConverter) then begin
    VBinData := FLibPng8bppTileSaverWithFreeImageConverter.Save(VBitmap);
    SaveBinData(VBinData, ChangeFileExt(cPngTestFile, '.8bpp.FreeImage.png'));
  end;

  if Assigned(FLibPng8bppTileSaverWithLibImageQuantConverter) then begin
    VBinData := FLibPng8bppTileSaverWithLibImageQuantConverter.Save(VBitmap);
    SaveBinData(VBinData, ChangeFileExt(cPngTestFile, '.8bpp.LibImageQuant.png'));
  end;

  VBinData := FLibPng24bppTileSaver.Save(VBitmap);
  SaveBinData(VBinData, ChangeFileExt(cPngTestFile, '.24bpp.png'));

  VBinData := FLibPng32bppTileSaver.Save(VBitmap);
  SaveBinData(VBinData, ChangeFileExt(cPngTestFile, '.32bpp.png'));
end;

initialization
  RegisterTest(TestTLibPngTileSaver.Suite);

end.


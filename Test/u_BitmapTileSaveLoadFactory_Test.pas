unit u_BitmapTileSaveLoadFactory_Test;

interface

uses
  TestFramework,
  i_BinaryData,
  i_Bitmap32Static,
  i_BitmapTileSaveLoad,
  i_BitmapTileSaveLoadFactory,
  u_BitmapTileSaveLoadFactory;

type
  TestTBitmapTileSaveLoadFactory = class(TTestCase)
  private
    FBitmap: IBitmap32Static;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    function BinaryDataFromFile(const AFileName: string): IBinaryData;
    procedure BinaryDataToFile(const AFileName: string; const AData: IBinaryData);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBmp;
    procedure TestGif;
    procedure TestPng;
    procedure TestJpeg;
  end;

implementation

uses
  Classes,
  u_Bitmap32StaticFactorySimple,
  u_BinaryDataByMemStream;

const
  cBitmapsFolder = './../Test/bitmaps/';

  cBmpSrcTestFile = cBitmapsFolder + 'testimg.bmp';
  cBmpDestTestFile = cBitmapsFolder + 'testimg_out.bmp';

  cGifSrcTestFile = cBitmapsFolder + 'giftest.gif';
  cGifDestTestFile = cBitmapsFolder + 'giftest_out.gif';

  cPngSrcTestFile = cBitmapsFolder + 'pngtest.png';
  cPngDestTestFile = cBitmapsFolder + 'pngtest_out.png';

  cJpegSrcTestFile = cBitmapsFolder + 'testimgfst.jpg';
  cJpegDestTestFile = cBitmapsFolder + 'testimgfst_out.jpg';

procedure TestTBitmapTileSaveLoadFactory.SetUp;
begin
  FBitmapTileSaveLoadFactory :=
    TBitmapTileSaveLoadFactory.Create(TBitmap32StaticFactorySimple.Create) as IBitmapTileSaveLoadFactory;
end;

procedure TestTBitmapTileSaveLoadFactory.TearDown;
begin
  FBitmapTileSaveLoadFactory := nil;
end;

function TestTBitmapTileSaveLoadFactory.BinaryDataFromFile(
  const AFileName: string
): IBinaryData;
var
  VStream: TMemoryStream;
begin
  VStream := TMemoryStream.Create;
  try
    VStream.LoadFromFile(AFileName);
    Result := TBinaryDataByMemStream.CreateWithOwn(VStream);
    VStream := nil;
  finally
    VStream.Free;
  end;
end;

procedure TestTBitmapTileSaveLoadFactory.BinaryDataToFile(
  const AFileName: string;
  const AData: IBinaryData
);
var
  VStream: TMemoryStream;
begin
  VStream := TMemoryStream.Create;
  try
    VStream.WriteBuffer(AData.Buffer^, AData.Size);
    VStream.SaveToFile(AFileName);
  finally
    VStream.Free;
  end;
end;

procedure TestTBitmapTileSaveLoadFactory.TestBmp;
var
  VSaver: IBitmapTileSaver;
  VLoader: IBitmapTileLoader;
  VBinaryData: IBinaryData;
begin
  VBinaryData := BinaryDataFromFile(cBmpSrcTestFile);

  VLoader := FBitmapTileSaveLoadFactory.CreateBmpLoader;
  FBitmap := VLoader.Load(VBinaryData);

  VSaver := FBitmapTileSaveLoadFactory.CreateBmpSaver;
  VBinaryData := VSaver.Save(FBitmap);

  BinaryDataToFile(cBmpDestTestFile, VBinaryData);
end;

procedure TestTBitmapTileSaveLoadFactory.TestGif;
var
  VSaver: IBitmapTileSaver;
  VLoader: IBitmapTileLoader;
  VBinaryData: IBinaryData;
begin
  VBinaryData := BinaryDataFromFile(cGifSrcTestFile);

  VLoader := FBitmapTileSaveLoadFactory.CreateGifLoader;
  FBitmap := VLoader.Load(VBinaryData);

  VSaver := FBitmapTileSaveLoadFactory.CreateGifSaver;
  VBinaryData := VSaver.Save(FBitmap);

  BinaryDataToFile(cGifDestTestFile, VBinaryData);
end;

procedure TestTBitmapTileSaveLoadFactory.TestPng;
var
  VSaver: IBitmapTileSaver;
  VLoader: IBitmapTileLoader;
  VBinaryData: IBinaryData;
begin
  VBinaryData := BinaryDataFromFile(cPngSrcTestFile);

  VLoader := FBitmapTileSaveLoadFactory.CreatePngLoader;
  FBitmap := VLoader.Load(VBinaryData);

  VSaver := FBitmapTileSaveLoadFactory.CreatePngSaver;
  VBinaryData := VSaver.Save(FBitmap);

  BinaryDataToFile(cPngDestTestFile, VBinaryData);
end;

procedure TestTBitmapTileSaveLoadFactory.TestJpeg;
var
  VSaver: IBitmapTileSaver;
  VLoader: IBitmapTileLoader;
  VBinaryData: IBinaryData;
begin
  VBinaryData := BinaryDataFromFile(cJpegSrcTestFile);

  VLoader := FBitmapTileSaveLoadFactory.CreateJpegLoader;
  FBitmap := VLoader.Load(VBinaryData);

  VSaver := FBitmapTileSaveLoadFactory.CreateJpegSaver;
  VBinaryData := VSaver.Save(FBitmap);

  BinaryDataToFile(cJpegDestTestFile, VBinaryData);
end;

initialization
  RegisterTest(TestTBitmapTileSaveLoadFactory.Suite);

end.


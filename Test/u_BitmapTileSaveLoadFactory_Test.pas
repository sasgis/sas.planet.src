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
    procedure DoTest(const AExt: string; const ALoader: IBitmapTileLoader; const ASaver: IBitmapTileSaver);
  public
    procedure SetUp; override;
  published
    procedure TestBmp;
    procedure TestGif;
    procedure TestPng;
    procedure TestJpeg;
    procedure TestWebp;
    procedure TestTiff;
  end;

implementation

uses
  Classes,
  u_HashFunctionByImpl,
  u_HashFunctionCRC64,
  u_Bitmap32BufferFactorySimple,
  u_Bitmap32StaticFactory,
  u_BinaryDataByMemStream;

const
  cBitmapsFolder = '.\..\..\Test\data\bitmaps\';
  cTestBitmapFileName = cBitmapsFolder + 'test';

procedure TestTBitmapTileSaveLoadFactory.SetUp;
begin
  FBitmapTileSaveLoadFactory :=
    TBitmapTileSaveLoadFactory.Create(
      TBitmap32StaticFactory.Create(
        THashFunctionByImpl.Create(THashFunctionCRC64.Create),
        TBitmap32BufferFactorySimple.Create
      )
    );
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

procedure TestTBitmapTileSaveLoadFactory.DoTest(
  const AExt: string;
  const ALoader: IBitmapTileLoader;
  const ASaver: IBitmapTileSaver
);
var
  VBinaryData: IBinaryData;
begin
  VBinaryData := BinaryDataFromFile(cTestBitmapFileName + '.' + AExt);
  FBitmap := ALoader.Load(VBinaryData);

  VBinaryData := ASaver.Save(FBitmap);
  BinaryDataToFile(cTestBitmapFileName + '_out' + '.' + AExt, VBinaryData);
end;

procedure TestTBitmapTileSaveLoadFactory.TestBmp;
begin
  DoTest(
    'bmp',
    FBitmapTileSaveLoadFactory.CreateBmpLoader,
    FBitmapTileSaveLoadFactory.CreateBmpSaver
  );
end;

procedure TestTBitmapTileSaveLoadFactory.TestGif;
begin
  DoTest(
    'gif',
    FBitmapTileSaveLoadFactory.CreateGifLoader,
    FBitmapTileSaveLoadFactory.CreateGifSaver
  );
end;

procedure TestTBitmapTileSaveLoadFactory.TestPng;
begin
  DoTest(
    'png',
    FBitmapTileSaveLoadFactory.CreatePngLoader,
    FBitmapTileSaveLoadFactory.CreatePngSaver
  );
end;

procedure TestTBitmapTileSaveLoadFactory.TestJpeg;
begin
  DoTest(
    'jpg',
    FBitmapTileSaveLoadFactory.CreateJpegLoader,
    FBitmapTileSaveLoadFactory.CreateJpegSaver
  );
end;

procedure TestTBitmapTileSaveLoadFactory.TestTiff;
begin
  DoTest(
    'tif',
    FBitmapTileSaveLoadFactory.CreateTiffLoader,
    FBitmapTileSaveLoadFactory.CreateTiffSaver
  );
end;

procedure TestTBitmapTileSaveLoadFactory.TestWebp;
begin
  DoTest(
    'webp',
    FBitmapTileSaveLoadFactory.CreateWebpLoader,
    FBitmapTileSaveLoadFactory.CreateWebpSaver
  );
end;

initialization
  RegisterTest(TestTBitmapTileSaveLoadFactory.Suite);

end.


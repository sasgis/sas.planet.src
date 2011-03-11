unit u_BitmapTileGELoader;

interface

uses
  Classes,
  GR32,
  i_BitmapTileSaveLoad;

type
  TBitmapTileGELoader = class(TInterfacedObject, IBitmapTileLoader)
  private
    FJpegLoader: IBitmapTileLoader;
    FDXTextureLoader: IBitmapTileLoader;
    procedure LoadFromMemStream(AStream: TCustomMemoryStream; ABtm: TCustomBitmap32);
  protected
    procedure LoadFromFile(AFileName: string; ABtm: TCustomBitmap32);
    procedure LoadFromStream(AStream: TStream; ABtm: TCustomBitmap32);
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  u_BitmapTileVampyreLoader,
  u_BitmapTileGEDXTextureLoader,
  u_GECrypt;

{ TBitmapTileGELoader }

constructor TBitmapTileGELoader.Create;
begin
  FJpegLoader := TVampyreBasicBitmapTileLoaderJPEG.Create;
  FDXTextureLoader := TBitmapTileGEDXTextureLoader.Create;
end;

destructor TBitmapTileGELoader.Destroy;
begin
  FJpegLoader := nil;
  FDXTextureLoader := nil;
  inherited;
end;

procedure TBitmapTileGELoader.LoadFromFile(AFileName: string;
  ABtm: TCustomBitmap32);
var
  VMemStream: TMemoryStream;
begin
  VMemStream := TMemoryStream.Create;
  try
    VMemStream.LoadFromFile(AFileName);
    LoadFromMemStream(VMemStream, ABtm);
  finally
    VMemStream.Free;
  end;
end;

procedure TBitmapTileGELoader.LoadFromStream(AStream: TStream;
  ABtm: TCustomBitmap32);
var
  VMemStream: TMemoryStream;
begin
  if AStream is TCustomMemoryStream then begin
    LoadFromMemStream(TCustomMemoryStream(AStream), ABtm);
  end else begin
    VMemStream := TMemoryStream.Create;
    try
      VMemStream.LoadFromStream(AStream);
      LoadFromMemStream(VMemStream, ABtm);
    finally
      VMemStream.Free;
    end;
  end;
end;

procedure TBitmapTileGELoader.LoadFromMemStream(AStream: TCustomMemoryStream;
  ABtm: TCustomBitmap32);
var
  VTileStart: LongWord;
begin
  AStream.Position := 0;
  AStream.ReadBuffer(VTileStart, SizeOf(VTileStart));
  case VTileStart of
    CRYPTED_JPEG: begin
      GEcrypt(AStream.Memory, AStream.Size);
      FJpegLoader.LoadFromStream(AStream, ABtm);
    end;
    DECRYPTED_JPEG: begin
      FJpegLoader.LoadFromStream(AStream, ABtm);
    end;
    CRYPTED_DXT1: begin
      GEcrypt(AStream.Memory, AStream.Size);
      FDXTextureLoader.LoadFromStream(AStream, ABtm);
    end;
    DECRYPTED_DXT1: begin
      FDXTextureLoader.LoadFromStream(AStream, ABtm);
    end;
  end;
end;

end.

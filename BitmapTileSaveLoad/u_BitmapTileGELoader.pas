unit u_BitmapTileGELoader;

interface

uses
  Classes,
  GR32,
  i_BinaryData,
  i_Bitmap32Static,
  i_InternalPerformanceCounter,
  i_BitmapTileSaveLoad;

type
  TBitmapTileGELoader = class(TInterfacedObject, IBitmapTileLoader)
  private
    FJpegLoader: IBitmapTileLoader;
    FDXTextureLoader: IBitmapTileLoader;
    procedure LoadFromMemStream(AStream: TCustomMemoryStream; ABtm: TCustomBitmap32);
  protected
    procedure LoadFromStream(AStream: TStream; ABtm: TCustomBitmap32);
    function Load(AData: IBinaryData): IBitmap32Static;
  public
    constructor Create(
      APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_BinaryDataByMemStream,
  u_BitmapTileVampyreLoader,
  u_BitmapTileGEDXTextureLoader,
  u_GECrypt;

{ TBitmapTileGELoader }

constructor TBitmapTileGELoader.Create(
  APerfCounterList: IInternalPerformanceCounterList
);
var
  VPerfCounterList: IInternalPerformanceCounterList;
begin
  VPerfCounterList := APerfCounterList.CreateAndAddNewSubList('GELoader');
  FJpegLoader := TVampyreBasicBitmapTileLoaderJPEG.Create(VPerfCounterList);
  FDXTextureLoader := TBitmapTileGEDXTextureLoader.Create(VPerfCounterList);
end;

destructor TBitmapTileGELoader.Destroy;
begin
  FJpegLoader := nil;
  FDXTextureLoader := nil;
  inherited;
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

function TBitmapTileGELoader.Load(AData: IBinaryData): IBitmap32Static;
var
  VTileStart: LongWord;
  VMemStream: TMemoryStream;
  VData: IBinaryData;
  VLoader: IBitmapTileLoader;
begin
  if AData.Size < SizeOf(VTileStart) then begin
    raise Exception.Create('No source data');
  end;
  VTileStart := PLongWord(AData.Buffer)^;
  case VTileStart of
    CRYPTED_JPEG: begin
      VMemStream := TMemoryStream.Create;
      try
        VMemStream.WriteBuffer(AData.Buffer^, AData.Size);
        GEcrypt(VMemStream.Memory, VMemStream.Size);
      except
        FreeAndNil(VMemStream);
        raise;
      end;
      VData := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
      VLoader := FJpegLoader;
    end;
    DECRYPTED_JPEG: begin
      VData := AData;
      VLoader := FJpegLoader;
    end;
    CRYPTED_DXT1: begin
      VMemStream := TMemoryStream.Create;
      try
        VMemStream.WriteBuffer(AData.Buffer^, AData.Size);
        GEcrypt(VMemStream.Memory, VMemStream.Size);
      except
        FreeAndNil(VMemStream);
        raise;
      end;
      VData := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
      VLoader := FDXTextureLoader;
    end;
    DECRYPTED_DXT1: begin
      VData := AData;
      VLoader := FDXTextureLoader;
    end;
  end;
  if VLoader <> nil then begin
    Result := VLoader.Load(VData);
  end;
end;

procedure TBitmapTileGELoader.LoadFromMemStream(AStream: TCustomMemoryStream;
  ABtm: TCustomBitmap32);
var
  VTileStart: LongWord;
begin
  AStream.Position := 0;
  AStream.ReadBuffer(VTileStart, SizeOf(VTileStart));
  AStream.Position := 0;
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

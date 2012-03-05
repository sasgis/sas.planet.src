unit u_BitmapTileGELoader;

interface

(*
uses
  Classes,
  GR32,
  i_InternalPerformanceCounter,
  i_BitmapTileSaveLoad;
*)

(*
type
  TBitmapTileGELoader = class(TInterfacedObject, IBitmapTileLoader)
  private
    FJpegLoader: IBitmapTileLoader;
    FDXTextureLoader: IBitmapTileLoader;
    procedure LoadFromMemStream(AStream: TCustomMemoryStream; ABtm: TCustomBitmap32);
  protected
    procedure LoadFromStream(AStream: TStream; ABtm: TCustomBitmap32);
  public
    constructor Create(
      APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;
*)

implementation

(*
uses
  u_BitmapTileVampyreLoader,
  u_BitmapTileGEDXTextureLoader,
  u_GECrypt;
*)

{ TBitmapTileGELoader }

(*
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

*)
end.

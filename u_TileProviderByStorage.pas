unit u_TileProviderByStorage;

interface

uses
  Types,
  i_Bitmap32Static,
  i_MapVersionConfig,
  i_BitmapTileSaveLoad,
  i_VectorDataItemSimple,
  i_CoordConverter,
  i_TileProvider,
  i_VectorDataLoader,
  i_ImageResamplerConfig,
  i_VectorDataFactory,
  u_TileStorageAbstract;

type
  TBitmapTileProviderByStorage = class(TInterfacedObject, IBitmapTileProvider)
  private
    FGeoConverter: ICoordConverter;
    FVersionConfig: IMapVersionConfig;
    FLoaderFromStorage: IBitmapTileLoader;
    FStorage: TTileStorageAbstract;
    FIsIgnoreError: Boolean;
    FImageResamplerConfig: IImageResamplerConfig;
  private
    function GetGeoConverter: ICoordConverter;
    function GetTile(
      const ATile: TPoint;
      const AZoom: Byte
    ): IBitmap32Static;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AImageResamplerConfig: IImageResamplerConfig;
      const AGeoConverter: ICoordConverter;
      const AVersionConfig: IMapVersionConfig;
      const ALoaderFromStorage: IBitmapTileLoader;
      const AStorage: TTileStorageAbstract
    );
  end;

  TVectorTileProviderByStorage = class(TInterfacedObject, IVectorTileProvider)
  private
    FGeoConverter: ICoordConverter;
    FVersionConfig: IMapVersionConfig;
    FLoaderFromStorage: IVectorDataLoader;
    FStorage: TTileStorageAbstract;
    FVectorDataFactory: IVectorDataFactory;
    FIsIgnoreError: Boolean;
  private
    function GetGeoConverter: ICoordConverter;
    function GetTile(
      const ATile: TPoint;
      const AZoom: Byte
    ): IVectorDataItemList;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AVectorDataFactory: IVectorDataFactory;
      const AGeoConverter: ICoordConverter;
      const AVersionConfig: IMapVersionConfig;
      const ALoaderFromStorage: IVectorDataLoader;
      const AStorage: TTileStorageAbstract
    );
  end;

implementation

uses
  GR32,
  GR32_Resamplers,
  i_TileInfoBasic,
  i_BinaryData,
  u_Bitmap32Static;

{ TBitmapTileProviderByStorage }

constructor TBitmapTileProviderByStorage.Create(
  const AIsIgnoreError: Boolean;
  const AImageResamplerConfig: IImageResamplerConfig;
  const AGeoConverter: ICoordConverter;
  const AVersionConfig: IMapVersionConfig;
  const ALoaderFromStorage: IBitmapTileLoader;
  const AStorage: TTileStorageAbstract);
begin
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FImageResamplerConfig := AImageResamplerConfig;
  FGeoConverter := AGeoConverter;
  FVersionConfig := AVersionConfig;
  FLoaderFromStorage := ALoaderFromStorage;
  FStorage := AStorage;
end;

function TBitmapTileProviderByStorage.GetGeoConverter: ICoordConverter;
begin
  Result := FGeoConverter;
end;

function TBitmapTileProviderByStorage.GetTile(
  const ATile: TPoint;
  const AZoom: Byte
): IBitmap32Static;
var
  VTileInfo: ITileInfoBasic;
  VData: IBinaryData;
  VRect: TRect;
  VSize: TPoint;
  VBitmap: TCustomBitmap32;
  VResampler: TCustomResampler;
begin
  Result := nil;
  try
    VData := FStorage.LoadTile(ATile, AZoom, FVersionConfig.Version, VTileInfo);
    if VData <> nil then begin
      Result := FLoaderFromStorage.Load(VData);
    end;
    if Result <> nil then begin
      VRect := FGeoConverter.TilePos2PixelRect(ATile, AZoom);
      VSize := Types.Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
      if (Result.Bitmap.Width <> VSize.X) or
        (Result.Bitmap.Height <> VSize.Y) then begin
        VResampler := FImageResamplerConfig.GetActiveFactory.CreateResampler;
        try
          VBitmap := TCustomBitmap32.Create;
          try
            VBitmap.SetSize(VSize.X, VSize.Y);
            StretchTransfer(
              VBitmap,
              VBitmap.BoundsRect,
              VBitmap.ClipRect,
              Result.Bitmap,
              Result.Bitmap.BoundsRect,
              VResampler,
              dmOpaque
            );
            Result := TBitmap32Static.CreateWithOwn(VBitmap);
            VBitmap := nil;
          finally
            VBitmap.Free;
          end;
        finally
          VResampler.Free;
        end;
      end;
    end;
  except
    if not FIsIgnoreError then begin
      raise;
    end else begin
      Result := nil;
    end;
  end;
end;

{ TVectorTileProviderByStorage }

constructor TVectorTileProviderByStorage.Create(
  const AIsIgnoreError: Boolean;
  const AVectorDataFactory: IVectorDataFactory;
  const AGeoConverter: ICoordConverter;
  const AVersionConfig: IMapVersionConfig;
  const ALoaderFromStorage: IVectorDataLoader;
  const AStorage: TTileStorageAbstract);
begin
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FVectorDataFactory := AVectorDataFactory;
  FGeoConverter := AGeoConverter;
  FVersionConfig := AVersionConfig;
  FLoaderFromStorage := ALoaderFromStorage;
  FStorage := AStorage;
end;

function TVectorTileProviderByStorage.GetGeoConverter: ICoordConverter;
begin
  Result := FGeoConverter;
end;

function TVectorTileProviderByStorage.GetTile(
  const ATile: TPoint;
  const AZoom: Byte
): IVectorDataItemList;
var
  VTileInfo: ITileInfoBasic;
  VData: IBinaryData;
begin
  Result := nil;
  try
    VData := FStorage.LoadTile(ATile, AZoom, FVersionConfig.Version, VTileInfo);
    if VData <> nil then begin
      Result := FLoaderFromStorage.Load(VData, FVectorDataFactory);
    end;
  except
    if not FIsIgnoreError then begin
      raise;
    end else begin
      Result := nil;
    end;
  end;
end;

end.

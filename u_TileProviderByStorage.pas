unit u_TileProviderByStorage;

interface

uses
  Types,
  SysUtils,
  i_NotifierTileRectUpdate,
  i_Bitmap32Static,
  i_MapVersionConfig,
  i_BitmapTileSaveLoad,
  i_VectorDataItemSimple,
  i_CoordConverter,
  i_TileProvider,
  i_VectorDataLoader,
  i_ImageResamplerConfig,
  i_VectorDataFactory,
  i_TileStorage;

type
  TBitmapTileProviderByStorage = class(TInterfacedObject, IBitmapTileProvider)
  private
    FGeoConverter: ICoordConverter;
    FVersionConfig: IMapVersionConfig;
    FLoaderFromStorage: IBitmapTileLoader;
    FStorage: ITileStorage;
    FIsIgnoreError: Boolean;
    FImageResamplerConfig: IImageResamplerConfig;
  private
    function GetGeoConverter: ICoordConverter;
    function GetTile(
      const ATile: TPoint;
      const AZoom: Byte
    ): IBitmap32Static;
    function GetChangeNotifier: INotifierTileRectUpdate;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AImageResamplerConfig: IImageResamplerConfig;
      const AVersionConfig: IMapVersionConfig;
      const ALoaderFromStorage: IBitmapTileLoader;
      const AStorage: ITileStorage
    );
  end;

  TVectorTileProviderByStorage = class(TInterfacedObject, IVectorTileProvider)
  private
    FGeoConverter: ICoordConverter;
    FVersionConfig: IMapVersionConfig;
    FLoaderFromStorage: IVectorDataLoader;
    FStorage: ITileStorage;
    FVectorDataFactory: IVectorDataFactory;
    FIsIgnoreError: Boolean;
  private
    function GetGeoConverter: ICoordConverter;
    function GetTile(
      const ATile: TPoint;
      const AZoom: Byte
    ): IVectorDataItemList;
    function GetChangeNotifier: INotifierTileRectUpdate;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AVectorDataFactory: IVectorDataFactory;
      const AVersionConfig: IMapVersionConfig;
      const ALoaderFromStorage: IVectorDataLoader;
      const AStorage: ITileStorage
    );
  end;

implementation

uses
  GR32,
  i_TileInfoBasic,
  u_BitmapFunc,
  u_Bitmap32Static;

{ TBitmapTileProviderByStorage }

constructor TBitmapTileProviderByStorage.Create(
  const AIsIgnoreError: Boolean;
  const AImageResamplerConfig: IImageResamplerConfig;
  const AVersionConfig: IMapVersionConfig;
  const ALoaderFromStorage: IBitmapTileLoader;
  const AStorage: ITileStorage
);
begin
  Assert(AImageResamplerConfig <> nil);
  Assert(AVersionConfig <> nil);
  Assert(ALoaderFromStorage <> nil);
  Assert(AStorage <> nil);
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FImageResamplerConfig := AImageResamplerConfig;
  FStorage := AStorage;
  FGeoConverter := FStorage.CoordConverter;
  FVersionConfig := AVersionConfig;
  FLoaderFromStorage := ALoaderFromStorage;
end;

function TBitmapTileProviderByStorage.GetChangeNotifier: INotifierTileRectUpdate;
begin
  Result := FStorage.TileNotifier;
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
  VTileInfo: ITileInfoWithData;
  VRect: TRect;
  VSize: TPoint;
  VBitmap: TCustomBitmap32;
  VResampler: TCustomResampler;
begin
  Result := nil;
  try
    if Supports(FStorage.GetTileInfo(ATile, AZoom, FVersionConfig.Version, gtimWithData), ITileInfoWithData, VTileInfo) then begin
      Result := FLoaderFromStorage.Load(VTileInfo.TileData);
    end;
    if Result <> nil then begin
      VRect := FGeoConverter.TilePos2PixelRect(ATile, AZoom);
      VSize := Types.Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
      if (Result.Size.X <> VSize.X) or
        (Result.Size.Y <> VSize.Y) then begin
        VResampler := FImageResamplerConfig.GetActiveFactory.CreateResampler;
        try
          VBitmap := TCustomBitmap32.Create;
          try
            VBitmap.SetSize(VSize.X, VSize.Y);
            StretchTransferFull(
              VBitmap,
              VBitmap.BoundsRect,
              Result,
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
  const AVersionConfig: IMapVersionConfig;
  const ALoaderFromStorage: IVectorDataLoader;
  const AStorage: ITileStorage);
begin
  Assert(AVectorDataFactory <> nil);
  Assert(AVersionConfig <> nil);
  Assert(ALoaderFromStorage <> nil);
  Assert(AStorage <> nil);
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FVectorDataFactory := AVectorDataFactory;
  FStorage := AStorage;
  FGeoConverter := FStorage.CoordConverter;
  FVersionConfig := AVersionConfig;
  FLoaderFromStorage := ALoaderFromStorage;
end;

function TVectorTileProviderByStorage.GetChangeNotifier: INotifierTileRectUpdate;
begin
  Result := FStorage.TileNotifier;
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
  VTileInfo: ITileInfoWithData;
begin
  Result := nil;
  try
    if Supports(FStorage.GetTileInfo(ATile, AZoom, FVersionConfig.Version, gtimWithData), ITileInfoWithData, VTileInfo) then begin
      Result := FLoaderFromStorage.Load(VTileInfo.TileData, nil, FVectorDataFactory);
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

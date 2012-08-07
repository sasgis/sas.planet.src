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
  i_VectorDataFactory,
  u_TileStorageAbstract;

type
  TBitmapTileProviderByStorage = class(TInterfacedObject, IBitmapTileProvider)
  private
    FGeoConverter: ICoordConverter;
    FVersionConfig: IMapVersionConfig;
    FLoaderFromStorage: IBitmapTileLoader;
    FStorage: TTileStorageAbstract;
  private
    function GetGeoConverter: ICoordConverter;
    function GetTile(
      const AZoom: Byte;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
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
  private
    function GetGeoConverter: ICoordConverter;
    function GetTile(
      const AZoom: Byte;
      const ATile: TPoint
    ): IVectorDataItemList;
  public
    constructor Create(
      const AVectorDataFactory: IVectorDataFactory;
      const AGeoConverter: ICoordConverter;
      const AVersionConfig: IMapVersionConfig;
      const ALoaderFromStorage: IVectorDataLoader;
      const AStorage: TTileStorageAbstract
    );
  end;

implementation

uses
  i_TileInfoBasic,
  i_BinaryData;

{ TBitmapTileProviderByStorage }

constructor TBitmapTileProviderByStorage.Create(
  const AGeoConverter: ICoordConverter;
  const AVersionConfig: IMapVersionConfig;
  const ALoaderFromStorage: IBitmapTileLoader;
  const AStorage: TTileStorageAbstract);
begin
  inherited Create;
  FGeoConverter := AGeoConverter;
  FVersionConfig := AVersionConfig;
  FLoaderFromStorage := ALoaderFromStorage;
  FStorage := AStorage;
end;

function TBitmapTileProviderByStorage.GetGeoConverter: ICoordConverter;
begin
  Result := FGeoConverter;
end;

function TBitmapTileProviderByStorage.GetTile(const AZoom: Byte;
  const ATile: TPoint): IBitmap32Static;
var
  VTileInfo: ITileInfoBasic;
  VData: IBinaryData;
begin
  Result := nil;
  VData := FStorage.LoadTile(ATile, AZoom, FVersionConfig.Version, VTileInfo);
  if VData <> nil then begin
    Result := FLoaderFromStorage.Load(VData);
  end;
end;

{ TVectorTileProviderByStorage }

constructor TVectorTileProviderByStorage.Create(
  const AVectorDataFactory: IVectorDataFactory;
  const AGeoConverter: ICoordConverter;
  const AVersionConfig: IMapVersionConfig;
  const ALoaderFromStorage: IVectorDataLoader;
  const AStorage: TTileStorageAbstract);
begin
  inherited Create;
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

function TVectorTileProviderByStorage.GetTile(const AZoom: Byte;
  const ATile: TPoint): IVectorDataItemList;
var
  VTileInfo: ITileInfoBasic;
  VData: IBinaryData;
begin
  Result := nil;
  VData := FStorage.LoadTile(ATile, AZoom, FVersionConfig.Version, VTileInfo);
  if VData <> nil then begin
    Result := FLoaderFromStorage.Load(VData, FVectorDataFactory);
  end;
end;

end.

unit u_TileProviderWithCache;

interface

uses
  Types,
  i_NotifierTileRectUpdate,
  i_Bitmap32Static,
  i_VectorDataItemSimple,
  i_CoordConverter,
  i_TileProvider,
  i_TileObjCache,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderWithCache = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FSource: IBitmapTileProvider;
    FCache: ITileObjCacheBitmap;
  private
    function GetGeoConverter: ICoordConverter;
    function GetTile(
      const ATile: TPoint;
      const AZoom: Byte
    ): IBitmap32Static;
    function GetChangeNotifier: INotifierTileRectUpdate;
  public
    constructor Create(
      const ASource: IBitmapTileProvider;
      const ACache: ITileObjCacheBitmap
    );
  end;

  TVectorTileProviderWithCache = class(TBaseInterfacedObject, IVectorTileProvider)
  private
    FSource: IVectorTileProvider;
    FCache: ITileObjCacheVector;
  private
    function GetGeoConverter: ICoordConverter;
    function GetTile(
      const ATile: TPoint;
      const AZoom: Byte
    ): IVectorDataItemList;
    function GetChangeNotifier: INotifierTileRectUpdate;
  public
    constructor Create(
      const ASource: IVectorTileProvider;
      const ACache: ITileObjCacheVector
    );
  end;

implementation

{ TBitmapTileProviderWithCache }

constructor TBitmapTileProviderWithCache.Create(
  const ASource: IBitmapTileProvider; const ACache: ITileObjCacheBitmap);
begin
  Assert(ASource <> nil);
  Assert(ACache <> nil);
  inherited Create;
  FSource := ASource;
  FCache := ACache;
end;

function TBitmapTileProviderWithCache.GetChangeNotifier: INotifierTileRectUpdate;
begin
  Result := FSource.ChangeNotifier;
end;

function TBitmapTileProviderWithCache.GetGeoConverter: ICoordConverter;
begin
  Result := FSource.GeoConverter;
end;

function TBitmapTileProviderWithCache.GetTile(
  const ATile: TPoint;
  const AZoom: Byte
): IBitmap32Static;
begin
  Result := FCache.TryLoadTileFromCache(ATile, AZoom);
  if Result = nil then begin
    Result := FSource.GetTile(ATile, AZoom);
    if Result <> nil then begin
      FCache.AddTileToCache(Result, ATile, AZoom);
    end;
  end;
end;

{ TVectorTileProviderWithCache }

constructor TVectorTileProviderWithCache.Create(
  const ASource: IVectorTileProvider; const ACache: ITileObjCacheVector);
begin
  Assert(ASource <> nil);
  Assert(ACache <> nil);
  inherited Create;
  FSource := ASource;
  FCache := ACache;
end;

function TVectorTileProviderWithCache.GetChangeNotifier: INotifierTileRectUpdate;
begin
  Result := FSource.ChangeNotifier;
end;

function TVectorTileProviderWithCache.GetGeoConverter: ICoordConverter;
begin
  Result := FSource.GeoConverter;
end;

function TVectorTileProviderWithCache.GetTile(
  const ATile: TPoint;
  const AZoom: Byte
): IVectorDataItemList;
begin
  Result := FCache.TryLoadTileFromCache(ATile, AZoom);
  if Result = nil then begin
    Result := FSource.GetTile(ATile, AZoom);
    if Result <> nil then begin
      FCache.AddTileToCache(Result, ATile, AZoom);
    end;
  end;
end;

end.

unit u_TileProviderByStorage;

interface

uses
  Types,
  SysUtils,
  i_NotifierTilePyramidUpdate,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_MapVersionConfig,
  i_BitmapTileSaveLoad,
  i_VectorDataItemSimple,
  i_ProjectionInfo,
  i_TileProvider,
  i_VectorDataLoader,
  i_ImageResamplerConfig,
  i_VectorDataFactory,
  i_TileStorage,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderByStorage = class(TBaseInterfacedObject, IBitmapTileProviderWithNotifier)
  private
    FProjectionInfo: IProjectionInfo;
    FVersionConfig: IMapVersionConfig;
    FLoaderFromStorage: IBitmapTileLoader;
    FBitmapFactory: IBitmap32StaticFactory;
    FStorage: ITileStorage;
    FIsIgnoreError: Boolean;
    FImageResamplerConfig: IImageResamplerConfig;
  private
    function GetProjectionInfo: IProjectionInfo;
    function GetTile(const ATile: TPoint): IBitmap32Static;
    function GetChangeNotifier: INotifierTilePyramidUpdate;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AImageResamplerConfig: IImageResamplerConfig;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AVersionConfig: IMapVersionConfig;
      const ALoaderFromStorage: IBitmapTileLoader;
      const AProjectionInfo: IProjectionInfo;
      const AStorage: ITileStorage
    );
  end;

  TVectorTileProviderByStorage = class(TBaseInterfacedObject, IVectorTileProviderWithNotifier)
  private
    FProjectionInfo: IProjectionInfo;
    FVersionConfig: IMapVersionConfig;
    FLoaderFromStorage: IVectorDataLoader;
    FStorage: ITileStorage;
    FVectorDataFactory: IVectorDataFactory;
    FIsIgnoreError: Boolean;
  private
    function GetProjectionInfo: IProjectionInfo;
    function GetTile(const ATile: TPoint): IVectorDataItemList;
    function GetChangeNotifier: INotifierTilePyramidUpdate;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AVectorDataFactory: IVectorDataFactory;
      const AVersionConfig: IMapVersionConfig;
      const ALoaderFromStorage: IVectorDataLoader;
      const AProjectionInfo: IProjectionInfo;
      const AStorage: ITileStorage
    );
  end;

implementation

uses
  GR32,
  i_TileInfoBasic,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapTileProviderByStorage }

constructor TBitmapTileProviderByStorage.Create(
  const AIsIgnoreError: Boolean;
  const AImageResamplerConfig: IImageResamplerConfig;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AVersionConfig: IMapVersionConfig;
  const ALoaderFromStorage: IBitmapTileLoader;
  const AProjectionInfo: IProjectionInfo;
  const AStorage: ITileStorage
);
begin
  Assert(AImageResamplerConfig <> nil);
  Assert(AVersionConfig <> nil);
  Assert(ALoaderFromStorage <> nil);
  Assert(AStorage <> nil);
  Assert(AProjectionInfo <> nil);
  Assert(AStorage.CoordConverter.IsSameConverter(AProjectionInfo.GeoConverter));
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FImageResamplerConfig := AImageResamplerConfig;
  FStorage := AStorage;
  FBitmapFactory := ABitmapFactory;
  FProjectionInfo := AProjectionInfo;
  FVersionConfig := AVersionConfig;
  FLoaderFromStorage := ALoaderFromStorage;
end;

function TBitmapTileProviderByStorage.GetChangeNotifier: INotifierTilePyramidUpdate;
begin
  Result := FStorage.TileNotifier;
end;

function TBitmapTileProviderByStorage.GetProjectionInfo: IProjectionInfo;
begin
  Result := FProjectionInfo;
end;

function TBitmapTileProviderByStorage.GetTile(const ATile: TPoint): IBitmap32Static;
var
  VTileInfo: ITileInfoWithData;
  VRect: TRect;
  VSize: TPoint;
  VBitmap: TBitmap32ByStaticBitmap;
  VResampler: TCustomResampler;
  VZoom: Byte;
begin
  Result := nil;
  try
    VZoom := FProjectionInfo.Zoom;
    if Supports(FStorage.GetTileInfo(ATile, VZoom, FVersionConfig.Version, gtimWithData), ITileInfoWithData, VTileInfo) then begin
      Result := FLoaderFromStorage.Load(VTileInfo.TileData);
    end;
    if Result <> nil then begin
      VRect := FProjectionInfo.GeoConverter.TilePos2PixelRect(ATile, VZoom);
      VSize := Types.Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
      if (Result.Size.X <> VSize.X) or
        (Result.Size.Y <> VSize.Y) then begin
        VResampler := FImageResamplerConfig.GetActiveFactory.CreateResampler;
        try
          VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
          try
            VBitmap.SetSize(VSize.X, VSize.Y);
            StretchTransferFull(
              VBitmap,
              VBitmap.BoundsRect,
              Result,
              VResampler,
              dmOpaque
            );
            Result := VBitmap.BitmapStatic;
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
  const AProjectionInfo: IProjectionInfo;
  const AStorage: ITileStorage);
begin
  Assert(AVectorDataFactory <> nil);
  Assert(AVersionConfig <> nil);
  Assert(ALoaderFromStorage <> nil);
  Assert(AStorage <> nil);
  Assert(AProjectionInfo <> nil);
  Assert(AStorage.CoordConverter.IsSameConverter(AProjectionInfo.GeoConverter));
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FVectorDataFactory := AVectorDataFactory;
  FStorage := AStorage;
  FProjectionInfo := AProjectionInfo;
  FVersionConfig := AVersionConfig;
  FLoaderFromStorage := ALoaderFromStorage;
end;

function TVectorTileProviderByStorage.GetChangeNotifier: INotifierTilePyramidUpdate;
begin
  Result := FStorage.TileNotifier;
end;

function TVectorTileProviderByStorage.GetProjectionInfo: IProjectionInfo;
begin
  Result := FProjectionInfo;
end;

function TVectorTileProviderByStorage.GetTile(const ATile: TPoint): IVectorDataItemList;
var
  VTileInfo: ITileInfoWithData;
  VZoom: Byte;
begin
  Result := nil;
  try
    VZoom := FProjectionInfo.Zoom;
    if Supports(FStorage.GetTileInfo(ATile, VZoom, FVersionConfig.Version, gtimWithData), ITileInfoWithData, VTileInfo) then begin
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

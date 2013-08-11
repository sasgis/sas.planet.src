unit u_MapLayerVectorMaps;

interface

uses
  Types,
  SysUtils,
  GR32_Image,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_InterfaceListSimple,
  i_InternalPerformanceCounter,
  i_MapTypes,
  i_KmlLayerConfig,
  i_Bitmap32StaticFactory,
  i_TileError,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_VectorItemsFactory,
  i_ImageResamplerConfig,
  i_IdCacheSimple,
  i_VectorItemSubsetChangeable,
  i_FindVectorItems,
  u_TiledLayerWithThreadBase;

type
  TMapLayerVectorMaps = class(TTiledLayerWithThreadBase, IFindVectorItems)
  private
    FConfig: IKmlLayerConfig;
    FVectorItemsFactory: IVectorItemsFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FLayersSet: IMapTypeSetChangeable;
    FErrorLogger: ITileErrorLogger;

    FProjectedCache: IIdCacheSimple;

    FVectorItems: IVectorItemSubsetChangeable;

    procedure OnConfigChange;
    procedure OnLayerSetChange;
    procedure OnItemsUpdated;

    function MouseOnElements(
      const AVisualConverter: ILocalCoordConverter;
      const ACopiedElements: IVectorItemSubset;
      const xy: TPoint
    ): IVectorItemSubset;
  protected
    function CreateLayerProvider(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALayerConverter: ILocalCoordConverter
    ): IBitmapLayerProvider; override;
    procedure StartThreads; override;
  private
    function FindItems(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IVectorItemSubset;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
      const AView: ILocalCoordConverterChangeable;
      const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ATimerNoifier: INotifierTime;
      const AErrorLogger: ITileErrorLogger;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AConfig: IKmlLayerConfig;
      const ALayersSet: IMapTypeSetChangeable
    );
  end;

implementation

uses
  GR32,
  t_GeoTypes,
  i_CoordConverter,
  i_TileMatrix,
  i_VectorItemProjected,
  i_LonLatRect,
  i_VectorItemDrawConfig,
  u_TileMatrixFactory,
  u_ListenerByEvent,
  u_IdCacheSimpleThreadSafe,
  u_VectorDataItemSubset,
  u_InterfaceListSimple,
  u_VectorItemSubsetChangeableForVectorLayers,
  u_BitmapLayerProviderByVectorSubset;

{ TWikiLayerNew }

constructor TMapLayerVectorMaps.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ATimerNoifier: INotifierTime;
  const AErrorLogger: ITileErrorLogger;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AConfig: IKmlLayerConfig;
  const ALayersSet: IMapTypeSetChangeable
);
var
  VTileMatrixFactory: ITileMatrixFactory;
begin
  VTileMatrixFactory :=
    TTileMatrixFactory.Create(
      ATileMatrixDraftResamplerConfig,
      ABitmapFactory,
      AConverterFactory
    );
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    APosition,
    AView,
    VTileMatrixFactory,
    ATimerNoifier,
    False,
    AConfig.ThreadConfig
  );
  FConfig := AConfig;
  FVectorItemsFactory := AVectorItemsFactory;
  FBitmapFactory := ABitmapFactory;
  FLayersSet := ALayersSet;
  FErrorLogger := AErrorLogger;

  FProjectedCache := TIdCacheSimpleThreadSafe.Create;

  FVectorItems :=
    TVectorItemSubsetChangeableForVectorLayers.Create(
      PerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      APosition,
      ALayersSet,
      AErrorLogger,
      FConfig.ThreadConfig
    );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerSetChange),
    FLayersSet.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnItemsUpdated),
    FVectorItems.ChangeNotifier
  );
end;

function TMapLayerVectorMaps.CreateLayerProvider(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALayerConverter: ILocalCoordConverter
): IBitmapLayerProvider;
var
  VConfig: IVectorItemDrawConfigStatic;
begin
  Result := nil;
  VConfig := FConfig.DrawConfig.GetStatic;

  Result :=
    TBitmapLayerProviderByVectorSubset.Create(
      VConfig.MainColor,
      VConfig.ShadowColor,
      VConfig.PointColor,
      FVectorItemsFactory,
      FBitmapFactory,
      FProjectedCache,
      FVectorItems
    );
end;

function TMapLayerVectorMaps.FindItems(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IVectorItemSubset;
var
  VElements: IVectorItemSubset;
begin
  Result := nil;
  if Visible then begin
    VElements := FVectorItems.GetStatic;
    if VElements <> nil then begin
      Result := MouseOnElements(AVisualConverter, VElements, ALocalPoint);
    end;
  end;
end;

function TMapLayerVectorMaps.MouseOnElements(
  const AVisualConverter: ILocalCoordConverter;
  const ACopiedElements: IVectorItemSubset;
  const xy: TPoint
): IVectorItemSubset;
var
  VRect: TRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  i: integer;
  VItem: IVectorDataItemSimple;
  VProjectdPath: IProjectedPath;
  VItemLine: IVectorDataItemLine;
  VItemPoly: IVectorDataItemPoly;
  VProjectdPolygon: IProjectedPolygon;
  Vtmp: IInterfaceListSimple;
begin
  Result := nil;
  Vtmp := TInterfaceListSimple.Create;

  if ACopiedElements.Count > 0 then begin
    VRect.Left := xy.X - 3;
    VRect.Top := xy.Y - 3;
    VRect.Right := xy.X + 3;
    VRect.Bottom := xy.Y + 3;

    VConverter := AVisualConverter.GetGeoConverter;
    VZoom := AVisualConverter.GetZoom;
    VMapRect := AVisualConverter.LocalRect2MapRectFloat(VRect);
    VConverter.CheckPixelRectFloat(VMapRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
    VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(xy);

    // check element
    for i := 0 to ACopiedElements.Count - 1 do begin
      VItem := ACopiedElements.GetItem(i);
      if VItem.LLRect.IsIntersecWithRect(VLonLatRect) then begin
        if Supports(VItem, IVectorDataItemPoint) then begin
          Vtmp.add(VItem);
        end else if Supports(VItem, IVectorDataItemLine, VItemLine) then begin
          if Supports(FProjectedCache.GetByID(Integer(VItemLine)), IProjectedPath, VProjectdPath) then begin
            if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
              Vtmp.add(VItem);
            end;
          end;
        end else if Supports(VItem, IVectorDataItemPoly, VItemPoly) then begin
          if Supports(FProjectedCache.GetByID(Integer(VItemPoly)), IProjectedPolygon, VProjectdPolygon) then begin
            if VProjectdPolygon.IsPointInPolygon(VPixelPos) then begin
              Vtmp.add(VItem);
            end;
          end;
        end;
      end;
    end;
  end;
  Result := TVectorItemSubset.Create(Vtmp.MakeStaticAndClear);
end;

procedure TMapLayerVectorMaps.OnConfigChange;
var
  VVectorMapsSet: IMapTypeSet;
begin
  VVectorMapsSet := FLayersSet.GetStatic;
  ViewUpdateLock;
  try
    Visible := (VVectorMapsSet <> nil) and (VVectorMapsSet.GetCount > 0);
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerVectorMaps.OnItemsUpdated;
begin
  ViewUpdateLock;
  try
    DelicateRedrawWithFullUpdate;
    FProjectedCache.Clear;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerVectorMaps.OnLayerSetChange;
begin
  ViewUpdateLock;
  try
    OnConfigChange;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerVectorMaps.StartThreads;
begin
  inherited;
  OnLayerSetChange;
  OnConfigChange;
end;

end.

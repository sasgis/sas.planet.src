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
  i_InternalPerformanceCounter,
  i_MapTypeSetChangeable,
  i_KmlLayerConfig,
  i_Bitmap32StaticFactory,
  i_TileError,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_ImageResamplerConfig,
  i_VectorItemSubsetBuilder,
  i_VectorItemSubsetChangeable,
  i_FindVectorItems,
  i_ProjectedGeometryProvider,
  u_TiledLayerWithThreadBase;

type
  TMapLayerVectorMaps = class(TTiledLayerWithThreadBase, IFindVectorItems)
  private
    FConfig: IKmlLayerConfig;
    FBitmapFactory: IBitmap32StaticFactory;
    FLayersSet: IMapTypeSetChangeable;
    FErrorLogger: ITileErrorLogger;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FProjectedProvider: IProjectedGeometryProvider;

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
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AProjectedProvider: IProjectedGeometryProvider;
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
  i_MapTypeSet,
  i_VectorItemDrawConfig,
  u_TileMatrixFactory,
  u_ListenerByEvent,
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
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AProjectedProvider: IProjectedGeometryProvider;
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
  FBitmapFactory := ABitmapFactory;
  FLayersSet := ALayersSet;
  FErrorLogger := AErrorLogger;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FProjectedProvider := AProjectedProvider;

  FVectorItems :=
    TVectorItemSubsetChangeableForVectorLayers.Create(
      PerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      APosition,
      ALayersSet,
      AErrorLogger,
      AVectorItemSubsetBuilderFactory,
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
      FBitmapFactory,
      FProjectedProvider,
      FVectorItems.GetStatic
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
  Vtmp: IVectorItemSubsetBuilder;
begin
  Result := nil;
  Vtmp := FVectorItemSubsetBuilderFactory.Build;

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
          VProjectdPath := FProjectedProvider.GetProjectedPath(AVisualConverter.ProjectionInfo,  VItemLine.Line);
          if Assigned(VProjectdPath) then begin
            if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
              Vtmp.add(VItem);
            end;
          end;
        end else if Supports(VItem, IVectorDataItemPoly, VItemPoly) then begin
          VProjectdPolygon := FProjectedProvider.GetProjectedPolygon(AVisualConverter.ProjectionInfo,  VItemPoly.Line);
          if Assigned(VProjectdPolygon) then begin
            if VProjectdPolygon.IsPointInPolygon(VPixelPos) then begin
              Vtmp.add(VItem);
            end;
          end;
        end;
      end;
    end;
  end;
  Result := Vtmp.MakeStaticAndClear;
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
    SetNeedUpdateLayerProvider;
    DelicateRedrawWithFullUpdate;
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

unit u_MapLayerMarks;

interface

uses
  Types,
  SysUtils,
  GR32_Image,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_InternalPerformanceCounter,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarksLayerConfig,
  i_VectorDataItemSimple,
  i_VectorItemSubsetBuilder,
  i_Bitmap32StaticFactory,
  i_ImageResamplerConfig,
  i_MarkerProviderForVectorItem,
  i_FindVectorItems,
  i_VectorItemSubset,
  i_VectorItemSubsetChangeable,
  i_ProjectedGeometryProvider,
  i_Mark,
  i_MarkSystem,
  u_TiledLayerWithThreadBase;

type
  TMapLayerMarks = class(TTiledLayerWithThreadBase, IFindVectorItems)
  private
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FProjectedCache: IProjectedGeometryProvider;

    FMouseOnRegCounter: IInternalPerformanceCounter;
    FMarksSubset: IVectorItemSubsetChangeable;
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
      const AProjectedCache: IProjectedGeometryProvider;
      const AMarkerProvider: IMarkerProviderForVectorItem;
      const ATimerNoifier: INotifierTime;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AConfig: IMarksLayerConfig;
      const AMarkDB: IMarkSystem
    );
  end;

implementation

uses
  ActiveX,
  t_GeoTypes,
  i_CoordConverter,
  i_TileMatrix,
  i_VectorItemProjected,
  i_InterfaceListStatic,
  i_BitmapLayerProviderChangeable,
  u_TileMatrixFactory,
  u_ListenerByEvent,
  u_Synchronizer,
  u_VectorItemSubsetChangeableForMarksLayer,
  u_BitmapLayerProviderChangeableForMarksLayer;

{ TMapMarksLayerNew }

constructor TMapLayerMarks.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixDraftResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AProjectedCache: IProjectedGeometryProvider;
  const AMarkerProvider: IMarkerProviderForVectorItem;
  const ATimerNoifier: INotifierTime;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AConfig: IMarksLayerConfig;
  const AMarkDB: IMarkSystem
);
var
  VTileMatrixFactory: ITileMatrixFactory;
  VProvider: IBitmapLayerProviderChangeable;
begin
  VTileMatrixFactory :=
    TTileMatrixFactory.Create(
      ATileMatrixDraftResamplerConfig,
      ABitmapFactory,
      AConverterFactory
    );
  FMarksSubset :=
    TVectorItemSubsetChangeableForMarksLayer.Create(
      APerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      APosition,
      AMarkDB,
      AConfig.MarksShowConfig,
      AConfig.ThreadConfig
    );
  VProvider :=
    TBitmapLayerProviderChangeableForMarksLayer.Create(
      AConfig.MarksDrawConfig,
      ABitmapFactory,
      AProjectedCache,
      AMarkerProvider,
      FMarksSubset
    );
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    APosition,
    AView,
    VTileMatrixFactory,
    VProvider,
    nil,
    ATimerNoifier,
    AConfig.ThreadConfig
  );
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FProjectedCache := AProjectedCache;

  FMouseOnRegCounter := APerfList.CreateAndAddNewCounter('MouseOnReg');
end;

function TMapLayerMarks.FindItems(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IVectorItemSubset;
var
  VLonLatRect: TDoubleRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VPixelPos: TDoublePoint;
  VZoom: Byte;
  VMark: IMark;
  VMapRect: TDoubleRect;
  VMarksSubset: IVectorItemSubset;
  VMarksEnum: IEnumUnknown;
  i: Cardinal;
  VCounterContext: TInternalPerformanceCounterContext;
  VMarkLine: IVectorDataItemLine;
  VMarkPoly: IVectorDataItemPoly;
  VProjectdPath: IProjectedPath;
  VProjectdPolygon: IProjectedPolygon;
  Vtmp: IVectorItemSubsetBuilder;
begin
  Result := nil;
  Vtmp := FVectorItemSubsetBuilderFactory.Build;
  VCounterContext := FMouseOnRegCounter.StartOperation;
  try
    VMarksSubset := FMarksSubset.GetStatic;

    if VMarksSubset <> nil then begin
      if not VMarksSubset.IsEmpty then begin
        VRect.Left := ALocalPoint.X - 8;
        VRect.Top := ALocalPoint.Y - 16;
        VRect.Right := ALocalPoint.X + 8;
        VRect.Bottom := ALocalPoint.Y + 16;
        VConverter := AVisualConverter.GetGeoConverter;
        VZoom := AVisualConverter.GetZoom;
        VMapRect := AVisualConverter.LocalRect2MapRectFloat(VRect);
        VConverter.CheckPixelRectFloat(VMapRect, VZoom);
        VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
        VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(ALocalPoint);
        VMarksEnum := VMarksSubset.GetEnum;
        while VMarksEnum.Next(1, VMark, @i) = S_OK do begin
          if VMark.LLRect.IsIntersecWithRect(VLonLatRect) then begin
            if Supports(VMark, IMarkPoint) then begin
              Vtmp.add(VMark);
            end else begin
              if Supports(VMark, IVectorDataItemLine, VMarkLine) then begin
                VProjectdPath := FProjectedCache.GetProjectedPath(AVisualConverter.ProjectionInfo, VMarkLine.Line);
                if Assigned(VProjectdPath) then begin
                  if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
                    Vtmp.Add(VMark);
                  end;
                end;
              end else if Supports(VMark, IVectorDataItemPoly, VMarkPoly) then begin
                VProjectdPolygon := FProjectedCache.GetProjectedPolygon(AVisualConverter.ProjectionInfo, VMarkPoly.Line);
                if Assigned(VProjectdPolygon) then begin
                  if VProjectdPolygon.IsPointInPolygon(VPixelPos) or
                    VProjectdPolygon.IsPointOnBorder(VPixelPos, 3) then begin
                    Vtmp.Add(VMark);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    FMouseOnRegCounter.FinishOperation(VCounterContext);
  end;
  Result := Vtmp.MakeStaticAndClear;
end;

end.

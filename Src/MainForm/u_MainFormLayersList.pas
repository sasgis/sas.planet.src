unit u_MainFormLayersList;

interface

uses
  GR32_Image,
  i_InterfaceListStatic,
  i_NotifierOperation,
  i_HashFunction,
  i_Bitmap32BufferFactory,
  i_ConfigDataProvider,
  i_ContentTypeManager,
  i_LocalCoordConverterFactorySimpe,
  i_VectorItemSubsetBuilder,
  i_GeometryLonLatFactory,
  i_GeometryProjectedFactory,
  i_GeometryProjectedProvider,
  i_ProjectionSetChangeable,
  i_ViewPortState,
  i_MainMapsState,
  i_MainFormLayersConfig,
  i_ImageResamplerFactory,
  i_BitmapPostProcessing,
  i_TileError,
  i_TileErrorLogProviedrStuped,
  i_LastSearchResult,
  i_PopUp,
  i_FillingMapPolygon,
  i_InternalPerformanceCounter,
  i_MarkSystem,
  i_TerrainProviderList,
  i_LanguageManager,
  i_NotifierTime,
  i_GPSRecorder,
  i_DownloadInfoSimple,
  i_GlobalInternetState,
  i_LastSelectionInfo,
  i_GlobalConfig,
  i_FindVectorItems,
  i_MergePolygonsResult,
  i_LineOnMapEdit,
  i_MouseState,
  i_MainFormState,
  i_SelectionRect,
  i_MapViewGoto,
  i_NavigationToPoint,
  i_PointOnMapEdit,
  i_SunCalcConfig,
  i_SunCalcProvider,
  i_CoordToStringConverter,
  i_ValueToStringConverter,
  u_BaseInterfacedObject;

type
  IMainFormLayersList = interface
    ['{D48DA854-9B28-4CE4-A51E-CB4618CE0819}']
    function GetWikiLayer: IFindVectorItems;
    property WikiLayer: IFindVectorItems read GetWikiLayer;

    function GetMarksLayer: IFindVectorItems;
    property MarksLayer: IFindVectorItems read GetMarksLayer;

    function GetSearchResultsLayer: IFindVectorItems;
    property SearchResultsLayer: IFindVectorItems read GetSearchResultsLayer;
  end;

type
  TMainFormLayersList = class(TBaseInterfacedObject, IMainFormLayersList)
  private
    FLayersList: IInterfaceListStatic;

    FWikiLayer: IFindVectorItems;
    FLayerMapMarks: IFindVectorItems;
    FLayerSearchResults: IFindVectorItems;

    function GetWikiLayer: IFindVectorItems;
    function GetMarksLayer: IFindVectorItems;
    function GetSearchResultsLayer: IFindVectorItems;
  public
    constructor Create(
      AParentMap: TImage32;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AResourceProvider: IConfigDataProvider;
      const ALanguageManager: ILanguageManager;
      const AGlobalConfig: IGlobalConfig;
      const AContentTypeManager: IContentTypeManager;
      const AGUISyncronizedTimerNotifier: INotifierTime;
      const ALastSearchResult: ILastSearchResult;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectedGeometryProvider: IGeometryProjectedProvider;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AHashFunction: IHashFunction;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AImageResamplerFactoryList: IImageResamplerFactoryList;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AGpsTrackRecorder: IGpsTrackRecorder;
      const AGPSRecorder: IGPSRecorder;
      const ATerrainProviderList: ITerrainProviderList;
      const ALastSelectionInfo: ILastSelectionInfo;
      const ADownloadInfo: IDownloadInfoSimple;
      const AGlobalInternetState: IGlobalInternetState;
      const AMarkSystem: IMarkSystem;
      const ALayersConfig: IMainFormLayersConfig;
      const AActiveProjectionSet: IProjectionSetChangeable;
      const AViewPortState: IViewPortState;
      const AMainFormState: IMainFormState;
      const AMouseState: IMouseState;
      const AMainMapState: IMainMapsState;
      const AFillingMapPolygon: IFillingMapPolygon;
      const AMergePolygonsResult: IMergePolygonsResult;
      const ACalcLinePath: IPathOnMapEdit;
      const ACircleOnMapEdit: ICircleOnMapEdit;
      const AEditLinePath: IPathOnMapEdit;
      const AEditPolygon: IPolygonOnMapEdit;
      const ASelectPolygon: IPolygonOnMapEdit;
      const ASelectLinePath: IPathOnMapEdit;
      const ASelectionRect: ISelectionRect;
      const AMapGoto: IMapViewGoto;
      const ANavToPoint: INavigationToPoint;
      const APointOnMapEdit: IPointOnMapEdit;
      const ATileErrorLogProvider: ITileErrorLogProviedrStuped;
      const ATileErrorLogger: ITileErrorLogger;
      const APerfListGroup: IInternalPerformanceCounterList;
      const ASunCalcConfig: ISunCalcConfig;
      const ASunCalcProvider: ISunCalcProvider;
      const ASunCalcPopupMenu: IPopUp;
      const AStatBarPopupMenu: IPopUp;
      const AScaleLinePopupMenu: IPopUp;
      const AMiniMapPopupMenu: IPopUp
    );
  end;


implementation

uses
  Types,
  t_GeoTypes,
  i_Bitmap32Static,
  i_MarkerDrawable,
  i_StringListChangeable,
  i_LocalCoordConverterChangeable,
  i_MarkerProviderForVectorItem,
  i_InterfaceListSimple,
  i_VectorItemSubsetChangeable,
  i_ImageResamplerFactoryChangeable,
  i_TileRectChangeable,
  i_BitmapLayerProviderChangeable,
  i_ObjectWithListener,
  i_BitmapTileMatrixChangeable,
  i_VectorTileRendererChangeable,
  i_VectorTileProviderChangeable,
  i_VectorTileMatrixChangeable,
  i_GeometryLonLatChangeable,
  u_InterfaceListSimple,
  u_TileRectChangeableByLocalConverter,
  u_GeometryLonLatLineChangeableByPathEdit,
  u_GeometryLonLatPolygonChangeableByPolygonEdit,
  u_GeometryLonLatPolygonChangeableByLastSelection,
  u_GeometryLonLatPolygonChangeableByMergePolygonsResult,
  u_GeometryLonLatPolygonChangeableByLineChangeable,
  u_LocalConverterChangeableOfMiniMap,
  u_ImageResamplerFactoryChangeableByConfig,
  u_BitmapLayerProviderChangeableForMainLayer,
  u_BitmapChangeableFaked,
  u_SourceDataUpdateInRectByMapsSet,
  u_SourceDataUpdateInRectByFillingMap,
  u_BitmapTileMatrixChangeableWithThread,
  u_BitmapLayerProviderChangeableForGrids,
  u_BitmapLayerProviderChangeableForFillingMap,
  u_BitmapLayerProviderChangeableForGpsTrack,
  u_VectorTileProviderChangeableForVectorLayers,
  u_VectorTileProviderChangeableForLastSearchResult,
  u_VectorTileMatrixChangeableForVectorLayers,
  u_BitmapTileMatrixChangeableByVectorMatrix,
  u_BitmapTileMatrixChangeableComposite,
  u_VectorTileMatrixChangeableByVectorSubsetChangeable,
  u_VectorItemSubsetChangeableForMarksLayer,
  u_FindVectorItemsForVectorTileMatrix,
  u_VectorTileRendererChangeableForVectorMaps,
  u_VectorTileRendererChangeableForMarksLayer,
  u_MarkerDrawableChangeableFaked,
  u_MarkerDrawableByBitmap32Static,
  u_MarkerDrawableChangeableSimple,
  u_MarkerDrawableSimpleSquare,
  u_MarkerDrawableSimpleArrow,
  u_MarkerDrawableSimpleCross,
  u_MarkerDrawableCenterScale,
  u_MarkerProviderForVectorItemForMarkPoints,
  u_MarkerProviderForVectorItemWithCache,
  u_ActiveMapsLicenseList,
  u_TiledMapLayer,
  u_MapLayerGPSMarker,
  u_MapLayerGPSMarkerRings,
  u_MapLayerSingleGeometry,
  u_MapLayerPointsSet,
  u_MapLayerCalcLineCaptions,
  u_MapLayerCalcCircleCaptions,
  u_MapLayerSelectionByRect,
  u_MapLayerGotoMarker,
  u_MapLayerNavToMark,
  u_MapLayerTileErrorInfo,
  u_MapLayerPointOnMapEdit,
  u_WindowLayerFullMapMouseCursor,
  u_WindowLayerCenterScale,
  u_WindowLayerScaleLineHorizontal,
  u_WindowLayerScaleLineVertical,
  u_WindowLayerLicenseList,
  u_WindowLayerStatusBar,
  u_WindowLayerSunCalcYearInfo,
  u_WindowLayerSunCalcDayInfo,
  u_WindowLayerSunCalcTimeInfo,
  u_WindowLayerSunCalcYearTimeLine,
  u_WindowLayerSunCalcDayTimeLine,
  u_WindowLayerSunCalcDetailsPanel,
  u_MiniMapLayerViewRect,
  u_MiniMapLayerTopBorder,
  u_MiniMapLayerLeftBorder,
  u_MiniMapLayerMinusButton,
  u_MiniMapLayerPlusButton,
  u_ConfigProviderHelpers,
  u_GeoFunc,
  u_Synchronizer;

{ TMainFormLayersList }

constructor TMainFormLayersList.Create(
  AParentMap: TImage32;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AResourceProvider: IConfigDataProvider;
  const ALanguageManager: ILanguageManager;
  const AGlobalConfig: IGlobalConfig;
  const AContentTypeManager: IContentTypeManager;
  const AGUISyncronizedTimerNotifier: INotifierTime;
  const ALastSearchResult: ILastSearchResult;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectedGeometryProvider: IGeometryProjectedProvider;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AHashFunction: IHashFunction;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AImageResamplerFactoryList: IImageResamplerFactoryList;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AGpsTrackRecorder: IGpsTrackRecorder;
  const AGPSRecorder: IGPSRecorder;
  const ATerrainProviderList: ITerrainProviderList;
  const ALastSelectionInfo: ILastSelectionInfo;
  const ADownloadInfo: IDownloadInfoSimple;
  const AGlobalInternetState: IGlobalInternetState;
  const AMarkSystem: IMarkSystem;
  const ALayersConfig: IMainFormLayersConfig;
  const AActiveProjectionSet: IProjectionSetChangeable;
  const AViewPortState: IViewPortState;
  const AMainFormState: IMainFormState;
  const AMouseState: IMouseState;
  const AMainMapState: IMainMapsState;
  const AFillingMapPolygon: IFillingMapPolygon;
  const AMergePolygonsResult: IMergePolygonsResult;
  const ACalcLinePath: IPathOnMapEdit;
  const ACircleOnMapEdit: ICircleOnMapEdit;
  const AEditLinePath: IPathOnMapEdit;
  const AEditPolygon: IPolygonOnMapEdit;
  const ASelectPolygon: IPolygonOnMapEdit;
  const ASelectLinePath: IPathOnMapEdit;
  const ASelectionRect: ISelectionRect;
  const AMapGoto: IMapViewGoto;
  const ANavToPoint: INavigationToPoint;
  const APointOnMapEdit: IPointOnMapEdit;
  const ATileErrorLogProvider: ITileErrorLogProviedrStuped;
  const ATileErrorLogger: ITileErrorLogger;
  const APerfListGroup: IInternalPerformanceCounterList;
  const ASunCalcConfig: ISunCalcConfig;
  const ASunCalcProvider: ISunCalcProvider;
  const ASunCalcPopupMenu: IPopUp;
  const AStatBarPopupMenu: IPopUp;
  const AScaleLinePopupMenu: IPopUp;
  const AMiniMapPopupMenu: IPopUp
);
var
  VBitmap: IBitmap32Static;
  VMarkerChangeable: IMarkerDrawableChangeable;
  VMarkerWithDirectionChangeable: IMarkerDrawableWithDirectionChangeable;
  VLicensList: IStringListChangeable;
  VMiniMapConverterChangeable: ILocalCoordConverterChangeable;
  VBitmapChangeable: IBitmapChangeable;
  VMarkerProviderForVectorItem: IMarkerProviderForVectorItem;
  VLayersList: IInterfaceListSimple;
  VVectorItems: IVectorItemSubsetChangeable;
  VPerfList: IInternalPerformanceCounterList;
  VTileMatrixDraftResampler: IImageResamplerFactoryChangeable;
  VTileRectForShow: ITileRectChangeable;
  VProvider: IBitmapLayerProviderChangeable;
  VSourceChangeNotifier: IObjectWithListener;
  VTileMatrix: IBitmapTileMatrixChangeable;
  VVectorRenderer: IVectorTileRendererChangeable;
  VVectorTileProvider: IVectorTileUniProviderChangeable;
  VVectorTileMatrix: IVectorTileMatrixChangeable;
  VLayer: IInterface;
  VDebugName: string;
  VVectorOversizeRect: TRect;
  VMatrixList: IInterfaceListSimple;
  VPolygonChangeable: IGeometryLonLatPolygonChangeable;
  VGeometryChangeableByPathEdit: TGeometryLonLatChangeableByPathEdit;
  VGeometryChangeableByPolygonEdit: TGeometryLonLatChangeableByPolygonEdit;
begin
  inherited Create;

  VTileRectForShow :=
    TTileRectChangeableByLocalConverterSmart.Create(
      AActiveProjectionSet,
      AViewPortState.View,
      GSync.SyncVariable.Make('TileRectForShowMain'),
      GSync.SyncVariable.Make('TileRectForShowResult')
    );

  VTileMatrixDraftResampler :=
    TImageResamplerFactoryChangeableByConfig.Create(
      AGlobalConfig.TileMatrixDraftResamplerConfig,
      AImageResamplerFactoryList
    );

  VLayersList := TInterfaceListSimple.Create;

  VMatrixList := TInterfaceListSimple.Create;
  // Main bitmap layer
  VDebugName := 'MainBitmapMaps';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VProvider :=
    TBitmapLayerProviderChangeableForMainLayer.Create(
      AMainMapState.ActiveMap,
      AMainMapState.ActiveBitmapLayersList,
      ABitmapPostProcessing,
      ALayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig,
      ABitmap32StaticFactory,
      ATileErrorLogger
    );

  VSourceChangeNotifier :=
    TSourceDataUpdateInRectByMapsSet.Create(
      AMainMapState.ActiveBitmapMapsSet
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableWithThread.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      VTileRectForShow,
      VTileMatrixDraftResampler,
      True,
      ABitmap32StaticFactory,
      AHashFunction,
      VProvider,
      VSourceChangeNotifier,
      ALayersConfig.MainMapLayerConfig.ThreadConfig,
      VDebugName
    );
  VMatrixList.Add(VTileMatrix);

  // Bitmap layer with grids
  VDebugName := 'Grids';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VProvider :=
    TBitmapLayerProviderChangeableForGrids.Create(
      ABitmap32StaticFactory,
      AActiveProjectionSet,
      ACoordToStringConverter,
      ALayersConfig.MapLayerGridsConfig
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableWithThread.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      VTileRectForShow,
      VTileMatrixDraftResampler,
      False,
      ABitmap32StaticFactory,
      AHashFunction,
      VProvider,
      nil,
      ALayersConfig.MapLayerGridsConfig.ThreadConfig,
      VDebugName
    );
  VMatrixList.Add(VTileMatrix);

  // Layer with randered vector maps
  VDebugName := 'VectorMaps';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VVectorOversizeRect := Rect(10, 10, 10, 10);

  VBitmap :=
    ReadBitmapByFileRef(
      AResourceProvider,
      'PANORAMIO.png',
      AContentTypeManager,
      nil
    );
  if VBitmap <> nil then begin
    VMarkerChangeable :=
      TMarkerDrawableChangeableFaked.Create(
        TMarkerDrawableByBitmap32Static.Create(
          VBitmap, DoublePoint(VBitmap.Size.X / 2, VBitmap.Size.Y / 2)
        )
      );
  end else begin
    VMarkerChangeable :=
      TMarkerDrawableChangeableSimple.Create(
        TMarkerDrawableSimpleSquare,
        ALayersConfig.KmlLayerConfig.PointMarkerConfig
      );
  end;

  VVectorTileProvider :=
    TVectorTileProviderChangeableForVectorLayers.Create(
      AMainMapState.ActiveKmlLayersSet,
      AVectorItemSubsetBuilderFactory,
      ATileErrorLogger,
      Rect(300, 300, 300, 300),
      VVectorOversizeRect
    );
  VSourceChangeNotifier :=
    TSourceDataUpdateInRectByMapsSet.Create(
      AMainMapState.ActiveKmlLayersSet
    );
  VVectorTileMatrix :=
    TVectorTileMatrixChangeableForVectorLayers.Create(
      VPerfList.CreateAndAddNewSubList('VectorMatrix'),
      AAppStartedNotifier,
      AAppClosingNotifier,
      VTileRectForShow,
      AHashFunction,
      AVectorItemSubsetBuilderFactory,
      False,
      VVectorTileProvider,
      VSourceChangeNotifier,
      ALayersConfig.KmlLayerConfig.ThreadConfig,
      VVectorOversizeRect,
      VDebugName
    );
  FWikiLayer :=
    TFindVectorItemsForVectorTileMatrix.Create(
      AVectorItemSubsetBuilderFactory,
      AProjectedGeometryProvider,
      VVectorTileMatrix,
      VPerfList.CreateAndAddNewCounter('FindItems'),
      6
    );
  VVectorRenderer :=
    TVectorTileRendererChangeableForVectorMaps.Create(
      ALayersConfig.KmlLayerConfig.DrawConfig,
      VMarkerChangeable,
      ABitmap32StaticFactory,
      AProjectedGeometryProvider
    );

  VTileMatrix :=
    TBitmapTileMatrixChangeableByVectorMatrix.Create(
      VPerfList.CreateAndAddNewSubList('BitmapMatrix'),
      AAppStartedNotifier,
      AAppClosingNotifier,
      VVectorTileMatrix,
      VVectorRenderer,
      VTileMatrixDraftResampler,
      True,
      ABitmap32StaticFactory,
      AHashFunction,
      ALayersConfig.KmlLayerConfig.ThreadConfig,
      VDebugName
    );
  VMatrixList.Add(VTileMatrix);

  // Filling map layer
  VDebugName := 'FillingMap';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VProvider :=
    TBitmapLayerProviderChangeableForFillingMap.Create(
      ABitmap32StaticFactory,
      AVectorGeometryProjectedFactory,
      AMainMapState.FillingMapActiveMap,
      AFillingMapPolygon,
      ALayersConfig.FillingMapLayerConfig
    );
  VSourceChangeNotifier :=
    TSourceDataUpdateInRectByFillingMap.Create(
      AMainMapState.FillingMapActiveMap,
      ALayersConfig.FillingMapLayerConfig
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableWithThread.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      VTileRectForShow,
      VTileMatrixDraftResampler,
      True,
      ABitmap32StaticFactory,
      AHashFunction,
      VProvider,
      VSourceChangeNotifier,
      ALayersConfig.FillingMapLayerConfig.ThreadConfig,
      VDebugName
    );
  VMatrixList.Add(VTileMatrix);

  // Marks from MarkSystem
  VDebugName := 'Marks';
  VVectorOversizeRect := ALayersConfig.MarksLayerConfig.MarksDrawConfig.DrawOrderConfig.OverSizeRect;
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VVectorItems :=
    TVectorItemSubsetChangeableForMarksLayer.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      VTileRectForShow,
      AMarkSystem,
      ALayersConfig.MarksLayerConfig.MarksShowConfig,
      VVectorOversizeRect,
      ALayersConfig.MarksLayerConfig.ThreadConfig
    );
  VVectorTileMatrix :=
    TVectorTileMatrixChangeableByVectorSubsetChangeable.Create(
      VPerfList.CreateAndAddNewSubList('VectorMatrix'),
      AAppStartedNotifier,
      AAppClosingNotifier,
      VTileRectForShow,
      AHashFunction,
      AVectorItemSubsetBuilderFactory,
      False,
      VVectorItems,
      ALayersConfig.MarksLayerConfig.ThreadConfig,
      VVectorOversizeRect,
      VDebugName
    );
  FLayerMapMarks :=
    TFindVectorItemsForVectorTileMatrix.Create(
      AVectorItemSubsetBuilderFactory,
      AProjectedGeometryProvider,
      VVectorTileMatrix,
      VPerfList.CreateAndAddNewCounter('FindItems'),
      24
    );

  VBitmap :=
    ReadBitmapByFileRef(
      AResourceProvider,
      'RED.png',
      AContentTypeManager,
      nil
    );
  VMarkerChangeable := nil;
  if VBitmap <> nil then begin
    VMarkerChangeable :=
      TMarkerDrawableChangeableFaked.Create(
        TMarkerDrawableByBitmap32Static.Create(VBitmap, DoublePoint(VBitmap.Size.X / 2, VBitmap.Size.Y))
      );
  end;
  VMarkerProviderForVectorItem :=
    TMarkerProviderForVectorItemWithCache.Create(
      VPerfList.CreateAndAddNewSubList('Marker'),
      AHashFunction,
      TMarkerProviderForVectorItemForMarkPoints.Create(ABitmap32StaticFactory, VMarkerChangeable)
    );

  VVectorRenderer :=
    TVectorTileRendererChangeableForMarksLayer.Create(
      ALayersConfig.MarksLayerConfig.MarksDrawConfig.CaptionDrawConfig,
      ABitmap32StaticFactory,
      AProjectedGeometryProvider,
      VMarkerProviderForVectorItem
    );

  VTileMatrix :=
    TBitmapTileMatrixChangeableByVectorMatrix.Create(
      VPerfList.CreateAndAddNewSubList('BitmapMatrix'),
      AAppStartedNotifier,
      AAppClosingNotifier,
      VVectorTileMatrix,
      VVectorRenderer,
      VTileMatrixDraftResampler,
      True,
      ABitmap32StaticFactory,
      AHashFunction,
      ALayersConfig.MarksLayerConfig.ThreadConfig,
      VDebugName
    );
  VMatrixList.Add(VTileMatrix);

  // Vector search results visualisation layer
  VDebugName := 'SearchResults';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VBitmap :=
    ReadBitmapByFileRef(
      AResourceProvider,
      'FOUNDPNT.png',
      AContentTypeManager,
      nil
    );
  VMarkerChangeable := nil;
  if VBitmap <> nil then begin
    VMarkerChangeable :=
      TMarkerDrawableChangeableFaked.Create(
        TMarkerDrawableByBitmap32Static.Create(VBitmap, DoublePoint(8, 8))
      );
  end;
  VVectorOversizeRect := Rect(10, 10, 10, 10);
  VVectorTileProvider :=
    TVectorTileProviderChangeableForLastSearchResult.Create(
      ALastSearchResult,
      AVectorItemSubsetBuilderFactory,
      VVectorOversizeRect
    );
  VVectorTileMatrix :=
    TVectorTileMatrixChangeableForVectorLayers.Create(
      VPerfList.CreateAndAddNewSubList('VectorMatrix'),
      AAppStartedNotifier,
      AAppClosingNotifier,
      VTileRectForShow,
      AHashFunction,
      AVectorItemSubsetBuilderFactory,
      False,
      VVectorTileProvider,
      nil,
      ALayersConfig.KmlLayerConfig.ThreadConfig,
      VVectorOversizeRect,
      VDebugName
    );
  FLayerSearchResults :=
    TFindVectorItemsForVectorTileMatrix.Create(
      AVectorItemSubsetBuilderFactory,
      AProjectedGeometryProvider,
      VVectorTileMatrix,
      VPerfList.CreateAndAddNewCounter('FindItems'),
      6
    );
  VVectorRenderer :=
    TVectorTileRendererChangeableForVectorMaps.Create(
      ALayersConfig.KmlLayerConfig.DrawConfig,
      VMarkerChangeable,
      ABitmap32StaticFactory,
      AProjectedGeometryProvider
    );

  VTileMatrix :=
    TBitmapTileMatrixChangeableByVectorMatrix.Create(
      VPerfList.CreateAndAddNewSubList('BitmapMatrix'),
      AAppStartedNotifier,
      AAppClosingNotifier,
      VVectorTileMatrix,
      VVectorRenderer,
      VTileMatrixDraftResampler,
      True,
      ABitmap32StaticFactory,
      AHashFunction,
      ALayersConfig.KmlLayerConfig.ThreadConfig,
      VDebugName
    );
  VMatrixList.Add(VTileMatrix);

  // GPS track visualisation layer
  VDebugName := 'GPSTrack';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VProvider :=
    TBitmapLayerProviderChangeableForGpsTrack.Create(
      VPerfList,
      AGUISyncronizedTimerNotifier,
      ALayersConfig.GPSTrackConfig,
      ABitmap32StaticFactory,
      AGpsTrackRecorder
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableWithThread.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      VTileRectForShow,
      VTileMatrixDraftResampler,
      True,
      ABitmap32StaticFactory,
      AHashFunction,
      VProvider,
      nil,
      ALayersConfig.GPSTrackConfig.ThreadConfig,
      VDebugName
    );
  VMatrixList.Add(VTileMatrix);

  // Composite tiled layer
  VDebugName := 'Composite';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VTileMatrix :=
    TBitmapTileMatrixChangeableComposite.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      VTileRectForShow,
      VMatrixList.MakeStaticAndClear,
      VTileMatrixDraftResampler,
      True,
      ABitmap32StaticFactory,
      AHashFunction,
      ALayersConfig.KmlLayerConfig.ThreadConfig,
      VDebugName
    );
  VLayer :=
    TTiledMapLayer.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AHashFunction,
      AViewPortState.View,
      VTileMatrix,
      AGUISyncronizedTimerNotifier,
      VDebugName
    );
  VLayersList.Add(VLayer);

  // GPS marker layer
  VMarkerChangeable :=
    TMarkerDrawableChangeableSimple.Create(
      TMarkerDrawableSimpleSquare,
      ALayersConfig.GPSMarker.StopedMarkerConfig
    );
  VMarkerWithDirectionChangeable :=
    TMarkerDrawableWithDirectionChangeableSimple.Create(
      TMarkerDrawableSimpleArrow,
      ALayersConfig.GPSMarker.MovedMarkerConfig
    );
  VDebugName := 'GPSMarker';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerGPSMarker.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AGUISyncronizedTimerNotifier,
      ALayersConfig.GPSMarker,
      VMarkerWithDirectionChangeable,
      VMarkerChangeable,
      AGPSRecorder
    );
  VLayersList.Add(VLayer);

  // Layer with rings around GPS marker
  VDebugName := 'GPSMarkerRings';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerGPSMarkerRings.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AGUISyncronizedTimerNotifier,
      AVectorGeometryProjectedFactory,
      AVectorGeometryLonLatFactory,
      ALayersConfig.GPSMarker.MarkerRingsConfig,
      AGPSRecorder
    );
  VLayersList.Add(VLayer);

  // Last selection visualisation layer
  VDebugName := 'LastSelection';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VPolygonChangeable :=
    TGeometryLonLatPolygonChangeableByLastSelection.Create(
      ALayersConfig.LastSelectionLayerConfig,
      ALastSelectionInfo
    );
  VLayer :=
    TMapLayerSinglePolygon.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      ALayersConfig.LastSelectionLayerConfig,
      VPolygonChangeable
    );
  VLayersList.Add(VLayer);

  // Merge polygons result visualisation layer
  VDebugName := 'MergePolygonsResult';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VPolygonChangeable :=
    TGeometryLonLatPolygonChangeableByMergePolygonsResult.Create(
      ALayersConfig.MergePolygonsResultLayerConfig,
      AMergePolygonsResult
    );
  VLayer :=
    TMapLayerSinglePolygon.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      ALayersConfig.MergePolygonsResultLayerConfig,
      VPolygonChangeable
    );
  VLayersList.Add(VLayer);

  // CalcLine points
  VGeometryChangeableByPathEdit :=
    TGeometryLonLatChangeableByPathEdit.Create(
      AVectorGeometryLonLatFactory,
      ACalcLinePath
    );

  VLayer := VGeometryChangeableByPathEdit;
  VLayersList.Add(VLayer);

  // CalcLine line visualisation layer
  VDebugName := 'CalcLine';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerSingleLine.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      ALayersConfig.CalcLineLayerConfig.LineConfig,
      VGeometryChangeableByPathEdit.LineChangeable
    );
  VLayersList.Add(VLayer);

  // CalcLine simple points visualisation layer
  VDebugName := 'CalcLineSimplePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcLineLayerConfig.PointsConfig.NormalPointMarker)
    );
  VLayersList.Add(VLayer);

  // CalcLine first points visualisation layer
  VDebugName := 'CalcFirstSimplePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcLineLayerConfig.PointsConfig.FirstPointMarker)
    );
  VLayersList.Add(VLayer);

  // CalcLine active points visualisation layer
  VDebugName := 'CalcActiveSimplePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcLineLayerConfig.PointsConfig.ActivePointMarker)
    );
  VLayersList.Add(VLayer);

  // CalcLine captions layer
  VDebugName := 'CalcLineCaptions';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerCalcLineCaptions.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      ACalcLinePath,
      ALayersConfig.CalcLineLayerConfig.CaptionConfig,
      AValueToStringConverter
    );
  VLayersList.Add(VLayer);

  {$REGION 'CalcCircle'}
  // CalcCircle polygon
  VGeometryChangeableByPolygonEdit :=
    TGeometryLonLatChangeableByPolygonEdit.Create(
      AVectorGeometryLonLatFactory,
      ACircleOnMapEdit.GetPolygonOnMapEdit
    );

  VLayer := VGeometryChangeableByPolygonEdit;
  VLayersList.Add(VLayer);

  // CalcCircle polygon visualisation layer
  VDebugName := 'CalcCirclePolygon';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerSinglePolygon.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      ALayersConfig.CalcCircleLayerConfig.PolygonConfig,
      VGeometryChangeableByPolygonEdit.PolygonChangeable
    );
  VLayersList.Add(VLayer);

  // CalcCircle line
  VGeometryChangeableByPathEdit :=
    TGeometryLonLatChangeableByPathEdit.Create(
      AVectorGeometryLonLatFactory,
      ACircleOnMapEdit
    );

  VLayer := VGeometryChangeableByPathEdit;
  VLayersList.Add(VLayer);

  // CalcCircle line visualisation layer
  VDebugName := 'CalcCircleLine';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerSingleLine.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      ALayersConfig.CalcCircleLayerConfig.LineConfig,
      VGeometryChangeableByPathEdit.LineChangeable
    );
  VLayersList.Add(VLayer);

  // CalcCircle simple points visualisation layer
  VDebugName := 'CalcCircleSimplePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcCircleLayerConfig.PointsConfig.NormalPointMarker)
    );
  VLayersList.Add(VLayer);

  // CalcCircle first points visualisation layer
  VDebugName := 'CalcCircleSimplePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcCircleLayerConfig.PointsConfig.FirstPointMarker)
    );
  VLayersList.Add(VLayer);

  // CalcCircle active points visualisation layer
  VDebugName := 'CalcCircleActiveSimplePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcCircleLayerConfig.PointsConfig.ActivePointMarker)
    );
  VLayersList.Add(VLayer);

  // CalcCircle captions layer
  VDebugName := 'CalcCircleCaptions';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerCalcCircleCaptions.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      ACircleOnMapEdit,
      ALayersConfig.CalcCircleLayerConfig.CaptionConfig,
      AValueToStringConverter
    );
  VLayersList.Add(VLayer);
  {$ENDREGION 'CalcCircle'}

  // PathEdit line visualisation layer
  VGeometryChangeableByPathEdit :=
    TGeometryLonLatChangeableByPathEdit.Create(
      AVectorGeometryLonLatFactory,
      AEditLinePath
    );

  VLayer := VGeometryChangeableByPathEdit;
  VLayersList.Add(VLayer);

  // PathEdit line visualisation layer
  VDebugName := 'PathEdit';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerSingleLine.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      ALayersConfig.MarkPolyLineLayerConfig.LineConfig,
      VGeometryChangeableByPathEdit.LineChangeable
    );
  VLayersList.Add(VLayer);

  // PathEdit simple points visualisation layer
  VDebugName := 'PathEditSimplePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolyLineLayerConfig.PointsConfig.NormalPointMarker)
    );
  VLayersList.Add(VLayer);

  // PathEdit first points visualisation layer
  VDebugName := 'PathEditFirstPoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolyLineLayerConfig.PointsConfig.FirstPointMarker)
    );
  VLayersList.Add(VLayer);

  // PathEdit active points visualisation layer
  VDebugName := 'PathEditActivePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolyLineLayerConfig.PointsConfig.ActivePointMarker)
    );
  VLayersList.Add(VLayer);

  // PathEdit captions layer
  VDebugName := 'PathEditCaptions';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerCalcLineCaptions.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AEditLinePath,
      ALayersConfig.MarkPolyLineLayerConfig.CaptionConfig,
      AValueToStringConverter
    );
  VLayersList.Add(VLayer);


  // PolygonEdit line and fill visualisation layer
  VGeometryChangeableByPolygonEdit :=
    TGeometryLonLatChangeableByPolygonEdit.Create(
      AVectorGeometryLonLatFactory,
      AEditPolygon
    );

  VLayer := VGeometryChangeableByPolygonEdit;
  VLayersList.Add(VLayer);

  // PolygonEdit line and fill visualisation layer
  VDebugName := 'PolygonEdit';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerSinglePolygon.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      ALayersConfig.MarkPolygonLayerConfig.LineConfig,
      VGeometryChangeableByPolygonEdit.PolygonChangeable
    );
  VLayersList.Add(VLayer);

  // PolygonEdit simple points visualisation layer
  VDebugName := 'PolygonEditSimplePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolygonLayerConfig.PointsConfig.NormalPointMarker)
    );
  VLayersList.Add(VLayer);

  // PolygonEdit first points visualisation layer
  VDebugName := 'PolygonEditFirstPoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolygonLayerConfig.PointsConfig.FirstPointMarker)
    );
  VLayersList.Add(VLayer);

  // PolygonEdit active points visualisation layer
  VDebugName := 'PolygonEditActivePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolygonLayerConfig.PointsConfig.ActivePointMarker)
    );
  VLayersList.Add(VLayer);

  // PolygonSelection line and fill visualisation layer
  VGeometryChangeableByPolygonEdit :=
    TGeometryLonLatChangeableByPolygonEdit.Create(
      AVectorGeometryLonLatFactory,
      ASelectPolygon
    );

  VLayer := VGeometryChangeableByPolygonEdit;
  VLayersList.Add(VLayer);

  // PolygonSelection line and fill visualisation layer
  VDebugName := 'PolygonSelection';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerSinglePolygon.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      ALayersConfig.SelectionPolygonLayerConfig.LineConfig,
      VGeometryChangeableByPolygonEdit.PolygonChangeable
    );
  VLayersList.Add(VLayer);

  // PolygonSelection simple points visualisation layer
  VDebugName := 'PolygonSelectionSimplePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolygonLayerConfig.PointsConfig.NormalPointMarker)
    );
  VLayersList.Add(VLayer);

  // PolygonSelection first points visualisation layer
  VDebugName := 'PolygonSelectionFirstPoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolygonLayerConfig.PointsConfig.FirstPointMarker)
    );
  VLayersList.Add(VLayer);

  // PolygonSelection active points visualisation layer
  VDebugName := 'PolygonSelectionActivePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolygonLayerConfig.PointsConfig.ActivePointMarker)
    );
  VLayersList.Add(VLayer);

  // SelectionByLine visualisation layer
  VGeometryChangeableByPathEdit :=
    TGeometryLonLatChangeableByPathEdit.Create(
      AVectorGeometryLonLatFactory,
      ASelectLinePath
    );

  VLayer := VGeometryChangeableByPathEdit;
  VLayersList.Add(VLayer);

  // SelectionByLine shadow visualisation layer
  VDebugName := 'SelectionByLineShadow';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VPolygonChangeable :=
    TGeometryLonLatPolygonChangeableByLineChangeable.Create(
      AVectorGeometryLonLatFactory,
      AViewPortState.View,
      VGeometryChangeableByPathEdit.LineChangeable,
      ALayersConfig.SelectionPolylineLayerConfig.ShadowConfig
    );
  VLayer :=
    TMapLayerSinglePolygon.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      ALayersConfig.SelectionPolylineLayerConfig.ShadowConfig,
      VPolygonChangeable
    );
  VLayersList.Add(VLayer);

  // SelectionByLyne line visualisation layer
  VDebugName := 'SelectionByLine';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerSingleLine.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      ALayersConfig.SelectionPolylineLayerConfig.LineConfig,
      VGeometryChangeableByPathEdit.LineChangeable
    );
  VLayersList.Add(VLayer);

  // SelectionByLyne simple points visualisation layer
  VDebugName := 'SelectionByLineSimplePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolylineLayerConfig.PointsConfig.NormalPointMarker)
    );
  VLayersList.Add(VLayer);

  // SelectionByLyne first points visualisation layer
  VDebugName := 'SelectionByLineFirstPoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolylineLayerConfig.PointsConfig.FirstPointMarker)
    );
  VLayersList.Add(VLayer);

  // SelectionByLyne active points visualisation layer
  VDebugName := 'SelectionByLineActivePoints';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointsSet.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AVectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolylineLayerConfig.PointsConfig.ActivePointMarker)
    );
  VLayersList.Add(VLayer);

  // SelectionByRect visualisation layer
  VDebugName := 'SelectionByRect';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerSelectionByRect.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      ASelectionRect,
      ALayersConfig.SelectionRectLayerConfig
    );
  VLayersList.Add(VLayer);

  // Goto marker visualisation layer
  VBitmap :=
    ReadBitmapByFileRef(
      AResourceProvider,
      'ICONIII.png',
      AContentTypeManager,
      nil
    );
  VMarkerChangeable := nil;
  if VBitmap <> nil then begin
    VMarkerChangeable :=
      TMarkerDrawableChangeableFaked.Create(
        TMarkerDrawableByBitmap32Static.Create(VBitmap, DoublePoint(7, 6))
      );
  end;
  VDebugName := 'GotoMarker';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerGotoMarker.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AGUISyncronizedTimerNotifier,
      AViewPortState.View,
      VMarkerChangeable,
      AMapGoto,
      ALayersConfig.GotoLayerConfig
    );
  VLayersList.Add(VLayer);

  // Navigation to mark marker visualisation layer
  VMarkerChangeable :=
    TMarkerDrawableChangeableSimple.Create(
      TMarkerDrawableSimpleCross,
      ALayersConfig.NavToPointMarkerConfig.ReachedMarkerConfig
    );
  VMarkerWithDirectionChangeable :=
    TMarkerDrawableWithDirectionChangeableSimple.Create(
      TMarkerDrawableSimpleArrow,
      ALayersConfig.NavToPointMarkerConfig.ArrowMarkerConfig
    );

  VDebugName := 'NavToMark';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerNavToMark.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      ANavToPoint,
      VMarkerWithDirectionChangeable,
      VMarkerChangeable,
      ALayersConfig.NavToPointMarkerConfig
    );
  VLayersList.Add(VLayer);

  // Error info visualisation layer
  VDebugName := 'TileErrorInfo';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerTileErrorInfo.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AMainMapState.AllMapsSet,
      ABitmap32StaticFactory,
      ATileErrorLogProvider,
      AGUISyncronizedTimerNotifier
    );
  VLayersList.Add(VLayer);

  // Point edit marker visualisation layer
  VBitmap :=
    ReadBitmapByFileRef(
      AResourceProvider,
      'ICONIII.png',
      AContentTypeManager,
      nil
    );
  VMarkerChangeable := nil;
  if VBitmap <> nil then begin
    VMarkerChangeable :=
      TMarkerDrawableChangeableFaked.Create(
        TMarkerDrawableByBitmap32Static.Create(VBitmap, DoublePoint(7, 6))
      );
  end;
  VDebugName := 'PointOnMapEdit';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerPointOnMapEdit.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      VMarkerChangeable,
      APointOnMapEdit
    );
  VLayersList.Add(VLayer);

  // Full map cursor layer
  VDebugName := 'FullMapMouseCursor';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerFullMapMouseCursor.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AMainFormState,
      AGUISyncronizedTimerNotifier,
      AMouseState,
      ALayersConfig.FullMapMouseCursorLayerConfig
    );
  VLayersList.Add(VLayer);

  // Center scale layer
  VMarkerChangeable :=
    TMarkerDrawableChangeableFaked.Create(
      TMarkerDrawableCenterScale.Create(ABitmap32StaticFactory)
    );
  VDebugName := 'CenterScale';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerCenterScale.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      VMarkerChangeable,
      ALayersConfig.CenterScaleConfig
    );
  VLayersList.Add(VLayer);

  // Horizontal scale line layer
  VDebugName := 'ScaleLineHorizontal';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerScaleLineHorizontal.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AGUISyncronizedTimerNotifier,
      AScaleLinePopupMenu,
      ALayersConfig.ScaleLineConfig
    );
  VLayersList.Add(VLayer);

  // Vertical scale line layer
  VDebugName := 'ScaleLineVertical';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerScaleLineVertical.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AGUISyncronizedTimerNotifier,
      AScaleLinePopupMenu,
      ALayersConfig.ScaleLineConfig
    );
  VLayersList.Add(VLayer);

  // Map licenses visualisation layer
  VDebugName := 'LicenseList';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLicensList :=
    TActiveMapsLicenseList.Create(
      ALanguageManager,
      AMainMapState.ActiveMapsSetLicenseNotEmpty
    );
  VLayer :=
    TWindowLayerLicenseList.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      VLicensList
    );
  VLayersList.Add(VLayer);

  // Status bar layer
  VDebugName := 'StatusBar';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerStatusBar.Create(
      ALanguageManager,
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      ALayersConfig.StatBar,
      ACoordToStringConverter,
      AValueToStringConverter,
      AMouseState,
      AGUISyncronizedTimerNotifier,
      ATerrainProviderList,
      AGlobalConfig.TerrainConfig,
      ADownloadInfo,
      AGlobalInternetState,
      AStatBarPopupMenu,
      AMainMapState.ActiveMap
    );
  VLayersList.Add(VLayer);

  {$REGION 'SunCalc'}
  // SunCalc Layer
  VDebugName := 'SunCalc/YearInfo';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerSunCalcYearInfo.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      ASunCalcConfig,
      ASunCalcProvider
    );
  VLayersList.Add(VLayer);

  VDebugName := 'SunCalc/DayInfo';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerSunCalcDayInfo.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      ASunCalcConfig,
      ASunCalcProvider
    );
  VLayersList.Add(VLayer);

  VDebugName := 'SunCalc/TimeInfo';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerSunCalcTimeInfo.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      ASunCalcConfig,
      ASunCalcProvider
    );
  VLayersList.Add(VLayer);

  VDebugName := 'SunCalc/YearTimeLine';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerSunCalcYearTimeLine.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AMouseState,
      AGUISyncronizedTimerNotifier,
      AViewPortState.View,
      ASunCalcConfig,
      ASunCalcProvider,
      ASunCalcPopupMenu
    );
  VLayersList.Add(VLayer);

  VDebugName := 'SunCalc/DayTimeLine';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerSunCalcDayTimeLine.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AMouseState,
      AGUISyncronizedTimerNotifier,
      AViewPortState.View,
      ASunCalcConfig,
      ASunCalcProvider,
      ASunCalcPopupMenu
    );
  VLayersList.Add(VLayer);

  VDebugName := 'SunCalc/DetailsPanel';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerSunCalcDetailsPanel.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      ASunCalcConfig,
      ASunCalcProvider,
      ASunCalcPopupMenu
    );
  VLayersList.Add(VLayer);
  {$ENDREGION 'SunCalc'}

  VMiniMapConverterChangeable :=
    TLocalConverterChangeableOfMiniMap.Create(
      APerfListGroup.CreateAndAddNewCounter('MiniMapConverter'),
      ALocalConverterFactory,
      AActiveProjectionSet,
      AViewPortState.View,
      ALayersConfig.MiniMapLayerConfig.LocationConfig
    );
  VTileRectForShow :=
    TTileRectChangeableByLocalConverterSmart.Create(
      AActiveProjectionSet,
      VMiniMapConverterChangeable,
      GSync.SyncVariable.Make('TileRectMiniMapForShowMain'),
      GSync.SyncVariable.Make('TileRectMiniMapForShowResult')
    );

  // MiniMap bitmap layer
  VDebugName := 'MiniMap';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VProvider :=
    TBitmapLayerProviderChangeableForMainLayer.Create(
      AMainMapState.MiniMapActiveMap,
      AMainMapState.MiniMapActiveBitmapLayersList,
      ABitmapPostProcessing,
      ALayersConfig.MiniMapLayerConfig.UseTilePrevZoomConfig,
      ABitmap32StaticFactory,
      ATileErrorLogger
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableWithThread.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      VTileRectForShow,
      VTileMatrixDraftResampler,
      True,
      ABitmap32StaticFactory,
      AHashFunction,
      VProvider,
      nil,
      ALayersConfig.MiniMapLayerConfig.ThreadConfig,
      VDebugName
    );
  VLayer :=
    TTiledMapLayer.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AHashFunction,
      VMiniMapConverterChangeable,
      VTileMatrix,
      AGUISyncronizedTimerNotifier,
      VDebugName
    );
  VLayersList.Add(VLayer);

  // View rect in MiniMap visualisation layer
  VDebugName := 'MiniMapViewRect';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMiniMapLayerViewRect.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState,
      VMiniMapConverterChangeable,
      AGUISyncronizedTimerNotifier,
      AMiniMapPopupMenu,
      ALayersConfig.MiniMapLayerConfig.LocationConfig
    );
  VLayersList.Add(VLayer);

  // Mini Map top border
  VDebugName := 'MiniMapTopBorder';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMiniMapLayerTopBorder.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      VMiniMapConverterChangeable,
      ALayersConfig.MiniMapLayerConfig
    );
  VLayersList.Add(VLayer);

  // Mini Map left border
  VDebugName := 'MiniMapLeftBorder';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMiniMapLayerLeftBorder.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      VMiniMapConverterChangeable,
      ALayersConfig.MiniMapLayerConfig
    );
  VLayersList.Add(VLayer);

  // Mini map Minus button
  VBitmap :=
    ReadBitmapByFileRef(
      AResourceProvider,
      'ICONII.png',
      AContentTypeManager,
      nil
    );
  VBitmapChangeable := nil;
  if VBitmap <> nil then begin
    VBitmapChangeable := TBitmapChangeableFaked.Create(VBitmap);
  end;
  VDebugName := 'MiniMapMinusButton';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMiniMapLayerMinusButton.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      VMiniMapConverterChangeable,
      VBitmapChangeable,
      ALayersConfig.MiniMapLayerConfig
    );
  VLayersList.Add(VLayer);

  // Mini Map plus button
  VBitmap :=
    ReadBitmapByFileRef(
      AResourceProvider,
      'ICONI.png',
      AContentTypeManager,
      nil
    );
  VBitmapChangeable := nil;
  if VBitmap <> nil then begin
    VBitmapChangeable := TBitmapChangeableFaked.Create(VBitmap);
  end;
  VDebugName := 'MiniMapPlusButton';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMiniMapLayerPlusButton.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      VMiniMapConverterChangeable,
      VBitmapChangeable,
      ALayersConfig.MiniMapLayerConfig
    );
  VLayersList.Add(VLayer);

  FLayersList := VLayersList.MakeStaticAndClear;
end;

function TMainFormLayersList.GetMarksLayer: IFindVectorItems;
begin
  Result := FLayerMapMarks;
end;

function TMainFormLayersList.GetSearchResultsLayer: IFindVectorItems;
begin
  Result := FLayerSearchResults;
end;

function TMainFormLayersList.GetWikiLayer: IFindVectorItems;
begin
  Result := FWikiLayer;
end;

end.

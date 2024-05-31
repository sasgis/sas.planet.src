{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MainFormLayersList;

interface

{$IFDEF DEBUG}
//  {$DEFINE DISABLE_RASTER_MAPS_LAYER}
//  {$DEFINE DISABLE_GRIDS_LAYER}
//  {$DEFINE DISABLE_VECTOR_MAPS_LAYER}
//  {$DEFINE DISABLE_FILLING_MAP_LAYER}
//  {$DEFINE DISABLE_MARKS_LAYER}
//  {$DEFINE DISABLE_SEARCH_RESULTS_LAYER}
//  {$DEFINE DISABLE_GPS_TRACK_LAYER}
//
//  {$DEFINE DISABLE_GPS_MARKER_LAYER}
//  {$DEFINE DISABLE_LAST_SELECTION_LAYER}
//  {$DEFINE DISABLE_MERGE_POLYGONS_RESULT_LAYER}
//  {$DEFINE DISABLE_CALC_LINE_LAYER}
//  {$DEFINE DISABLE_CALC_CIRCLE_LAYER}
//  {$DEFINE DISABLE_PATH_EDIT_LAYER}
//  {$DEFINE DISABLE_POLYGON_EDIT_LAYER}
//  {$DEFINE DISABLE_POLYGON_SELECTION_LAYER}
//  {$DEFINE DISABLE_SELECTION_BY_LINE_LAYER}
//  {$DEFINE DISABLE_SELECTION_BY_RECT_LAYER}
//
//  {$DEFINE DISABLE_GOTO_MARKER_LAYER}
//  {$DEFINE DISABLE_GPS_TRACK_GOTO_MARKER_LAYER}
//  {$DEFINE DISABLE_NAVIGATION_TO_MARK_MARKER_LAYER}
//  {$DEFINE DISABLE_ERROR_INFO_LAYER}
//  {$DEFINE DISABLE_POINT_EDIT_MARKER_LAYER}
//  {$DEFINE DISABLE_FULL_MAP_CURSOR_LAYER}
//  {$DEFINE DISABLE_CENTER_SCALE_LAYER}
//  {$DEFINE DISABLE_SCALE_LINE_LAYER}
//  {$DEFINE DISABLE_MAP_LICENSES_LAYER}
//  {$DEFINE DISABLE_STATUS_BAR_LAYER}
//  {$DEFINE DISABLE_SUN_CALC_LAYER}
//  {$DEFINE DISABLE_MINI_MAP_LAYER}
{$ENDIF}

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
  private
    { IMainFormLayersList }
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
      const AGpsTrackGoTo: IMapViewGoto;
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
  Math,
  t_GeoTypes,
  i_Bitmap32Static,
  i_MarkerDrawable,
  i_BitmapMarker,
  i_StringListChangeable,
  i_LocalCoordConverterChangeable,
  i_MarkerProviderByAppearancePointIcon,
  i_MarkerProviderForVectorItem,
  i_InterfaceListSimple,
  i_VectorItemSubsetChangeable,
  i_ImageResamplerFactoryChangeable,
  i_TileRectChangeable,
  i_TextDrawerBasic,
  i_BitmapLayerProviderChangeable,
  i_ObjectWithListener,
  i_BitmapTileMatrixChangeable,
  i_VectorTileRendererChangeable,
  i_VectorTileProviderChangeable,
  i_VectorTileMatrixChangeable,
  i_GeometryLonLatChangeable,
  i_LocalCoordConverter,
  i_VectorItemSubset,
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
  u_TextDrawerBasic,
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
  u_BitmapMarker,
  u_MarkerProviderForVectorItemForMarkPoints,
  u_MarkerProviderForVectorItemWithCache,
  u_MarkerProviderByAppearancePointIcon,
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
  u_MapLayerGpsTrackGoToMarker,
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

type
  TFindVectorItemsFake = class(TBaseInterfacedObject, IFindVectorItems)
  private
    function FindItems(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IVectorItemSubset;
  end;

{ TFindVectorItemsFake }

function TFindVectorItemsFake.FindItems(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IVectorItemSubset;
begin
  Result := nil;
end;

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
  const AGpsTrackGoTo: IMapViewGoto;
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

  procedure BuildRasterMapsMatrixLayer(
    const AMatrixList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable;
    const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
  );
  const
    CDebugName = 'RasterMaps';
  var
    VPerfList: IInternalPerformanceCounterList;
    VProvider: IBitmapLayerProviderChangeable;
    VSourceChangeNotifier: IObjectWithListener;
    VTileMatrix: IBitmapTileMatrixChangeable;
  begin
    {$IFDEF DISABLE_RASTER_MAPS_LAYER} Exit; {$ENDIF}
    VPerfList := APerfListGroup.CreateAndAddNewSubList(CDebugName);
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
        ATileRectForShow,
        ATileMatrixDraftResampler,
        True,
        ABitmap32StaticFactory,
        AHashFunction,
        VProvider,
        VSourceChangeNotifier,
        ALayersConfig.MainMapLayerConfig.ThreadConfig,
        CDebugName
      );
    AMatrixList.Add(VTileMatrix);
  end;

  procedure BuildGridsMatrixLayer(
    const AMatrixList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable;
    const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
  );
  const
    CDebugName = 'Grids';
  var
    VPerfList: IInternalPerformanceCounterList;
    VProvider: IBitmapLayerProviderChangeable;
    VTileMatrix: IBitmapTileMatrixChangeable;
  begin
    {$IFDEF DISABLE_GRIDS_LAYER} Exit; {$ENDIF}
    VPerfList := APerfListGroup.CreateAndAddNewSubList(CDebugName);
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
        ATileRectForShow,
        ATileMatrixDraftResampler,
        False,
        ABitmap32StaticFactory,
        AHashFunction,
        VProvider,
        nil,
        ALayersConfig.MapLayerGridsConfig.ThreadConfig,
        CDebugName
      );
    AMatrixList.Add(VTileMatrix);
  end;

  procedure BuildVectorMapsMatrixLayer(
    const AMatrixList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable;
    const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable;
    const AMarkerIconProvider: IMarkerProviderByAppearancePointIcon
  );
  const
    CDebugName = 'VectorMaps';
  var
    VPerfList: IInternalPerformanceCounterList;
    VVectorOversizeRect: TRect;
    VMarkerChangeable: IMarkerDrawableChangeable;
    VSourceChangeNotifier: IObjectWithListener;
    VVectorTileProvider: IVectorTileUniProviderChangeable;
    VVectorTileMatrix: IVectorTileMatrixChangeable;
    VTileMatrix: IBitmapTileMatrixChangeable;
    VVectorRenderer: IVectorTileRendererChangeable;
  begin
    {$IFDEF DISABLE_VECTOR_MAPS_LAYER}
    FWikiLayer := TFindVectorItemsFake.Create;
    Exit;
    {$ENDIF}
    VPerfList := APerfListGroup.CreateAndAddNewSubList(CDebugName);
    VVectorOversizeRect := Rect(10, 10, 10, 10);
    VMarkerChangeable :=
      TMarkerDrawableChangeableSimple.Create(
        TMarkerDrawableSimpleSquare,
        ALayersConfig.KmlLayerConfig.PointMarkerConfig
      );
    VVectorTileProvider :=
      TVectorTileProviderChangeableForVectorLayers.Create(
        AMainMapState.ActiveKmlLayersSet,
        ALayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig,
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
        ATileRectForShow,
        AHashFunction,
        AVectorItemSubsetBuilderFactory,
        False,
        VVectorTileProvider,
        VSourceChangeNotifier,
        ALayersConfig.KmlLayerConfig.ThreadConfig,
        VVectorOversizeRect,
        CDebugName
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
        nil,
        VMarkerChangeable,
        ABitmap32StaticFactory,
        AProjectedGeometryProvider,
        AMarkerIconProvider
      );
    VTileMatrix :=
      TBitmapTileMatrixChangeableByVectorMatrix.Create(
        VPerfList.CreateAndAddNewSubList('BitmapMatrix'),
        AAppStartedNotifier,
        AAppClosingNotifier,
        VVectorTileMatrix,
        VVectorRenderer,
        ATileMatrixDraftResampler,
        True,
        ABitmap32StaticFactory,
        AHashFunction,
        ALayersConfig.KmlLayerConfig.ThreadConfig,
        CDebugName
      );
    AMatrixList.Add(VTileMatrix);
  end;

  procedure BuildFillingMapMatrixLayer(
    const AMatrixList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable;
    const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
  );
  const
    CDebugName = 'FillingMap';
  var
    VPerfList: IInternalPerformanceCounterList;
    VProvider: IBitmapLayerProviderChangeable;
    VSourceChangeNotifier: IObjectWithListener;
    VTileMatrix: IBitmapTileMatrixChangeable;
  begin
    {$IFDEF DISABLE_FILLING_MAP_LAYER} Exit; {$ENDIF}
    VPerfList := APerfListGroup.CreateAndAddNewSubList(CDebugName);
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
        ATileRectForShow,
        ATileMatrixDraftResampler,
        True,
        ABitmap32StaticFactory,
        AHashFunction,
        VProvider,
        VSourceChangeNotifier,
        ALayersConfig.FillingMapLayerConfig.ThreadConfig,
        CDebugName
      );
    AMatrixList.Add(VTileMatrix);
  end;

  procedure BuildMarksMatrixLayer(
    const AMatrixList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable;
    const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable;
    const AMarkerIconProvider: IMarkerProviderByAppearancePointIcon
  );
  const
    CDebugName = 'Marks';
  var
    VPerfList: IInternalPerformanceCounterList;
    VVectorOversizeRect: TRect;
    VMarkerChangeable: IMarkerDrawableChangeable;
    VMarkerProviderForVectorItem: IMarkerProviderForVectorItem;
    VVectorTileMatrix: IVectorTileMatrixChangeable;
    VTileMatrix: IBitmapTileMatrixChangeable;
    VVectorRenderer: IVectorTileRendererChangeable;
    VBitmap: IBitmap32Static;
    VTextDrawerBasic: ITextDrawerBasic;
    VVectorItems: IVectorItemSubsetChangeable;
  begin
    {$IFDEF DISABLE_MARKS_LAYER}
    FLayerMapMarks := TFindVectorItemsFake.Create;
    Exit;
    {$ENDIF}
    VVectorOversizeRect := ALayersConfig.MarksLayerConfig.MarksDrawConfig.DrawOrderConfig.OverSizeRect;
    VPerfList := APerfListGroup.CreateAndAddNewSubList(CDebugName);
    VVectorItems :=
      TVectorItemSubsetChangeableForMarksLayer.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        ATileRectForShow,
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
        ATileRectForShow,
        AHashFunction,
        AVectorItemSubsetBuilderFactory,
        False,
        VVectorItems,
        ALayersConfig.MarksLayerConfig.ThreadConfig,
        VVectorOversizeRect,
        CDebugName
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
    VTextDrawerBasic :=
      TTextDrawerBasic.Create(
        VPerfList.CreateAndAddNewSubList('Caption'),
        AHashFunction,
        ABitmap32StaticFactory,
        Max(Max(VVectorOversizeRect.Left, VVectorOversizeRect.Right), Max(VVectorOversizeRect.Top, VVectorOversizeRect.Bottom)),
        1,
        ALayersConfig.MarksLayerConfig.MarksDrawConfig.CaptionDrawConfig.FontName
      );
    VMarkerProviderForVectorItem :=
      TMarkerProviderForVectorItemWithCache.Create(
        VPerfList.CreateAndAddNewSubList('Marker'),
        AHashFunction,
        TMarkerProviderForVectorItemForMarkPoints.Create(VTextDrawerBasic, AMarkerIconProvider)
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
        ATileMatrixDraftResampler,
        True,
        ABitmap32StaticFactory,
        AHashFunction,
        ALayersConfig.MarksLayerConfig.ThreadConfig,
        CDebugName
      );
    AMatrixList.Add(VTileMatrix);
  end;

  procedure BuildSearchResultsMatrixLayer(
    const AMatrixList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable;
    const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable;
    const AMarkerIconProvider: IMarkerProviderByAppearancePointIcon
  );
  const
    CDebugName = 'SearchResults';
  var
    VPerfList: IInternalPerformanceCounterList;
    VVectorOversizeRect: TRect;
    VVectorTileMatrix: IVectorTileMatrixChangeable;
    VTileMatrix: IBitmapTileMatrixChangeable;
    VVectorTileProvider: IVectorTileUniProviderChangeable;
    VVectorRenderer: IVectorTileRendererChangeable;
    VBitmap: IBitmap32Static;
    VBitmapMarker: IBitmapMarker;
  begin
    {$IFDEF DISABLE_SEARCH_RESULTS_LAYER}
    FLayerSearchResults := TFindVectorItemsFake.Create;
    Exit;
    {$ENDIF}
    VPerfList := APerfListGroup.CreateAndAddNewSubList(CDebugName);
    VBitmap :=
      ReadBitmapByFileRef(
        AResourceProvider,
        'FOUNDPNT.png',
        AContentTypeManager,
        nil
      );
    VBitmapMarker :=
      TBitmapMarker.Create(
        VBitmap,
        DoublePoint(8, 8)
      );
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
        ATileRectForShow,
        AHashFunction,
        AVectorItemSubsetBuilderFactory,
        False,
        VVectorTileProvider,
        nil,
        ALayersConfig.KmlLayerConfig.ThreadConfig,
        VVectorOversizeRect,
        CDebugName
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
        VBitmapMarker,
        nil,
        ABitmap32StaticFactory,
        AProjectedGeometryProvider,
        AMarkerIconProvider
      );
    VTileMatrix :=
      TBitmapTileMatrixChangeableByVectorMatrix.Create(
        VPerfList.CreateAndAddNewSubList('BitmapMatrix'),
        AAppStartedNotifier,
        AAppClosingNotifier,
        VVectorTileMatrix,
        VVectorRenderer,
        ATileMatrixDraftResampler,
        True,
        ABitmap32StaticFactory,
        AHashFunction,
        ALayersConfig.KmlLayerConfig.ThreadConfig,
        CDebugName
      );
    AMatrixList.Add(VTileMatrix);
  end;

  procedure BuildGpsTrackMatrixLayer(
    const AMatrixList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable;
    const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
  );
  const
    CDebugName = 'GpsTrack';
  var
    VPerfList: IInternalPerformanceCounterList;
    VProvider: IBitmapLayerProviderChangeable;
    VTileMatrix: IBitmapTileMatrixChangeable;
  begin
    {$IFDEF DISABLE_GPS_TRACK_LAYER} Exit; {$ENDIF}
    VPerfList := APerfListGroup.CreateAndAddNewSubList(CDebugName);
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
        ATileRectForShow,
        ATileMatrixDraftResampler,
        True,
        ABitmap32StaticFactory,
        AHashFunction,
        VProvider,
        nil,
        ALayersConfig.GPSTrackConfig.ThreadConfig,
        CDebugName
      );
    AMatrixList.Add(VTileMatrix);
  end;

  procedure BuildGpsMarkerLayer(
    const ALayersList: IInterfaceListSimple
  );
  var
    VLayer: IInterface;
    VPerfList: IInternalPerformanceCounterList;
    VMarkerChangeable: IMarkerDrawableChangeable;
    VMarkerWithDirectionChangeable: IMarkerDrawableWithDirectionChangeable;
  begin
    {$IFDEF DISABLE_GPS_MARKER_LAYER} Exit; {$ENDIF}
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
    VPerfList := APerfListGroup.CreateAndAddNewSubList('GpsMarker');
    VLayer :=
      TMapLayerGPSMarker.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AGUISyncronizedTimerNotifier,
        ALayersConfig.GPSMarker,
        VMarkerWithDirectionChangeable,
        VMarkerChangeable,
        AGPSRecorder
      );
    ALayersList.Add(VLayer);

    // Layer with rings around GPS marker
    VPerfList := APerfListGroup.CreateAndAddNewSubList('GpsMarkerRings');
    VLayer :=
      TMapLayerGPSMarkerRings.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AGUISyncronizedTimerNotifier,
        AVectorGeometryProjectedFactory,
        AVectorGeometryLonLatFactory,
        ALayersConfig.GPSMarker.MarkerRingsConfig,
        AGPSRecorder
      );
    ALayersList.Add(VLayer);
  end;

  procedure BuildLastSelectionLayer(
    const ALayersList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable
  );
  var
    VLayer: IInterface;
    VPerfList: IInternalPerformanceCounterList;
    VPolygonChangeable: IGeometryLonLatPolygonChangeable;
  begin
    {$IFDEF DISABLE_LAST_SELECTION_LAYER} Exit; {$ENDIF}
    VPerfList := APerfListGroup.CreateAndAddNewSubList('LastSelection');
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
        AMainFormState,
        ATileRectForShow,
        AVectorGeometryProjectedFactory,
        ALayersConfig.LastSelectionLayerConfig,
        VPolygonChangeable
      );
    ALayersList.Add(VLayer);
  end;

  procedure BuildMergePolygonsResultLayer(
    const ALayersList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable
  );
  var
    VLayer: IInterface;
    VPerfList: IInternalPerformanceCounterList;
    VPolygonChangeable: IGeometryLonLatPolygonChangeable;
  begin
    {$IFDEF DISABLE_MERGE_POLYGONS_RESULT_LAYER} Exit; {$ENDIF}
    VPerfList := APerfListGroup.CreateAndAddNewSubList('MergePolygonsResult');
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
        AMainFormState,
        ATileRectForShow,
        AVectorGeometryProjectedFactory,
        ALayersConfig.MergePolygonsResultLayerConfig,
        VPolygonChangeable
      );
    ALayersList.Add(VLayer);
  end;

  procedure BuildCalcLineLayer(
    const ALayersList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable
  );
  var
    VLayer: IInterface;
    VPerfList: IInternalPerformanceCounterList;
    VGeometryChangeableByPathEdit: TGeometryLonLatChangeableByPathEdit;
  begin
    {$IFDEF DISABLE_CALC_LINE_LAYER} Exit; {$ENDIF}
    // CalcLine points
    VGeometryChangeableByPathEdit :=
      TGeometryLonLatChangeableByPathEdit.Create(
        AVectorGeometryLonLatFactory,
        ACalcLinePath
      );

    VLayer := VGeometryChangeableByPathEdit;
    ALayersList.Add(VLayer);

    // CalcLine line visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('CalcLine');
    VLayer :=
      TMapLayerSingleLine.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        ATileRectForShow,
        AVectorGeometryProjectedFactory,
        ALayersConfig.CalcLineLayerConfig.LineConfig,
        VGeometryChangeableByPathEdit.LineChangeable
      );
    ALayersList.Add(VLayer);

    // CalcLine simple points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('CalcLineSimplePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.OtherPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcLineLayerConfig.PointsConfig.NormalPointMarker)
      );
    ALayersList.Add(VLayer);

    // CalcLine first points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('CalcLineFirstPoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.FirstPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcLineLayerConfig.PointsConfig.FirstPointMarker)
      );
    ALayersList.Add(VLayer);

    // CalcLine active points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('CalcLineActivePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.ActivePointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcLineLayerConfig.PointsConfig.ActivePointMarker)
      );
    ALayersList.Add(VLayer);

    // CalcLine captions layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('CalcLineCaptions');
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
    ALayersList.Add(VLayer);
  end;

  procedure BuildCalcCircleLayer(
    const ALayersList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable
  );
  var
    VLayer: IInterface;
    VPerfList: IInternalPerformanceCounterList;
    VGeometryChangeableByPathEdit: TGeometryLonLatChangeableByPathEdit;
    VGeometryChangeableByPolygonEdit: TGeometryLonLatChangeableByPolygonEdit;
  begin
    {$IFDEF DISABLE_CALC_CIRCLE_LAYER} Exit; {$ENDIF}
    // CalcCircle polygon
    VGeometryChangeableByPolygonEdit :=
      TGeometryLonLatChangeableByPolygonEdit.Create(
        AVectorGeometryLonLatFactory,
        ACircleOnMapEdit.GetPolygonOnMapEdit
      );

    VLayer := VGeometryChangeableByPolygonEdit;
    ALayersList.Add(VLayer);

    // CalcCircle polygon visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('CalcCirclePolygon');
    VLayer :=
      TMapLayerSinglePolygon.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        ATileRectForShow,
        AVectorGeometryProjectedFactory,
        ALayersConfig.CalcCircleLayerConfig.PolygonConfig,
        VGeometryChangeableByPolygonEdit.PolygonChangeable,
        True
      );
    ALayersList.Add(VLayer);

    // CalcCircle line
    VGeometryChangeableByPathEdit :=
      TGeometryLonLatChangeableByPathEdit.Create(
        AVectorGeometryLonLatFactory,
        ACircleOnMapEdit
      );

    VLayer := VGeometryChangeableByPathEdit;
    ALayersList.Add(VLayer);

    // CalcCircle line visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('CalcCircleLine');
    VLayer :=
      TMapLayerSingleLine.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        ATileRectForShow,
        AVectorGeometryProjectedFactory,
        ALayersConfig.CalcCircleLayerConfig.LineConfig,
        VGeometryChangeableByPathEdit.LineChangeable
      );
    ALayersList.Add(VLayer);

    // CalcCircle simple points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('CalcCircleSimplePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.OtherPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcCircleLayerConfig.PointsConfig.NormalPointMarker)
      );
    ALayersList.Add(VLayer);

    // CalcCircle first points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('CalcCircleFirstPoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.FirstPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcCircleLayerConfig.PointsConfig.FirstPointMarker)
      );
    ALayersList.Add(VLayer);

    // CalcCircle active points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('CalcCircleActivePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.ActivePointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.CalcCircleLayerConfig.PointsConfig.ActivePointMarker)
      );
    ALayersList.Add(VLayer);

    // CalcCircle captions layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('CalcCircleCaptions');
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
    ALayersList.Add(VLayer);
  end;

  procedure BuildPathEditLayer(
    const ALayersList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable
  );
  var
    VLayer: IInterface;
    VPerfList: IInternalPerformanceCounterList;
    VGeometryChangeableByPathEdit: TGeometryLonLatChangeableByPathEdit;
  begin
    {$IFDEF DISABLE_PATH_EDIT_LAYER} Exit; {$ENDIF}
    VGeometryChangeableByPathEdit :=
      TGeometryLonLatChangeableByPathEdit.Create(
        AVectorGeometryLonLatFactory,
        AEditLinePath
      );

    VLayer := VGeometryChangeableByPathEdit;
    ALayersList.Add(VLayer);

    // PathEdit line visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PathEdit');
    VLayer :=
      TMapLayerSingleLine.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        ATileRectForShow,
        AVectorGeometryProjectedFactory,
        ALayersConfig.MarkPolyLineLayerConfig.LineConfig,
        VGeometryChangeableByPathEdit.LineChangeable
      );
    ALayersList.Add(VLayer);

    // PathEdit simple points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PathEditSimplePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.OtherPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolyLineLayerConfig.PointsConfig.NormalPointMarker)
      );
    ALayersList.Add(VLayer);

    // PathEdit first points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PathEditFirstPoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.FirstPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolyLineLayerConfig.PointsConfig.FirstPointMarker)
      );
    ALayersList.Add(VLayer);

    // PathEdit active points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PathEditActivePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.ActivePointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolyLineLayerConfig.PointsConfig.ActivePointMarker)
      );
    ALayersList.Add(VLayer);

    // PathEdit captions layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PathEditCaptions');
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
    ALayersList.Add(VLayer);
  end;

  procedure BuildPolygonEditLayer(
    const ALayersList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable
  );
  var
    VLayer: IInterface;
    VPerfList: IInternalPerformanceCounterList;
    VGeometryChangeableByPolygonEdit: TGeometryLonLatChangeableByPolygonEdit;
  begin
    {$IFDEF DISABLE_POLYGON_EDIT_LAYER} Exit; {$ENDIF}
    VGeometryChangeableByPolygonEdit :=
      TGeometryLonLatChangeableByPolygonEdit.Create(
        AVectorGeometryLonLatFactory,
        AEditPolygon
      );

    VLayer := VGeometryChangeableByPolygonEdit;
    ALayersList.Add(VLayer);

    // PolygonEdit line and fill visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PolygonEdit');
    VLayer :=
      TMapLayerSinglePolygon.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        ATileRectForShow,
        AVectorGeometryProjectedFactory,
        ALayersConfig.MarkPolygonLayerConfig.LineConfig,
        VGeometryChangeableByPolygonEdit.PolygonChangeable
      );
    ALayersList.Add(VLayer);

    // PolygonEdit simple points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PolygonEditSimplePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPolygonEdit.OtherPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolygonLayerConfig.PointsConfig.NormalPointMarker)
      );
    ALayersList.Add(VLayer);

    // PolygonEdit first points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PolygonEditFirstPoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPolygonEdit.FirstPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolygonLayerConfig.PointsConfig.FirstPointMarker)
      );
    ALayersList.Add(VLayer);

    // PolygonEdit active points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PolygonEditActivePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPolygonEdit.ActivePointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.MarkPolygonLayerConfig.PointsConfig.ActivePointMarker)
      );
    ALayersList.Add(VLayer);
  end;

  procedure BuildPolygonSelectionLayer(
    const ALayersList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable
  );
  var
    VLayer: IInterface;
    VPerfList: IInternalPerformanceCounterList;
    VGeometryChangeableByPolygonEdit: TGeometryLonLatChangeableByPolygonEdit;
  begin
    {$IFDEF DISABLE_POLYGON_SELECTION_LAYER} Exit; {$ENDIF}
    VGeometryChangeableByPolygonEdit :=
      TGeometryLonLatChangeableByPolygonEdit.Create(
        AVectorGeometryLonLatFactory,
        ASelectPolygon
      );

    VLayer := VGeometryChangeableByPolygonEdit;
    ALayersList.Add(VLayer);

    // PolygonSelection line and fill visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PolygonSelection');
    VLayer :=
      TMapLayerSinglePolygon.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        ATileRectForShow,
        AVectorGeometryProjectedFactory,
        ALayersConfig.SelectionPolygonLayerConfig.LineConfig,
        VGeometryChangeableByPolygonEdit.PolygonChangeable,
        True
      );
    ALayersList.Add(VLayer);

    // PolygonSelection simple points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PolygonSelectionSimplePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPolygonEdit.OtherPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolygonLayerConfig.PointsConfig.NormalPointMarker)
      );
    ALayersList.Add(VLayer);

    // PolygonSelection first points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PolygonSelectionFirstPoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPolygonEdit.FirstPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolygonLayerConfig.PointsConfig.FirstPointMarker)
      );
    ALayersList.Add(VLayer);

    // PolygonSelection active points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('PolygonSelectionActivePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPolygonEdit.ActivePointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolygonLayerConfig.PointsConfig.ActivePointMarker)
      );
    ALayersList.Add(VLayer);
  end;

  procedure BuildSelectionByLineLayer(
    const ALayersList: IInterfaceListSimple;
    const ATileRectForShow: ITileRectChangeable
  );
  var
    VLayer: IInterface;
    VPerfList: IInternalPerformanceCounterList;
    VPolygonChangeable: IGeometryLonLatPolygonChangeable;
    VGeometryChangeableByPathEdit: TGeometryLonLatChangeableByPathEdit;
  begin
    {$IFDEF DISABLE_SELECTION_BY_LINE_LAYER} Exit; {$ENDIF}
    VGeometryChangeableByPathEdit :=
      TGeometryLonLatChangeableByPathEdit.Create(
        AVectorGeometryLonLatFactory,
        ASelectLinePath
      );

    VLayer := VGeometryChangeableByPathEdit;
    ALayersList.Add(VLayer);

    // SelectionByLine shadow visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('SelectionByLineShadow');
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
        AMainFormState,
        ATileRectForShow,
        AVectorGeometryProjectedFactory,
        ALayersConfig.SelectionPolylineLayerConfig.ShadowConfig,
        VPolygonChangeable
      );
    ALayersList.Add(VLayer);

    // SelectionByLyne line visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('SelectionByLine');
    VLayer :=
      TMapLayerSingleLine.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        ATileRectForShow,
        AVectorGeometryProjectedFactory,
        ALayersConfig.SelectionPolylineLayerConfig.LineConfig,
        VGeometryChangeableByPathEdit.LineChangeable
      );
    ALayersList.Add(VLayer);

    // SelectionByLyne simple points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('SelectionByLineSimplePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.OtherPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolylineLayerConfig.PointsConfig.NormalPointMarker)
      );
    ALayersList.Add(VLayer);

    // SelectionByLyne first points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('SelectionByLineFirstPoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.FirstPointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolylineLayerConfig.PointsConfig.FirstPointMarker)
      );
    ALayersList.Add(VLayer);

    // SelectionByLyne active points visualisation layer
    VPerfList := APerfListGroup.CreateAndAddNewSubList('SelectionByLineActivePoints');
    VLayer :=
      TMapLayerPointsSet.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        AVectorGeometryProjectedFactory,
        VGeometryChangeableByPathEdit.ActivePointsChangeable,
        TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, ALayersConfig.SelectionPolylineLayerConfig.PointsConfig.ActivePointMarker)
      );
    ALayersList.Add(VLayer);
  end;

  procedure BuildSelectionByRectLayer(
    const ALayersList: IInterfaceListSimple
  );
  var
    VLayer: IInterface;
    VPerfList: IInternalPerformanceCounterList;
  begin
    {$IFDEF DISABLE_SELECTION_BY_RECT_LAYER} Exit; {$ENDIF}
    VPerfList := APerfListGroup.CreateAndAddNewSubList('SelectionByRect');
    VLayer :=
      TMapLayerSelectionByRect.Create(
        VPerfList,
        AAppStartedNotifier,
        AAppClosingNotifier,
        AParentMap,
        AViewPortState.View,
        AMainFormState,
        ASelectionRect,
        ALayersConfig.SelectionRectLayerConfig,
        True
      );
    ALayersList.Add(VLayer);
  end;

var
  VBitmap: IBitmap32Static;
  VBitmapMarker: IBitmapMarker;
  VTextDrawerBasic: ITextDrawerBasic;
  VMarkerChangeable: IMarkerDrawableChangeable;
  VMarkerWithDirectionChangeable: IMarkerDrawableWithDirectionChangeable;
  VLicensList: IStringListChangeable;
  VMiniMapConverterChangeable: ILocalCoordConverterChangeable;
  VBitmapChangeable: IBitmapChangeable;
  VMarkerProviderForVectorItem: IMarkerProviderForVectorItem;
  VMarkerIconProvider: IMarkerProviderByAppearancePointIcon;
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

  VMarkerIconProvider :=
    TMarkerProviderByAppearancePointIcon.Create(
      APerfListGroup.CreateAndAddNewSubList('VectorItemIcons'),
      AHashFunction,
      ABitmap32StaticFactory,
      nil
    );

  VLayersList := TInterfaceListSimple.Create;
  VMatrixList := TInterfaceListSimple.Create;

  // ========== Tiled Layers ==========

  // Raster maps visualisation layer
  BuildRasterMapsMatrixLayer(
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler
  );

  // Grids visualisation layer
  BuildGridsMatrixLayer(
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler
  );

  // Vector maps visualisation layer
  BuildVectorMapsMatrixLayer(
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler,
    VMarkerIconProvider
  );

  // Filling map visualisation layer
  BuildFillingMapMatrixLayer(
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler
  );

  // Marks from MarkSystem visualisation layer
  BuildMarksMatrixLayer(
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler,
    VMarkerIconProvider
  );

  // Vector search results visualisation layer
  BuildSearchResultsMatrixLayer(
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler,
    VMarkerIconProvider
  );

  // GPS track visualisation layer
  BuildGpsTrackMatrixLayer(
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler
  );

  // ========== Composite Tiled Layer ==========

  VDebugName := 'Composite';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  if VMatrixList.Count > 1 then begin
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
  end else
  if VMatrixList.Count > 0 then begin
    VTileMatrix := VMatrixList.Items[0] as IBitmapTileMatrixChangeable;
  end else begin
    VTileMatrix := nil;
  end;
  if VTileMatrix <> nil then begin
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
  end;

  VMatrixList := nil;

  // ========== Regular Layers ==========

  // GPS marker layer
  BuildGpsMarkerLayer(VLayersList);

  // Last selection visualisation layer
  BuildLastSelectionLayer(VLayersList, VTileRectForShow);

  // Merge polygons result visualisation layer
  BuildMergePolygonsResultLayer(VLayersList, VTileRectForShow);

  // Calc line visualisation layer
  BuildCalcLineLayer(VLayersList, VTileRectForShow);

  // Calc circle visualisation layer
  BuildCalcCircleLayer(VLayersList, VTileRectForShow);

  // Path edit visualisation layer
  BuildPathEditLayer(VLayersList, VTileRectForShow);

  // Polygon edit visualisation layer
  BuildPolygonEditLayer(VLayersList, VTileRectForShow);

  // Polygon selection visualisation layer
  BuildPolygonSelectionLayer(VLayersList, VTileRectForShow);

  // Selection by line visualisation layer
  BuildSelectionByLineLayer(VLayersList, VTileRectForShow);

  // Selection by rect visualisation layer
  BuildSelectionByRectLayer(VLayersList);

  // Goto marker visualisation layer
  {$IFNDEF DISABLE_GOTO_MARKER_LAYER}
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
      AMainFormState,
      VMarkerChangeable,
      AMapGoto,
      ALayersConfig.GotoLayerConfig
    );
  VLayersList.Add(VLayer);
  {$ENDIF}

  // Gps Track Goto marker visualisation layer
  {$IFNDEF DISABLE_GPS_TRACK_GOTO_MARKER_LAYER}
  VBitmap :=
    ReadBitmapByFileRef(
      AResourceProvider,
      'ARROW.png',
      AContentTypeManager,
      nil
    );
  VMarkerChangeable := nil;
  if VBitmap <> nil then begin
    VMarkerChangeable :=
      TMarkerDrawableChangeableFaked.Create(
        TMarkerDrawableByBitmap32Static.Create(VBitmap, DoublePoint(12, 24))
      );
  end;
  VDebugName := 'GpsTrackGoToMarker';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TMapLayerGpsTrackGoToMarker.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AMainFormState,
      VMarkerChangeable,
      AGpsTrackGoTo
    );
  VLayersList.Add(VLayer);
  {$ENDIF}

  // Navigation to mark marker visualisation layer
  {$IFNDEF DISABLE_NAVIGATION_TO_MARK_MARKER_LAYER}
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
      AMainFormState,
      ANavToPoint,
      VMarkerWithDirectionChangeable,
      VMarkerChangeable,
      ALayersConfig.NavToPointMarkerConfig
    );
  VLayersList.Add(VLayer);
  {$ENDIF}

  // Error info visualisation layer
  {$IFNDEF DISABLE_ERROR_INFO_LAYER}
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
  {$ENDIF}

  // Point edit marker visualisation layer
  {$IFNDEF DISABLE_POINT_EDIT_MARKER_LAYER}
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
      AMainFormState,
      VMarkerChangeable,
      APointOnMapEdit
    );
  VLayersList.Add(VLayer);
  {$ENDIF}

  // Full map cursor layer
  {$IFNDEF DISABLE_FULL_MAP_CURSOR_LAYER}
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
  {$ENDIF}

  // Center scale layer
  {$IFNDEF DISABLE_CENTER_SCALE_LAYER}
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
  {$ENDIF}

  // Scale line layer
  {$IFNDEF DISABLE_SCALE_LINE_LAYER}
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
  {$ENDIF}

  // Map licenses visualisation layer
  {$IFNDEF DISABLE_MAP_LICENSES_LAYER}
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
  {$ENDIF}

  // Status bar layer
  {$IFNDEF DISABLE_STATUS_BAR_LAYER}
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
  {$ENDIF}

  // SunCalc Layer
  {$IFNDEF DISABLE_SUN_CALC_LAYER}
  VDebugName := 'SunCalc/YearInfo';
  VPerfList := APerfListGroup.CreateAndAddNewSubList(VDebugName);
  VLayer :=
    TWindowLayerSunCalcYearInfo.Create(
      VPerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      AParentMap,
      AViewPortState.View,
      AMainFormState,
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
      AMainFormState,
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
      AMainFormState,
      ASunCalcConfig,
      ASunCalcProvider,
      AGUISyncronizedTimerNotifier
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
  {$ENDIF}

  // MiniMap layer
  {$IFNDEF DISABLE_MINI_MAP_LAYER}
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
  {$ENDIF}

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

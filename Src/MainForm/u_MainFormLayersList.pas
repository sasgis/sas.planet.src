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
//  {$DEFINE DISABLE_GOTO_MARKER_LAYER}
//  {$DEFINE DISABLE_GPS_TRACK_GOTO_MARKER_LAYER}
//  {$DEFINE DISABLE_NAVIGATION_TO_MARK_MARKER_LAYER}
//  {$DEFINE DISABLE_TILE_ERROR_INFO_LAYER}
//  {$DEFINE DISABLE_POINT_EDIT_MARKER_LAYER}
//  {$DEFINE DISABLE_FULL_MAP_MOUSE_CURSOR_LAYER}
//  {$DEFINE DISABLE_CENTER_SCALE_LAYER}
//  {$DEFINE DISABLE_SCALE_LINE_LAYER}
//  {$DEFINE DISABLE_LICENSE_LIST_LAYER}
//  {$DEFINE DISABLE_STATUS_BAR_LAYER}
//  {$DEFINE DISABLE_SUN_CALC_LAYER}
//  {$DEFINE DISABLE_MINI_MAP_LAYER}
{$ENDIF}

uses
  t_MainFormLayersListParams,
  i_InterfaceListSimple,
  i_InterfaceListStatic,
  i_TileRectChangeable,
  i_ImageResamplerFactoryChangeable,
  i_MarkerProviderByAppearancePointIcon,
  i_InternalPerformanceCounter,
  i_FindVectorItems,
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
  private
    procedure BuildCalcCircleLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable
    );
    procedure BuildCalcLineLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable
    );
    procedure BuildFillingMapMatrixLayer(
      const AParams: TMainFormLayersListParams;
      const AMatrixList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable;
      const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
    );
    procedure BuildGpsMarkerLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildGpsTrackMatrixLayer(
      const AParams: TMainFormLayersListParams;
      const AMatrixList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable;
      const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
    );
    procedure BuildGridsMatrixLayer(
      const AParams: TMainFormLayersListParams;
      const AMatrixList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable;
      const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
    );
    procedure BuildLastSelectionLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable
    );
    procedure BuildMarksMatrixLayer(
      const AParams: TMainFormLayersListParams;
      const AMatrixList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable;
      const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable;
      const AMarkerIconProvider: IMarkerProviderByAppearancePointIcon
    );
    procedure BuildMergePolygonsResultLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable
    );
    procedure BuildPathEditLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable
    );
    procedure BuildPolygonEditLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable
    );
    procedure BuildPolygonSelectionLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable
    );
    procedure BuildRasterMapsMatrixLayer(
      const AParams: TMainFormLayersListParams;
      const AMatrixList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable;
      const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
    );
    procedure BuildSearchResultsMatrixLayer(
      const AParams: TMainFormLayersListParams;
      const AMatrixList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable;
      const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable;
      const AMarkerIconProvider: IMarkerProviderByAppearancePointIcon
    );
    procedure BuildSelectionByLineLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable
    );
    procedure BuildSelectionByRectLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildVectorMapsMatrixLayer(
      const AParams: TMainFormLayersListParams;
      const AMatrixList: IInterfaceListSimple;
      const ATileRectForShow: ITileRectChangeable;
      const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable;
      const AMarkerIconProvider: IMarkerProviderByAppearancePointIcon
    );
    procedure BuildGotoMarkerLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildGpsTrackGotoMarkerLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildNavigationToMarkMarkerLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildTileErrorInfoLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildPointEditMarkerLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildFullMapMouseCursorLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildCenterScaleLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildScaleLineLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildLicenseListLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildStatusBarLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildSunCalcLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple
    );
    procedure BuildMiniMapLayer(
      const AParams: TMainFormLayersListParams;
      const ALayersList: IInterfaceListSimple;
      const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
    );
  public
    constructor Create(
      const AParams: TMainFormLayersListParams
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
  i_MarkerProviderForVectorItem,
  i_VectorItemSubsetChangeable,
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
  u_MapLayerPolygonCaptions,
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
  const AParams: TMainFormLayersListParams
);
var
  VLayer: IInterface;
  VDebugName: string;
  VPerfList: IInternalPerformanceCounterList;
  VMatrixList: IInterfaceListSimple;
  VLayersList: IInterfaceListSimple;
  VTileRectForShow: ITileRectChangeable;
  VTileMatrix: IBitmapTileMatrixChangeable;
  VTileMatrixDraftResampler: IImageResamplerFactoryChangeable;
  VMarkerIconProvider: IMarkerProviderByAppearancePointIcon;
begin
  inherited Create;

  VTileRectForShow :=
    TTileRectChangeableByLocalConverterSmart.Create(
      AParams.ActiveProjectionSet,
      AParams.ViewPortState.View,
      GSync.SyncVariable.Make('TileRectForShowMain'),
      GSync.SyncVariable.Make('TileRectForShowResult')
    );

  VTileMatrixDraftResampler :=
    TImageResamplerFactoryChangeableByConfig.Create(
      AParams.GlobalConfig.TileMatrixDraftResamplerConfig,
      AParams.ImageResamplerFactoryList
    );

  VMarkerIconProvider :=
    TMarkerProviderByAppearancePointIcon.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('VectorItemIcons'),
      AParams.HashFunction,
      AParams.Bitmap32StaticFactory,
      nil
    );

  VLayersList := TInterfaceListSimple.Create;
  VMatrixList := TInterfaceListSimple.Create;

  // ========== Tiled Layers ==========

  // Raster maps visualisation layer
  BuildRasterMapsMatrixLayer(
    AParams,
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler
  );

  // Grids visualisation layer
  BuildGridsMatrixLayer(
    AParams,
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler
  );

  // Vector maps visualisation layer
  BuildVectorMapsMatrixLayer(
    AParams,
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler,
    VMarkerIconProvider
  );

  // Filling map visualisation layer
  BuildFillingMapMatrixLayer(
    AParams,
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler
  );

  // Marks from MarkSystem visualisation layer
  BuildMarksMatrixLayer(
    AParams,
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler,
    VMarkerIconProvider
  );

  // Vector search results visualisation layer
  BuildSearchResultsMatrixLayer(
    AParams,
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler,
    VMarkerIconProvider
  );

  // GPS track visualisation layer
  BuildGpsTrackMatrixLayer(
    AParams,
    VMatrixList,
    VTileRectForShow,
    VTileMatrixDraftResampler
  );

  // ========== Composite Tiled Layer ==========

  VDebugName := 'Composite';
  VPerfList := AParams.PerfListGroup.CreateAndAddNewSubList(VDebugName);
  if VMatrixList.Count > 1 then begin
    VTileMatrix :=
      TBitmapTileMatrixChangeableComposite.Create(
        VPerfList,
        AParams.AppStartedNotifier,
        AParams.AppClosingNotifier,
        VTileRectForShow,
        VMatrixList.MakeStaticAndClear,
        VTileMatrixDraftResampler,
        True,
        AParams.Bitmap32StaticFactory,
        AParams.HashFunction,
        AParams.LayersConfig.KmlLayerConfig.ThreadConfig,
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
        AParams.AppStartedNotifier,
        AParams.AppClosingNotifier,
        AParams.ParentMap,
        AParams.HashFunction,
        AParams.ViewPortState.View,
        VTileMatrix,
        AParams.GUISyncronizedTimerNotifier,
        VDebugName
      );
    VLayersList.Add(VLayer);
  end;

  VMatrixList := nil;

  // ========== Regular Layers ==========

  // GPS marker layer
  BuildGpsMarkerLayer(AParams, VLayersList);

  // Last selection visualisation layer
  BuildLastSelectionLayer(AParams, VLayersList, VTileRectForShow);

  // Merge polygons result visualisation layer
  BuildMergePolygonsResultLayer(AParams, VLayersList, VTileRectForShow);

  // Calc line visualisation layer
  BuildCalcLineLayer(AParams, VLayersList, VTileRectForShow);

  // Calc circle visualisation layer
  BuildCalcCircleLayer(AParams, VLayersList, VTileRectForShow);

  // Path edit visualisation layer
  BuildPathEditLayer(AParams, VLayersList, VTileRectForShow);

  // Polygon edit visualisation layer
  BuildPolygonEditLayer(AParams, VLayersList, VTileRectForShow);

  // Polygon selection visualisation layer
  BuildPolygonSelectionLayer(AParams, VLayersList, VTileRectForShow);

  // Selection by line visualisation layer
  BuildSelectionByLineLayer(AParams, VLayersList, VTileRectForShow);

  // Selection by rect visualisation layer
  BuildSelectionByRectLayer(AParams, VLayersList);

  // Goto marker visualisation layer
  BuildGotoMarkerLayer(AParams, VLayersList);

  // Gps Track Goto marker visualisation layer
  BuildGpsTrackGotoMarkerLayer(AParams, VLayersList);

  // Navigation to mark marker visualisation layer
  BuildNavigationToMarkMarkerLayer(AParams, VLayersList);

  // Tile error info visualisation layer
  BuildTileErrorInfoLayer(AParams, VLayersList);

  // Point edit marker visualisation layer
  BuildPointEditMarkerLayer(AParams, VLayersList);

  // Full map mouse cursor layer
  BuildFullMapMouseCursorLayer(AParams, VLayersList);

  // Center scale layer
  BuildCenterScaleLayer(AParams, VLayersList);

  // Scale line layer
  BuildScaleLineLayer(AParams, VLayersList);

  // Map licenses visualisation layer
  BuildLicenseListLayer(AParams, VLayersList);

  // Status bar layer
  BuildStatusBarLayer(AParams, VLayersList);

  // Sun/Moon calc layer
  BuildSunCalcLayer(AParams, VLayersList);

  // Mini map layer
  BuildMiniMapLayer(AParams, VLayersList, VTileMatrixDraftResampler);

  FLayersList := VLayersList.MakeStaticAndClear;
end;

procedure TMainFormLayersList.BuildRasterMapsMatrixLayer(
  const AParams: TMainFormLayersListParams;
  const AMatrixList: IInterfaceListSimple;
  const ATileRectForShow: ITileRectChangeable;
  const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
);
const
  CDebugName = 'RasterMaps';
var
  VProvider: IBitmapLayerProviderChangeable;
  VSourceChangeNotifier: IObjectWithListener;
  VTileMatrix: IBitmapTileMatrixChangeable;
begin
  {$IFDEF DISABLE_RASTER_MAPS_LAYER} Exit; {$ENDIF}
  VProvider :=
    TBitmapLayerProviderChangeableForMainLayer.Create(
      AParams.MainMapState.ActiveMap,
      AParams.MainMapState.ActiveBitmapLayersList,
      AParams.BitmapPostProcessing,
      AParams.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig,
      AParams.Bitmap32StaticFactory,
      AParams.TileErrorLogger
    );
  VSourceChangeNotifier :=
    TSourceDataUpdateInRectByMapsSet.Create(
      AParams.MainMapState.ActiveBitmapMapsSet
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableWithThread.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList(CDebugName),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      ATileRectForShow,
      ATileMatrixDraftResampler,
      True,
      AParams.Bitmap32StaticFactory,
      AParams.HashFunction,
      VProvider,
      VSourceChangeNotifier,
      AParams.LayersConfig.MainMapLayerConfig.ThreadConfig,
      CDebugName
    );
  AMatrixList.Add(VTileMatrix);
end;

procedure TMainFormLayersList.BuildGridsMatrixLayer(
  const AParams: TMainFormLayersListParams;
  const AMatrixList: IInterfaceListSimple;
  const ATileRectForShow: ITileRectChangeable;
  const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
);
const
  CDebugName = 'Grids';
var
  VProvider: IBitmapLayerProviderChangeable;
  VTileMatrix: IBitmapTileMatrixChangeable;
begin
  {$IFDEF DISABLE_GRIDS_LAYER} Exit; {$ENDIF}
  VProvider :=
    TBitmapLayerProviderChangeableForGrids.Create(
      AParams.Bitmap32StaticFactory,
      AParams.ActiveProjectionSet,
      AParams.CoordToStringConverter,
      AParams.LayersConfig.MapLayerGridsConfig
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableWithThread.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList(CDebugName),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      ATileRectForShow,
      ATileMatrixDraftResampler,
      False,
      AParams.Bitmap32StaticFactory,
      AParams.HashFunction,
      VProvider,
      nil,
      AParams.LayersConfig.MapLayerGridsConfig.ThreadConfig,
      CDebugName
    );
  AMatrixList.Add(VTileMatrix);
end;

procedure TMainFormLayersList.BuildVectorMapsMatrixLayer(
  const AParams: TMainFormLayersListParams;
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
  VPerfList := AParams.PerfListGroup.CreateAndAddNewSubList(CDebugName);
  VVectorOversizeRect := Rect(10, 10, 10, 10);
  VMarkerChangeable :=
    TMarkerDrawableChangeableSimple.Create(
      TMarkerDrawableSimpleSquare,
      AParams.LayersConfig.KmlLayerConfig.PointMarkerConfig
    );
  VVectorTileProvider :=
    TVectorTileProviderChangeableForVectorLayers.Create(
      AParams.MainMapState.ActiveKmlLayersSet,
      AParams.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig,
      AParams.VectorItemSubsetBuilderFactory,
      AParams.TileErrorLogger,
      Rect(300, 300, 300, 300),
      VVectorOversizeRect
    );
  VSourceChangeNotifier :=
    TSourceDataUpdateInRectByMapsSet.Create(
      AParams.MainMapState.ActiveKmlLayersSet
    );
  VVectorTileMatrix :=
    TVectorTileMatrixChangeableForVectorLayers.Create(
      VPerfList.CreateAndAddNewSubList('VectorMatrix'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      ATileRectForShow,
      AParams.HashFunction,
      AParams.VectorItemSubsetBuilderFactory,
      False,
      VVectorTileProvider,
      VSourceChangeNotifier,
      AParams.LayersConfig.KmlLayerConfig.ThreadConfig,
      VVectorOversizeRect,
      CDebugName
    );

  FWikiLayer :=
    TFindVectorItemsForVectorTileMatrix.Create(
      AParams.VectorItemSubsetBuilderFactory,
      AParams.ProjectedGeometryProvider,
      VVectorTileMatrix,
      VPerfList.CreateAndAddNewCounter('FindItems'),
      6
    );

  VVectorRenderer :=
    TVectorTileRendererChangeableForVectorMaps.Create(
      AParams.LayersConfig.KmlLayerConfig.DrawConfig,
      nil,
      VMarkerChangeable,
      AParams.Bitmap32StaticFactory,
      AParams.ProjectedGeometryProvider,
      AMarkerIconProvider
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableByVectorMatrix.Create(
      VPerfList.CreateAndAddNewSubList('BitmapMatrix'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      VVectorTileMatrix,
      VVectorRenderer,
      ATileMatrixDraftResampler,
      True,
      AParams.Bitmap32StaticFactory,
      AParams.HashFunction,
      AParams.LayersConfig.KmlLayerConfig.ThreadConfig,
      CDebugName
    );
  AMatrixList.Add(VTileMatrix);
end;

procedure TMainFormLayersList.BuildFillingMapMatrixLayer(
  const AParams: TMainFormLayersListParams;
  const AMatrixList: IInterfaceListSimple;
  const ATileRectForShow: ITileRectChangeable;
  const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
);
const
  CDebugName = 'FillingMap';
var
  VProvider: IBitmapLayerProviderChangeable;
  VSourceChangeNotifier: IObjectWithListener;
  VTileMatrix: IBitmapTileMatrixChangeable;
begin
  {$IFDEF DISABLE_FILLING_MAP_LAYER} Exit; {$ENDIF}
  VProvider :=
    TBitmapLayerProviderChangeableForFillingMap.Create(
      AParams.Bitmap32StaticFactory,
      AParams.VectorGeometryProjectedFactory,
      AParams.MainMapState.FillingMapActiveMap,
      AParams.FillingMapPolygon,
      AParams.LayersConfig.FillingMapLayerConfig
    );
  VSourceChangeNotifier :=
    TSourceDataUpdateInRectByFillingMap.Create(
      AParams.MainMapState.FillingMapActiveMap,
      AParams.LayersConfig.FillingMapLayerConfig
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableWithThread.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList(CDebugName),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      ATileRectForShow,
      ATileMatrixDraftResampler,
      True,
      AParams.Bitmap32StaticFactory,
      AParams.HashFunction,
      VProvider,
      VSourceChangeNotifier,
      AParams.LayersConfig.FillingMapLayerConfig.ThreadConfig,
      CDebugName
    );
  AMatrixList.Add(VTileMatrix);
end;

procedure TMainFormLayersList.BuildMarksMatrixLayer(
  const AParams: TMainFormLayersListParams;
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
  VVectorOversizeRect := AParams.LayersConfig.MarksLayerConfig.MarksDrawConfig.DrawOrderConfig.OverSizeRect;
  VPerfList := AParams.PerfListGroup.CreateAndAddNewSubList(CDebugName);
  VVectorItems :=
    TVectorItemSubsetChangeableForMarksLayer.Create(
      VPerfList,
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      ATileRectForShow,
      AParams.MarkSystem,
      AParams.LayersConfig.MarksLayerConfig.MarksShowConfig,
      VVectorOversizeRect,
      AParams.LayersConfig.MarksLayerConfig.ThreadConfig
    );
  VVectorTileMatrix :=
    TVectorTileMatrixChangeableByVectorSubsetChangeable.Create(
      VPerfList.CreateAndAddNewSubList('VectorMatrix'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      ATileRectForShow,
      AParams.HashFunction,
      AParams.VectorItemSubsetBuilderFactory,
      False,
      VVectorItems,
      AParams.LayersConfig.MarksLayerConfig.ThreadConfig,
      VVectorOversizeRect,
      CDebugName
    );

  FLayerMapMarks :=
    TFindVectorItemsForVectorTileMatrix.Create(
      AParams.VectorItemSubsetBuilderFactory,
      AParams.ProjectedGeometryProvider,
      VVectorTileMatrix,
      VPerfList.CreateAndAddNewCounter('FindItems'),
      24
    );

  VBitmap :=
    ReadBitmapByFileRef(
      AParams.ResourceProvider,
      'RED.png',
      AParams.ContentTypeManager,
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
      AParams.HashFunction,
      AParams.Bitmap32StaticFactory,
      Max(Max(VVectorOversizeRect.Left, VVectorOversizeRect.Right), Max(VVectorOversizeRect.Top, VVectorOversizeRect.Bottom)),
      True,
      AParams.LayersConfig.MarksLayerConfig.MarksDrawConfig.CaptionDrawConfig.FontName
    );
  VMarkerProviderForVectorItem :=
    TMarkerProviderForVectorItemWithCache.Create(
      VPerfList.CreateAndAddNewSubList('Marker'),
      AParams.HashFunction,
      TMarkerProviderForVectorItemForMarkPoints.Create(VTextDrawerBasic, AMarkerIconProvider)
    );
  VVectorRenderer :=
    TVectorTileRendererChangeableForMarksLayer.Create(
      AParams.LayersConfig.MarksLayerConfig.MarksDrawConfig.CaptionDrawConfig,
      AParams.Bitmap32StaticFactory,
      AParams.ProjectedGeometryProvider,
      VMarkerProviderForVectorItem
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableByVectorMatrix.Create(
      VPerfList.CreateAndAddNewSubList('BitmapMatrix'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      VVectorTileMatrix,
      VVectorRenderer,
      ATileMatrixDraftResampler,
      True,
      AParams.Bitmap32StaticFactory,
      AParams.HashFunction,
      AParams.LayersConfig.MarksLayerConfig.ThreadConfig,
      CDebugName
    );
  AMatrixList.Add(VTileMatrix);
end;

procedure TMainFormLayersList.BuildSearchResultsMatrixLayer(
  const AParams: TMainFormLayersListParams;
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
  VPerfList := AParams.PerfListGroup.CreateAndAddNewSubList(CDebugName);
  VBitmap :=
    ReadBitmapByFileRef(
      AParams.ResourceProvider,
      'FOUNDPNT.png',
      AParams.ContentTypeManager,
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
      AParams.LastSearchResult,
      AParams.VectorItemSubsetBuilderFactory,
      VVectorOversizeRect
    );
  VVectorTileMatrix :=
    TVectorTileMatrixChangeableForVectorLayers.Create(
      VPerfList.CreateAndAddNewSubList('VectorMatrix'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      ATileRectForShow,
      AParams.HashFunction,
      AParams.VectorItemSubsetBuilderFactory,
      False,
      VVectorTileProvider,
      nil,
      AParams.LayersConfig.KmlLayerConfig.ThreadConfig,
      VVectorOversizeRect,
      CDebugName
    );

  FLayerSearchResults :=
    TFindVectorItemsForVectorTileMatrix.Create(
      AParams.VectorItemSubsetBuilderFactory,
      AParams.ProjectedGeometryProvider,
      VVectorTileMatrix,
      VPerfList.CreateAndAddNewCounter('FindItems'),
      6
    );

  VVectorRenderer :=
    TVectorTileRendererChangeableForVectorMaps.Create(
      AParams.LayersConfig.KmlLayerConfig.DrawConfig,
      VBitmapMarker,
      nil,
      AParams.Bitmap32StaticFactory,
      AParams.ProjectedGeometryProvider,
      AMarkerIconProvider
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableByVectorMatrix.Create(
      VPerfList.CreateAndAddNewSubList('BitmapMatrix'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      VVectorTileMatrix,
      VVectorRenderer,
      ATileMatrixDraftResampler,
      True,
      AParams.Bitmap32StaticFactory,
      AParams.HashFunction,
      AParams.LayersConfig.KmlLayerConfig.ThreadConfig,
      CDebugName
    );
  AMatrixList.Add(VTileMatrix);
end;

procedure TMainFormLayersList.BuildGpsTrackMatrixLayer(
  const AParams: TMainFormLayersListParams;
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
  VPerfList := AParams.PerfListGroup.CreateAndAddNewSubList(CDebugName);
  VProvider :=
    TBitmapLayerProviderChangeableForGpsTrack.Create(
      VPerfList,
      AParams.GUISyncronizedTimerNotifier,
      AParams.LayersConfig.GPSTrackConfig,
      AParams.Bitmap32StaticFactory,
      AParams.GpsTrackRecorder
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableWithThread.Create(
      VPerfList,
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      ATileRectForShow,
      ATileMatrixDraftResampler,
      True,
      AParams.Bitmap32StaticFactory,
      AParams.HashFunction,
      VProvider,
      nil,
      AParams.LayersConfig.GPSTrackConfig.ThreadConfig,
      CDebugName
    );
  AMatrixList.Add(VTileMatrix);
end;

procedure TMainFormLayersList.BuildGpsMarkerLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
  VMarkerChangeable: IMarkerDrawableChangeable;
  VMarkerWithDirectionChangeable: IMarkerDrawableWithDirectionChangeable;
begin
  {$IFDEF DISABLE_GPS_MARKER_LAYER} Exit; {$ENDIF}
  // GPS marker layer
  VMarkerChangeable :=
    TMarkerDrawableChangeableSimple.Create(
      TMarkerDrawableSimpleSquare,
      AParams.LayersConfig.GPSMarker.StopedMarkerConfig
    );
  VMarkerWithDirectionChangeable :=
    TMarkerDrawableWithDirectionChangeableSimple.Create(
      TMarkerDrawableSimpleArrow,
      AParams.LayersConfig.GPSMarker.MovedMarkerConfig
    );
  VLayer :=
    TMapLayerGPSMarker.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('GpsMarker'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.GUISyncronizedTimerNotifier,
      AParams.LayersConfig.GPSMarker,
      VMarkerWithDirectionChangeable,
      VMarkerChangeable,
      AParams.GPSRecorder
    );
  ALayersList.Add(VLayer);

  // Layer with rings around GPS marker
  VLayer :=
    TMapLayerGPSMarkerRings.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('GpsMarkerRings'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.GUISyncronizedTimerNotifier,
      AParams.VectorGeometryProjectedFactory,
      AParams.VectorGeometryLonLatFactory,
      AParams.LayersConfig.GPSMarker.MarkerRingsConfig,
      AParams.GPSRecorder
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildLastSelectionLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple;
  const ATileRectForShow: ITileRectChangeable
);
var
  VLayer: IInterface;
  VPolygonChangeable: IGeometryLonLatPolygonChangeable;
begin
  {$IFDEF DISABLE_LAST_SELECTION_LAYER} Exit; {$ENDIF}
  VPolygonChangeable :=
    TGeometryLonLatPolygonChangeableByLastSelection.Create(
      AParams.LayersConfig.LastSelectionLayerConfig,
      AParams.LastSelectionInfo
    );
  VLayer :=
    TMapLayerSinglePolygon.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('LastSelection'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      ATileRectForShow,
      AParams.VectorGeometryProjectedFactory,
      AParams.LayersConfig.LastSelectionLayerConfig,
      VPolygonChangeable
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildMergePolygonsResultLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple;
  const ATileRectForShow: ITileRectChangeable
);
var
  VLayer: IInterface;
  VPolygonChangeable: IGeometryLonLatPolygonChangeable;
begin
  {$IFDEF DISABLE_MERGE_POLYGONS_RESULT_LAYER} Exit; {$ENDIF}
  VPolygonChangeable :=
    TGeometryLonLatPolygonChangeableByMergePolygonsResult.Create(
      AParams.LayersConfig.MergePolygonsResultLayerConfig,
      AParams.MergePolygonsResult
    );
  VLayer :=
    TMapLayerSinglePolygon.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('MergePolygonsResult'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      ATileRectForShow,
      AParams.VectorGeometryProjectedFactory,
      AParams.LayersConfig.MergePolygonsResultLayerConfig,
      VPolygonChangeable
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildCalcLineLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple;
  const ATileRectForShow: ITileRectChangeable
);
var
  VLayer: IInterface;
  VGeometryChangeableByPathEdit: TGeometryLonLatChangeableByPathEdit;
begin
  {$IFDEF DISABLE_CALC_LINE_LAYER} Exit; {$ENDIF}
  // CalcLine points
  VGeometryChangeableByPathEdit :=
    TGeometryLonLatChangeableByPathEdit.Create(
      AParams.VectorGeometryLonLatFactory,
      AParams.CalcLinePath
    );

  VLayer := VGeometryChangeableByPathEdit;
  ALayersList.Add(VLayer);

  // Line visualisation layer
  VLayer :=
    TMapLayerSingleLine.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CalcLine'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      ATileRectForShow,
      AParams.VectorGeometryProjectedFactory,
      AParams.LayersConfig.CalcLineLayerConfig.LineConfig,
      VGeometryChangeableByPathEdit.LineChangeable
    );
  ALayersList.Add(VLayer);

  // Simple points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CalcLineSimplePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.CalcLineLayerConfig.PointsConfig.NormalPointMarker)
    );
  ALayersList.Add(VLayer);

  // First points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CalcLineFirstPoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.CalcLineLayerConfig.PointsConfig.FirstPointMarker)
    );
  ALayersList.Add(VLayer);

  // Active points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CalcLineActivePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.CalcLineLayerConfig.PointsConfig.ActivePointMarker)
    );
  ALayersList.Add(VLayer);

  // Captions layer
  VLayer :=
    TMapLayerCalcLineCaptions.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CalcLineCaptions'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.CalcLinePath,
      AParams.LayersConfig.CalcLineLayerConfig.CaptionConfig,
      AParams.ValueToStringConverter,
      AParams.GeoCalc
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildCalcCircleLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple;
  const ATileRectForShow: ITileRectChangeable
);
var
  VLayer: IInterface;
  VGeometryChangeableByPathEdit: TGeometryLonLatChangeableByPathEdit;
  VGeometryChangeableByPolygonEdit: TGeometryLonLatChangeableByPolygonEdit;
begin
  {$IFDEF DISABLE_CALC_CIRCLE_LAYER} Exit; {$ENDIF}
  // Circle polygon
  VGeometryChangeableByPolygonEdit :=
    TGeometryLonLatChangeableByPolygonEdit.Create(
      AParams.VectorGeometryLonLatFactory,
      AParams.CircleOnMapEdit.GetPolygonOnMapEdit
    );

  VLayer := VGeometryChangeableByPolygonEdit;
  ALayersList.Add(VLayer);

  // Circle polygon visualisation layer
  VLayer :=
    TMapLayerSinglePolygon.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CalcCirclePolygon'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      ATileRectForShow,
      AParams.VectorGeometryProjectedFactory,
      AParams.LayersConfig.CalcCircleLayerConfig.PolygonConfig,
      VGeometryChangeableByPolygonEdit.PolygonChangeable,
      True
    );
  ALayersList.Add(VLayer);

  // Circle line
  VGeometryChangeableByPathEdit :=
    TGeometryLonLatChangeableByPathEdit.Create(
      AParams.VectorGeometryLonLatFactory,
      AParams.CircleOnMapEdit
    );

  VLayer := VGeometryChangeableByPathEdit;
  ALayersList.Add(VLayer);

  // Circle line visualisation layer
  VLayer :=
    TMapLayerSingleLine.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CalcCircleLine'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      ATileRectForShow,
      AParams.VectorGeometryProjectedFactory,
      AParams.LayersConfig.CalcCircleLayerConfig.LineConfig,
      VGeometryChangeableByPathEdit.LineChangeable
    );
  ALayersList.Add(VLayer);

  // Simple points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CalcCircleSimplePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.CalcCircleLayerConfig.PointsConfig.NormalPointMarker)
    );
  ALayersList.Add(VLayer);

  // First points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CalcCircleFirstPoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.CalcCircleLayerConfig.PointsConfig.FirstPointMarker)
    );
  ALayersList.Add(VLayer);

  // Active points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CalcCircleActivePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.CalcCircleLayerConfig.PointsConfig.ActivePointMarker)
    );
  ALayersList.Add(VLayer);

  // Captions layer
  VLayer :=
    TMapLayerCalcCircleCaptions.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CalcCircleCaptions'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.CircleOnMapEdit,
      AParams.LayersConfig.CalcCircleLayerConfig.CaptionConfig,
      AParams.ValueToStringConverter
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildPathEditLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple;
  const ATileRectForShow: ITileRectChangeable
);
var
  VLayer: IInterface;
  VGeometryChangeableByPathEdit: TGeometryLonLatChangeableByPathEdit;
begin
  {$IFDEF DISABLE_PATH_EDIT_LAYER} Exit; {$ENDIF}
  VGeometryChangeableByPathEdit :=
    TGeometryLonLatChangeableByPathEdit.Create(
      AParams.VectorGeometryLonLatFactory,
      AParams.EditLinePath
    );

  VLayer := VGeometryChangeableByPathEdit;
  ALayersList.Add(VLayer);

  // Line visualisation layer
  VLayer :=
    TMapLayerSingleLine.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PathEdit'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      ATileRectForShow,
      AParams.VectorGeometryProjectedFactory,
      AParams.LayersConfig.MarkPolyLineLayerConfig.LineConfig,
      VGeometryChangeableByPathEdit.LineChangeable
    );
  ALayersList.Add(VLayer);

  // Simple points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PathEditSimplePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.MarkPolyLineLayerConfig.PointsConfig.NormalPointMarker)
    );
  ALayersList.Add(VLayer);

  // First points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PathEditFirstPoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.MarkPolyLineLayerConfig.PointsConfig.FirstPointMarker)
    );
  ALayersList.Add(VLayer);

  // Active points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PathEditActivePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.MarkPolyLineLayerConfig.PointsConfig.ActivePointMarker)
    );
  ALayersList.Add(VLayer);

  // Captions layer
  VLayer :=
    TMapLayerCalcLineCaptions.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PathEditCaptions'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.EditLinePath,
      AParams.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig,
      AParams.ValueToStringConverter,
      AParams.GeoCalc
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildPolygonEditLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple;
  const ATileRectForShow: ITileRectChangeable
);
var
  VLayer: IInterface;
  VGeometryChangeableByPolygonEdit: TGeometryLonLatChangeableByPolygonEdit;
begin
  {$IFDEF DISABLE_POLYGON_EDIT_LAYER} Exit; {$ENDIF}
  VGeometryChangeableByPolygonEdit :=
    TGeometryLonLatChangeableByPolygonEdit.Create(
      AParams.VectorGeometryLonLatFactory,
      AParams.EditPolygon
    );

  VLayer := VGeometryChangeableByPolygonEdit;
  ALayersList.Add(VLayer);

  // Line and Fill visualisation layer
  VLayer :=
    TMapLayerSinglePolygon.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PolygonEdit'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      ATileRectForShow,
      AParams.VectorGeometryProjectedFactory,
      AParams.LayersConfig.MarkPolygonLayerConfig.LineConfig,
      VGeometryChangeableByPolygonEdit.PolygonChangeable
    );
  ALayersList.Add(VLayer);

  // Simple points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PolygonEditSimplePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.MarkPolygonLayerConfig.PointsConfig.NormalPointMarker)
    );
  ALayersList.Add(VLayer);

  // First points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PolygonEditFirstPoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.MarkPolygonLayerConfig.PointsConfig.FirstPointMarker)
    );
  ALayersList.Add(VLayer);

  // Active points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PolygonEditActivePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.MarkPolygonLayerConfig.PointsConfig.ActivePointMarker)
    );
  ALayersList.Add(VLayer);

  // Captions layer
  VLayer :=
    TMapLayerPolygonCaptions.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PolygonEditCaption'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.EditPolygon,
      AParams.LayersConfig.MarkPolygonLayerConfig.CaptionsConfig,
      AParams.ValueToStringConverter,
      AParams.GeoCalc
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildPolygonSelectionLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple;
  const ATileRectForShow: ITileRectChangeable
);
var
  VLayer: IInterface;
  VGeometryChangeableByPolygonEdit: TGeometryLonLatChangeableByPolygonEdit;
begin
  {$IFDEF DISABLE_POLYGON_SELECTION_LAYER} Exit; {$ENDIF}
  VGeometryChangeableByPolygonEdit :=
    TGeometryLonLatChangeableByPolygonEdit.Create(
      AParams.VectorGeometryLonLatFactory,
      AParams.SelectPolygon
    );

  VLayer := VGeometryChangeableByPolygonEdit;
  ALayersList.Add(VLayer);

  // Line and Fill visualisation layer
  VLayer :=
    TMapLayerSinglePolygon.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PolygonSelection'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      ATileRectForShow,
      AParams.VectorGeometryProjectedFactory,
      AParams.LayersConfig.SelectionPolygonLayerConfig.LineConfig,
      VGeometryChangeableByPolygonEdit.PolygonChangeable,
      True
    );
  ALayersList.Add(VLayer);

  // Simple points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PolygonSelectionSimplePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.SelectionPolygonLayerConfig.PointsConfig.NormalPointMarker)
    );
  ALayersList.Add(VLayer);

  // First points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PolygonSelectionFirstPoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.SelectionPolygonLayerConfig.PointsConfig.FirstPointMarker)
    );
  ALayersList.Add(VLayer);

  // Active points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PolygonSelectionActivePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPolygonEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.SelectionPolygonLayerConfig.PointsConfig.ActivePointMarker)
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildSelectionByLineLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple;
  const ATileRectForShow: ITileRectChangeable
);
var
  VLayer: IInterface;
  VPolygonChangeable: IGeometryLonLatPolygonChangeable;
  VGeometryChangeableByPathEdit: TGeometryLonLatChangeableByPathEdit;
begin
  {$IFDEF DISABLE_SELECTION_BY_LINE_LAYER} Exit; {$ENDIF}
  VGeometryChangeableByPathEdit :=
    TGeometryLonLatChangeableByPathEdit.Create(
      AParams.VectorGeometryLonLatFactory,
      AParams.SelectLinePath
    );

  VLayer := VGeometryChangeableByPathEdit;
  ALayersList.Add(VLayer);

  // Shadow visualisation layer
  VPolygonChangeable :=
    TGeometryLonLatPolygonChangeableByLineChangeable.Create(
      AParams.VectorGeometryLonLatFactory,
      AParams.ViewPortState.View,
      VGeometryChangeableByPathEdit.LineChangeable,
      AParams.LayersConfig.SelectionPolylineLayerConfig.ShadowConfig
    );
  VLayer :=
    TMapLayerSinglePolygon.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SelectionByLineShadow'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      ATileRectForShow,
      AParams.VectorGeometryProjectedFactory,
      AParams.LayersConfig.SelectionPolylineLayerConfig.ShadowConfig,
      VPolygonChangeable
    );
  ALayersList.Add(VLayer);

  // Line visualisation layer
  VLayer :=
    TMapLayerSingleLine.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SelectionByLine'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      ATileRectForShow,
      AParams.VectorGeometryProjectedFactory,
      AParams.LayersConfig.SelectionPolylineLayerConfig.LineConfig,
      VGeometryChangeableByPathEdit.LineChangeable
    );
  ALayersList.Add(VLayer);

  // Simple points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SelectionByLineSimplePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.OtherPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.SelectionPolylineLayerConfig.PointsConfig.NormalPointMarker)
    );
  ALayersList.Add(VLayer);

  // First points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SelectionByLineFirstPoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.FirstPointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.SelectionPolylineLayerConfig.PointsConfig.FirstPointMarker)
    );
  ALayersList.Add(VLayer);

  // Active points visualisation layer
  VLayer :=
    TMapLayerPointsSet.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SelectionByLineActivePoints'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.VectorGeometryProjectedFactory,
      VGeometryChangeableByPathEdit.ActivePointsChangeable,
      TMarkerDrawableChangeableSimple.Create(TMarkerDrawableSimpleSquare, AParams.LayersConfig.SelectionPolylineLayerConfig.PointsConfig.ActivePointMarker)
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildSelectionByRectLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
begin
  {$IFDEF DISABLE_SELECTION_BY_RECT_LAYER} Exit; {$ENDIF}
  VLayer :=
    TMapLayerSelectionByRect.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SelectionByRect'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.SelectionRect,
      AParams.LayersConfig.SelectionRectLayerConfig,
      True
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildGotoMarkerLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
  VBitmap: IBitmap32Static;
  VMarkerChangeable: IMarkerDrawableChangeable;
begin
  {$IFDEF DISABLE_GOTO_MARKER_LAYER} Exit; {$ENDIF}
  VBitmap :=
    ReadBitmapByFileRef(
      AParams.ResourceProvider,
      'ICONIII.png',
      AParams.ContentTypeManager,
      nil
    );
  VMarkerChangeable := nil;
  if VBitmap <> nil then begin
    VMarkerChangeable :=
      TMarkerDrawableChangeableFaked.Create(
        TMarkerDrawableByBitmap32Static.Create(VBitmap, DoublePoint(7, 6))
      );
  end;
  VLayer :=
    TMapLayerGotoMarker.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('GotoMarker'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.GUISyncronizedTimerNotifier,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      VMarkerChangeable,
      AParams.MapGoto,
      AParams.LayersConfig.GotoLayerConfig
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildGpsTrackGotoMarkerLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
  VBitmap: IBitmap32Static;
  VMarkerChangeable: IMarkerDrawableChangeable;
begin
  {$IFDEF DISABLE_GPS_TRACK_GOTO_MARKER_LAYER} Exit; {$ENDIF}
  VBitmap :=
    ReadBitmapByFileRef(
      AParams.ResourceProvider,
      'ARROW.png',
      AParams.ContentTypeManager,
      nil
    );
  VMarkerChangeable := nil;
  if VBitmap <> nil then begin
    VMarkerChangeable :=
      TMarkerDrawableChangeableFaked.Create(
        TMarkerDrawableByBitmap32Static.Create(VBitmap, DoublePoint(12, 24))
      );
  end;
  VLayer :=
    TMapLayerGpsTrackGoToMarker.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('GpsTrackGotoMarker'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      VMarkerChangeable,
      AParams.GpsTrackGoTo
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildNavigationToMarkMarkerLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
  VMarkerChangeable: IMarkerDrawableChangeable;
  VMarkerWithDirectionChangeable: IMarkerDrawableWithDirectionChangeable;
begin
  {$IFDEF DISABLE_NAVIGATION_TO_MARK_MARKER_LAYER} Exit; {$ENDIF}
  VMarkerChangeable :=
    TMarkerDrawableChangeableSimple.Create(
      TMarkerDrawableSimpleCross,
      AParams.LayersConfig.NavToPointMarkerConfig.ReachedMarkerConfig
    );
  VMarkerWithDirectionChangeable :=
    TMarkerDrawableWithDirectionChangeableSimple.Create(
      TMarkerDrawableSimpleArrow,
      AParams.LayersConfig.NavToPointMarkerConfig.ArrowMarkerConfig
    );
  VLayer :=
    TMapLayerNavToMark.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('NavToMark'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.NavToPoint,
      VMarkerWithDirectionChangeable,
      VMarkerChangeable,
      AParams.LayersConfig.NavToPointMarkerConfig
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildTileErrorInfoLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
begin
  {$IFDEF DISABLE_TILE_ERROR_INFO_LAYER} Exit; {$ENDIF}
  VLayer :=
    TMapLayerTileErrorInfo.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('TileErrorInfo'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.MainMapState.AllMapsSet,
      AParams.Bitmap32StaticFactory,
      AParams.TileErrorLogProvider,
      AParams.GUISyncronizedTimerNotifier
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildPointEditMarkerLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
  VBitmap: IBitmap32Static;
  VMarkerChangeable: IMarkerDrawableChangeable;
begin
  {$IFDEF DISABLE_POINT_EDIT_MARKER_LAYER} Exit; {$ENDIF}
  VBitmap :=
    ReadBitmapByFileRef(
      AParams.ResourceProvider,
      'ICONIII.png',
      AParams.ContentTypeManager,
      nil
    );
  VMarkerChangeable := nil;
  if VBitmap <> nil then begin
    VMarkerChangeable :=
      TMarkerDrawableChangeableFaked.Create(
        TMarkerDrawableByBitmap32Static.Create(VBitmap, DoublePoint(7, 6))
      );
  end;
  VLayer :=
    TMapLayerPointOnMapEdit.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('PointOnMapEdit'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      VMarkerChangeable,
      AParams.PointOnMapEdit
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildFullMapMouseCursorLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
begin
  {$IFDEF DISABLE_FULL_MAP_MOUSE_CURSOR_LAYER} Exit; {$ENDIF}
  VLayer :=
    TWindowLayerFullMapMouseCursor.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('FullMapMouseCursor'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.GUISyncronizedTimerNotifier,
      AParams.MouseState,
      AParams.LayersConfig.FullMapMouseCursorLayerConfig
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildCenterScaleLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
  VMarkerChangeable: IMarkerDrawableChangeable;
begin
  {$IFDEF DISABLE_CENTER_SCALE_LAYER} Exit; {$ENDIF}
  VMarkerChangeable :=
    TMarkerDrawableChangeableFaked.Create(
      TMarkerDrawableCenterScale.Create(AParams.Bitmap32StaticFactory)
    );
  VLayer :=
    TWindowLayerCenterScale.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('CenterScale'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      VMarkerChangeable,
      AParams.LayersConfig.CenterScaleConfig
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildScaleLineLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
begin
  {$IFDEF DISABLE_SCALE_LINE_LAYER} Exit; {$ENDIF}
  // Horizontal scale line layer
  VLayer :=
    TWindowLayerScaleLineHorizontal.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('ScaleLineHorizontal'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.GUISyncronizedTimerNotifier,
      AParams.ScaleLinePopupMenu,
      AParams.LayersConfig.ScaleLineConfig
    );
  ALayersList.Add(VLayer);

  // Vertical scale line layer
  VLayer :=
    TWindowLayerScaleLineVertical.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('ScaleLineVertical'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.GUISyncronizedTimerNotifier,
      AParams.ScaleLinePopupMenu,
      AParams.LayersConfig.ScaleLineConfig
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildLicenseListLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
  VLicensList: IStringListChangeable;
begin
  {$IFDEF DISABLE_LICENSE_LIST_LAYER} Exit; {$ENDIF}
  VLicensList :=
    TActiveMapsLicenseList.Create(
      AParams.LanguageManager,
      AParams.MainMapState.ActiveMapsSetLicenseNotEmpty
    );
  VLayer :=
    TWindowLayerLicenseList.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('LicenseList'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      VLicensList
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildStatusBarLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
begin
  {$IFDEF DISABLE_STATUS_BAR_LAYER} Exit; {$ENDIF}
  VLayer :=
    TWindowLayerStatusBar.Create(
      AParams.LanguageManager,
      AParams.PerfListGroup.CreateAndAddNewSubList('StatusBar'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.LayersConfig.StatBar,
      AParams.CoordToStringConverter,
      AParams.ValueToStringConverter,
      AParams.MouseState,
      AParams.GUISyncronizedTimerNotifier,
      AParams.TerrainProviderList,
      AParams.GlobalConfig.TerrainConfig,
      AParams.DownloadInfo,
      AParams.GlobalInternetState,
      AParams.StatBarPopupMenu,
      AParams.MainMapState.ActiveMap
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildSunCalcLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple
);
var
  VLayer: IInterface;
begin
  {$IFDEF DISABLE_SUN_CALC_LAYER} Exit; {$ENDIF}
  VLayer :=
    TWindowLayerSunCalcYearInfo.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SunCalc/YearInfo'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.SunCalcConfig,
      AParams.SunCalcProvider
    );
  ALayersList.Add(VLayer);

  VLayer :=
    TWindowLayerSunCalcDayInfo.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SunCalc/DayInfo'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.SunCalcConfig,
      AParams.SunCalcProvider
    );
  ALayersList.Add(VLayer);

  VLayer :=
    TWindowLayerSunCalcTimeInfo.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SunCalc/TimeInfo'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.MainFormState,
      AParams.SunCalcConfig,
      AParams.SunCalcProvider,
      AParams.GUISyncronizedTimerNotifier
    );
  ALayersList.Add(VLayer);

  VLayer :=
    TWindowLayerSunCalcYearTimeLine.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SunCalc/YearTimeLine'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.MouseState,
      AParams.GUISyncronizedTimerNotifier,
      AParams.ViewPortState.View,
      AParams.SunCalcConfig,
      AParams.SunCalcProvider,
      AParams.SunCalcPopupMenu
    );
  ALayersList.Add(VLayer);

  VLayer :=
    TWindowLayerSunCalcDayTimeLine.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SunCalc/DayTimeLine'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.MouseState,
      AParams.GUISyncronizedTimerNotifier,
      AParams.ViewPortState.View,
      AParams.SunCalcConfig,
      AParams.SunCalcProvider,
      AParams.SunCalcPopupMenu
    );
  ALayersList.Add(VLayer);

  VLayer :=
    TWindowLayerSunCalcDetailsPanel.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('SunCalc/DetailsPanel'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState.View,
      AParams.SunCalcConfig,
      AParams.SunCalcProvider,
      AParams.SunCalcPopupMenu
    );
  ALayersList.Add(VLayer);
end;

procedure TMainFormLayersList.BuildMiniMapLayer(
  const AParams: TMainFormLayersListParams;
  const ALayersList: IInterfaceListSimple;
  const ATileMatrixDraftResampler: IImageResamplerFactoryChangeable
);
const
  CDebugName = 'MiniMap';
var
  VLayer: IInterface;
  VPerfList: IInternalPerformanceCounterList;
  VMiniMapConverterChangeable: ILocalCoordConverterChangeable;
  VTileRectForShow: ITileRectChangeable;
  VProvider: IBitmapLayerProviderChangeable;
  VTileMatrix: IBitmapTileMatrixChangeable;
  VBitmap: IBitmap32Static;
  VBitmapChangeable: IBitmapChangeable;
begin
  {$IFDEF DISABLE_MINI_MAP_LAYER} Exit; {$ENDIF}
  VMiniMapConverterChangeable :=
    TLocalConverterChangeableOfMiniMap.Create(
      AParams.PerfListGroup.CreateAndAddNewCounter('MiniMapConverter'),
      AParams.LocalConverterFactory,
      AParams.ActiveProjectionSet,
      AParams.ViewPortState.View,
      AParams.LayersConfig.MiniMapLayerConfig.LocationConfig
    );
  VTileRectForShow :=
    TTileRectChangeableByLocalConverterSmart.Create(
      AParams.ActiveProjectionSet,
      VMiniMapConverterChangeable,
      GSync.SyncVariable.Make('TileRectMiniMapForShowMain'),
      GSync.SyncVariable.Make('TileRectMiniMapForShowResult')
    );

  // Bitmap layer
  VPerfList := AParams.PerfListGroup.CreateAndAddNewSubList(CDebugName);
  VProvider :=
    TBitmapLayerProviderChangeableForMainLayer.Create(
      AParams.MainMapState.MiniMapActiveMap,
      AParams.MainMapState.MiniMapActiveBitmapLayersList,
      AParams.BitmapPostProcessing,
      AParams.LayersConfig.MiniMapLayerConfig.UseTilePrevZoomConfig,
      AParams.Bitmap32StaticFactory,
      AParams.TileErrorLogger
    );
  VTileMatrix :=
    TBitmapTileMatrixChangeableWithThread.Create(
      VPerfList,
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      VTileRectForShow,
      ATileMatrixDraftResampler,
      True,
      AParams.Bitmap32StaticFactory,
      AParams.HashFunction,
      VProvider,
      nil,
      AParams.LayersConfig.MiniMapLayerConfig.ThreadConfig,
      CDebugName
    );
  VLayer :=
    TTiledMapLayer.Create(
      VPerfList,
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.HashFunction,
      VMiniMapConverterChangeable,
      VTileMatrix,
      AParams.GUISyncronizedTimerNotifier,
      CDebugName
    );
  ALayersList.Add(VLayer);

  // View rect
  VLayer :=
    TMiniMapLayerViewRect.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('MiniMapViewRect'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      AParams.ViewPortState,
      VMiniMapConverterChangeable,
      AParams.GUISyncronizedTimerNotifier,
      AParams.MiniMapPopupMenu,
      AParams.LayersConfig.MiniMapLayerConfig.LocationConfig
    );
  ALayersList.Add(VLayer);

  // Top border
  VLayer :=
    TMiniMapLayerTopBorder.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('MiniMapTopBorder'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      VMiniMapConverterChangeable,
      AParams.LayersConfig.MiniMapLayerConfig
    );
  ALayersList.Add(VLayer);

  // Left border
  VLayer :=
    TMiniMapLayerLeftBorder.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('MiniMapLeftBorder'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      VMiniMapConverterChangeable,
      AParams.LayersConfig.MiniMapLayerConfig
    );
  ALayersList.Add(VLayer);

  // Minus button
  VBitmap :=
    ReadBitmapByFileRef(
      AParams.ResourceProvider,
      'ICONII.png',
      AParams.ContentTypeManager,
      nil
    );
  VBitmapChangeable := nil;
  if VBitmap <> nil then begin
    VBitmapChangeable := TBitmapChangeableFaked.Create(VBitmap);
  end;
  VLayer :=
    TMiniMapLayerMinusButton.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('MiniMapMinusButton'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      VMiniMapConverterChangeable,
      VBitmapChangeable,
      AParams.LayersConfig.MiniMapLayerConfig
    );
  ALayersList.Add(VLayer);

  // Plus button
  VBitmap :=
    ReadBitmapByFileRef(
      AParams.ResourceProvider,
      'ICONI.png',
      AParams.ContentTypeManager,
      nil
    );
  VBitmapChangeable := nil;
  if VBitmap <> nil then begin
    VBitmapChangeable := TBitmapChangeableFaked.Create(VBitmap);
  end;
  VLayer :=
    TMiniMapLayerPlusButton.Create(
      AParams.PerfListGroup.CreateAndAddNewSubList('MiniMapPlusButton'),
      AParams.AppStartedNotifier,
      AParams.AppClosingNotifier,
      AParams.ParentMap,
      VMiniMapConverterChangeable,
      VBitmapChangeable,
      AParams.LayersConfig.MiniMapLayerConfig
    );
  ALayersList.Add(VLayer);
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

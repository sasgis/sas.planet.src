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

unit t_MainFormLayersListParams;

interface

uses
  GR32_Image,
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
  i_ValueToStringConverter;

type
  TMainFormLayersListParams = record
    ParentMap: TImage32;
    AppStartedNotifier: INotifierOneOperation;
    AppClosingNotifier: INotifierOneOperation;
    ResourceProvider: IConfigDataProvider;
    LanguageManager: ILanguageManager;
    GlobalConfig: IGlobalConfig;
    ContentTypeManager: IContentTypeManager;
    GUISyncronizedTimerNotifier: INotifierTime;
    LastSearchResult: ILastSearchResult;
    LocalConverterFactory: ILocalCoordConverterFactorySimpe;
    VectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    VectorGeometryLonLatFactory: IGeometryLonLatFactory;
    VectorGeometryProjectedFactory: IGeometryProjectedFactory;
    ProjectedGeometryProvider: IGeometryProjectedProvider;
    CoordToStringConverter: ICoordToStringConverterChangeable;
    ValueToStringConverter: IValueToStringConverterChangeable;
    HashFunction: IHashFunction;
    Bitmap32StaticFactory: IBitmap32StaticFactory;
    ImageResamplerFactoryList: IImageResamplerFactoryList;
    BitmapPostProcessing: IBitmapPostProcessingChangeable;
    GpsTrackRecorder: IGpsTrackRecorder;
    GPSRecorder: IGPSRecorder;
    TerrainProviderList: ITerrainProviderList;
    LastSelectionInfo: ILastSelectionInfo;
    DownloadInfo: IDownloadInfoSimple;
    GlobalInternetState: IGlobalInternetState;
    MarkSystem: IMarkSystem;
    LayersConfig: IMainFormLayersConfig;
    ActiveProjectionSet: IProjectionSetChangeable;
    ViewPortState: IViewPortState;
    MainFormState: IMainFormState;
    MouseState: IMouseState;
    MainMapState: IMainMapsState;
    FillingMapPolygon: IFillingMapPolygon;
    MergePolygonsResult: IMergePolygonsResult;
    CalcLinePath: IPathOnMapEdit;
    CircleOnMapEdit: ICircleOnMapEdit;
    EditLinePath: IPathOnMapEdit;
    EditPolygon: IPolygonOnMapEdit;
    SelectPolygon: IPolygonOnMapEdit;
    SelectLinePath: IPathOnMapEdit;
    SelectionRect: ISelectionRect;
    MapGoto: IMapViewGoto;
    GpsTrackGoTo: IMapViewGoto;
    NavToPoint: INavigationToPoint;
    PointOnMapEdit: IPointOnMapEdit;
    TileErrorLogProvider: ITileErrorLogProviedrStuped;
    TileErrorLogger: ITileErrorLogger;
    PerfListGroup: IInternalPerformanceCounterList;
    SunCalcConfig: ISunCalcConfig;
    SunCalcProvider: ISunCalcProvider;
    SunCalcPopupMenu: IPopUp;
    StatBarPopupMenu: IPopUp;
    ScaleLinePopupMenu: IPopUp;
    MiniMapPopupMenu: IPopUp;
  end;

function MainFormLayersListParams(
  const AParentMap: TImage32;
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
): TMainFormLayersListParams; inline;

implementation

function MainFormLayersListParams(
  const AParentMap: TImage32;
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
): TMainFormLayersListParams;
begin
  with Result do begin
    ParentMap := AParentMap;
    AppStartedNotifier := AAppStartedNotifier;
    AppClosingNotifier := AAppClosingNotifier;
    ResourceProvider := AResourceProvider;
    LanguageManager := ALanguageManager;
    GlobalConfig := AGlobalConfig;
    ContentTypeManager := AContentTypeManager;
    GUISyncronizedTimerNotifier := AGUISyncronizedTimerNotifier;
    LastSearchResult := ALastSearchResult;
    LocalConverterFactory := ALocalConverterFactory;
    VectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
    VectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
    VectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
    ProjectedGeometryProvider := AProjectedGeometryProvider;
    CoordToStringConverter := ACoordToStringConverter;
    ValueToStringConverter := AValueToStringConverter;
    HashFunction := AHashFunction;
    Bitmap32StaticFactory := ABitmap32StaticFactory;
    ImageResamplerFactoryList := AImageResamplerFactoryList;
    BitmapPostProcessing := ABitmapPostProcessing;
    GpsTrackRecorder := AGpsTrackRecorder;
    GPSRecorder := AGPSRecorder;
    TerrainProviderList := ATerrainProviderList;
    LastSelectionInfo := ALastSelectionInfo;
    DownloadInfo := ADownloadInfo;
    GlobalInternetState := AGlobalInternetState;
    MarkSystem := AMarkSystem;
    LayersConfig := ALayersConfig;
    ActiveProjectionSet := AActiveProjectionSet;
    ViewPortState := AViewPortState;
    MainFormState := AMainFormState;
    MouseState := AMouseState;
    MainMapState := AMainMapState;
    FillingMapPolygon := AFillingMapPolygon;
    MergePolygonsResult := AMergePolygonsResult;
    CalcLinePath := ACalcLinePath;
    CircleOnMapEdit := ACircleOnMapEdit;
    EditLinePath := AEditLinePath;
    EditPolygon := AEditPolygon;
    SelectPolygon := ASelectPolygon;
    SelectLinePath := ASelectLinePath;
    SelectionRect := ASelectionRect;
    MapGoto := AMapGoto;
    GpsTrackGoTo := AGpsTrackGoTo;
    NavToPoint := ANavToPoint;
    PointOnMapEdit := APointOnMapEdit;
    TileErrorLogProvider := ATileErrorLogProvider;
    TileErrorLogger := ATileErrorLogger;
    PerfListGroup := APerfListGroup;
    SunCalcConfig := ASunCalcConfig;
    SunCalcProvider := ASunCalcProvider;
    SunCalcPopupMenu := ASunCalcPopupMenu;
    ScaleLinePopupMenu := AScaleLinePopupMenu;
    StatBarPopupMenu := AStatBarPopupMenu;
    MiniMapPopupMenu := AMiniMapPopupMenu;
  end;
end;

end.

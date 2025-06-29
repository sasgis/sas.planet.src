{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit frm_Main;

interface

uses
  Windows,
  Types,
  Messages,
  SysUtils,
  Forms,
  ShellApi,
  Classes,
  Menus,
  ActiveX,
  ShlObj,
  ComObj,
  Graphics,
  StdCtrls,
  Controls,
  ActnList,
  Actions,
  ImageList,
  ExtCtrls,
  ExtActns,
  Dialogs,
  Spin,
  ImgList,
  ComCtrls,
  UITypes,
  GR32,
  GR32_Layers,
  GR32_Image,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  TB2ExtItems,
  TB2ToolWindow,
  TBXToolPals,
  TBX,
  TBXDkPanels,
  TBXExtItems,
  TBXGraphics,
  t_GeoTypes,
  i_GeoCalc,
  i_GUIDSet,
  i_Listener,
  i_Notifier,
  i_ListenerNotifierLinksList,
  i_ConfigDataWriteProvider,
  i_TileError,
  i_TileErrorLogProviedrStuped,
  i_MapTypeConfigModalEdit,
  i_MapTypeHotKeyListStatic,
  i_VectorDataItemSimple,
  i_MainFormConfig,
  i_MainMapsState,
  i_ProjectionSetChangeable,
  i_ViewPortState,
  i_StickToGrid,
  i_SensorList,
  i_ElevationMetaWriter,
  i_ElevationProfilePresenter,
  i_SearchResultPresenter,
  i_MergePolygonsResult,
  i_MergePolygonsPresenter,
  i_MainWindowPosition,
  i_SelectionRect,
  i_RegionProcess,
  i_LineOnMapEdit,
  i_PointOnMapEdit,
  i_MarkOnMapEditProvider,
  i_MapTypeIconsList,
  i_MessageHandler,
  i_MouseState,
  i_MainFormState,
  i_GeometryLonLat,
  i_LocalCoordConverter,
  i_MouseHandler,
  i_Timer,
  i_TreeChangeable,
  i_StringListStatic,
  i_MapViewGoto,
  i_StaticTreeItem,
  i_MenuGeneratorByTree,
  i_FindVectorItems,
  i_PlayerPlugin,
  i_VectorItemSubset,
  i_ImportConfig,
  i_PanelsPositionsSaveLoad,
  i_FillingMapPolygon,
  i_FavoriteMapSetHelper,
  i_FavoriteMapSetHotKeyList,
  i_UrlByCoordProvider,
  i_SunCalcConfig,
  i_SunCalcProvider,
  i_CmdLineArgProcessor,
  u_CmdLineArgProcessorAPI,
  u_MapHintWindow,
  u_ShortcutManager,
  u_MarksDbMenu,
  u_MarkDbGUIHelper,
  u_TileStorageImporter,
  u_FavoriteMapSetMenu,
  u_MainFormLayersList,
  u_SearchToolbarContainer,
  u_TBXSASTheme,
  u_TBXExtItems,
  u_SpinEditExt,
  frm_About,
  frm_Settings,
  frm_MapLayersOptions,
  frm_RegionProcess,
  frm_MarksExplorer,
  frm_CacheManager,
  frm_GoTo,
  frm_PointProjecting,
  frm_UpdateChecker,
  frm_PascalScriptIDE,
  frm_MarkPictureConfig,
  frm_FavoriteMapSetEditor,
  frm_FavoriteMapSetManager,
  u_CommonFormAndFrameParents;

type
  TfrmMain = class(TCommonFormParent)
    map: TImage32;
    dlgOpenHlgLoad: TOpenDialog;
    SaveLink: TSaveDialog;
    TBDock: TTBXDock;
    TBMainToolBar: TTBXToolbar;
    TBDockBottom: TTBXDock;
    TBDockLeft: TTBXDock;
    SrcToolbar: TTBXToolbar;
    TBMarksToolbar: TTBXToolbar;
    GPSToolbar: TTBXToolbar;
    TBExit: TTBXToolbar;
    ZoomToolBar: TTBXToolbar;
    TBControlItem2: TTBControlItem;
    labZoom: TLabel;
    TBDockRight: TTBXDock;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    TBXSeparatorItem3: TTBXSeparatorItem;
    TBXMainMenu: TTBXToolbar;
    NSMB: TTBXSubmenuItem;
    NLayerSel: TTBXSubmenuItem;
    NOperations: TTBXSubmenuItem;
    NView: TTBXSubmenuItem;
    NSources: TTBXSubmenuItem;
    NMarks: TTBXSubmenuItem;
    tbsbmGPS: TTBXSubmenuItem;
    NParams: TTBXSubmenuItem;
    NLayerParams: TTBXSubmenuItem;
    tbsbmHelp: TTBXSubmenuItem;
    NSRCic: TTBXItem;
    NSRCinet: TTBXItem;
    NSRCesh: TTBXItem;
    TBGPSconn: TTBXItem;
    TBGPSPath: TTBXSubmenuItem;
    TBSrc: TTBXSubmenuItem;
    TBSMB: TTBXSubmenuItem;
    TBLayerSel: TTBXSubmenuItem;
    TBFullSize: TTBXItem;
    TBmove: TTBXItem;
    TBCalcRas: TTBXItem;
    TBRectSave: TTBXSubmenuItem;
    TBMapZap: TTBXSubmenuItem;
    TBGoTo: TTBXSubmenuItem;
    TBZoomIn: TTBXItem;
    TBZoom_out: TTBXItem;
    tbitmCreateShortcut: TTBXItem;
    NZoomIn: TTBXItem;
    NZoomOut: TTBXItem;
    tbitmGoToModal: TTBXItem;
    NCalcRast: TTBXItem;
    tbitmCacheManager: TTBXItem;
    tbitmQuit: TTBXItem;
    tbitmGPSTrackSaveToMarks: TTBXItem;
    TBItemDelTrack: TTBXItem;
    NFoolSize: TTBXItem;
    NGoToCur: TTBXItem;
    Nbackload: TTBXItem;
    NbackloadLayer: TTBXItem;
    Nanimate: TTBXItem;
    tbitmGauge: TTBXItem;
    Ninvertcolor: TTBXItem;
    NPanels: TTBXSubmenuItem;
    tbsbmInterface: TTBXSubmenuItem;
    NFillMap: TTBXSubmenuItem;
    TBFillingTypeMap: TTBXSubmenuItem;
    tbtpltCachedTilesMap: TTBXToolPalette;
    NShowGran: TTBXSubmenuItem;
    tbsbmGenShtabScale: TTBXSubmenuItem;
    NGShScale0: TTBXItem;
    NGShScale1000000: TTBXItem;
    NGShScale500000: TTBXItem;
    NGShScale200000: TTBXItem;
    NGShScale100000: TTBXItem;
    NGShScale50000: TTBXItem;
    NGShScale25000: TTBXItem;
    NGShScale10000: TTBXItem;
    tbitmOnlineHelp: TTBXItem;
    tbitmAbout: TTBXItem;
    tbitmOnlineHome: TTBXItem;
    tbitmOnlineForum: TTBXItem;
    NMapParams: TTBXItem;
    tbitmOptions: TTBXItem;
    tbitmInterfaceOptions: TTBXItem;
    TBLang: TTBXSubmenuItem;
    tbitmGPSConnect: TTBXItem;
    tbitmGPSTrackShow: TTBXItem;
    tbitmGPSCenterMap: TTBXItem;
    tbitmSaveCurrentPosition: TTBXItem;
    tbitmGPSTrackSaveToDb: TTBXItem;
    tbitmGPSTrackClear: TTBXItem;
    Showstatus: TTBXItem;
    ShowMiniMap: TTBXItem;
    ShowLine: TTBXItem;
    TBXExit: TTBXItem;
    TBXSeparatorItem4: TTBXSeparatorItem;
    TBXSeparatorItem5: TTBXSeparatorItem;
    TBXSeparatorItem6: TTBXSeparatorItem;
    TBXSeparatorItem7: TTBXSeparatorItem;
    TBXSeparatorItem8: TTBXSeparatorItem;
    NRectSave: TTBXSubmenuItem;
    TBXSeparatorItem9: TTBXSeparatorItem;
    TBXSeparatorItem10: TTBXSeparatorItem;
    TBXSeparatorItem11: TTBXSeparatorItem;
    tbsprtGPS1: TTBXSeparatorItem;
    TBXSeparatorItem14: TTBXSeparatorItem;
    tbxSep3: TTBXSeparatorItem;
    TBXSensorsBar: TTBXToolWindow;
    ScrollBox1: TScrollBox;
    TBXDock1: TTBXDock;
    NSensors: TTBXSubmenuItem;
    TBXPopupMenuSensors: TTBXPopupMenu;
    tbitmSaveCurrentPositionToolbar: TTBXItem;
    TBXSeparatorItem16: TTBXSeparatorItem;
    TBXSeparatorItem17: TTBXSeparatorItem;
    TBXToolBarSearch: TTBXToolbar;
    TBXSelectSrchType: TTBXSubmenuItem;
    tbsprtGPS2: TTBXSeparatorItem;
    tbitmOpenFile: TTBXItem;
    NShowSelection: TTBXItem;
    TBRECT: TTBXItem;
    TBREGION: TTBXItem;
    TBCOORD: TTBXItem;
    TBPrevious: TTBXItem;
    TBLoadSelFromFile: TTBXItem;
    TBGPSToPoint: TTBXSubmenuItem;
    TBGPSToPointCenter: TTBXItem;
    tbitmGPSToPointCenter: TTBXItem;
    tbtmHelpBugTrack: TTBXItem;
    tbitmShowDebugInfo: TTBXItem;
    PanelsImageList: TTBXImageList;
    ZSlider: TImage32;
    TBControlItem1: TTBControlItem;
    TBXPopupPanels: TTBXPopupMenu;
    MenusImageList: TTBXImageList;
    ScalesImageList: TTBXImageList;
    MainPopupMenu: TTBXPopupMenu;
    tbitmMarkEditPosition: TTBXItem;
    NMarkDel: TTBXItem;
    NMarkOper: TTBXItem;
    NMarkNav: TTBXItem;
    NMarkExport: TTBXItem;
    tbsprtMainPopUp0: TTBXSeparatorItem;
    NaddPoint: TTBXItem;
    tbsprtMainPopUp1: TTBXSeparatorItem;
    tbitmCenterWithZoom: TTBXSubmenuItem;
    tbsprtMainPopUp2: TTBXSeparatorItem;
    tbitmCopyToClipboard: TTBXSubmenuItem;
    tbxCopyUrlToGoogleMaps: TTBXItem;
    tbxCopyUrlToYandexMaps: TTBXItem;
    tbxCopyUrlToKosmosnimkiRu: TTBXItem;
    tbxCopyUrlToBingMaps: TTBXItem;
    tbsprtCopyToClipboard0: TTBXSeparatorItem;
    tbitmCopyToClipboardMainMapUrl: TTBXItem;
    tbitmCopyToClipboardCoordinates: TTBXItem;
    tbitmCopyToClipboardMainMapTile: TTBXItem;
    tbitmCopyToClipboardMainMapTileFileName: TTBXItem;
    Nopendir: TTBXItem;
    tbitmOpenFolderMainMapTile: TTBXItem;
    tbsprtMainPopUp3: TTBXSeparatorItem;
    tbitmAdditionalOperations: TTBXSubmenuItem;
    NGTOPO30: TTBXItem;
    NSRTM3: TTBXItem;
    tbsprtAdditionalOperations0: TTBXSeparatorItem;
    tbsprtMainPopUp4: TTBXSeparatorItem;
    tbitmDownloadMainMapTile: TTBXItem;
    NDel: TTBXItem;
    tbsprtMainPopUp5: TTBXSeparatorItem;
    NMapInfo: TTBXItem;
    ldm: TTBXSubmenuItem;
    dlm: TTBXSubmenuItem;
    tbtpltCenterWithZoom: TTBXToolPalette;
    tbtpltViewGridTile: TTBXToolPalette;
    TBOpenDirLayer: TTBXSubmenuItem;
    TBCopyLinkLayer: TTBXSubmenuItem;
    TBLayerInfo: TTBXSubmenuItem;
    TBScreenSelect: TTBXItem;
    NMainToolBarShow: TTBXVisibilityToggleItem;
    NZoomToolBarShow: TTBXVisibilityToggleItem;
    NsrcToolBarShow: TTBXVisibilityToggleItem;
    NGPSToolBarShow: TTBXVisibilityToggleItem;
    TBXVisibilityToggleItem1: TTBXVisibilityToggleItem;
    TBXVisibilityToggleItem2: TTBXVisibilityToggleItem;
    TBXSeparatorItem13: TTBXSeparatorItem;
    TBXSeparatorItem18: TTBXSeparatorItem;
    NBlock_toolbars: TTBXItem;
    TBXSeparatorItem19: TTBXSeparatorItem;
    tbitmGPSOptions: TTBXItem;
    TrayIcon: TTrayIcon;
    TrayPopupMenu: TTBXPopupMenu;
    TrayItemRestore: TTBItem;
    TBSeparatorItem1: TTBSeparatorItem;
    TrayItemQuit: TTBItem;
    NAnimateMove: TTBXItem;
    tbiSearch: u_TBXExtItems.TTBXComboBoxItem;
    NSearchResults: TTBXVisibilityToggleItem;
    TBSearchWindow: TTBXDockablePanel;
    PanelSearch: TPanel;
    TBXDockForSearch: TTBXDock;
    ScrollBoxSearchWindow: TScrollBox;
    TBPolylineSelect: TTBXItem;
    TBEditPath: TTBXToolbar;
    TBEditPathDel: TTBXItem;
    TBEditMagnetDraw: TTBXItem;
    TBEditSelectPolylineRadiusCap1: TTBXLabelItem;
    TBControlItem4: TTBControlItem;
    TBEditSelectPolylineRadiusCap2: TTBXLabelItem;
    TBEditPathMarsh: TTBXSubmenuItem;
    TBEditPathOk: TTBXItem;
    TBEditSelectPolylineRadius: TSpinEdit;
    tbitmShowMarkCaption: TTBXItem;
    NMarksGroup: TTBGroupItem;
    TBAdd_Point: TTBXItem;
    TBAdd_Line: TTBXItem;
    TBAdd_Poly: TTBXItem;
    TBXSeparatorItem12: TTBXSeparatorItem;
    tbitmPlacemarkManager: TTBXItem;
    TBHideMarks: TTBXItem;
    tbxCopyUrlToOpenStreetMap: TTBXItem;
    TBXSeparatorItem20: TTBXSeparatorItem;
    NFillMode3: TTBXItem;
    NFillMode2: TTBXItem;
    NFillMode1: TTBXItem;
    TBXSeparatorItem21: TTBXSeparatorItem;
    NShowFillDates: TTBXItem;
    FillDates: TTBXToolbar;
    TBControlItem7: TTBControlItem;
    TBControlItem6: TTBControlItem;
    TBControlItem8: TTBControlItem;
    TBControlItem9: TTBControlItem;
    Label1: TLabel;
    Label2: TLabel;
    DateTimePicker1: TDateTimePicker;
    DateTimePicker2: TDateTimePicker;
    TBXVisibilityToggleItem3: TTBXVisibilityToggleItem;
    DegreedLinesSubMenu: TTBXSubmenuItem;
    NDegScale10000: TTBXItem;
    NDegScale25000: TTBXItem;
    NDegScale50000: TTBXItem;
    NDegScale100000: TTBXItem;
    NDegScale200000: TTBXItem;
    NDegScale500000: TTBXItem;
    NDegScale1000000: TTBXItem;
    NDegScale0: TTBXItem;
    TBXSeparatorItem22: TTBXSeparatorItem;
    NDegScaleUser: TTBXItem;
    NDegValue: TTBXEditItem;
    TBSeparatorItem2: TTBSeparatorItem;
    NDegScaleAuto: TTBXItem;
    tbpmiVersions: TTBXSubmenuItem;
    tbpmiClearVersion: TTBXItem;
    tbitmNavigationArrow: TTBXItem;
    tbitmMarkEditProperties: TTBXItem;
    tbitmFitMarkToScreen: TTBXItem;
    tbitmEditLastSelection: TTBXItem;
    tbitmHideThisMark: TTBXItem;
    tbitmSaveMark: TTBXSubmenuItem;
    tbitmSaveMarkAsNew: TTBXItem;
    tbxpmnSearchResult: TTBXPopupMenu;
    tbitmCopySearchResultCoordinates: TTBXItem;
    tbitmCopySearchResultDescription: TTBXItem;
    tbitmCreatePlaceMarkBySearchResult: TTBXItem;
    tbitmFitEditToScreen: TTBXItem;
    NMarkPlay: TTBXItem;
    tbitmMarkInfo: TTBXItem;
    tbitmCopyToClipboardGenshtabName: TTBXItem;
    tbxWeatherUnderground: TTBXItem;
    NMapStorageInfo: TTBXItem;
    tbitmMakeVersionByMark: TTBXItem;
    tbitmSelectVersionByMark: TTBXItem;
    TBSeparatorItem3: TTBSeparatorItem;
    NGShauto: TTBXItem;
    NGShScale5000: TTBXItem;
    NGShScale2500: TTBXItem;
    tbpmiShowOtherVersions: TTBXItem;
    tbxsbmProjection: TTBXSubmenuItem;
    tbxSep1: TTBXSeparatorItem;
    tbitmCheckUpdate: TTBXItem;
    btnHideAll: TTBXItem;
    HideSeparator: TTBSeparatorItem;
    tbitmFillingMapAsMain: TTBXItem;
    tbxEditPathLabelVisible: TTBSubmenuItem;
    tbxEditPathShowIntermediateDist: TTBXItem;
    tbxEditPathShowDistIncrement: TTBXItem;
    tbxEditPathShowAzimuth: TTBXItem;
    tbitmPointProject: TTBXItem;
    TBXNextVer: TTBXItem;
    TBXPrevVer: TTBXItem;
    TBXSubmnMapVer: TTBXSubmenuItem;
    TBXSubmenuMap: TTBXSubmenuItem;
    tbxnxtmap: TTBXItem;
    tbxprevmap: TTBXItem;
    tbxtmPascalScriptIDE: TTBXItem;
    tbxSep2: TTBXSeparatorItem;
    tbMergePolygons: TTBXDockablePanel;
    mmoMergePolyHint: TMemo;
    tbxMergePolygonsShow: TTBXVisibilityToggleItem;
    tbxMergePolygonsShow1: TTBXVisibilityToggleItem;
    tbxtmAddToMergePolygons: TTBXItem;
    tbxFillingMap: TTBXSubmenuItem;
    tbxFavorite: TTBXSubmenuItem;
    TBXFavoriteToolbar: TTBXToolbar;
    TBFavorite: TTBXSubmenuItem;
    tbxAddToFavorite: TTBXItem;
    TBXSeparatorItem15: TTBXSeparatorItem;
    NFavoriteToolbarShow: TTBXVisibilityToggleItem;
    tbxManageFavorite: TTBXItem;
    TBEditPathSplit: TTBXItem;
    tbxtmSaveMarkAsSeparateSegment: TTBXItem;
    tbiFillingMapMaps: TTBGroupItem;
    tbiLayersList: TTBGroupItem;
    actlstMain: TActionList;
    actSelectByPolygon: TAction;
    actSelectByRect: TAction;
    actSelectByLine: TAction;
    actSelectByCoordinates: TAction;
    actSelectByVisibleArea: TAction;
    actMakeLinkOnDesktop: TAction;
    actFileOpen: TAction;
    actZoomIn: TAction;
    actZoomOut: TAction;
    actShowGoTo: TAction;
    actSelectByLastSelection: TAction;
    actSelectByLastSelectionEdit: TAction;
    actSelectBySelectionFromFile: TAction;
    actShowCacheManager: TAction;
    actQuit: TAction;
    actDistanceCalculation: TAction;
    actMoveMap: TAction;
    actViewFullScreen: TAction;
    actConfigZoomToCursor: TAction;
    actConfigUsePrevForMap: TAction;
    actConfigUsePrevForLayers: TAction;
    actConfigUseZoomAnimation: TAction;
    actConfigUseInertialMovement: TAction;
    actConfigAzimuthCircle: TAction;
    actConfigColorInversion: TAction;
    actConfigPreviousSelectionVisible: TAction;
    actViewNavigation: TAction;
    actShowDebugInfo: TAction;
    actConfigStatusBarVisible: TAction;
    actConfigMiniMapVisible: TAction;
    actConfigScaleLineVisible: TAction;
    actViewToolbarsLock: TAction;
    actViewGridGenShtabNo: TAction;
    actViewGridGenShtab_1_000_000: TAction;
    actViewGridGenShtab_500_000: TAction;
    actViewGridGenShtab_200_000: TAction;
    actViewGridGenShtab_100_000: TAction;
    actViewGridGenShtab_50_000: TAction;
    actViewGridGenShtab_25_000: TAction;
    actViewGridGenShtab_10_000: TAction;
    actViewGridGenShtab_5_000: TAction;
    actViewGridGenShtab_2_500: TAction;
    actViewGridGenShtabAuto: TAction;
    actViewGridLonLatNo: TAction;
    actViewGridLonLat_10_000: TAction;
    actViewGridLonLat_05_000: TAction;
    actViewGridLonLat_02_000: TAction;
    actViewGridLonLat_01_000: TAction;
    actViewGridLonLat_00_500: TAction;
    actViewGridLonLat_00_250: TAction;
    actViewGridLonLat_00_125: TAction;
    actViewGridLonLat_User: TAction;
    actViewGridLonLatAuto: TAction;
    actHelpOpenOnline: TBrowseURL;
    actHelpShowAbout: TAction;
    actHelpOpenWebSite: TBrowseURL;
    actHelpOpenIssueTracker: TBrowseURL;
    actHelpOpenCommunity: TBrowseURL;
    actShowPascalScriptIde: TAction;
    actShowUpddateChecker: TAction;
    actViewFillingMapMarkUnexisting: TAction;
    actViewFillingMapMarkExisting: TAction;
    actViewFillingMapMarkGradient: TAction;
    actViewFillingMapFilterMode: TAction;
    actViewSelectNextMapWithTile: TAction;
    actViewSelectPrevMapWithTile: TAction;
    actViewSelectNextMapVersion: TAction;
    actViewSelectPrevMapVersion: TAction;
    actConfigMarksNamesVisible: TAction;
    actShowPointProject: TAction;
    actMarksAddPoint: TAction;
    actMarksAddLine: TAction;
    actMarksAddPolygon: TAction;
    actShowPlacemarkManager: TAction;
    actConfigMarksHide: TAction;
    actConfigDownloadModeCache: TAction;
    actConfigDownloadModeInternet: TAction;
    actConfigDownloadModeCacheInternet: TAction;
    actMapsEditMapParams: TAction;
    actConfigOptionsShow: TAction;
    actGpsConnect: TAction;
    actConfigGpsShowTrack: TAction;
    actConfigGpsFollowPosition: TAction;
    actConfigGpsFollowPositionAtCenter: TAction;
    actGpsMarkPointAdd: TAction;
    actGpsTrackSaveToDb: TAction;
    actGpsTrackClear: TAction;
    actConfigGpsOptionsShow: TAction;
    tbiProjectionOfMap: TTBXItem;
    actConfigProjectionOfMapUse: TAction;
    tbiProjections: TTBGroupItem;
    actViewFillingMapMainMapUse: TAction;
    actConfigInterfaceOptionsShow: TAction;
    actMapsAllLayersHide: TAction;
    actFavoriteAdd: TAction;
    actFavoriteManage: TAction;
    tbiFavoriteItems: TTBGroupItem;
    actViewNotMinimized: TAction;
    actConfigScaleLineExtended: TAction;
    actConfigScaleLineOptionsShow: TAction;
    actConfigScaleLineNumberFormatNice: TAction;
    actConfigScaleLineNumberFormatRound: TAction;
    actConfigScaleLineNumberFormatScience: TAction;
    tbxpmnScaleLine: TTBXPopupMenu;
    tbiConfigScaleLineExtended: TTBXItem;
    tbxsbmScaleLineNumberFormat: TTBXSubmenuItem;
    tbiConfigScaleLineNumberFormatNice: TTBXItem;
    tbiConfigScaleLineNumberFormatRound: TTBXItem;
    tbiConfigScaleLineNumberFormatScience: TTBXItem;
    tbiConfigScaleLineVisible: TTBXItem;
    tbiConfigScaleLineOptionsShow: TTBXItem;
    tbxIconsSettings: TTBXItem;
    TBSeparatorItem4: TTBSeparatorItem;
    actIconsSettings: TAction;
    TBCircleCalc: TTBXItem;
    actCircleCalculation: TAction;
    NCircleCalc: TTBXItem;
    actViewSunCalc: TAction;
    tbxSunCalc: TTBXItem;
    tbxYandexWeather: TTBXItem;
    actConfigUsePrevForVectorLayers: TAction;
    tbiConfigUsePrevForVectorLayer: TTBXItem;
    TBXSeparatorItem23: TTBXSeparatorItem;
    tbxMarksDbList: TTBXSubmenuItem;
    tbxSep4: TTBXSeparatorItem;
    tbxDoSearch: TTBXItem;
    tbxMoonCalc: TTBXItem;
    actViewMoonCalc: TAction;
    tbxExtendRoute: TTBXSubmenuItem;
    TBXSeparatorItem24: TTBXSeparatorItem;
    TBXSeparatorItem25: TTBXSeparatorItem;
    tbxUndoRouteCalc: TTBXItem;
    actMarksEditSnapToMarkers: TAction;
    actMarksEditDeleteGeometryPoint: TAction;
    tbxCalcLineLabelVisible: TTBSubmenuItem;
    tbxCalcLineShowDistIncrement: TTBXItem;
    tbxCalcLineShowIntermediateDist: TTBXItem;
    tbxCalcLineShowAzimuth: TTBXItem;
    actCalcLineLabelVisible: TAction;
    actCalcLineShowAzimuth: TAction;
    actCalcLineShowDistIncrement: TAction;
    actCalcLineShowIntermediateDist: TAction;
    actEditPathLabelVisible: TAction;
    actEditPathShowAzimuth: TAction;
    actEditPathShowDistIncrement: TAction;
    actEditPathShowIntermediateDist: TAction;
    actSelectByGeometryFinish: TAction;
    actLineEditSplitTogle: TAction;
    actLineEditFitToScreen: TAction;
    actMarkSave: TAction;
    actMarkSaveAsNew: TAction;
    actMarkSaveAsSeparateSegments: TAction;
    actEditPathRouteCalcUndo: TAction;
    tbElevationProfile: TTBXDockablePanel;
    tbxElevationProfileShow: TTBXVisibilityToggleItem;
    tbxitmElevationProfile: TTBXItem;
    actViewBordersVisible: TAction;
    tbxMainWindowBordersVisible: TTBXItem;
    tbitmFullMapMouseCursor: TTBXItem;
    actViewFullMapMouseCursorVisible: TAction;
    tbxEditPathTools: TTBXSubmenuItem;
    tbxEditPathReverse: TTBXItem;
    tbxEditPathReplaceElevation: TTBXItem;
    actLineEditReverse: TAction;
    actLineEditReplaceElevation: TAction;
    tbxEditPathShowPointHint: TTBXItem;
    actEditPathShowPointHint: TAction;
    tbxEditPolygonLabelVisible: TTBSubmenuItem;
    tbxEditPolygonShowPerimeter: TTBXItem;
    tbxEditPolygonShowArea: TTBXItem;
    actEditPolygonShowPerimeter: TAction;
    actEditPolygonShowArea: TAction;
    actEditPolygonLabelVisible: TAction;
    actGeoCalcUseGpsDatum: TAction;
    actGeoCalcUseZmpDatum: TAction;
    tbxsbmGeoCalc: TTBXSubmenuItem;
    tbxGeoCalcUseGpsDatum: TTBXItem;
    tbxGeoCalcUseZmpDatum: TTBXItem;
    tbxCopyUrlToNakarteMe: TTBXItem;
    tbitmCopyToClipboardCoordinatesAndElev: TTBXItem;
    TBSeparatorItem5: TTBSeparatorItem;
    TBSeparatorItem6: TTBSeparatorItem;
    tbxCopyUrlToGoogleEarthWeb: TTBXItem;

    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(
      Sender: TObject;
      var Action: TCloseAction
    );
    procedure ZoomToolBarDockChanging(
      Sender: TObject;
      Floating: Boolean;
      DockingTo: TTBDock
    );
    procedure tbitmOnInterfaceOptionsClick(Sender: TObject);
    procedure NaddPointClick(Sender: TObject);
    procedure tbitmCopyToClipboardMainMapTileClick(Sender: TObject);
    procedure tbitmCopyToClipboardMainMapTileFileNameClick(Sender: TObject);
    procedure tbitmDownloadMainMapTileClick(Sender: TObject);
    procedure NopendirClick(Sender: TObject);
    procedure tbitmOpenFolderMainMapTileClick(Sender: TObject);
    procedure NDelClick(Sender: TObject);
    procedure tbitmCopyToClipboardCoordinatesClick(Sender: TObject);
    procedure tbxCopyUrlToGoogleMapsClick(Sender: TObject);
    procedure mapResize(Sender: TObject);
    procedure tbxCopyUrlToYandexMapsClick(Sender: TObject);
    procedure tbxCopyUrlToKosmosnimkiRuClick(Sender: TObject);
    procedure mapDblClick(Sender: TObject);
    procedure tbitmMarkEditPositionClick(Sender: TObject);
    procedure NMarkDelClick(Sender: TObject);
    procedure NMarkOperClick(Sender: TObject);
    procedure tbxCopyUrlToBingMapsClick(Sender: TObject);
    procedure tbitmCopyToClipboardMainMapUrlClick(Sender: TObject);
    procedure mapMouseLeave(Sender: TObject);
    procedure mapMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer;
      Layer: TCustomLayer
    );
    procedure mapMouseUp(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer;
      Layer: TCustomLayer
    );
    procedure mapMouseMove(
      Sender: TObject;
      Shift: TShiftState;
      AX, AY: Integer;
      Layer: TCustomLayer
    );
    procedure TBEditPathClose(Sender: TObject);
    procedure NSRTM3Click(Sender: TObject);
    procedure NGTOPO30Click(Sender: TObject);
    procedure NMarkNavClick(Sender: TObject);
    procedure AdjustFont(
      Item: TTBCustomItem;
      Viewer: TTBItemViewer;
      Font: TFont;
      StateFlags: Integer
    );
    procedure NMapInfoClick(Sender: TObject);
    procedure tbtpltCachedTilesMapCellClick(
      Sender: TTBXCustomToolPalette;
      var ACol, ARow: Integer;
      var AllowChange: Boolean
    );
    procedure TBXSensorsBarVisibleChanged(Sender: TObject);
    procedure NMarkExportClick(Sender: TObject);
    procedure ZSliderMouseMove(
      Sender: TObject;
      Shift: TShiftState;
      X,
      Y: Integer;
      Layer: TCustomLayer
    );
    procedure ZSliderMouseUp(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer;
      Layer: TCustomLayer
    );
    procedure MainPopupMenuPopup(Sender: TObject);
    procedure tbtpltCenterWithZoomCellClick(
      Sender: TTBXCustomToolPalette;
      var ACol, ARow: Integer;
      var AllowChange: Boolean
    );
    procedure tbtpltViewGridTileCellClick(
      Sender: TTBXCustomToolPalette;
      var ACol, ARow: Integer;
      var AllowChange: Boolean
    );
    procedure tbtpltViewGridTileGetCellVisible(
      Sender: TTBXCustomToolPalette;
      ACol, ARow: Integer;
      var Visible: Boolean
    );
    procedure NSensorsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShortCut(
      var Msg: TWMKey;
      var Handled: Boolean
    );
    procedure NParamsPopup(
      Sender: TTBCustomItem;
      FromLink: Boolean
    );
    procedure TBSearchWindowClose(Sender: TObject);
    procedure TBEditSelectPolylineRadiusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tbxCopyUrlToOpenStreetMapClick(Sender: TObject);
    procedure DateTimePicker1Change(Sender: TObject);
    procedure DateTimePicker2Change(Sender: TObject);
    procedure FormMouseWheel(
      Sender: TObject;
      Shift: TShiftState;
      WheelDelta:
      Integer;
      MousePos: TPoint;
      var Handled: Boolean
    );
    procedure NDegValueAcceptText(
      Sender: TObject;
      var NewText: string;
      var Accept: Boolean
    );
    procedure tbpmiVersionsPopup(
      Sender: TTBCustomItem;
      FromLink: Boolean
    );
    procedure tbpmiClearVersionClick(Sender: TObject);
    procedure tbitmCopySearchResultCoordinatesClick(Sender: TObject);
    procedure tbitmMarkEditPropertiesClick(Sender: TObject);
    procedure tbitmFitMarkToScreenClick(Sender: TObject);
    procedure tbitmHideThisMarkClick(Sender: TObject);
    procedure tbitmCopySearchResultDescriptionClick(Sender: TObject);
    procedure tbitmCreatePlaceMarkBySearchResultClick(Sender: TObject);
    procedure NMarkPlayClick(Sender: TObject);
    procedure tbitmMarkInfoClick(Sender: TObject);
    procedure tbitmCopyToClipboardGenshtabNameClick(Sender: TObject);
    procedure tbxWeatherUndergroundClick(Sender: TObject);
    procedure NMapStorageInfoClick(Sender: TObject);
    procedure tbitmMakeVersionByMarkClick(Sender: TObject);
    procedure tbitmSelectVersionByMarkClick(Sender: TObject);
    procedure tbpmiShowOtherVersionsClick(Sender: TObject);
    procedure tbMergePolygonsClose(Sender: TObject);
    procedure tbxtmAddToMergePolygonsClick(Sender: TObject);
    procedure tbxitmElevationProfileClick(Sender: TObject);
    procedure tbxFillingMapClick(Sender: TObject);
    procedure tbxYandexWeatherClick(Sender: TObject);

    procedure actSelectByRectExecute(Sender: TObject);
    procedure actSelectByPolygonExecute(Sender: TObject);
    procedure actSelectByLineExecute(Sender: TObject);
    procedure actSelectByCoordinatesExecute(Sender: TObject);
    procedure actSelectByVisibleAreaExecute(Sender: TObject);
    procedure actMakeLinkOnDesktopExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure actShowGoToExecute(Sender: TObject);
    procedure actSelectByLastSelectionExecute(Sender: TObject);
    procedure actSelectByLastSelectionEditExecute(Sender: TObject);
    procedure actSelectBySelectionFromFileExecute(Sender: TObject);
    procedure actShowCacheManagerExecute(Sender: TObject);
    procedure actQuitExecute(Sender: TObject);
    procedure actDistanceCalculationExecute(Sender: TObject);
    procedure actMoveMapExecute(Sender: TObject);
    procedure actViewFullScreenExecute(Sender: TObject);
    procedure actConfigZoomToCursorExecute(Sender: TObject);
    procedure actConfigUsePrevForMapExecute(Sender: TObject);
    procedure actConfigUsePrevForLayersExecute(Sender: TObject);
    procedure actConfigUseZoomAnimationExecute(Sender: TObject);
    procedure actConfigUseInertialMovementExecute(Sender: TObject);
    procedure actConfigAzimuthCircleExecute(Sender: TObject);
    procedure actConfigColorInversionExecute(Sender: TObject);
    procedure actConfigPreviousSelectionVisibleExecute(Sender: TObject);
    procedure actViewNavigationExecute(Sender: TObject);
    procedure actShowDebugInfoExecute(Sender: TObject);
    procedure actConfigStatusBarVisibleExecute(Sender: TObject);
    procedure actConfigMiniMapVisibleExecute(Sender: TObject);
    procedure actConfigScaleLineVisibleExecute(Sender: TObject);
    procedure actViewToolbarsLockExecute(Sender: TObject);
    procedure actViewGridGenShtabExecute(Sender: TObject);
    procedure actViewGridLonLatExecute(Sender: TObject);
    procedure actHelpShowAboutExecute(Sender: TObject);
    procedure actShowPascalScriptIdeExecute(Sender: TObject);
    procedure actShowUpddateCheckerExecute(Sender: TObject);
    procedure actViewFillingMapMarkUnexistingExecute(Sender: TObject);
    procedure actViewFillingMapMarkExistingExecute(Sender: TObject);
    procedure actViewFillingMapMarkGradientExecute(Sender: TObject);
    procedure actViewFillingMapFilterModeExecute(Sender: TObject);
    procedure actViewSelectNextMapWithTileExecute(Sender: TObject);
    procedure actViewSelectPrevMapWithTileExecute(Sender: TObject);
    procedure actViewSelectNextMapVersionExecute(Sender: TObject);
    procedure actViewSelectPrevMapVersionExecute(Sender: TObject);
    procedure actConfigMarksNamesVisibleExecute(Sender: TObject);
    procedure actShowPointProjectExecute(Sender: TObject);
    procedure actMarksAddPointExecute(Sender: TObject);
    procedure actMarksAddLineExecute(Sender: TObject);
    procedure actMarksAddPolygonExecute(Sender: TObject);
    procedure actShowPlacemarkManagerExecute(Sender: TObject);
    procedure actConfigMarksHideExecute(Sender: TObject);
    procedure actConfigDownloadModeExecute(Sender: TObject);
    procedure actMapsEditMapParamsExecute(Sender: TObject);
    procedure actConfigOptionsShowExecute(Sender: TObject);
    procedure actGpsConnectExecute(Sender: TObject);
    procedure actConfigGpsShowTrackExecute(Sender: TObject);
    procedure actConfigGpsFollowPositionExecute(Sender: TObject);
    procedure actConfigGpsFollowPositionAtCenterExecute(Sender: TObject);
    procedure actGpsMarkPointAddExecute(Sender: TObject);
    procedure actGpsTrackSaveToDbExecute(Sender: TObject);
    procedure actGpsTrackClearExecute(Sender: TObject);
    procedure actConfigGpsOptionsShowExecute(Sender: TObject);
    procedure actConfigProjectionUseExecute(Sender: TObject);
    procedure actViewFillingMapMapUseExecute(Sender: TObject);
    procedure actConfigInterfaceOptionsShowExecute(Sender: TObject);
    procedure actMapsAllLayersHideExecute(Sender: TObject);
    procedure actFavoriteAddExecute(Sender: TObject);
    procedure actFavoriteManageExecute(Sender: TObject);
    procedure actViewNotMinimizedExecute(Sender: TObject);
    procedure actViewTilesGridExecute(Sender: TObject);
    procedure actConfigScaleLineExtendedExecute(Sender: TObject);
    procedure actConfigScaleLineOptionsShowExecute(Sender: TObject);
    procedure actConfigScaleLineNumberFormatExecute(Sender: TObject);
    procedure actIconsSettingsExecute(Sender: TObject);
    procedure actCircleCalculationExecute(Sender: TObject);
    procedure actViewSunCalcExecute(Sender: TObject);
    procedure actViewMoonCalcExecute(Sender: TObject);
    procedure actConfigUsePrevForVectorLayersExecute(Sender: TObject);
    procedure actMarksEditSnapToMarkersExecute(Sender: TObject);
    procedure actMarksEditDeleteGeometryPointExecute(Sender: TObject);
    procedure actCalcLineLabelVisibleExecute(Sender: TObject);
    procedure actCalcLineShowAzimuthExecute(Sender: TObject);
    procedure actCalcLineShowDistIncrementExecute(Sender: TObject);
    procedure actCalcLineShowIntermediateDistExecute(Sender: TObject);
    procedure actEditPathLabelVisibleExecute(Sender: TObject);
    procedure actEditPathShowAzimuthExecute(Sender: TObject);
    procedure actEditPathShowDistIncrementExecute(Sender: TObject);
    procedure actEditPathShowIntermediateDistExecute(Sender: TObject);
    procedure actSelectByGeometryFinishExecute(Sender: TObject);
    procedure actLineEditSplitTogleExecute(Sender: TObject);
    procedure actLineEditFitToScreenExecute(Sender: TObject);
    procedure actMarkSaveAsNewExecute(Sender: TObject);
    procedure actMarkSaveExecute(Sender: TObject);
    procedure actMarkSaveAsSeparateSegmentsExecute(Sender: TObject);
    procedure actEditPathRouteCalcUndoExecute(Sender: TObject);
    procedure actViewBordersVisibleExecute(Sender: TObject);
    procedure actViewFullMapMouseCursorVisibleExecute(Sender: TObject);
    procedure actLineEditReverseExecute(Sender: TObject);
    procedure actLineEditReplaceElevationExecute(Sender: TObject);
    procedure actEditPathShowPointHintExecute(Sender: TObject);
    procedure actEditPolygonShowPerimeterExecute(Sender: TObject);
    procedure actEditPolygonShowAreaExecute(Sender: TObject);
    procedure actEditPolygonLabelVisibleExecute(Sender: TObject);
    procedure actGeoCalcUseZmpDatumExecute(Sender: TObject);
    procedure actGeoCalcUseGpsDatumExecute(Sender: TObject);
    procedure tbxCopyUrlToNakarteMeClick(Sender: TObject);
    procedure tbitmCopyToClipboardCoordinatesAndElevClick(Sender: TObject);
    procedure tbxCopyUrlToGoogleEarthWebClick(Sender: TObject);
  private
    FactlstProjections: TActionList;
    FactlstLanguages: TActionList;
    FactlstTileGrids: TActionList;
    FLinksList: IListenerNotifierLinksList;
    FConfig: IMainFormConfig;
    FMainMapState: IMainMapsState;
    FTimer: ITimer;
    FActiveProjectionSet: IProjectionSetChangeable;
    FViewPortState: IViewPortState;
    FStickToGrid: IStickToGrid;
    FSensorList: ISensorList;
    FCenterToGPSDelta: TDoublePoint;
    FMapHintWindow: TMapHintWindow;
    FKeyMovingHandler: IMessageHandler;
    FMouseHandler: IMouseHandler;
    FMouseState: IMouseState;
    FMoveByMouseStartPoint: TPoint;

    FRouteComment: string;
    FRouteUndoPath: IPathOnMapEdit;

    FUIDownload: IInterface;

    ProgramStart: Boolean;
    FStartedNormal: Boolean;

    FMapTypeIcons18List: IMapTypeIconsList;
    FMapTypeIcons24List: IMapTypeIconsList;

    FNLayerParamsItemList: IGUIDObjectSet; //����� ������� ���� ���������/��������� ����
    FNDwnItemList: IGUIDObjectSet; //����� ������������ ���� ��������� ���� ����
    FNDelItemList: IGUIDObjectSet; //����� ������������ ���� ������� ���� ����
    FNOpenDirItemList: IGUIDObjectSet; //����� ������������ ���� ������� ����� ����
    FNCopyLinkItemList: IGUIDObjectSet; //����� ������������ ���� ���������� ������ �� ���� ����
    FNLayerInfoItemList: IGUIDObjectSet; //����� ������������ ���� ���������� � ����

    FShortCutManager: TShortcutManager;
    FLayersList: IMainFormLayersList;
    FWikiLayer: IFindVectorItems;
    FLayerMapMarks: IFindVectorItems;
    FLayerSearchResults: IFindVectorItems;

    FSearchPresenter: ISearchResultPresenter;
    FSearchToolbarContainer: TSearchToolbarContainer;

    FMergePolygonsPresenter: IMergePolygonsPresenter;
    FMergePolygonsResult: IMergePolygonsResult;

    FElevationMetaWriter: IElevationMetaWriter;
    FElevationProfilePresenter: IElevationProfilePresenter;

    FMapMovingButton: TMouseButton;
    FMapZoomAnimtion: Boolean;
    FMapMoveAnimtion: Boolean;
    FMovePoint: Boolean;
    FSelectedMark: IVectorDataItem;
    FSelectedWiki: IVectorDataItem;
    FEditMarkPoint: IVectorDataItem;
    FEditMarkLine: IVectorDataItem;
    FEditMarkPoly: IVectorDataItem;

    FState: IMainFormState;
    FStateValueOld: TStateEnum;

    FUrlProviderOSM: IUrlByCoordProvider;
    FUrlProviderBing: IUrlByCoordProvider;
    FUrlProviderGoogle: IUrlByCoordProvider;
    FUrlProviderGoogleEarthWeb: IUrlByCoordProvider;
    FUrlProviderYandex: IUrlByCoordProvider;
    FUrlProviderNakarte: IUrlByCoordProvider;
    FUrlProviderKosmosnimki: IUrlByCoordProvider;

    FUrlProviderGTopo30: IUrlByCoordProvider;
    FUrlProviderSTRM3: IUrlByCoordProvider;
    FUrlProviderWeatherUnderground: IUrlByCoordProvider;
    FUrlProviderYandexWeather: IUrlByCoordProvider;

    FWinPosition: IMainWindowPosition;
    FPanelPositionSaveLoad: IPanelsPositionsSaveLoad;
    FStateConfigDataProvider: IConfigDataWriteProvider;

    FMarkOnMapEditProvider: IMarkOnMapEditProviderInternal;
    FLineOnMapEdit: ILineOnMapEdit;
    FLineOnMapByOperation: array [TStateEnum] of ILineOnMapEdit;
    FPointOnMapEdit: IPointOnMapEdit;
    FSelectionRect: ISelectionRect;
    FMarkDBGUI: TMarkDbGUIHelper;
    FPlacemarkPlayerPlugin: IPlayerPlugin;

    FTileStorageImporter: TTileStorageImporter;

    FTileErrorLogger: ITileErrorLogger;
    FTileErrorLogProvider: ITileErrorLogProviedrStuped;

    FRuller: TBitmap32;
    FTumbler: TBitmap32;
    FSensorViewList: IGUIDInterfaceSet;
    FFormRegionProcess: TfrmRegionProcess;
    FRegionProcess: IRegionProcess;
    FfrmGoTo: TfrmGoTo;
    FfrmSettings: TfrmSettings;
    FfrmMapLayersOptions: TfrmMapLayersOptions;
    FfrmCacheManager: TfrmCacheManager;
    FfrmMarksExplorer: TfrmMarksExplorer;
    FfrmAbout: TfrmAbout;
    FfrmMarkPictureConfig: TfrmMarkPictureConfig;
    FfrmPointProjecting: TfrmPointProjecting;
    FfrmUpdateChecker: TfrmUpdateChecker;
    FfrmPascalScriptIDE: TfrmPascalScriptIDE;

    FPathProvidersTree: ITreeChangeable;
    FPathProvidersTreeStatic: IStaticTreeItem;
    FPathProvidersMenuBuilder: IMenuGeneratorByTree;
    FPathProvidersConfigMenuBuilder: IMenuGeneratorByTree;
    FMapHotKeyList: IMapTypeHotKeyListStatic;
    FMapTypeEditor: IMapTypeConfigModalEdit;

    FMapGoto: IMapViewGoto;
    FGpsTrackGoTo: IMapViewGoto;

    FArgProcessor: ICmdLineArgProcessor;
    FFillingMapPolygon: IFillingMapPolygon;
    FSelectedPolygon: IGeometryLonLatPolygon;

    FSunCalcProvider: ISunCalcProvider;

    FFavoriteMapSetMenu: TFavoriteMapSetMenu;
    FFavoriteMapSetHelper: IFavoriteMapSetHelper;
    FFavoriteMapSetHotKeyList: IFavoriteMapSetHotKeyList;
    FfrmFavoriteMapSetEditor: TfrmFavoriteMapSetEditor;
    FfrmFavoriteMapSetManager: TfrmFavoriteMapSetManager;

    FMarksDbMenu: TMarksDbMenu;

    procedure InitSearchers;
    procedure InitMergepolygons;
    procedure InitElevationProfile;
    procedure InitLayers;
    procedure InitGridsMenus;
    procedure InitMouseCursors;
    procedure InitUrlProviders;
    procedure ProcessCmdLineArgs;
    procedure LoadPosition;
    procedure LoadMapIconsList;
    procedure CreateMapUIMapsList;
    procedure CreateMapUILayersList;
    procedure CreateMapUIFillingList;
    procedure CreateMapUILayerSubMenu;
    procedure CreateLangMenu;
    procedure CreateViewTilesGridActions;
    procedure CreateViewTilesGridMenu;

    procedure CreateProjectionActions;

    procedure CreateProjectionMenu;

    procedure GPSReceiverDisconnect;
    procedure GPSReceiverStateChange;
    procedure GPSReceiverConnect;
    procedure GPSReceiverTimeout;
    procedure GPSReceiverConnectError;
    procedure GPSReceiverReceive;

    procedure OnMapGUIChange;
    procedure OnWinPositionChange;
    procedure OnToolbarsLockChange;
    procedure OnLineOnMapEditChange;
    procedure OnPathProvidesChange;
    procedure OnNavToMarkChange;
    procedure OnMarkEditConfigsChange;

    procedure DoMessageEvent(
      var Msg: TMsg;
      var Handled: Boolean
    );

    procedure WMGetMinMaxInfo(var msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMTimeChange(var m: TMessage); message WM_TIMECHANGE;
    Procedure WMSize(Var Msg: TWMSize); Message WM_SIZE;
    Procedure WMMove(Var Msg: TWMMove); Message WM_MOVE;
    Procedure WMSysCommand(Var Msg: TMessage); Message WM_SYSCOMMAND;
    Procedure WMCopyData(Var Msg: TMessage); Message WM_COPYDATA;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMFriendOrFoeMessage(var Msg: TMessage); message u_CmdLineArgProcessorAPI.WM_FRIEND_OR_FOE;

    procedure ProcessViewGridTileCellClick(const ATag: Integer);

    procedure MapZoom(
      const ANewZoom: Byte;
      const AFreezePos: TPoint
    );
    procedure MapMoveAnimate(
      const AMouseMoveSpeed: TDoublePoint;
      const AZoom: Byte;
      const AMousePos: TPoint
    );
    procedure ProcessPosChangeMessage;
    function GetIgnoredMenuItemsList: TList;
    procedure MapLayersVisibleChange;
    procedure OnMainFormMainConfigChange;
    procedure OnStateChange;

    procedure OnGeoCalcConfigChange;
    procedure OnViewProjectionConfigChange;
    procedure OnGridGenshtabChange;
    procedure OnGridLonLatChange;
    procedure OnGridTileChange;
    procedure OnMainMapChange;
    procedure OnActivLayersChange;
    procedure OnFillingMapChange;
    procedure OnShowSearchResults(Sender: TObject);
    procedure OnShowMergePolygons(Sender: TObject);

    procedure PaintZSlider(zoom: integer);

    procedure OnBeforeViewChange;
    procedure OnAfterViewChange;
    procedure SavePosition(const AProvider: IConfigDataWriteProvider);
    procedure SaveWindowConfigToIni(const AProvider: IConfigDataWriteProvider);
    procedure DoSelectSpecialVersion(Sender: TObject);

    procedure MakeBackupOfPathOnMapEdit(
      const APathOnMapEdit: IPathOnMapEdit;
      const AIncludeActivePoint: Boolean
    );
    procedure TBEditPathMarshClick(Sender: TObject);
    procedure tbxExtendRouteSelect(Sender: TObject);
    procedure ExtendRoute;

    procedure TBEditSelectPolylineRadiusOnUpClick(Sender: TObject);
    procedure TBEditSelectPolylineRadiusOnDownClick(Sender: TObject);

    procedure tbiEditSrchAcceptText(
      Sender: TObject;
      var NewText: String;
      var Accept: Boolean
    );

    procedure SaveConfig(Sender: TObject);
    function ConvLatLon2Scale(const Astr: string): Double;
    function Deg2StrValue(const aDeg: Double): string;

    function SelectForEdit(
      const AList: IVectorItemSubset;
      const ALocalConverter: ILocalCoordConverter
    ): IVectorDataItem;

    function FindItems(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IVectorItemSubset;

    function FindPathOnMapEdit(
      const AVisualConverter: ILocalCoordConverter;
      const ALocalPoint: TPoint
    ): IVectorItemSubset;

    procedure ProcessOpenFiles(const AFiles: IStringListStatic);

    procedure SwitchSunCalc(const ACalcType: TSunCalcDataProviderType);
    procedure OnEditMarkPosition(const AMark: IVectorDataItem);
    procedure OnElevationMetaWriterResult(const ALine: IGeometryLonLatLine);
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    procedure RefreshTranslation; override;
    function IsShortCut(var AMsg: TWMKey): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  StrUtils,
  Math,
  IniFiles,
  gnugettext,
  GR32_Gamma,
  t_CommonTypes,
  t_FillingMapModes,
  t_MainFormLayersListParams,
  c_ZeroGUID,
  c_InternalBrowser,
  i_GlobalConfig,
  i_GlobalInternetState,
  i_MarkSystem,
  i_MainFormLayersConfig,
  i_ContentTypeManager,
  i_NotifierTime,
  i_LastSearchResult,
  i_NavigationToPoint,
  i_LocalCoordConverterFactorySimpe,
  i_GeometryProjectedProvider,
  i_CoordToStringConverter,
  i_ValueToStringConverter,
  i_HashFunction,
  i_Bitmap32BufferFactory,
  i_ImageResamplerFactory,
  i_BitmapPostProcessing,
  i_TerrainProviderList,
  i_LastSelectionInfo,
  i_DownloadInfoSimple,
  i_PathDetalizeConfig,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_InternalPerformanceCounter,
  i_MarkId,
  i_MapType,
  i_MapTypeSet,
  i_GeoCoderList,
  i_ProjectionType,
  i_Projection,
  i_GeometryProjected,
  i_LonLatRect,
  i_LocalCoordConverterChangeable,
  i_GUIDListStatic,
  i_ActiveMapsConfig,
  i_LanguageManager,
  i_DoublePointFilter,
  i_PathDetalizeProviderTreeEntity,
  i_SensorViewListGenerator,
  i_VectorItemSubsetBuilder,
  i_GeometryLonLatFactory,
  i_GeometryProjectedFactory,
  i_ConfigDataProvider,
  i_PointCaptionsLayerConfig,
  i_PolygonCaptionsLayerConfig,
  i_MapVersionInfo,
  i_MapVersionRequest,
  i_MapVersionListStatic,
  i_MapLayerGridsConfig,
  i_InternalDomainOptions,
  i_InterfaceListSimple,
  i_TileInfoBasic,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileRectChangeable,
  i_TerrainInfo,
  i_DownloadRequest,
  i_GPS,
  i_GeoCoder,
  i_GeoCalcConfig,
  i_GPSRecorder,
  i_PathDetalizeProvider,
  i_FillingMapLayerConfig,
  i_PopUp,
  i_ProjectionSet,
  i_ProjectionSetList,
  i_ScaleLineConfig,
  i_DoublePointsAggregator,
  i_FavoriteMapSetItemStatic,
  u_SortFunc,
  u_InterfaceListSimple,
  u_FavoriteMapSetHotKeyList,
  u_FavoriteMapSetHelper,
  u_StickToGrids,
  u_Dialogs,
  u_GeoFunc,
  u_GeoToStrFunc,
  u_HtmlDoc,
  u_StringListStatic,
  u_ResStrings,
  u_SensorViewListGeneratorStuped,
  u_MainWindowPositionConfig,
  u_TileErrorLogProviedrStuped,
  u_TileRectChangeableByLocalConverter,
  u_TerrainInfo,
  u_LineOnMapEdit,
  u_PointOnMapEdit,
  u_MarkOnMapEditProvider,
  u_MapTypeIconsList,
  u_SelectionRect,
  u_KeyMovingHandler,
  u_MapViewGoto,
  u_ActionListByLanguageManager,
  u_MouseState,
  u_UITileDownloadList,
  u_MapTypeConfigModalEditByForm,
  u_ConfigProviderHelpers,
  u_EnumDoublePointLine2Poly,
  u_SaveLoadTBConfigByConfigProvider,
  u_MapTypeMenuItemsGeneratorBasic,
  u_MenuGeneratorByStaticTreeSimple,
  u_MainMapsState,
  u_NotifierOperation,
  u_MainFormState,
  u_ViewPortState,
  u_ProjectionSetChangeableByConfig,
  u_MainFormConfig,
  u_SensorListStuped,
  u_SearchResultPresenterOnPanel,
  u_MergePolygonsResult,
  u_MergePolygonsPresenterOnPanel,
  u_ElevationMetaWriter,
  u_ElevationProfilePresenterOnPanel,
  u_ListenerNotifierLinksList,
  u_TileDownloaderUIOneTile,
  u_ListenerByEvent,
  u_GUIDObjectSet,
  u_GlobalState,
  u_Synchronizer,
  u_GeometryFunc,
  u_InetFunc,
  u_BitmapFunc,
  u_ClipboardFunc,
  u_UrlByCoordProvider,
  u_LayerSunCalcPopupMenu,
  u_LayerScaleLinePopupMenu,
  u_LayerStatBarPopupMenu,
  u_LayerMiniMapPopupMenu,
  u_PlayerPlugin,
  u_FillingMapPolygon,
  u_SunCalcProvider,
  u_DoublePointsMetaFunc,
  u_DoublePointsAggregator,
  u_ConfigDataWriteProviderByIniFile,
  u_CmdLineArgProcessor,
  u_CmdLineArgProcessorAsync,
  u_CmdLineArgProcessorHelpers,
  frm_LonLatRectEdit;

type
  TTBXItemSelectMapVersion = class(TTBXItem)
  protected
    MapVersion: IMapVersionInfo;
  end;

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
var
  VLogger: TTileErrorLogProviedrStuped;
  VMouseState: TMouseState;
  VLineOnMapEditChangeListener: IListener;
  VMarkEditConfigsListener: IListener;
  VBitmapStatic: IBitmap32Static;
  VFormStateFileName: string;
  VIniFile: TMemIniFile;
begin
  inherited;

  GR32_Gamma.SetGamma(1);

  FStartedNormal := False;
  FMovePoint := False;
  FMapZoomAnimtion := False;
  FTimer := GState.Timer;
  FLinksList := TListenerNotifierLinksList.Create;
  FState := TMainFormState.Create;
  VMouseState := TMouseState.Create(GState.Timer);
  FMouseHandler := VMouseState;
  FMouseState := VMouseState;
  FFillingMapPolygon := TFillingMapPolygon.Create;
  FMergePolygonsResult := TMergePolygonsResult.Create;

  FConfig := TMainFormConfig.Create(GState.BaseConfigPath, GState.MapType.DefaultMainMapGUID);
  FConfig.ReadConfig(GState.MainConfigProvider);

  VFormStateFileName := FConfig.FormStateConfigPath.FullPath;
  VIniFile := TMeminifile.Create(VFormStateFileName);
  try
    VIniFile.Encoding := TEncoding.UTF8;
    FStateConfigDataProvider := TConfigDataWriteProviderByIniFile.CreateWithOwn(VIniFile);
    VIniFile := nil;
  finally
    VIniFile.Free;
  end;

  FMainMapState :=
    TMainMapsState.Create(
      GState.MapTypeSetBuilderFactory,
      GState.MapTypeListBuilderFactory,
      GState.MapType.MapsSet,
      GState.MapType.LayersSet,
      GState.MapType.FirstMainMapGUID,
      FConfig.MainMapConfig,
      FConfig.MapLayersConfig,
      FConfig.LayersConfig.MiniMapLayerConfig.MapConfig,
      FConfig.LayersConfig.MiniMapLayerConfig.LayersConfig,
      FConfig.LayersConfig.FillingMapLayerConfig.SourceMap
    );
  FActiveProjectionSet :=
    TProjectionSetChangeableByConfig.Create(
      GState.ProjectionSetFactory,
      FMainMapState.ActiveMap,
      FConfig.ViewProjectionConfig
    );
  FViewPortState :=
    TViewPortState.Create(
      GState.LocalConverterFactory,
      FActiveProjectionSet,
      GState.DebugInfoSubSystem.RootCounterList.CreateAndAddNewSubList('ViewState')
    );
  LoadPosition;

  InitUrlProviders;

  LoadMapIconsList;

  FMapGoto := TMapViewGoto.Create(FActiveProjectionSet, FViewPortState);
  FGpsTrackGoTo := TMapViewGoto.Create(FActiveProjectionSet, FViewPortState);

  FMarkOnMapEditProvider := TMarkOnMapEditProvider.Create(Self.OnEditMarkPosition);

  FMarkDBGUI :=
    TMarkDbGUIHelper.Create(
      Self,
      GState.Config.LanguageManager,
      FActiveProjectionSet,
      GState.Config.MediaDataPath,
      GState.Config.MarksFactoryConfig,
      GState.Config.MarksGUIConfig,
      FConfig.MarksExplorerConfig,
      GState.MarkPictureList,
      GState.AppearanceOfMarkFactory,
      GState.MarksDb,
      FMarkOnMapEditProvider,
      GState.GeoCalc,
      GState.Config.InetConfig,
      GState.InternalDomainUrlHandler,
      GState.ExporterList,
      GState.ImporterList,
      FViewPortState.View,
      GState.VectorDataFactory,
      GState.VectorDataItemMainInfoFactory,
      GState.VectorGeometryLonLatFactory,
      GState.ArchiveReadWriteFactory,
      GState.VectorItemSubsetBuilderFactory,
      GState.Config.CoordRepresentationConfig,
      GState.CoordFromStringParser,
      GState.CoordToStringConverter,
      GState.ValueToStringConverter
    );
  FFormRegionProcess :=
    TfrmRegionProcess.Create(
      GState.Config.LanguageManager,
      GState.PerfCounterList.CreateAndAddNewSubList('RegionProcess'),
      GState.AppClosingNotifier,
      GState.GUISyncronizedTimerNotifier,
      GState.LastSelectionInfo,
      FConfig.MainMapConfig,
      FConfig.MapLayersConfig,
      FConfig.ViewProjectionConfig,
      FMainMapState.ActiveBitmapLayersList,
      GState.MapTypeListBuilderFactory,
      GState.GlobalBerkeleyDBHelper,
      FViewPortState.View,
      FActiveProjectionSet,
      GState.MapType.FullMapsSet,
      GState.MapType.GUIConfigList,
      GState.ContentTypeManager,
      GState.ProjectionSetFactory,
      GState.TileStorageTypeList,
      GState.TileNameGenerator,
      GState.Config.ViewConfig,
      FConfig.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig,
      GState.ImageResamplerFactoryList,
      GState.Config.ImageResamplerConfig,
      GState.Config.TileReprojectResamplerConfig,
      FConfig.LayersConfig.MarksLayerConfig.MarksShowConfig,
      FConfig.LayersConfig.MarksLayerConfig.MarksDrawConfig,
      GState.MarksDb,
      GState.HashFunction,
      GState.BitmapPostProcessing,
      GState.ProjectionSetList,
      GState.VectorGeometryLonLatFactory,
      GState.VectorGeometryProjectedFactory,
      GState.ProjectedGeometryProvider,
      GState.VectorItemSubsetBuilderFactory,
      GState.Bitmap32StaticFactory,
      GState.BitmapTileSaveLoadFactory,
      GState.ArchiveReadWriteFactory,
      GState.MapCalibrationList,
      GState.Config.DownloadConfig,
      GState.DownloadInfo,
      FConfig.LayersConfig.FillingMapLayerConfig,
      FMainMapState.FillingMapActiveMap,
      FFillingMapPolygon,
      FConfig.LayersConfig.MapLayerGridsConfig,
      GState.CoordToStringConverter,
      GState.ValueToStringConverter,
      FMapGoto,
      FMarkDBGUI
    );
  FRegionProcess := FFormRegionProcess;
  FFormRegionProcess.PopupParent := Self;

  FfrmCacheManager :=
    TfrmCacheManager.Create(
      GState.Config.LanguageManager,
      GState.BaseApplicationPath,
      GState.AppClosingNotifier,
      GState.GUISyncronizedTimerNotifier,
      GState.BGTimerNotifier,
      GState.MapVersionFactoryList,
      GState.GlobalBerkeleyDBHelper,
      GState.ContentTypeManager,
      GState.ProjectionSetFactory,
      GState.ArchiveReadWriteFactory,
      GState.TileStorageTypeList,
      GState.TileNameGenerator,
      GState.TileNameParser,
      GState.ValueToStringConverter
    );
  FfrmCacheManager.PopupParent := Self;

  FfrmUpdateChecker :=
    TfrmUpdateChecker.Create(
      GState.Config.LanguageManager,
      GState.Config.UpdatesPath,
      GState.BuildInfo,
      GState.Config.InetConfig,
      GState.DownloaderFactory
    );
  FfrmUpdateChecker.PopupParent := Self;

  FfrmPascalScriptIDE :=
    TfrmPascalScriptIDE.Create(
      GState.AppEnum.CurrentID,
      GState.BGTimerNotifier,
      GState.MapType.GUIConfigList,
      FMainMapState,
      GState.Config.ZmpConfig,
      GState.Config.LogsPath,
      GState.DownloaderFactory,
      GState.ProjectionSetFactory,
      GState.ContentTypeManager,
      GState.AppearanceOfMarkFactory,
      GState.MarkPictureList,
      GState.MapVersionFactoryList.GetSimpleVersionFactory,
      GState.BufferFactory,
      GState.Bitmap32StaticFactory,
      GState.Config.InetConfig,
      GState.ProjConverterFactory,
      GState.ArchiveReadWriteFactory,
      GState.Config.LanguageManager,
      GState.AppClosingNotifier,
      FViewPortState.View,
      GState.TileStorageTypeList
    );
  FfrmPascalScriptIDE.PopupParent := Self;

  FfrmPointProjecting :=
    TfrmPointProjecting.Create(
      GState.Config.LanguageManager,
      GState.VectorGeometryLonLatFactory,
      FMarkDBGUI,
      FViewPortState.View,
      GState.GeoCalc
    );
  FfrmPointProjecting.PopupParent := Self;

  FFavoriteMapSetHotKeyList :=
    TFavoriteMapSetHotKeyList.Create(
      GState.FavoriteMapSetConfig
    );

  FFavoriteMapSetHelper :=
    TFavoriteMapSetHelper.Create(
      GState.MapType.FullMapsSet,
      FConfig.MainMapConfig,
      FConfig.MapLayersConfig,
      FViewPortState
    );

  FfrmFavoriteMapSetEditor :=
    TfrmFavoriteMapSetEditor.Create(
      GState.Config.LanguageManager,
      GState.FavoriteMapSetConfig,
      FViewPortState.View,
      FActiveProjectionSet,
      GState.Config.CoordRepresentationConfig,
      GState.CoordFromStringParser,
      GState.CoordToStringConverter,
      FConfig.MainMapConfig,
      FConfig.MapLayersConfig,
      GState.MapType.FullMapsSet,
      GState.MapType.GUIConfigList
    );
  FfrmFavoriteMapSetEditor.PopupParent := Self;

  FfrmFavoriteMapSetManager :=
    TfrmFavoriteMapSetManager.Create(
      GState.Config.LanguageManager,
      GState.MapType.FullMapsSet,
      GState.CoordToStringConverter,
      GState.FavoriteMapSetConfig,
      FFavoriteMapSetHelper,
      FfrmFavoriteMapSetEditor
    );
  FfrmFavoriteMapSetManager.PopupParent := Self;

  FfrmMapLayersOptions := TfrmMapLayersOptions.Create(
    GState.Config.LanguageManager,
    FConfig.LayersConfig.ScaleLineConfig,
    FConfig.LayersConfig.StatBar,
    GState.Config.TerrainConfig,
    GState.TerrainProviderList
  );

  FMapTypeEditor :=
    TMapTypeConfigModalEditByForm.Create(
      GState.Config.LanguageManager,
      FMainMapState,
      GState.TileStorageTypeList
    );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnStateChange),
    FState.ChangeNotifier
  );
  VLogger := TTileErrorLogProviedrStuped.Create;
  FTileErrorLogger := VLogger;
  FTileErrorLogProvider := VLogger;

  FCenterToGPSDelta := CEmptyDoublePoint;

  FPlacemarkPlayerPlugin := TPlayerPlugin.Create;

  TBSMB.Images := FMapTypeIcons24List.GetImageList;
  TBSMB.SubMenuImages := FMapTypeIcons18List.GetImageList;
  TBLayerSel.SubMenuImages := FMapTypeIcons18List.GetImageList;
  TBFillingTypeMap.SubMenuImages := FMapTypeIcons18List.GetImageList;
  NSMB.SubMenuImages := FMapTypeIcons18List.GetImageList;
  NLayerSel.SubMenuImages := FMapTypeIcons18List.GetImageList;
  NLayerParams.SubMenuImages := FMapTypeIcons18List.GetImageList;
  ldm.SubMenuImages := FMapTypeIcons18List.GetImageList;
  dlm.SubMenuImages := FMapTypeIcons18List.GetImageList;
  TBOpenDirLayer.SubMenuImages := FMapTypeIcons18List.GetImageList;
  TBCopyLinkLayer.SubMenuImages := FMapTypeIcons18List.GetImageList;
  TBLayerInfo.SubMenuImages := FMapTypeIcons18List.GetImageList;

  FNLayerParamsItemList := TGUIDObjectSet.Create(False);
  FNDwnItemList := TGUIDObjectSet.Create(False);
  FNDelItemList := TGUIDObjectSet.Create(False);
  FNOpenDirItemList := TGUIDObjectSet.Create(False);
  FNCopyLinkItemList := TGUIDObjectSet.Create(False);
  FNLayerInfoItemList := TGUIDObjectSet.Create(False);

  FWinPosition := TMainWindowPositionConfig.Create(BoundsRect);
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnWinPositionChange),
    FWinPosition.GetChangeNotifier
  );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnToolbarsLockChange),
    FConfig.ToolbarsLock.GetChangeNotifier
  );

  VMarkEditConfigsListener := TNotifyNoMmgEventListener.Create(Self.OnMarkEditConfigsChange);
  FLinksList.Add(
    VMarkEditConfigsListener,
    FConfig.LayersConfig.CalcLineLayerConfig.CaptionConfig.GetChangeNotifier
  );
  FLinksList.Add(
    VMarkEditConfigsListener,
    FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.GetChangeNotifier
  );
  FLinksList.Add(
    VMarkEditConfigsListener,
    FConfig.LayersConfig.MarkPolygonLayerConfig.CaptionsConfig.GetChangeNotifier
  );

  FLineOnMapByOperation[ao_movemap] := nil;
  FLineOnMapByOperation[ao_edit_point] := nil;
  FLineOnMapByOperation[ao_select_rect] := nil;
  FLineOnMapByOperation[ao_edit_line] := TPathOnMapEdit.Create(GState.VectorGeometryLonLatFactory);
  FLineOnMapByOperation[ao_edit_poly] := TPolygonOnMapEdit.Create(GState.VectorGeometryLonLatFactory);
  FLineOnMapByOperation[ao_calc_line] := TPathOnMapEdit.Create(GState.VectorGeometryLonLatFactory);
  FLineOnMapByOperation[ao_select_poly] := TPolygonOnMapEdit.Create(GState.VectorGeometryLonLatFactory);
  FLineOnMapByOperation[ao_select_line] := TPathOnMapEdit.Create(GState.VectorGeometryLonLatFactory);

  FLineOnMapByOperation[ao_calc_circle] :=
    TCircleOnMapEdit.Create(
      GState.VectorGeometryLonLatFactory,
      GState.GeoCalc
    );

  FPointOnMapEdit := TPointOnMapEdit.Create;

  FStickToGrid :=
    TStickToGrids.Create(
      FActiveProjectionSet,
      FConfig.LayersConfig.MapLayerGridsConfig
    );

  FSelectionRect :=
    TSelectionRect.Create(
      FViewPortState.View,
      FStickToGrid
    );

  VLineOnMapEditChangeListener := TNotifyNoMmgEventListener.Create(Self.OnLineOnMapEditChange);
  FLinksList.Add(
    VLineOnMapEditChangeListener,
    FLineOnMapByOperation[ao_edit_line].GetChangeNotifier
  );
  FLinksList.Add(
    VLineOnMapEditChangeListener,
    FLineOnMapByOperation[ao_edit_poly].GetChangeNotifier
  );
  FLinksList.Add(
    VLineOnMapEditChangeListener,
    FLineOnMapByOperation[ao_calc_line].GetChangeNotifier
  );
  FLinksList.Add(
    VLineOnMapEditChangeListener,
    FLineOnMapByOperation[ao_calc_circle].GetChangeNotifier
  );
  FLinksList.Add(
    VLineOnMapEditChangeListener,
    FLineOnMapByOperation[ao_select_poly].GetChangeNotifier
  );
  FLinksList.Add(
    VLineOnMapEditChangeListener,
    FLineOnMapByOperation[ao_select_line].GetChangeNotifier
  );

  FRuller := TBitmap32.Create;
  VBitmapStatic :=
    ReadBitmapByFileRef(
      GState.ResourceProvider,
      'VRULLER.png',
      GState.ContentTypeManager,
      nil
    );
  if VBitmapStatic <> nil then begin
    AssignStaticToBitmap32(FRuller, VBitmapStatic);
  end;
  FTumbler := TBitmap32.Create;
  VBitmapStatic :=
    ReadBitmapByFileRef(
      GState.ResourceProvider,
      'VTUMBLER.png',
      GState.ContentTypeManager,
      nil
    );
  if VBitmapStatic <> nil then begin
    AssignStaticToBitmap32(FTumbler, VBitmapStatic);
  end;
  FKeyMovingHandler :=
    TKeyMovingHandler.Create(
      FViewPortState,
      GState.Timer,
      GState.GUISyncronizedTimerNotifier,
      FConfig.KeyMovingConfig
    );
  CreateProjectionActions;
  FactlstLanguages :=
    TActionListByLanguageManager.Create(
      Self,
      GState.Config.LanguageManager
    );
  FMarksDbMenu :=
    TMarksDbMenu.Create(
      Self,
      tbxMarksDbList,
      GState.MarkSystemConfig
    );

  FMapHintWindow :=
    TMapHintWindow.Create(
      Self,
      map,
      GState.GeoCalc,
      GState.ProjectedGeometryProvider,
      FGpsTrackGoTo,
      GState.ValueToStringConverter,
      FViewPortState.View
    );

  FTileStorageImporter :=
    TTileStorageImporter.Create(
      FMainMapState.AllMapsSet,
      FConfig.MainMapConfig,
      FConfig.MapLayersConfig,
      GState.ContentTypeManager,
      GState.ArchiveReadWriteFactory
    );
end;

procedure TfrmMain.CreateWnd;
begin
  inherited;
  DragAcceptFiles(Self.Handle, True);
end;

procedure TfrmMain.DestroyWnd;
begin
  DragAcceptFiles(Self.Handle, False);
  inherited;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  VProvider: IConfigDataProvider;
  VSensorViewGenerator: ISensorViewListGenerator;
begin
  Caption := GState.ApplicationCaption;

  TBXSetTheme(CTTBSASThemeName);

  VProvider := FStateConfigDataProvider.GetSubItem('MainForm');
  FWinPosition.ReadConfig(VProvider);

  FPanelPositionSaveLoad := TPanelsPositionsSaveLoad.Create(FStateConfigDataProvider);

  TBEditPath.Floating := True;
  TBEditPath.MoveOnScreen(True);
  TBEditPath.FloatingPosition := Point(Left + map.Left + 30, Top + map.Top + 70);

  TBEditSelectPolylineRadius.OnUpClick := Self.TBEditSelectPolylineRadiusOnUpClick;
  TBEditSelectPolylineRadius.OnDownClick := Self.TBEditSelectPolylineRadiusOnDownClick;

  FSensorList :=
    TSensorListStuped.Create(
      GState.Config.LanguageManager,
      FViewPortState.View,
      FConfig.NavToPoint,
      GState.SystemTime,
      GState.GPSRecorder,
      GState.GpsSystem,
      GState.BatteryStatus
    );

  VSensorViewGenerator :=
    TSensorViewListGeneratorStuped.Create(
      GState.GUISyncronizedTimerNotifier,
      GState.CoordToStringConverter,
      GState.ValueToStringConverter,
      GState.Config.LanguageManager,
      Self,
      TBXDock1,
      NSensors,
      MenusImageList,
      40
    );
  FSensorViewList := VSensorViewGenerator.CreateSensorViewList(FSensorList);
  FPanelPositionSaveLoad.Load(Self);
  OnToolbarsLockChange;
  TBEditPath.Visible := False;
  TrayIcon.Icon.LoadFromResourceName(Hinstance, 'MAINICON');

  FSelectedPolygon := nil;

  FSunCalcProvider :=
    TSunCalcProvider.Create(
      FConfig.LayersConfig.SunCalcConfig
    );

  InitLayers;

  FArgProcessor :=
    TCmdLineArgProcessor.Create(
      GState.MarksDb,
      FMapGoto,
      FActiveProjectionSet,
      FViewPortState,
      FMainMapState.AllMapsSet,
      FConfig,
      GState.VectorGeometryLonLatFactory,
      GState.AppearanceOfMarkFactory,
      GState.ImporterList,
      FTileStorageImporter
    );

  FFavoriteMapSetMenu :=
    TFavoriteMapSetMenu.Create(
      GState.FavoriteMapSetConfig,
      FFavoriteMapSetHelper,
      tbiFavoriteItems
    );

  ProgramStart := True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if tbxpmnSearchResult.Tag <> 0 then begin
    IInterface(tbxpmnSearchResult.Tag)._Release;
    tbxpmnSearchResult.Tag := 0;
  end;
  FSensorViewList := nil;
  FArgProcessor := nil;
end;

function TfrmMain.FindItems(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IVectorItemSubset;

  procedure _AddItems(const AItems: IVectorItemSubset; const ABuilder: IVectorItemSubsetBuilder);
  var
    I: Integer;
    VRect: ILonLatRect;
    VItem: IVectorDataItem;
    VList: IInterfaceListSimple;
    VArea: array of Integer;
  begin
    if AItems = nil then begin
      Exit;
    end;

    if AItems.Count = 1 then begin
      ABuilder.Add(AItems[0]);
      Exit;
    end;

    // sort by area
    VList := TInterfaceListSimple.Create;
    VList.Capacity := AItems.Count;
    SetLength(VArea, AItems.Count);
    for I := 0 to AItems.Count - 1 do begin
      VItem := AItems[I];
      VRect := VItem.Geometry.Bounds;
      VArea[I] := Round((VRect.Right - VRect.Left) * (VRect.Top - VRect.Bottom) * 1e6);
      VList.Add(VItem);
    end;

    StableSortInterfaceListByIntegerMeasure(VList, VArea);

    // add
    for I := 0 to VList.Count - 1 do begin
      ABuilder.Add( IVectorDataItem(VList[I]) );
    end;
  end;

var
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  VSubsetBuilder := GState.VectorItemSubsetBuilderFactory.Build;

  // vector layers
  _AddItems(
    FWikiLayer.FindItems(AVisualConverter, ALocalPoint),
    VSubsetBuilder
  );

  // search results
  _AddItems(
    FLayerSearchResults.FindItems(AVisualConverter, ALocalPoint),
    VSubsetBuilder
  );

  // placemarks
  _AddItems(
    FLayerMapMarks.FindItems(AVisualConverter, ALocalPoint),
    VSubsetBuilder
  );

  Result := VSubsetBuilder.MakeStaticAndClear;
end;

function TfrmMain.FindPathOnMapEdit(
  const AVisualConverter: ILocalCoordConverter;
  const ALocalPoint: TPoint
): IVectorItemSubset;
var
  VRect: TDoubleRect;
  VProjection: IProjection;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  VGeometry: IGeometryLonLatLine;
  VProjectdPath: IGeometryProjectedLine;
  VPathOnMapEdit: IPathOnMapEdit;
  VSubsetBuilder: IVectorItemSubsetBuilder;
  VMainInfo: IVectorDataItemMainInfo;
begin
  Result := nil;
  if Supports(FLineOnMapEdit, IPathOnMapEdit, VPathOnMapEdit) then begin
    if Assigned(VPathOnMapEdit.Path) and IsValidLonLatLine(VPathOnMapEdit.Path.Geometry) then begin
      VRect.Left := ALocalPoint.X - 12;
      VRect.Top := ALocalPoint.Y - 12;
      VRect.Right := ALocalPoint.X + 12;
      VRect.Bottom := ALocalPoint.Y + 12;

      VProjection := AVisualConverter.Projection;
      VMapRect := AVisualConverter.LocalRectFloat2MapRectFloat(VRect);
      VProjection.ValidatePixelRectFloat(VMapRect);
      VLonLatRect := VProjection.PixelRectFloat2LonLatRect(VMapRect);
      VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(ALocalPoint);
      VProjection.ValidatePixelPosFloatStrict(VPixelPos, False);

      VGeometry := VPathOnMapEdit.Path.Geometry;
      if VGeometry.Bounds.IsIntersecWithRect(VLonLatRect) then begin
        VProjectdPath := GState.ProjectedGeometryProvider.GetProjectedPath(VProjection, VGeometry);
        if Assigned(VProjectdPath) then begin
          if VProjectdPath.IsPointOnPath(VPixelPos, 6) then begin
            if FEditMarkLine <> nil then begin
              VMainInfo := FEditMarkLine.MainInfo;
            end else begin
              VMainInfo := GState.VectorDataItemMainInfoFactory.BuildMainInfo(nil, '', '');
            end;
            VSubsetBuilder := GState.VectorItemSubsetBuilderFactory.Build;
            VSubsetBuilder.Add( GState.VectorDataFactory.BuildItem(VMainInfo, nil, VGeometry) );
            Result := VSubsetBuilder.MakeStaticAndClear;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
var
  VMapLayersVsibleChangeListener: IListener;
  VMainFormMainConfigChangeListener: IListener;
  VGPSReceiverStateChangeListener: IListener;
  VTileRectForDownload: ITileRectChangeable;
begin
  if not ProgramStart then begin
    exit;
  end;
  ProgramStart := False;
  try
    FViewPortState.ChangeViewSize(Point(map.Width, map.Height));
    OnWinPositionChange;

    Application.HelpFile := ExtractFilePath(Application.ExeName) + 'help.hlp';
    InitMouseCursors;

    CreateViewTilesGridActions;
    CreateViewTilesGridMenu;

    FShortCutManager :=
      TShortcutManager.Create(
        GState.Bitmap32StaticFactory,
        TBXMainMenu.Items,
        GetIgnoredMenuItemsList
      );
    FShortCutManager.Load(GState.MainConfigProvider.GetSubItem('HOTKEY'));

    actShowDebugInfo.Visible := GState.Config.InternalDebugConfig.IsShowDebugInfo;

    InitGridsMenus;

    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnMapGUIChange),
      GState.MapType.GUIConfigList.GetChangeNotifier
    );
    OnMapGUIChange;


    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnBeforeViewChange),
      FViewPortState.View.BeforeChangeNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnAfterViewChange),
      FViewPortState.View.AfterChangeNotifier
    );

    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.ProcessPosChangeMessage),
      FViewPortState.View.GetChangeNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnMainMapChange),
      FMainMapState.ActiveMap.ChangeNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnActivLayersChange),
      FMainMapState.ActiveLayersSet.ChangeNotifier
    );


    VMapLayersVsibleChangeListener := TNotifyNoMmgEventListener.Create(Self.MapLayersVisibleChange);
    FLinksList.Add(
      VMapLayersVsibleChangeListener,
      FConfig.LayersConfig.StatBar.GetChangeNotifier
    );
    FLinksList.Add(
      VMapLayersVsibleChangeListener,
      FConfig.LayersConfig.SunCalcConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMapLayersVsibleChangeListener,
      FConfig.LayersConfig.MiniMapLayerConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMapLayersVsibleChangeListener,
      FConfig.LayersConfig.ScaleLineConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMapLayersVsibleChangeListener,
      FConfig.DownloadUIConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMapLayersVsibleChangeListener,
      FConfig.LayersConfig.GPSTrackConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMapLayersVsibleChangeListener,
      FConfig.LayersConfig.CenterScaleConfig.ChangeNotifier
    );
    FLinksList.Add(
      VMapLayersVsibleChangeListener,
      FConfig.LayersConfig.FullMapMouseCursorLayerConfig.ChangeNotifier
    );
    FLinksList.Add(
      VMapLayersVsibleChangeListener,
      FConfig.LayersConfig.LastSelectionLayerConfig.ChangeNotifier
    );

    VGPSReceiverStateChangeListener :=
      TNotifyEventListenerSync.Create(
        GState.GUISyncronizedTimerNotifier,
        500,
        Self.GPSReceiverStateChange
      );
    FLinksList.Add(
      VGPSReceiverStateChangeListener,
      GState.GpsSystem.ConnectingNotifier
    );
    FLinksList.Add(
      VGPSReceiverStateChangeListener,
      GState.GpsSystem.DisconnectedNotifier
    );

    FLinksList.Add(
      TNotifyEventListenerSync.Create(GState.GUISyncronizedTimerNotifier, 500, Self.GPSReceiverConnect),
      GState.GpsSystem.ConnectedNotifier
    );
    FLinksList.Add(
      TNotifyEventListenerSync.Create(GState.GUISyncronizedTimerNotifier, 500, Self.GPSReceiverDisconnect),
      GState.GpsSystem.DisconnectedNotifier
    );
    FLinksList.Add(
      TNotifyEventListenerSync.Create(GState.GUISyncronizedTimerNotifier, 500, Self.GPSReceiverConnectError),
      GState.GpsSystem.ConnectErrorNotifier
    );
    FLinksList.Add(
      TNotifyEventListenerSync.Create(GState.GUISyncronizedTimerNotifier, 1000, Self.GPSReceiverTimeout),
      GState.GpsSystem.TimeOutNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.GPSReceiverReceive),
      GState.GpsSystem.DataReciveNotifier
    );

    VMainFormMainConfigChangeListener := TNotifyEventListenerSync.Create(GState.GUISyncronizedTimerNotifier, 500, Self.OnMainFormMainConfigChange);
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      FConfig.MainConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      GState.Config.BitmapPostProcessingConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      FConfig.GPSBehaviour.GetChangeNotifier
    );
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      FConfig.MainGeoCoderConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      GState.Config.ViewConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      FConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      FConfig.LayersConfig.MarksLayerConfig.MarksDrawConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      FConfig.MapZoomingConfig.ChangeNotifier
    );
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      FConfig.MapMovingConfig.ChangeNotifier
    );
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      FConfig.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig.ChangeNotifier
    );

    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnViewProjectionConfigChange),
      FConfig.ViewProjectionConfig.ChangeNotifier
    );

    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnGridGenshtabChange),
      FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.ChangeNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnGridLonLatChange),
      FConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.ChangeNotifier
    );
    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnGridTileChange),
      FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.ChangeNotifier
    );

    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnFillingMapChange),
      FConfig.LayersConfig.FillingMapLayerConfig.GetChangeNotifier
    );

    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnNavToMarkChange),
      FConfig.NavToPoint.ChangeNotifier
    );

    DateTimePicker1.DateTime := FConfig.LayersConfig.FillingMapLayerConfig.FillFirstDay;
    DateTimePicker2.DateTime := FConfig.LayersConfig.FillingMapLayerConfig.FillLastDay;

    FPathProvidersTree := GState.PathDetalizeTree;
    FPathProvidersMenuBuilder := TMenuGeneratorByStaticTreeSimple.Create(Self.TBEditPathMarshClick);
    FPathProvidersConfigMenuBuilder := TMenuGeneratorByStaticTreeSimple.Create(Self.tbxExtendRouteSelect);

    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnPathProvidesChange),
      FPathProvidersTree.ChangeNotifier
    );

    FLinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnGeoCalcConfigChange),
      GState.Config.GeoCalcConfig.ChangeNotifier
    );

    InitSearchers;
    InitMergePolygons;
    InitElevationProfile;
    CreateLangMenu;

    FfrmGoTo :=
      TfrmGoTo.Create(
       GState.Config.LanguageManager,
       FActiveProjectionSet,
       GState.VectorItemSubsetBuilderFactory,
       GState.GeoCodePlacemarkFactory,
       GState.MarksDb.MarkDb,
       Gstate.GeoCoderList,
       FConfig.SearchHistory,
       FConfig.MainGeoCoderConfig,
       FViewPortState.View,
       GState.Config.CoordRepresentationConfig,
       GState.CoordFromStringParser,
       GState.CoordToStringConverter,
       FSearchPresenter
    );
    FfrmGoTo.PopupParent := Self;

    FfrmSettings :=
      TfrmSettings.Create(
        GState.Config.LanguageManager,
        FConfig,
        FMainMapState,
        GState.Config.GeoCoderConfig,
        FSensorList,
        FShortCutManager,
        FMapTypeEditor,
        FFavoriteMapSetHelper,
        FfrmFavoriteMapSetEditor,
        Self.SaveConfig
      );

    FfrmSettings.SetProxy;

    FfrmMarksExplorer :=
      TfrmMarksExplorer.Create(
        False,
        GState.Config.LanguageManager,
        GState.VectorGeometryLonLatFactory,
        FViewPortState.View,
        FConfig.NavToPoint,
        FConfig.MarksExplorerConfig,
        FConfig.LayersConfig.MarksLayerConfig.MarksShowConfig,
        FMergePolygonsPresenter,
        FMarkDBGUI,
        GState.MarksDb.ImplFactoryList,
        GState.MarkSystemConfig,
        FMapGoto,
        FElevationProfilePresenter,
        FFormRegionProcess
      );
    FfrmMarksExplorer.PopupParent := Self;

    FLinksList.ActivateLinks;
    GState.StartThreads;

    VTileRectForDownload :=
      TTileRectChangeableByLocalConverterSimple.Create(
        FViewPortState.View,
        GSync.SyncVariable.Make('TileRectForDownloadMain'),
        GSync.SyncVariable.Make('TileRectForDownloadResult')
      );
    FUIDownload :=
      TUITileDownloadList.Create(
        GState.BGTimerNotifier,
        GState.AppClosingNotifier,
        FConfig.DownloadUIConfig,
        VTileRectForDownload,
        FMainMapState.AllMapsSet,
        FMainMapState.AllActiveMapsSet,
        GState.DownloadInfo,
        GState.GlobalInternetState,
        FTileErrorLogger
      );

    OnMainFormMainConfigChange;
    MapLayersVisibleChange;
    OnFillingMapChange;
    OnMainMapChange;
    OnActivLayersChange;
    ProcessPosChangeMessage;
    OnPathProvidesChange;
    OnNavToMarkChange;
    OnMarkEditConfigsChange;
    OnGeoCalcConfigChange;

    PaintZSlider(FViewPortState.View.GetStatic.Projection.Zoom);
    Application.OnMessage := DoMessageEvent;
    map.OnMouseDown := Self.mapMouseDown;
    map.OnMouseUp := Self.mapMouseUp;
    map.OnMouseMove := Self.mapMouseMove;
    map.OnResize := Self.mapResize;
    TBXMainMenu.ProcessShortCuts := True;

    CreateProjectionMenu;

    {$IF (CompilerVersion >= 23)} // XE2 and UP
    Self.Touch.InteractiveGestures := Self.Touch.InteractiveGestures + [igZoom];
    {$IFEND}

    ProcessCmdLineArgs;

    FStartedNormal := True;
  finally
    map.SetFocus;
  end;
end;

procedure TfrmMain.InitGridsMenus;
begin
  OnGridGenshtabChange;
  OnGridLonLatChange;
  OnGridTileChange;

  actViewGridLonLat_00_500.Caption := FloatToStr(0.5) + '�';
  actViewGridLonLat_00_250.Caption := FloatToStr(0.25) + '�';
  actViewGridLonLat_00_125.Caption := FloatToStr(0.125) + '�';
end;

procedure TfrmMain.InitLayers;
var
  VSunCalcPopupMenu: IPopUp;
  VScaleLinePopupMenu: IPopUp;
  VStatBarPopupMenu: IPopUp;
  VMiniMapPopupMenu: IPopUp;
begin
  VStatBarPopupMenu :=
    TLayerStatBarPopupMenu.Create(
      GState.Config.LanguageManager,
      map,
      FConfig.LayersConfig.StatBar,
      GState.Config.TerrainConfig,
      GState.TerrainProviderList,
      Self.tbitmOnInterfaceOptionsClick
    );

  VScaleLinePopupMenu :=
    TLayerScaleLinePopupMenu.Create(
      map,
      tbxpmnScaleLine
    );

  VMiniMapPopupMenu :=
    TLayerMiniMapPopupMenu.Create(
      map,
      FConfig.LayersConfig.MiniMapLayerConfig.MapConfig,
      FConfig.LayersConfig.MiniMapLayerConfig.LayersConfig,
      FMainMapState.MiniMapMapsSet,
      FMainMapState.MiniMapLayersSet,
      GState.MapType.GUIConfigList,
      FMapTypeIcons18List
    );

  VSunCalcPopupMenu :=
    TLayerSunCalcPopupMenu.Create(
      GState.Config.LanguageManager,
      map,
      FConfig.LayersConfig.SunCalcConfig,
      FSunCalcProvider
    );

  FLayersList :=
    TMainFormLayersList.Create(
      MainFormLayersListParams(
        map,
        GState.AppStartedNotifier,
        GState.AppClosingNotifier,
        GState.ResourceProvider,
        GState.Config.LanguageManager,
        GState.Config,
        GState.ContentTypeManager,
        GState.GUISyncronizedTimerNotifier,
        GState.LastSearchResult,
        GState.LocalConverterFactory,
        GState.VectorItemSubsetBuilderFactory,
        GState.VectorGeometryLonLatFactory,
        GState.VectorGeometryProjectedFactory,
        GState.ProjectedGeometryProvider,
        GState.CoordToStringConverter,
        GState.ValueToStringConverter,
        GState.HashFunction,
        GState.Bitmap32StaticFactory,
        GState.ImageResamplerFactoryList,
        GState.BitmapPostProcessing,
        GState.GpsTrackRecorder,
        GState.GPSRecorder,
        GState.TerrainProviderList,
        GState.LastSelectionInfo,
        GState.DownloadInfo,
        GState.GlobalInternetState,
        GState.MarksDb,
        FConfig.LayersConfig,
        FActiveProjectionSet,
        FViewPortState,
        FState,
        FMouseState,
        FMainMapState,
        FFillingMapPolygon,
        FMergePolygonsResult,
        FLineOnMapByOperation[ao_calc_line] as IPathOnMapEdit,
        FLineOnMapByOperation[ao_calc_circle] as ICircleOnMapEdit,
        FLineOnMapByOperation[ao_edit_line] as IPathOnMapEdit,
        FLineOnMapByOperation[ao_edit_poly] as IPolygonOnMapEdit,
        FLineOnMapByOperation[ao_select_poly] as IPolygonOnMapEdit,
        FLineOnMapByOperation[ao_select_line] as IPathOnMapEdit,
        FSelectionRect,
        FMapGoto,
        FGpsTrackGoTo,
        FConfig.NavToPoint,
        FPointOnMapEdit,
        FTileErrorLogProvider,
        FTileErrorLogger,
        GState.PerfCounterList.CreateAndAddNewSubList('MapLayer'),
        FConfig.LayersConfig.SunCalcConfig,
        FSunCalcProvider,
        VSunCalcPopupMenu,
        VStatBarPopupMenu,
        VScaleLinePopupMenu,
        VMiniMapPopupMenu,
        GState.GeoCalc
      )
    );
  FWikiLayer := FLayersList.WikiLayer;
  FLayerMapMarks := FLayersList.MarksLayer;
  FLayerSearchResults := FLayersList.SearchResultsLayer;
end;

procedure TfrmMain.InitMouseCursors;
begin
  Screen.Cursors[1] := LoadCursor(HInstance, 'SEL');
  Screen.Cursors[2] := LoadCursor(HInstance, 'LEN');
  Screen.Cursors[3] := LoadCursor(HInstance, 'HAND');
  Screen.Cursors[4] := LoadCursor(HInstance, 'SELPOINT');
end;


procedure SubMenuByActionList(AParent: TTBCustomItem; AActionList: TActionList);
  procedure _AddItem(const AAction: TContainedAction);
  var
    VMenuItem: TTBXItem;
  begin
    VMenuItem := TTBXItem.Create(AParent);
    VMenuItem.Action := AAction;
    if AAction.Name <> '' then begin
      VMenuItem.Name := 'tbitm' + AAction.Name;
    end;
    AParent.Add(VMenuItem);
  end;
var
  I: Integer;
begin
  for I := 0 to AActionList.ActionCount - 1 do begin
    _AddItem(AActionList.Actions[i]);
  end;
end;

procedure TfrmMain.InitSearchers;
var
  I: Integer;
  VItem: IGeoCoderListEntity;
  VGeoCoderList: IGeoCoderListStatic;
  VTBEditItem: TTBEditItem;
  VActiveGeoCoder: TGUID;
  VIsActiveGeoCoderFound: Boolean;
begin
  FSearchPresenter :=
    TSearchResultPresenterOnPanel.Create(
      GState.InternalBrowser,
      FMapGoto,
      ScrollBoxSearchWindow,
      tbxpmnSearchResult,
      Self.OnShowSearchResults,
      GState.CoordToStringConverter,
      GState.LastSearchResult
    );

  VGeoCoderList := GState.GeoCoderList;
  VActiveGeoCoder := FConfig.MainGeoCoderConfig.ActiveGeoCoderGUID;
  VIsActiveGeoCoderFound := False;

  for I := 0 to VGeoCoderList.Count - 1 do begin
    VItem := VGeoCoderList.Items[I];

    VTBEditItem := TTBEditItem.Create(Self);
    VTBEditItem.EditCaption := VItem.GetCaption;
    VTBEditItem.Caption := VItem.GetCaption;
    VTBEditItem.EditWidth := 150;
    VTBEditItem.Hint := '';
    VTBEditItem.Tag := NativeInt(VItem);
    VTBEditItem.OnAcceptText := Self.tbiEditSrchAcceptText;

    TBGoTo.Add(VTBEditItem);

    if IsEqualGUID(VActiveGeoCoder, VItem.GUID) then begin
      VIsActiveGeoCoderFound := True;
    end;
  end;

  if not VIsActiveGeoCoderFound then begin
    if VGeoCoderList.Count > 0 then begin
      VActiveGeoCoder := VGeoCoderList.Items[0].GUID;
      FConfig.MainGeoCoderConfig.ActiveGeoCoderGUID := VActiveGeoCoder;
    end else begin
      //ToDo
    end;
  end;

  FSearchToolbarContainer :=
    TSearchToolbarContainer.Create(
      TBXSelectSrchType,
      tbiSearch,
      tbxDoSearch,
      GState.AppClosingNotifier,
      VGeoCoderList,
      FConfig.MainGeoCoderConfig,
      FConfig.SearchHistory,
      FViewPortState.View,
      FSearchPresenter
    );
end;

procedure TfrmMain.InitUrlProviders;
begin
  FUrlProviderOSM := TUrlByCoordProviderOSM.Create(GState.ProjectionSetFactory);
  FUrlProviderBing := TUrlByCoordProviderBing.Create(GState.ProjectionSetFactory);
  FUrlProviderGoogle := TUrlByCoordProviderGoogle.Create(GState.ProjectionSetFactory);
  FUrlProviderGoogleEarthWeb := TUrlByCoordProviderGoogleEarthWeb.Create(GState.ProjectionSetFactory);
  FUrlProviderYandex := TUrlByCoordProviderYandex.Create(GState.ProjectionSetFactory);
  FUrlProviderNakarte := TUrlByCoordProviderNakarte.Create(GState.ProjectionSetFactory);
  FUrlProviderKosmosnimki := TUrlByCoordProviderKosmosnimki.Create(GState.ProjectionSetFactory);

  FUrlProviderGTopo30 := TUrlByCoordProviderGTopo30.Create;
  FUrlProviderSTRM3 := TUrlByCoordProviderSTRM3.Create;
  FUrlProviderWeatherUnderground := TUrlByCoordProviderWeatherUnderground.Create;
  FUrlProviderYandexWeather := TUrlByCoordProviderYandexWeather.Create;
end;

procedure TfrmMain.InitMergePolygons;
begin
  FMergePolygonsPresenter :=
    TMergePolygonsPresenterOnPanel.Create(
      tbMergePolygons,
      Self.OnShowMergePolygons,
      GState.Config.LanguageManager,
      GState.AppClosingNotifier,
      GState.VectorDataFactory,
      GState.VectorGeometryLonLatFactory,
      FMergePolygonsResult,
      FMapGoto,
      FRegionProcess,
      FMarkDBGUI
    );
  mmoMergePolyHint.Text := _('Press Ctrl and click on polygon to add one...');
end;

procedure TfrmMain.InitElevationProfile;
begin
  FElevationMetaWriter :=
    TElevationMetaWriter.Create(
      GState.Config.LanguageManager,
      GState.AppClosingNotifier,
      GState.Config.TerrainConfig,
      GState.TerrainProviderList,
      GState.VectorGeometryLonLatFactory,
      GState.GeoCalc.GpsDatum
    );

  FElevationProfilePresenter :=
    TElevationProfilePresenterOnPanel.Create(
      tbElevationProfile,
      tbxElevationProfileShow,
      GState.Config.ElevationProfileConfig,
      GState.Config.TerrainConfig,
      GState.Config.LanguageManager,
      GState.GeoCalc,
      FGpsTrackGoTo,
      FElevationMetaWriter
    );
end;

procedure TfrmMain.CreateProjectionActions;
  procedure _AddItem(const ACaption: string; const ATag: Integer);
  var
    VAction: TAction;
  begin
    VAction := TAction.Create(Self);
    VAction.Caption := ACaption;
    VAction.Tag := ATag;
    VAction.OnExecute := Self.actConfigProjectionUseExecute;
    VAction.ActionList := FactlstProjections;
  end;
var
  I: Integer;
  VProjList: IProjectionSetList;
begin
  FactlstProjections := TActionList.Create(Self);
  VProjList := GState.ProjectionSetList;
  Assert(VProjList <> nil);

  for I := 0 to VProjList.Count - 1 do begin
    _AddItem(VProjList.Captions[I], I);
  end;
end;

procedure TfrmMain.CreateProjectionMenu;
begin
  SubMenuByActionList(tbiProjections, FactlstProjections);
  OnViewProjectionConfigChange;
end;

procedure TfrmMain.CreateLangMenu;
begin
  SubMenuByActionList(TBLang, FactlstLanguages);
end;

procedure TfrmMain.CreateMapUIFillingList;
var
  VGenerator: TMapMenuGeneratorBasic;
begin
  VGenerator := TMapMenuGeneratorBasic.Create(
    GState.MapType.GUIConfigList,
    FMainMapState.AllMapsSet,
    FConfig.LayersConfig.FillingMapLayerConfig.SourceMap,
    nil,
    tbiFillingMapMaps,
    FMapTypeIcons18List
  );
  try
    VGenerator.BuildControls;
  finally
    FreeAndNil(VGenerator);
  end;
end;

procedure TfrmMain.CreateMapUILayersList;
var
  VGenerator: TMapMenuGeneratorBasic;
  VLayersSet: IMapTypeSet;
begin
  VLayersSet := FMainMapState.LayersSet;
  if Assigned(VLayersSet) then begin
    VGenerator := TMapMenuGeneratorBasic.Create(
      GState.MapType.GUIConfigList,
      VLayersSet,
      nil,
      FConfig.MapLayersConfig,
      tbiLayersList,
      FMapTypeIcons18List
    );
    try
      VGenerator.BuildControls;
    finally
      FreeAndNil(VGenerator);
    end;
    btnHideAll.Visible := True;
    HideSeparator.Visible := True;
  end else begin
    btnHideAll.Visible := False;
    HideSeparator.Visible := False;
  end;
end;

procedure TfrmMain.CreateMapUILayerSubMenu;
var
  i: integer;
  VMapType: IMapType;

  NLayerParamsItem: TTBXItem; //����� ������� ���� ���������/��������� ����
  NDwnItem: TTBXItem; //����� ������������ ���� ��������� ���� ����
  NDelItem: TTBXItem; //����� ������������ ���� ������� ���� ����
  NOpenDirItem: TTBXItem;
  NCopyLinkItem: TTBXItem;
  NLayerInfoItem: TTBXItem;

  VIcon18Index: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  ldm.Clear;
  dlm.Clear;
  TBOpenDirLayer.Clear;
  NLayerParams.Clear;
  TBCopyLinkLayer.Clear;
  TBLayerInfo.Clear;

  FNLayerParamsItemList.Clear;
  FNLayerInfoItemList.Clear;
  FNDwnItemList.Clear;
  FNDelItemList.Clear;
  FNOpenDirItemList.Clear;
  FNCopyLinkItemList.Clear;

  VGUIDList := GState.MapType.GUIConfigList.OrderedMapGUIDList;
  for i := 0 to VGUIDList.Count - 1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := GState.MapType.FullMapsSet.GetMapTypeByGUID(VGUID);
    VIcon18Index := FMapTypeIcons18List.GetIconIndexByGUID(VGUID);
    if VMapType.Zmp.IsLayer then begin
      NDwnItem := tTBXItem.Create(ldm);
      FNDwnItemList.Add(VGUID, NDwnItem);
      NDwnItem.Caption := VMapType.GUIConfig.Name.Value;
      NDwnItem.ImageIndex := VIcon18Index;
      NDwnItem.OnClick := tbitmDownloadMainMapTileClick;
      NDwnItem.Tag := NativeInt(VMapType);
      ldm.Add(NDwnItem);

      NDelItem := tTBXItem.Create(dlm);
      FNDelItemList.Add(VGUID, NDelItem);
      NDelItem.Caption := VMapType.GUIConfig.Name.Value;
      NDelItem.ImageIndex := VIcon18Index;
      NDelItem.OnClick := NDelClick;
      NDelItem.Tag := NativeInt(VMapType);
      dlm.Add(NDelItem);

      NOpenDirItem := tTBXItem.Create(TBOpenDirLayer);
      FNOpenDirItemList.Add(VGUID, NOpenDirItem);
      NOpenDirItem.Caption := VMapType.GUIConfig.Name.Value;
      NOpenDirItem.ImageIndex := VIcon18Index;
      NOpenDirItem.OnClick := tbitmOpenFolderMainMapTileClick;
      NOpenDirItem.Tag := NativeInt(VMapType);
      TBOpenDirLayer.Add(NOpenDirItem);

      NCopyLinkItem := tTBXItem.Create(TBCopyLinkLayer);
      FNCopyLinkItemList.Add(VGUID, NCopyLinkItem);
      NCopyLinkItem.Caption := VMapType.GUIConfig.Name.Value;
      NCopyLinkItem.ImageIndex := VIcon18Index;
      NCopyLinkItem.OnClick := tbitmCopyToClipboardMainMapUrlClick;
      NCopyLinkItem.Tag := NativeInt(VMapType);
      TBCopyLinkLayer.Add(NCopyLinkItem);

      NLayerParamsItem := tTBXItem.Create(NLayerParams);
      FNLayerParamsItemList.Add(VGUID, NLayerParamsItem);
      NLayerParamsItem.Caption := VMapType.GUIConfig.Name.Value;
      NLayerParamsItem.ImageIndex := VIcon18Index;
      NLayerParamsItem.OnClick := actMapsEditMapParamsExecute;
      NLayerParamsItem.Tag := NativeInt(VMapType);
      NLayerParams.Add(NLayerParamsItem);

      NLayerInfoItem := tTBXItem.Create(TBLayerInfo);
      FNLayerInfoItemList.Add(VGUID, NLayerInfoItem);
      NLayerInfoItem.Caption := VMapType.GUIConfig.Name.Value;
      NLayerInfoItem.ImageIndex := VIcon18Index;
      NLayerInfoItem.OnClick := NMapInfoClick;
      NLayerInfoItem.Tag := NativeInt(VMapType);
      TBLayerInfo.Add(NLayerInfoItem);
    end;
  end;
end;

procedure TfrmMain.CreateMapUIMapsList;
var
  VGenerator: TMapMenuGeneratorBasic;
begin
  VGenerator := TMapMenuGeneratorBasic.Create(
    GState.MapType.GUIConfigList,
    FMainMapState.MapsSet,
    FConfig.MainMapConfig,
    nil,
    TBSMB,
    FMapTypeIcons18List
  );
  try
    VGenerator.BuildControls;
  finally
    FreeAndNil(VGenerator);
  end;
end;

function TfrmMain.GetIgnoredMenuItemsList: TList;
begin
  Result := TList.Create;
  Result.Add(NSMB);
  Result.Add(tbiLayersList);
  Result.Add(tbiFillingMapMaps);
  Result.Add(NLayerParams);
  Result.Add(TBLang);
  if not GState.Config.InternalDebugConfig.IsShowDebugInfo then begin
    Result.Add(tbitmShowDebugInfo);
  end;
end;

procedure TfrmMain.CreateViewTilesGridActions;
var
  I: Integer;
  VAction: TAction;
  VCaption: string;
  VName: TComponentName;
begin
  FactlstTileGrids := TActionList.Create(Self);
  for I := 0 to 30 do begin
    VAction := TAction.Create(Self);
    if I = 0 then begin
      VCaption := _('No');
      VName := 'TileGridNo'; // do not Localize
    end else if (I >= 1) and (I <= 24) then begin
      VCaption := 'z' + IntToStr(I);
      VName := 'TileGrid' + VCaption;
    end else begin
      VCaption := '+' + IntToStr(I - 24 - 1);
      VName := 'TileGrid' + IntToStr(I - 24 - 1) + 'p';
    end;
    VAction.Caption := VCaption;
    VAction.Checked := False;
    VAction.Visible := False;
    VAction.Enabled := True;
    VAction.Tag := I + 100;
    VAction.Name := VName;
    VAction.OnExecute := Self.actViewTilesGridExecute;
    VAction.ActionList := FactlstTileGrids;
  end;
end;

procedure TfrmMain.CreateViewTilesGridMenu;
begin
  SubMenuByActionList(NShowGran, FactlstTileGrids);
end;

procedure TfrmMain.FormClose(
  Sender: TObject;
  var Action: TCloseAction
);
var
  i: integer;
begin
  map.OnResize := nil;
  map.OnMouseDown := nil;
  map.OnMouseUp := nil;
  map.OnMouseMove := nil;
  map.OnDblClick := nil;
  map.OnMouseLeave := nil;

  if Assigned(FFillingMapPolygon.Polygon) then begin
    FConfig.LayersConfig.FillingMapLayerConfig.Visible := False;
  end;

  FLinksList.DeactivateLinks;
  GState.SendTerminateToThreads;
  for i := 0 to Screen.FormCount - 1 do begin
    if (Screen.Forms[i] <> Application.MainForm) and (Screen.Forms[i].Visible) then begin
      Screen.Forms[i].Close;
    end;
  end;
  Application.ProcessMessages;
  if FStartedNormal then begin
    SaveConfig(nil);
  end;
  Application.ProcessMessages;
  FWikiLayer := nil;
  FLayerMapMarks := nil;
  FLayerSearchResults := nil;
  FLayersList := nil;

  FreeAndNil(FShortCutManager);
end;

destructor TfrmMain.Destroy;
begin
  FMarkOnMapEditProvider.SetEnabled(False);
  FSearchToolbarContainer.Free;
  FSearchPresenter := nil;
  FPlacemarkPlayerPlugin := nil;
  FLineOnMapEdit := nil;
  FWinPosition := nil;
  FMergePolygonsPresenter := nil;
  FNLayerParamsItemList := nil;
  FNLayerInfoItemList := nil;
  FNDwnItemList := nil;
  FNDelItemList := nil;
  FNOpenDirItemList := nil;
  FNCopyLinkItemList := nil;
  FLinksList := nil;
  FRegionProcess := nil;
  FreeAndNil(FMapHintWindow);
  FreeAndNil(FfrmAbout);
  FreeAndNil(FfrmMarkPictureConfig);
  FreeAndNil(FTumbler);
  FreeAndNil(FRuller);
  FreeAndNil(FfrmGoTo);
  FreeAndNil(FfrmSettings);
  FreeAndNil(FfrmMapLayersOptions);
  FreeAndNil(FfrmCacheManager);
  FreeAndNil(FfrmMarksExplorer);
  FreeAndNil(FFormRegionProcess);
  FreeAndNil(FfrmPointProjecting);
  FreeAndNil(FMarkDBGUI);
  FreeAndNil(FfrmUpdateChecker);
  FreeAndNil(FfrmPascalScriptIDE);
  FreeAndNil(FFavoriteMapSetMenu);
  FreeAndNil(FfrmFavoriteMapSetManager);
  FreeAndNil(FfrmFavoriteMapSetEditor);
  FreeAndNil(FTileStorageImporter);
  inherited;
end;

procedure TfrmMain.MapLayersVisibleChange;
var
  VIsVisible: Boolean;
  VProvider: TSunCalcDataProviderType;
  VUseDownload: TTileSource;
begin
  VIsVisible := FConfig.LayersConfig.SunCalcConfig.Visible;
  VProvider := FConfig.LayersConfig.SunCalcConfig.DataProviderType;
  actViewSunCalc.Checked := VIsVisible and (VProvider = scdpSun);
  actViewMoonCalc.Checked := VIsVisible and (VProvider = scdpMoon);
  if VIsVisible then begin
    //ToDo: Change TopMargin in LicenseLayer (TWindowLayerLicenseList)
  end;
  actViewFullMapMouseCursorVisible.Checked := FConfig.LayersConfig.FullMapMouseCursorLayerConfig.Enabled;
  actConfigStatusBarVisible.Checked := FConfig.LayersConfig.StatBar.Visible;
  if actConfigStatusBarVisible.Checked then begin
    FConfig.LayersConfig.ScaleLineConfig.BottomMargin := FConfig.LayersConfig.StatBar.Height;
    FConfig.LayersConfig.MiniMapLayerConfig.LocationConfig.BottomMargin := FConfig.LayersConfig.StatBar.Height;
  end else begin
    FConfig.LayersConfig.ScaleLineConfig.BottomMargin := 0;
    FConfig.LayersConfig.MiniMapLayerConfig.LocationConfig.BottomMargin := 0;
  end;
  actConfigMiniMapVisible.Checked := FConfig.LayersConfig.MiniMapLayerConfig.LocationConfig.Visible;
  actConfigScaleLineVisible.Checked := FConfig.LayersConfig.ScaleLineConfig.Visible;
  actConfigScaleLineExtended.Checked := FConfig.LayersConfig.ScaleLineConfig.Extended;
  case FConfig.LayersConfig.ScaleLineConfig.NumbersFormat of
    slnfNice: begin
      actConfigScaleLineNumberFormatNice.Checked := True;
    end;
    slnfScienceRound: begin
      actConfigScaleLineNumberFormatRound.Checked := True;
    end;
    slnfScience: begin
      actConfigScaleLineNumberFormatScience.Checked := True;
    end;
  end;
  actConfigPreviousSelectionVisible.Checked := FConfig.LayersConfig.LastSelectionLayerConfig.Visible;
  actConfigAzimuthCircle.Checked := FConfig.LayersConfig.CenterScaleConfig.Visible;

  actConfigGpsShowTrack.Checked := FConfig.LayersConfig.GPSTrackConfig.Visible;
  VUseDownload := FConfig.DownloadUIConfig.UseDownload;
  TBSrc.ImageIndex := integer(VUseDownload);
  case VUseDownload of
    tsInternet: begin
      actConfigDownloadModeInternet.Checked := True;
      actConfigDownloadModeCache.Checked := False;
      actConfigDownloadModeCacheInternet.Checked := False;
    end;
    tsCache: begin
      actConfigDownloadModeInternet.Checked := False;
      actConfigDownloadModeCache.Checked := True;
      actConfigDownloadModeCacheInternet.Checked := False;
    end;
    tsCacheInternet: begin
      actConfigDownloadModeInternet.Checked := False;
      actConfigDownloadModeCache.Checked := False;
      actConfigDownloadModeCacheInternet.Checked := True;
    end;
  end;

  mapResize(nil);
end;

procedure TfrmMain.ProcessPosChangeMessage;
var
  VProjection: IProjection;
  VZoomCurr: Byte;
  VGPSLonLat: TDoublePoint;
  VGPSMapPoint: TDoublePoint;
  VCenterMapPoint: TDoublePoint;
  VConverter: ILocalCoordConverter;
  VPosition: IGPSPosition;
begin
  VConverter := FViewPortState.View.GetStatic;
  VProjection := VConverter.Projection;

  VPosition := GState.GPSRecorder.CurrentPosition;
  if not VPosition.PositionOK then begin
    // no position
    FCenterToGPSDelta := CEmptyDoublePoint;
  end else begin
    // ok
    VGPSLonLat := VPosition.LonLat;
    VProjection.ProjectionType.ValidateLonLatPos(VGPSLonLat);
    VGPSMapPoint := VProjection.LonLat2PixelPosFloat(VGPSLonLat);

    VCenterMapPoint := VConverter.GetCenterMapPixelFloat;
    FCenterToGPSDelta.X := VGPSMapPoint.X - VCenterMapPoint.X;
    FCenterToGPSDelta.Y := VGPSMapPoint.Y - VCenterMapPoint.Y;
  end;

  VZoomCurr := VProjection.Zoom;
  PaintZSlider(VZoomCurr);
  labZoom.caption := 'z' + IntToStr(VZoomCurr + 1);
end;

procedure TfrmMain.OnStateChange;
var
  VNewState: TStateEnum;
  VIsMarkEdit: Boolean;
  VIsRoutingVisible: Boolean;
begin
  VNewState := FState.State;

  if VNewState = FStateValueOld then begin
    // ignore notifications from IsMapMoving value chanage
    Exit;
  end;

  FStateValueOld := VNewState;

  if VNewState <> ao_select_rect then begin
    FSelectionRect.Reset;
  end;
  FRouteComment := '';

  actMoveMap.Checked := VNewState = ao_movemap;
  actDistanceCalculation.Checked := VNewState = ao_calc_line;
  actCircleCalculation.Checked := VNewState = ao_calc_circle;
  actSelectByRect.Checked := VNewState = ao_select_rect;
  actSelectByPolygon.Checked := VNewState = ao_select_poly;
  actSelectByLine.Checked := VNewState = ao_select_line;

  actMarksAddPoint.Checked := VNewState = ao_edit_point;
  actMarksAddLine.Checked := VNewState = ao_edit_line;
  actMarksAddPolygon.Checked := VNewState = ao_edit_poly;
  TBEditPath.Visible := False;

  VIsMarkEdit :=
    ((VNewState = ao_edit_point) and (FEditMarkPoint <> nil)) or
    ((VNewState = ao_edit_line) and (FEditMarkLine <> nil)) or
    ((VNewState = ao_edit_poly) and (FEditMarkPoly <> nil));

  if VIsMarkEdit then begin
    tbitmSaveMark.Action := actMarkSave;
    tbitmSaveMark.Hint := _('Save (Enter)');
  end else begin
    tbitmSaveMark.Action := actMarkSaveAsNew;
    tbitmSaveMark.Hint := _('Save as... (Enter)');
  end;
  tbitmSaveMark.DropdownCombo := VIsMarkEdit;
  tbitmSaveMarkAsNew.Visible := VIsMarkEdit;

  tbitmSaveMark.Visible :=
    (VNewState = ao_calc_line) or
    (VNewState = ao_calc_circle) or
    (VNewState = ao_edit_line) or
    (VNewState = ao_edit_poly);

  tbxtmSaveMarkAsSeparateSegment.Visible := VIsMarkEdit;
  tbxtmSaveMarkAsSeparateSegment.Enabled :=
    ((VNewState = ao_edit_line) and (FEditMarkLine <> nil) and Supports(FEditMarkLine.Geometry, IGeometryLonLatMultiLine)) or
    ((VNewState = ao_edit_poly) and (FEditMarkPoly <> nil) and Supports(FEditMarkPoly.Geometry, IGeometryLonLatMultiPolygon));

  actSelectByGeometryFinish.Visible :=
    (VNewState = ao_select_poly) or
    (VNewState = ao_select_line);

  tbxCalcLineLabelVisible.Visible := (VNewState = ao_calc_line);
  tbxEditPathLabelVisible.Visible := (VNewState = ao_edit_line);
  tbxEditPolygonLabelVisible.Visible := (VNewState = ao_edit_poly);

  actLineEditSplitTogle.Visible := (VNewState = ao_calc_line) or (VNewState = ao_edit_line);

  tbxEditPathTools.Visible := (VNewState = ao_calc_line) or (VNewState = ao_edit_line);

  VIsRoutingVisible :=
    (VNewState = ao_edit_line) and
    Assigned(FPathProvidersTreeStatic) and
    (FPathProvidersTreeStatic.SubItemCount > 0);
  TBEditPathMarsh.Visible := VIsRoutingVisible;
  tbxExtendRoute.Visible := VIsRoutingVisible;
  actEditPathRouteCalcUndo.Visible := VIsRoutingVisible;
  actEditPathRouteCalcUndo.Enabled := False;
  TBXSeparatorItem24.Visible := VIsRoutingVisible;
  TBXSeparatorItem25.Visible := VIsRoutingVisible;
  FRouteUndoPath := nil;

  TBEditSelectPolylineRadius.Visible := (VNewState = ao_select_line) or (VNewState = ao_calc_circle);
  TBEditSelectPolylineRadiusCap1.Visible := TBEditSelectPolylineRadius.Visible;
  TBEditSelectPolylineRadiusCap2.Visible := TBEditSelectPolylineRadius.Visible;

  if FLineOnMapEdit <> nil then begin
    FLineOnMapEdit.Clear;
  end;

  if VNewState <> ao_edit_point then begin
    FPointOnMapEdit.Clear;
  end;

  FLineOnMapEdit := FLineOnMapByOperation[VNewState];
  if VNewState = ao_select_line then begin
    TBEditSelectPolylineRadius.MinValue := 1;
    TBEditSelectPolylineRadius.MaxValue := 100000;
    TBEditSelectPolylineRadius.Value := Round(FConfig.LayersConfig.SelectionPolylineLayerConfig.ShadowConfig.Radius);
  end else
  if VNewState = ao_calc_circle then begin
    TBEditSelectPolylineRadius.MinValue := 0;
    TBEditSelectPolylineRadius.MaxValue := MaxInt;
    TBEditSelectPolylineRadius.Value := Round((FLineOnMapEdit as ICircleOnMapEdit).Radius);
  end;

  if Assigned(FLineOnMapEdit) then begin
    actLineEditSplitTogle.Checked := FLineOnMapEdit.IsNearSplit;
  end;

  case VNewState of
    ao_movemap: begin
      map.Cursor := crDefault;
    end;
    ao_calc_line, ao_calc_circle: begin
      map.Cursor := 2;
    end;
    ao_select_poly, ao_select_rect, ao_select_line: begin
      map.Cursor := crDrag;
    end;
    ao_edit_point, ao_edit_line, ao_edit_poly: begin
      map.Cursor := 4;
    end;
  end;
  if VNewState <> ao_edit_line then begin
    FEditMarkLine := nil;
  end;
  if VNewState <> ao_edit_poly then begin
    FEditMarkPoly := nil;
  end;
  if VNewState <> ao_edit_point then begin
    FEditMarkPoint := nil;
  end;
end;

procedure TfrmMain.OnActivLayersChange;
var
  VLayerSet: IMapTypeSet;
begin
  VLayerSet := FMainMapState.ActiveLayersSet.GetStatic;
  TBLayerSel.Checked := Assigned(VLayerSet) and (VLayerSet.Count > 0);
end;

procedure TfrmMain.OnAfterViewChange;
begin
  map.ForceFullInvalidate;
  map.Changed;
  map.EndUpdate;
  map.Repaint;
end;

procedure TfrmMain.OnBeforeViewChange;
begin
  FMapHintWindow.HideHint;
  map.BeginUpdate;
end;

procedure TfrmMain.OnFillingMapChange;
var
  VConfig: IFillingMapLayerConfigStatic;
  VFillMode: TFillMode;
var
  VZoom: Byte;
  VSelectedCell: TPoint;
  VFilterMode: Boolean;
begin
  VConfig := FConfig.LayersConfig.FillingMapLayerConfig.GetStatic;
  VZoom := VConfig.Zoom;
  VFillMode := VConfig.FillMode;
  VFilterMode := VConfig.FilterMode;

  if VConfig.Visible then begin
    if VConfig.UseRelativeZoom then begin
      TBMapZap.Caption := '+' + IntToStr(VZoom);
      VSelectedCell.X := (VZoom + 25) mod 5;
      VSelectedCell.Y := (VZoom + 25) div 5;
    end else begin
      TBMapZap.Caption := 'z' + IntToStr(VZoom + 1);
      VSelectedCell.X := (VZoom + 1) mod 5;
      VSelectedCell.Y := (VZoom + 1) div 5;
    end;
  end else begin
    TBMapZap.Caption := '';
    VSelectedCell := Point(0, 0);
  end;
  tbtpltCachedTilesMap.SelectedCell := VSelectedCell;

  if (VFillMode = fmUnexisting) then begin
    actViewFillingMapMarkUnexisting.Checked := True;
  end else if (VFillMode = fmExisting) then begin
    actViewFillingMapMarkExisting.Checked := True;
  end else if (VFillMode = fmGradient) then begin
    actViewFillingMapMarkGradient.Checked := True;
  end;

  NShowFillDates.Checked := VFilterMode;
  DateTimePicker1.DateTime := VConfig.FillFirstDay;
  DateTimePicker2.DateTime := VConfig.FillLastDay;

  FillDates.Visible := VFilterMode;
  actViewFillingMapMainMapUse.Checked := IsEqualGUID(VConfig.SelectedMap, CGUID_Zero);
end;

procedure TfrmMain.OnGeoCalcConfigChange;
var
  VMapType: IMapType;
begin
  case GState.Config.GeoCalcConfig.DatumSource of
    TGeoCalcDatumSource.dsWGS84: begin
      GState.GeoCalc.Datum := GState.GeoCalc.GpsDatum;
      actGeoCalcUseGpsDatum.Checked := True;
    end;
    TGeoCalcDatumSource.dsZMP: begin
      VMapType := FMainMapState.ActiveMap.GetStatic;
      GState.GeoCalc.Datum := VMapType.ProjectionSet.Zooms[0].ProjectionType.Datum;
      actGeoCalcUseZmpDatum.Checked := True;
    end;
  else
    Assert(False);
  end;
end;

procedure TfrmMain.OnGridGenshtabChange;
var
  VScale: Integer;
begin
  VScale := FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Scale;
  if FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Visible then begin
    if VScale = 1000000 then begin
      actViewGridGenShtab_1_000_000.Checked := True;
    end else if VScale = 500000 then begin
      actViewGridGenShtab_500_000.Checked := True;
    end else if VScale = 200000 then begin
      actViewGridGenShtab_200_000.Checked := True;
    end else if VScale = 100000 then begin
      actViewGridGenShtab_100_000.Checked := True;
    end else if VScale = 50000 then begin
      actViewGridGenShtab_50_000.Checked := True;
    end else if VScale = 25000 then begin
      actViewGridGenShtab_25_000.Checked := True;
    end else if VScale = 10000 then begin
      actViewGridGenShtab_10_000.Checked := True;
    end else if VScale = 5000 then begin
      actViewGridGenShtab_5_000.Checked := True;
    end else if VScale = 2500 then begin
      actViewGridGenShtab_2_500.Checked := True;
    end else if VScale = 0 then begin
      actViewGridGenShtabNo.Checked := True;
    end else begin
      actViewGridGenShtabAuto.Checked := True;
      if VScale > 0 then begin
        FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Scale := -1;
      end;
    end;
  end else begin
    actViewGridGenShtabNo.Checked := True;
  end;
end;

procedure TfrmMain.OnGridLonLatChange;
var
  VDegScale: Integer;
begin
  VDegScale := Trunc(FConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.Scale);
  if FConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.Visible then begin
    if VDegScale = 12500000 then begin
      actViewGridLonLat_00_125.Checked := True;
    end else if VDegScale = 25000000 then begin
      actViewGridLonLat_00_250.Checked := True;
    end else if VDegScale = 50000000 then begin
      actViewGridLonLat_00_500.Checked := True;
    end else if VDegScale = 100000000 then begin
      actViewGridLonLat_01_000.Checked := True;
    end else if VDegScale = 200000000 then begin
      actViewGridLonLat_02_000.Checked := True;
    end else if VDegScale = 500000000 then begin
      actViewGridLonLat_05_000.Checked := True;
    end else if VDegScale = 1000000000 then begin
      actViewGridLonLat_10_000.Checked := True;
    end else if VDegScale = 0 then begin
      actViewGridLonLatNo.Checked := True;
    end else if VDegScale < 0 then begin
      actViewGridLonLatAuto.Checked := True;
    end else begin
      actViewGridLonLat_User.Checked := True;
      actViewGridLonLat_User.Tag := VDegScale;
    end;
  end else begin
    actViewGridLonLatNo.Checked := True;
  end;
  NDegValue.text := Deg2StrValue(actViewGridLonLat_User.Tag);
end;

procedure TfrmMain.OnGridTileChange;
var
  VGridVisible: Boolean;
  VRelativeZoom: Boolean;
  VGridZoom: Integer;
  VSelectedCell: TPoint;
begin
  FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.LockRead;
  try
    VGridVisible := FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.Visible;
    VRelativeZoom := FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.UseRelativeZoom;
    VGridZoom := FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.Zoom;
  finally
    FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.UnlockRead;
  end;

  if VGridVisible then begin
    if VRelativeZoom then begin
      VSelectedCell.X := (VGridZoom + 25) mod 5;
      VSelectedCell.Y := (VGridZoom + 25) div 5;
    end else begin
      VSelectedCell.X := (VGridZoom + 1) mod 5;
      VSelectedCell.Y := (VGridZoom + 1) div 5;
    end;
  end else begin
    VSelectedCell := Point(0, 0);
  end;
  tbtpltViewGridTile.SelectedCell := VSelectedCell;
end;

procedure TfrmMain.OnLineOnMapEditChange;
var
  VLineOnMapEdit: ILineOnMapEdit;
  VPathOnMapEdit: IPathOnMapEdit;
  VPolygonOnMapEdit: IPolygonOnMapEdit;
  VCircleOnMapEdit: ICircleOnMapEdit;
  VSaveAvailable: Boolean;
  VPath: IGeometryLonLatLine;
  VPoly: IGeometryLonLatPolygon;
  VIsMultiItem: Boolean;
begin
  VLineOnMapEdit := FLineOnMapEdit;
  if VLineOnMapEdit <> nil then begin
    VSaveAvailable := False;
    VIsMultiItem := False;
    if Supports(VLineOnMapEdit, ICircleOnMapEdit, VCircleOnMapEdit) then begin
      if TBEditSelectPolylineRadius.Value <> Round(VCircleOnMapEdit.Radius) then begin
        TBEditSelectPolylineRadius.Value := Round(VCircleOnMapEdit.Radius);
      end;
      if Assigned(VCircleOnMapEdit.Path) then begin
        VPath := VCircleOnMapEdit.Path.Geometry;
        VSaveAvailable := IsValidLonLatLine(VPath);
      end;
    end else if Supports(VLineOnMapEdit, IPathOnMapEdit, VPathOnMapEdit) then begin
      if Assigned(VPathOnMapEdit.Path) then begin
        VPath := VPathOnMapEdit.Path.Geometry;
        VSaveAvailable := IsValidLonLatLine(VPath);
        VIsMultiItem := Supports(VPath, IGeometryLonLatMultiLine);
      end;
    end else if Supports(VLineOnMapEdit, IPolygonOnMapEdit, VPolygonOnMapEdit) then begin
      if Assigned(VPolygonOnMapEdit.Polygon) then begin
        VPoly := VPolygonOnMapEdit.Polygon.Geometry;
        VSaveAvailable := IsValidLonLatPolygon(VPoly);
        VIsMultiItem := Supports(VPoly, IGeometryLonLatMultiPolygon);
      end;
    end;
    TBEditPath.Visible := VSaveAvailable;
    tbxtmSaveMarkAsSeparateSegment.Enabled := VIsMultiItem;
    if Assigned(FLineOnMapEdit) then begin
      actLineEditSplitTogle.Checked := FLineOnMapEdit.IsNearSplit;
    end;
  end;
end;

procedure TfrmMain.OnMainFormMainConfigChange;
begin
  actConfigUsePrevForMap.Checked := FConfig.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig.UsePrevZoomAtMap;
  actConfigUsePrevForLayers.Checked := FConfig.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig.UsePrevZoomAtLayer;
  actConfigUsePrevForVectorLayers.Checked := FConfig.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig.UsePrevZoomAtVectorLayer;
  map.Color := GState.Config.ViewConfig.BackGroundColor;

  actConfigZoomToCursor.Checked := FConfig.MapZoomingConfig.ZoomingAtMousePos;
  actConfigUseZoomAnimation.Checked := FConfig.MapZoomingConfig.AnimateZoom;
  actConfigUseInertialMovement.Checked := FConfig.MapMovingConfig.AnimateMove;
  actConfigColorInversion.Checked := GState.Config.BitmapPostProcessingConfig.InvertColor;
  actConfigGpsFollowPosition.Checked := FConfig.GPSBehaviour.MapMove;
  actConfigGpsFollowPositionAtCenter.Checked := FConfig.GPSBehaviour.MapMoveCentered;
  actConfigMarksNamesVisible.Checked := FConfig.LayersConfig.MarksLayerConfig.MarksDrawConfig.CaptionDrawConfig.ShowPointCaption;
  actMarksEditSnapToMarkers.Checked := FConfig.MainConfig.MagnetDraw;

  actConfigMarksHide.Checked := not (FConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IsUseMarks);

  if FConfig.MainConfig.ShowMapName then begin
    TBSMB.Caption := FMainMapState.ActiveMap.GetStatic.GUIConfig.Name.Value;
  end else begin
    TBSMB.Caption := '';
  end;
end;

procedure TfrmMain.OnMainMapChange;
var
  VGUID: TGUID;
  VMapType: IMapType;
begin
  VMapType := FMainMapState.ActiveMap.GetStatic;
  VGUID := VMapType.GUID;
  TBSMB.ImageIndex := FMapTypeIcons24List.GetIconIndexByGUID(VGUID);
  if FConfig.MainConfig.ShowMapName and (VMapType <> nil) then begin
    TBSMB.Caption := VMapType.GUIConfig.Name.Value;
  end else begin
    TBSMB.Caption := '';
  end;
  TBXSubmnMapVer.Visible := VMapType.TileStorage.StorageTypeAbilities.VersionSupport = tstvsMultiVersions;
  if GState.Config.GeoCalcConfig.DatumSource = TGeoCalcDatumSource.dsZMP then begin
    GState.GeoCalc.Datum := VMapType.ProjectionSet.Zooms[0].ProjectionType.Datum;
  end;
end;

procedure TfrmMain.OnMapGUIChange;
begin
  FMapHotKeyList := GState.MapType.GUIConfigList.HotKeyList;
  CreateMapUIMapsList;
  CreateMapUILayersList;
  CreateMapUIFillingList;
  CreateMapUILayerSubMenu;

  OnMainMapChange;
end;

procedure TfrmMain.OnMarkEditConfigsChange;
var
  VConfig: IPointCaptionsLayerConfig;
  VPolygonConfig: IPolygonCaptionsLayerConfig;
begin
  // Calc line
  VConfig := FConfig.LayersConfig.CalcLineLayerConfig.CaptionConfig;
  VConfig.LockRead;
  try
    actCalcLineShowIntermediateDist.Checked := VConfig.ShowIntermediateDist;
    actCalcLineShowDistIncrement.Checked := VConfig.ShowDistIncrement;
    actCalcLineShowAzimuth.Checked := VConfig.ShowAzimuth;
    actCalcLineLabelVisible.Checked := VConfig.Visible;
  finally
    VConfig.UnlockRead;
  end;

  // Path edit
  VConfig := FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig;
  VConfig.LockRead;
  try
    actEditPathShowIntermediateDist.Checked := VConfig.ShowIntermediateDist;
    actEditPathShowDistIncrement.Checked := VConfig.ShowDistIncrement;
    actEditPathShowAzimuth.Checked := VConfig.ShowAzimuth;
    actEditPathShowPointHint.Checked := VConfig.ShowPointHint;
    actEditPathLabelVisible.Checked := VConfig.Visible;
  finally
    VConfig.UnlockRead;
  end;

  // Polygon edit
  VPolygonConfig := FConfig.LayersConfig.MarkPolygonLayerConfig.CaptionsConfig;
  VPolygonConfig.LockRead;
  try
    actEditPolygonShowPerimeter.Checked := VPolygonConfig.ShowPerimeter;
    actEditPolygonShowArea.Checked := VPolygonConfig.ShowArea;
    actEditPolygonLabelVisible.Checked := VPolygonConfig.Visible;
  finally
    VPolygonConfig.UnlockRead;
  end;
end;

procedure TfrmMain.OnToolbarsLockChange;
var
  VValue: Boolean;
begin
  VValue := FConfig.ToolbarsLock.GetIsLock;
  TBDock.AllowDrag := not VValue;
  TBDockLeft.AllowDrag := not VValue;
  TBDockRight.AllowDrag := not VValue;
  TBDockBottom.AllowDrag := not VValue;
  TBXDock1.AllowDrag := not VValue;
  TBXDockForSearch.AllowDrag := not VValue;
  actViewToolbarsLock.Checked := VValue;
end;

procedure TfrmMain.OnViewProjectionConfigChange;
var
  VEPSG: Integer;
  VProjList: IProjectionSetList;
  i: Integer;
  VIndex: Integer;
begin
  VEPSG := FConfig.ViewProjectionConfig.EPSG;
  VIndex := -1;
  if VEPSG <> 0 then begin
    VProjList := GState.ProjectionSetList;
    for i := 0 to VProjList.Count - 1 do begin
      if VProjList.Items[i].Zooms[0].ProjectionType.ProjectionEPSG = VEpsg then begin
        VIndex := i;
        break;
      end;
    end;
  end;
  for i := 0 to FactlstProjections.ActionCount - 1 do begin
    TCustomAction(FactlstProjections.Actions[i]).Checked := (i = VIndex);
  end;
  if (VIndex < 0) or (VIndex >= FactlstProjections.ActionCount) then begin
    actConfigProjectionOfMapUse.Checked := True;
    if VEPSG <> 0 then begin
      FConfig.ViewProjectionConfig.EPSG := 0;
    end;
  end else begin
    actConfigProjectionOfMapUse.Checked := False;
  end;
end;

procedure TfrmMain.OnWinPositionChange;
var
  VIsFullScreen: Boolean;
  VIsBordersVisible: Boolean;
  VIsMaximized: Boolean;
  VIsMinimized: Boolean;
  VRect: TRect;
  VIsDockVisible: Boolean;
begin
  FWinPosition.LockRead;
  try
    VIsFullScreen := FWinPosition.GetIsFullScreen;
    VIsBordersVisible := FWinPosition.GetIsBordersVisible;
    VIsMaximized := FWinPosition.GetIsMaximized;
    VIsMinimized := FWinPosition.IsMinimized;
    VRect := FWinPosition.GetBoundsRect;
  finally
    FWinPosition.UnlockRead;
  end;

  if VIsBordersVisible then begin
    if BorderStyle <> bsSizeable then begin
      BorderStyle := bsSizeable;
    end;
  end else begin
    if BorderStyle <> bsNone then begin
      BorderStyle := bsNone;
    end;
  end;

  if VIsMinimized then begin
    if (not TrayIcon.Visible) and GState.Config.GlobalAppConfig.IsShowIconInTray then begin
      TrayIcon.Visible := True;
      ShowWindow(Self.Handle, SW_HIDE);
    end else begin
      Self.WindowState := wsMinimized;
    end;
  end else begin
    if (TrayIcon.Visible) and GState.Config.GlobalAppConfig.IsShowIconInTray then begin
      ShowWindow(Self.Handle, SW_SHOW);
      TrayIcon.Visible := False;
    end;
    actViewFullScreen.Checked := VIsFullScreen;
    actViewBordersVisible.Checked := not VIsBordersVisible;

    VIsDockVisible := not VIsFullScreen and VIsBordersVisible;

    TBExit.Visible := not VIsDockVisible;

    TBDock.Parent := Self;
    TBDockLeft.Parent := Self;
    TBDockBottom.Parent := Self;
    TBDockRight.Parent := Self;
    TBDock.Visible := VIsDockVisible;
    TBDockLeft.Visible := VIsDockVisible;
    TBDockBottom.Visible := VIsDockVisible;
    TBDockRight.Visible := VIsDockVisible;

    if VIsFullScreen then begin
      Self.WindowState := wsMaximized;
      SetBounds(
        Monitor.Left + Self.Left - Self.ClientOrigin.X,
        Monitor.Top + Self.Top - Self.ClientOrigin.Y,
        Monitor.Width + (Self.Width - Self.ClientWidth),
        Monitor.Height + (Self.Height - Self.ClientHeight)
      );
    end else begin
      if VIsMaximized then begin
        if Self.WindowState <> wsMaximized then begin
          if not Types.EqualRect(Self.BoundsRect, VRect) then begin
            Self.BoundsRect := VRect;
          end;
        end;
        Self.WindowState := wsMaximized;
        SetBounds(
          Monitor.Left,
          Monitor.Top,
          Monitor.Width,
          Monitor.Height
        );
      end else begin
        Self.WindowState := wsNormal;
        Self.BoundsRect := VRect;
      end;
    end;
  end;
end;

//��������� ������� ������� � ��������
procedure TfrmMain.DoMessageEvent(
  var Msg: TMsg;
  var Handled: Boolean
);
begin
  if Self.Active then begin
    if not FMapZoomAnimtion then begin
      FKeyMovingHandler.DoMessageEvent(Msg, Handled);
    end;
  end;
end;

procedure TfrmMain.DoSelectSpecialVersion(Sender: TObject);
var
  VVersion: IMapVersionInfo;
  VVersionString: string;
  VMapType: IMapType;
begin
  if (nil <> Sender) and (Sender is TTBXItemSelectMapVersion) then begin
    if TTBXItemSelectMapVersion(Sender).Checked then begin
      VVersion := nil;
    end else begin
      VVersion := TTBXItemSelectMapVersion(Sender).MapVersion;
    end;
  end else begin
    // clear
    VVersion := nil;
  end;

  // for current map
  VMapType := FMainMapState.ActiveMap.GetStatic;

  if Assigned(VVersion) then begin
    VVersionString := VVersion.StoreString;
  end else begin
    VVersionString := VMapType.Zmp.Version;
  end;
  // apply this version or clear (uncheck) version
  VMapType.VersionRequestConfig.Version := VVersionString;
end;

procedure TfrmMain.FormShortCut(
  var Msg: TWMKey;
  var Handled: Boolean
);
var
  VKey: Word;
  VShortCut: TShortCut;
  VMapType: IMapType;
  VErrMsg: string;
  VFavoriteMapSet: IFavoriteMapSetItemStatic;
  VCancelSelection: Boolean;
  VLineOnMapEdit: ILineOnMapEdit;
  VLonLat: TDoublePoint;
begin
  if TBEditSelectPolylineRadius.Focused then begin
    // workaround: actlstMain can steal some hotkeys
    VKey := ShortCutFromMessage(Msg);
    if VKey in [VK_BACK, VK_RETURN, VK_ESCAPE] then begin
      TBEditSelectPolylineRadiusKeyDown(Self, VKey, []);
      Handled := True;
      Exit;
    end;
  end;

  if Self.Active then begin
    if Self.ActiveControl is TCustomEdit then begin
      Exit;
    end;
    VShortCut := ShortCutFromMessage(Msg);
    case VShortCut of
      VK_LEFT + scCtrl: begin
        VLineOnMapEdit := FLineOnMapEdit;
        if VLineOnMapEdit <> nil then begin
          VLonLat := VLineOnMapEdit.SetSelectedPrevPoint;
          if not PointIsEmpty(VLonLat) then begin
            FViewPortState.ChangeLonLat(VLonLat);
          end;
          Handled := True;
        end;
      end;
      VK_RIGHT + scCtrl: begin
        VLineOnMapEdit := FLineOnMapEdit;
        if VLineOnMapEdit <> nil then begin
          VLonLat := VLineOnMapEdit.SetSelectedNextPoint;
          if not PointIsEmpty(VLonLat) then begin
            FViewPortState.ChangeLonLat(VLonLat);
          end;
          Handled := True;
        end;
      end;
      VK_BACK: begin
        VLineOnMapEdit := FLineOnMapEdit;
        if VLineOnMapEdit <> nil then begin
          VLineOnMapEdit.DeleteActivePoint;
          Handled := True;
        end;
      end;
      VK_BACK + scCtrl: begin
        if FLineOnMapEdit <> nil then begin
          actEditPathRouteCalcUndo.Execute;
          Handled := True;
        end;
      end;
      VK_ESCAPE: begin
        case FState.State of
          ao_select_rect: begin
            VCancelSelection := False;
            if FSelectionRect.IsEmpty then begin
              VCancelSelection := True;
            end;
            FSelectionRect.Reset;
            if VCancelSelection then begin
              FState.State := ao_movemap;
            end;
            Handled := True;
          end;
          ao_edit_point: begin
            FState.State := ao_movemap;
            Handled := True;
          end;
          ao_select_poly,
          ao_select_line,
          ao_calc_line,
          ao_calc_circle: begin
            VLineOnMapEdit := FLineOnMapEdit;
            if VLineOnMapEdit <> nil then begin
              if not VLineOnMapEdit.IsEmpty then begin
                VLineOnMapEdit.Clear;
                FMovePoint := false;
              end else begin
                FState.State := ao_movemap;
              end;
              Handled := True;
            end;
          end;
          ao_edit_line: begin
            if FEditMarkLine = nil then begin
              VLineOnMapEdit := FLineOnMapEdit;
              if VLineOnMapEdit <> nil then begin
                if not VLineOnMapEdit.IsEmpty then begin
                  VLineOnMapEdit.Clear;
                  FMovePoint := false;
                end else begin
                  FState.State := ao_movemap;
                end;
                Handled := True;
              end;
            end else begin
              FState.State := ao_movemap;
              Handled := True;
            end;
          end;
          ao_edit_poly: begin
            if FEditMarkPoly = nil then begin
              VLineOnMapEdit := FLineOnMapEdit;
              if VLineOnMapEdit <> nil then begin
                if not VLineOnMapEdit.IsEmpty then begin
                  VLineOnMapEdit.Clear;
                  FMovePoint := false;
                end else begin
                  FState.State := ao_movemap;
                end;
                Handled := True;
              end;
            end else begin
              FState.State := ao_movemap;
              Handled := True;
            end;
          end;
        end;
      end;
      VK_RETURN: begin
        case FState.State of
          ao_edit_poly: begin
            VLineOnMapEdit := FLineOnMapEdit;
            if VLineOnMapEdit <> nil then begin
              if VLineOnMapEdit.IsReady then begin
                tbitmSaveMark.Action.Execute;
                Handled := True;
              end;
            end;
          end;
          ao_calc_line,
          ao_calc_circle,
          ao_edit_line,
          ao_select_line: begin
            VLineOnMapEdit := FLineOnMapEdit;
            if VLineOnMapEdit <> nil then begin
              if VLineOnMapEdit.IsReady then begin
                tbitmSaveMark.Action.Execute;
                Handled := True;
              end;
            end;
          end;
          ao_select_poly: begin
            VLineOnMapEdit := FLineOnMapEdit;
            if VLineOnMapEdit <> nil then begin
              if VLineOnMapEdit.IsReady then begin
                actSelectByGeometryFinish.Execute;
                Handled := True;
              end;
            end;
          end;
        end;
      end;
    else
      begin
        VFavoriteMapSet := FFavoriteMapSetHotKeyList.GetMapSetByHotKey(VShortCut);
        if VFavoriteMapSet <> nil then begin
          if not FFavoriteMapSetHelper.TrySwitchOn(VFavoriteMapSet, VErrMsg) then begin
            ShowErrorMessage(Handle, VErrMsg);
          end;
          Handled := True;
          Exit;
        end;

        VMapType := FMapHotKeyList.GetMapTypeByHotKey(VShortCut);
        if VMapType <> nil then begin
          if VMapType.Zmp.IsLayer then begin
            FConfig.MapLayersConfig.InvertLayerSelectionByGUID(VMapType.GUID);
          end else begin
            FConfig.MainMapConfig.MainMapGUID := VMapType.GUID;
          end;
          Handled := True;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.MapZoom(
  const ANewZoom: Byte;
  const AFreezePos: TPoint
);
var
  ts1, ts2, ts3, fr: int64;
  VScale: Double;
  VZoom: Byte;
  VAlfa: Double;
  VTime: Double;
  VLastTime: Double;
  VMaxTime: Double;
  VUseAnimation: Boolean;
  VScaleFinish: Double;
  VScaleStart: Double;
begin
  if (FMapZoomAnimtion) or (ANewZoom > 23) then begin
    Exit;
  end;

  FMapZoomAnimtion := True;
  FState.MapMovingBegin;
  try
    VZoom := FViewPortState.View.GetStatic.Projection.Zoom;
    if VZoom <> ANewZoom then begin
      VMaxTime := FConfig.MapZoomingConfig.AnimateZoomTime;

      VUseAnimation :=
        (FConfig.MapZoomingConfig.AnimateZoom) and
        ((VZoom = ANewZoom + 1) or (VZoom + 1 = ANewZoom)) and
        (VMaxTime > 0);

      if VUseAnimation then begin
        FViewPortState.ChangeZoomWithFreezeAtVisualPointWithScale(ANewZoom, AFreezePos);

        VScaleStart := FViewPortState.View.GetStatic.GetScale;
        VScaleFinish := 1;
        VTime := 0;
        VLastTime := 0;
        ts1 := FTimer.CurrentTime;
        fr := FTimer.Freq;
        ts3 := ts1;
        while (VTime + VLastTime < VMaxTime) do begin
          VAlfa := VTime / VMaxTime;
          VScale := VScaleStart + (VScaleFinish - VScaleStart) * VAlfa;
          FViewPortState.ScaleTo(VScale, AFreezePos);

          Application.ProcessMessages;

          ts2 := FTimer.CurrentTime;
          VLastTime := (ts2 - ts3) / (fr / 1000);
          VTime := (ts2 - ts1) / (fr / 1000);
          ts3 := ts2;
        end;
        VScale := VScaleFinish;
        FViewPortState.ScaleTo(VScale, AFreezePos);
      end else begin
        FViewPortState.ChangeZoomWithFreezeAtVisualPoint(ANewZoom, AFreezePos);
      end;
    end;
  finally
    FState.MapMovingEnd;
    FMapZoomAnimtion := False;
  end;
end;

procedure TfrmMain.MapMoveAnimate(
  const AMouseMoveSpeed: TDoublePoint;
  const AZoom: Byte;
  const AMousePos: TPoint
);
var
  VTimeStart: Int64;
  VTimeEnd: Int64;
  VAnimationTime: Double;
  VAnimationTimeMax: Double;
  VDist: Double;
  VMapDelta: TDoublePoint;
  VMapDeltaMul: TDoublePoint;
  VLastDrawTime: double;
  VMousePPS: Double;
  VLastTime: Double;
begin
  FMapMoveAnimtion := True;
  FState.MapMovingBegin;
  try
    VMousePPS := Sqrt(Sqr(AMouseMoveSpeed.X) + Sqr(AMouseMoveSpeed.Y));

    if (FConfig.MapMovingConfig.AnimateMove) and (VMousePPS > FConfig.MapMovingConfig.AnimateMinStartSpeed) then begin
      // ����� ��������� � ������ ��������
      VAnimationTime := 0;

      // ������������ ����� �� ����������� ��������
      VAnimationTimeMax := FConfig.MapMovingConfig.AnimateMoveTime / 1000;

      if VMousePPS > FConfig.MapMovingConfig.AnimateMaxStartSpeed then begin
        VMousePPS := FConfig.MapMovingConfig.AnimateMaxStartSpeed;
      end;

      VMapDeltaMul.X := AMouseMoveSpeed.X / VMousePPS;
      VMapDeltaMul.Y := AMouseMoveSpeed.Y / VMousePPS;

      // ����� ��������� �������� (���������������� ��������� ���� ������ ����, ����� �� ���� ���������� �����)
      VLastTime := 0.000000001;

      repeat
        // ���������� � ��������, ������� �� ������� �� ��������� VMousePPS �� ����� ��������
        VDist := VMousePPS * VAnimationTimeMax;

        // �� ����� ���������� ��������� ��, ������� �� ������ �� ����� VLastTime
        // (����� ����������� �� ��������� ChangeMapPixelByLocalDelta)
        VDist := VDist * (VLastTime / VAnimationTimeMax);

        // ��������� ���������������, ����� -exp(-1) ����� ��� ����, ����� � ��������� ������� ��������
        // � ��� �������� ���� ����� 0
        VDist := VDist * (exp(-VAnimationTime / VAnimationTimeMax) - exp(-1));

        VMapDelta.X := VMapDeltaMul.X * VDist;
        VMapDelta.Y := VMapDeltaMul.Y * VDist;

        VTimeStart := FTimer.CurrentTime;
        FViewPortState.ChangeMapPixelByLocalDelta(VMapDelta);

        Application.ProcessMessages;

        VTimeEnd := FTimer.CurrentTime;

        VLastDrawTime := (VTimeEnd - VTimeStart) / FTimer.Freq;
        VAnimationTime := VAnimationTime + VLastDrawTime;

        // ����� ��������� �������� ���������� � ����������� (����� �������� ���� ������ �� ����� ��������)
        VLastTime := VLastTime + 0.3 * (VLastDrawTime - VLastTime);
      until
        (VAnimationTime >= VAnimationTimeMax) or
        (AZoom <> FViewPortState.View.GetStatic.Projection.Zoom) or
        (AMousePos.X <> FMouseState.GetLastUpPos(FMapMovingButton).X) or
        (AMousePos.Y <> FMouseState.GetLastUpPos(FMapMovingButton).Y);
    end;
  finally
    FState.MapMovingEnd;
    FMapMoveAnimtion := False;
  end;
end;

procedure TfrmMain.tbpmiClearVersionClick(Sender: TObject);
begin
  DoSelectSpecialVersion(nil);
end;

procedure TfrmMain.tbpmiShowOtherVersionsClick(Sender: TObject);
var
  VMapType: IMapType;
begin
  // for current map
  VMapType := FMainMapState.ActiveMap.GetStatic;

  // apply this version or clear (uncheck) version
  VMapType.VersionRequestConfig.ShowOtherVersions := tbpmiShowOtherVersions.Checked;
end;

procedure TfrmMain.tbpmiVersionsPopup(
  Sender: TTBCustomItem;
  FromLink: Boolean
);
var
  VMapType: IMapType;
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMousePos: TPoint;
  VMouseMapPoint: TDoublePoint;
  VLonLat: TDoublePoint;
  VMapTile: Tpoint;
  VMapProjection: IProjection;
  I: Integer;
  VMenuItem: TTBXItemSelectMapVersion;
  VCurrentVersion: String;
  VSorted: Boolean;
  VList: IMapVersionListStatic;
  VVersion: IMapVersionRequest;
  VVersionInfo: IMapVersionInfo;
  VNewIndex: Integer;
  VStartingNewIndex: Integer;
  VAllowListOfTileVersions: Boolean;
begin
  // remove all versions
  for I := (tbpmiVersions.Count - 1) downto 0 do begin
    if (tbpmiVersions.Items[I] is TTBXItemSelectMapVersion) then begin
      tbpmiVersions.Delete(I);
    end;
  end;
  VStartingNewIndex := tbpmiVersions.Count;

  // and add view items
  VMapType := FMainMapState.ActiveMap.GetStatic;
  VAllowListOfTileVersions := VMapType.TileStorage.StorageTypeAbilities.VersionSupport = tstvsMultiVersions;
  tbpmiShowOtherVersions.Visible := VAllowListOfTileVersions;

  if VAllowListOfTileVersions then begin
    // to lonlat
    VLocalConverter := FViewPortState.View.GetStatic;
    VProjection := VLocalConverter.Projection;
    VMousePos := FMouseState.GetLastDownPos(mbRight);
    VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(VMousePos);
    VProjection.ValidatePixelPosFloatStrict(VMouseMapPoint, False);
    VLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);

    VMapProjection := VMapType.ProjectionSet.GetSuitableProjection(VProjection);
    // to map
    VMapTile :=
      PointFromDoublePoint(
        VMapProjection.LonLat2TilePosFloat(VLonLat),
        prToTopLeft
      );
    // get current version
    VVersion := VMapType.VersionRequest.GetStatic;
    VCurrentVersion := VVersion.BaseVersion.StoreString;
    tbpmiShowOtherVersions.Checked := VVersion.ShowOtherVersions;
    VList := VMapType.TileStorage.GetListOfTileVersions(VMapTile, VMapProjection.Zoom, VVersion);
    VVersion := nil;
    // parse list
    if Assigned(VList) then begin
      VSorted := VList.Sorted;
      for I := 0 to VList.Count - 1 do begin
        VVersionInfo := VList.Item[I];
        VMenuItem := TTBXItemSelectMapVersion.Create(tbpmiVersions);
        VMenuItem.MapVersion := VVersionInfo;
        VMenuItem.Caption := VVersionInfo.Caption;
        VMenuItem.Checked := ((Length(VCurrentVersion) > 0) and (VCurrentVersion = VVersionInfo.StoreString));
        VMenuItem.OnClick := DoSelectSpecialVersion;
        VMenuItem.Tag := NativeInt(VVersionInfo);

        if VSorted then begin
          tbpmiVersions.Add(VMenuItem);
        end else begin
          // get index (for sorting)
          VNewIndex := VStartingNewIndex;
          repeat
            if (VNewIndex >= tbpmiVersions.Count) then begin
              Break;
            end;
            if CompareStr(VVersionInfo.Caption, tbpmiVersions.Items[VNewIndex].Caption) > 0 then begin
              break;
            end;
            Inc(VNewIndex);
          until False;
          // insert it
          tbpmiVersions.Insert(VNewIndex, VMenuItem);
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  inherited;
  with Msg.MinMaxInfo^.ptMaxTrackSize do begin
    X := Monitor.Width + (Width - ClientWidth);
    Y := Monitor.Height + (Height - ClientHeight);
  end;
end;

Procedure TfrmMain.WMMove(Var Msg: TWMMove);
Begin
  Inherited;
  if FWinPosition <> nil then begin
    if not FWinPosition.GetIsFullScreen then begin
      if Self.WindowState = wsMaximized then begin
        FWinPosition.SetMaximized;
      end else if Self.WindowState = wsNormal then begin
        FWinPosition.SetWindowPosition(Self.BoundsRect);
      end;
    end;
  end;
End;

procedure TfrmMain.WMSize(var Msg: TWMSize);
begin
  inherited;
  if FWinPosition <> nil then begin
    if Msg.SizeType = SIZE_MINIMIZED then begin
      if not FWinPosition.IsMinimized then begin
        FWinPosition.SetMinimized;
      end;
    end else if Msg.SizeType = SIZE_MAXIMIZED then begin
      if FWinPosition.IsMinimized then begin
        FWinPosition.SetNotMinimized;
      end;
    end else if Msg.SizeType = SIZE_RESTORED then begin
      if FWinPosition.IsMinimized then begin
        FWinPosition.SetNotMinimized;
      end else begin
        if not FWinPosition.IsFullScreen and not FWinPosition.IsMaximized then begin
          FWinPosition.SetWindowPosition(Self.BoundsRect);
        end;
      end;
    end;
  end;
end;

Procedure TfrmMain.WMSysCommand(var Msg: TMessage);
begin
  if (Msg.WParam = SC_RESTORE) then begin
    if FWinPosition.IsMinimized then begin
      FWinPosition.SetNotMinimized;
    end else if FWinPosition.IsMaximized and (Self.WindowState = wsMaximized) then begin
      FWinPosition.SetNormalWindow;
    end else begin
      inherited;
    end;
  end else if (Msg.WParam = SC_MINIMIZE) then begin
    if (not FWinPosition.IsMinimized) then begin
      FWinPosition.SetMinimized;
    end else begin
      inherited;
    end;
  end else begin
    inherited;
  end;
end;

procedure TfrmMain.WMCopyData(var Msg: TMessage);
var
  VResult: Integer;
  VPCD: PCopyDataStruct;
  VRecievedStr: AnsiString;
begin
  try
    VPCD := PCopyDataStruct(Msg.LParam);
    VRecievedStr := PAnsiChar(VPCD.lpData);
    SetLength(VRecievedStr, VPCD.cbData);
    VResult := FArgProcessor.Process(VRecievedStr, FFormRegionProcess);
  except
    on E: Exception do begin
      VResult := cCmdLineArgProcessorSASExceptionRaised;
      ShowErrorMessage(Handle, E.ClassName + ': ' + E.Message);
    end;
  end;
  Msg.Result := VResult;
end;

procedure TfrmMain.WMDropFiles(var Msg: TWMDropFiles);
var
  I: Integer;
  VDropH: HDROP;
  VDroppedFileCount: Integer;
  VFileName: string;
  VFileNameLength: Integer;
  VImportConfig: IImportConfig;
  VLastMark: IVectorDataItem;
  VFiles: TStringList;
  VList: IStringListStatic;
begin
  inherited;
  Msg.Result := 0;
  VDropH := Msg.Drop;
  try
    VLastMark := nil;
    VImportConfig := nil;
    VDroppedFileCount := DragQueryFile(VDropH, $FFFFFFFF, nil, 0);
    VFiles := TStringList.Create;
    try
      for I := 0 to Pred(VDroppedFileCount) do begin
        VFileNameLength := DragQueryFile(VDropH, I, nil, 0);
        SetLength(VFileName, VFileNameLength);
        DragQueryFile(VDropH, I, PChar(VFileName), VFileNameLength + 1);
        VFiles.Add(VFileName);
      end;
      VList := TStringListStatic.CreateWithOwn(VFiles);
      ProcessOpenFiles(VList);
    finally
      FreeAndNil(VFiles);
    end;
  finally
    DragFinish(VDropH);
  end;
end;

procedure TfrmMain.WMTimeChange(var m: TMessage);
begin
  inherited;
  GState.SystemTimeChanged;
end;

procedure TfrmMain.WMFriendOrFoeMessage(var Msg: TMessage);
begin
  Msg.Result := u_CmdLineArgProcessorAPI.WM_FRIEND_OR_FOE;
end;

procedure TfrmMain.ZoomToolBarDockChanging(
  Sender: TObject;
  Floating: Boolean;
  DockingTo: TTBDock
);
begin
  if (DockingTo = TBDockLeft) or (DockingTo = TBDockRight) then begin
    if FRuller.Width > FRuller.Height then begin
      FRuller.Rotate270();
      FTumbler.Rotate270();
    end;
    ZoomToolBar.Items.Move(ZoomToolBar.Items.IndexOf(TBZoom_out), 4);
    ZoomToolBar.Items.Move(ZoomToolBar.Items.IndexOf(TBZoomin), 0);
  end else begin
    if FRuller.Width < FRuller.Height then begin
      FRuller.Rotate90();
      FTumbler.Rotate90();
    end;
    ZoomToolBar.Items.Move(ZoomToolBar.Items.IndexOf(TBZoom_out), 0);
    ZoomToolBar.Items.Move(ZoomToolBar.Items.IndexOf(TBZoomin), 4);
  end;
  PaintZSlider(FViewPortState.View.GetStatic.Projection.Zoom);
end;

procedure TfrmMain.tbitmOnInterfaceOptionsClick(Sender: TObject);
begin
  if Sender is TLayerStatBarPopupMenu then begin
    FfrmMapLayersOptions.ShowStatusBarOptions;
  end else if (Sender is TLayerScaleLinePopupMenu) then begin
    FfrmMapLayersOptions.ShowScaleLineOptions;
  end else begin
    FfrmMapLayersOptions.ShowModal;
  end;
end;

procedure TfrmMain.NaddPointClick(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VMouseMapPoint: TDoublePoint;
  VProjection: IProjection;
  VMouseLonLat: TDoublePoint;
  VPoint: IGeometryLonLatPoint;
begin
  VLocalConverter := FViewPortState.View.GetStatic;
  VProjection := VLocalConverter.Projection;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseState.GetLastDownPos(mbRight));
  VProjection.ValidatePixelPosFloatStrict(VMouseMapPoint, True);
  VMouseLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
  VPoint := GState.VectorGeometryLonLatFactory.CreateLonLatPoint(VMouseLonLat);
  if FMarkDBGUI.SaveMarkModal(nil, VPoint) then begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.tbitmCopyToClipboardMainMapTileClick(Sender: TObject);
var
  VMouseMapPoint: TDoublePoint;
  VProjection: IProjection;
  VMapType: IMapType;
  VLocalConverter: ILocalCoordConverter;
  VTile: TPoint;
  VBitmapTile: IBitmap32Static;
begin
  VLocalConverter := FViewPortState.View.GetStatic;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseState.GetLastDownPos(mbRight));

  VProjection := VLocalConverter.Projection;
  VMapType := FMainMapState.ActiveMap.GetStatic;

  VProjection.ValidatePixelPosFloatStrict(VMouseMapPoint, True);
  VTile :=
    PointFromDoublePoint(
      VProjection.PixelPosFloat2TilePosFloat(VMouseMapPoint),
      prToTopLeft
    );
  VBitmapTile := VMapType.LoadTileUni(VTile, VProjection, VMapType.VersionRequest.GetStatic, True, True, False);
  if VBitmapTile <> nil then begin
    CopyBitmapToClipboard(Handle, VBitmapTile);
  end;
end;

procedure TfrmMain.tbitmCopyToClipboardCoordinatesClick(Sender: TObject);
var
  VMouseLonLat: TDoublePoint;
  VStr: string;
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMouseMapPoint: TDoublePoint;
begin
  VLocalConverter := FViewPortState.View.GetStatic;
  VProjection := VLocalConverter.Projection;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseState.GetLastDownPos(mbRight));
  VProjection.ValidatePixelPosFloatStrict(VMouseMapPoint, True);
  VMouseLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
  VStr := GState.CoordToStringConverter.GetStatic.LonLatConvert(VMouseLonLat);
  CopyStringToClipboard(Handle, VStr);
end;

procedure TfrmMain.tbitmCopyToClipboardCoordinatesAndElevClick(Sender: TObject);
var
  VMouseLonLat: TDoublePoint;
  VStr: string;
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VTerrainInfo: ITerrainInfo;
  VMouseMapPoint: TDoublePoint;
begin
  VLocalConverter := FViewPortState.View.GetStatic;
  VProjection := VLocalConverter.Projection;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseState.GetLastDownPos(mbRight));
  VProjection.ValidatePixelPosFloatStrict(VMouseMapPoint, True);
  VMouseLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
  VTerrainInfo := TTerrainInfo.Create(GState.Config.TerrainConfig, GState.TerrainProviderList);
  VStr := GState.CoordToStringConverter.GetStatic.LonLatConvert(VMouseLonLat) + ' ' +
    VTerrainInfo.GetElevationInfoStr(VMouseLonLat, VProjection.Zoom);
  CopyStringToClipboard(Handle, VStr);
end;

procedure TfrmMain.tbitmCopyToClipboardGenshtabNameClick(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VListName: string;
begin
  VLocalConverter := FViewPortState.View.GetStatic;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseState.GetLastDownPos(mbRight));
  VProjection := VLocalConverter.Projection;
  VProjection.ValidatePixelPosFloatStrict(VMouseMapPoint, True);
  VMouseLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
  if FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Visible then begin
    VListName := LonLat2GShListName(VMouseLonLat, GetActualGshSCale(FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Scale, VProjection.Zoom), 100000000);
  end else begin
    VListName := '';
  end;
  CopyStringToClipboard(Handle, VListName);
end;

procedure TfrmMain.tbitmCopyToClipboardMainMapTileFileNameClick(Sender: TObject);
var
  VMapProjection: IProjection;
  VMapType: IMapType;
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
begin
  VMapType := FMainMapState.ActiveMap.GetStatic;
  VLocalConverter := FViewPortState.View.GetStatic;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseState.GetLastDownPos(mbRight));
  VProjection := VLocalConverter.Projection;
  VProjection.ValidatePixelPosFloatStrict(VMouseMapPoint, True);
  VMouseLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
  VMapProjection := VMapType.ProjectionSet.GetSuitableProjection(VProjection);
  VMapProjection.ProjectionType.ValidateLonLatPos(VMouseLonLat);

  VTile :=
    PointFromDoublePoint(
      VMapProjection.LonLat2TilePosFloat(VMouseLonLat),
      prToTopLeft
    );

  CopyStringToClipboard(Handle, VMapType.TileStorage.GetTileFileName(VTile, VMapProjection.Zoom, VMapType.VersionRequest.GetStatic.BaseVersion));
end;

procedure TfrmMain.tbitmDownloadMainMapTileClick(Sender: TObject);
var
  VPath: string;
  VMapType: IMapType;
  VMapProjection: IProjection;
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
  VVersion: IMapVersionInfo;
  VTileInfo: ITileInfoBasic;
begin
  if TComponent(Sender).Tag <> 0 then begin
    VMapType := IMapType(TComponent(Sender).Tag);
  end else begin
    VMapType := FMainMapState.ActiveMap.GetStatic;
  end;

  VLocalConverter := FViewPortState.View.GetStatic;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseState.GetLastDownPos(mbRight));
  VProjection := VLocalConverter.Projection;
  if VProjection.CheckPixelPosFloatStrict(VMouseMapPoint) then begin
    VMouseLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
    VMapProjection := VMapType.ProjectionSet.GetSuitableProjection(VProjection);
    if VMapProjection.ProjectionType.CheckLonLatPos(VMouseLonLat) then begin
      VTile :=
        PointFromDoublePoint(
          VMapProjection.LonLat2TilePosFloat(VMouseLonLat),
          prToTopLeft
        );

      VVersion := VMapType.VersionRequest.GetStatic.BaseVersion;
      VPath := VMapType.GetTileShowName(VTile, VMapProjection.Zoom, VVersion);
      VTileInfo := VMapType.TileStorage.GetTileInfo(VTile, VMapProjection.Zoom, VVersion, gtimAsIs);

      if not Assigned(VTileInfo) or
         not VTileInfo.GetIsExists or
         (ShowQuestionMessage(Handle, Format(SAS_MSG_TileExists, [VPath]), MB_YESNO) = ID_YES)
      then begin
        TTileDownloaderUIOneTile.Create(
          GState.Config.DownloaderThreadConfig,
          GState.AppClosingNotifier,
          VTile,
          VMapProjection.Zoom,
          VMapType,
          VVersion,
          GState.DownloadInfo,
          GState.GlobalInternetState,
          FTileErrorLogger
        );
      end;
    end;
  end;
end;

procedure TfrmMain.tbitmHideThisMarkClick(Sender: TObject);
var
  VMark: IVectorDataItem;
  VMarkId: IMarkId;
begin
  VMark := FSelectedMark;
  if Assigned(VMark) and Supports(VMark.MainInfo, IMarkId, VMarkId) then begin
    FMarkDBGUI.MarksDb.MarkDb.SetMarkVisibleByID(VMarkId, False);
  end;
end;

procedure TfrmMain.tbitmFitMarkToScreenClick(Sender: TObject);
var
  VMark: IVectorDataItem;
  VLLRect: TDoubleRect;
begin
  VMark := FSelectedMark;
  if VMark <> nil then begin
    VLLRect := VMark.Geometry.Bounds.Rect;
    FMapGoto.FitRectToScreen(VLLRect);
  end;
end;

procedure TfrmMain.NopendirClick(Sender: TObject);
var
  VMapProjection: IProjection;
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
  VMapType: IMapType;
  VFileName: string;
begin
  VMapType := FMainMapState.ActiveMap.GetStatic;
  if VMapType.TileStorage.StorageTypeAbilities.StorageClass = tstcInSeparateFiles then begin
    VLocalConverter := FViewPortState.View.GetStatic;
    VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseState.GetLastDownPos(mbRight));
    VProjection := VLocalConverter.Projection;
    VProjection.ValidatePixelPosFloatStrict(VMouseMapPoint, True);
    VMouseLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
    VMapProjection := VMapType.ProjectionSet.GetSuitableProjection(VProjection);
    VMapProjection.ProjectionType.ValidateLonLatPos(VMouseLonLat);
    VTile :=
      PointFromDoublePoint(
        VMapProjection.LonLat2TilePosFloat(VMouseLonLat),
        prToTopLeft
      );
    VFileName := VMapType.TileStorage.GetTileFileName(VTile, VMapProjection.Zoom, VMapType.VersionRequest.GetStatic.BaseVersion);
    if FileExists(VFileName) then begin
      OpenFileInDefaultProgram(VFileName);
    end else begin
      ShowErrorMessage(Handle, Format(SAS_ERR_FileNotExistFmt, [VFileName]));
    end;
  end else begin
    ShowErrorMessage(Handle, SAS_MSG_CantGetTileFileName);
  end;
end;

procedure TfrmMain.tbitmOpenFolderMainMapTileClick(Sender: TObject);
var
  VTilePath: string;
  VTileFileName: string;
  VMapProjection: IProjection;
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
  VMapType: IMapType;
begin
  if TComponent(Sender).Tag <> 0 then begin
    VMapType := IMapType(TComponent(Sender).Tag);
  end else begin
    VMapType := FMainMapState.ActiveMap.GetStatic;
  end;

  VLocalConverter := FViewPortState.View.GetStatic;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseState.GetLastDownPos(mbRight));
  VProjection := VLocalConverter.Projection;
  VProjection.ValidatePixelPosFloatStrict(VMouseMapPoint, True);
  VMouseLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
  VMapProjection := VMapType.ProjectionSet.GetSuitableProjection(VProjection);
  VMapProjection.ProjectionType.ValidateLonLatPos(VMouseLonLat);
  VTile :=
    PointFromDoublePoint(
      VMapProjection.LonLat2TilePosFloat(VMouseLonLat),
      prToTopLeft
    );
  VTileFileName := VMapType.TileStorage.GetTileFileName(VTile, VMapProjection.Zoom, VMapType.VersionRequest.GetStatic.BaseVersion);
  VTilePath := ExtractFilePath(VTileFileName);
  if DirectoryExists(VTilePath) then begin
    if FileExists(VTileFileName) then begin
      SelectFileInExplorer(VTileFileName);
    end else begin
      SelectPathInExplorer(VTilePath);
    end;
  end else begin
    ShowMessageFmt(SAS_ERR_DirectoryNotExistFmt, [VTilePath]);
  end;
end;

function TfrmMain.ConvLatLon2Scale(const Astr: string): Double;
var
  rest: boolean;
  res: Double;
  i, delitel: integer;
  gms: double;
  VText: string;
begin

  VText := Astr;
  rest := True;
  i := 1;
  while i <= length(VText) do begin
    if not CharInSet(VText[i], ['0'..'9', '-', '+', '.', ',', ' ']) then begin
      VText[i] := ' ';
      dec(i);
    end;
    if ((i = 1) and (VText[i] = ' ')) or
      ((i = length(VText)) and (VText[i] = ' ')) or
      ((i < length(VText) - 1) and (VText[i] = ' ') and (VText[i + 1] = ' ')) or
      ((i > 1) and (VText[i] = ' ') and (not CharInSet(VText[i - 1], ['0'..'9']))) or
      ((i < length(VText) - 1) and (VText[i] = ',') and (VText[i + 1] = ' ')) then begin
      Delete(VText, i, 1);
      dec(i);
    end;
    inc(i);
  end;

  try
    res := 0;
    delitel := 1;
    repeat
      i := posEx(' ', VText, 1);
      if i = 0 then begin
        gms := str2r(VText);
      end else begin
        gms := str2r(copy(VText, 1, i - 1));
        Delete(VText, 1, i);
      end;
      if ((delitel > 1) and (abs(gms) > 60)) or
        ((delitel = 1) and (abs(gms) > 180)) then begin
        Rest := False;
      end;
      if res < 0 then begin
        res := res - gms / delitel;
      end else begin
        res := res + gms / delitel;
      end;
      delitel := delitel * 60;
    until (i = 0) or (delitel > 3600) or (not rest);
  except
    res := 0;
  end;
  result := res;
end;

function TfrmMain.Deg2StrValue(const aDeg: Double): string;
var
  Vmin: integer;
  VDegScale: Double;
begin
   // convert to  � ' "
  VDegScale := abs(aDeg / 100000000);
  result := IntToStr(Trunc(VDegScale)) + '�';
  VDegScale := Frac(VDegScale + 0.0000000001) * 60;
  Vmin := Trunc(VDegScale);
  if Vmin < 10 then begin
    result := result + '0' + IntToStr(Vmin) + '''';
  end else begin
    result := result + IntToStr(Vmin) + '''';
  end;
  VDegScale := Frac(VDegScale) * 60;
  result := result + FormatFloat('00.00', VDegScale) + '"';
end;

procedure TfrmMain.NDegValueAcceptText(
  Sender: TObject;
  var NewText: string;
  var Accept: Boolean
);
var
  VTag: Integer;
begin
  VTag := Trunc(ConvLatLon2Scale(NewText) * 100000000);
  NewText := Deg2StrValue(VTag);
  actViewGridLonLat_User.Tag := VTag;
  actViewGridLonLat_User.Checked := False;
  actViewGridLonLat_User.Execute;
end;

procedure TfrmMain.NDelClick(Sender: TObject);
var
  VPath: string;
  VMapType: IMapType;
  VMapProjection: IProjection;
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
  VMessage: string;
  VVersion: IMapVersionInfo;
begin
  if TComponent(Sender).Tag <> 0 then begin
    VMapType := IMapType(TComponent(Sender).Tag);
  end else begin
    VMapType := FMainMapState.ActiveMap.GetStatic;
  end;

  VLocalConverter := FViewPortState.View.GetStatic;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseState.GetLastDownPos(mbRight));
  VProjection := VLocalConverter.Projection;
  if VProjection.CheckPixelPosFloatStrict(VMouseMapPoint) then begin
    VMouseLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
    VMapProjection := VMapType.ProjectionSet.GetSuitableProjection(VProjection);
    if VMapProjection.ProjectionType.CheckLonLatPos(VMouseLonLat) then begin
      VTile :=
        PointFromDoublePoint(
          VMapProjection.LonLat2TilePosFloat(VMouseLonLat),
          prToTopLeft
        );
      VVersion := VMapType.VersionRequest.GetStatic.BaseVersion;
      VPath := VMapType.GetTileShowName(VTile, VMapProjection.Zoom, VVersion);
      VMessage := Format(SAS_MSG_DeleteTileOneTileAsk, [VPath]);
      if ShowQuestionMessage(Handle, VMessage, MB_YESNO) = ID_YES then begin
        VMapType.TileStorage.DeleteTile(VTile, VMapProjection.Zoom, VVersion);
      end;
    end;
  end;
end;

{$REGION 'CachedTilesMap'}
procedure TfrmMain.DateTimePicker1Change(Sender: TObject);
begin
  if (DateTimePicker2.DateTime < DateTimePicker1.DateTime) then begin
    DateTimePicker2.DateTime := DateTimePicker1.DateTime;
  end;
  FConfig.LayersConfig.FillingMapLayerConfig.LockWrite;
  try
    FConfig.LayersConfig.FillingMapLayerConfig.FillFirstDay := DateTimePicker1.DateTime;
    FConfig.LayersConfig.FillingMapLayerConfig.FillLastDay := DateTimePicker2.DateTime;
  finally
    FConfig.LayersConfig.FillingMapLayerConfig.UnlockWrite;
  end;
end;

procedure TfrmMain.DateTimePicker2Change(Sender: TObject);
begin
  if (DateTimePicker1.DateTime > DateTimePicker2.DateTime) then begin
    DateTimePicker1.DateTime := DateTimePicker2.DateTime;
  end;
  FConfig.LayersConfig.FillingMapLayerConfig.LockWrite;
  try
    FConfig.LayersConfig.FillingMapLayerConfig.FillFirstDay := DateTimePicker1.DateTime;
    FConfig.LayersConfig.FillingMapLayerConfig.FillLastDay := DateTimePicker2.DateTime;
  finally
    FConfig.LayersConfig.FillingMapLayerConfig.UnlockWrite;
  end;
end;

procedure TfrmMain.tbtpltCachedTilesMapCellClick(
  Sender: TTBXCustomToolPalette;
  var ACol, ARow: Integer;
  var AllowChange: Boolean
);
var
  VZoom: Byte;
  VRelative: Boolean;
begin
  if (ACol = 0) and (ARow = 0) then begin
    FConfig.LayersConfig.FillingMapLayerConfig.Visible := False;
    FFillingMapPolygon.Polygon := nil;
  end else begin
    if ARow < 5 then begin
      VZoom := 5 * ARow + ACol - 1;
      VRelative := False;
    end else begin
      VZoom := 5 * (ARow - 5) + ACol;
      VRelative := True;
    end;

    if Assigned(FSelectedPolygon) then begin
      FFillingMapPolygon.Polygon := FSelectedPolygon;
      FSelectedPolygon := nil;
    end;

    FConfig.LayersConfig.FillingMapLayerConfig.LockWrite;
    try
      FConfig.LayersConfig.FillingMapLayerConfig.Visible := True;
      FConfig.LayersConfig.FillingMapLayerConfig.UseRelativeZoom := VRelative;
      FConfig.LayersConfig.FillingMapLayerConfig.Zoom := VZoom;
    finally
      FConfig.LayersConfig.FillingMapLayerConfig.UnlockWrite;
    end;
  end;
end;

procedure TfrmMain.tbxFillingMapClick(Sender: TObject);
var
  VMark: IVectorDataItem;
begin
  FSelectedPolygon := nil;
  VMark := FSelectedMark;
  if not Assigned(VMark) then begin
    VMark := FSelectedWiki;
  end;
  if Assigned(VMark) then begin
    Supports(VMark.Geometry, IGeometryLonLatPolygon, FSelectedPolygon);
  end;
end;
{$ENDREGION 'CachedTilesMap'}

procedure TfrmMain.tbtpltCenterWithZoomCellClick(
  Sender: TTBXCustomToolPalette;
  var ACol, ARow: Integer;
  var AllowChange: Boolean
);
var
  VZoom: Byte;
  VMouseDownPoint: TPoint;
begin
  AllowChange := False;
  VZoom := ((5 * ARow) + ACol) - 1;
  VMouseDownPoint := FMouseState.GetLastDownPos(mbRight);
  MapZoom(VZoom, VMouseDownPoint);
end;

{$REGION 'TileBoundaries'}
procedure TfrmMain.ProcessViewGridTileCellClick(const ATag: Integer);
var
  VZoom: Byte;
  VIsRelativeZoom: Boolean;
  VTileGrid: ITileGridConfig;
begin
  VTileGrid := FConfig.LayersConfig.MapLayerGridsConfig.TileGrid;

  case ATag of
    0: begin
      VTileGrid.Visible := False;
    end;
    1..30: begin
      if ATag <= 24 then begin
        VZoom := ATag - 1;
        VIsRelativeZoom := False;
      end else begin
        VZoom := ATag - 24 - 1;
        VIsRelativeZoom := True;
      end;

      if (VTileGrid.Zoom = VZoom) and (VTileGrid.UseRelativeZoom = VIsRelativeZoom) then begin
        VTileGrid.Visible := not VTileGrid.Visible;
      end else begin
        VTileGrid.LockWrite;
        try
          VTileGrid.Visible := True;
          VTileGrid.UseRelativeZoom := VIsRelativeZoom;
          VTileGrid.Zoom := VZoom;
        finally
          VTileGrid.UnlockWrite;
        end;
      end;
    end;
  else
    Assert(False, 'Tag out of range [0..30]: ' + IntToStr(ATag));
  end;
end;

procedure TfrmMain.actViewTilesGridExecute(Sender: TObject);
begin
  ProcessViewGridTileCellClick(TComponent(Sender).Tag - 100);
end;

procedure TfrmMain.tbtpltViewGridTileCellClick(
  Sender: TTBXCustomToolPalette;
  var ACol, ARow: Integer;
  var AllowChange: Boolean
);
begin
  ProcessViewGridTileCellClick(5 * ARow + ACol);
end;

procedure TfrmMain.tbtpltViewGridTileGetCellVisible(
  Sender: TTBXCustomToolPalette;
  ACol, ARow: Integer;
  var Visible: Boolean
);
begin
  Visible := (5 * (ARow - 5) + ACol) <= 5; // hide relative zooms > +5
end;
{$ENDREGION 'TileBoundaries'}

procedure TfrmMain.TBEditSelectPolylineRadiusOnUpClick(Sender: TObject);
var
  VKey: Word;
begin
  VKey := VK_UP;
  TBEditSelectPolylineRadiusKeyDown(Sender, VKey, []);
end;

procedure TfrmMain.TBEditSelectPolylineRadiusOnDownClick(Sender: TObject);
var
  VKey: Word;
begin
  VKey := VK_DOWN;
  TBEditSelectPolylineRadiusKeyDown(Sender, VKey, []);
end;

procedure TfrmMain.TBEditSelectPolylineRadiusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  VText: string;
  VSelStart: Integer;
  VSpinEdit: TSpinEdit;
  VCircleOnMapEdit: ICircleOnMapEdit;
begin
  VSpinEdit := TBEditSelectPolylineRadius;

  case Key of
    VK_BACK: begin
      if VSpinEdit.SelText <> '' then begin
        VSpinEdit.SetSelText('');
      end else begin
        VSelStart := VSpinEdit.SelStart;
        if VSelStart > 0 then begin
          VText := VSpinEdit.Text;
          Delete(VText, VSelStart, 1);
          VSpinEdit.Text := VText;
          VSpinEdit.SelStart := VSelStart - 1;
        end;
      end;
    end;

    VK_RETURN, VK_UP, VK_DOWN: begin
      if FState.State = ao_select_line then begin
        FConfig.LayersConfig.SelectionPolylineLayerConfig.ShadowConfig.Radius := VSpinEdit.Value;
      end else
      if FState.State = ao_calc_circle then begin
        if Supports(FLineOnMapEdit, ICircleOnMapEdit, VCircleOnMapEdit) then begin
          if Round(VCircleOnMapEdit.Radius) <> VSpinEdit.Value then begin
            VCircleOnMapEdit.Radius := VSpinEdit.Value;
          end;
        end;
      end;
      if Key = VK_RETURN then begin
        Self.SetFocus;
      end;
    end;

    VK_ESCAPE: begin
      if FState.State = ao_select_line then begin
        VSpinEdit.Value := Round(FConfig.LayersConfig.SelectionPolylineLayerConfig.ShadowConfig.Radius);
      end else
      if FState.State = ao_calc_circle then begin
        if Supports(FLineOnMapEdit, ICircleOnMapEdit, VCircleOnMapEdit) then begin
          VSpinEdit.Value := Round(VCircleOnMapEdit.Radius);
        end;
      end;
      Self.SetFocus;
    end;
  end;
end;

procedure TfrmMain.mapResize(Sender: TObject);
begin
  FViewPortState.ChangeViewSize(Point(map.Width, map.Height));
end;

procedure TfrmMain.mapDblClick(Sender: TObject);
var
  r: TPoint;
  i: Integer;
  VLayer: TCustomLayer;
begin
  if FState.State = ao_movemap then begin
    r := map.ScreenToClient(Mouse.CursorPos);
    for i := 0 to map.Layers.Count - 1 do begin
      VLayer := map.Layers[i];
      if VLayer.MouseEvents then begin
        if VLayer.HitTest(r.X, r.Y) then begin
          Exit;
        end;
      end;
    end;
    if FState.IsMapMoving then begin
      Assert(False, 'Unexpected state: IsMapMoving = True');
      //FState.MapMovingEnd;
    end;
    FViewPortState.ChangeMapPixelToVisualPoint(r);
  end;
end;

procedure TfrmMain.tbitmMarkEditPositionClick(Sender: TObject);
begin
  Self.OnEditMarkPosition(FSelectedMark);
end;

procedure TfrmMain.OnEditMarkPosition(const AMark: IVectorDataItem);
var
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatLine;
  VPoly: IGeometryLonLatPolygon;
  VPathOnMapEdit: IPathOnMapEdit;
  VPolygonOnMapEdit: IPolygonOnMapEdit;
begin
  if AMark = nil then begin
    Exit;
  end;
  if Supports(AMark.Geometry, IGeometryLonLatPoint, VPoint) then begin
    FEditMarkPoint := AMark;
    FState.State := ao_edit_point;
    FPointOnMapEdit.Point := VPoint.Point;
  end else
  if Supports(AMark.Geometry, IGeometryLonLatLine, VLine) then begin
    FEditMarkLine := AMark;
    FState.State := ao_edit_line;
    if Supports(FLineOnMapEdit, IPathOnMapEdit, VPathOnMapEdit) then begin
      VPathOnMapEdit.SetPath(VLine);
    end;
  end else
  if Supports(AMark.Geometry, IGeometryLonLatPolygon, VPoly) then begin
    FEditMarkPoly := AMark;
    FState.State := ao_edit_poly;
    if Supports(FLineOnMapEdit, IPolygonOnMapEdit, VPolygonOnMapEdit) then begin
      VPolygonOnMapEdit.SetPolygon(VPoly);
    end;
  end;
end;

procedure TfrmMain.NMarkExportClick(Sender: TObject);
var
  VMark: IVectorDataItem;
begin
  VMark := FSelectedMark;
  if VMark <> nil then begin
    FMarkDBGUI.ExportMark(VMark);
  end;
end;

procedure TfrmMain.NMarkDelClick(Sender: TObject);
var
  VMark: IVectorDataItem;
begin
  VMark := FSelectedMark;
  if VMark <> nil then begin
    FMarkDBGUI.DeleteMarkModal(VMark.MainInfo as IMarkId, Handle);
  end;
end;

procedure TfrmMain.NMarkOperClick(Sender: TObject);
var
  VMark: IVectorDataItem;
  VSelectedWiki: IVectorDataItem;
  VPolygon: IGeometryLonLatPolygon;
begin
  VPolygon := nil;
  VMark := FSelectedMark;
  if VMark <> nil then begin
    VPolygon := FMarkDBGUI.PolygonForOperation(VMark.Geometry);
  end else begin
    // no mark - try to select wiki
    VSelectedWiki := FSelectedWiki;
    if VSelectedWiki <> nil then begin
      VPolygon := FMarkDBGUI.PolygonForOperation(VSelectedWiki.Geometry);
    end;
  end;
  if VPolygon <> nil then begin
    FRegionProcess.ProcessPolygon(VPolygon);
  end;
end;

procedure TfrmMain.NMarkPlayClick(Sender: TObject);
begin
  if (nil = FSelectedMark) then begin
    Exit;
  end;
  if (nil = FPlacemarkPlayerPlugin) then begin
    Exit;
  end;
  {FPlacemarkPlayerTask := }FPlacemarkPlayerPlugin.PlayByDescription(FSelectedMark.Desc);
end;

procedure TfrmMain.tbxWeatherUndergroundClick(Sender: TObject);
var
  VRequest: IDownloadRequest;
begin
  VRequest := FUrlProviderWeatherUnderground.GetUrl(FViewPortState.View.GetStatic, FMouseState.GetLastDownPos(mbRight));
  if Assigned(VRequest) then begin
    GState.InternalBrowser.NavigateByRequest(tbxWeatherUnderground.Caption, VRequest);
  end;
end;

procedure TfrmMain.tbxYandexWeatherClick(Sender: TObject);
var
  VRequest: IDownloadRequest;
begin
  VRequest := FUrlProviderYandexWeather.GetUrl(FViewPortState.View.GetStatic, FMouseState.GetLastDownPos(mbRight));
  if Assigned(VRequest) then begin
    GState.InternalBrowser.NavigateByRequest(tbxYandexWeather.Caption, VRequest);
  end;
end;

procedure TfrmMain.tbitmCopyToClipboardMainMapUrlClick(Sender: TObject);
var
  VMapProjection: IProjection;
  VMapType: IMapType;
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
  VRequest: IDownloadRequest;
begin
  if TComponent(Sender).Tag <> 0 then begin
    VMapType := IMapType(TComponent(Sender).Tag);
  end else begin
    VMapType := FMainMapState.ActiveMap.GetStatic;
  end;
  if VMapType.Zmp.TileDownloaderConfig.Enabled then begin
    VLocalConverter := FViewPortState.View.GetStatic;
    VProjection := VLocalConverter.Projection;
    VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseState.GetLastDownPos(mbRight));
    VProjection.ValidatePixelPosFloatStrict(VMouseMapPoint, True);
    VMouseLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
    VMapProjection := VMapType.ProjectionSet.GetSuitableProjection(VProjection);
    VMapProjection.ProjectionType.ValidateLonLatPos(VMouseLonLat);
    VTile :=
      PointFromDoublePoint(
        VMapProjection.LonLat2TilePosFloat(VMouseLonLat),
        prToTopLeft
      );
    VRequest :=
      VMapType.TileDownloadSubsystem.GetRequest(
        VTile,
        VMapProjection.Zoom,
        VMapType.VersionRequest.GetStatic.BaseVersion
      );
    if Assigned(VRequest) then begin
      CopyDownloadRequestToClipboard(Handle, VRequest);
    end else begin
      ShowErrorMessage(Handle, _('Nothing to copy: URL is empty!'));
    end;
  end;
end;

procedure TfrmMain.mapMouseLeave(Sender: TObject);
begin
  FMapHintWindow.HideHint;
end;

procedure TfrmMain.GPSReceiverDisconnect;
begin
  if FConfig.GPSBehaviour.SensorsAutoShow then begin
    TBXSensorsBar.Visible := False;
  end;
  actGpsConnect.Enabled := True;
  actGpsConnect.Checked := False;
end;

procedure TfrmMain.GPSReceiverReceive;
var
  VGPSNewPos: TDoublePoint;
  VCenterToGPSDelta: TDoublePoint;
  VPointDelta: TDoublePoint;
  VCenterMapPoint: TDoublePoint;
  VGPSMapPoint: TDoublePoint;
  VPosition: IGPSPosition;
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMapMove: Boolean;
  VMapMoveCentred: Boolean;
  VMinDelta: Double;
  VProcessGPSIfActive: Boolean;
  VDelta: Double;
begin
  VPosition := GState.GPSRecorder.CurrentPosition;

  // no position?
  if (not VPosition.PositionOK) then begin
    Exit;
  end;

  if not ((FState.IsMapMoving) or (FMapZoomAnimtion)) then begin
    FConfig.GPSBehaviour.LockRead;
    try
      VMapMove := FConfig.GPSBehaviour.MapMove;
      VMapMoveCentred := FConfig.GPSBehaviour.MapMoveCentered;
      VMinDelta := FConfig.GPSBehaviour.MinMoveDelta;
      VProcessGPSIfActive := FConfig.GPSBehaviour.ProcessGPSIfActive;
    finally
      FConfig.GPSBehaviour.UnlockRead;
    end;
    if (not VProcessGPSIfActive) or (Screen.ActiveForm = Self) then begin
      if (VMapMove) then begin
        VGPSNewPos := VPosition.LonLat;
        if VMapMoveCentred then begin
          VLocalConverter := FViewPortState.View.GetStatic;
          VProjection := VLocalConverter.Projection;
          VCenterMapPoint := VLocalConverter.GetCenterMapPixelFloat;
          VGPSMapPoint := VProjection.LonLat2PixelPosFloat(VGPSNewPos);
          VPointDelta.X := VCenterMapPoint.X - VGPSMapPoint.X;
          VPointDelta.Y := VCenterMapPoint.Y - VGPSMapPoint.Y;
          VDelta := Sqrt(Sqr(VPointDelta.X) + Sqr(VPointDelta.Y));
          if VDelta > VMinDelta then begin
            FViewPortState.ChangeLonLat(VGPSNewPos);
          end;
        end else begin
          VLocalConverter := FViewPortState.View.GetStatic;
          VProjection := VLocalConverter.Projection;
          VGPSMapPoint := VProjection.LonLat2PixelPosFloat(VGPSNewPos);
          if PixelPointInRect(VGPSMapPoint, VLocalConverter.GetRectInMapPixelFloat) then begin
            VCenterMapPoint := VLocalConverter.GetCenterMapPixelFloat;
            VCenterToGPSDelta.X := VGPSMapPoint.X - VCenterMapPoint.X;
            VCenterToGPSDelta.Y := VGPSMapPoint.Y - VCenterMapPoint.Y;
            VPointDelta := FCenterToGPSDelta;
            if PointIsEmpty(VPointDelta) then begin
              FCenterToGPSDelta := VCenterToGPSDelta;
            end else begin
              VPointDelta.X := VCenterToGPSDelta.X - VPointDelta.X;
              VPointDelta.Y := VCenterToGPSDelta.Y - VPointDelta.Y;
              VDelta := Sqrt(Sqr(VPointDelta.X) + Sqr(VPointDelta.Y));
              if VDelta > VMinDelta then begin
                FViewPortState.ChangeMapPixelByLocalDelta(VPointDelta);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.GPSReceiverStateChange;
begin
  actGpsConnect.Enabled := False;
end;

procedure TfrmMain.GPSReceiverConnect;
begin
  actGpsConnect.Enabled := True;
  actGpsConnect.Checked := True;
  if FConfig.GPSBehaviour.SensorsAutoShow then begin
    TBXSensorsBar.Visible := True;
  end;
end;

procedure TfrmMain.GPSReceiverConnectError;
begin
  ShowErrorMessage(Handle, SAS_ERR_GpsConnecting);
end;

procedure TfrmMain.GPSReceiverTimeout;
begin
  actGpsConnect.Enabled := True;
  ShowErrorMessage(Handle, SAS_ERR_GpsCommunication);
end;

procedure TfrmMain.NMapStorageInfoClick(Sender: TObject);
var
  VMapType: IMapType;
  VInternalDomainOptions: IInternalDomainOptions;
  VUrl: string;
begin
  // show storage options
  VMapType := FMainMapState.ActiveMap.GetStatic;
  if Assigned(VMapType) then begin
    if Assigned(VMapType.TileStorage) then begin
      if Supports(VMapType.TileStorage, IInternalDomainOptions, VInternalDomainOptions) then begin
        VUrl := CTileStorageOptionsInternalURL + GUIDToString(VMapType.Zmp.GUID);
        GState.InternalBrowser.Navigate(VMapType.Zmp.FileName, VUrl);
      end;
    end;
  end;
end;

procedure TfrmMain.mapMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer;
  Layer: TCustomLayer
);
var
  VClickLonLat: TDoublePoint;
  VClickRect: TRect;
  VClickLonLatRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMouseMapPoint: TDoublePoint;
  VClickMapRect: TDoubleRect;
  VIsClickInMap: Boolean;
  VVectorItem: IVectorDataItem;
  VVectorItems: IVectorItemSubset;
  VMagnetPoint: TDoublePoint;
begin
  FMapHintWindow.HideHint;

  if Layer <> nil then begin
    Exit;
  end;

  FMouseHandler.OnMouseDown(Button, Shift, Point(X, Y));

  if (FMapZoomAnimtion) or
    (ssDouble in Shift) or
    (Button = mbMiddle) or
    (ssRight in Shift) and (ssLeft in Shift) or
    (HiWord(GetKeyState(VK_DELETE)) <> 0) or
    (HiWord(GetKeyState(VK_INSERT)) <> 0)
  then begin
    Exit;
  end;

  Screen.ActiveForm.SetFocusedControl(map);
  VLocalConverter := FViewPortState.View.GetStatic;
  VProjection := VLocalConverter.Projection;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(Point(x, y));
  VIsClickInMap := VProjection.CheckPixelPosFloat(VMouseMapPoint);

  if (Button = mbLeft) and (FState.State <> ao_movemap) then begin
    if FLineOnMapEdit <> nil then begin
      FMovePoint := True;
      if VIsClickInMap then begin
        VClickRect.Left := X - 5;
        VClickRect.Top := Y - 5;
        VClickRect.Right := X + 5;
        VClickRect.Bottom := Y + 5;
        VClickMapRect := VLocalConverter.LocalRect2MapRectFloat(VClickRect);
        VProjection.ValidatePixelRectFloat(VClickMapRect);
        VClickLonLatRect := VProjection.PixelRectFloat2LonLatRect(VClickMapRect);
        if not FLineOnMapEdit.SelectPointInLonLatRect(VClickLonLatRect) then begin
          VVectorItem := nil;
          VMagnetPoint := CEmptyDoublePoint;
          if FConfig.MainConfig.MagnetDraw then begin
            VVectorItems := FLayerMapMarks.FindItems(VLocalConverter, Point(x, y));
            if ((VVectorItems <> nil) and (VVectorItems.Count > 0)) then begin
              VVectorItem := SelectForEdit(VVectorItems, VLocalConverter);
            end;
          end;
          if VVectorItem <> nil then begin
            VMagnetPoint :=
              GetGeometryLonLatNearestPoint(
                VVectorItem.Geometry,
                VLocalConverter.Projection,
                VMouseMapPoint,
                FConfig.MainConfig.MagnetDrawSize
              );
          end;
          if not PointIsEmpty(VMagnetPoint) then begin
            VClickLonLat := VMagnetPoint;
          end else begin
            VClickLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
          end;
          if FState.State = ao_calc_circle then begin
            if FLineOnMapEdit.IsReady then begin
              FLineOnMapEdit.MoveActivePoint(VClickLonLat);
            end else begin
              FLineOnMapEdit.InsertPoint(VClickLonLat);
            end;
          end else begin
            FLineOnMapEdit.InsertPoint(VClickLonLat);
            if
              (FState.State = ao_edit_line) and
              FLineOnMapEdit.IsReady and
              (HiWord(GetKeyState(VK_CONTROL)) <> 0) then
            begin
              ExtendRoute;
              FMovePoint := False;
              Exit;
            end;
          end;
        end;
      end;
    end;
    if FState.State = ao_select_rect then begin
      if not FSelectionRect.IsEmpty then begin
        VProjection.ValidatePixelPosFloat(VMouseMapPoint, False);
        VClickLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
        FSelectionRect.SetNextPoint(VClickLonLat, Shift);
      end;
    end;
    if FState.State = ao_edit_point then begin
      VProjection.ValidatePixelPosFloat(VMouseMapPoint, False);
      VClickLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
      FPointOnMapEdit.Point := VClickLonLat;
      FMovePoint := True;
    end;

    Exit;
  end;

  if (VIsClickInMap) and (Button = mbright) and (FState.State = ao_movemap) then begin
    VVectorItem := nil;
    VVectorItems := FLayerMapMarks.FindItems(VLocalConverter, Point(x, y));
    if (VVectorItems <> nil) and (VVectorItems.Count > 0) then begin
      VVectorItem := SelectForEdit(VVectorItems, VLocalConverter);
    end;
    if not Supports(VVectorItem, IVectorDataItem, FSelectedMark) then begin
      FSelectedMark := nil;
    end;
    map.PopupMenu := MainPopupMenu;
  end else begin
    FState.MapMovingBegin;
    FMapMovingButton := Button;
    FMoveByMouseStartPoint := Point(X, Y);
    FSelectedMark := nil;
    map.PopupMenu := nil;
  end;

  if FSelectedMark <> nil then begin
    // mark selected
    FSelectedWiki := nil;
  end else begin
    // try to select wiki object
    VVectorItems := FWikiLayer.FindItems(VLocalConverter, Point(x, y));
    if (VVectorItems <> nil) and (VVectorItems.Count > 0) then begin
      FSelectedWiki := SelectForEdit(VVectorItems, VLocalConverter);
    end;
  end;
end;

function TfrmMain.SelectForEdit(
  const AList: IVectorItemSubset;
  const ALocalConverter: ILocalCoordConverter
): IVectorDataItem;
var
  VMarksEnum: IEnumUnknown;
  VMark: IVectorDataItem;
  i: integer;
  VPoly: IGeometryLonLatPolygon;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VSize: Double;
  VArea: Double;
  VVectorGeometryProjectedFactory: IGeometryProjectedFactory;
begin
  Result := nil;
  if AList.Count = 1 then begin
    Result := AList.GetItem(0);
    Exit;
  end;
  VMarksEnum := AList.GetEnum;
  while VMarksEnum.Next(1, VMark, @i) = S_OK do begin
    if Supports(VMark.Geometry, IGeometryLonLatPoint) then begin
      Result := VMark;
      Exit;
    end;
  end;
  VMarksEnum := AList.GetEnum;
  while VMarksEnum.Next(1, VMark, @i) = S_OK do begin
    if Supports(VMark.Geometry, IGeometryLonLatLine) then begin
      Result := VMark;
      Exit;
    end;
  end;
  VSize := -1;
  VMarksEnum := AList.GetEnum;
  VVectorGeometryProjectedFactory := GState.VectorGeometryProjectedFactory;
  while VMarksEnum.Next(1, VMark, @i) = S_OK do begin
    if Supports(VMark.Geometry, IGeometryLonLatPolygon, VPoly) then begin
      VProjectedPolygon := VVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
          ALocalConverter.Projection,
          VPoly,
          nil
        );
      VArea := VProjectedPolygon.CalcArea();
      if ((VArea < VSize) or (VSize < 0)) then begin
        Result := VMark;
        VSize := VArea;
      end;
    end;
  end;
end;

procedure TfrmMain.mapMouseUp(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer;
  Layer: TCustomLayer
);
var
  I: Integer;
  VMapProjection: IProjection;
  VSelectionRect: TDoubleRect;
  VIsSelectionFinished: Boolean;
  VPoly: IGeometryLonLatPolygon;
  VPoint: IGeometryLonLatPoint;
  VIsMapMoving: Boolean;
  VMapType: IMapType;
  VIsValidPoint: Boolean;
  VProjection: IProjection;
  VTile: TPoint;
  VLonLat: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
  VMouseMapPoint: TDoublePoint;
  VMouseDownPos: TPoint;
  VMouseMoveDelta: TPoint;
  VVectorItems: IVectorItemSubset;
  VDescription: string;
  VTitle: string;
  VMark: IVectorDataItem;
begin
  FMouseHandler.OnMouseUp(Button, Shift, Point(X, Y));

  if FMapZoomAnimtion then begin
    Exit;
  end;

  if Button = mbMiddle then begin
    FWinPosition.ToggleFullScreen;
    Exit;
  end;

  VIsMapMoving := FState.IsMapMoving and (FMapMovingButton = Button);

  if VIsMapMoving then begin
    FState.MapMovingEnd;
  end;

  if not VIsMapMoving and (Layer <> nil) then begin
    Exit;
  end;

  VLocalConverter := FViewPortState.View.GetStatic;
  if not VIsMapMoving and (Button = mbLeft) then begin
    VProjection := VLocalConverter.Projection;
    VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(Point(x, y));
    VIsValidPoint := VProjection.CheckPixelPosFloat(VMouseMapPoint);

    // Delete or Download tile
    if VIsValidPoint then begin
      VMapType := FMainMapState.ActiveMap.GetStatic;
      VLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
      VMapProjection := VMapType.ProjectionSet.GetSuitableProjection(VProjection);
      if VMapProjection.ProjectionType.CheckLonLatPos(VLonLat) then begin
        VTile :=
          PointFromDoublePoint(
            VMapProjection.LonLat2TilePosFloat(VLonLat),
            prToTopLeft
          );
        if HiWord(GetKeyState(VK_DELETE)) <> 0 then begin
          VMapType.TileStorage.DeleteTile(VTile, VMapProjection.Zoom, VMapType.VersionRequest.GetStatic.BaseVersion);
          Exit;
        end;
        if HiWord(GetKeyState(VK_INSERT)) <> 0 then begin
          TTileDownloaderUIOneTile.Create(
            GState.Config.DownloaderThreadConfig,
            GState.AppClosingNotifier,
            VTile,
            VMapProjection.Zoom,
            VMapType,
            VMapType.VersionRequest.GetStatic.BaseVersion,
            GState.DownloadInfo,
            GState.GlobalInternetState,
            FTileErrorLogger
          );
          Exit;
        end;
      end;
    end;

    // Add placemark
    if FState.State = ao_edit_point then begin
      VProjection.ValidatePixelPosFloat(VMouseMapPoint, False);
      VLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
      FPointOnMapEdit.Point := VLonLat;
      VPoint := GState.VectorGeometryLonLatFactory.CreateLonLatPoint(FPointOnMapEdit.Point);
      if FMarkDBGUI.SaveMarkModal(FEditMarkPoint, VPoint) then begin
        FState.State := ao_movemap;
      end;
      FMovePoint := False;
      Exit;
    end;

    // Select region
    if FState.State = ao_select_rect then begin
      VIsSelectionFinished := not FSelectionRect.IsEmpty;
      VProjection.ValidatePixelPosFloat(VMouseMapPoint, False);
      VLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
      FSelectionRect.SetNextPoint(VLonLat, Shift);
      VSelectionRect := FSelectionRect.GetRect;
      if VIsSelectionFinished then begin
        FSelectionRect.Reset;
        VPoly := GState.VectorGeometryLonLatFactory.CreateLonLatPolygonByRect(VSelectionRect);
        FState.State := ao_movemap;
        FRegionProcess.ProcessPolygonWithZoom(VProjection.Zoom, VPoly); // TODO: replace Zoom in ProcessPolygonWithZoom to smth
      end;
      Exit;
    end;
  end;

  FMovePoint := False;

  if (((FState.State <> ao_movemap) and (Button = mbLeft)) or
     ((FState.State = ao_movemap) and (Button = mbRight))) then begin
    Exit;
  end;

  map.Enabled := False;
  map.Enabled := True;

  VMouseDownPos := FMouseState.GetLastDownPos(Button);
  VMouseMoveDelta := Point(VMouseDownPos.x - X, VMouseDownPos.y - y);

  if VIsMapMoving and ((VMouseMoveDelta.X <> 0) or (VMouseMoveDelta.Y <> 0)) then begin
    MapMoveAnimate(
      FMouseState.CurentSpeed,
      FViewPortState.View.GetStatic.Projection.Zoom,
      FMouseState.GetLastUpPos(Button)
    );
  end;

  if (VMouseMoveDelta.X = 0) and (VMouseMoveDelta.Y = 0) and
     (FState.State = ao_movemap) and (Button = mbLeft)
  then begin
    // Update SunCalc location
    if FConfig.LayersConfig.SunCalcConfig.Visible then begin
      VProjection := VLocalConverter.Projection;
      VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(Point(X, Y));
      VProjection.ValidatePixelPosFloat(VMouseMapPoint, False);
      VLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
      FSunCalcProvider.Location := VLonLat;
      Exit;
    end;
    // Process vector items
    VVectorItems := FindItems(VLocalConverter, Point(X, Y));
    if (VVectorItems <> nil) and (VVectorItems.Count > 0) then begin
      if (ssCtrl in Shift) then begin
        // Add polygon(s) to Merge Polygons tool
        for I := 0 to VVectorItems.Count - 1 do begin
          if Supports(VVectorItems.Items[I].Geometry, IGeometryLonLatPolygon) then begin
            FMergePolygonsPresenter.AddVectorItems(VVectorItems);
            Exit;
          end;
        end;
        // Show Elevation Profile for path
        for I := 0 to VVectorItems.Count - 1 do begin
          if Supports(VVectorItems.Items[I].Geometry, IGeometryLonLatLine) then begin
            VProjection := VLocalConverter.Projection;
            VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(Point(X, Y));
            VProjection.ValidatePixelPosFloat(VMouseMapPoint, False);
            VLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);
            FElevationProfilePresenter.ShowProfile(VVectorItems.Items[I], @VLonLat);
            Exit;
          end;
        end;
      end;
      // Show placemark(s) description
      if THtmlDoc.FromVectorItemsDescription(VVectorItems, VTitle, VDescription) then begin
        GState.InternalBrowser.ShowMessage(VTitle, VDescription);
      end else begin
        VMark := VVectorItems.Items[0];
        Assert(VMark.GetInfoUrl <> '');
        GState.InternalBrowser.Navigate(VMark.GetInfoCaption, VMark.GetInfoUrl + CVectorItemDescriptionSuffix);
      end;
    end;
  end;
end;

procedure TfrmMain.mapMouseMove(
  Sender: TObject;
  Shift: TShiftState;
  AX, AY: Integer;
  Layer: TCustomLayer
);

  function _AllowShowHint: Boolean;
  var
    hf: HWND;
    dwProcessId: DWORD;
  begin
    // do not capture focus on mouse hovering
    hf := GetForegroundWindow;
    if Self.HandleAllocated and (Self.Handle = hf) then begin
      // foreground
      Result := True;
    end else begin
      // we have foreground window
      GetWindowThreadProcessId(hf, dwProcessId);
      Result := (dwProcessId = GetCurrentProcessId);
    end;
  end;

var
  VProjection: IProjection;
  VLonLat: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
  VMouseMapPoint: TDoublePoint;
  VMouseMoveDelta: TPoint;
  VMousePosPrev: TPoint;
  VMousePos: TPoint;
  VVectorItem: IVectorDataItem;
  VMagnetPoint: TDoublePoint;
  VVectorItems: IVectorItemSubset;
  VShowExtendedHint: Boolean;
begin
  VMousePosPrev := FMouseState.CurentPos;
  FMouseHandler.OnMouseMove(Shift, Point(AX, AY));
  VMousePos := FMouseState.CurentPos;

  if not FState.IsMapMoving and (Layer <> nil) then begin
    Exit;
  end;

  if FMapZoomAnimtion or (ssDouble in Shift) then begin
    Exit;
  end;

  VLocalConverter := FViewPortState.View.GetStatic;
  VProjection := VLocalConverter.Projection;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(VMousePos);
  VProjection.ValidatePixelPosFloatStrict(VMouseMapPoint, False);
  VLonLat := VProjection.PixelPosFloat2LonLat(VMouseMapPoint);

  if (FLineOnMapEdit <> nil) and FMovePoint then begin
    VMagnetPoint := CEmptyDoublePoint;
    if FConfig.MainConfig.MagnetDraw then begin
      if ssShift in Shift then begin
        VMagnetPoint := FStickToGrid.PointStick(VLocalConverter.Projection, VLonLat);
      end;

      VVectorItem := nil;
      VVectorItems := FLayerMapMarks.FindItems(VLocalConverter, VMousePos);
      if (VVectorItems <> nil) and (VVectorItems.Count > 0) then begin
        VVectorItem := SelectForEdit(VVectorItems, VLocalConverter);
      end;
      if VVectorItem <> nil then begin
        VMagnetPoint :=
          GetGeometryLonLatNearestPoint(
            VVectorItem.Geometry,
            VLocalConverter.Projection,
            VMouseMapPoint,
            FConfig.MainConfig.MagnetDrawSize
          );
      end;
      if not PointIsEmpty(VMagnetPoint) then begin
        VLonLat := VMagnetPoint;
      end;
    end;
    FLineOnMapEdit.MoveActivePoint(VLonLat);
    Exit;
  end;

  if (FState.State = ao_edit_point) and FMovePoint then begin
    FPointOnMapEdit.Point := VLonLat;
  end;

  if FState.State = ao_select_rect then begin
    if not FSelectionRect.IsEmpty then begin
      FSelectionRect.SetNextPoint(VLonLat, Shift);
    end;
  end;

  if FWinPosition.IsFullScreen or not FWinPosition.IsBordersVisible then begin
    if VMousePos.Y < 10 then begin
      TBDock.Parent := map;
      TBDock.Visible := True;
    end else begin
      TBDock.Visible := False;
      TBDock.Parent := Self;
    end;
    if VMousePos.X < 10 then begin
      TBDockLeft.Parent := map;
      TBDockLeft.Visible := True;
    end else begin
      TBDockLeft.Visible := False;
      TBDockLeft.Parent := Self;
    end;
    if VMousePos.Y > Map.Height - 10 then begin
      TBDockBottom.Parent := map;
      TBDockBottom.Visible := True;
    end else begin
      TBDockBottom.Visible := False;
      TBDockBottom.Parent := Self;
    end;
    if VMousePos.X > Map.Width - 10 then begin
      TBDockRight.Parent := map;
      TBDockRight.Visible := True;
    end else begin
      TBDockRight.Visible := False;
      TBDockRight.Parent := Self;
    end;
  end;

  if FMapMoveAnimtion then begin
    Exit;
  end;

  if FState.IsMapMoving then begin
    VMouseMoveDelta := Point(FMoveByMouseStartPoint.X - VMousePos.X, FMoveByMouseStartPoint.Y - VMousePos.Y);
    FMoveByMouseStartPoint := VMousePos;
    FViewPortState.ChangeMapPixelByLocalDelta(DoublePoint(VMouseMoveDelta));
  end;

  // Hint
  if (not FMapMoveAnimtion) and
    (not FState.IsMapMoving) and
    ((VMousePos.X <> VMousePosPrev.X) or (VMousePos.Y <> VMousePosPrev.Y)) and
    (FConfig.MainConfig.ShowHintOnMarks) and
    ((not FConfig.MainConfig.ShowHintOnlyInMapMoveMode) or (FState.State = ao_movemap)) and
    _AllowShowHint
  then begin
    VVectorItems := nil;
    VShowExtendedHint := ssCtrl in Shift;

    if not VShowExtendedHint and
       (FLineOnMapEdit <> nil) and
       (FState.State = ao_edit_line) and
       FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.Visible and
       FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.ShowPointHint
    then begin
      VVectorItems := FindPathOnMapEdit(VLocalConverter, VMousePos);
      VShowExtendedHint := VShowExtendedHint or ((VVectorItems <> nil) and (VVectorItems.Count > 0));
    end;

    if VVectorItems = nil then begin
      VVectorItems := FindItems(VLocalConverter, VMousePos);
    end;

    if (VVectorItems <> nil) and (VVectorItems.Count > 0) then begin
      FMapHintWindow.ShowHint(VMousePos, VVectorItems, VShowExtendedHint);
    end else begin
      FMapHintWindow.HideHint;
    end;
  end;
end;

procedure CreateLink(const PathObj, PathLink, Desc, Param: string);
var
  IObject: IUnknown;
  SLink: IShellLink;
  PFile: IPersistFile;
begin
  IObject := CreateComObject(CLSID_ShellLink);
  SLink := IObject as IShellLink;
  PFile := IObject as IPersistFile;
  with SLink do begin
    SetArguments(PChar(Param));
    SetDescription(PChar(Desc));
    SetPath(PChar(PathObj));
  end;
  PFile.Save(POleStr(UnicodeString(PathLink)), False);
end;

procedure TfrmMain.tbitmSelectVersionByMarkClick(Sender: TObject);
var
  VMark: IVectorDataItem;
  VMapType: IMapType;
  VInternalDomainOptions: IInternalDomainOptions;
  VBase, VDesc, VVersionStr: String;
  VFlags: TDomainOptionsResponseFlags;
begin
  // select version by selected placemark description
  VMark := FSelectedMark;
  if VMark <> nil then begin
    VMapType := FMainMapState.ActiveMap.GetStatic;
    if Assigned(VMapType) then begin
      if Assigned(VMapType.TileStorage) then begin
        if Supports(VMapType.TileStorage, IInternalDomainOptions, VInternalDomainOptions) then begin
          VBase := CTileStorageOptionsInternalURL + GUIDToString(VMapType.Zmp.GUID);
          VDesc := VMark.Desc;
      // get version to open
          if (0 < Length(VDesc)) then begin
            if VInternalDomainOptions.DomainHtmlOptions(VBase, VDesc, VVersionStr, VFlags, c_IDO_RT_SelectVersionByDescription) then begin
              if (0 < Length(VVersionStr)) then begin
        // switch to given VVersionStr
                VMapType.VersionRequestConfig.Version := VVersionStr;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.TBEditPathClose(Sender: TObject);
begin
  FState.State := ao_movemap;
end;

procedure TfrmMain.NMarkNavClick(Sender: TObject);
var
  VLonLat: TDoublePoint;
  VMark: IVectorDataItem;
  VMarkStringId: string;
begin
  VMark := FSelectedMark;
  if VMark <> nil then begin
    if (not NMarkNav.Checked) then begin
      VLonLat := VMark.Geometry.GetGoToPoint;
      VMarkStringId := FMarkDBGUI.MarksDb.GetStringIdByMark(VMark);
      FConfig.NavToPoint.StartNavToMark(VMarkStringID, VLonLat);
    end else begin
      FConfig.NavToPoint.StopNav;
    end;
  end;
end;

procedure TfrmMain.AdjustFont(
  Item: TTBCustomItem;
  Viewer: TTBItemViewer;
  Font: TFont;
  StateFlags: Integer
);
begin
  if TTBXItem(Item).Checked then begin
    TTBXItem(Item).FontSettings.Bold := tsTrue;
  end else begin
    TTBXItem(Item).FontSettings.Bold := tsDefault;
  end;
end;

procedure TfrmMain.FormMouseWheel(
  Sender: TObject;
  Shift: TShiftState;
  WheelDelta: Integer;
  MousePos: TPoint;
  var Handled: Boolean
);
var
  z: integer;
  VZoom: Byte;
  VNewZoom: integer;
  VMousePos: TPoint;
  VFreezePos: TPoint;
  VLocalConverter: ILocalCoordConverter;
begin
  if not Handled then begin
    if not FConfig.MainConfig.DisableZoomingByMouseScroll then begin
      if Types.PtInRect(map.BoundsRect, Self.ScreenToClient(MousePos)) then begin
        if not FMapZoomAnimtion then begin
          VMousePos := map.ScreenToClient(MousePos);
          if FConfig.MainConfig.MouseScrollInvert then begin
            z := -1;
          end else begin
            z := 1;
          end;
          VLocalConverter := FViewPortState.View.GetStatic;
          VZoom := VLocalConverter.Projection.Zoom;
          if WheelDelta < 0 then begin
            VNewZoom := VZoom - z;
          end else begin
            VNewZoom := VZoom + z;
          end;
          if VNewZoom < 0 then begin
            VNewZoom := 0;
          end;

          if FConfig.MapZoomingConfig.ZoomingAtMousePos then begin
            VFreezePos := VMousePos;
          end else begin
            VFreezePos := CenterPoint(VLocalConverter.GetLocalRect);
          end;

          MapZoom(VNewZoom, VFreezePos);
        end;
        Handled := True;
      end;
    end;
  end;
end;

procedure TfrmMain.NMapInfoClick(Sender: TObject);
var
  VMapType: IMapType;
  VUrl: string;
begin
  if TComponent(Sender).Tag <> 0 then begin
    VMapType := IMapType(TComponent(Sender).Tag);
  end else begin
    VMapType := FMainMapState.ActiveMap.GetStatic;
  end;
  VUrl := VMapType.GUIConfig.InfoUrl.Value;
  if VUrl <> '' then begin
    VUrl := CZmpInfoInternalURL + GUIDToString(VMapType.Zmp.GUID) + VUrl;
    GState.InternalBrowser.Navigate(VMapType.Zmp.FileName, VUrl);
  end;
end;

procedure TfrmMain.SaveConfig(Sender: TObject);
begin
  try
    GState.SaveMainParams;
    SaveWindowConfigToIni(GState.MainConfigProvider);
  except
  end;
end;

procedure TfrmMain.SavePosition(const AProvider: IConfigDataWriteProvider);
var
  VLonLat: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
begin
  inherited;
  VLocalConverter := FViewPortState.View.GetStatic;
  VLonLat := VLocalConverter.GetCenterLonLat;
  AProvider.WriteInteger('Zoom', VLocalConverter.Projection.Zoom);
  AProvider.WriteFloat('X', VLonLat.X);
  AProvider.WriteFloat('Y', VLonLat.Y);
end;

procedure TfrmMain.SaveWindowConfigToIni(const AProvider: IConfigDataWriteProvider);
var
  VProvider: IConfigDataWriteProvider;
begin
  VProvider := AProvider.GetOrCreateSubItem('HOTKEY');
  FShortCutManager.Save(VProvider);

  VProvider := FStateConfigDataProvider.GetOrCreateSubItem('MainForm');
  FWinPosition.WriteConfig(VProvider);

  VProvider := FStateConfigDataProvider.GetOrCreateSubItem('Position');
  SavePosition(VProvider);

  FConfig.WriteConfig(AProvider);

  FPanelPositionSaveLoad.Save(Self);
end;

procedure TfrmMain.TBXSensorsBarVisibleChanged(Sender: TObject);
begin
  NSensors.Checked := TTBXToolWindow(Sender).Visible;
end;

procedure TfrmMain.tbxtmAddToMergePolygonsClick(Sender: TObject);
var
  VMouseDownPos: TPoint;
  VVectorItems: IVectorItemSubset;
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FViewPortState.View.GetStatic;
  VMouseDownPos := FMouseState.GetLastDownPos(mbRight);
  VVectorItems := FindItems(VLocalConverter, VMouseDownPos);
  if (VVectorItems <> nil) and (VVectorItems.Count > 0) then begin
    FMergePolygonsPresenter.AddVectorItems(VVectorItems);
  end;
end;

procedure TfrmMain.tbxitmElevationProfileClick(Sender: TObject);
var
  I: Integer;
  VMouseDownPos: TPoint;
  VVectorItems: IVectorItemSubset;
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FViewPortState.View.GetStatic;
  VMouseDownPos := FMouseState.GetLastDownPos(mbRight);
  VVectorItems := FindItems(VLocalConverter, VMouseDownPos);
  if VVectorItems <> nil then begin
    for I := 0 to VVectorItems.Count - 1 do begin
      if Supports(VVectorItems.Items[I].Geometry, IGeometryLonLatLine) then begin
        FElevationProfilePresenter.ShowProfile(VVectorItems.Items[I]);
        Break;
      end;
    end;
  end;
end;

procedure TfrmMain.NSensorsClick(Sender: TObject);
begin
  TBXSensorsBar.Visible := TTBXItem(Sender).Checked;
end;

procedure TfrmMain.tbitmMarkEditPropertiesClick(Sender: TObject);
var
  VMark: IVectorDataItem;
  VMarkModifed: IVectorDataItem;
  VVisible: Boolean;
  VResult: IVectorDataItem;
begin
  VMark := FSelectedMark;
  if VMark <> nil then begin
    VVisible := FMarkDBGUI.MarksDb.MarkDb.GetMarkVisible(VMark);
    VMarkModifed := FMarkDBGUI.EditMarkModal(VMark, False, VVisible);
    if VMarkModifed <> nil then begin
      VResult := FMarkDBGUI.MarksDb.MarkDb.UpdateMark(VMark, VMarkModifed);
      if VResult <> nil then begin
        FMarkDBGUI.MarksDb.MarkDb.SetMarkVisible(VResult, VVisible);
      end;
    end;
  end;
end;

procedure TfrmMain.tbxCopyUrlToOpenStreetMapClick(Sender: TObject);
var
  VRequest: IDownloadRequest;
begin
  VRequest := FUrlProviderOSM.GetUrl(FViewPortState.View.GetStatic, FMouseState.GetLastDownPos(mbRight));
  if Assigned(VRequest) then begin
    CopyDownloadRequestToClipboard(Handle, VRequest);
  end;
end;

procedure TfrmMain.ProcessOpenFiles(const AFiles: IStringListStatic);
begin
  if Assigned(AFiles) and (AFiles.Count > 0) then begin
    u_CmdLineArgProcessorHelpers.ProcessOpenFiles(
      AFiles,
      FMapGoto,
      FFormRegionProcess,
      True, // start downloads from *.sls in paused state
      True, // show import dialog for vector data (*.kml/*.gpx/etc.)
      FMarkDBGUI,
      nil,
      nil,
      nil,
      FTileStorageImporter
    );
  end;
end;

procedure TfrmMain.OnShowMergePolygons(Sender: TObject);
begin
  tbMergePolygons.Show;
  if FWinPosition.IsFullScreen then begin
    TBDockLeft.Parent := map;
    TBDockLeft.Visible := True;
  end;
end;

procedure TfrmMain.tbMergePolygonsClose(Sender: TObject);
begin
  FMergePolygonsPresenter.ClearAll;
end;

procedure TfrmMain.TBSearchWindowClose(Sender: TObject);
begin
  GState.LastSearchResult.ClearGeoCodeResult;
  if tbxpmnSearchResult.Tag <> 0 then begin
    IInterface(tbxpmnSearchResult.Tag)._Release;
    tbxpmnSearchResult.Tag := 0;
  end;
end;

procedure TfrmMain.tbiEditSrchAcceptText(
  Sender: TObject;
  var NewText: String;
  var Accept: Boolean
);
var
  VResult: IGeoCodeResult;
  VItem: IGeoCoderListEntity;
  VLocalConverter: ILocalCoordConverter;
  VText: string;
  VNotifier: INotifierOperation;
begin
  if Assigned(Sender) then begin
    VItem := IGeoCoderListEntity(TComponent(Sender).Tag);
    if VItem <> nil then begin
      VLocalConverter := FViewPortState.View.GetStatic;
      VText := Trim(NewText);
      VNotifier := TNotifierOperationFake.Create;
      VResult := VItem.GetGeoCoder.GetLocations(VNotifier, VNotifier.CurrentOperation, VText, VLocalConverter);
      FConfig.SearchHistory.AddItem(VText);
      FSearchPresenter.ShowSearchResults(VResult);
    end;
  end;
end;

procedure TfrmMain.NSRTM3Click(Sender: TObject);
var
  VRequest: IDownloadRequest;
begin
  VRequest := FUrlProviderSTRM3.GetUrl(FViewPortState.View.GetStatic, FMouseState.GetLastDownPos(mbRight));
  if Assigned(VRequest) then begin
    //GState.DownloaderFactory.BuildDownloaderAsync(True, True, False, nil).DoRequestAsync()
    GState.InternalBrowser.NavigateByRequest('http://api.geonames.org/srtm3', VRequest);
  end;
end;

procedure TfrmMain.NGTOPO30Click(Sender: TObject);
var
  VRequest: IDownloadRequest;
begin
  VRequest := FUrlProviderGTopo30.GetUrl(FViewPortState.View.GetStatic, FMouseState.GetLastDownPos(mbRight));
  if Assigned(VRequest) then begin
    GState.InternalBrowser.NavigateByRequest('http://api.geonames.org/gtopo30', VRequest);
  end;
end;

procedure TfrmMain.tbxCopyUrlToGoogleMapsClick(Sender: TObject);
var
  VRequest: IDownloadRequest;
begin
  VRequest := FUrlProviderGoogle.GetUrl(FViewPortState.View.GetStatic, FMouseState.GetLastDownPos(mbRight));
  if Assigned(VRequest) then begin
    CopyDownloadRequestToClipboard(Handle, VRequest);
  end;
end;

procedure TfrmMain.tbxCopyUrlToGoogleEarthWebClick(Sender: TObject);
var
  VRequest: IDownloadRequest;
begin
  VRequest := FUrlProviderGoogleEarthWeb.GetUrl(FViewPortState.View.GetStatic, FMouseState.GetLastDownPos(mbRight));
  if Assigned(VRequest) then begin
    CopyDownloadRequestToClipboard(Handle, VRequest);
  end;
end;

procedure TfrmMain.tbxCopyUrlToYandexMapsClick(Sender: TObject);
var
  VRequest: IDownloadRequest;
begin
  VRequest := FUrlProviderYandex.GetUrl(FViewPortState.View.GetStatic, FMouseState.GetLastDownPos(mbRight));
  if Assigned(VRequest) then begin
    CopyDownloadRequestToClipboard(Handle, VRequest);
  end;
end;

procedure TfrmMain.tbxCopyUrlToKosmosnimkiRuClick(Sender: TObject);
var
  VRequest: IDownloadRequest;
begin
  VRequest := FUrlProviderKosmosnimki.GetUrl(FViewPortState.View.GetStatic, FMouseState.GetLastDownPos(mbRight));
  if Assigned(VRequest) then begin
    CopyDownloadRequestToClipboard(Handle, VRequest);
  end;
end;

procedure TfrmMain.tbxCopyUrlToNakarteMeClick(Sender: TObject);
var
  VRequest: IDownloadRequest;
begin
  VRequest := FUrlProviderNakarte.GetUrl(FViewPortState.View.GetStatic, FMouseState.GetLastDownPos(mbRight));
  if Assigned(VRequest) then begin
    CopyDownloadRequestToClipboard(Handle, VRequest);
  end;
end;

procedure TfrmMain.tbxCopyUrlToBingMapsClick(Sender: TObject);
var
  VRequest: IDownloadRequest;
begin
  VRequest := FUrlProviderBing.GetUrl(FViewPortState.View.GetStatic, FMouseState.GetLastDownPos(mbRight));
  if Assigned(VRequest) then begin
    CopyDownloadRequestToClipboard(Handle, VRequest);
  end;
end;

procedure TfrmMain.LoadMapIconsList;
var
  VMapType: IMapType;
  VList18: TMapTypeIconsList;
  VList24: TMapTypeIconsList;
  i: Integer;
begin
  VList18 := TMapTypeIconsList.Create(18, 18);
  FMapTypeIcons18List := VList18;

  VList24 := TMapTypeIconsList.Create(24, 24);
  FMapTypeIcons24List := VList24;

  for i := 0 to GState.MapType.FullMapsSet.Count - 1 do begin
    VMapType := GState.MapType.FullMapsSet.Items[i];
    VList18.Add(VMapType.GUID, VMapType.Zmp.GUI.Bmp18);
    VList24.Add(VMapType.GUID, VMapType.Zmp.GUI.Bmp24);
  end;
end;

procedure TfrmMain.ProcessCmdLineArgs;
begin
  TCmdLineArgProcessorAsync.Create(
    GState.MarksDb,
    GState.AppClosingNotifier,
    FArgProcessor,
    FFormRegionProcess
  );
end;

procedure TfrmMain.LoadPosition;
var
  VConfigData: IConfigDataProvider;
  VLonLat: TDoublePoint;
  VProjectionSet: IProjectionSet;
  VZoom: Byte;
  VProjection: IProjection;
  VLocalConverter: ILocalCoordConverter;
begin
  inherited;
  VConfigData := FStateConfigDataProvider.GetSubItem('Position');
  if VConfigData <> nil then begin
    VLocalConverter := FViewPortState.View.GetStatic;
    VProjectionSet := FActiveProjectionSet.GetStatic;
    VZoom := VConfigData.ReadInteger('Zoom', VLocalConverter.Projection.Zoom);
    VProjectionSet.ValidateZoom(VZoom);
    VProjection := VProjectionSet.Zooms[VZoom];
    VLonLat := VLocalConverter.GetCenterLonLat;
    VLonLat.X := VConfigData.ReadFloat('X', VLonLat.X);
    VLonLat.Y := VConfigData.ReadFloat('Y', VLonLat.Y);
    VProjection.ProjectionType.ValidateLonLatPos(VLonLat);

    FViewPortState.ChangeLonLatAndZoom(VZoom, VLonLat);
  end;
end;

procedure TfrmMain.MainPopupMenuPopup(Sender: TObject);
var
  i: Integer;
  VMapType: IMapType;
  VLayerIsActive: Boolean;
  VActiveLayersSet: IGUIDSetStatic;
  VMenuItem: TTBXItem;
  VGUID: TGUID;
  VGUIDList: IGUIDListStatic;
  VMark: IVectorDataItem;
  VMarkStringId: string;
  VInternalDomainOptions: IInternalDomainOptions;
begin
  VMark := FSelectedMark;
  tbitmMarkEditPosition.Visible := VMark <> nil;
  tbitmFitMarkToScreen.Visible :=
    Assigned(VMark) and
    (Supports(VMark.Geometry, IGeometryLonLatLine) or
    Supports(VMark.Geometry, IGeometryLonLatPolygon));
  if VMark <> nil then begin
    tbitmHideThisMark.Visible := not FConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IgnoreMarksVisible;
  end else begin
    tbitmHideThisMark.Visible := False;
  end;

  tbxtmAddToMergePolygons.Visible :=
    (Assigned(VMark) and Supports(VMark.Geometry, IGeometryLonLatPolygon)) or
    (FSelectedWiki <> nil);

  tbxitmElevationProfile.Visible :=
    Assigned(VMark) and Supports(VMark.Geometry, IGeometryLonLatLine);

  tbxFillingMap.Visible :=
    (Assigned(VMark) and Supports(VMark.Geometry, IGeometryLonLatPolygon)) or
    (FSelectedWiki <> nil);

  tbitmMarkEditProperties.Visible := VMark <> nil;
  NMarkExport.Visible := VMark <> nil;
  NMarkDel.Visible := VMark <> nil;
  tbsprtMainPopUp0.Visible := VMark <> nil;
  NMarkOper.Visible := (VMark <> nil) or (FSelectedWiki <> nil);
  NMarkNav.Visible := VMark <> nil;
  NMarkPlay.Visible := (VMark <> nil) and (FPlacemarkPlayerPlugin <> nil) and (FPlacemarkPlayerPlugin.Available);
  tbitmMarkInfo.Visible := (VMark <> nil);
  tbitmCopyToClipboardGenshtabName.Visible := FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Visible;
  if (VMark <> nil) and (FConfig.NavToPoint.IsActive) and (FConfig.NavToPoint.MarkId <> '') then begin
    VMarkStringId := FMarkDBGUI.MarksDb.GetStringIdByMark(VMark);
    if (VMarkStringID = FConfig.NavToPoint.MarkId) then begin
      NMarkNav.Checked := True;
    end else begin
      NMarkNav.Checked := False;
    end;
  end else begin
    NMarkNav.Checked := False;
  end;
  ldm.Visible := False;
  dlm.Visible := False;
  TBOpenDirLayer.Visible := False;
  TBCopyLinkLayer.Visible := False;
  TBLayerInfo.Visible := False;
  VActiveLayersSet := FConfig.MapLayersConfig.LayerGuids;
  VGUIDList := GState.MapType.GUIConfigList.OrderedMapGUIDList;
  for i := 0 to VGUIDList.Count - 1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := GState.MapType.FullMapsSet.GetMapTypeByGUID(VGUID);
    if (VMapType.Zmp.IsLayer) then begin
      VLayerIsActive := Assigned(VActiveLayersSet) and VActiveLayersSet.IsExists(VGUID);
      TTBXItem(FNDwnItemList.GetByGUID(VGUID)).Visible := VLayerIsActive;
      TTBXItem(FNDelItemList.GetByGUID(VGUID)).Visible := VLayerIsActive;
      TTBXItem(FNOpenDirItemList.GetByGUID(VGUID)).Visible := VLayerIsActive;
      TTBXItem(FNCopyLinkItemList.GetByGUID(VGUID)).Visible := VLayerIsActive;
      VMenuItem := TTBXItem(FNLayerInfoItemList.GetByGUID(VGUID));
      VMenuItem.Visible := VLayerIsActive;
      if VLayerIsActive then begin
        VMenuItem.Enabled := VMapType.GUIConfig.InfoUrl.Value <> '';
      end;
      if VLayerIsActive then begin
        ldm.Visible := True;
        dlm.Visible := True;
        TBCopyLinkLayer.Visible := True;
        TBOpenDirLayer.Visible := True;
        TBLayerInfo.Visible := True;
      end;
    end;
  end;
  // current map
  VMapType := FMainMapState.ActiveMap.GetStatic;
  // allow to view map info
  NMapInfo.Enabled := VMapType.GUIConfig.InfoUrl.Value <> '';

  // allow to show Map Storage Info
  NMapStorageInfo.Visible := Supports(VMapType.TileStorage, IInternalDomainOptions, VInternalDomainOptions);
  NMapStorageInfo.Enabled := NMapStorageInfo.Visible;

  // allow to clear or select versions
  tbpmiClearVersion.Visible := (0 < Length(VMapType.VersionRequest.GetStatic.BaseVersion.StoreString));
  // make and select version by placemark
  tbitmMakeVersionByMark.Visible := (VMark <> nil) and (VInternalDomainOptions <> nil);
  tbitmSelectVersionByMark.Visible := tbitmMakeVersionByMark.Visible;
  // versions submenu
  tbpmiVersions.Visible := (VMapType.TileStorage.StorageTypeAbilities.VersionSupport = tstvsMultiVersions) or tbpmiClearVersion.Visible or tbitmMakeVersionByMark.Visible;
end;

procedure TfrmMain.NParamsPopup(
  Sender: TTBCustomItem;
  FromLink: Boolean
);
var
  i: Integer;
  VMapType: IMapType;
  VLayerIsActive: Boolean;
  VActiveLayersSet: IGUIDSetStatic;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  NLayerParams.Visible := False;
  VActiveLayersSet := FConfig.MapLayersConfig.LayerGuids;
  VGUIDList := GState.MapType.GUIConfigList.OrderedMapGUIDList;
  for i := 0 to VGUIDList.Count - 1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := GState.MapType.FullMapsSet.GetMapTypeByGUID(VGUID);
    if (VMapType.Zmp.IsLayer) then begin
      VLayerIsActive := Assigned(VActiveLayersSet) and VActiveLayersSet.IsExists(VGUID);
      TTBXItem(FNLayerParamsItemList.GetByGUID(VGUID)).Visible := VLayerIsActive;
      if VLayerIsActive then begin
        NLayerParams.Visible := True;
      end;
    end;
  end;
end;

procedure TfrmMain.MakeBackupOfPathOnMapEdit(
  const APathOnMapEdit: IPathOnMapEdit;
  const AIncludeActivePoint: Boolean
);
begin
  FRouteUndoPath := TPathOnMapEdit.Create(GState.VectorGeometryLonLatFactory);
  FRouteUndoPath.SetPath(APathOnMapEdit.Path);
  if not AIncludeActivePoint then begin
    FRouteUndoPath.DeleteActivePoint;
  end;
end;

procedure TfrmMain.TBEditPathMarshClick(Sender: TObject);
var
  VRoute: IGeometryLonLatLine;
  VEntity: IPathDetalizeProviderTreeEntity;
  VProvider: IPathDetalizeProvider;
  VIsError: Boolean;
  VErrorMessage: string;
  VInterface: IInterface;
  VPathOnMapEdit: IPathOnMapEdit;
  VOperationNotifier: INotifierOperation;
begin
  if Supports(FLineOnMapEdit, IPathOnMapEdit, VPathOnMapEdit) then begin
    VInterface := IInterface(TComponent(Sender).tag);
    if Supports(VInterface, IPathDetalizeProviderTreeEntity, VEntity) then begin
      VProvider := VEntity.GetProvider;
      VIsError := True;
      try
        VOperationNotifier := TNotifierOperationFake.Create;
        VRoute :=
          VProvider.GetRoute(
            VOperationNotifier,
            VOperationNotifier.CurrentOperation,
            VPathOnMapEdit.Path.Geometry,
            FRouteComment,
            VErrorMessage
          );
        VIsError := (VRoute = nil);
      except
        on E: Exception do begin
          ShowErrorMessage(Handle, E.ClassName + ': ' + E.Message);
        end;
      end;

      if not VIsError then begin
        MakeBackupOfPathOnMapEdit(VPathOnMapEdit, True);
        VPathOnMapEdit.SetPath(VRoute);
      end else begin
        FRouteComment := '';
        if VErrorMessage <> '' then begin
          ShowErrorMessage(Handle, VErrorMessage);
        end;
      end;
    end;
  end;
  actEditPathRouteCalcUndo.Enabled := FRouteUndoPath <> nil;
end;

procedure TfrmMain.tbxExtendRouteSelect(Sender: TObject);
var
  VEntity: IPathDetalizeProviderTreeEntity;
  VInterface: IInterface;
begin
  VInterface := IInterface(TComponent(Sender).tag);
  if Supports(VInterface, IPathDetalizeProviderTreeEntity, VEntity) then begin
    GState.Config.PathDetalizeConfig.DefaultProvider := VEntity.GUID;
  end;
end;

procedure TfrmMain.ExtendRoute;

  function GetPathDetalizeProvider(
    const ATree: IStaticTreeItem;
    const AGuid: TGUID
  ): IPathDetalizeProvider;
  var
    I: Integer;
    VEntity: IPathDetalizeProviderTreeEntity;
  begin
    Result := nil;
    if Supports(ATree.Data, IPathDetalizeProviderTreeEntity, VEntity) then begin
      if IsEqualGUID(VEntity.GUID, AGuid) then begin
        Result := VEntity.GetProvider;
        Exit;
      end;
    end;
    for I := 0 to ATree.SubItemCount - 1 do begin
      Result := GetPathDetalizeProvider(ATree.SubItem[I], AGUID);
      if Result <> nil then begin
        Break;
      end;
    end;
  end;

  function GetLonLatLine(const APath: ILonLatPathWithSelected): IGeometryLonLatLine;
  var
    I, J: Integer;
    VPoints: array [0..1] of TDoublePoint;
  begin
    Assert(APath.Count > 1);
    Assert(APath.GetSelectedPointIndex > 0);

    J := Length(VPoints) - 1;
    for I := APath.GetSelectedPointIndex downto 0 do begin
      VPoints[J] := APath.Points[I];
      if not PointIsEmpty(VPoints[J]) then begin
        Dec(J);
        if J < 0 then begin
          Break;
        end;
      end;
    end;

    if J < 0 then begin
      Result := GState.VectorGeometryLonLatFactory.CreateLonLatLine(
        @VPoints[0],
        nil,
        Length(VPoints)
      );
    end else begin
      Result := nil;
    end;
  end;

  function MergeRouteWithPath(
    const ARoute: IGeometryLonLatLine;
    const APath: ILonLatPathWithSelected
  ): ILonLatPathWithSelected;
  var
    I: Integer;
    VCountBefore: Integer;
    VCountAfter: Integer;
    VPoints: PDoublePointArray;
    VMeta: PDoublePointsMeta;
    VMetaSlice: TDoublePointsMeta;
    VAggregator: IDoublePointsAggregator;
    VSingle: IGeometryLonLatSingleLine;
    VMulti: IGeometryLonLatMultiLine;
    VPath: IPathOnMapEdit;
    VLonLatLine: IGeometryLonLatLine;
    VSelectedPoint: TDoublePoint;
  begin
    VCountBefore := APath.GetSelectedPointIndex; // exclude active point
    VCountAfter := APath.Count - VCountBefore;

    VPoints := APath.Points;
    VMeta := APath.Meta;

    VAggregator := TDoublePointsAggregator.Create(APath.Count);

    if VCountBefore > 0 then begin
      VAggregator.AddPoints(@VPoints[0], VMeta, VCountBefore);
    end;

    if Supports(ARoute, IGeometryLonLatSingleLine, VSingle) then begin
      VAggregator.AddPoints(VSingle.Points, nil, VSingle.Count);
    end else if Supports(ARoute, IGeometryLonLatMultiLine, VMulti) then begin
      for I := 0 to VMulti.Count - 1 do begin
        VSingle := VMulti.Item[I];
        VAggregator.AddPoints(VSingle.Points, nil, VSingle.Count);
        VAggregator.Add(CEmptyDoublePoint);
      end;
    end else begin
      Assert(False);
    end;

    if VCountAfter > 0 then begin
      SliceMeta(@VMetaSlice, VMeta, APath.GetSelectedPointIndex);
      VAggregator.AddPoints(@VPoints[APath.GetSelectedPointIndex], @VMetaSlice, VCountAfter);
      VSelectedPoint := VAggregator.Points[(VAggregator.Count - 1) - (VCountAfter - 1)];
    end else begin
      if VAggregator.Count > 0 then begin
        VSelectedPoint := VAggregator.Points[VAggregator.Count - 1];
      end else begin
        VSelectedPoint := CEmptyDoublePoint;
      end;
    end;

    VLonLatLine := GState.VectorGeometryLonLatFactory.CreateLonLatLine(
      VAggregator.Points,
      VAggregator.Meta,
      VAggregator.Count
    );

    VPath := TPathOnMapEdit.Create(GState.VectorGeometryLonLatFactory);
    VPath.SetPath(VLonLatLine);

    if not PointIsEmpty(VSelectedPoint) then begin
      // restore previously selected point
      VPath.SelectPointInLonLatRect( DoubleRect(VSelectedPoint, VSelectedPoint) );
    end;

    Result := VPath.Path;
  end;

var
  VConfig: IPathDetalizeConfig;
  VLonLatLine, VRoute: IGeometryLonLatLine;
  VResult: ILonLatPathWithSelected;
  VProvider: IPathDetalizeProvider;
  VIsError: Boolean;
  VErrorMessage: string;
  VPathOnMapEdit: IPathOnMapEdit;
  VOperationNotifier: INotifierOperation;
begin
  VConfig := GState.Config.PathDetalizeConfig;
  if not VConfig.EnableAutomaticRouting then begin
    Exit;
  end;

  if not Supports(FLineOnMapEdit, IPathOnMapEdit, VPathOnMapEdit) then begin
    Assert(False);
    Exit;
  end;

  VProvider := GetPathDetalizeProvider(
    FPathProvidersTreeStatic,
    VConfig.DefaultProvider
  );

  if not Assigned(VProvider) then begin
    Assert(False, 'PathDetalizeProvider not assigned!');
    Exit;
  end;

  VIsError := True;
  try
    VOperationNotifier := TNotifierOperationFake.Create;

    VLonLatLine := GetLonLatLine(VPathOnMapEdit.Path);
    if not Assigned(VLonLatLine) then begin
      Assert(False, 'Can''t build LonLatLine from current Path!');
      Exit;
    end;

    VRoute :=
      VProvider.GetRoute(
        VOperationNotifier,
        VOperationNotifier.CurrentOperation,
        VLonLatLine,
        FRouteComment,
        VErrorMessage
      );
    VIsError := (VRoute = nil);
  except
    on E: Exception do begin
      ShowErrorMessage(Handle, E.ClassName + ': ' + E.Message);
    end;
  end;

  if not VIsError then begin
    MakeBackupOfPathOnMapEdit(VPathOnMapEdit, False);
    VResult := MergeRouteWithPath(VRoute, VPathOnMapEdit.Path);
    VPathOnMapEdit.SetPath(VResult);
  end else begin
    FRouteComment := '';
    if VErrorMessage <> '' then begin
      ShowErrorMessage(Handle, VErrorMessage);
    end;
  end;

  actEditPathRouteCalcUndo.Enabled := FRouteUndoPath <> nil;
end;

procedure TfrmMain.ZSliderMouseMove(
  Sender: TObject;
  Shift: TShiftState;
  X,
  Y: Integer;
  Layer: TCustomLayer
);
var
  h, xy: integer;
begin
  if ssLeft in Shift then begin
    if FRuller.Width < FRuller.Height then begin
      XY := ZSlider.Height - Y;
      h := (ZSlider.Height div 24);
    end else begin
      XY := X;
      h := (ZSlider.Width div 24);
    end;
    if XY in [h..h * 24] then begin
      ZSlider.Tag := (XY div h) - 1;
      PaintZSlider(ZSlider.Tag);
      labZoom.Caption := 'z' + inttostr(ZSlider.Tag + 1);
    end;
  end;
end;

procedure TfrmMain.ZSliderMouseUp(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer;
  Layer: TCustomLayer
);
begin
  if Button = mbLeft then begin
    ZSliderMouseMove(Sender, [ssLeft], X, Y, Layer);
    MapZoom(
      ZSlider.Tag,
      CenterPoint(FViewPortState.View.GetStatic.GetLocalRect)
    );
  end;
end;

procedure TfrmMain.PaintZSlider(zoom: integer);
var
  tumbpos: TPoint;
begin
  if FRuller.Height > FRuller.Width then begin
    tumbpos.Y := FRuller.Height - ((FRuller.Height div 24) * (zoom + 1)) - (FTumbler.Height div 2);
    tumbpos.X := (FRuller.Width div 2) - (FTumbler.Width div 2);
  end else begin
    tumbpos.X := (FRuller.Width div 24) * (zoom + 1) - (FTumbler.Width div 2);
    tumbpos.Y := (FRuller.Height div 2) - (FTumbler.Height div 2);
  end;
  ZSlider.Bitmap.Assign(FRuller);
  FTumbler.DrawTo(ZSlider.Bitmap, tumbpos.X, tumbpos.Y);
end;

procedure TfrmMain.OnNavToMarkChange;
begin
  actViewNavigation.Checked := FConfig.NavToPoint.IsActive;
end;

procedure TfrmMain.OnPathProvidesChange;
var
  I: Integer;
  VTree: IStaticTreeItem;
  VItem: TTBCustomItem;
  VGUID: TGUID;
  VIsDefaultFound: Boolean;
  VEntity: IPathDetalizeProviderTreeEntity;
  VInterface: IInterface;
begin
  VTree := FPathProvidersTree.GetStatic;
  FPathProvidersTreeStatic := VTree;

  FPathProvidersMenuBuilder.BuildMenu(TBEditPathMarsh, VTree);
  FPathProvidersConfigMenuBuilder.BuildMenu(tbxExtendRoute, VTree);

  VIsDefaultFound := False;
  VGUID := GState.Config.PathDetalizeConfig.DefaultProvider;
  for I := 0 to tbxExtendRoute.Count - 1 do begin
    VItem := tbxExtendRoute.Items[I];
    if Assigned(VItem.OnClick) then begin
      VItem.RadioItem := True;
      VItem.GroupIndex := 1;
      VItem.Checked := False;
      VItem.AutoCheck := True;
      VInterface := IInterface(VItem.Tag);
      if Supports(VInterface, IPathDetalizeProviderTreeEntity, VEntity) then begin
        VItem.Checked := IsEqualGUID(VGUID, VEntity.GUID);
        VIsDefaultFound := VIsDefaultFound or VItem.Checked;
      end;
    end;
  end;
  if not VIsDefaultFound then begin
    for I := 0 to tbxExtendRoute.Count - 1 do begin
      VItem := tbxExtendRoute.Items[I];
      if Assigned(VItem.OnClick) then begin
        VInterface := IInterface(VItem.Tag);
        if Supports(VInterface, IPathDetalizeProviderTreeEntity, VEntity) then begin
          GState.Config.PathDetalizeConfig.DefaultProvider := VEntity.GUID;
          VItem.Checked := True;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.OnShowSearchResults(Sender: TObject);
begin
  TBSearchWindow.Show;
  if FWinPosition.IsFullScreen then begin
    TBDockLeft.Parent := map;
    TBDockLeft.Visible := True;
  end;
end;

// TrayIcon

procedure TfrmMain.tbitmCopySearchResultCoordinatesClick(Sender: TObject);
var
  VStr: string;
  VPlacemark: IVectorDataItem;
begin
  if tbxpmnSearchResult.Tag <> 0 then begin
    VPlacemark := IVectorDataItem(tbxpmnSearchResult.Tag);
    VStr := GState.CoordToStringConverter.GetStatic.LonLatConvert(VPlacemark.Geometry.GetGoToPoint);
    CopyStringToClipboard(Handle, VStr);
  end;
end;

procedure TfrmMain.tbitmCopySearchResultDescriptionClick(Sender: TObject);
var
  VStr: string;
  VPlacemark: IVectorDataItem;
begin
  if tbxpmnSearchResult.Tag <> 0 then begin
    VPlacemark := IVectorDataItem(tbxpmnSearchResult.Tag);
    VStr := VPlacemark.GetInfoHTML;
    if VStr = '' then begin
      VStr := VPlacemark.GetDesc;
    end;
    CopyStringToClipboard(Handle, VStr);
  end;
end;

procedure TfrmMain.tbitmCreatePlaceMarkBySearchResultClick(Sender: TObject);
var
  VStr: string;
  VPlacemark: IVectorDataItem;
  VMark: IVectorDataItem;
  VVisible: Boolean;
  VResult: IVectorDataItem;
begin
  if tbxpmnSearchResult.Tag <> 0 then begin
    VPlacemark := IVectorDataItem(tbxpmnSearchResult.Tag);
    VStr := VPlacemark.GetInfoHTML;
    if VStr = '' then begin
      VStr := VPlacemark.GetDesc;
    end;
    VMark :=
      FMarkDBGUI.MarksDb.MarkDb.Factory.CreateNewMark(
        VPlacemark.Geometry,
        VPlacemark.Name,
        VStr
      );
    VVisible := True;
    VMark := FMarkDBGUI.EditMarkModal(VMark, True, VVisible);
    if VMark <> nil then begin
      VResult := FMarkDBGUI.MarksDb.MarkDb.UpdateMark(nil, VMark);
      if VResult <> nil then begin
        FMarkDBGUI.MarksDb.MarkDb.SetMarkVisible(VMark, VVisible);
      end;
    end;
  end;
end;

procedure TfrmMain.tbitmMakeVersionByMarkClick(Sender: TObject);
var
  VMark: IVectorDataItem;
  VMapType: IMapType;
  VInternalDomainOptions: IInternalDomainOptions;
  VBase, VDesc, VUrl: String;
  VFlags: TDomainOptionsResponseFlags;
begin
  // make version by selected placemark description
  VMark := FSelectedMark;
  if VMark <> nil then begin
    VMapType := FMainMapState.ActiveMap.GetStatic;
    if Assigned(VMapType) then begin
      if Assigned(VMapType.TileStorage) then begin
        if Supports(VMapType.TileStorage, IInternalDomainOptions, VInternalDomainOptions) then begin
          VBase := CTileStorageOptionsInternalURL + GUIDToString(VMapType.Zmp.GUID);
          VDesc := VMark.Desc;
      // get URL to open
          if (0 < Length(VDesc)) then begin
            if VInternalDomainOptions.DomainHtmlOptions(VBase, VDesc, VUrl, VFlags, c_IDO_RT_MakeVersionByDescriptionURL) then begin
              if (0 < Length(VUrl)) then begin
        // open given URL
                GState.InternalBrowser.Navigate(VMapType.Zmp.FileName, VUrl);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.tbitmMarkInfoClick(Sender: TObject);
var
  VMark: IVectorDataItem;
begin
  VMark := FSelectedMark;
  if VMark <> nil then begin
    FMarkDBGUI.ShowMarkInfo(VMark);
  end;
end;

procedure TfrmMain.RefreshTranslation;
begin
  inherited;
  Caption := GState.ApplicationCaption;
  OnMainFormMainConfigChange;
end;

procedure TfrmMain.actConfigAzimuthCircleExecute(Sender: TObject);
begin
  FConfig.LayersConfig.CenterScaleConfig.Visible :=
    not FConfig.LayersConfig.CenterScaleConfig.Visible;
end;

procedure TfrmMain.actConfigColorInversionExecute(Sender: TObject);
begin
  GState.Config.BitmapPostProcessingConfig.InvertColor :=
    not GState.Config.BitmapPostProcessingConfig.InvertColor;
end;

procedure TfrmMain.actConfigDownloadModeExecute(Sender: TObject);
begin
  FConfig.DownloadUIConfig.UseDownload := TTileSource(TComponent(Sender).Tag);
end;

procedure TfrmMain.actConfigGpsFollowPositionAtCenterExecute(Sender: TObject);
begin
  FConfig.GPSBehaviour.MapMoveCentered := not FConfig.GPSBehaviour.MapMoveCentered;
end;

procedure TfrmMain.actConfigGpsFollowPositionExecute(Sender: TObject);
begin
  FConfig.GPSBehaviour.MapMove := not FConfig.GPSBehaviour.MapMove;
end;

procedure TfrmMain.actConfigGpsOptionsShowExecute(Sender: TObject);
begin
  FfrmSettings.ShowGPSSettings;
end;

procedure TfrmMain.actConfigGpsShowTrackExecute(Sender: TObject);
begin
  FConfig.LayersConfig.GPSTrackConfig.Visible :=
    not FConfig.LayersConfig.GPSTrackConfig.Visible;
end;

procedure TfrmMain.actConfigInterfaceOptionsShowExecute(Sender: TObject);
begin
  FfrmMapLayersOptions.ShowModal;
end;

procedure TfrmMain.actConfigMarksHideExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IsUseMarks :=
    not FConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IsUseMarks;
end;

procedure TfrmMain.actConfigMarksNamesVisibleExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MarksLayerConfig.MarksDrawConfig.CaptionDrawConfig.ShowPointCaption :=
    not FConfig.LayersConfig.MarksLayerConfig.MarksDrawConfig.CaptionDrawConfig.ShowPointCaption;
end;

procedure TfrmMain.actConfigMiniMapVisibleExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MiniMapLayerConfig.LocationConfig.Visible :=
    not FConfig.LayersConfig.MiniMapLayerConfig.LocationConfig.Visible;
end;

procedure TfrmMain.actConfigOptionsShowExecute(Sender: TObject);
begin
  FfrmSettings.ShowModal;
end;

procedure TfrmMain.actConfigPreviousSelectionVisibleExecute(Sender: TObject);
begin
  FConfig.LayersConfig.LastSelectionLayerConfig.Visible :=
    not FConfig.LayersConfig.LastSelectionLayerConfig.Visible;
end;

procedure TfrmMain.actConfigProjectionUseExecute(Sender: TObject);
var
  VIndex: Integer;
  VProjList: IProjectionSetList;
  VEpsg: Integer;
  VNewProjectionSet: IProjectionSet;
begin
  if Assigned(Sender) then begin
    VIndex := TComponent(Sender).Tag;
    VProjList := GState.ProjectionSetList;
    Assert(VProjList <> nil);
    if (VIndex >= 0) and (VProjList.Count > VIndex) then begin
      VNewProjectionSet := VProjList.Items[VIndex];
      Assert(Assigned(VNewProjectionSet));
      VEpsg := VNewProjectionSet.Zooms[0].ProjectionType.ProjectionEPSG;
    end else begin
      VEpsg := 0; // reset to default
    end;
    FConfig.ViewProjectionConfig.EPSG := VEpsg;
  end;
end;

procedure TfrmMain.actConfigScaleLineExtendedExecute(Sender: TObject);
begin
  FConfig.LayersConfig.ScaleLineConfig.Extended :=
    not FConfig.LayersConfig.ScaleLineConfig.Extended;
end;

procedure TfrmMain.actConfigScaleLineNumberFormatExecute(Sender: TObject);
begin
  if Assigned(Sender) then begin
    case TComponent(Sender).Tag of
      0: begin
        FConfig.LayersConfig.ScaleLineConfig.NumbersFormat := slnfNice;
      end;
      1: begin
        FConfig.LayersConfig.ScaleLineConfig.NumbersFormat := slnfScienceRound;
      end;
      2: begin
        FConfig.LayersConfig.ScaleLineConfig.NumbersFormat := slnfScience;
      end;
    end;
  end;
end;

procedure TfrmMain.actConfigScaleLineOptionsShowExecute(Sender: TObject);
begin
  FfrmMapLayersOptions.ShowScaleLineOptions;
end;

procedure TfrmMain.actConfigScaleLineVisibleExecute(Sender: TObject);
begin
  FConfig.LayersConfig.ScaleLineConfig.Visible :=
    not FConfig.LayersConfig.ScaleLineConfig.Visible;
end;

procedure TfrmMain.actConfigStatusBarVisibleExecute(Sender: TObject);
begin
  FConfig.LayersConfig.StatBar.Visible := not FConfig.LayersConfig.StatBar.Visible;
end;

procedure TfrmMain.actConfigUseInertialMovementExecute(Sender: TObject);
begin
  FConfig.MapMovingConfig.AnimateMove := not FConfig.MapMovingConfig.AnimateMove;
end;

procedure TfrmMain.actConfigUsePrevForLayersExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig.UsePrevZoomAtLayer :=
    not FConfig.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig.UsePrevZoomAtLayer;
end;

procedure TfrmMain.actConfigUsePrevForMapExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig.UsePrevZoomAtMap :=
    not FConfig.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig.UsePrevZoomAtMap;
end;

procedure TfrmMain.actConfigUsePrevForVectorLayersExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig.UsePrevZoomAtVectorLayer :=
    not FConfig.LayersConfig.MainMapLayerConfig.UseTilePrevZoomConfig.UsePrevZoomAtVectorLayer;
end;

procedure TfrmMain.actConfigUseZoomAnimationExecute(Sender: TObject);
begin
  FConfig.MapZoomingConfig.AnimateZoom := not FConfig.MapZoomingConfig.AnimateZoom;
end;

procedure TfrmMain.actConfigZoomToCursorExecute(Sender: TObject);
begin
  FConfig.MapZoomingConfig.ZoomingAtMousePos := not FConfig.MapZoomingConfig.ZoomingAtMousePos;
end;

procedure TfrmMain.actDistanceCalculationExecute(Sender: TObject);
begin
  if FState.State <> ao_calc_line then begin
    FState.State := ao_calc_line;
  end else begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actEditPathLabelVisibleExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.Visible :=
    not FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.Visible;
end;

procedure TfrmMain.actEditPathRouteCalcUndoExecute(Sender: TObject);
var
  VPathOnMapEdit: IPathOnMapEdit;
begin
  if
    Assigned(FRouteUndoPath) and
    Supports(FLineOnMapEdit, IPathOnMapEdit, VPathOnMapEdit) then
  begin
    VPathOnMapEdit.SetPath(FRouteUndoPath.Path);
    FRouteUndoPath := nil;
    actEditPathRouteCalcUndo.Enabled := False;
  end;
end;

procedure TfrmMain.actEditPathShowAzimuthExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.ShowAzimuth :=
    not FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.ShowAzimuth;
end;

procedure TfrmMain.actEditPathShowDistIncrementExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.ShowDistIncrement :=
    not FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.ShowDistIncrement;
end;

procedure TfrmMain.actEditPathShowIntermediateDistExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.ShowIntermediateDist :=
    not FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.ShowIntermediateDist;
end;

procedure TfrmMain.actEditPathShowPointHintExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.ShowPointHint :=
    not FConfig.LayersConfig.MarkPolyLineLayerConfig.CaptionConfig.ShowPointHint;
end;

procedure TfrmMain.actEditPolygonLabelVisibleExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MarkPolygonLayerConfig.CaptionsConfig.Visible :=
    not FConfig.LayersConfig.MarkPolygonLayerConfig.CaptionsConfig.Visible;
end;

procedure TfrmMain.actEditPolygonShowAreaExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MarkPolygonLayerConfig.CaptionsConfig.ShowArea :=
    not FConfig.LayersConfig.MarkPolygonLayerConfig.CaptionsConfig.ShowArea;
end;

procedure TfrmMain.actEditPolygonShowPerimeterExecute(Sender: TObject);
begin
  FConfig.LayersConfig.MarkPolygonLayerConfig.CaptionsConfig.ShowPerimeter :=
    not FConfig.LayersConfig.MarkPolygonLayerConfig.CaptionsConfig.ShowPerimeter;
end;

procedure TfrmMain.actCalcLineLabelVisibleExecute(Sender: TObject);
begin
  FConfig.LayersConfig.CalcLineLayerConfig.CaptionConfig.Visible :=
    not FConfig.LayersConfig.CalcLineLayerConfig.CaptionConfig.Visible;
end;

procedure TfrmMain.actCalcLineShowAzimuthExecute(Sender: TObject);
begin
  FConfig.LayersConfig.CalcLineLayerConfig.CaptionConfig.ShowAzimuth :=
    not FConfig.LayersConfig.CalcLineLayerConfig.CaptionConfig.ShowAzimuth;
end;

procedure TfrmMain.actCalcLineShowDistIncrementExecute(Sender: TObject);
begin
  FConfig.LayersConfig.CalcLineLayerConfig.CaptionConfig.ShowDistIncrement :=
    not FConfig.LayersConfig.CalcLineLayerConfig.CaptionConfig.ShowDistIncrement;
end;

procedure TfrmMain.actCalcLineShowIntermediateDistExecute(Sender: TObject);
begin
  FConfig.LayersConfig.CalcLineLayerConfig.CaptionConfig.ShowIntermediateDist :=
    not FConfig.LayersConfig.CalcLineLayerConfig.CaptionConfig.ShowIntermediateDist;
end;

procedure TfrmMain.actCircleCalculationExecute(Sender: TObject);
begin
  if FState.State <> ao_calc_circle then begin
    FState.State := ao_calc_circle;
  end else begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actFavoriteAddExecute(Sender: TObject);
begin
  FfrmFavoriteMapSetEditor.DoAdd;
end;

procedure TfrmMain.actFavoriteManageExecute(Sender: TObject);
begin
  FfrmFavoriteMapSetManager.Show;
end;

procedure TfrmMain.actFileOpenExecute(Sender: TObject);
var
  VList: IStringListStatic;
begin
  VList := FMarkDBGUI.ImportFileDialog(Self.Handle);
  if Assigned(VList) then begin
    ProcessOpenFiles(VList);
  end;
end;

procedure TfrmMain.actGeoCalcUseGpsDatumExecute(Sender: TObject);
begin
  GState.Config.GeoCalcConfig.DatumSource := TGeoCalcDatumSource.dsWGS84;
end;

procedure TfrmMain.actGeoCalcUseZmpDatumExecute(Sender: TObject);
begin
  GState.Config.GeoCalcConfig.DatumSource := TGeoCalcDatumSource.dsZMP;
end;

procedure TfrmMain.actGpsConnectExecute(Sender: TObject);
begin
  GState.Config.GPSConfig.GPSEnabled :=
    not GState.Config.GPSConfig.GPSEnabled;
  TCustomAction(Sender).Enabled := False;
end;

procedure TfrmMain.actGpsMarkPointAddExecute(Sender: TObject);
var
  VPosition: IGPSPosition;
  VLonLat: TDoublePoint;
  VPoint: IGeometryLonLatPoint;
begin
  VPosition := GState.GPSRecorder.CurrentPosition;

  if (VPosition.PositionOK) then begin
    VLonLat := VPosition.LonLat;
  end else begin
    VLonLat := FViewPortState.View.GetStatic.GetCenterLonLat;
  end;
  VPoint := GState.VectorGeometryLonLatFactory.CreateLonLatPoint(VLonLat);

  if FMarkDBGUI.SaveMarkModal(nil, VPoint) then begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actGpsTrackClearExecute(Sender: TObject);
begin
  if GState.GpsTrackRecorder.IsEmpty then begin
    ShowInfoMessage(Handle, _('Nothing to delete - GPS track is empty.'));
  end else
  if ShowQuestionMessage(Handle, SAS_MSG_DeleteGPSTrackAsk, MB_YESNO) = ID_YES then begin
    GState.GpsTrackRecorder.ClearTrack;
  end;
end;

procedure TfrmMain.actGpsTrackSaveToDbExecute(Sender: TObject);
var
  VAllPoints: IGeometryLonLatLine;
begin
  VAllPoints := GState.GpsTrackRecorder.GetAllPoints;
  if Assigned(VAllPoints) then begin
    if FMarkDBGUI.SaveMarkModal(nil, VAllPoints, False, 'time: ' + DateTimeToStr(Now) + sLineBreak + 'track: true') then begin
      FState.State := ao_movemap;
    end;
  end else begin
    ShowErrorMessage(Handle, SAS_ERR_Nopoints);
  end;
end;

procedure TfrmMain.actHelpShowAboutExecute(Sender: TObject);
begin
  if FfrmAbout = nil then begin
    FfrmAbout := TfrmAbout.Create(
      GState.Config.LanguageManager,
      GState.BuildInfo,
      GState.ContentTypeManager,
      GState.MainConfigProvider
    );
  end;
  FfrmAbout.ShowModal;
end;

procedure TfrmMain.actIconsSettingsExecute(Sender: TObject);
begin
  if FfrmMarkPictureConfig = nil then begin
    FfrmMarkPictureConfig := TfrmMarkPictureConfig.Create(
      GState.Config.LanguageManager,
      GState.Config.MarksIconsPath,
      GState.MarkPictureList,
      GState.Config.MarkPictureConfig
    );
  end;
  FfrmMarkPictureConfig.ShowModal;
end;

procedure TfrmMain.actLineEditFitToScreenExecute(Sender: TObject);
var
  VLLRect: TDoubleRect;
  VPathEdit: IPathOnMapEdit;
  VPolyEdit: IPolygonOnMapEdit;
  VCircleEdit: ICircleOnMapEdit;
begin
  if Supports(FLineOnMapEdit, ICircleOnMapEdit, VCircleEdit) then begin
    VPolyEdit := VCircleEdit.GetPolygonOnMapEdit;
    VLLRect := VPolyEdit.Polygon.Geometry.Bounds.Rect;
  end else if Supports(FLineOnMapEdit, IPathOnMapEdit, VPathEdit) then begin
    VLLRect := VPathEdit.Path.Geometry.Bounds.Rect;
  end else if Supports(FLineOnMapEdit, IPolygonOnMapEdit, VPolyEdit) then begin
    VLLRect := VPolyEdit.Polygon.Geometry.Bounds.Rect;
  end;
  FMapGoto.FitRectToScreen(VLLRect);
end;

procedure TfrmMain.OnElevationMetaWriterResult(const ALine: IGeometryLonLatLine);
var
  VPathEdit: IPathOnMapEdit;
begin
  if Supports(FLineOnMapEdit, IPathOnMapEdit, VPathEdit) then begin
    VPathEdit.SetPath(ALine);
  end;
end;

procedure TfrmMain.actLineEditReplaceElevationExecute(Sender: TObject);
var
  VPathEdit: IPathOnMapEdit;
begin
  if Supports(FLineOnMapEdit, IPathOnMapEdit, VPathEdit) then begin
    FElevationMetaWriter.ProcessLineAsync(VPathEdit.Path.Geometry, Self.OnElevationMetaWriterResult);
  end;
end;

procedure TfrmMain.actLineEditReverseExecute(Sender: TObject);
begin
  if FLineOnMapEdit <> nil then begin
    FLineOnMapEdit.ReverseDirection;
  end;
end;

procedure TfrmMain.actLineEditSplitTogleExecute(Sender: TObject);
begin
  if FLineOnMapEdit <> nil then begin
    FLineOnMapEdit.ToggleSplit;
  end;
end;

procedure TfrmMain.actMakeLinkOnDesktopExecute(Sender: TObject);
var
  VLonLat: TDoublePoint;
  VArgStr: string;
  VZoomCurr: Byte;
  VMapType: IMapType;
  VLocalConverter: ILocalCoordConverter;
begin
  if SaveLink.Execute then begin
    VMapType := FMainMapState.ActiveMap.GetStatic;
    VLocalConverter := FViewPortState.View.GetStatic;
    VZoomCurr := VLocalConverter.Projection.Zoom;
    VLonLat := VLocalConverter.GetCenterLonLat;
    VArgStr :=
      '--map=' + GUIDToString(VMapType.Zmp.GUID) + ' ' +
      '--zoom=' + IntToStr(VZoomCurr + 1) + ' ' +
      '--move=(' + R2StrPoint(VLonLat.X) + ',' + R2StrPoint(VLonLat.Y) + ')';
    CreateLink(ParamStr(0), SaveLink.filename, '', VArgStr);
  end;
end;

procedure TfrmMain.actMapsAllLayersHideExecute(Sender: TObject);
begin
  FConfig.MapLayersConfig.LayerGuids := nil;
end;

procedure TfrmMain.actMapsEditMapParamsExecute(Sender: TObject);
var
  VMapType: IMapType;
begin
  if TComponent(Sender).Tag = 0 then begin
    VMapType := FMainMapState.ActiveMap.GetStatic;
  end else begin
    VMapType := IMapType(TComponent(Sender).Tag);
  end;
  FMapTypeEditor.EditMap(VMapType);
end;

procedure TfrmMain.actMarksAddLineExecute(Sender: TObject);
begin
  FEditMarkLine := nil;
  if FState.State <> ao_edit_line then begin
    FState.State := ao_edit_line;
  end else begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actMarksAddPointExecute(Sender: TObject);
begin
  FEditMarkPoint := nil;
  if FState.State <> ao_edit_point then begin
    FState.State := ao_edit_point;
  end else begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actMarksAddPolygonExecute(Sender: TObject);
begin
  FEditMarkPoly := nil;
  if FState.State <> ao_edit_poly then begin
    FState.State := ao_edit_poly;
  end else begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actMarksEditDeleteGeometryPointExecute(Sender: TObject);
begin
  if FLineOnMapEdit <> nil then begin
    FLineOnMapEdit.DeleteActivePoint;
  end;
end;

procedure TfrmMain.actMarksEditSnapToMarkersExecute(Sender: TObject);
begin
  FConfig.MainConfig.MagnetDraw := not FConfig.MainConfig.MagnetDraw;
end;

procedure TfrmMain.actMoveMapExecute(Sender: TObject);
begin
  FState.State := ao_movemap;
end;

procedure TfrmMain.actQuitExecute(Sender: TObject);
begin
  TrayIcon.Visible := False;
  Close;
end;

procedure TfrmMain.actMarkSaveAsNewExecute(Sender: TObject);
var
  VResult: boolean;
  VPathEdit: IPathOnMapEdit;
  VPolygonEdit: IPolygonOnMapEdit;
  VCircleEdit: ICircleOnMapEdit;
begin
  VResult := False;
  case FState.State of
    ao_edit_poly: begin
      if Supports(FLineOnMapEdit, IPolygonOnMapEdit, VPolygonEdit) then begin
        VResult := FMarkDBGUI.SaveMarkModal(FEditMarkPoly, VPolygonEdit.Polygon.Geometry, True);
      end;
    end;
    ao_edit_line, ao_calc_line: begin
      if Supports(FLineOnMapEdit, IPathOnMapEdit, VPathEdit) then begin
        VResult := FMarkDBGUI.SaveMarkModal(FEditMarkLine, VPathEdit.Path.Geometry, True, FRouteComment);
      end;
    end;
    ao_calc_circle: begin
      if Supports(FLineOnMapEdit, ICircleOnMapEdit, VCircleEdit) then begin
        VResult := FMarkDBGUI.SaveMarkModal(nil, VCircleEdit.GetPolygonOnMapEdit.Polygon.Geometry, True);
      end;
    end;
  end;
  if VResult then begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actMarkSaveAsSeparateSegmentsExecute(Sender: TObject);
var
  VResult: boolean;
  VPathEdit: IPathOnMapEdit;
  VPolygonEdit: IPolygonOnMapEdit;
begin
  VResult := False;
  case FState.State of
    ao_edit_line: begin
      if Supports(FLineOnMapEdit, IPathOnMapEdit, VPathEdit) then begin
        if Supports(VPathEdit.Path.Geometry, IGeometryLonLatMultiLine) then begin
          VResult := FMarkDBGUI.SaveMarkUngroupModal(FEditMarkLine, VPathEdit.Path.Geometry);
        end;
      end;
    end;
    ao_edit_poly: begin
      if Supports(FLineOnMapEdit, IPolygonOnMapEdit, VPolygonEdit) then begin
        if Supports(VPolygonEdit.Polygon.Geometry, IGeometryLonLatMultiPolygon) then begin
          VResult := FMarkDBGUI.SaveMarkUngroupModal(FEditMarkPoly, VPolygonEdit.Polygon.Geometry);
        end;
      end;
    end;
  end;
  if VResult then begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actMarkSaveExecute(Sender: TObject);
var
  VResult: boolean;
  VPathEdit: IPathOnMapEdit;
  VPolygonEdit: IPolygonOnMapEdit;
begin
  VResult := False;
  case FState.State of
    ao_edit_poly: begin
      if Supports(FLineOnMapEdit, IPolygonOnMapEdit, VPolygonEdit) then begin
        VResult := FMarkDBGUI.UpdateMark(FEditMarkPoly, VPolygonEdit.Polygon.Geometry);
      end;
    end;
    ao_edit_line: begin
      if Supports(FLineOnMapEdit, IPathOnMapEdit, VPathEdit) then begin
        VResult := FMarkDBGUI.UpdateMark(FEditMarkLine, VPathEdit.Path.Geometry);
      end;
    end;
  end;
  if VResult then begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actSelectByCoordinatesExecute(Sender: TObject);
var
  VPolygon: IGeometryLonLatPolygon;
  VSelLonLat: TfrmLonLatRectEdit;
  VLonLatRect: TDoubleRect;
  VAction: TBasicAction;
begin
  VAction := Sender as TBasicAction;
  TBRectSave.Action := VAction;
  VSelLonLat :=
    TfrmLonLatRectEdit.Create(
      GState.Config.LanguageManager,
      FActiveProjectionSet,
      FViewPortState.View,
      GState.Config.CoordRepresentationConfig,
      GState.CoordFromStringParser,
      GState.CoordToStringConverter
    );
  try
    VPolygon := GState.LastSelectionInfo.Polygon;
    if Assigned(VPolygon) then begin
      VLonLatRect := VPolygon.Bounds.Rect;
    end else begin
      VLonLatRect.TopLeft := FViewPortState.View.GetStatic.GetCenterLonLat;
      VLonLatRect.BottomRight := VLonLatRect.TopLeft;
    end;
    if VSelLonLat.Execute(VLonLatRect) Then Begin
      VPolygon := GState.VectorGeometryLonLatFactory.CreateLonLatPolygonByRect(VLonLatRect);
      FState.State := ao_movemap;
      FRegionProcess.ProcessPolygon(VPolygon);
    end else begin
      FState.State := ao_movemap;
    end;
  finally
    VSelLonLat.Free;
  end;
end;

procedure TfrmMain.actSelectByGeometryFinishExecute(Sender: TObject);
var
  VPoly: IGeometryLonLatPolygon;
  VPath: IGeometryLonLatLine;
  VLineOnMapEdit: ILineOnMapEdit;
  VFilter: ILonLatPointFilter;
begin
  VLineOnMapEdit := FLineOnMapEdit;
  if VLineOnMapEdit <> nil then begin
    case FState.State of
      ao_select_poly: begin
        VPoly := (VLineOnMapEdit as IPolygonOnMapEdit).Polygon.Geometry;
        FState.State := ao_movemap;
        FRegionProcess.ProcessPolygon(VPoly);
      end;
      ao_select_line: begin
        VPath := (VLineOnMapEdit as IPathOnMapEdit).Path.Geometry;
        VFilter :=
          TLonLatPointFilterLine2Poly.Create(
            FConfig.LayersConfig.SelectionPolylineLayerConfig.ShadowConfig.Radius,
            FViewPortState.View.GetStatic.Projection
          );
        VPoly :=
          GState.VectorGeometryLonLatFactory.CreateLonLatPolygonByLonLatPathAndFilter(
            VPath,
            VFilter
          );
        FState.State := ao_movemap;
        FRegionProcess.ProcessPolygon(VPoly);
      end;
    end;
  end;
end;

procedure TfrmMain.actSelectByLastSelectionEditExecute(Sender: TObject);
var
  VPolygon: IGeometryLonLatPolygon;
  VLineOnMapEdit: ILineOnMapEdit;
  VPolygonOnMapEdit: IPolygonOnMapEdit;
begin
  VPolygon := GState.LastSelectionInfo.Polygon;
  FState.State := ao_select_poly;
  TBRectSave.Action := actSelectByPolygon;
  if Assigned(VPolygon) then begin
    VLineOnMapEdit := FLineOnMapEdit;
    if Supports(VLineOnMapEdit, IPolygonOnMapEdit, VPolygonOnMapEdit) then begin
      VPolygonOnMapEdit.SetPolygon(VPolygon);
    end;
  end;
end;

procedure TfrmMain.actSelectByLastSelectionExecute(Sender: TObject);
var
  VZoom: Byte;
  VPolygon: IGeometryLonLatPolygon;
begin
  VZoom := GState.LastSelectionInfo.Zoom;
  VPolygon := GState.LastSelectionInfo.Polygon;
  if Assigned(VPolygon) then begin
    FState.State := ao_movemap;
    FRegionProcess.ProcessPolygonWithZoom(VZoom, VPolygon);
  end else begin
    ShowErrorMessage(Handle, SAS_MSG_NeedHL);
  end;
end;

procedure TfrmMain.actSelectByLineExecute(Sender: TObject);
var
  VAction: TBasicAction;
begin
  VAction := Sender as TBasicAction;
  TBRectSave.Action := VAction;
  if FState.State <> ao_select_line then begin
    FState.State := ao_select_line;
  end else begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actSelectByPolygonExecute(Sender: TObject);
var
  VAction: TBasicAction;
begin
  VAction := Sender as TBasicAction;
  TBRectSave.Action := VAction;
  if FState.State <> ao_select_poly then begin
    FState.State := ao_select_poly;
  end else begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actSelectByRectExecute(Sender: TObject);
var
  VAction: TBasicAction;
begin
  VAction := Sender as TBasicAction;
  TBRectSave.Action := VAction;
  if FState.State <> ao_select_rect then begin
    FState.State := ao_select_rect;
  end else begin
    FState.State := ao_movemap;
  end;
end;

procedure TfrmMain.actSelectBySelectionFromFileExecute(Sender: TObject);
var
  VList: IStringListStatic;
begin
  if (dlgOpenHlgLoad.Execute) then begin
    FState.State := ao_movemap;
    VList := TStringListStatic.CreateByStrings(dlgOpenHlgLoad.Files);
    ProcessOpenFiles(VList);
  end;
end;

procedure TfrmMain.actSelectByVisibleAreaExecute(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VPolygon: IGeometryLonLatPolygon;
  VAction: TBasicAction;
begin
  VAction := Sender as TBasicAction;
  TBRectSave.Action := VAction;
  VLocalConverter := FViewPortState.View.GetStatic;
  VProjection := VLocalConverter.Projection;
  VMapRect := VLocalConverter.GetRectInMapPixelFloat;
  VProjection.ValidatePixelRectFloat(VMapRect);
  VLonLatRect := VProjection.PixelRectFloat2LonLatRect(VMapRect);

  VPolygon := GState.VectorGeometryLonLatFactory.CreateLonLatPolygonByRect(VLonLatRect);
  FState.State := ao_movemap;
  FRegionProcess.ProcessPolygonWithZoom(VProjection.Zoom, VPolygon);
end;

procedure TfrmMain.actShowCacheManagerExecute(Sender: TObject);
begin
  FfrmCacheManager.Show;
end;

procedure TfrmMain.actShowDebugInfoExecute(Sender: TObject);
begin
  GState.DebugInfoWindow.Show;
end;

procedure TfrmMain.actShowGoToExecute(Sender: TObject);
begin
  FfrmGoTo.ShowGotoDialog();
end;

procedure TfrmMain.actShowPascalScriptIdeExecute(Sender: TObject);
begin
  FfrmPascalScriptIDE.Show;
end;

procedure TfrmMain.actShowPlacemarkManagerExecute(Sender: TObject);
begin
  FfrmMarksExplorer.ToggleVisible;
end;

procedure TfrmMain.actShowPointProjectExecute(Sender: TObject);
begin
  FfrmPointProjecting.Show;
end;

procedure TfrmMain.actShowUpddateCheckerExecute(Sender: TObject);
begin
  FfrmUpdateChecker.Show;
end;

procedure TfrmMain.actViewBordersVisibleExecute(Sender: TObject);
begin
  FWinPosition.ToggleBordersVisible;
end;

procedure TfrmMain.actViewFillingMapFilterModeExecute(Sender: TObject);
begin
  FConfig.LayersConfig.FillingMapLayerConfig.FilterMode :=
    not FConfig.LayersConfig.FillingMapLayerConfig.FilterMode;
end;

procedure TfrmMain.actViewFillingMapMapUseExecute(Sender: TObject);
var
  VSender: TComponent;
  VMapType: IMapType;
  VGUID: TGUID;
begin
  if Sender is TComponent then begin
    VSender := TComponent(Sender);
    if VSender.Tag <> 0 then begin
      VMapType := IMapType(VSender.Tag);
      VGUID := VMapType.GUID;
    end else begin
      VGUID := CGUID_Zero;
    end;
    FConfig.LayersConfig.FillingMapLayerConfig.GetSourceMap.MainMapGUID := VGUID;
  end;
end;

procedure TfrmMain.actViewFillingMapMarkExistingExecute(Sender: TObject);
begin
  FConfig.LayersConfig.FillingMapLayerConfig.FillMode := fmExisting;
end;

procedure TfrmMain.actViewFillingMapMarkGradientExecute(Sender: TObject);
begin
  FConfig.LayersConfig.FillingMapLayerConfig.FillMode := fmGradient;
end;

procedure TfrmMain.actViewFillingMapMarkUnexistingExecute(Sender: TObject);
begin
  FConfig.LayersConfig.FillingMapLayerConfig.FillMode := fmUnexisting;
end;

procedure TfrmMain.actViewFullMapMouseCursorVisibleExecute(Sender: TObject);
begin
  FConfig.LayersConfig.FullMapMouseCursorLayerConfig.Enabled :=
    not FConfig.LayersConfig.FullMapMouseCursorLayerConfig.Enabled;
end;

procedure TfrmMain.actViewFullScreenExecute(Sender: TObject);
begin
  FWinPosition.ToggleFullScreen;
end;

procedure TfrmMain.actViewGridGenShtabExecute(Sender: TObject);
var
  VTag: Integer;
begin
  VTag := TComponent(Sender).Tag;
  FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.LockWrite;
  try
    if VTag = 0 then begin
      FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Visible :=
        not FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Visible;
    end else begin
      if TCustomAction(Sender).Checked then begin
        FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Visible := False;
      end else begin
        FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Visible := True;
        FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Scale := VTag;
      end;
    end;
  finally
    FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.UnlockWrite;
  end;
end;

procedure TfrmMain.actViewGridLonLatExecute(Sender: TObject);
var
  VTag: Double;
begin
  VTag := TComponent(Sender).Tag;
  FConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.LockWrite;
  try
    if VTag = 0 then begin
      FConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.Visible :=
        not FConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.Visible;
    end else begin
      if TCustomAction(Sender).Checked then begin
        FConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.Visible := False;
      end else begin
        FConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.Visible := True;
        FConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.Scale := VTag;
      end;
    end;
  finally
    FConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.UnlockWrite;
  end;
end;

procedure TfrmMain.actViewNavigationExecute(Sender: TObject);
var
  VPoint: TDoublePoint;
begin
  if FConfig.NavToPoint.IsActive then begin
    FConfig.NavToPoint.StopNav;
  end else begin
    VPoint := FConfig.NavToPoint.LonLat;
    if PointIsEmpty(VPoint) then begin
      ShowErrorMessage(Handle, _('Click on the placemark with the right mouse button and select "Navigation to Placemark"'));
    end else begin
      FConfig.NavToPoint.StartNavLonLat(VPoint);
    end;
  end;
end;

procedure TfrmMain.actViewNotMinimizedExecute(Sender: TObject);
begin
  FWinPosition.SetNotMinimized;
end;

procedure TfrmMain.actViewSelectNextMapVersionExecute(Sender: TObject);
var
  VMapType: IMapType;
  VLocalConverter: ILocalCoordConverter;
begin
  VMapType := FMainMapState.ActiveMap.GetStatic;
  VLocalConverter := FViewPortState.View.GetStatic;
  VMapType.NextVersion(VLocalConverter, +1);
end;

procedure TfrmMain.actViewSelectNextMapWithTileExecute(Sender: TObject);
var
  VNextMap: IMapType;
begin
  VNextMap :=
    GState.MapType.NextMapWithTile(
      FViewPortState.View.GetStatic,
      FMainMapState.ActiveMap.GetStatic,
      +1
    );

  if Assigned(VNextMap) then begin
    FConfig.MainMapConfig.MainMapGUID := VNextMap.GUID;
  end;
end;

procedure TfrmMain.actViewSelectPrevMapVersionExecute(Sender: TObject);
var
  VMapType: IMapType;
  VLocalConverter: ILocalCoordConverter;
begin
  VMapType := FMainMapState.ActiveMap.GetStatic;
  VLocalConverter := FViewPortState.View.GetStatic;
  VMapType.NextVersion(VLocalConverter, -1);
end;

procedure TfrmMain.actViewSelectPrevMapWithTileExecute(Sender: TObject);
var
  VNextMap: IMapType;
begin
  VNextMap :=
    GState.MapType.NextMapWithTile(
      FViewPortState.View.GetStatic,
      FMainMapState.ActiveMap.GetStatic,
      -1
    );

  if Assigned(VNextMap) then begin
    FConfig.MainMapConfig.MainMapGUID := VNextMap.GUID;
  end;
end;

procedure TfrmMain.SwitchSunCalc(const ACalcType: TSunCalcDataProviderType);
var
  VProvType: TSunCalcDataProviderType;
  VIsVisible: Boolean;
  VSunCalcConfig: ISunCalcConfig;
begin
  VSunCalcConfig := FConfig.LayersConfig.SunCalcConfig;

  VProvType := VSunCalcConfig.DataProviderType;
  VIsVisible := VSunCalcConfig.Visible;

  VSunCalcConfig.StopNotify;
  try
    if VIsVisible then begin
      if VProvType = ACalcType then begin
        // hide
        FSunCalcProvider.Reset;
        VSunCalcConfig.Visible := False;
        VSunCalcConfig.IsRealTime := False;
      end else begin
        // switch provider
        VSunCalcConfig.DataProviderType := ACalcType;
      end;
    end else begin
      FSunCalcProvider.StopNotify;
      try
        FSunCalcProvider.LocalDateTime := Now;
        FSunCalcProvider.Location := FViewPortState.View.GetStatic.GetCenterLonLat;
      finally
        FSunCalcProvider.StartNotify;
      end;
      VSunCalcConfig.DataProviderType := ACalcType;
      VSunCalcConfig.Visible := True;
      VSunCalcConfig.IsRealTime := True;
    end;
  finally
    VSunCalcConfig.StartNotify;
  end;
end;

procedure TfrmMain.actViewSunCalcExecute(Sender: TObject);
begin
  SwitchSunCalc(scdpSun);
end;

procedure TfrmMain.actViewMoonCalcExecute(Sender: TObject);
begin
  SwitchSunCalc(scdpMoon);
end;

procedure TfrmMain.actViewToolbarsLockExecute(Sender: TObject);
begin
  FConfig.ToolbarsLock.IsLock := not FConfig.ToolbarsLock.IsLock;
end;

procedure TfrmMain.actZoomInExecute(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VFreezePos: TPoint;
begin
  VLocalConverter := FViewPortState.View.GetStatic;
  VFreezePos := CenterPoint(VLocalConverter.GetLocalRect);
  MapZoom(
    VLocalConverter.Projection.Zoom + 1,
    VFreezePos
  );
end;

procedure TfrmMain.actZoomOutExecute(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VFreezePos: TPoint;
begin
  VLocalConverter := FViewPortState.View.GetStatic;
  VFreezePos := CenterPoint(VLocalConverter.GetLocalRect);
  MapZoom(
    VLocalConverter.Projection.Zoom - 1,
    VFreezePos
  );
end;

function TfrmMain.IsShortCut(var AMsg: TWMKey): Boolean;
begin
  // Work around to avoid main form stealing shortcuts from active, non-modal forms
  if (Screen.ActiveForm <> nil) and (Screen.ActiveForm <> Self) then begin
    Result := Screen.ActiveForm.IsShortCut(AMsg);
  end else begin
    Result := inherited IsShortCut(AMsg);
  end;
end;

end.

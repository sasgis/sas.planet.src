unit frm_Main;

interface

uses
  Windows,
  Types,
  Messages,
  SysUtils,
  Forms,
  Math,
  ShellApi,
  Classes,
  Menus,
  Variants,
  ActiveX,
  ShlObj,
  ComObj,
  Graphics,
  StdCtrls,
  OleCtrls,
  Controls,
  ExtCtrls,
  Buttons,
  Dialogs,
  ExtDlgs,
  ImgList,
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
  TBXControls,
  TBXExtItems,
  TBXGraphics,
  TBXSASTheme,
  u_CommonFormAndFrameParents,
  i_JclNotify,
  i_GUIDList,
  t_GeoTypes,
  i_JclListenerNotifierLinksList,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_GeoToStr,
  t_CommonTypes,
  i_GPSRecorder,
  i_GeoCoder,
  i_MarksSimple,
  i_MainFormConfig,
  i_SearchResultPresenter,
  i_MainWindowPosition,
  i_LineOnMapEdit,
  i_PathDetalizeProvider,
  u_WindowLayerBasicList,
  u_GeoFun,
  u_MapLayerWiki,
  u_MapType,
  u_ResStrings,
  u_ShortcutManager,
  u_MapMainLayer,
  u_LayerStatBar,
  u_LayerScaleLine,
  u_MapMarksLayer,
  u_MapGPSLayer,
  u_MapLayerNavToMark,
  u_MapLayerFillingMap,
  u_MiniMapLayer,
  u_MapLayerGrids,
  u_MapLayerGoto,
  u_MapLayerShowError,
  u_CenterScale,
  u_SelectionLayer,
  u_CalcLineLayer,
  u_MarkPolyLineLayer,
  u_MarkPolygonLayer,
  u_SelectionPolygonLayer,
  u_SelectionRectLayer,
  u_MapLayerGPSMarker,
  u_MarksDbGUIHelper,
  u_TileDownloaderUI;

type
  TAOperation = (
    ao_movemap,
    ao_add_line ,
    ao_add_poly,
    ao_add_point,
    ao_edit_point,
    ao_edit_line,
    ao_edit_poly,
    ao_calc_line,
    ao_select_rect,
    ao_select_poly
  );

  TfrmMain = class(TCommonFormParent)
    map: TImage32;
    OpenDialog1: TOpenDialog;
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
    TBEditPath: TTBXToolbar;
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
    NHelp: TTBXSubmenuItem;
    NSRCic: TTBXItem;
    NSRCinet: TTBXItem;
    NSRCesh: TTBXItem;
    TBAdd_Point: TTBXItem;
    TBAdd_Line: TTBXItem;
    TBAdd_Poly: TTBXItem;
    TBItem6: TTBXItem;
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
    tbiEditYandexSrch: TTBEditItem;
    tbiEditGoogleSrch: TTBEditItem;
    TBZoomIn: TTBXItem;
    TBZoom_out: TTBXItem;
    N35: TTBXItem;
    NZoomIn: TTBXItem;
    NZoomOut: TTBXItem;
    N14: TTBXItem;
    NCalcRast: TTBXItem;
    N6: TTBXItem;
    TBEditPathDel: TTBXItem;
    TBEditPathLabel: TTBXItem;
    TBEditPathSave: TTBXItem;
    TBEditPathOk: TTBXItem;
    TBEditPathMarsh: TTBXSubmenuItem;
    TBItem8: TTBXItem;
    TBItem9: TTBXItem;
    TBItem7: TTBXItem;
    TBItem5: TTBXItem;
    TBItemDelTrack: TTBXItem;
    NFoolSize: TTBXItem;
    NGoToCur: TTBXItem;
    Nbackload: TTBXItem;
    NbackloadLayer: TTBXItem;
    Nanimate: TTBXItem;
    N32: TTBXItem;
    Ninvertcolor: TTBXItem;
    NPanels: TTBXSubmenuItem;
    N31: TTBXSubmenuItem;
    NFillMap: TTBXSubmenuItem;
    TBFillingTypeMap: TTBXSubmenuItem;
    TBXToolPalette1: TTBXToolPalette;
    NShowGran: TTBXSubmenuItem;
    N40: TTBXSubmenuItem;
    NGShScale0: TTBXItem;
    NGShScale1000000: TTBXItem;
    NGShScale500000: TTBXItem;
    NGShScale200000: TTBXItem;
    NGShScale100000: TTBXItem;
    NGShScale50000: TTBXItem;
    NGShScale25000: TTBXItem;
    NGShScale10000: TTBXItem;
    N29: TTBXItem;
    N16: TTBXItem;
    NGoToSite: TTBXItem;
    NGoToForum: TTBXItem;
    NMapParams: TTBXItem;
    N8: TTBXItem;
    TBLang: TTBXSubmenuItem;
    tbitmGPSConnect: TTBXItem;
    tbitmGPSTrackShow: TTBXItem;
    tbitmGPSCenterMap: TTBXItem;
    tbitmGPSTrackSave: TTBXItem;
    tbitmGPSTrackSaveToDb: TTBXItem;
    tbitmGPSTrackClear: TTBXItem;
    Showstatus: TTBXItem;
    ShowMiniMap: TTBXItem;
    ShowLine: TTBXItem;
    N000: TTBXItem;
    N001: TTBXItem;
    N002: TTBXItem;
    N003: TTBXItem;
    N004: TTBXItem;
    N005: TTBXItem;
    N006: TTBXItem;
    N007: TTBXItem;
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
    TBXSeparatorItem12: TTBXSeparatorItem;
    tbsprtGPS1: TTBXSeparatorItem;
    TBXSeparatorItem14: TTBXSeparatorItem;
    TBXSeparatorItem15: TTBXSeparatorItem;
    OpenPictureDialog: TOpenPictureDialog;
    TBXSensorsBar: TTBXToolWindow;
    ScrollBox1: TScrollBox;
    TBXDock1: TTBXDock;
    NSensors: TTBXSubmenuItem;
    TBXPopupMenuSensors: TTBXPopupMenu;
    TBXItem1: TTBXItem;
    TBXLabelItem1: TTBXLabelItem;
    TBXLabelItem2: TTBXLabelItem;
    TBXItem2: TTBXItem;
    TBXItem3: TTBXItem;
    TBXItem4: TTBXItem;
    TBXItem5: TTBXItem;
    TBXSeparatorItem16: TTBXSeparatorItem;
    TBXSeparatorItem17: TTBXSeparatorItem;
    TBXToolBarSearch: TTBXToolbar;
    TBXSearchEdit: TTBXEditItem;
    TBXSelectSrchType: TTBXSubmenuItem;
    TBXSelectGoogleSrch: TTBXItem;
    TBXSelectYandexSrch: TTBXItem;
    tbsprtGPS2: TTBXSeparatorItem;
    tbitmPositionByGSM: TTBXItem;
    TBXItem6: TTBXItem;
    OpenSessionDialog: TOpenDialog;
    NShowSelection: TTBXItem;
    TBRECT: TTBXItem;
    TBREGION: TTBXItem;
    TBCOORD: TTBXItem;
    TBPrevious: TTBXItem;
    TBLoadSelFromFile: TTBXItem;
    TBXSignalStrengthBar: TTBXToolWindow;
    TBXLabel5: TTBXLabel;
    TBGPSToPoint: TTBXSubmenuItem;
    TBGPSToPointCenter: TTBXItem;
    tbitmGPSToPointCenter: TTBXItem;
    tbtmHelpBugTrack: TTBXItem;
    tbitmShowDebugInfo: TTBXItem;
    PanelsImageList: TTBXImageList;
    TBHideMarks: TTBXItem;
    ZSlider: TImage32;
    TBControlItem1: TTBControlItem;
    TBXPopupPanels: TTBXPopupMenu;
    MenusImageList: TTBXImageList;
    ScalesImageList: TTBXImageList;
    EditCommentsImgs: TTBXImageList;
    MainPopupMenu: TTBXPopupMenu;
    NMarkEdit: TTBXItem;
    NMarkDel: TTBXItem;
    NMarkOper: TTBXItem;
    NMarkNav: TTBXItem;
    NMarkExport: TTBXItem;
    NMarksCalcs: TTBXSubmenuItem;
    NMarksCalcsLen: TTBXItem;
    NMarksCalcsPer: TTBXItem;
    NMarksCalcsSq: TTBXItem;
    NMarkSep: TTBXSeparatorItem;
    NaddPoint: TTBXItem;
    N47: TTBXSeparatorItem;
    N28: TTBXSubmenuItem;
    N22: TTBXSeparatorItem;
    N43: TTBXSubmenuItem;
    Google1: TTBXItem;
    YaLink: TTBXItem;
    kosmosnimkiru1: TTBXItem;
    livecom1: TTBXItem;
    ImageAtlas1: TTBXItem;
    N51: TTBXSeparatorItem;
    N13: TTBXItem;
    N30: TTBXItem;
    N20: TTBXItem;
    N15: TTBXItem;
    Nopendir: TTBXItem;
    N25: TTBXItem;
    N23: TTBXSeparatorItem;
    N26: TTBXSubmenuItem;
    NGTOPO30: TTBXItem;
    NSRTM3: TTBXItem;
    N49: TTBXSeparatorItem;
    DigitalGlobe1: TTBXItem;
    N27: TTBXSeparatorItem;
    N24: TTBXSeparatorItem;
    N21: TTBXItem;
    NDel: TTBXItem;
    N1: TTBXSeparatorItem;
    NMapInfo: TTBXItem;
    ldm: TTBXSubmenuItem;
    dlm: TTBXSubmenuItem;
    TBXToolPalette2: TTBXToolPalette;
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
    NSignalStrengthBar: TTBXVisibilityToggleItem;
    TBXSeparatorItem18: TTBXSeparatorItem;
    NBlock_toolbars: TTBXItem;
    procedure FormActivate(Sender: TObject);
    procedure NzoomInClick(Sender: TObject);
    procedure NZoomOutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TBZoom_outClick(Sender: TObject);
    procedure TBZoomInClick(Sender: TObject);
    procedure TBmoveClick(Sender: TObject);
    procedure TBFullSizeClick(Sender: TObject);
    procedure NCalcRastClick(Sender: TObject);
    procedure NFoolSizeClick(Sender: TObject);
    procedure N6Click(Sender: TObject);
    procedure ZoomToolBarDockChanging(Sender: TObject; Floating: Boolean; DockingTo: TTBDock);
    procedure N8Click(Sender: TObject);
    procedure NbackloadClick(Sender: TObject);
    procedure NaddPointClick(Sender: TObject);
    procedure N20Click(Sender: TObject);
    procedure N15Click(Sender: TObject);
    procedure N21Click(Sender: TObject);
    procedure NopendirClick(Sender: TObject);
    procedure N25Click(Sender: TObject);
    procedure NDelClick(Sender: TObject);
    procedure TBREGIONClick(Sender: TObject);
    procedure NShowGranClick(Sender: TObject);
    procedure NFillMapClick(Sender: TObject);
    procedure NSRCinetClick(Sender: TObject);
    procedure N16Click(Sender: TObject);
    procedure TBRECTClick(Sender: TObject);
    procedure TBRectSaveClick(Sender: TObject);
    procedure TBPreviousClick(Sender: TObject);
    procedure TBCalcRasClick(Sender: TObject);
    procedure N29Click(Sender: TObject);
    procedure tbiEditSrchAcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
    procedure TBSubmenuItem1Click(Sender: TObject);
    procedure N000Click(Sender: TObject);
    procedure TBItem2Click(Sender: TObject);
    procedure TBGPSconnClick(Sender: TObject);
    procedure TBGPSPathClick(Sender: TObject);
    procedure TBGPSToPointClick(Sender: TObject);
    procedure N30Click(Sender: TObject);
    procedure TBCOORDClick(Sender: TObject);
    procedure ShowstatusClick(Sender: TObject);
    procedure ShowMiniMapClick(Sender: TObject);
    procedure ShowLineClick(Sender: TObject);
    procedure N32Click(Sender: TObject);
    procedure Google1Click(Sender: TObject);
    procedure mapResize(Sender: TObject);
    procedure TBLoadSelFromFileClick(Sender: TObject);
    procedure YaLinkClick(Sender: TObject);
    procedure kosmosnimkiru1Click(Sender: TObject);
    procedure NinvertcolorClick(Sender: TObject);
    procedure mapDblClick(Sender: TObject);
    procedure TBAdd_PointClick(Sender: TObject);
    procedure TBAdd_LineClick(Sender: TObject);
    procedure TBAdd_PolyClick(Sender: TObject);
    procedure TBItem5Click(Sender: TObject);
    procedure NMarkEditClick(Sender: TObject);
    procedure NMarkDelClick(Sender: TObject);
    procedure NMarkOperClick(Sender: TObject);
    procedure livecom1Click(Sender: TObject);
    procedure N13Click(Sender: TObject);
    procedure ImageAtlas1Click(Sender: TObject);
    procedure DigitalGlobe1Click(Sender: TObject);
    procedure mapMouseLeave(Sender: TObject);
    procedure GPSReceiverDisconnect(Sender: TObject);
    procedure GPSReceiverStateChange(Sender: TObject);
    procedure GPSReceiverConnect(Sender: TObject);
    procedure GPSReceiverTimeout(Sender: TObject);
    procedure GPSReceiverConnectError(Sender: TObject);
    procedure GPSReceiverReceive(Sender: TObject);
    procedure NMapParamsClick(Sender: TObject);
    procedure mapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure mapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure mapMouseMove(Sender: TObject; Shift: TShiftState; AX, AY: Integer; Layer: TCustomLayer);
    procedure N35Click(Sender: TObject);
    procedure TBItemDelTrackClick(Sender: TObject);
    procedure NGShScale01Click(Sender: TObject);
    procedure TBEditPathDelClick(Sender: TObject);
    procedure TBEditPathLabelClick(Sender: TObject);
    procedure TBEditPathSaveClick(Sender: TObject);
    procedure TBEditPathClose(Sender: TObject);
    procedure NGoToForumClick(Sender: TObject);
    procedure NGoToSiteClick(Sender: TObject);
    procedure TBItem6Click(Sender: TObject);
    procedure NSRTM3Click(Sender: TObject);
    procedure NGTOPO30Click(Sender: TObject);
    procedure NMarkNavClick(Sender: TObject);
    procedure TBEditPathMarshClick(Sender: TObject);
    procedure AdjustFont(Item: TTBCustomItem; Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
    procedure NParamsClick(Sender: TObject);
    procedure TBfillMapAsMainClick(Sender: TObject);
    procedure NMarksCalcsLenClick(Sender: TObject);
    procedure NMarksCalcsSqClick(Sender: TObject);
    procedure NMarksCalcsPerClick(Sender: TObject);
    procedure TBEditPathOkClick(Sender: TObject);
    procedure NMapInfoClick(Sender: TObject);
    procedure TBXToolPalette1CellClick(Sender: TTBXCustomToolPalette;var ACol, ARow: Integer; var AllowChange: Boolean);
    procedure NanimateClick(Sender: TObject);
    procedure NbackloadLayerClick(Sender: TObject);
    procedure TBXSensorsBarVisibleChanged(Sender: TObject);
    procedure TBXItem1Click(Sender: TObject);
    procedure TBXItem5Click(Sender: TObject);
    procedure TBXSelectSrchClick(Sender: TObject);
    procedure TBXSearchEditAcceptText(Sender: TObject; var NewText: String;
      var Accept: Boolean);
    procedure tbitmPositionByGSMClick(Sender: TObject);
    procedure TBXItem6Click(Sender: TObject);
    procedure NShowSelectionClick(Sender: TObject);
    procedure NGoToCurClick(Sender: TObject);
    procedure TBGPSToPointCenterClick(Sender: TObject);
    procedure tbtmHelpBugTrackClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tbitmShowDebugInfoClick(Sender: TObject);
    procedure NMarkExportClick(Sender: TObject);
    procedure TBHideMarksClick(Sender: TObject);
    procedure ZSliderMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure ZSliderMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure MainPopupMenuPopup(Sender: TObject);
    procedure TBXToolPalette2CellClick(Sender: TTBXCustomToolPalette; var ACol,
      ARow: Integer; var AllowChange: Boolean);
    procedure TBScreenSelectClick(Sender: TObject);
    procedure NSensorsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NBlock_toolbarsClick(Sender: TObject);
  private
    FLinksList: IJclListenerNotifierLinksList;
    FConfig: IMainFormConfig;
    FIsGPSPosChanged: Boolean;
    FCenterToGPSDelta: TDoublePoint;
    FShowActivHint: boolean;
    FHintWindow: THintWindow;
    Frect_dwn: Boolean;
    Frect_p2: boolean;
    FMainLayer: TMapMainLayer;
    FLayerStatBar: TLayerStatBar;
    FShowErrorLayer: TTileErrorInfoLayer;
    FWikiLayer: TWikiLayer;
    FdWhenMovingButton: integer;
    FMarshrutComment: string;
    movepoint: boolean;
    FSelectionRect: TDoubleRect;

    FLayerScaleLine: TLayerScaleLine;
    FCalcLineLayer: TCalcLineLayer;
    FMarkPolyLineLayer: TMarkPolyLineLayer;
    FMarkPolygonLayer: TMarkPolygonLayer;
    FSelectionPolygonLayer: TSelectionPolygonLayer;
    FSelectionRectLayer: TSelectionRectLayer;

    FLayerMapGPS: TMapGPSLayer;
    FLayerGoto: TGotoLayer;
    FLayerFillingMap: TMapLayerFillingMap;
    FLayerMapMarks: TMapMarksLayer;
    FLayerMapCenterScale: TCenterScale;
    FLayerMiniMap: TMiniMapLayer;
    FLayerSelection: TSelectionLayer;
    FLayerGPSMarker: TMapLayerGPSMarker;
    FLayerGrids: TMapLayerGrids;
    LayerMapNavToMark: TNavToMarkLayer;
    FUIDownLoader: TTileDownloaderUI;

    ProgramStart: Boolean;
    ProgramClose: Boolean;

    FNLayerParamsItemList: IGUIDObjectList; //Пункт гланого меню Параметры/Параметры слоя
    FNDwnItemList: IGUIDObjectList; //Пункт контекстного меню Загрузить тайл слоя
    FNDelItemList: IGUIDObjectList; //Пункт контекстного меню Удалить тайл слоя
    FNOpenDirItemList: IGUIDObjectList; //Пункт контекстного меню Открыть папку слоя
    FNCopyLinkItemList: IGUIDObjectList; //Пункт контекстного меню копировать ссылку на тайл слоя
    FNLayerInfoItemList: IGUIDObjectList; //Пункт контекстного меню информация о слое

    FShortCutManager: TShortcutManager;
    FLayersList: TWindowLayerBasicList;

    FSearchPresenter: ISearchResultPresenter;
    FMouseDownPoint: TPoint;
    FMouseUpPoint: TPoint;
    FmoveTrue: Tpoint;
    FMapMoving: Boolean;
    FMapZoomAnimtion: Boolean;
    FEditMark: IMarkFull;
    FCurrentOper: TAOperation;

    FWinPosition: IMainWindowPosition;

    FLineOnMapEdit: ILineOnMapEdit;
    FMarkDBGUI: TMarksDbGUIHelper;

    FRuller:TBitmap32;
    FTumbler:TBitmap32;
    FSensorViewList: IGUIDInterfaceList;

    procedure InitSearchers;
    procedure CreateMapUIMapsList;
    procedure CreateMapUILayersList;
    procedure CreateMapUIFillingList;
    procedure CreateMapUILayerSubMenu;
    procedure CreateLangMenu;

    procedure OnWinPositionChange(Sender: TObject);
    procedure OnToolbarsLockChange(Sender: TObject);
    procedure OnLineOnMapEditChange(Sender: TObject);
    procedure DoMessageEvent(var Msg: TMsg; var Handled: Boolean);
    procedure WMGetMinMaxInfo(var msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure zooming(ANewZoom: byte; move: boolean);
    procedure PrepareSelectionRect(Shift: TShiftState; var ASelectedLonLat: TDoubleRect);
    procedure ProcessPosChangeMessage(Sender: TObject);
    procedure CopyBtmToClipboard(btm: TBitmap);
    function GetIgnoredMenuItemsList: TList;
    procedure MapLayersVisibleChange(Sender: TObject);
    procedure OnMainFormMainConfigChange(Sender: TObject);
    procedure OnTimerEvent(Sender: TObject);

    procedure CopyStringToClipboard(s: Widestring);
    procedure setalloperationfalse(newop: TAOperation);
    procedure UpdateGPSSatellites;
    procedure OnClickMapItem(Sender: TObject);
    procedure OnClickLayerItem(Sender: TObject);
    procedure OnMainMapChange(Sender: TObject);
    procedure OnFillingMapChange(Sender: TObject);

    procedure PaintZSlider(zoom:integer);
    procedure SetToolbarsLock(AValue: Boolean);

    Procedure FormMove(Var Msg: TWMMove); Message WM_MOVE;
  public
    MouseCursorPos: Tpoint;
    property ShortCutManager: TShortcutManager read FShortCutManager;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure topos(LL: TDoublePoint; zoom_: byte; draw: boolean);
    procedure CreateMapUI;
    procedure SaveWindowConfigToIni(AProvider: IConfigDataWriteProvider);
    procedure OnMapTileUpdate(AMapType: TMapType; AZoom: Byte; ATile: TPoint);
    procedure OnMapUpdate(AMapType: TMapType);
    procedure LayerMapMarksRedraw;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  StrUtils,
  u_GUIDObjectList,
  u_GlobalState,
  frm_GoTo,
  frm_About,
  frm_Settings,
  frm_RegionProcess,
  frm_LonLatRectEdit,
  frm_MapTypeEdit,
  frm_IntrnalBrowser,
  frm_MarksExplorer,
  frm_DGAvailablePic,
  c_ZeroGUID,
  c_SasVersion,
  c_GeoCoderGUIDSimple,
  u_JclListenerNotifierLinksList,
  u_TileDownloaderUIOneTile,
  u_LogForTaskThread,
  u_NotifyEventListener,
  i_MapTypes,
  i_GPS,
  i_GeoCoderList,
  i_LogSimple,
  i_LogForTaskThread,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_ValueToStringConverter,
  i_ActiveMapsConfig,
  i_LanguageManager,
  i_SensorViewListGenerator,
  u_SensorViewListGeneratorStuped,
  u_MainWindowPositionConfig,
  u_LineOnMapEdit,
  i_MapViewGoto,
  u_MapViewGotoOnFMain,
  u_LanguageTBXItem,
  i_ImportConfig,
  u_ThreadDownloadTiles,
  u_PathDetalizeProviderMailRu,
  u_PathDetalizeProviderYourNavigation,
  u_SaveLoadTBConfigByConfigProvider,
  u_MapTypeMenuItemsGeneratorBasic,
  u_PosFromGSM,
  u_ExportMarks2KML,
  frm_SearchResults,
  frm_ProgressDownload,
  frm_InvisibleBrowser,
  frm_DebugInfo,
  frm_StartLogo,
  frm_ImportConfigEdit;

{$R *.dfm}

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FLinksList := TJclListenerNotifierLinksList.Create;
  FConfig := GState.MainFormConfig;
  FIsGPSPosChanged := False;
  FdWhenMovingButton := 5;

  TBSMB.Images := GState.MapTypeIcons24List.GetImageList;
  TBSMB.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
  TBLayerSel.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
  TBFillingTypeMap.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
  NSMB.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
  NLayerSel.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
  NLayerParams.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
  ldm.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
  dlm.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
  TBOpenDirLayer.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
  TBCopyLinkLayer.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
  TBLayerInfo.SubMenuImages := GState.MapTypeIcons18List.GetImageList;

  FNLayerParamsItemList := TGUIDObjectList.Create(False);
  FNDwnItemList := TGUIDObjectList.Create(False);
  FNDelItemList := TGUIDObjectList.Create(False);
  FNOpenDirItemList := TGUIDObjectList.Create(False);
  FNCopyLinkItemList := TGUIDObjectList.Create(False);
  FNLayerInfoItemList := TGUIDObjectList.Create(False);

  FLayersList := TWindowLayerBasicList.Create;
  FWinPosition := TMainWindowPositionConfig.Create(BoundsRect);
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnWinPositionChange),
    FWinPosition.GetChangeNotifier
  );

  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnToolbarsLockChange),
    FConfig.ToolbarsLock.GetChangeNotifier
  );

  FLineOnMapEdit := TLineOnMapEdit.Create;
  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnLineOnMapEditChange),
    FLineOnMapEdit.GetChangeNotifier
  );

  FRuller:=TBitmap32.Create;
  GState.LoadBitmapFromRes('VRULLER', FRuller);
  FTumbler:=TBitmap32.Create;
  GState.LoadBitmapFromRes('VTUMBLER', FTumbler);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  VProvider: IConfigDataProvider;
  VSensorViewGenerator: ISensorViewListGenerator;
begin
  ProgramStart:=true;
  Application.Title:=Caption;
  Caption:=Caption+' '+SASVersion;
  TBXSetTheme('SAStbxTheme');

  VProvider := GState.MainConfigProvider.GetSubItem('MainForm');
  FWinPosition.ReadConfig(VProvider);

  VProvider := GState.MainConfigProvider.GetSubItem('PANEL');

  TBEditPath.Floating:=true;
  TBEditPath.MoveOnScreen(true);
  TBEditPath.FloatingPosition:=Point(Left+map.Left+30,Top+map.Top+70);
  VSensorViewGenerator := TSensorViewListGeneratorStuped.Create(GState.GUISyncronizedTimerNotifier, Self, TBXDock1, NSensors, MenusImageList, 40);
  FSensorViewList := VSensorViewGenerator.CreateSensorViewList(GState.SensorList);
  TBConfigProviderLoadPositions(Self, VProvider);
  OnToolbarsLockChange(nil);
  TBEditPath.Visible:=false;
  FMarkDBGUI := TMarksDbGUIHelper.Create(GState.MarksDB, GState.ValueToStringConverterConfig, GState.MarksDB.MarksFactoryConfig.PointTemplateConfig.MarkPictureList);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FSensorViewList := nil;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
var
  param:string;
  VGUID: TGUID;
  VLonLat: TDoublePoint;
  VMapLayersVsibleChangeListener: IJclListener;
  VMainFormMainConfigChangeListener: IJclListener;
  VGPSReceiverStateChangeListener: IJclListener;
  VScale: Integer;
  VZoom: Byte;
begin
  if not ProgramStart then exit;
  FConfig.ViewPortState.ChangeViewSize(Point(map.Width, map.Height));
  Enabled:=false;
  try
    OnWinPositionChange(nil);

    movepoint:=false;

    Enabled:=true;
    Application.OnMessage := DoMessageEvent;
    Application.HelpFile := ExtractFilePath(Application.ExeName)+'help.hlp';
    Screen.Cursors[1]:=LoadCursor(HInstance, 'SEL');
    Screen.Cursors[2]:=LoadCursor(HInstance, 'LEN');
    Screen.Cursors[3]:=LoadCursor(HInstance, 'HAND');
    Screen.Cursors[4]:=LoadCursor(HInstance, 'SELPOINT');
    Map.Cursor:=crDefault;

    FMouseDownPoint := point(0,0);
    FMouseUpPoint := point(0,0);
    FMapZoomAnimtion:=False;
    FShortCutManager := TShortcutManager.Create(TBXMainMenu.Items, GetIgnoredMenuItemsList);
    FShortCutManager.Load(GState.MainConfigProvider.GetSubItem('HOTKEY'));

    tbitmShowDebugInfo.Visible := GState.ShowDebugInfo;

    FMainLayer := TMapMainLayer.Create(map, FConfig.ViewPortState, FConfig.MainMapsConfig, GState.BitmapPostProcessingConfig);
    FLayersList.Add(FMainLayer);
    FLayerGrids := TMapLayerGrids.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.MapLayerGridsConfig);
    FLayersList.Add(FLayerGrids);
    FWikiLayer := TWikiLayer.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.KmlLayerConfig, FConfig.MainMapsConfig.GetKmlLayersSet);
    FLayersList.Add(FWikiLayer);
    FLayerFillingMap:=TMapLayerFillingMap.create(map, FConfig.ViewPortState, FConfig.LayersConfig.FillingMapLayerConfig);
    FLayersList.Add(FLayerFillingMap);
    FLayerMapMarks:= TMapMarksLayer.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.MarksShowConfig, FMarkDBGUI);
    FLayersList.Add(FLayerMapMarks);
    FLayerMapGPS:= TMapGPSLayer.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.GPSTrackConfig, GState.GPSRecorder);
    FLayersList.Add(FLayerMapGPS);
    FLayerGPSMarker := TMapLayerGPSMarker.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.GPSMarker, GState.GPSRecorder);
    FLayersList.Add(FLayerGPSMarker);
    FLayerSelection := TSelectionLayer.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.LastSelectionLayerConfig, GState.LastSelectionInfo);
    FLayersList.Add(FLayerSelection);
    FCalcLineLayer := TCalcLineLayer.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.CalcLineLayerConfig, GState.ValueToStringConverterConfig);
    FLayersList.Add(FCalcLineLayer);
    FMarkPolyLineLayer := TMarkPolyLineLayer.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.MarkPolyLineLayerConfig);
    FLayersList.Add(FMarkPolyLineLayer);
    FMarkPolygonLayer := TMarkPolygonLayer.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.MarkPolygonLayerConfig);
    FLayersList.Add(FMarkPolygonLayer);
    FSelectionPolygonLayer := TSelectionPolygonLayer.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.SelectionPolygonLayerConfig);
    FLayersList.Add(FSelectionPolygonLayer);
    FSelectionRectLayer := TSelectionRectLayer.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.SelectionRectLayerConfig);
    FLayersList.Add(FSelectionRectLayer);
    FLayerGoto := TGotoLayer.Create(map, FConfig.ViewPortState);
    FLayersList.Add(FLayerGoto);
    LayerMapNavToMark := TNavToMarkLayer.Create(map, FConfig.ViewPortState, FConfig.NavToPoint, FConfig.LayersConfig.NavToPointMarkerConfig);
    FLayersList.Add(LayerMapNavToMark);
    FShowErrorLayer := TTileErrorInfoLayer.Create(map, FConfig.ViewPortState);
    FLayersList.Add(FShowErrorLayer);
    FLayerMapCenterScale := TCenterScale.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.CenterScaleConfig);
    FLayersList.Add(FLayerMapCenterScale);
    FLayerScaleLine := TLayerScaleLine.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.ScaleLineConfig);
    FLayersList.Add(FLayerScaleLine);
    FLayerStatBar:=TLayerStatBar.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.StatBar);
    FLayersList.Add(FLayerStatBar);
    FLayerMiniMap := TMiniMapLayer.Create(map, FConfig.ViewPortState, FConfig.LayersConfig.MiniMapLayerConfig, GState.BitmapPostProcessingConfig);
    FLayersList.Add(FLayerMiniMap);
    FMainLayer.ErrorShowLayer := FShowErrorLayer;

    FUIDownLoader := TTileDownloaderUI.Create(FConfig.DownloadUIConfig, FConfig.ViewPortState, FConfig.MainMapsConfig.GetAllActiveMapsSet, Self.OnMapTileUpdate, FShowErrorLayer);

    CreateMapUI;

    VScale := FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Scale;
    NGShScale10000.Checked := VScale = 10000;
    NGShScale25000.Checked := VScale = 25000;
    NGShScale50000.Checked := VScale = 50000;
    NGShScale100000.Checked := VScale = 100000;
    NGShScale200000.Checked := VScale = 200000;
    NGShScale500000.Checked := VScale = 500000;
    NGShScale1000000.Checked := VScale = 1000000;
    NGShScale0.Checked := VScale = 0;

    FLinksList.Add(
      TNotifyEventListener.Create(Self.ProcessPosChangeMessage),
      FConfig.ViewPortState.GetChangeNotifier
    );
    FLinksList.Add(
      TNotifyEventListener.Create(Self.OnMainMapChange),
      FConfig.MainMapsConfig.GetActiveMap.GetChangeNotifier
    );

    VMapLayersVsibleChangeListener := TNotifyEventListener.Create(Self.MapLayersVisibleChange);
    FLinksList.Add(
      VMapLayersVsibleChangeListener,
      FConfig.LayersConfig.StatBar.GetChangeNotifier
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

    VGPSReceiverStateChangeListener := TNotifyEventListenerSync.Create(Self.GPSReceiverStateChange);
    FLinksList.Add(
      VGPSReceiverStateChangeListener,
      GState.GPSpar.ConnectingNotifier
    );
    FLinksList.Add(
      VGPSReceiverStateChangeListener,
      GState.GPSpar.DisconnectedNotifier
    );

    FLinksList.Add(
      TNotifyEventListenerSync.Create(Self.GPSReceiverConnect),
      GState.GPSpar.ConnectedNotifier
    );
    FLinksList.Add(
      TNotifyEventListenerSync.Create(Self.GPSReceiverDisconnect),
      GState.GPSpar.DisconnectedNotifier
    );
    FLinksList.Add(
      TNotifyEventListenerSync.Create(Self.GPSReceiverConnectError),
      GState.GPSpar.ConnectErrorNotifier
    );
    FLinksList.Add(
      TNotifyEventListenerSync.Create(Self.GPSReceiverTimeout),
      GState.GPSpar.TimeOutNotifier
    );
    FLinksList.Add(
      TNotifyEventListener.Create(Self.GPSReceiverReceive),
      GState.GPSpar.DataReciveNotifier
    );

    VMainFormMainConfigChangeListener := TNotifyEventListenerSync.Create(Self.OnMainFormMainConfigChange);
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      FConfig.MainConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      GState.BitmapPostProcessingConfig.GetChangeNotifier
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
      GState.ViewConfig.GetChangeNotifier
    );
    FLinksList.Add(
      VMainFormMainConfigChangeListener,
      FConfig.LayersConfig.MarksShowConfig.GetChangeNotifier
    );


    FLinksList.Add(
      TNotifyEventListener.Create(Self.OnFillingMapChange),
      FConfig.LayersConfig.FillingMapLayerConfig.GetChangeNotifier
    );

    FLinksList.Add(
      TNotifyEventListener.Create(Self.OnTimerEvent),
      GState.GUISyncronizedTimerNotifier
    );

    ProgramStart:=false;

    if ParamCount > 1 then begin
      try
        param:=paramstr(1);
        if param<>'' then begin
          try
            VGUID := StringToGUID(param);
          except
            VGUID := CGUID_Zero;
          end;
          if not IsEqualGUID(VGUID, CGUID_Zero) then begin
            FConfig.MainMapsConfig.SelectMainByGUID(VGUID);
          end;
        end;
        if  (paramstr(2)<>'') and (paramstr(3)<>'')and(paramstr(4)<>'') then begin
          VZoom := strtoint(paramstr(2)) - 1;
          FConfig.ViewPortState.ChangeZoomWithFreezeAtCenter(VZoom);
          VLonLat.X := str2r(paramstr(3));
          VLonLat.Y := str2r(paramstr(4));
          FConfig.ViewPortState.ChangeLonLat(VLonLat);
        end else if paramstr(2)<>'' then begin
          VZoom := strtoint(paramstr(2)) - 1;
          FConfig.ViewPortState.ChangeZoomWithFreezeAtCenter(VZoom);
        end;
      except
      end;
    end;
    InitSearchers;
    CreateLangMenu;
    FMapMoving:=false;

    SetProxy;

    if GState.WebReportToAuthor then begin
      frmInvisibleBrowser.NavigateAndWait('http://sasgis.ru/stat/index.html');
    end;

    FLinksList.ActivateLinks;
    FLayersList.StartThreads;
    GState.StartThreads;
    FUIDownLoader.StartThreads;
    OnMainFormMainConfigChange(nil);
    MapLayersVisibleChange(nil);
    OnFillingMapChange(nil);
    OnMainMapChange(nil);
    ProcessPosChangeMessage(nil);

    PaintZSlider(FConfig.ViewPortState.GetCurrentZoom);
  finally
    Enabled:=true;
    map.SetFocus;
    TfrmStartLogo.ReadyToHideLogo;
  end;
  TBXMainMenu.ProcessShortCuts:=true;
end;

procedure TfrmMain.InitSearchers;
var
  VGoto: IMapViewGoto;
  VItem: IGeoCoderListEntity;
  VTBXItem: TTBXItem;
  VTBEditItem: TTBEditItem;
begin
  VGoto := TMapViewGotoOnFMain.Create(Self.topos);
  FSearchPresenter := TSearchResultPresenterWithForm.Create(VGoto);
  VItem := FConfig.MainGeoCoderConfig.GetList.Get(CGeoCoderGoogleGUID);
  VTBXItem := TBXSelectGoogleSrch;
  VTBEditItem := tbiEditGoogleSrch;

  VTBEditItem.Tag := Integer(VItem);
  VTBEditItem.OnAcceptText := Self.tbiEditSrchAcceptText;
  VTBEditItem.EditCaption := VItem.GetCaption;
  VTBEditItem.Caption := VItem.GetCaption;
  VTBXItem.Tag := Integer(VItem);
  VTBXItem.OnClick := Self.TBXSelectSrchClick;
  VTBXItem.Caption := VItem.GetCaption;

  VItem := FConfig.MainGeoCoderConfig.GetList.Get(CGeoCoderYandexGUID);
  VTBXItem := TBXSelectYandexSrch;
  VTBEditItem := tbiEditYandexSrch;

  VTBEditItem.Tag := Integer(VItem);
  VTBEditItem.OnAcceptText := Self.tbiEditSrchAcceptText;
  VTBEditItem.EditCaption := VItem.GetCaption;
  VTBEditItem.Caption := VItem.GetCaption;
  VTBXItem.Tag := Integer(VItem);
  VTBXItem.OnClick := Self.TBXSelectSrchClick;
  VTBXItem.Caption := VItem.GetCaption;
end;

procedure TfrmMain.CreateLangMenu;
var
  i: Integer;
  VManager: ILanguageManager;
begin
  VManager := GState.LanguageManager;
  for i := 0 to VManager.GetCount - 1 do begin
    TLanguageTBXItem.Create(Self, TBLang, VManager, i);
  end;
end;

procedure TfrmMain.CreateMapUI;
begin
  if GState.MapType.Count>0 then begin
    CreateMapUIMapsList;
    CreateMapUILayersList;
    CreateMapUIFillingList;
    CreateMapUILayerSubMenu;
  end;
end;

procedure TfrmMain.CreateMapUIFillingList;
var
  VGenerator: TMapMenuGeneratorBasic;
begin
  VGenerator := TMapMenuGeneratorBasic.Create(
    FConfig.LayersConfig.FillingMapLayerConfig.GetSourceMap.GetMapsSet,
    TBFillingTypeMap,
    Self.TBfillMapAsMainClick,
    GState.MapTypeIcons18List,
    false
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
begin
  VGenerator := TMapMenuGeneratorBasic.Create(
    FConfig.MainMapsConfig.GetLayers,
    TBLayerSel,
    Self.OnClickLayerItem,
    GState.MapTypeIcons18List,
    true
  );
  try
   VGenerator.BuildControls;
  finally
    FreeAndNil(VGenerator);
  end;
end;

procedure TfrmMain.CreateMapUILayerSubMenu;
var
  i: integer;
  VMapType: TMapType;

  NLayerParamsItem: TTBXItem; //Пункт гланого меню Параметры/Параметры слоя
  NDwnItem: TTBXItem; //Пункт контекстного меню Загрузить тайл слоя
  NDelItem: TTBXItem; //Пункт контекстного меню Удалить тайл слоя
  NOpenDirItem: TTBXItem;
  NCopyLinkItem: TTBXItem;
  NLayerInfoItem: TTBXItem;

  VIcon18Index: Integer;
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

  if GState.MapType.Count>0 then begin
    for i:=0 to GState.MapType.Count-1 do begin
      VMapType := GState.MapType[i];
      VIcon18Index := GState.MapTypeIcons18List.GetIconIndexByGUID(VMapType.GUID);
      if VMapType.asLayer then begin
        NDwnItem:=TTBXItem.Create(ldm);
        FNDwnItemList.Add(VMapType.GUID, NDwnItem);
        NDwnItem.Caption:=VMapType.name;
        NDwnItem.ImageIndex:=VIcon18Index;
        NDwnItem.OnClick:=N21Click;
        NDwnItem.Tag:=longint(VMapType);
        ldm.Add(NDwnItem);

        NDelItem:=TTBXItem.Create(dlm);
        FNDelItemList.Add(VMapType.GUID, NDelItem);
        NDelItem.Caption:=VMapType.name;
        NDelItem.ImageIndex:=VIcon18Index;
        NDelItem.OnClick:=NDelClick;
        NDelItem.Tag:=longint(VMapType);
        dlm.Add(NDelItem);

        NOpenDirItem:=TTBXItem.Create(TBOpenDirLayer);
        FNOpenDirItemList.Add(VMapType.GUID, NOpenDirItem);
        NOpenDirItem.Caption:=VMapType.name;
        NOpenDirItem.ImageIndex:=VIcon18Index;
        NOpenDirItem.OnClick:=N25Click;
        NOpenDirItem.Tag:=longint(VMapType);
        TBOpenDirLayer.Add(NOpenDirItem);

        NCopyLinkItem:=TTBXItem.Create(TBCopyLinkLayer);
        FNCopyLinkItemList.Add(VMapType.GUID, NCopyLinkItem);
        NCopyLinkItem.Caption:=VMapType.name;
        NCopyLinkItem.ImageIndex:=VIcon18Index;
        NCopyLinkItem.OnClick:=N13Click;
        NCopyLinkItem.Tag:=longint(VMapType);
        TBCopyLinkLayer.Add(NCopyLinkItem);

        NLayerParamsItem:=TTBXItem.Create(NLayerParams);
        FNLayerParamsItemList.Add(VMapType.GUID, NLayerParamsItem);
        NLayerParamsItem.Caption:=VMapType.name;
        NLayerParamsItem.ImageIndex:=VIcon18Index;
        NLayerParamsItem.OnClick:=NMapParamsClick;
        NLayerParamsItem.Tag:=longint(VMapType);
        NLayerParams.Add(NLayerParamsItem);

        NLayerInfoItem:=TTBXItem.Create(TBLayerInfo);
        FNLayerInfoItemList.Add(VMapType.GUID, NLayerInfoItem);
        NLayerInfoItem.Caption:=VMapType.name;
        NLayerInfoItem.ImageIndex:=VIcon18Index;
        NLayerInfoItem.OnClick:=NMapInfoClick;
        NLayerInfoItem.Tag:=longint(VMapType);
        TBLayerInfo.Add(NLayerInfoItem);
      end;
    end;
  end;
end;

procedure TfrmMain.CreateMapUIMapsList;
var
  VGenerator: TMapMenuGeneratorBasic;
begin
  VGenerator := TMapMenuGeneratorBasic.Create(
    FConfig.MainMapsConfig.GetMapsSet,
    TBSMB,
    Self.OnClickMapItem,
    GState.MapTypeIcons18List,
    true
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
  Result.Add(NLayerSel);
  Result.Add(TBFillingTypeMap);
  Result.Add(NLayerParams);
  Result.Add(TBLang);
  Result.Add(N002);
  Result.Add(N003);
  Result.Add(N004);
  Result.Add(N005);
  Result.Add(N006);
  Result.Add(N007);
  Result.Add(NFillMap);
  if not GState.ShowDebugInfo then begin
    Result.Add(tbitmShowDebugInfo);
  end; 
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i:integer;
begin
  ProgramClose:=true;
  FLinksList.DeactivateLinks;
  //останавливаем GPS
  GState.SendTerminateToThreads;
  for i := 0 to Screen.FormCount - 1 do begin
    if (Screen.Forms[i]<>Application.MainForm)and(Screen.Forms[i].Visible) then begin
      Screen.Forms[i].Close;
    end;
  end;
  FUIDownLoader.SendTerminateToThreads;
  FLayersList.SendTerminateToThreads;
  Application.ProcessMessages;
  if GState.MapType.Count > 0 then frmSettings.Save(GState.MainConfigProvider);
  Application.ProcessMessages;
  FreeAndNil(FLayersList);
  FreeAndNil(FUIDownLoader);
  FreeAndNil(FShortCutManager);
  FreeAndNil(FMarkDBGUI);
end;

destructor TfrmMain.Destroy;
begin
  FLineOnMapEdit := nil;
  FWinPosition := nil;
  FSearchPresenter := nil;
  FNLayerParamsItemList := nil;
  FNLayerInfoItemList := nil;
  FNDwnItemList := nil;
  FNDelItemList := nil;
  FNOpenDirItemList := nil;
  FNCopyLinkItemList := nil;
  FLinksList := nil;
  FreeAndNil(FTumbler);
  FreeAndNil(FRuller);
  inherited;
end;

procedure TfrmMain.MapLayersVisibleChange(Sender: TObject);
var
  VUseDownload: TTileSource;
begin
  Showstatus.Checked := FConfig.LayersConfig.StatBar.Visible;
  if Showstatus.Checked then begin
    FLayerScaleLine.BottomMargin := FConfig.LayersConfig.StatBar.Height;
    FLayerMiniMap.BottomMargin := FConfig.LayersConfig.StatBar.Height;
  end else begin
    FLayerScaleLine.BottomMargin := 0;
    FLayerMiniMap.BottomMargin := 0;
  end;
  ShowMiniMap.Checked := FConfig.LayersConfig.MiniMapLayerConfig.Visible;
  ShowLine.Checked := FConfig.LayersConfig.ScaleLineConfig.Visible;
  NShowSelection.Checked := FConfig.LayersConfig.LastSelectionLayerConfig.Visible;
  N32.Checked := FConfig.LayersConfig.CenterScaleConfig.Visible;

  TBGPSPath.Checked := FConfig.LayersConfig.GPSTrackConfig.Visible;
  tbitmGPSTrackShow.Checked := TBGPSPath.Checked;
  VUseDownload := FConfig.DownloadUIConfig.UseDownload;
  TBSrc.ImageIndex := integer(VUseDownload);
  case VUseDownload of
    tsInternet: NSRCinet.Checked:=true;
    tsCache: NSRCesh.Checked:=true;
    tsCacheInternet: NSRCic.Checked:=true;
  end;

  mapResize(nil);
end;

procedure TfrmMain.ProcessPosChangeMessage(Sender: TObject);
var
  VZoomCurr: Byte;
  VGPSLonLat: TDoublePoint;
  VGPSMapPoint: TDoublePoint;
  VCenterMapPoint: TDoublePoint;
  VConverter: ILocalCoordConverter;
begin
  VConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VZoomCurr := VConverter.GetZoom;
  VCenterMapPoint := VConverter.GetCenterMapPixelFloat;
  VGPSLonLat := GState.GPSRecorder.LastPosition;
  VGPSMapPoint := VConverter.GetGeoConverter.LonLat2PixelPosFloat(VGPSLonLat, VConverter.GetZoom);
  FCenterToGPSDelta.X := VGPSMapPoint.X - VCenterMapPoint.X;
  FCenterToGPSDelta.Y := VGPSMapPoint.Y - VCenterMapPoint.Y;

  NZoomIn.Enabled:=TBZoomIn.Enabled;
  NZoomOut.Enabled:=TBZoom_Out.Enabled;
  labZoom.caption:= 'z' + inttostr(VZoomCurr + 1);
end;

procedure TfrmMain.CopyBtmToClipboard(btm: TBitmap);
var hSourcDC, hDestDC, hBM, hbmOld: THandle;
begin
  hSourcDC := btm.Canvas.Handle;
  hDestDC := CreateCompatibleDC(hSourcDC);
  hBM := CreateCompatibleBitmap(hSourcDC, btm.width, btm.height);
  hbmold:= SelectObject(hDestDC, hBM);
  BitBlt(hDestDC, 0, 0, btm.width, btm.height, hSourcDC, 0, 0, SRCCopy);
  OpenClipBoard(handle);
  EmptyClipBoard;
  SetClipBoardData(CF_Bitmap, hBM);
  CloseClipBoard;
  SelectObject(hDestDC,hbmold);
  DeleteObject(hbm);
  DeleteDC(hDestDC);
  DeleteDC(hSourcDC);
end;

procedure TfrmMain.CopyStringToClipboard(s: Widestring);
var hg: THandle;
    P: PChar;
begin
  if OpenClipboard(Handle) then
  begin
    try
      EmptyClipBoard;
      hg:=GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, Length(S)+1);
      try
        P:=GlobalLock(hg);
        try
          StrPCopy(P, s);
          SetClipboardData(CF_TEXT, hg);
        finally
          GlobalUnlock(hg);
        end;
      except
        GlobalFree(hg);
        raise
      end;
    finally
      CloseClipboard;
    end;
  end
end;

procedure TfrmMain.setalloperationfalse(newop: TAOperation);
begin
 if FCurrentOper=newop then newop:=ao_movemap;
 FMarshrutComment:='';
 TBmove.Checked:=newop=ao_movemap;
 TBCalcRas.Checked:=newop=ao_calc_line;
 TBRectSave.Checked:=(newop=ao_select_poly)or(newop=ao_select_rect);
 TBAdd_Point.Checked:=newop=ao_Add_Point;
 TBAdd_Line.Checked:=newop=ao_Add_line;
 TBAdd_Poly.Checked:=newop=ao_Add_Poly;
 TBEditPath.Visible:=false;
 TBEditPathSave.Visible:=(newop=ao_Add_line)or(newop=ao_Add_Poly)or(newop=ao_Edit_line)or(newop=ao_Edit_Poly);
 TBEditPathOk.Visible:=(newop=ao_select_poly);
 TBEditPathLabel.Visible:=(newop=ao_calc_line);
 TBEditPathMarsh.Visible:=(newop=ao_Add_line)or(newop=ao_Edit_line);
 Frect_dwn:=false;
 FLineOnMapEdit.Empty;
 Frect_p2:=false;
 case newop of
  ao_movemap:  map.Cursor:=crDefault;
  ao_calc_line:     map.Cursor:=2;
  ao_select_poly,ao_select_rect: map.Cursor:=crDrag;
  ao_Add_Point,ao_Add_Poly,ao_Add_Line,ao_edit_Line,ao_edit_poly: map.Cursor:=4;
 end;
  FCurrentOper:=newop;
  if not(FCurrentOper in[ao_edit_line,ao_edit_poly]) then begin
    FEditMark := nil;
  end;
  if FCurrentOper <> ao_calc_line then begin
    FCalcLineLayer.DrawNothing;
  end;
  if not(FCurrentOper in [ao_edit_line, ao_add_line]) then begin
    FMarkPolyLineLayer.DrawNothing;
  end;
  if not(FCurrentOper in [ao_edit_poly, ao_add_poly]) then begin
    FMarkPolygonLayer.DrawNothing;
  end;
  if FCurrentOper <> ao_select_poly then begin
    FSelectionPolygonLayer.DrawNothing;
  end;
  if FCurrentOper <> ao_select_rect then begin
    FSelectionRectLayer.DrawNothing;
  end;
end;

procedure TfrmMain.OnClickLayerItem(Sender: TObject);
var
  VSender: TTBCustomItem;
  VAtiveMap: IActiveMapSingle;
  VMapType: IMapType;
begin
  if Sender is TTBCustomItem then begin
    VSender := TTBCustomItem(Sender);
    VAtiveMap := IActiveMapSingle(VSender.Tag);
    if VAtiveMap <> nil then begin
      VMapType := VAtiveMap.GetMapType;
      if VMapType <> nil then begin
        FConfig.MainMapsConfig.LockWrite;
        try
          if not FConfig.MainMapsConfig.GetLayers.IsGUIDSelected(VMapType.GUID) then begin
            FConfig.MainMapsConfig.SelectLayerByGUID(VMapType.GUID);
          end else begin
            FConfig.MainMapsConfig.UnSelectLayerByGUID(VMapType.GUID);
          end;
        finally
          FConfig.MainMapsConfig.UnlockWrite;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.OnClickMapItem(Sender: TObject);
var
  VSender: TComponent;
  VAtiveMap: IActiveMapSingle;
  VMapType: IMapType;
begin
  if Sender is TComponent then begin
    VSender := TComponent(Sender);
    VAtiveMap := IActiveMapSingle(VSender.Tag);
    if VAtiveMap <> nil then begin
      VMapType := VAtiveMap.GetMapType;
      if VMapType <> nil then begin
        FConfig.MainMapsConfig.SelectMainByGUID(VMapType.GUID);
      end;
    end;
  end;
end;

procedure TfrmMain.OnFillingMapChange(Sender: TObject);
begin
  if FConfig.LayersConfig.FillingMapLayerConfig.Visible then begin
    TBMapZap.Caption:='z'+inttostr(FConfig.LayersConfig.FillingMapLayerConfig.SourceZoom + 1);
  end else begin
    TBMapZap.Caption:='';
  end;
end;

procedure TfrmMain.OnLineOnMapEditChange(Sender: TObject);
begin
  FLineOnMapEdit.LockRead;
  try
    TBEditPath.Visible:=(FLineOnMapEdit.GetCount > 1);
    case FCurrentOper of
      ao_select_poly:
        FSelectionPolygonLayer.DrawLine(FLineOnMapEdit.GetPoints, FLineOnMapEdit.GetActiveIndex);
      ao_calc_line:
        FCalcLineLayer.DrawLine(FLineOnMapEdit.GetPoints, FLineOnMapEdit.GetActiveIndex);
      ao_add_poly,
      ao_edit_poly:
        FMarkPolygonLayer.DrawLine(FLineOnMapEdit.GetPoints, FLineOnMapEdit.GetActiveIndex);
      ao_edit_line,
      ao_add_line:
        FMarkPolyLineLayer.DrawLine(FLineOnMapEdit.GetPoints, FLineOnMapEdit.GetActiveIndex);
    end;
  finally
    FLineOnMapEdit.UnlockRead;
  end;
end;

procedure TfrmMain.OnMainFormMainConfigChange(Sender: TObject);
var
  VGUID: TGUID;
  i: Integer;
  VToolbarItem: TTBCustomItem;
  VItem: IGeoCoderListEntity;
begin
  Nbackload.Checked := GState.ViewConfig.UsePrevZoomAtMap;
  NbackloadLayer.Checked := GState.ViewConfig.UsePrevZoomAtLayer;
  map.Color := GState.ViewConfig.BackGroundColor;

  NGoToCur.Checked := FConfig.MainConfig.GetZoomingAtMousePos;
  Ninvertcolor.Checked:=GState.BitmapPostProcessingConfig.InvertColor;
  TBGPSToPoint.Checked:=FConfig.GPSBehaviour.MapMove;
  tbitmGPSCenterMap.Checked:=TBGPSToPoint.Checked;
  TBGPSToPointCenter.Checked:=FConfig.GPSBehaviour.MapMoveCentered;
  tbitmGPSToPointCenter.Checked:=TBGPSToPointCenter.Checked;
  NBlock_toolbars.Checked:=GState.MainFormConfig.ToolbarsLock.GetIsLock;

  TBHideMarks.Checked := not(FConfig.LayersConfig.MarksShowConfig.IsUseMarks);

  if FConfig.MainConfig.ShowMapName then begin
    TBSMB.Caption := FConfig.MainMapsConfig.GetSelectedMapType.MapType.Name;
  end else begin
    TBSMB.Caption := '';
  end;

  Nanimate.Checked := FConfig.MainConfig.AnimateZoom;

  VGUID := FConfig.MainGeoCoderConfig.ActiveGeoCoderGUID;
  for i := 0 to TBXSelectSrchType.Count - 1 do begin
    VToolbarItem := TBXSelectSrchType.Items[i];
    VItem := IGeoCoderListEntity(VToolbarItem.Tag);
    if VItem <> nil then begin
      if IsEqualGUID(VGUID, VItem.GetGUID) then begin
        VToolbarItem.Checked := True;
        TBXSelectSrchType.Caption := VToolbarItem.Caption;
      end;
    end;
  end;
end;

procedure TfrmMain.OnMainMapChange(Sender: TObject);
var
  VGUID: TGUID;
  VMapType: IMapType;
begin
  VGUID := FConfig.MainMapsConfig. GetActiveMap.GetSelectedGUID;

  TBSMB.ImageIndex := GState.MapTypeIcons24List.GetIconIndexByGUID(VGUID);
  if FConfig.MainConfig.ShowMapName then begin
    VMapType := FConfig.MainMapsConfig.GetActiveMap.GetMapsList.GetMapTypeByGUID(VGUID);
    TBSMB.Caption := VMapType.MapType.Name;
  end else begin
    TBSMB.Caption := '';
  end;
end;

procedure TfrmMain.OnMapTileUpdate(AMapType: TMapType; AZoom: Byte;
  ATile: TPoint);
begin
  if AMapType <> nil then begin
    AMapType.Cache.DeleteTileFromCache(ATile, AZoom);
    if AMapType.IsBitmapTiles then begin
      if FMainLayer <> nil then begin
        FMainLayer.Redraw;
      end;
    end else if AMapType.IsKmlTiles then begin
      if FWikiLayer <> nil then begin
        FWikiLayer.Redraw;
      end;
    end;
  end;
end;

procedure TfrmMain.OnMapUpdate(AMapType: TMapType);
begin
  if AMapType <> nil then begin
    AMapType.Cache.Clear;
    if AMapType.IsBitmapTiles then begin
      if FMainLayer <> nil then begin
        FMainLayer.Redraw;
      end;
    end else if AMapType.IsKmlTiles then begin
      if FWikiLayer <> nil then begin
        FWikiLayer.Redraw;
      end;
    end;
  end;
end;

procedure TfrmMain.SetToolbarsLock(AValue: Boolean);
begin
  TBDock.AllowDrag := not AValue;
  TBDockLeft.AllowDrag := not AValue;
  TBDockRight.AllowDrag := not AValue;
  TBDockBottom.AllowDrag := not AValue;
end;

procedure TfrmMain.OnToolbarsLockChange(Sender: TObject);
begin
  SetToolbarsLock(FConfig.ToolbarsLock.GetIsLock);
end;

procedure TfrmMain.OnWinPositionChange(Sender: TObject);
var
  VIsFullScreen: Boolean;
  VIsMaximized: Boolean;
  VRect: TRect;
begin
  FWinPosition.LockRead;
  try
    VIsFullScreen := FWinPosition.GetIsFullScreen;
    VIsMaximized := FWinPosition.GetIsMaximized;
    VRect := FWinPosition.GetBoundsRect;
  finally
    FWinPosition.UnlockRead;
  end;
  TBFullSize.Checked := VIsFullScreen;
  NFoolSize.Checked:=VIsFullScreen;
  TBexit.Visible:=VIsFullScreen;
  TBDock.Parent:=Self;
  TBDockLeft.Parent:=Self;
  TBDockBottom.Parent:=Self;
  TBDockRight.Parent:=Self;
  TBDock.Visible:=not(VIsFullScreen);
  TBDockLeft.Visible:=not(VIsFullScreen);
  TBDockBottom.Visible:=not(VIsFullScreen);
  TBDockRight.Visible:=not(VIsFullScreen);
  if VIsFullScreen then begin
    Self.WindowState := wsMaximized;
    SetBounds(
      Left-ClientOrigin.X,
      Top-ClientOrigin.Y,
      GetDeviceCaps(Canvas.handle, HORZRES) + (Width - ClientWidth),
      GetDeviceCaps(Canvas.handle, VERTRES) + (Height - ClientHeight)
    );
  end else begin
    if VIsMaximized then begin
      if Self.WindowState <> wsMaximized then begin
        if not EqualRect(BoundsRect, VRect) then begin
          Self.BoundsRect:= VRect;
        end;
      end;
      Self.WindowState := wsMaximized;
      SetBounds(
        0,
        0,
        GetDeviceCaps(Canvas.handle, HORZRES),
        GetDeviceCaps(Canvas.handle, VERTRES)
      );
    end else begin
      Self.WindowState := wsNormal;
      Self.BoundsRect:= VRect;
    end;
  end;
end;

//Обработка нажатий кнопоки и калесика
procedure TfrmMain.DoMessageEvent(var Msg: TMsg; var Handled: Boolean);
var
  z: integer;
  dWMB: integer;
  VZoom: Byte;
  VNewZoom: integer;
  VMoveByDelta: Boolean;
  VPointDelta: TDoublePoint;
begin
  if Active then begin
    if not FMapZoomAnimtion then begin
      case Msg.message of
        WM_MOUSEWHEEL: begin
          MouseCursorPos:=FmoveTrue;
          if FConfig.MainConfig.MouseScrollInvert then z:=-1 else z:=1;
          VZoom := FConfig.ViewPortState.GetCurrentZoom;
          if Msg.wParam<0 then begin
            VNewZoom := VZoom-(1*z);
          end else begin
            VNewZoom := VZoom+(1*z);
          end;
          if VNewZoom < 0 then VNewZoom := 0;
          zooming(VNewZoom, FConfig.MainConfig.ZoomingAtMousePos);
        end;
        WM_KEYFIRST: begin
          VMoveByDelta := False;
          case Msg.wParam of
            VK_RIGHT,
            VK_LEFT,
            VK_DOWN,
            VK_UP: VMoveByDelta := True;
          end;
          if VMoveByDelta then begin
            if (FdWhenMovingButton<35) then begin
              inc(FdWhenMovingButton);
            end;
            dWMB:=trunc(Power(FdWhenMovingButton,1.5));
            case Msg.wParam of
              VK_RIGHT: VPointDelta := DoublePoint(dWMB, 0);
              VK_LEFT: VPointDelta := DoublePoint(-dWMB, 0);
              VK_DOWN: VPointDelta := DoublePoint(0, dWMB);
              VK_UP: VPointDelta := DoublePoint(0, -dWMB);
            else
              VPointDelta := DoublePoint(0, 0);
            end;
            map.BeginUpdate;
            try
              FConfig.ViewPortState.ChangeMapPixelByDelta(VPointDelta);
            finally
              map.EndUpdate;
              map.Changed;
            end;
          end;
        end;
        WM_KEYUP: begin
          FdWhenMovingButton:=5;
          case Msg.wParam of
            VK_BACK: begin
              if FCurrentOper in [ao_calc_line, ao_select_poly, ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly] then begin
               FLineOnMapEdit.DeleteActivePoint;
              end;
            end;
            VK_ESCAPE: begin
              case FCurrentOper of
                ao_select_rect: begin
                  if Frect_dwn then begin
                    setalloperationfalse(ao_movemap);
                    setalloperationfalse(ao_select_rect);
                  end else begin
                    setalloperationfalse(ao_movemap);
                  end;
                end;
                ao_Add_Point: begin
                  setalloperationfalse(ao_movemap);
                end;
                ao_select_poly,
                ao_calc_line,
                ao_add_line,
                ao_add_poly,
                ao_edit_line,
                ao_edit_poly: begin
                  if (FLineOnMapEdit.GetCount>0) then begin
                    FLineOnMapEdit.Empty;
                  end else begin
                    setalloperationfalse(ao_movemap);
                  end;
                end;
              end;
            end;
            VK_RETURN: begin
              case FCurrentOper of
                ao_add_Poly,
                ao_edit_Poly: begin
                  if FLineOnMapEdit.GetCount > 2 then begin
                    TBEditPathSaveClick(Self);
                  end;
                end;
                ao_add_line,
                ao_edit_line: begin
                  if FLineOnMapEdit.GetCount > 1 then begin
                    TBEditPathSaveClick(Self);
                  end;
                end;
                ao_select_poly: begin
                  if FLineOnMapEdit.GetCount > 2 then begin
                    TBEditPathOkClick(Self)
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.PrepareSelectionRect(Shift: TShiftState;
  var ASelectedLonLat: TDoubleRect);
var
  VConverter: ICoordConverter;
  VTemp: Double;
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;

  VConverter.CheckLonLatRect(ASelectedLonLat);
  if ASelectedLonLat.Left > ASelectedLonLat.Right then begin
    VTemp := ASelectedLonLat.Left;
    ASelectedLonLat.Left := ASelectedLonLat.Right;
    ASelectedLonLat.Right := VTemp;
  end;
  if ASelectedLonLat.Top < ASelectedLonLat.Bottom then begin
    VTemp := ASelectedLonLat.Top;
    ASelectedLonLat.Top := ASelectedLonLat.Bottom;
    ASelectedLonLat.Bottom := VTemp;
  end;
  if (ssCtrl in Shift) then begin
    ASelectedLonLat := FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.GetRectStickToGrid(VLocalConverter, ASelectedLonLat);
  end;
  if (ssShift in Shift) then begin
    ASelectedLonLat := FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.GetRectStickToGrid(VLocalConverter, ASelectedLonLat);
  end;
end;

procedure TfrmMain.UpdateGPSSatellites;
var
  i,bar_width,bar_height,bar_x1,bar_dy:integer;
  VPosition: IGPSPosition;
  VSattelite: IGPSSatelliteInfo;
begin
   TBXSignalStrengthBar.Repaint;
   VPosition := GState.GPSRecorder.CurrentPosition;
   if VPosition.Satellites.FixCount > 0 then begin
    with TBXSignalStrengthBar do begin
       Canvas.Lock;
       try
         Canvas.Pen.Color:=clBlack;
         Canvas.Brush.Color:=clGreen;
         bar_x1:=0;
         bar_dy:=8;
         bar_width:=((Width-15) div VPosition.Satellites.FixCount);
         for I := 0 to VPosition.Satellites.Count-1 do begin
           VSattelite := VPosition.Satellites.Item[i];
           if VSattelite.IsFix then begin
             bar_height:=trunc(14*((VSattelite.SignalToNoiseRatio)/100));
             Canvas.Rectangle(bar_x1+2,Height-bar_dy-bar_height,bar_x1+bar_width-2,Height-bar_dy);
             inc(bar_x1,bar_width);
           end;
         end;
       finally
         Canvas.Unlock;
       end;
    end;
   end;
end;

procedure TfrmMain.OnTimerEvent(Sender: TObject);
var
  VGPSNewPos: TDoublePoint;
  VCenterToGPSDelta: TDoublePoint;
  VPointDelta: TDoublePoint;
  VCenterMapPoint: TDoublePoint;
  VGPSMapPoint: TDoublePoint;
  VPosition: IGPSPosition;
  VConverter: ILocalCoordConverter;
  VMapMove: Boolean;
  VMapMoveCentred: Boolean;
  VMinDelta: Double;
  VProcessGPSIfActive: Boolean;
  VDelta: Double;
  VNeedTrackRedraw: Boolean;
begin
  if FIsGPSPosChanged then begin
    FIsGPSPosChanged := False;
    VPosition := GState.GPSRecorder.CurrentPosition;
    if frmSettings.Visible then frmSettings.SatellitePaint;
    if TBXSignalStrengthBar.Visible then UpdateGPSSatellites;
    if (VPosition.IsFix=0) then exit;
    if not((FMapMoving)or(FMapZoomAnimtion)) then begin
      FConfig.GPSBehaviour.LockRead;
      try
        VMapMove := FConfig.GPSBehaviour.MapMove;
        VMapMoveCentred := FConfig.GPSBehaviour.MapMoveCentered;
        VMinDelta := FConfig.GPSBehaviour.MinMoveDelta;
        VProcessGPSIfActive := FConfig.GPSBehaviour.ProcessGPSIfActive;
      finally
        FConfig.GPSBehaviour.UnlockRead;
      end;
      if (not VProcessGPSIfActive) or (Screen.ActiveForm=Self) then begin
        VNeedTrackRedraw := True;
        if (VMapMove) then begin
          VGPSNewPos := GState.GPSRecorder.LastPosition;
          if VMapMoveCentred then begin
            VConverter := FConfig.ViewPortState.GetVisualCoordConverter;
            VCenterMapPoint := VConverter.GetCenterMapPixelFloat;
            VGPSMapPoint := VConverter.GetGeoConverter.LonLat2PixelPosFloat(VGPSNewPos, VConverter.GetZoom);
            VPointDelta.X := VCenterMapPoint.X - VGPSMapPoint.X;
            VPointDelta.Y := VCenterMapPoint.Y - VGPSMapPoint.Y;
            VDelta := Sqrt(Sqr(VPointDelta.X) + Sqr(VPointDelta.Y));
            if VDelta > VMinDelta then begin
              FConfig.ViewPortState.ChangeLonLat(VGPSNewPos);
              VNeedTrackRedraw := False;
            end;
          end else begin
              VConverter := FConfig.ViewPortState.GetVisualCoordConverter;
              VGPSMapPoint := VConverter.GetGeoConverter.LonLat2PixelPosFloat(VGPSNewPos, VConverter.GetZoom);
              if PixelPointInRect(VGPSMapPoint, VConverter.GetRectInMapPixelFloat) then  begin
                VCenterMapPoint := VConverter.GetCenterMapPixelFloat;
                VCenterToGPSDelta.X := VGPSMapPoint.X - VCenterMapPoint.X;
                VCenterToGPSDelta.Y := VGPSMapPoint.Y - VCenterMapPoint.Y;
                VPointDelta := FCenterToGPSDelta;
                VPointDelta.X := VCenterToGPSDelta.X - VPointDelta.X;
                VPointDelta.Y := VCenterToGPSDelta.Y - VPointDelta.Y;
                VDelta := Sqrt(Sqr(VPointDelta.X) + Sqr(VPointDelta.Y));
                if VDelta > VMinDelta then begin
                  FConfig.ViewPortState.ChangeMapPixelByDelta(VPointDelta);
                  VNeedTrackRedraw := False;
                end;
              end;
          end;
        end;
        if VNeedTrackRedraw then begin
          FLayerMapGPS.Redraw;
        end;
      end;
    end;
  end;
  FLayerStatBar.Redraw;
end;

procedure TfrmMain.topos(LL:TDoublePoint;zoom_:byte;draw:boolean);
begin
  FConfig.ViewPortState.LockWrite;
  try
    FConfig.ViewPortState.ChangeLonLat(LL);
    FConfig.ViewPortState.ChangeZoomWithFreezeAtCenter(zoom_);
  finally
    FConfig.ViewPortState.UnlockWrite;
  end;
  if draw then begin
    FLayerGoto.ShowGotoIcon(LL);
  end;
end;

procedure TfrmMain.zooming(ANewZoom:byte;move:boolean);
  procedure usleep(mils:integer);
  var startTS,endTS,freqTS:int64;
  begin
   if mils>0 then begin
     QueryPerformanceCounter(startTS);
     repeat
       QueryPerformanceCounter(endTS);
       QueryPerformanceFrequency(freqTS);
     until ((endTS-startTS)/(freqTS/1000))>mils;
   end;
  end;
var i,steps:integer;
    ts1,ts2,fr:int64;
    Scale: Double;
    VZoom: Byte;
begin
  TBZoom_Out.Enabled := False;
  TBZoomIn.Enabled := False;
  NZoomIn.Enabled := False;
  NZoomOut.Enabled := False;
  VZoom := FConfig.ViewPortState.GetCurrentZoom;
  if (FMapZoomAnimtion)or(FMapMoving)or(ANewZoom>23) then exit;
  FMapZoomAnimtion:=True;

  if (abs(ANewZoom-VZoom)=1)and(FConfig.MainConfig.AnimateZoom) then begin
   steps:=11;
   for i:=0 to steps-1 do begin
     QueryPerformanceCounter(ts1);
      if VZoom>ANewZoom then begin
        //Scale := 1 - (i/(steps - 1))/2;
        Scale := 1/(1 + i/(steps - 1));
      end else begin
        //Scale := 1 + i/(steps - 1);
        Scale := 3 - (1/(1+i/(steps - 1)))*2;
      end;
      map.BeginUpdate;
      try
      if move then begin
        FConfig.ViewPortState.ScaleTo(Scale, MouseCursorPos);
      end else begin
        FConfig.ViewPortState.ScaleTo(Scale);
      end;
      finally
        map.EndUpdate;
        map.Changed;
      end;
     application.ProcessMessages;
     QueryPerformanceCounter(ts2);
     QueryPerformanceFrequency(fr);
     ts1:=round((ts2-ts1)/(fr/1000));
     if (25-ts1>0) then begin
       usleep(25-ts1);
     end;
   end;
   application.ProcessMessages;
  end;
  if move then begin
    FConfig.ViewPortState.ChangeZoomWithFreezeAtVisualPoint(ANewZoom, MouseCursorPos);
  end else begin
    FConfig.ViewPortState.ChangeZoomWithFreezeAtCenter(ANewZoom);
  end;

  if ANewZoom>0 then begin
    TBZoom_Out.Enabled:=true;
    NZoomOut.Enabled:=true;
  end;
  if ANewZoom<23 then begin
    TBZoomIn.Enabled:=true;
    NZoomIn.Enabled:=true;
  end;
  FMapZoomAnimtion:=False;
  PaintZSlider(FConfig.ViewPortState.GetCurrentZoom);
end;

procedure TfrmMain.NzoomInClick(Sender: TObject);
begin
 zooming(FConfig.ViewPortState.GetCurrentZoom + 1, false);
end;

procedure TfrmMain.NZoomOutClick(Sender: TObject);
begin
 zooming(FConfig.ViewPortState.GetCurrentZoom - 1, false);
end;


Procedure TfrmMain.FormMove(Var Msg: TWMMove);
Begin
  Inherited;
  FormResize(self);
End;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  if FWinPosition <> nil then begin
    if not FWinPosition.GetIsFullScreen then begin
      if Self.WindowState = wsMaximized then begin
        FWinPosition.SetMaximized;
      end else if Self.WindowState = wsNormal then begin
        FWinPosition.SetWindowPosition(Self.BoundsRect);
      end;
    end;
  end;
end;

procedure TfrmMain.TBmoveClick(Sender: TObject);
begin
 setalloperationfalse(ao_movemap);
end;

procedure TfrmMain.TBZoom_outClick(Sender: TObject);
begin
 zooming(FConfig.ViewPortState.GetCurrentZoom - 1, false);
end;

procedure TfrmMain.TBZoomInClick(Sender: TObject);
begin
 zooming(FConfig.ViewPortState.GetCurrentZoom + 1, false);
end;

procedure TfrmMain.WMGetMinMaxInfo(var msg:TWMGetMinMaxInfo);
begin
 inherited;
 with msg.MinMaxInfo^.ptMaxTrackSize do begin
  X:=GetDeviceCaps(Canvas.handle,HORZRES)+(Width-ClientWidth);
  Y:=GetDeviceCaps(Canvas.handle,VERTRES)+(Height-ClientHeight);
 end;
end;

procedure TfrmMain.TBFullSizeClick(Sender:TObject);
begin
  if TBFullSize.Checked then begin
    FWinPosition.SetFullScreen;
  end else begin
    FWinPosition.SetNoFullScreen;
  end;
end;

procedure TfrmMain.ZoomToolBarDockChanging(Sender: TObject; Floating: Boolean; DockingTo: TTBDock);
begin
  if (DockingTo=TBDockLeft)or(DockingTo=TBDockRight) then begin
    if FRuller.Width>FRuller.Height then begin
        FRuller.Rotate270();
        FTumbler.Rotate270();
    end;
    TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoom_out),4);
    TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoomin),0);
  end else begin
    if FRuller.Width<FRuller.Height then begin
        FRuller.Rotate90();
        FTumbler.Rotate90();
    end;
    TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoom_out),0);
    TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoomin),4);
  end;
  PaintZSlider(FConfig.ViewPortState.GetCurrentZoom);
end;

procedure TfrmMain.NCalcRastClick(Sender: TObject);
begin
 TBCalcRas.Checked:=true;
 TBCalcRasClick(self);
end;

procedure TfrmMain.NFoolSizeClick(Sender: TObject);
begin
  if NFoolSize.Checked then begin
    FWinPosition.SetFullScreen;
  end else begin
    FWinPosition.SetNoFullScreen;
  end;
end;

procedure TfrmMain.N6Click(Sender: TObject);
begin
 close;
end;

procedure TfrmMain.N8Click(Sender: TObject);
begin
  frmSettings.ShowModal;
end;

procedure TfrmMain.NbackloadClick(Sender: TObject);
begin
  GState.ViewConfig.UsePrevZoomAtMap := Nbackload.Checked;
end;

procedure TfrmMain.NbackloadLayerClick(Sender: TObject);
begin
  GState.ViewConfig.UsePrevZoomAtLayer := NbackloadLayer.Checked;
end;

procedure TfrmMain.NBlock_toolbarsClick(Sender: TObject);
begin
  GState.MainFormConfig.ToolbarsLock.SetLock(NBlock_toolbars.Checked);
end;

procedure TfrmMain.NaddPointClick(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VMouseMapPoint: TDoublePoint;
  VConverter: ICoordConverter;
  VZoomCurr: Byte;
  VMouseLonLat: TDoublePoint;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VZoomCurr := VLocalConverter.GetZoom;
  VConverter := VLocalConverter.GetGeoConverter;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseUpPoint);
  VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoomCurr, True);
  VMouseLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);
  if FMarkDBGUI.AddNewPointModal(VMouseLonLat) then begin
    setalloperationfalse(ao_movemap);
    FLayerMapMarks.Redraw;
  end;
end;

procedure TfrmMain.N20Click(Sender: TObject);
var
  btm:TBitmap32;
  btm1:TBitmap;
  VMouseMapPoint: TDoublePoint;
  VZoomCurr: Byte;
  VConverter: ICoordConverter;
  VMapType: TMapType;
  VLocalConverter: ILocalCoordConverter;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
  VZoomCurr := VLocalConverter.GetZoom;
  VConverter := VLocalConverter.GetGeoConverter;
  VMapType := FConfig.MainMapsConfig.GetSelectedMapType.MapType;

  VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoomCurr, True);
  VMouseLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);
  VMapType.GeoConvert.CheckLonLatPos(VMouseLonLat);
  VTile := VMapType.GeoConvert.LonLat2TilePos(VMouseLonLat, VZoomCurr);
  btm:=TBitmap32.Create;
  try
    if VMapType.LoadTile(btm, VTile, VZoomCurr, false, False) then begin
      btm1:=TBitmap.Create;
      try
        btm1.Width:=btm.Width;
        btm1.Height:=btm.Height;
        btm.DrawTo(btm1.Canvas.Handle,0,0);
        CopyBtmToClipboard(btm1);
      finally
        btm1.Free;
      end;
    end;
  finally
    btm.Free;
  end;
end;

procedure TfrmMain.N30Click(Sender: TObject);
var
  VMouseLonLat: TDoublePoint;
  VStr: string;
  VLocalConverter: ILocalCoordConverter;
  VZoomCurr: Byte;
  VConverter: ICoordConverter;
  VMouseMapPoint: TDoublePoint;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VZoomCurr := VLocalConverter.GetZoom;
  VConverter := VLocalConverter.GetGeoConverter;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
  VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoomCurr, True);
  VMouseLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);
  VStr := GState.ValueToStringConverterConfig.GetStaticConverter.LonLatConvert(VMouseLonLat);
  CopyStringToClipboard(VStr);
end;

procedure TfrmMain.N15Click(Sender: TObject);
var
  VZoomCurr: Byte;
  VMapType: TMapType;
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
begin
  VMapType := FConfig.MainMapsConfig.GetSelectedMapType.MapType;
  if VMapType.TileStorage.GetIsStoreFileCache then begin
    VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
    VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
    VZoomCurr := VLocalConverter.GetZoom;
    VConverter := VLocalConverter.GetGeoConverter;
    VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoomCurr, True);
    VMouseLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);
    VMapType.GeoConvert.CheckLonLatPos(VMouseLonLat);
    VTile := VMapType.GeoConvert.LonLat2TilePos(VMouseLonLat, VZoomCurr);

   // Копирование в имени файла в буффер обмена. Заменить на обобщенное имя тайла.
   CopyStringToClipboard(VMapType.GetTileFileName(VTile, VZoomCurr));
  end else begin
    ShowMessage(SAS_MSG_CantGetTileFileName);
  end;
end;

procedure TfrmMain.N21Click(Sender: TObject);
var
  path:string;
  VMapType:TMapType;
  VZoomCurr: Byte;
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
begin
  if TMenuItem(sender).Tag<>0 then begin
    VMapType := TMapType(TMenuItem(sender).Tag);
  end else begin
    VMapType := FConfig.MainMapsConfig.GetSelectedMapType.MapType;
  end;

  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseUpPoint);
  VZoomCurr := VLocalConverter.GetZoom;
  VConverter := VLocalConverter.GetGeoConverter;
  if VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoomCurr, True) then begin
    VMouseLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);
    if VMapType.GeoConvert.CheckLonLatPos(VMouseLonLat) then begin
      VTile := VMapType.GeoConvert.LonLat2TilePos(VMouseLonLat, VZoomCurr);

      path := VMapType.GetTileShowName(VTile, VZoomCurr);

      if ((not(VMapType.tileExists(VTile, VZoomCurr)))or
          (MessageBox(handle,pchar(Format(SAS_MSG_FileExists, [path])),pchar(SAS_MSG_coution),36)=IDYES))
      then begin
        TTileDownloaderUIOneTile.Create(
          VTile,
          VZoomCurr,
          VMapType,
          Self.OnMapTileUpdate,
          FShowErrorLayer
        );
      end;
    end;
  end;
end;

procedure TfrmMain.NopendirClick(Sender: TObject);
var
  VZoomCurr: Byte;
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
  VMapType: TMapType;
begin
  VMapType := FConfig.MainMapsConfig.GetSelectedMapType.MapType;
  if VMapType.TileStorage.GetIsStoreFileCache then begin
    VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
    VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
    VZoomCurr := VLocalConverter.GetZoom;
    VConverter := VLocalConverter.GetGeoConverter;
    VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoomCurr, True);
    VMouseLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);
    VMapType.GeoConvert.CheckLonLatPos(VMouseLonLat);
    VTile := VMapType.GeoConvert.LonLat2TilePos(VMouseLonLat, VZoomCurr);
    // Открыть файл в просмотрщике. Заменить на проверку возможности сделать это или дописать экспорт во временный файл.
    ShellExecute(0,'open',PChar(VMapType.GetTileFileName(VTile, VZoomCurr)),nil,nil,SW_SHOWNORMAL);
  end else begin
    ShowMessage(SAS_MSG_CantGetTileFileName);
  end;
end;

procedure TfrmMain.N25Click(Sender: TObject);
var
  s:string;
  VZoomCurr: Byte;
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
  VMapType: TMapType;
begin
  if TMenuItem(sender).Tag<>0 then begin
    VMapType := TMapType(TMenuItem(sender).Tag);
  end else begin
    VMapType := FConfig.MainMapsConfig.GetSelectedMapType.MapType;
  end;

  if VMapType.TileStorage.GetIsStoreFileCache then begin
    VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
    VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
    VZoomCurr := VLocalConverter.GetZoom;
    VConverter := VLocalConverter.GetGeoConverter;
    VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoomCurr, True);
    VMouseLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);
    VMapType.GeoConvert.CheckLonLatPos(VMouseLonLat);
    VTile := VMapType.GeoConvert.LonLat2TilePos(VMouseLonLat, VZoomCurr);
    s := VMapType.GetTileFileName(VTile, VZoomCurr);
    s := ExtractFilePath(s);
    ShellExecute(0,'open',PChar(s),nil,nil,SW_SHOWNORMAL);
  end else begin
    ShowMessage(SAS_MSG_CantGetTileFileName);
  end;
end;

procedure TfrmMain.NDelClick(Sender: TObject);
var
  s:string;
  VMapType:TMapType;
  VZoomCurr: Byte;
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
begin
  if TMenuItem(sender).Tag<>0 then begin
    VMapType := TMapType(TMenuItem(sender).Tag);
  end else begin
    VMapType := FConfig.MainMapsConfig.GetSelectedMapType.MapType;
  end;

  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseUpPoint);
  VZoomCurr := VLocalConverter.GetZoom;
  VConverter := VLocalConverter.GetGeoConverter;
  if VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoomCurr, True) then begin
    VMouseLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);
    if VMapType.GeoConvert.CheckLonLatPos(VMouseLonLat) then begin
      VTile := VMapType.GeoConvert.LonLat2TilePos(VMouseLonLat, VZoomCurr);
      s:=VMapType.GetTileShowName(VTile, VZoomCurr);
      if (MessageBox(handle,pchar(SAS_MSG_youasure+' '+s+'?'),pchar(SAS_MSG_coution),36)=IDYES) then begin
        VMapType.DeleteTile(VTile, VZoomCurr);
        OnMapTileUpdate(VMapType, VZoomCurr, VTile);
      end;
    end;
  end;
end;

procedure TfrmMain.NSRCinetClick(Sender: TObject);
begin
  FConfig.DownloadUIConfig.UseDownload := TTileSource(TTBXItem(Sender).Tag);
end;

procedure TfrmMain.N16Click(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.TBREGIONClick(Sender: TObject);
begin
 TBRectSave.ImageIndex:=13;
 TBRectSave.Checked:=true;
 setalloperationfalse(ao_select_poly);
end;

procedure TfrmMain.TBRECTClick(Sender: TObject);
begin
 TBRectSave.ImageIndex:=10;
 TBRectSave.Checked:=true;
 setalloperationfalse(ao_select_rect);
end;

procedure TfrmMain.TBRectSaveClick(Sender: TObject);
begin
  case TBRectSave.ImageIndex of
   10: begin
         setalloperationfalse(ao_select_rect);
       end;
   13: begin
         setalloperationfalse(ao_select_poly);
       end;
   12: begin
         TBCOORDClick(sender);
       end;
   20: begin
         TBScreenSelectClick(sender);
       end;
  end;
end;

procedure TfrmMain.TBPreviousClick(Sender: TObject);
var
  VZoom: Byte;
  VPolygon: TArrayOfDoublePoint;
begin
  VZoom := GState.LastSelectionInfo.Zoom;
  VPolygon := Copy(GState.LastSelectionInfo.Polygon);
  if length(VPolygon)>0 then begin
    frmRegionProcess.Show_(VZoom, VPolygon);
  end else begin
    showmessage(SAS_MSG_NeedHL);
  end;
end;

//карта заполнения в основном окне
procedure TfrmMain.NFillMapClick(Sender: TObject);
var
  VZoom: Integer;
begin
  VZoom := -1;
  if FConfig.LayersConfig.FillingMapLayerConfig.Visible then begin
    VZoom := FConfig.LayersConfig.FillingMapLayerConfig.SourceZoom;
  end;
  if VZoom > -1 then begin
    TBXToolPalette1.SelectedCell:=Point((VZoom + 1) mod 5,(VZoom + 1) div 5);
  end else begin
    TBXToolPalette1.SelectedCell:=Point(0,0);
  end;
end;

procedure TfrmMain.TBXToolPalette1CellClick(Sender: TTBXCustomToolPalette; var ACol, ARow: Integer; var AllowChange: Boolean);
var
  Vzoom_mapzap: integer;
begin
  Vzoom_mapzap:=((5*ARow)+ACol)-1;
  if Vzoom_mapzap < 0 then begin
    FConfig.LayersConfig.FillingMapLayerConfig.Visible := False;
  end else begin
    FConfig.LayersConfig.FillingMapLayerConfig.LockWrite;
    try
      FConfig.LayersConfig.FillingMapLayerConfig.Visible := True;
      FConfig.LayersConfig.FillingMapLayerConfig.SourceZoom := Vzoom_mapzap;
    finally
      FConfig.LayersConfig.FillingMapLayerConfig.UnlockWrite;
    end;
  end;
end;
procedure TfrmMain.TBXToolPalette2CellClick(Sender: TTBXCustomToolPalette;
  var ACol, ARow: Integer; var AllowChange: Boolean);
var
  VZoom: Byte;
  VZoomCurr: Byte;
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
begin
  AllowChange:=false;
  VZoom := ((5*ARow)+ACol);
  if VZoom>0 then begin
    VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
    VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
    VZoomCurr := VLocalConverter.GetZoom;
    VConverter := VLocalConverter.GetGeoConverter;
    VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoomCurr, True);
    VMouseLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);
    topos(VMouseLonLat,VZoom-1,true);
  end;
end;

//X-карта заполнения в основном окне

procedure TfrmMain.TBCalcRasClick(Sender: TObject);
begin
  setalloperationfalse(ao_calc_line);
end;

procedure TfrmMain.N29Click(Sender: TObject);
begin
  OpenUrlInBrowser('http://sasgis.ru/wikisasiya/');
end;

procedure TfrmMain.N000Click(Sender: TObject);
var
  VTag: Integer;
begin
  VTag := TMenuItem(Sender).Tag;
  if VTag = 0 then begin
    FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.Visible := False;
  end else begin
    FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.LockWrite;
    try
      FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.Visible := True;
      if VTag = 99 then begin
        FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.UseRelativeZoom := True;
        FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.Zoom := 0;
      end else begin
        FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.UseRelativeZoom := False;
        FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.Zoom := VTag - 1;
      end;
    finally
      FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.UnlockWrite;
    end;
  end;
end;

procedure TfrmMain.NShowGranClick(Sender: TObject);
var
  i:integer;
  VZoom: Byte;
  VGridVisible: Boolean;
  VRelativeZoom: Boolean;
  VGridZoom: Byte;
  VZoomCurr: Byte;
begin
  FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.LockRead;
  try
    VGridVisible := FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.Visible;
    VRelativeZoom := FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.UseRelativeZoom;
    VGridZoom := FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.Zoom;
  finally
    FConfig.LayersConfig.MapLayerGridsConfig.TileGrid.UnlockRead;
  end;

  if not VGridVisible then begin
    NShowGran.Items[0].Checked:=true;
  end else begin
    if VRelativeZoom then begin
      NShowGran.Items[1].Checked:=true;
    end;
  end;
  VZoomCurr := FConfig.ViewPortState.GetCurrentZoom;
  NShowGran.Items[1].Caption:=SAS_STR_activescale+' (z'+inttostr(VZoomCurr + 1)+')';
  for i:=2 to 7 do begin
    VZoom := VZoomCurr + i - 2;
    if VZoom < 24 then begin
      NShowGran.Items[i].Caption:=SAS_STR_for+' z'+inttostr(VZoom+1);
      NShowGran.Items[i].Visible:=true;
      NShowGran.Items[i].Tag:=VZoom+1;
      if VGridVisible and not VRelativeZoom and (VZoom = VGridZoom) then begin
        NShowGran.Items[i].Checked:=true
      end else begin
        NShowGran.Items[i].Checked:=false;
      end;
    end else begin
      NShowGran.Items[i].Visible:=false;
    end;
  end;
end;

procedure TfrmMain.TBItem2Click(Sender: TObject);
begin
 close;
end;

procedure TfrmMain.TBGPSconnClick(Sender: TObject);
begin
  GState.GPSConfig.GPSEnabled := TTBXitem(sender).Checked;
end;

procedure TfrmMain.TBGPSPathClick(Sender: TObject);
begin
  FConfig.LayersConfig.GPSTrackConfig.Visible := TTBXitem(sender).Checked;
end;

procedure TfrmMain.TBGPSToPointClick(Sender: TObject);
begin
  FConfig.GPSBehaviour.MapMove := TTBXitem(sender).Checked;
end;

procedure TfrmMain.TBHideMarksClick(Sender: TObject);
begin
  FConfig.LayersConfig.MarksShowConfig.IsUseMarks := not(TBHideMarks.Checked);
end;

procedure TfrmMain.TBCOORDClick(Sender: TObject);
var
  Poly: TArrayOfDoublePoint;
  VSelLonLat: TfrmLonLatRectEdit;
  VLonLatRect: TDoubleRect;
begin
  TBRectSave.ImageIndex:=12;
  VSelLonLat:= TfrmLonLatRectEdit.Create(Self);
  Try
    Poly := GState.LastSelectionInfo.Polygon;
    GetMinMax(VLonLatRect, Poly);
    if VSelLonLat.Execute(VLonLatRect) Then Begin
      Poly := PolygonFromRect(VLonLatRect);
      frmRegionProcess.Show_(FConfig.ViewPortState.GetCurrentZoom, Poly);
      Poly := nil;
    End;
  Finally
    VSelLonLat.Free;
  End;
  TBmoveClick(Sender);
end;

procedure TfrmMain.ShowstatusClick(Sender: TObject);
begin
  FConfig.LayersConfig.StatBar.Visible := TTBXItem(Sender).Checked;
end;

procedure TfrmMain.ShowMiniMapClick(Sender: TObject);
begin
  GState.MainFormConfig.LayersConfig.MiniMapLayerConfig.Visible := TTBXItem(Sender).Checked;
end;

procedure TfrmMain.ShowLineClick(Sender: TObject);
begin
  FConfig.LayersConfig.ScaleLineConfig.Visible := TTBXItem(Sender).Checked;
end;

procedure TfrmMain.N32Click(Sender: TObject);
begin
  FConfig.LayersConfig.CenterScaleConfig.Visible := TTBXItem(Sender).Checked;
end;

procedure TfrmMain.TBItem5Click(Sender: TObject);
var
  VAllPoints: TArrayOfDoublePoint;
begin
  VAllPoints := GState.GPSRecorder.GetAllPoints;
  if length(VAllPoints)>1 then begin
    if FMarkDBGUI.SaveLineModal(nil, VAllPoints, '') then begin
      setalloperationfalse(ao_movemap);
      FLayerMapMarks.Redraw;
    end;
  end else begin
    ShowMessage(SAS_ERR_Nopoints);
  end;
end;

procedure TfrmMain.mapResize(Sender: TObject);
begin
  if (not ProgramClose)and(not ProgramStart)then begin
    FConfig.ViewPortState.ChangeViewSize(Point(map.Width, map.Height));
  end;
end;

procedure TfrmMain.TBLoadSelFromFileClick(Sender: TObject);
begin
  if (OpenDialog1.Execute) then begin
    frmRegionProcess.LoadSelFromFile(OpenDialog1.FileName);
  end
end;

procedure TfrmMain.NinvertcolorClick(Sender: TObject);
begin
 GState.BitmapPostProcessingConfig.InvertColor:=Ninvertcolor.Checked;
end;

procedure TfrmMain.mapDblClick(Sender: TObject);
var
  r: TPoint;
  i: Integer;
  VLayer: TCustomLayer;
begin
  r:=map.ScreenToClient(Mouse.CursorPos);
  for i := 0 to map.Layers.Count - 1 do begin
    VLayer := map.Layers[i];
    if VLayer.MouseEvents then begin
      if VLayer.HitTest(r.X, r.Y) then begin
        Exit;
      end;
    end;
  end;
  FMapMoving:=false;
  if (FCurrentOper=ao_movemap) then begin
    FConfig.ViewPortState.ChangeMapPixelToVisualPoint(r);
  end;
end;

procedure TfrmMain.TBAdd_PointClick(Sender: TObject);
begin
 setalloperationfalse(ao_add_point);
end;

procedure TfrmMain.TBAdd_LineClick(Sender: TObject);
begin
 setalloperationfalse(ao_add_Line);
end;

procedure TfrmMain.TBAdd_PolyClick(Sender: TObject);
begin
 setalloperationfalse(ao_add_poly);
end;

procedure TfrmMain.NMarkEditClick(Sender: TObject);
var
  VMark: IMarkFull;
begin
  FLayerMapMarks.MouseOnMyReg(FmoveTrue, VMark);
  if VMark <> nil then begin
    if VMark.IsPoint then begin
      VMark := FMarkDBGUI.EditMarkModal(VMark);
      if VMark <> nil then begin
        GState.MarksDB.MarksDb.WriteMark(VMark);
        FLayerMapMarks.Redraw;
      end;
    end else if VMark.IsPoly then begin
      setalloperationfalse(ao_edit_poly);
      FEditMark := VMark;
      FLineOnMapEdit.SetPoints(VMark.Points);
    end else if VMark.IsLine then begin
      setalloperationfalse(ao_edit_line);
      FEditMark := VMark;
      FLineOnMapEdit.SetPoints(VMark.Points);
    end;
  end;
end;

procedure TfrmMain.NMarkExportClick(Sender: TObject);
var
  KMLExport:TExportMarks2KML;
  VMark: IMarkFull;
begin
  FLayerMapMarks.MouseOnMyReg(FmoveTrue, VMark);
  if VMark <> nil then begin
    KMLExport:=TExportMarks2KML.Create(false);
    try
      frmMarksExplorer.ExportDialog.FileName := VMark.Name;
      if (frmMarksExplorer.ExportDialog.Execute)and(frmMarksExplorer.ExportDialog.FileName<>'') then begin
        KMLExport.ExportMarkToKML(VMark,frmMarksExplorer.ExportDialog.FileName);
      end;
    finally
      KMLExport.free;
    end;
  end;
end;

procedure TfrmMain.NMarkDelClick(Sender: TObject);
var
  VMark: IMarkFull;
begin
  FLayerMapMarks.MouseOnMyReg(FmoveTrue, VMark);
  if VMark <> nil then begin
    if FMarkDBGUI.DeleteMarkModal(VMark as IMarkID, Handle) then
      FLayerMapMarks.Redraw;
  end;
end;

procedure TfrmMain.NMarkOperClick(Sender: TObject);
var
  VMark: IMarkFull;
begin
  FLayerMapMarks.MouseOnMyReg(FmoveTrue, VMark);
  if VMark <> nil then begin
    FMarkDBGUI.OperationMark(VMark, FConfig.ViewPortState.GetCurrentZoom);
  end;
end;

procedure TfrmMain.N13Click(Sender: TObject);
var
  VZoomCurr: Byte;
  VMapType: TMapType;
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VMouseMapPoint: TDoublePoint;
  VMouseLonLat: TDoublePoint;
  VTile: TPoint;
begin
  if TMenuItem(sender).Tag<>0 then begin
    VMapType := TMapType(TMenuItem(sender).Tag);
  end else begin
    VMapType := FConfig.MainMapsConfig.GetSelectedMapType.MapType;
  end;
  if VMapType.UseDwn then begin
    VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
    VConverter := VLocalConverter.GetGeoConverter;
    VZoomCurr := VLocalConverter.GetZoom;
    VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
    VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoomCurr, True);
    VMouseLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);
    VMapType.GeoConvert.CheckLonLatPos(VMouseLonLat);
    VTile := VMapType.GeoConvert.LonLat2TilePos(VMouseLonLat, VZoomCurr);
    CopyStringToClipboard(VMapType.GetLink(VTile, VZoomCurr));
  end;
end;

procedure TfrmMain.DigitalGlobe1Click(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  frmDGAvailablePic.setup(VLocalConverter, FMouseDownPoint);
end;

procedure TfrmMain.mapMouseLeave(Sender: TObject);
begin
 if (FHintWindow<>nil) then
  begin
   FHintWindow.ReleaseHandle;
   FreeAndNil(FHintWindow);
  end;
end;

procedure TfrmMain.GPSReceiverDisconnect(Sender: TObject);
begin
  if FConfig.GPSBehaviour.SensorsAutoShow then TBXSensorsBar.Visible:=false;
  if TBXSignalStrengthBar.Visible then UpdateGPSSatellites;
  tbitmGPSConnect.Enabled := True;
  TBGPSconn.Enabled := True;
  tbitmGPSConnect.Checked:=false;
  TBGPSconn.Checked:=false;
end;

procedure TfrmMain.GPSReceiverReceive(Sender: TObject);
begin
  FIsGPSPosChanged := True;
end;

procedure TfrmMain.GPSReceiverStateChange(Sender: TObject);
begin
  tbitmGPSConnect.Enabled := False;
  TBGPSconn.Enabled := False;
end;

procedure TfrmMain.GPSReceiverConnect(Sender: TObject);
begin
  tbitmGPSConnect.Enabled := True;
  TBGPSconn.Enabled := True;
  tbitmGPSConnect.Checked:=True;
  TBGPSconn.Checked:=True;
  if FConfig.GPSBehaviour.SensorsAutoShow then TBXSensorsBar.Visible:=true;
end;

procedure TfrmMain.GPSReceiverConnectError(Sender: TObject);
begin
  ShowMessage(SAS_ERR_PortOpen);
end;

procedure TfrmMain.GPSReceiverTimeout(Sender: TObject);
begin
  tbitmGPSConnect.Enabled := True;
  TBGPSconn.Enabled := True;
  ShowMessage(SAS_ERR_Communication);
end;

procedure TfrmMain.NMapParamsClick(Sender: TObject);
var
  VMapType: TMapType;
begin
  if TTBXItem(sender).Tag=0 then begin
    VMapType := FConfig.MainMapsConfig.GetSelectedMapType.MapType;
  end else begin
    VMapType := TMapType(TTBXItem(sender).Tag);
  end;
  if frmMapTypeEdit.EditMapModadl(VMapType) then begin
    CreateMapUI;
    OnMapUpdate(VMapType);
  end;
end;

procedure TfrmMain.mapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  VSelectionRect: TDoubleRect;
  VClickLonLat: TDoublePoint;
  VClickRect: TRect;
  VClickLonLatRect: TDoubleRect;
  VPoly:  TArrayOfDoublePoint;
  Vlastpoint: Integer;
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMouseMapPoint: TDoublePoint;
  VClickMapRect: TDoubleRect;
  VIsClickInMap: Boolean;
  VMark: IMarkFull;
begin
  if (FHintWindow<>nil) then begin
    FHintWindow.ReleaseHandle;
    FreeAndNil(FHintWindow);
  end;
  if (Layer <> nil) then begin
    exit;
  end;
  if (FMapZoomAnimtion)or
     (ssDouble in Shift)or
     (button=mbMiddle)or
     (ssRight in Shift)and(ssLeft in Shift)or
     (HiWord(GetKeyState(VK_DELETE))<>0)or
     (HiWord(GetKeyState(VK_INSERT))<>0)or
     (HiWord(GetKeyState(VK_F6))<>0) then begin
    exit;
  end;
  Screen.ActiveForm.SetFocusedControl(map);
  FMouseDownPoint := Point(x, y);
  FmoveTrue := FMouseDownPoint;
  FMouseUpPoint := FMouseDownPoint;
  MouseCursorPos := FMouseDownPoint;
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
  VIsClickInMap := VConverter.CheckPixelPosFloat(VMouseMapPoint, VZoom, False);
  VClickLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoom);

  if (Button=mbLeft)and(FCurrentOper<>ao_movemap) then begin
    if (FCurrentOper in [ao_select_poly, ao_calc_line,ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly])then begin
      movepoint:=true;
      Vlastpoint := -1;
      if VIsClickInMap then begin
        VClickRect.Left := FMouseDownPoint.X - 5;
        VClickRect.Top := FMouseDownPoint.Y - 5;
        VClickRect.Right := FMouseDownPoint.X + 5;
        VClickRect.Bottom := FMouseDownPoint.Y + 5;
        VClickMapRect := VLocalConverter.LocalRect2MapRectFloat(VClickRect);
        VConverter.CheckPixelRectFloat(VClickMapRect, VZoom);
        VClickLonLatRect := VConverter.PixelRectFloat2LonLatRect(VClickMapRect, VZoom);
        Vlastpoint := FLineOnMapEdit.GetPointIndexInLonLatRect(VClickLonLatRect);
      end;
      if Vlastpoint < 0 then begin
        FLineOnMapEdit.InsertPoint(VClickLonLat);
      end else begin
        FLineOnMapEdit.SetActiveIndex(Vlastpoint);
      end;
    end;
    if (FCurrentOper=ao_select_rect)then begin
      if Frect_dwn then begin
        FSelectionRect.BottomRight:= VClickLonLat;
        Frect_p2:=true;
      end else begin
        FSelectionRect.TopLeft:= VClickLonLat;
        FSelectionRect.BottomRight:=FSelectionRect.TopLeft
      end;
      Frect_dwn:=not(Frect_dwn);
      VSelectionRect := FSelectionRect;
      PrepareSelectionRect(Shift, VSelectionRect);
      FSelectionRectLayer.DrawSelectionRect(VSelectionRect);
      if (Frect_p2) then begin
        VPoly := PolygonFromRect(VSelectionRect);
        frmRegionProcess.Show_(VZoom, VPoly);
        FSelectionRectLayer.DrawNothing;
        VPoly := nil;
        Frect_p2:=false;
      end;
    end;
    if (FCurrentOper=ao_add_point) then begin
      if(FMarkDBGUI.AddNewPointModal(VClickLonLat)) then begin
        setalloperationfalse(ao_movemap);
        FLayerMapMarks.Redraw;
      end;
    end;
    exit;
  end;
  if FMapMoving then exit;
  
  if (VIsClickInMap)and (Button=mbright)and(FCurrentOper=ao_movemap) then begin
    FMouseUpPoint:=FMouseDownPoint;
    VMark := nil;
    if FConfig.LayersConfig.MarksShowConfig.IsUseMarks then begin
      FLayerMapMarks.MouseOnMyReg(FMouseDownPoint, VMark);
    end;
    NMarkEdit.Visible := VMark <> nil;
    NMarkExport.Visible := VMark <> nil;
    NMarkDel.Visible := VMark <> nil;
    NMarkSep.Visible := VMark <> nil;
    NMarkOper.Visible := VMark <> nil;
    NMarkNav.Visible := VMark <> nil;
    if (VMark <> nil) and (not VMark.IsPoint) then begin
      NMarksCalcsSq.Visible := VMark.IsPoly;
      NMarksCalcsPer.Visible := VMark.IsPoly;
      NMarksCalcsLen.Visible:= VMark.IsLine;
      NMarksCalcs.Visible := true;
    end else begin
      NMarksCalcs.Visible := false;
    end;
    if (VMark <> nil) and (FConfig.NavToPoint.IsActive) and (FConfig.NavToPoint.Id = VMark.Id) then begin
      NMarkNav.Checked:=true
    end else begin
      NMarkNav.Checked:=false;
    end;
    map.PopupMenu:=MainPopupMenu;
  end else begin
    FMapMoving:=true;
    map.PopupMenu:=nil;
  end;
end;

procedure TfrmMain.mapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  VPWL:TResObj;
  stw:String;
  VZoomCurr: Byte;
  VSelectionRect: TDoubleRect;
  VMapMoving: Boolean;
  VMapType: TMapType;
  VValidPoint: Boolean;
  VConverter: ICoordConverter;
  VTile: TPoint;
  VLonLat: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
  VMouseMapPoint: TDoublePoint;
  VMouseMoveDelta: TPoint;
  VMark: IMarkFull;
  VMarkS: Double;
begin
  if (Layer <> nil) then begin
    exit;
  end;
  if (ssDouble in Shift)or
     ((Button=mbRight)and(ssLeft in Shift))or
     ((Button=mbLeft)and(ssRight in Shift)) then begin
    exit;
  end;
  VMapMoving := FMapMoving;
  FMapMoving:=false;
  FMouseUpPoint := Point(x, y);
  VMapType := FConfig.MainMapsConfig.GetSelectedMapType.MapType;
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoomCurr := VLocalConverter.GetZoom;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseUpPoint);
  VValidPoint := VConverter.CheckPixelPosFloat(VMouseMapPoint, VZoomCurr, False);
  VLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);

  if VValidPoint then begin
    if VMapType.GeoConvert.CheckLonLatPos(VLonLat) then begin
      VTile := VMapType.GeoConvert.LonLat2TilePos(VLonLat, VZoomCurr);
      if HiWord(GetKeyState(VK_DELETE))<>0 then begin
        VMapType.DeleteTile(VTile, VZoomCurr);
        OnMapTileUpdate(VMapType, VZoomCurr, VTile);
      end;
      if HiWord(GetKeyState(VK_INSERT))<>0 then begin
        TTileDownloaderUIOneTile.Create(
          VTile,
          VZoomCurr,
          VMapType,
          Self.OnMapTileUpdate,
          FShowErrorLayer
        );
      end;
    end;
    if HiWord(GetKeyState(VK_F6))<>0 then begin
      frmDGAvailablePic.setup(VLocalConverter, FMouseUpPoint);
    end;
  end;

  movepoint:=false;

  if (((FCurrentOper<>ao_movemap)and(Button=mbLeft))or
     ((FCurrentOper=ao_movemap)and(Button=mbRight))) then exit;
  if (FMapZoomAnimtion) then exit;

  map.Enabled:=false;
  map.Enabled:=true;
  if button=mbMiddle then begin
    FWinPosition.LockWrite;
    try
      if FWinPosition.GetIsFullScreen then begin
        FWinPosition.SetNoFullScreen;
      end else begin
        FWinPosition.SetFullScreen;
      end;
    finally
      FWinPosition.UnlockWrite;
    end;
    exit;
  end;
  VMouseMoveDelta := Point(FMouseDownPoint.x-FMouseUpPoint.X, FMouseDownPoint.y-FMouseUpPoint.y);

  if VMapMoving then begin
    FConfig.ViewPortState.ChangeMapPixelByDelta(DoublePoint(VMouseMoveDelta));
  end;

  if (VMouseMoveDelta.X = 0)and(VMouseMoveDelta.Y = 0) then begin
    if FCurrentOper=ao_select_rect then begin
      VSelectionRect := FSelectionRect;
      PrepareSelectionRect(Shift, VSelectionRect);
      FSelectionRectLayer.DrawSelectionRect(VSelectionRect);
    end;
    if (FCurrentOper=ao_movemap)and(button=mbLeft) then begin
      VPWL.S:=0;
      VPWL.find:=false;
      if (FWikiLayer.Visible) then
        FWikiLayer.MouseOnReg(VPWL, Point(x,y));
      VMark := nil;
      if (FConfig.LayersConfig.MarksShowConfig.IsUseMarks) then
        FLayerMapMarks.MouseOnMyReg(Point(x,y), VMark, VMarkS);
      if VMark <> nil then begin
        if (not VPWL.find) or (not VMark.IsPoly) or (VPWL.S >= VMarkS) then begin
          VPWL.find := True;
          VPWL.name := VMark.Name;
          VPWL.descr := VMark.Desc;
          VPWL.S := VMarkS;
        end;
      end;
      if VPWL.find  then begin
        if VPWL.descr <> '' then begin
          stw:='<HTML><BODY>';
          stw:=VPWL.descr;
          stw:=stw+'</BODY></HTML>';
          frmIntrnalBrowser.showmessage(VPWL.name,stw);
        end;
      end;
    end;
  end;
end;

function HTML2Txt(OrigHTML: String): String;
var
  NoHTML: String;
function MidStr(const pString, pAbre, pFecha: String; pInclui: boolean): string;
var
  lIni, lFim : integer;
begin
  if (pInclui = False) then begin
    lIni := System.Pos(UpperCase(pAbre), UpperCase(pString)) + Length(pAbre);
    lFim := PosEx(UpperCase(pFecha),UpperCase(pString),lIni)+1;
  end else begin
    lIni := System.Pos(UpperCase(pAbre), UpperCase(pString));
    lFim := PosEx(UpperCase(pFecha),UpperCase(pString),lIni + Length(pAbre))+1;
  end;
  result := Copy(pString, lIni, lFim - lIni);
end;
function mid(str:string; pos:integer):string;
begin
 result:=copy(str,pos, length(str)-pos+1);
end;
begin
  if System.Pos('<body', LowerCase(OrigHTML)) > 0 Then begin
    OrigHTML := Mid(OrigHTML, System.Pos('<body', LowerCase(OrigHTML)));
    OrigHTML := Mid(OrigHTML, System.Pos('>', OrigHTML) + 1);
    if System.Pos('</body>', LowerCase(OrigHTML)) > 0 Then
      OrigHTML := Copy(OrigHTML,1 , System.Pos('</body>', LowerCase(OrigHTML)) - 1);
  end;
  OrigHTML := StringReplace(OrigHTML, Chr(13), '', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, Chr(10), '', [rfReplaceAll]);
  while System.Pos('  ', OrigHTML) > 0 do
    OrigHTML := StringReplace(OrigHTML, '  ', ' ', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '<br>', #13#10, [rfReplaceAll, rfIgnoreCase]);
  OrigHTML := StringReplace(OrigHTML, '</div>', #13#10#13#10, [rfReplaceAll, rfIgnoreCase]);
  while System.Pos('<p', OrigHTML) > 0 do begin
    NoHTML   := MidStr(OrigHTML, '<p', '>', True);
    OrigHTML := StringReplace(OrigHTML, NoHTML, (#13#10#13#10), [rfReplaceAll, rfIgnoreCase]);
  end;
  while System.Pos('<', OrigHTML) > 0 do begin
    NoHTML   := MidStr(OrigHTML, '<', '>', True);
    OrigHTML := StringReplace(OrigHTML, NoHTML, '', [rfReplaceAll, rfIgnoreCase]);
  end;
  OrigHTML := StringReplace(OrigHTML, '&#36;',     '$', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '#37;',      '%', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&#187;',    '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&aacute;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&atilde;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&ccedil;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&eacute;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&ecirc;',   '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&iacute;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&oacute;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&ocirc;',   '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&otilde;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Aacute;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Atilde;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Ccedil;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Eacute;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Ecirc;',   '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Iacute;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Oacute;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Ocirc;',   '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Otilde;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&amp;',     '&', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&quot;',    '"', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&lt;',      '<', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&gt;',      '>', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&nbsp;',    ' ', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&copy;',    '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&reg;',     '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&raquo;',   '»', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&laquo;',   '«', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Uacute;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&uacute;',  '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&uuml;',    '?', [rfReplaceAll]);
  result := OrigHTML;
end;


procedure TfrmMain.mapMouseMove(Sender: TObject; Shift: TShiftState; AX, AY: Integer; Layer: TCustomLayer);
var
  i,j:integer;
  nms:string;
  hintrect:TRect;
  CState: Integer;
  VZoomCurr: Byte;
  VSelectionRect: TDoubleRect;
  VConverter: ICoordConverter;
  VLonLat: TDoublePoint;
  VPWL: TResObj;
  VLocalConverter: ILocalCoordConverter;
  VMouseMapPoint: TDoublePoint;
  VMouseMoveDelta: TPoint;
  VLastMouseMove: TPoint;
  VMark: IMarkFull;
  VMarkS: Double;
begin
  if ProgramClose then begin
    exit;
  end;
  VLastMouseMove := FmoveTrue;
  FmoveTrue:=point(Ax,Ay);
  MouseCursorPos:=FmoveTrue;
  VMouseMoveDelta := Point(FMouseDownPoint.X-FmoveTrue.X, FMouseDownPoint.Y-FmoveTrue.Y);
  if (Layer <> nil) then begin
    exit;
  end;
  if (FMapZoomAnimtion)or(
    (ssDouble in Shift){or(HiWord(GetKeyState(VK_DELETE))<>0)or(HiWord(GetKeyState(VK_INSERT))<>0))
    or(HiWord(GetKeyState(VK_F6))<>0})
  then begin
    exit;
  end;
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoomCurr := VLocalConverter.GetZoom;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FmoveTrue);
  VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoomCurr, False);
  VLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoomCurr);
// CState:=ShowCursor(True);
// while CState < 0 do begin
//  CState:= ShowCursor(true);
// end;
// sleep(5);
  if (movepoint) then begin
    FLineOnMapEdit.MoveActivePoint(VLonLat);
    exit;
  end;
 if (FCurrentOper=ao_select_rect)and(Frect_dwn)and(not(ssRight in Shift)) then begin
   FSelectionRect.BottomRight:=VLonLat;
   VSelectionRect := FSelectionRect;
   PrepareSelectionRect(Shift,VSelectionRect);
   FSelectionRectLayer.DrawSelectionRect(VSelectionRect);
 end;
 if FWinPosition.GetIsFullScreen then begin
                       if FmoveTrue.y<10 then begin
                                     TBDock.Parent:=map;
                                     TBDock.Visible:=true;
                                    end
                               else begin
                                     TBDock.Visible:=false;
                                     TBDock.Parent:=Self;
                                    end;
                       if FmoveTrue.x<10 then begin
                                     TBDockLeft.Parent:=map;
                                     TBDockLeft.Visible:=true;
                                    end
                               else begin
                                     TBDockLeft.Visible:=false;
                                     TBDockLeft.Parent:=Self;
                                    end;
                       if FmoveTrue.y>Map.Height-10 then begin
                                     TBDockBottom.Parent:=map;
                                     TBDockBottom.Visible:=true;
                                    end
                               else begin
                                     TBDockBottom.Visible:=false;
                                     TBDockBottom.Parent:=Self;
                                    end;
                       if FmoveTrue.x>Map.Width-10 then begin
                                     TBDockRight.Parent:=map;
                                     TBDockRight.Visible:=true;
                                    end
                               else begin
                                     TBDockRight.Visible:=false;
                                     TBDockRight.Parent:=Self;
                                    end;
 end;
 if FMapZoomAnimtion then exit;
 if FMapMoving then begin
  map.BeginUpdate;
  try
    FConfig.ViewPortState.MoveTo(VMouseMoveDelta);
  finally
    map.EndUpdate;
    map.Changed;
  end;
 end;
 if not(FMapMoving) then begin
    FLayerStatBar.Redraw;
 end;

 if (not FShowActivHint) then begin
   if (FHintWindow<>nil) then begin
     FHintWindow.ReleaseHandle;
     FreeAndNil(FHintWindow);
    end;
 end;
 FShowActivHint:=false;
 if not(FMapMoving)and((FmoveTrue.x<>VLastMouseMove.X)or(FmoveTrue.y<>VLastMouseMove.y))and(FConfig.MainConfig.ShowHintOnMarks) then begin
    VPWL.S:=0;
    VPWL.find:=false;
    if (FWikiLayer.Visible) then
      FWikiLayer.MouseOnReg(VPWL,FmoveTrue);
    VMark := nil;
    if (FConfig.LayersConfig.MarksShowConfig.IsUseMarks) then
      FLayerMapMarks.MouseOnMyReg(FmoveTrue, VMark, VMarkS);
    if VMark <> nil then begin
      if (not VPWL.find) or (not VMark.IsPoly) or (VPWL.S >= VMarkS) then begin
        VPWL.find := True;
        VPWL.name := VMark.Name;
        VPWL.descr := VMark.Desc;
        VPWL.S := VMarkS;
      end;
    end;
   if (VPWL.find) then begin
     if FHintWindow<>nil then FHintWindow.ReleaseHandle;
     if (length(VPWL.name)>0) then begin
       if System.Pos('<',VPWL.name)>0 then nms:=HTML2Txt(VPWL.name)
                                     else nms:=VPWL.name;
     end;
     if (length(VPWL.descr)>0) then begin
       if length(nms)>0 then nms:=nms+#13#10;
       if System.Pos('<',VPWL.descr)>0 then nms:=nms+HTML2Txt(VPWL.descr)
                                      else nms:=nms+VPWL.descr;
     end;
     i:=1;
     j:=0;
     while (i<length(nms))and(i<>0) do begin
       inc(j);
       if (nms[i]=#13)or(nms[i]=#10) then j:=0;
       if (j>40)and(nms[i]=' ')and(length(nms)-i>5)then begin
         if i>500 then begin
           Insert('...',nms,i);
           Delete(nms,i+3,length(nms)-i+3);
           i:=0;
           continue;
         end;
         Delete(nms,i,1);
         Insert(#13#10,nms,i);
         j:=0;
        end;
       inc(i);
      end;
     if nms<>'' then begin
      if FHintWindow=nil then begin
        FHintWindow:=THintWindow.Create(Self);
        FHintWindow.Brush.Color:=clInfoBk;
        FHintWindow.Font.Charset:=RUSSIAN_CHARSET;
      end;
      hintrect:=FHintWindow.CalcHintRect(Screen.Width, nms, nil);
      FHintWindow.ActivateHint(Bounds(Mouse.CursorPos.x+13,Mouse.CursorPos.y-13,abs(hintrect.Right-hintrect.Left),abs(hintrect.Top-hintrect.Bottom)),nms);
      FHintWindow.Repaint;
     end;
     FShowActivHint:=true;
    end;
  end;
end;

procedure CreateLink(const PathObj,PathLink, Desc, Param: string);
var
  IObject: IUnknown;
  SLink: IShellLink;
  PFile: IPersistFile;
begin
  IObject := CreateComObject(CLSID_ShellLink);
  SLink := IObject as IShellLink;
  PFile := IObject as IPersistFile;
  with SLink do
  begin
    SetArguments(PChar(Param));
    SetDescription(PChar(Desc));
    SetPath(PChar(PathObj));
  end;
  PFile.Save(PWChar(WideString(PathLink)), FALSE);
end;

procedure TfrmMain.N35Click(Sender: TObject);
var
  VLonLat:TDoublePoint;
  param:string;
  VZoomCurr: Byte;
  VMapType: TMapType;
  VLocalConverter: ILocalCoordConverter;
begin
  if SaveLink.Execute then begin
    VMapType := FConfig.MainMapsConfig.GetSelectedMapType.MapType;
    VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
    VZoomCurr := VLocalConverter.GetZoom;
    VLonLat := VLocalConverter.GetCenterLonLat;
    param:=' '+VMapType.GUIDString+' '+IntToStr(VZoomCurr + 1)+' '+FloatToStr(VLonLat.x)+' '+FloatToStr(VLonLat.y);
    CreateLink(ParamStr(0), SaveLink.filename, '', param)
  end;
end;

procedure TfrmMain.TBItemDelTrackClick(Sender: TObject);
begin
  GState.GPSRecorder.LockWrite;
  try
    GState.GPSRecorder.ClearTrack;
    GState.GPSRecorder.ResetMaxSpeed;
  finally
    GState.GPSRecorder.UnlockWrite;
  end;
  FLayerMapGPS.Redraw;
end;

procedure TfrmMain.NGShScale01Click(Sender: TObject);
var
  VTag: Integer;
begin
  VTag := TTBXItem(sender).Tag;
  FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.LockWrite;
  try
    if VTag = 0 then begin
      FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Visible := False;
    end else begin
      FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Visible := True;
      FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.Scale := VTag;
    end;
  finally
    FConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.UnlockWrite;
  end;
end;

procedure TfrmMain.TBEditPathDelClick(Sender: TObject);
begin
 case FCurrentOper of
  ao_select_poly,
  ao_calc_line,
  ao_add_poly,
  ao_add_line,
  ao_edit_line,
  ao_edit_poly:
    FLineOnMapEdit.DeleteActivePoint;
 end;
end;

procedure TfrmMain.TBEditPathLabelClick(Sender: TObject);
begin
  if FCurrentOper = ao_calc_line then begin
    FConfig.LayersConfig.CalcLineLayerConfig.LockWrite;
    try
      FConfig.LayersConfig.CalcLineLayerConfig.LenShow :=
        not FConfig.LayersConfig.CalcLineLayerConfig.LenShow;
    finally
      FConfig.LayersConfig.CalcLineLayerConfig.UnlockWrite;
    end;
  end;
end;

procedure TfrmMain.TBEditPathSaveClick(Sender: TObject);
var result:boolean;
begin
  result := false;
  case FCurrentOper of
    ao_add_Poly: begin
      result:=FMarkDBGUI.SavePolyModal(nil, FLineOnMapEdit.GetPoints);
    end;
    ao_edit_poly: begin
      result:=FMarkDBGUI.SavePolyModal(FEditMark, FLineOnMapEdit.GetPoints);
    end;
    ao_add_Line: begin
      result:=FMarkDBGUI.SaveLineModal(nil, FLineOnMapEdit.GetPoints, FMarshrutComment);
    end;
    ao_edit_line: begin
      result:=FMarkDBGUI.SaveLineModal(FEditMark, FLineOnMapEdit.GetPoints, '');
    end;
  end;
  if result then begin
    setalloperationfalse(ao_movemap);
    FLayerMapMarks.Redraw;
  end;
end;

procedure TfrmMain.TBEditPathClose(Sender: TObject);
begin
 setalloperationfalse(ao_movemap);
end;

procedure TfrmMain.TBItem6Click(Sender: TObject);
begin
  frmMarksExplorer.EditMarks(FMarkDBGUI);
end;

procedure TfrmMain.LayerMapMarksRedraw;
begin
  FLayerMapMarks.Redraw;
end;

procedure TfrmMain.NMarkNavClick(Sender: TObject);
var
  LL:TDoublePoint;
  VMark: IMarkFull;
begin
  FLayerMapMarks.MouseOnMyReg(FmoveTrue, VMark);
  if VMark <> nil then begin
    if (not NMarkNav.Checked) then begin
      LL := VMark.GetGoToLonLat;
      FConfig.NavToPoint.StartNavToMark(VMark.Id, ll);
    end else begin
      FConfig.NavToPoint.StopNav;
    end;
  end;
end;

procedure TfrmMain.AdjustFont(Item: TTBCustomItem;
  Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
begin
 if TTBXItem(Item).Checked then TTBXItem(Item).FontSettings.Bold:=tsTrue
                           else TTBXItem(Item).FontSettings.Bold:=tsDefault;
end;

procedure TfrmMain.NParamsClick(Sender: TObject);
var
  i:Integer;
  VMapType: TMapType;
  VLayerIsActive: Boolean;
  VActiveLayersList: IMapTypeList;
begin
  NLayerParams.Visible:=false;
  VActiveLayersList := FConfig.MainMapsConfig.GetLayers.GetSelectedMapsList;
  For i:=0 to GState.MapType.Count-1 do begin
    VMapType := GState.MapType[i];
    if (VMapType.asLayer) then begin
      VLayerIsActive := VActiveLayersList.GetMapTypeByGUID(VMapType.GUID) <> nil;
      TTBXItem(FNLayerParamsItemList.GetByGUID(VMapType.GUID)).Visible := VLayerIsActive;
      if VLayerIsActive then begin
        NLayerParams.Visible:=true;
      end
    end;
  end;
end;

procedure TfrmMain.TBfillMapAsMainClick(Sender: TObject);
var
  VSender: TComponent;
  VAtiveMap: IActiveMapSingle;
  VMapType: IMapType;
  VGUID: TGUID;
begin
  if Sender is TComponent then begin
    VSender := TComponent(Sender);
    VAtiveMap := IActiveMapSingle(VSender.Tag);
    if VAtiveMap <> nil then begin
      VMapType := VAtiveMap.GetMapType;
      VGUID := CGUID_Zero;
      if VMapType <> nil then begin
        VGUID := VMapType.GUID;
      end;
      FConfig.LayersConfig.FillingMapLayerConfig.GetSourceMap.SelectMainByGUID(VGUID);
    end;
  end;
end;

procedure TfrmMain.NMarksCalcsLenClick(Sender: TObject);
var
  VMark: IMarkFull;
begin
  FLayerMapMarks.MouseOnMyReg(FmoveTrue, VMark);
  if VMark <> nil then begin
    FMarkDBGUI.ShowMarkLength(VMark, FConfig.ViewPortState.GetCurrentCoordConverter, Self.Handle);
  end;
end;

procedure TfrmMain.NMarksCalcsSqClick(Sender: TObject);
var
  VMark: IMarkFull;
begin
  FLayerMapMarks.MouseOnMyReg(FmoveTrue, VMark);
  if VMark <> nil then begin
    FMarkDBGUI.ShowMarkSq(VMark, FConfig.ViewPortState.GetCurrentCoordConverter, Self.Handle);
  end;
end;

procedure TfrmMain.NMarksCalcsPerClick(Sender: TObject);
var
  VMark: IMarkFull;
begin
  FLayerMapMarks.MouseOnMyReg(FmoveTrue, VMark);
  if VMark <> nil then begin
    FMarkDBGUI.ShowMarkLength(VMark, FConfig.ViewPortState.GetCurrentCoordConverter, Self.Handle);
  end;
end;

procedure TfrmMain.TBEditPathOkClick(Sender: TObject);
begin
  case FCurrentOper of
   ao_select_poly: begin
         frmRegionProcess.Show_(FConfig.ViewPortState.GetCurrentZoom, FLineOnMapEdit.GetPoints);
         setalloperationfalse(ao_movemap);
        end;
  end;
end;

procedure TfrmMain.NMapInfoClick(Sender: TObject);
var
  VMapType: TMapType;
begin
  if TMenuItem(sender).Tag<>0 then begin
    VMapType := TMapType(TMenuItem(sender).Tag);
  end else begin
    VMapType := FConfig.MainMapsConfig.GetSelectedMapType.MapType;
  end;
  if VMapType.MapInfo <> '' then begin
    frmIntrnalBrowser.showmessage(VMapType.zmpfilename,VMapType.MapInfo);
  end;
end;

procedure TfrmMain.NanimateClick(Sender: TObject);
begin
  FConfig.MainConfig.AnimateZoom := Nanimate.Checked;
end;

procedure TfrmMain.SaveWindowConfigToIni(AProvider: IConfigDataWriteProvider);
var
  lock_tb_b:boolean;
  VProvider: IConfigDataWriteProvider;
begin
  VProvider := AProvider.GetOrCreateSubItem('MainForm');
  FWinPosition.WriteConfig(VProvider);

  VProvider := AProvider.GetOrCreateSubItem('PANEL');
  lock_tb_b:=FConfig.ToolbarsLock.GetIsLock;
  SetToolbarsLock(False);
  TBConfigProviderSavePositions(Self, VProvider);
  SetToolbarsLock(lock_tb_b);
end;

procedure TfrmMain.TBXSensorsBarVisibleChanged(Sender: TObject);
begin
  NSensors.Checked := TTBXToolWindow(sender).Visible;
end;

procedure TfrmMain.NSensorsClick(Sender: TObject);
begin
  TBXSensorsBar.Visible := TTBXItem(sender).Checked;
end;

procedure TfrmMain.TBXItem5Click(Sender: TObject);
var
  VPosition: IGPSPosition;
begin
  VPosition := GState.GPSRecorder.CurrentPosition;
  if VPosition.IsFix > 0 then begin
    if FMarkDBGUI.AddNewPointModal(VPosition.Position) then begin
      setalloperationfalse(ao_movemap);
      FLayerMapMarks.Redraw;
    end;
  end;
end;

procedure TfrmMain.tbitmPositionByGSMClick(Sender: TObject);
var
  PosFromGSM: TPosFromGSM;
begin
 PosFromGSM:=TPosFromGSM.Create(Self.topos);
 try
   PosFromGSM.GetPos(FConfig.ViewPortState.GetCurrentZoom);
 finally
   PosFromGSM.Free;
 end;
end;

procedure TfrmMain.tbitmShowDebugInfoClick(Sender: TObject);
begin
  if frmDebugInfo <> nil then begin
    frmDebugInfo.ShowStatistic(FLayersList);
  end;
end;

procedure TfrmMain.TBXItem6Click(Sender: TObject);
var
  VLog: TLogForTaskThread;
  VSimpleLog: ILogSimple;
  VThreadLog:ILogForTaskThread;
  VThread: TThreadDownloadTiles;
  VFileName: string;
  VImportConfig: IImportConfig;
begin
  if (OpenSessionDialog.Execute) then begin
    VFileName := OpenSessionDialog.FileName;
    if FileExists(VFileName) then begin
      if ExtractFileExt(VFileName)='.sls' then begin
        VLog := TLogForTaskThread.Create(5000, 0);
        VSimpleLog := VLog;
        VThreadLog := VLog;
        VThread := TThreadDownloadTiles.Create(VSimpleLog, VFileName, GState.SessionLastSuccess, FConfig.ViewPortState.GetCurrentZoom);
        TfrmProgressDownload.Create(Application, VThread, VThreadLog, Self.OnMapUpdate);
      end else if ExtractFileExt(VFileName)='.hlg' then begin
        frmRegionProcess.LoadSelFromFile(VFileName);
      end else begin
        VImportConfig := frmImportConfigEdit.GetImportConfig(FMarkDBGUI);
        if VImportConfig <> nil then begin
          GState.ImportFileByExt.ProcessImport(VFileName, VImportConfig);
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.TBScreenSelectClick(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VPolygon: TArrayOfDoublePoint;
begin
  TBRectSave.ImageIndex:=20;
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VMapRect := VLocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);

  VPolygon := PolygonFromRect(VLonLatRect);
  frmRegionProcess.Show_(VZoom, VPolygon);
end;

procedure TfrmMain.TBGPSToPointCenterClick(Sender: TObject);
begin
  FConfig.GPSBehaviour.MapMoveCentered := TTBXitem(sender).Checked;
end;

procedure TfrmMain.NShowSelectionClick(Sender: TObject);
begin
  FConfig.LayersConfig.LastSelectionLayerConfig.Visible := TTBXItem(sender).Checked;
end;

procedure TfrmMain.NGoToCurClick(Sender: TObject);
begin
  FConfig.MainConfig.ZoomingAtMousePos := (Sender as TTBXItem).Checked
end;

procedure TfrmMain.TBXSelectSrchClick(Sender: TObject);
var
  VToolbarItem: TTBXItem;
  VItem: IGeoCoderListEntity;
begin
  if Sender is TTBXItem then begin
    VToolbarItem := TTBXItem(Sender);
    VItem := IGeoCoderListEntity(VToolbarItem.tag);
    if VItem <> nil then begin
      FConfig.MainGeoCoderConfig.ActiveGeoCoderGUID := VItem.GetGUID;
    end;
  end;
end;

procedure TfrmMain.TBXSearchEditAcceptText(Sender: TObject;
  var NewText: String; var Accept: Boolean);
var
  VItem: IGeoCoderListEntity;
  VResult: IGeoCodeResult;
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VItem := FConfig.MainGeoCoderConfig.GetActiveGeoCoder;
  VResult := VItem.GetGeoCoder.GetLocations(Trim(NewText), VLocalConverter.GetCenterLonLat);
  FSearchPresenter.ShowSearchResults(VResult, VLocalConverter.GetZoom);
end;

procedure TfrmMain.tbiEditSrchAcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
var
  VResult: IGeoCodeResult;
  VToolbarItem: TTBCustomItem;
  VItem: IGeoCoderListEntity;
  VLocalConverter: ILocalCoordConverter;
begin
  if Sender is TTBCustomItem then begin
    VToolbarItem := TTBCustomItem(Sender);
    VItem := IGeoCoderListEntity(VToolbarItem.Tag);
    if VItem <> nil then begin
      VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
      VResult := VItem.GetGeoCoder.GetLocations(Trim(NewText), VLocalConverter.GetCenterLonLat);
      FSearchPresenter.ShowSearchResults(VResult, VLocalConverter.GetZoom);
    end;
  end;
end;

procedure TfrmMain.TBSubmenuItem1Click(Sender: TObject);
var
  VResult: IGeoCodeResult;
  VLocalConverter: ILocalCoordConverter;
  VZoom: Byte;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VZoom := VLocalConverter.GetZoom;
  if frmGoTo.ShowGeocodeModal(VLocalConverter.GetCenterLonLat, VResult, VZoom, FMarkDBGUI) then begin
    FSearchPresenter.ShowSearchResults(VResult, VZoom);
  end;
end;

procedure TfrmMain.NSRTM3Click(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMouseMapPoint: TDoublePoint;
  VLonLat:TDoublePoint;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
  VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoom, False);
  VLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoom);
  frmIntrnalBrowser.showmessage('',SAS_STR_WiteLoad);
  frmIntrnalBrowser.Navigate('http://ws.geonames.org/srtm3?lat='+R2StrPoint(VLonLat.y)+'&lng='+R2StrPoint(VLonLat.x));
end;

procedure TfrmMain.NGTOPO30Click(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMouseMapPoint: TDoublePoint;
  VLonLat:TDoublePoint;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
  VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoom, False);
  VLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoom);
  frmIntrnalBrowser.showmessage('',SAS_STR_WiteLoad);
  frmIntrnalBrowser.Navigate('http://ws.geonames.org/gtopo30?lat='+R2StrPoint(VLonLat.y)+'&lng='+R2StrPoint(VLonLat.x));
end;

procedure TfrmMain.Google1Click(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMouseMapPoint: TDoublePoint;
  VLonLat:TDoublePoint;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
  VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoom, False);
  VLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoom);
  CopyStringToClipboard('http://maps.google.com/?ie=UTF8&ll='+R2StrPoint(VLonLat.y)+','+R2StrPoint(VLonLat.x)+'&spn=57.249013,100.371094&t=h&z='+inttostr(VZoom));
end;

procedure TfrmMain.YaLinkClick(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMouseMapPoint: TDoublePoint;
  VLonLat:TDoublePoint;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
  VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoom, False);
  VLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoom);
  VMapRect := VLocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
  CopyStringToClipboard(
    'http://beta-maps.yandex.ru/?ll='+
    R2StrPoint(round(VLonLat.x*100000)/100000)+'%2C'+
    R2StrPoint(round(VLonLat.y*100000)/100000)+
    '&spn='+R2StrPoint(abs(VLonLatRect.Left-VLonLatRect.Right))+'%2C'+
    R2StrPoint(abs(VLonLatRect.Top-VLonLatRect.Bottom))+'&l=sat'
  );
end;

procedure TfrmMain.kosmosnimkiru1Click(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMouseMapPoint: TDoublePoint;
  VLonLat:TDoublePoint;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
  VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoom, False);
  VLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoom);
  CopyStringToClipboard('http://kosmosnimki.ru/?x='+R2StrPoint(VLonLat.x)+'&y='+R2StrPoint(VLonLat.y)+'&z='+inttostr(VZoom)+'&fullscreen=false&mode=satellite');
end;

procedure TfrmMain.livecom1Click(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMouseMapPoint: TDoublePoint;
  VLonLat:TDoublePoint;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
  VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoom, False);
  VLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoom);
  CopyStringToClipboard('http://maps.live.com/default.aspx?v=2&cp='+R2StrPoint(VLonLat.y)+'~'+R2StrPoint(VLonLat.x)+'&style=h&lvl='+inttostr(VZoom));
end;

procedure TfrmMain.MainPopupMenuPopup(Sender: TObject);
var
  i:Integer;
  VMapType: TMapType;
  VLayerIsActive: Boolean;
  VActiveLayers: IMapTypeList;
  VMenuItem: TTBXItem;
begin
  ldm.Visible:=false;
  dlm.Visible:=false;
  TBOpenDirLayer.Visible:=false;
  TBCopyLinkLayer.Visible:=false;
  TBLayerInfo.Visible:=false;
  VActiveLayers := FConfig.MainMapsConfig.GetLayers.GetSelectedMapsList;
  For i:=0 to GState.MapType.Count-1 do begin
    VMapType := GState.MapType[i];
    if (VMapType.asLayer) then begin
      VLayerIsActive := VActiveLayers.GetMapTypeByGUID(VMapType.GUID) <> nil;
      TTBXItem(FNDwnItemList.GetByGUID(VMapType.GUID)).Visible := VLayerIsActive;
      TTBXItem(FNDelItemList.GetByGUID(VMapType.GUID)).Visible := VLayerIsActive;
      TTBXItem(FNOpenDirItemList.GetByGUID(VMapType.GUID)).Visible := VLayerIsActive;
      TTBXItem(FNCopyLinkItemList.GetByGUID(VMapType.GUID)).Visible := VLayerIsActive;
      VMenuItem := TTBXItem(FNLayerInfoItemList.GetByGUID(VMapType.GUID));
      VMenuItem.Visible := VLayerIsActive;
      if VLayerIsActive then begin
        VMenuItem.Enabled := VMapType.MapInfo <> '';
      end;
      if VLayerIsActive then begin
        ldm.Visible:=true;
        dlm.Visible:=true;
        TBCopyLinkLayer.Visible:=true;
        TBOpenDirLayer.Visible:=true;
        TBLayerInfo.Visible:=true;
      end
    end;
  end;
  VMapType:=FConfig.MainMapsConfig.GetSelectedMapType.MapType;
  NMapInfo.Enabled:=VMapType.MapInfo<>'';
end;

procedure TfrmMain.ImageAtlas1Click(Sender: TObject);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMouseMapPoint: TDoublePoint;
  VLonLat:TDoublePoint;
begin
  VLocalConverter := FConfig.ViewPortState.GetVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;
  VMouseMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(FMouseDownPoint);
  VConverter.CheckPixelPosFloatStrict(VMouseMapPoint, VZoom, False);
  VLonLat := VConverter.PixelPosFloat2LonLat(VMouseMapPoint, VZoom);
  CopyStringToClipboard(
    'http://imageatlas.digitalglobe.com/ia-webapp/?lat='+
    R2StrPoint(VLonLat.y)+'&lon='+R2StrPoint(VLonLat.x)+
    '&zoom='+inttostr(VZoom)
  );
end;

procedure TfrmMain.NGoToForumClick(Sender: TObject);
begin
  OpenUrlInBrowser('http://sasgis.ru/forum');
end;

procedure TfrmMain.NGoToSiteClick(Sender: TObject);
begin
  OpenUrlInBrowser('http://sasgis.ru/');
end;

procedure TfrmMain.tbtmHelpBugTrackClick(Sender: TObject);
begin
  OpenUrlInBrowser('http://sasgis.ru/mantis/');
end;

procedure TfrmMain.TBEditPathMarshClick(Sender: TObject);
var
  VResult: TArrayOfDoublePoint;
  VProvider: IPathDetalizeProvider;
  VIsError: Boolean;
begin
  case TTBXItem(Sender).tag of
    1:VProvider := TPathDetalizeProviderMailRu1.Create;
    2:VProvider := TPathDetalizeProviderMailRu2.Create;
    3:VProvider := TPathDetalizeProviderMailRu3.Create;
  end;
  if VProvider <> nil then begin
    VIsError := True;
    try
      VResult := VProvider.GetPath(FLineOnMapEdit.GetPoints, FMarshrutComment);
      VIsError := False;
    except
      on E: Exception do begin
        ShowMessage(E.Message);
      end;
    end;
    if not VIsError then begin
      if Length(VResult) > 0 then begin
        FLineOnMapEdit.SetPoints(VResult);
      end;
    end else begin
      FMarshrutComment := '';
    end;
  end;
end;

procedure TfrmMain.TBXItem1Click(Sender: TObject);
var
  VResult: TArrayOfDoublePoint;
  VProvider: IPathDetalizeProvider;
  VIsError: Boolean;
begin
  case TTBXItem(Sender).tag of
    1:VProvider := TPathDetalizeProviderYourNavigation1.Create;
    11:VProvider := TPathDetalizeProviderYourNavigation11.Create;
    2:VProvider := TPathDetalizeProviderYourNavigation2.Create;
    22:VProvider := TPathDetalizeProviderYourNavigation22.Create;
  end;
  if VProvider <> nil then begin
    VIsError := True;
    try
      VResult := VProvider.GetPath(FLineOnMapEdit.GetPoints, FMarshrutComment);
      VIsError := False;
    except
      on E: Exception do begin
        ShowMessage(E.Message);
      end;
    end;
    if not VIsError then begin
      if Length(VResult) > 0 then begin
        FLineOnMapEdit.SetPoints(VResult);
      end;
    end else begin
      FMarshrutComment := '';
    end;
  end;
end;

procedure TfrmMain.ZSliderMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
var h,xy:integer;
begin
  if ssLeft in Shift then begin
    if FRuller.Width<FRuller.Height then begin
      XY:=ZSlider.Height-Y;
      h:=(ZSlider.Height div 24);
    end else begin
      XY:=X;
      h:=(ZSlider.Width div 24);
    end;
    if XY in [h..h*24] then begin
      ZSlider.Tag:=(XY div h)-1;
      PaintZSlider(ZSlider.Tag);
      labZoom.Caption:='z'+inttostr(ZSlider.Tag+1);
    end;
  end;
end;

procedure TfrmMain.ZSliderMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Button=mbLeft then begin
    ZSliderMouseMove(Sender,[ssLeft],X,Y,Layer);
    zooming(ZSlider.Tag,false);
  end;
end;

procedure TfrmMain.PaintZSlider(zoom:integer);
var tumbpos:TPoint;
begin
  if FRuller.Height>FRuller.Width then begin
    tumbpos.Y:=FRuller.Height-((FRuller.Height div 24)*(zoom+1))-(FTumbler.Height div 2);
    tumbpos.X:=(FRuller.Width div 2) - (FTumbler.Width div 2);
  end else begin
    tumbpos.X:=(FRuller.Width div 24)*(zoom+1)-(FTumbler.Width div 2);
    tumbpos.Y:=(FRuller.Height div 2) - (FTumbler.Height div 2);
  end;
  ZSlider.Bitmap.Assign(FRuller);
  FTumbler.DrawTo(ZSlider.Bitmap,tumbpos.X,tumbpos.Y);
end;

end.





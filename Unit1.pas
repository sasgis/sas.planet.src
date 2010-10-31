unit Unit1;

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
  MSHTML,
  Variants,
  ActiveX,
  ShlObj,
  ComObj,
  Graphics,
  StdCtrls,
  OleCtrls,
  Controls,
  Buttons,
  WinInet,
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
  RXSlider,
  EmbeddedWB,
  SHDocVw_EWB,
  TB2ToolWindow,
  TBXToolPals,
  EwbCore,
  TBX,
  TBXControls,
  TBXExtItems,
  u_CommonFormAndFrameParents,
  i_JclNotify,
  i_IGUIDList,
  i_IMapChangeMessage,
  i_IHybrChangeMessage,
  i_IPosChangeMessage,
  t_LoadEvent,
  i_IConfigDataWriteProvider,
  u_GeoToStr,
  t_CommonTypes,
  i_IGPSRecorder,
  i_GeoCoder,
  i_ISearchResultPresenter,
  u_WindowLayerBasicList,
  u_MarksSimple,
  Ugeofun,
  u_MapLayerWiki,
  ULogo,
  UMapType,
  UResStrings,
  u_ShortcutManager,
  u_MapMainLayer,
  u_LayerStatBar,
  u_LayerScaleLine,
  u_MapMarksLayer,
  u_MapGPSLayer,
  u_MapLayerNavToMark,
  u_MapFillingLayer,
  u_MiniMapLayer,
  u_MapNalLayer,
  u_MapLayerGoto,
  u_MapLayerShowError,
  u_CenterScale,
  u_SelectionLayer,
  t_GeoTypes;

type
  TAOperation = (
    ao_movemap,
    ao_add_line ,
    ao_add_poly,
    ao_add_point,
    ao_edit_point,
    ao_edit_line,
    ao_edit_poly,
    ao_line,
    ao_rect,
    ao_reg
  );

  TFmain = class(TCommonFormParent)
    map: TImage32;
    PopupMenu1: TPopupMenu;
    NaddPoint: TMenuItem;
    N15: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    N11: TMenuItem;
    N24: TMenuItem;
    Nopendir: TMenuItem;
    N25: TMenuItem;
    NDel: TMenuItem;
    TBImageList1: TTBImageList;
    N28: TMenuItem;
    N012: TMenuItem;
    N022: TMenuItem;
    N032: TMenuItem;
    N042: TMenuItem;
    N052: TMenuItem;
    N062: TMenuItem;
    N072: TMenuItem;
    N082: TMenuItem;
    N091: TMenuItem;
    N101: TMenuItem;
    N111: TMenuItem;
    N121: TMenuItem;
    N131: TMenuItem;
    N141: TMenuItem;
    N151: TMenuItem;
    N161: TMenuItem;
    N171: TMenuItem;
    N181: TMenuItem;
    N191: TMenuItem;
    N201: TMenuItem;
    N211: TMenuItem;
    N221: TMenuItem;
    N231: TMenuItem;
    N241: TMenuItem;
    N30: TMenuItem;
    Google1: TMenuItem;
    N43: TMenuItem;
    TBImageList2: TTBImageList;
    OpenDialog1: TOpenDialog;
    YaLink: TMenuItem;
    kosmosnimkiru1: TMenuItem;
    N51: TMenuItem;
    NMarkDel: TMenuItem;
    NMarkEdit: TMenuItem;
    NMarkSep: TMenuItem;
    NMarkOper: TMenuItem;
    livecom1: TMenuItem;
    N13: TMenuItem;
    ImageAtlas1: TMenuItem;
    N26: TMenuItem;
    N27: TMenuItem;
    DigitalGlobe1: TMenuItem;
    ldm: TMenuItem;
    dlm: TMenuItem;
    SaveLink: TSaveDialog;
    NSRTM3: TMenuItem;
    N47: TMenuItem;
    N49: TMenuItem;
    NGTOPO30: TMenuItem;
    NMarkNav: TMenuItem;
    TBDock: TTBXDock;
    TBMainToolBar: TTBXToolbar;
    TBDockBottom: TTBXDock;
    TBDockLeft: TTBXDock;
    SrcToolbar: TTBXToolbar;
    TBMarksToolbar: TTBXToolbar;
    GPSToolbar: TTBXToolbar;
    TBExit: TTBXToolbar;
    ZoomToolBar: TTBXToolbar;
    TBControlItem1: TTBControlItem;
    RxSlider1: TRxSlider;
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
    NMarksCalcs: TMenuItem;
    NMarksCalcsLen: TMenuItem;
    NMarksCalcsSq: TMenuItem;
    NMarksCalcsPer: TMenuItem;
    WebBrowser1: TEmbeddedWB;
    N1: TMenuItem;
    NMapInfo: TMenuItem;
    TBImageList1_24: TTBImageList;
    PMNRObject: TPopupMenu;
    NGoHim: TMenuItem;
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
    ImagesSrc24: TTBImageList;
    TBFullSize: TTBXItem;
    TBmove: TTBXItem;
    TBCalcRas: TTBXItem;
    TBRectSave: TTBXSubmenuItem;
    TBMapZap: TTBXSubmenuItem;
    TBGoTo: TTBXSubmenuItem;
    TBEditItem2: TTBEditItem;
    TBEditItem1: TTBEditItem;
    EditGoogleSrch: TTBEditItem;
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
    TBItem3: TTBXItem;
    TBItem5: TTBXItem;
    TBItemDelTrack: TTBXItem;
    NFoolSize: TTBXItem;
    NGoToCur: TTBXItem;
    Nbackload: TTBXItem;
    NbackloadLayer: TTBXItem;
    Nanimate: TTBXItem;
    N32: TTBXItem;
    Ninvertcolor: TTBXItem;
    N4: TTBXSubmenuItem;
    N31: TTBXSubmenuItem;
    NFillMap: TTBXSubmenuItem;
    TBFillingTypeMap: TTBXSubmenuItem;
    TBfillMapAsMain: TTBXItem;
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
    TBXLangRus: TTBXItem;
    TBXLangEng: TTBXItem;
    tbitmGPSConnect: TTBXItem;
    tbitmGPSTrackShow: TTBXItem;
    tbitmGPSCenterMap: TTBXItem;
    tbitmGPSTrackSave: TTBXItem;
    tbitmGPSTrackSaveToDb: TTBXItem;
    tbitmGPSTrackClear: TTBXItem;
    NMainToolBarShow: TTBXItem;
    NZoomToolBarShow: TTBXItem;
    NsrcToolBarShow: TTBXItem;
    NGPSToolBarShow: TTBXItem;
    NMarksBarShow: TTBXItem;
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
    EditCommentsImgs: TImageList;
    OpenPictureDialog: TOpenPictureDialog;
    TBXSensorsBar: TTBXToolWindow;
    ScrollBox1: TScrollBox;
    TBXDock1: TTBXDock;
    TBXSensorSpeedAvgBar: TTBXToolWindow;
    TBXSensorSpeedAvg: TTBXLabel;
    TBXSensorSpeedBar: TTBXToolWindow;
    TBXSensorSpeed: TTBXLabel;
    TBXsensorOdometrBar: TTBXToolWindow;
    TBXSensorOdometr: TTBXLabel;
    TBXSensorPathBar: TTBXToolWindow;
    TBXOdometrNow: TTBXLabel;
    TBXSensorBattaryBar: TTBXToolWindow;
    TBXSensorBattary: TTBXLabel;
    TBXSensorLenToMarkBar: TTBXToolWindow;
    TBXSensorLenToMark: TTBXLabel;
    TBXLabel8: TTBXLabel;
    TBXLabel9: TTBXLabel;
    TBXLabel10: TTBXLabel;
    TBXLabel11: TTBXLabel;
    TBXLabel13: TTBXLabel;
    TBXLabel14: TTBXLabel;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SBClearSensor: TSpeedButton;
    NSensorsBar: TTBXItem;
    NSensors: TTBXSubmenuItem;
    NSensorLenToMarkBar: TTBXItem;
    NsensorOdometrBar: TTBXItem;
    NSensorPathBar: TTBXItem;
    NSensorSpeedAvgBar: TTBXItem;
    NSensorSpeedBar: TTBXItem;
    NSensorBattaryBar: TTBXItem;
    TBXPopupMenuSensors: TTBXPopupMenu;
    TBXItem1: TTBXItem;
    TBXLabelItem1: TTBXLabelItem;
    TBXLabelItem2: TTBXLabelItem;
    TBXItem2: TTBXItem;
    TBXItem3: TTBXItem;
    TBXItem4: TTBXItem;
    TBXSensorAltitudeBar: TTBXToolWindow;
    SpeedButton4: TSpeedButton;
    TBXSensorAltitude: TTBXLabel;
    TBXLabel2: TTBXLabel;
    NSensorAltitudeBar: TTBXItem;
    TBXSensorSpeedMaxBar: TTBXToolWindow;
    SpeedButton5: TSpeedButton;
    TBXSensorSpeedMax: TTBXLabel;
    TBXLabel3: TTBXLabel;
    NSensorSpeedMaxBar: TTBXItem;
    SpeedButton6: TSpeedButton;
    TBXItem5: TTBXItem;
    TBXSeparatorItem16: TTBXSeparatorItem;
    TBXSeparatorItem17: TTBXSeparatorItem;
    TBXSensorAzimutBar: TTBXToolWindow;
    TBXSensorAzimut: TTBXLabel;
    TBXLabel4: TTBXLabel;
    NSensorAzimutBar: TTBXItem;
    TBXToolBarSearch: TTBXToolbar;
    TBXSearchEdit: TTBXEditItem;
    TBXSelectSrchType: TTBXSubmenuItem;
    TBXSelectGoogleSrch: TTBXItem;
    TBXSelectYandexSrch: TTBXItem;
    NToolBarSearch: TTBXItem;
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
    NSignalStrengthBar: TTBXItem;
    TBXSeparatorItem19: TTBXSeparatorItem;
    TBXItem8: TTBXItem;
    TBXItem9: TTBXItem;
    TBControlItem3: TTBControlItem;
    Label1: TLabel;
    TBXsensorOdometr2Bar: TTBXToolWindow;
    SpeedButton1: TSpeedButton;
    TBXSensorOdometr2: TTBXLabel;
    TBXLabel6: TTBXLabel;
    NsensorOdometr2Bar: TTBXItem;
    TBGPSToPoint: TTBXSubmenuItem;
    TBGPSToPointCenter: TTBXItem;
    tbitmGPSToPointCenter: TTBXItem;
    procedure FormActivate(Sender: TObject);
    procedure NzoomInClick(Sender: TObject);
    procedure NZoomOutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TBZoom_outClick(Sender: TObject);
    procedure TBZoomInClick(Sender: TObject);
    procedure TBmoveClick(Sender: TObject);
    procedure TBFullSizeClick(Sender: TObject);
    procedure RxSlider1Change(Sender: TObject);
    procedure NCalcRastClick(Sender: TObject);
    procedure NFoolSizeClick(Sender: TObject);
    procedure N6Click(Sender: TObject);
    procedure ZoomToolBarDockChanging(Sender: TObject; Floating: Boolean; DockingTo: TTBDock);
    procedure N8Click(Sender: TObject);
    procedure TBmap1Click(Sender: TObject);
    procedure NbackloadClick(Sender: TObject);
    procedure NaddPointClick(Sender: TObject);
    procedure N20Click(Sender: TObject);
    procedure N15Click(Sender: TObject);
    procedure N21Click(Sender: TObject);
    procedure N11Click(Sender: TObject);
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
    procedure N012Click(Sender: TObject);
    procedure N29Click(Sender: TObject);
    procedure NMainToolBarShowClick(Sender: TObject);
    procedure NZoomToolBarShowClick(Sender: TObject);
    procedure NsrcToolBarShowClick(Sender: TObject);
    procedure EditGoogleSrchAcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
    procedure TBSubmenuItem1Click(Sender: TObject);
    procedure TBMainToolBarClose(Sender: TObject);
    procedure N000Click(Sender: TObject);
    procedure TBItem2Click(Sender: TObject);
    procedure TBGPSconnClick(Sender: TObject);
    procedure NGPSToolBarShowClick(Sender: TObject);
    procedure TBGPSPathClick(Sender: TObject);
    procedure TBGPSToPointClick(Sender: TObject);
    procedure N30Click(Sender: TObject);
    procedure TBCOORDClick(Sender: TObject);
    procedure ShowstatusClick(Sender: TObject);
    procedure ShowMiniMapClick(Sender: TObject);
    procedure ShowLineClick(Sender: TObject);
    procedure N32Click(Sender: TObject);
    procedure TBItem3Click(Sender: TObject);
    procedure Google1Click(Sender: TObject);
    procedure mapResize(Sender: TObject);
    procedure TBLoadSelFromFileClick(Sender: TObject);
    procedure TBEditItem1AcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
    procedure YaLinkClick(Sender: TObject);
    procedure kosmosnimkiru1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure NinvertcolorClick(Sender: TObject);
    procedure mapDblClick(Sender: TObject);
    procedure TBAdd_PointClick(Sender: TObject);
    procedure TBAdd_LineClick(Sender: TObject);
    procedure TBAdd_PolyClick(Sender: TObject);
    procedure TBItem5Click(Sender: TObject);
    procedure NMarkEditClick(Sender: TObject);
    procedure NMarkDelClick(Sender: TObject);
    procedure NMarksBarShowClick(Sender: TObject);
    procedure NMarkOperClick(Sender: TObject);
    procedure livecom1Click(Sender: TObject);
    procedure N13Click(Sender: TObject);
    procedure ImageAtlas1Click(Sender: TObject);
    procedure DigitalGlobe1Click(Sender: TObject);
    procedure RxSlider1Changed(Sender: TObject);
    procedure mapMouseLeave(Sender: TObject);
    procedure GPSReceiverDisconnect;
    procedure GPSReceiverConnect;
    procedure GPSReceiverTimeout;
    procedure GPSReceiverConnectError;
    procedure GPSReceiverReceive;
    procedure NMapParamsClick(Sender: TObject);
    procedure mapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure mapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure mapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
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
    procedure TBItem1Click(Sender: TObject);
    procedure NMapInfoClick(Sender: TObject);
    procedure TBXToolPalette1CellClick(Sender: TTBXCustomToolPalette;var ACol, ARow: Integer; var AllowChange: Boolean);
    procedure WebBrowser1Authenticate(Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName, szPassWord: WideString; var Rezult: HRESULT);
    procedure NanimateClick(Sender: TObject);
    procedure NbackloadLayerClick(Sender: TObject);
    procedure SBClearSensorClick(Sender: TObject);
    procedure TBXSensorsBarVisibleChanged(Sender: TObject);
    procedure NSensorsBarClick(Sender: TObject);
    procedure TBXItem1Click(Sender: TObject);
    procedure TBXItem5Click(Sender: TObject);
    procedure TBXSelectYandexSrchClick(Sender: TObject);
    procedure TBXSearchEditAcceptText(Sender: TObject; var NewText: String;
      var Accept: Boolean);
    procedure tbitmPositionByGSMClick(Sender: TObject);
    procedure TBXItem6Click(Sender: TObject);
    procedure NShowSelectionClick(Sender: TObject);
    procedure NGoToCurClick(Sender: TObject);
    procedure TBXItem8Click(Sender: TObject);
    procedure TBXItem9Click(Sender: TObject);
    procedure TBGPSToPointCenterClick(Sender: TObject);
  private
    FnilLastLoad: TLastLoad;
    FShowActivHint: boolean;
    FHintWindow: THintWindow;
    Flock_toolbars: boolean;
    Frect_dwn: Boolean;
    Frect_p2: boolean;
    FMainLayer: TMapMainLayer;
    FLayerStatBar: TLayerStatBar;
    FShowErrorLayer: TTileErrorInfoLayer;
    FWikiLayer: TWikiLayer;
    FdWhenMovingButton: integer;
    FLenShow: boolean;
    RectWindow: TRect;
    FMarshrutComment: string;
    Fmovepoint: integer;
    Flastpoint: integer;
    FSelectionRect: TExtendedRect;
    Flength_arr: TExtendedPointArray;
    Fadd_line_arr: TExtendedPointArray;
    Freg_arr: TExtendedPointArray;
    FPWL: TResObj;

    FLayerScaleLine: TLayerScaleLine;
    FLayerMapNal: TMapNalLayer;
    FLayerMapGPS: TMapGPSLayer;
    FLayerGoto: TGotoLayer;
    FLayerFillingMap: TMapFillingLayer;
    FLayerMapMarks: TMapMarksLayer;
    FLayerMapScale: TCenterScale;
    FLayerMiniMap: TMiniMapLayer;
    FLayerSelection: TSelectionLayer;

    ProgramStart: Boolean;
    ProgramClose: Boolean;
    FMapPosChangeListener: IJclListener;
    FMainMapChangeListener: IJclListener;
    FHybrChangeListener: IJclListener;
    FMapLayersVsibleChangeListener: IJclListener;
    FGPSConntectListener: IJclListener;
    FGPSDisconntectListener: IJclListener;
    FGPSConntectErrorListener: IJclListener;
    FGPSTimeOutListener: IJclListener;
    FGPSReceiveListener: IJclListener;

    FMainToolbarItemList: IGUIDObjectList; //Пункт списка в главном тулбаре
    FMainToolbarSubMenuItemList: IGUIDObjectList; //Подпункт списка в главном тулбаре
    FTBFillingItemList: IGUIDObjectList; //Пункт главного меню Вид/Карта заполнения/Формировать для
    FNLayerParamsItemList: IGUIDObjectList; //Пункт гланого меню Параметры/Параметры слоя
    FNDwnItemList: IGUIDObjectList; //Пункт контекстного меню Загрузить тайл слоя
    FNDelItemList: IGUIDObjectList; //Пункт контекстного меню Удалить тайл слоя

    FShortCutManager: TShortcutManager;
    FLayersList: TWindowLayerBasicList;

    FSearchPresenter: ISearchResultPresenter;
    FMouseDownPoint: TPoint;
    FMouseUpPoint: TPoint;
    FmoveTrue: Tpoint;
    FMapMoving: Boolean;
    FMapZoomAnimtion: Boolean;
    FEditMarkId:integer;
    FCurrentOper: TAOperation;

    procedure DoMessageEvent(var Msg: TMsg; var Handled: Boolean);
    procedure WMGetMinMaxInfo(var msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure Set_lock_toolbars(const Value: boolean);
    procedure MouseOnMyReg(var APWL:TResObj;xy:TPoint);
    procedure InitSearchers;
    procedure zooming(ANewZoom: byte; move: boolean);
    procedure PrepareSelectionRect(Shift: TShiftState; var ASelectedLonLat: TExtendedRect);
    procedure insertinpath(pos: integer; APoint: TExtendedPoint);
    procedure delfrompath(pos: integer);
    procedure ProcessPosChangeMessage(AMessage: IPosChangeMessage);
    procedure ProcessMapChangeMessage(AMessage: IMapChangeMessage);
    procedure ProcessHybrChangeMessage(AMessage: IHybrChangeMessage);
    procedure CopyBtmToClipboard(btm: TBitmap);
    function GetStreamFromURL(var ms: TMemoryStream; url: string; conttype: string): integer;
    function GetIgnoredMenuItemsList: TList;
    procedure MapLayersVisibleChange;
    procedure CopyStringToClipboard(s: Widestring);
    procedure UpdateGPSsensors;
    procedure setalloperationfalse(newop: TAOperation);
    procedure UpdateGPSSatellites;
  public
    FGoogleGeoCoder: IGeoCoder;
    FYandexGeoCoder: IGeoCoder;
    LayerMapNavToMark: TNavToMarkLayer;
    MouseCursorPos: Tpoint;
    property lock_toolbars: boolean read Flock_toolbars write Set_lock_toolbars;
    property ShortCutManager: TShortcutManager read FShortCutManager;
    property LayerMiniMap: TMiniMapLayer read FLayerMiniMap;

    constructor Create(AOwner: TComponent); override;
    procedure generate_im(lastload: TLastLoad; err: string); overload;
    procedure generate_im; overload;
    procedure topos(LL: TExtendedPoint; zoom_: byte; draw: boolean);
    procedure OpenUrlInBrowser(URL: string);
    procedure CreateMapUI;
    procedure SaveWindowConfigToIni(AProvider: IConfigDataWriteProvider);
    function GetMarksIterator(AZoom: Byte; ARect: TExtendedRect;
      AShowType: TMarksShowType): TMarksIteratorBase;
  end;


const
  GSHprec=100000000;

var
  Fmain: TFmain;

implementation

uses
  StrUtils,
  DateUtils,
  u_JclNotify,
  u_GUIDObjectList,
  u_GlobalState,
  frm_GoTo,
  UAbout,
  Usettings,
  USaveas,
  UProgress,
  Unit4,
  USelLonLat,
  UEditMap,
  Ubrowser,
  UMarksExplorer,
  UFDGAvailablePic,
  u_TileDownloaderUIOneTile,
  u_LogForTaskThread,
  i_GPS,
  i_ILogSimple,
  i_ILogForTaskThread,
  i_ICoordConverter,
  u_KmlInfoSimple,
  i_IMapViewGoto,
  u_MapViewGotoOnFMain,
  frm_SearchResults,
  i_IProxySettings,
  u_ProxySettingsFromTInetConnect,
  u_GeoCoderByGoogle,
  u_GeoCoderByYandex,
  u_MarksReadWriteSimple,
  u_ThreadDownloadTiles,
  u_SaveLoadTBConfigByConfigProvider,
  UGSM,
  UImport,
  u_MapViewPortState;

{$R *.dfm}

{ TListenerOfMainForm }

type
  TListenerOfMainForm = class(TJclBaseListener)
  protected
    FMainForm: TFmain;
  public
    constructor Create(AMainForm: TFmain);
  end;

constructor TListenerOfMainForm.Create(AMainForm: TFmain);
begin
  FMainForm := AMainForm;
end;


{ TChangePosListenerOfMainForm }

type
  TChangePosListenerOfMainForm = class(TListenerOfMainForm)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

procedure TChangePosListenerOfMainForm.Notification(
  msg: IJclNotificationMessage);
var
  VMessage: IPosChangeMessage;
begin
  VMessage := msg as IPosChangeMessage;
  FMainForm.ProcessPosChangeMessage(VMessage);
end;

{ TMapLayersVisibleChange }

type
  TMapLayersVisibleChange = class(TListenerOfMainForm)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

procedure TMapLayersVisibleChange.Notification(
  msg: IJclNotificationMessage);
begin
  FMainForm.MapLayersVisibleChange;
end;

{ TGPSConnected }

type
  TGPSConnected = class(TListenerOfMainForm)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

procedure TGPSConnected.Notification(
  msg: IJclNotificationMessage);
begin
  TThread.Synchronize(nil, FMainForm.GPSReceiverConnect);
end;

{ TGPSDisonnected }

type
  TGPSDisonnected = class(TListenerOfMainForm)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

procedure TGPSDisonnected.Notification(
  msg: IJclNotificationMessage);
begin
  TThread.Synchronize(nil, FMainForm.GPSReceiverDisconnect);
end;

{ TGPSReceive }

type
  TGPSReceive = class(TListenerOfMainForm)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

procedure TGPSReceive.Notification(
  msg: IJclNotificationMessage);
begin
  TThread.Synchronize(nil, FMainForm.GPSReceiverReceive);
end;

{ TGPSTimeOut }

type
  TGPSTimeOut = class(TListenerOfMainForm)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

procedure TGPSTimeOut.Notification(
  msg: IJclNotificationMessage);
begin
  TThread.Synchronize(nil, FMainForm.GPSReceiverTimeout);
end;

{ TGPSConnectError }

type
  TGPSConnectError = class(TListenerOfMainForm)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

procedure TGPSConnectError.Notification(
  msg: IJclNotificationMessage);
begin
  TThread.Synchronize(nil, FMainForm.GPSReceiverConnectError);
end;

{ TMainMapChangeListenerOfMainForm }

type
  TMainMapChangeListenerOfMainForm = class(TListenerOfMainForm)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

procedure TMainMapChangeListenerOfMainForm.Notification(
  msg: IJclNotificationMessage);
var
  VMessage: IMapChangeMessage;
begin
  VMessage := msg as IMapChangeMessage;
  FMainForm.ProcessMapChangeMessage(VMessage);
end;

{ TChangeHybrChangeListenerOfMainForm }

type
  THybrChangeListenerOfMainForm = class(TListenerOfMainForm)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

procedure THybrChangeListenerOfMainForm.Notification(
  msg: IJclNotificationMessage);
var
  VMessage: IHybrChangeMessage;
begin
  VMessage := msg as IHybrChangeMessage;
  FMainForm.ProcessHybrChangeMessage(VMessage);
end;

procedure TFmain.MapLayersVisibleChange;
var
  VGUID: TGUID;
  VEnumGUID: IEnumGUID;
  i: Cardinal;
  VMapType: TMapType;
begin
  Showstatus.Checked := FLayerStatBar.Visible;
  if FLayerStatBar.Visible then begin
    FLayerScaleLine.BottomMargin := 17;
    FLayerMiniMap.BottomMargin := 17;
  end else begin
    FLayerScaleLine.BottomMargin := 0;
    FLayerMiniMap.BottomMargin := 0;
  end;
  ShowMiniMap.Checked := FLayerMiniMap.Visible;
  ShowLine.Checked := FLayerScaleLine.Visible;
  NShowSelection.Checked := FLayerSelection.Visible;
  N32.Checked:=FLayerMapScale.Visible;

  TBSrc.ImageIndex := integer(FMainLayer.UseDownload);
  case FMainLayer.UseDownload of
    tsInternet: NSRCinet.Checked:=true;
    tsCache: NSRCesh.Checked:=true;
    tsCacheInternet: NSRCic.Checked:=true;
  end;

  if FLayerFillingMap.SourceZoom >-1 then begin
    TBMapZap.Caption:='x'+inttostr(FLayerFillingMap.SourceZoom+1);
  end else begin
    TBMapZap.Caption:='';
  end;

  VMapType := FLayerFillingMap.SourceSelected;
  if VMapType <> nil then begin
    TBfillMapAsMain.Checked:=false;
    VEnumGUID := FTBFillingItemList.GetGUIDEnum;
    while VEnumGUID.Next(1, VGUID, i) = S_OK  do begin
      if IsEqualGUID(VMapType.GUID, VGUID) then begin
        TTBXItem(FTBFillingItemList.GetByGUID(VGUID)).Checked := True;
      end else begin
        TTBXItem(FTBFillingItemList.GetByGUID(VGUID)).Checked := False;
      end;
    end;
  end else begin
    TBfillMapAsMain.Checked:=true;
    VEnumGUID := FTBFillingItemList.GetGUIDEnum;
    while VEnumGUID.Next(1, VGUID, i) = S_OK  do begin
      TTBXItem(FTBFillingItemList.GetByGUID(VGUID)).Checked := False;
    end;
  end;

  mapResize(nil);
end;

procedure TFmain.ProcessHybrChangeMessage(AMessage: IHybrChangeMessage);
var
  i:integer;
  VMapType: TMapType;
  VWikiLayersVisible: Boolean;
begin
  VWikiLayersVisible := False;
  for i:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[i];
    if VMapType.asLayer then begin
      if GState.ViewState.IsHybrGUIDSelected(VMapType.GUID) then begin
        TTBXItem(FMainToolbarItemList.GetByGUID(VMapType.GUID)).Checked := True;
        if VMapType.IsKmlTiles then begin
          VWikiLayersVisible := True;
        end;
      end else begin
        TTBXItem(FMainToolbarItemList.GetByGUID(VMapType.GUID)).Checked := False;
      end;
    end;
  end;
  generate_im;
  FWikiLayer.Visible := VWikiLayersVisible;
end;

procedure TFmain.ProcessMapChangeMessage(AMessage: IMapChangeMessage);
var
  VMainMapOld: TMapType;
  VMainMapNew: TMapType;
  VMainToolbarItem: TTBXItem;
begin
  VMainMapNew := AMessage.GetNewMap;
  VMainMapOld := AMessage.GetSorurceMap;

  VMainToolbarItem := TTBXItem(FMainToolbarItemList.GetByGUID(VMainMapOld.GUID));
  VMainToolbarItem.Checked:=false;

  VMainToolbarItem := TTBXItem(FMainToolbarItemList.GetByGUID(VMainMapNew.GUID));
  VMainToolbarItem.Checked:=true;
  TBSMB.ImageIndex := GState.MapTypeIcons24List.GetIconIndexByGUID(VMainMapNew.GUID);
  if GState.Showmapname then begin
    TBSMB.Caption:=VMainMapNew.name;
  end else begin
    TBSMB.Caption:='';
  end;
  generate_im;
end;

procedure TFmain.ProcessPosChangeMessage(AMessage: IPosChangeMessage);
var
  VPoint: TPoint;
  VZoomCurr: Byte;
  ts2,ts3,fr:int64;
  VConverter: ICoordConverter;
begin
  QueryPerformanceCounter(ts2);
  try
    VPoint := AMessage.GetMapPixel;
    VZoomCurr := AMessage.GetZoom;
    VConverter := AMessage.GetMap.GeoConvert;

    if VZoomCurr<=0  then TBZoom_Out.Enabled:=false
          else TBZoom_Out.Enabled:=true;
    if VZoomCurr>=23 then TBZoomIn.Enabled:=false
          else TBZoomIn.Enabled:=true;
    NZoomIn.Enabled:=TBZoomIn.Enabled;
    NZoomOut.Enabled:=TBZoom_Out.Enabled;
    RxSlider1.Value:=VZoomCurr;
    labZoom.caption:=inttostr(VZoomCurr + 1)+'x';
    map.BeginUpdate;
    try
      FLayerStatBar.Redraw;
      FLayerScaleLine.Redraw;
      FMainLayer.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
      FLayerFillingMap.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
      FLayerMapMarks.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
      FLayerMapNal.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
      FWikiLayer.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
      FLayerMapGPS.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
      FLayerGoto.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
      LayerMapNavToMark.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
      FShowErrorLayer.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
      FLayerMiniMap.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
    finally
      map.EndUpdate;
      map.Changed;
    end;
  finally
    QueryPerformanceCounter(ts3);
    QueryPerformanceFrequency(fr);
    Label1.caption :=FloatToStr((ts3-ts2)/(fr/1000));
  end;
end;

procedure TFMain.Set_lock_toolbars(const Value: boolean);
begin
 TBDock.AllowDrag:=not value;
 TBDockLeft.AllowDrag:=not value;
 TBDockRight.AllowDrag:=not value;
 TBDockBottom.AllowDrag:=not value;
 Flock_toolbars:=value;
end;

procedure TFmain.CopyBtmToClipboard(btm: TBitmap);
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

procedure TFmain.CopyStringToClipboard(s: Widestring);
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

procedure TFmain.insertinpath(pos: integer; APoint: TExtendedPoint);
var
  VCount: Integer;
begin
  VCount := Length(Fadd_line_arr);
  if (pos >=0) and (pos <= VCount)  then begin
    SetLength(Fadd_line_arr, VCount + 1);
    if pos < VCount then begin
      CopyMemory(@Fadd_line_arr[pos + 1], @Fadd_line_arr[pos], (VCount-pos)*sizeOf(TExtendedPoint));
    end;
    Fadd_line_arr[pos] := APoint;
  end;
end;

procedure TFmain.delfrompath(pos: integer);
var
  VCount: Integer;
begin
  VCount := Length(Fadd_line_arr);
  if (pos >=0) and (pos < VCount)  then begin
    if pos < VCount - 1 then begin
      CopyMemory(@Fadd_line_arr[pos], @Fadd_line_arr[pos+1], (VCount-pos-1)*sizeOf(TExtendedPoint));
    end;
    SetLength(Fadd_line_arr, VCount - 1);
    if Flastpoint > 0 then begin
      Dec(Flastpoint);
    end;
  end;
end;

procedure TFmain.setalloperationfalse(newop: TAOperation);
begin
 if FCurrentOper=newop then newop:=ao_movemap;
 FLayerMapNal.DrawNothing;
 FMarshrutComment:='';
 FLayerMapNal.Visible:=newop<>ao_movemap;
 TBmove.Checked:=newop=ao_movemap;
 TBCalcRas.Checked:=newop=ao_line;
 TBRectSave.Checked:=(newop=ao_reg)or(newop=ao_rect);
 TBAdd_Point.Checked:=newop=ao_Add_Point;
 TBAdd_Line.Checked:=newop=ao_Add_line;
 TBAdd_Poly.Checked:=newop=ao_Add_Poly;
 TBEditPath.Visible:=false;
 TBEditPathSave.Visible:=(newop=ao_Add_line)or(newop=ao_Add_Poly)or(newop=ao_Edit_line)or(newop=ao_Edit_Poly);
 TBEditPathOk.Visible:=(newop=ao_reg);
 TBEditPathLabel.Visible:=(newop=ao_line);
 TBEditPathMarsh.Visible:=(newop=ao_Add_line)or(newop=ao_Edit_line);
 Frect_dwn:=false;
 setlength(Flength_arr,0);
 setlength(Fadd_line_arr,0);
 setlength(Freg_arr,0);
 Frect_p2:=false;
 Flastpoint:=-1;
 case newop of
  ao_movemap:  map.Cursor:=crDefault;
  ao_line:     map.Cursor:=2;
  ao_reg,ao_rect: map.Cursor:=crDrag;
  ao_Add_Point,ao_Add_Poly,ao_Add_Line,ao_edit_Line,ao_edit_poly: map.Cursor:=4;
 end;
 if (FCurrentOper=ao_edit_line)or(FCurrentOper=ao_edit_poly) then begin
   FEditMarkId:=-1;
   FLayerMapMarks.Redraw;
 end;
 FCurrentOper:=newop;
end;

procedure TFmain.OpenUrlInBrowser(URL: string);
begin
 ShellExecute(Handle, nil, PChar(URL), nil, nil, SW_RESTORE);
end;

//Обработка нажатий кнопоки и калесика
procedure TFmain.DoMessageEvent(var Msg: TMsg; var Handled: Boolean);
var
  z: integer;
  dWMB: integer;
  VZoom: Byte;
  VNewZoom: integer;
begin

 if Active then
  case Msg.message of
   WM_MOUSEWHEEL: if not FMapZoomAnimtion then
                 begin
                  MouseCursorPos:=FmoveTrue;
                  if GState.MouseWheelInv then z:=-1 else z:=1;
                  VZoom := GState.ViewState.GetCurrentZoom;
                  if Msg.wParam<0 then VNewZoom := VZoom-(1*z)
                                  else VNewZoom := VZoom+(1*z);
                  if VNewZoom < 0 then VNewZoom := 0;
                  zooming(VNewZoom, GState.ZoomingAtMousePos);
                 end;
   WM_KEYFIRST: begin
                 if (FdWhenMovingButton<35) then begin
                  inc(FdWhenMovingButton);
                 end;
                 dWMB:=trunc(Power(FdWhenMovingButton,1.5));
                 if Msg.wParam=VK_RIGHT then GState.ViewState.ChangeMapPixelByDelta(Point(dWMB, 0));
                 if Msg.wParam=VK_Left then GState.ViewState.ChangeMapPixelByDelta(Point(-dWMB, 0));
                 if Msg.wParam=VK_Down then GState.ViewState.ChangeMapPixelByDelta(Point(0, dWMB));
                 if Msg.wParam=VK_Up then GState.ViewState.ChangeMapPixelByDelta(Point(0, -dWMB));
                end;
   WM_KEYUP: begin
             FdWhenMovingButton:=5;
             if (Msg.wParam=VK_Delete)and(FCurrentOper=ao_line) then
               begin
                if length(Flength_arr)>0 then setlength(Flength_arr,length(Flength_arr)-1);
                TBEditPath.Visible:=(length(Flength_arr)>1);
                FLayerMapNal.DrawLineCalc(Flength_arr, FLenShow);
               end;
             if (Msg.wParam=VK_Delete)and(FCurrentOper=ao_reg) then
               begin
                if length(Freg_arr)>0 then setlength(Freg_arr,length(Freg_arr)-1);
                TBEditPath.Visible:=(length(Freg_arr)>1);
                FLayerMapNal.DrawReg(Freg_arr);
               end;
             if (Msg.wParam=VK_Delete)and(FCurrentOper in [ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly]) then
              if length(Fadd_line_arr)>0 then
               begin
                delfrompath(Flastpoint);
                TBEditPath.Visible:=(length(Fadd_line_arr)>1);
                FLayerMapNal.DrawNewPath(Fadd_line_arr, (FCurrentOper=ao_add_poly)or(FCurrentOper=ao_edit_poly), Flastpoint);
               end;
             if (Msg.wParam=VK_ESCAPE)and(FCurrentOper=ao_Reg) then
              if length(Freg_arr)=0 then TBmoveClick(self)
                                   else begin
                                         setlength(Freg_arr,0);
                                         TBEditPath.Visible:=(length(Freg_arr)>1);
                                         FLayerMapNal.DrawReg(Freg_arr);
                                        end;
             if (Msg.wParam=VK_ESCAPE)and(FCurrentOper=ao_line) then
              if length(Flength_arr)=0 then TBmoveClick(self)
                                      else begin
                                            setlength(Flength_arr,0);
                                            TBEditPath.Visible:=(length(Flength_arr)>1);
                                            FLayerMapNal.DrawLineCalc(Flength_arr, FLenShow);
                                           end;
             if (Msg.wParam=VK_ESCAPE)and(FCurrentOper=ao_rect) then
              begin
               if Frect_dwn then begin
                                 setalloperationfalse(ao_movemap);
                                 setalloperationfalse(ao_rect);
                                end
                           else setalloperationfalse(ao_movemap);
              end;
             if (Msg.wParam=VK_ESCAPE)and(FCurrentOper=ao_Add_Point) then setalloperationfalse(ao_movemap);
             if (Msg.wParam=VK_ESCAPE)and(FCurrentOper in [ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly]) then begin
               setalloperationfalse(ao_movemap)
             end;
             if (Msg.wParam=13)and(FCurrentOper in [ao_add_Poly,ao_add_line,ao_edit_Poly,ao_edit_line])and(length(Fadd_line_arr)>1) then begin
               TBEditPathSaveClick(Self);
             end;
            end;
  end;
end;

procedure TFmain.PrepareSelectionRect(Shift: TShiftState;
  var ASelectedLonLat: TExtendedRect);
var
  VZoomCurr: Byte;
  VSelectedPixels: TRect;
  bxy: Integer;
  VSelectedTiles: TRect;
  VTileGridZoom: byte;
  VSelectedRelative: TExtendedRect;
  z: TExtendedPoint;
  VConverter: ICoordConverter;
begin
  GState.ViewState.LockRead;
  try
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VConverter := GState.ViewState.GetCurrentCoordConverter;
  finally
    GState.ViewState.UnLockRead;
  end;
  VConverter.CheckLonLatRect(ASelectedLonLat);
  VSelectedPixels := VConverter.LonLatRect2PixelRect(ASelectedLonLat, VZoomCurr);

  if VSelectedPixels.Left > VSelectedPixels.Right then begin
    bxy := VSelectedPixels.Left;
    VSelectedPixels.Left := VSelectedPixels.Right;
    VSelectedPixels.Right := bxy;
  end;

  if VSelectedPixels.Top > VSelectedPixels.Bottom then begin
    bxy := VSelectedPixels.Top;
    VSelectedPixels.Top := VSelectedPixels.Bottom;
    VSelectedPixels.Bottom := bxy;
  end;

  if (ssCtrl in Shift) then begin
    if (GState.TileGridZoom = 0) or (GState.TileGridZoom = 99) then begin
      VSelectedTiles := VConverter.PixelRect2TileRect(VSelectedPixels, VZoomCurr);
      VSelectedPixels := VConverter.TileRect2PixelRect(VSelectedTiles, VZoomCurr);
    end else begin
      VTileGridZoom := GState.TileGridZoom - 1;
      VConverter.CheckZoom(VTileGridZoom);
      VSelectedRelative := VConverter.PixelRect2RelativeRect(VSelectedPixels, VZoomCurr);
      VSelectedTiles := VConverter.RelativeRect2TileRect(VSelectedRelative, VTileGridZoom);
      VSelectedRelative := VConverter.TileRect2RelativeRect(VSelectedTiles, VTileGridZoom);
      VSelectedPixels := VConverter.RelativeRect2PixelRect(VSelectedRelative, VZoomCurr);
    end;
  end;
  ASelectedLonLat := VConverter.PixelRect2LonLatRect(VSelectedPixels, VZoomCurr);

  if (ssShift in Shift)and(GState.GShScale>0) then begin
    z := GetGhBordersStepByScale(GState.GShScale);

    ASelectedLonLat.Left := ASelectedLonLat.Left-(round(ASelectedLonLat.Left*GSHprec) mod round(z.X*GSHprec))/GSHprec;
    if ASelectedLonLat.Left < 0 then ASelectedLonLat.Left := ASelectedLonLat.Left-z.X;

    ASelectedLonLat.Top := ASelectedLonLat.Top-(round(ASelectedLonLat.Top*GSHprec) mod round(z.Y*GSHprec))/GSHprec;
    if ASelectedLonLat.Top > 0 then ASelectedLonLat.Top := ASelectedLonLat.Top+z.Y;

    ASelectedLonLat.Right := ASelectedLonLat.Right-(round(ASelectedLonLat.Right*GSHprec) mod round(z.X*GSHprec))/GSHprec;
    if ASelectedLonLat.Right >= 0 then ASelectedLonLat.Right := ASelectedLonLat.Right+z.X;

    ASelectedLonLat.Bottom := ASelectedLonLat.Bottom-(round(ASelectedLonLat.Bottom*GSHprec) mod round(z.Y*GSHprec))/GSHprec;
    if ASelectedLonLat.Bottom <= 0 then ASelectedLonLat.Bottom := ASelectedLonLat.Bottom-z.Y;
  end;
end;

procedure TFmain.UpdateGPSsensors;
var
  s_len,n_len: string;
  sps: _SYSTEM_POWER_STATUS;
  VPoint: TExtendedPoint;
  VDist: Extended;
begin
 try
   //скорость
   TBXSensorSpeed.Caption:=RoundEx(GState.GPSpar.speed,2);
   //средняя скорость
   TBXSensorSpeedAvg.Caption:=RoundEx(GState.GPSpar.sspeed,2);
   //максимальная скорость
   TBXSensorSpeedMax.Caption:=RoundEx(GState.GPSpar.maxspeed,2);
   //высота
   TBXSensorAltitude.Caption:=RoundEx(GState.GPSpar.altitude,2);
   //пройденный путь
   s_len := DistToStrWithUnits(GState.GPSpar.len, GState.num_format);
   TBXOdometrNow.Caption:=s_len;
   //расстояние до метки
   if (LayerMapNavToMark<>nil)and(LayerMapNavToMark.Visible) then begin
     VPoint := GState.ViewState.GetCenterLonLat;
     VDist := GState.ViewState.GetCurrentCoordConverter.CalcDist(LayerMapNavToMark.GetMarkLonLat, VPoint);
     n_len:=DistToStrWithUnits(VDist, GState.num_format);
     TBXSensorLenToMark.Caption:=n_len;
   end else begin
     TBXSensorLenToMark.Caption:='-';
   end;
   //одометр
   TBXSensorOdometr.Caption:=DistToStrWithUnits(GState.GPSpar.Odometr, GState.num_format);
   TBXSensorOdometr2.Caption:=DistToStrWithUnits(GState.GPSpar.Odometr2, GState.num_format);
   //батарея
   GetSystemPowerStatus(sps);
   if sps.ACLineStatus=0 then begin
     case sps.BatteryFlag of
       128: TBXSensorBattary.Caption:='От сети';
         8: TBXSensorBattary.Caption:='Заряжается';
       else if sps.BatteryLifePercent=255 then TBXSensorBattary.Caption:='Неизвестно'
                                          else TBXSensorBattary.Caption:=inttostr(sps.BatteryLifePercent)+'%';
     end
   end
   else begin
     TBXSensorBattary.Caption:='От сети';
   end;
   //Азимут
   TBXSensorAzimut.Caption:=RoundEx(GState.GPSpar.azimut,2)+'°';
   //Сила сигнала, кол-во спутников
 except
 end;
end;

procedure TFmain.UpdateGPSSatellites;
var
  i,bar_width,bar_height,bar_x1,bar_dy:integer;
  VPosition: IGPSPosition;
  VSattelite: IGPSSatelliteInfo;
begin
   TBXSignalStrengthBar.Repaint;
   VPosition := GState.GPSpar.GPSModele.Position;
   if VPosition.Satellites.FixCount > 0 then begin
    with TBXSignalStrengthBar do begin
       Canvas.Lock;
       try
         bar_height:=42;
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

procedure TFmain.topos(LL:TExtendedPoint;zoom_:byte;draw:boolean);
begin
  GState.ViewState.LockWrite;
  GState.ViewState.ChangeZoomAndUnlock(zoom_, LL);
  if draw then begin
    FLayerGoto.ShowGotoIcon(LL);
  end;
end;

procedure TFmain.generate_im;
begin
  generate_im(FnilLastLoad, '');
end;

procedure TFmain.generate_im(LastLoad:TLastLoad;err:string);
var
  ts2,ts3,fr:int64;
  VSelectionRect: TExtendedRect;
begin
  if not Enabled then Exit;
  if FMapMoving then Exit;
  if FMapZoomAnimtion then Exit;

  QueryPerformanceCounter(ts2);

  if (lastload.use)and(err<>'') then begin
    FShowErrorLayer.ShowError(lastload.TilePos, lastload.Zoom, lastload.mt, err);
  end else begin
    FShowErrorLayer.Visible := False;
  end;

  FMainLayer.Redraw;
  FLayerScaleLine.Redraw;
  FLayerMapMarks.Redraw;
  FWikiLayer.Redraw;

  if not(lastload.use) then begin
    case FCurrentOper of
      ao_line: begin
        TBEditPath.Visible:=(length(Flength_arr)>1);
        FLayerMapNal.DrawLineCalc(Flength_arr, FLenShow);
      end;
      ao_reg: begin
        TBEditPath.Visible:=(length(Freg_arr)>1);
        FLayerMapNal.DrawReg(Freg_arr);
      end;
      ao_rect: begin
        VSelectionRect := FSelectionRect;
        PrepareSelectionRect([], VSelectionRect);
        FLayerMapNal.DrawSelectionRect(VSelectionRect);
      end;
      ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly: begin
        TBEditPath.Visible:=(length(Fadd_line_arr)>1);
        FLayerMapNal.DrawNewPath(Fadd_line_arr, (FCurrentOper=ao_add_poly)or(FCurrentOper=ao_edit_poly), Flastpoint);
      end;
    end;

    if GState.GPSpar.GPSModele.IsConnected then begin
       FLayerMapGPS.Redraw;
       UpdateGPSsensors;
    end;
    try
      FLayerMapMarks.Visible := GState.show_point <> mshNone;
    except
    end;
  end;
  FLayerStatBar.Redraw;
  QueryPerformanceCounter(ts3);
  QueryPerformanceFrequency(fr);
  Label1.caption :=FloatToStr((ts3-ts2)/(fr/1000));
end;

constructor TFmain.Create(AOwner: TComponent);
begin
  inherited;
  FLayersList := TWindowLayerBasicList.Create;
end;

procedure TFmain.CreateMapUI;
var
  i,j: integer;
  VMapType: TMapType;

  MainToolbarItem: TTBXItem; //Пункт списка в главном тулбаре
  MainToolbarSubMenuItem: TTBXSubmenuItem; //Подпункт списка в главном тулбаре
  TBFillingItem: TTBXItem; //Пункт главного меню Вид/Карта заполнения/Формировать для

  NLayerParamsItem: TTBXItem; //Пункт гланого меню Параметры/Параметры слоя
  NDwnItem: TMenuItem; //Пункт контекстного меню Загрузить тайл слоя
  NDelItem: TMenuItem; //Пункт контекстного меню Удалить тайл слоя

  VIcon18Index: Integer;
begin
  TBSMB.Clear;
  NSMB.Clear;
  ldm.Clear;
  dlm.Clear;
  NLayerParams.Clear;

  FMainToolbarItemList.Clear;
  FMainToolbarSubMenuItemList.Clear;
  FTBFillingItemList.Clear;
  FNLayerParamsItemList.Clear;
  FNDwnItemList.Clear;
  FNDelItemList.Clear;

  for i:=0 to NLayerSel.Count-1 do NLayerSel.Items[0].Free;
  for i:=0 to TBLayerSel.Count-1 do TBLayerSel.Items[0].Free;
  for i:=0 to TBFillingTypeMap.Count-2 do TBFillingTypeMap.Items[1].Free;

  if length(GState.MapType)>0 then begin
    for i:=0 to length(GState.MapType)-1 do begin
      VMapType := GState.MapType[i];
      VIcon18Index := GState.MapTypeIcons18List.GetIconIndexByGUID(VMapType.GUID);
      With VMapType do begin
        MainToolbarItem:=TTBXItem.Create(TBSMB);
        FMainToolbarItemList.Add(VMapType.GUID, MainToolbarItem);
        if ParentSubMenu='' then begin
          if asLayer then begin
            TBLayerSel.Add(MainToolbarItem);
          end else begin
            TBSMB.Add(MainToolbarItem);
          end;
        end else begin
          j:=0;
          While GState.MapType[j].ParentSubMenu<>ParentSubMenu do inc(j);
          if j=i then begin
            MainToolbarSubMenuItem:=TTBXSubmenuItem.Create(TBSMB);
            FMainToolbarSubMenuItemList.Add(VMapType.GUID, MainToolbarSubMenuItem);
            MainToolbarSubMenuItem.caption:=ParentSubMenu;
            MainToolbarSubMenuItem.Images:= GState.MapTypeIcons18List.GetImageList;
            if asLayer then begin
              TBLayerSel.Add(MainToolbarSubMenuItem)
            end else begin
              TBSMB.Add(MainToolbarSubMenuItem);
            end;
          end;
          MainToolbarSubMenuItem := TTBXSubmenuItem(FMainToolbarSubMenuItemList.GetByGUID(GState.MapType[j].GUID));
          MainToolbarSubMenuItem.Add(MainToolbarItem);
        end;
        MainToolbarItem.Name:='TBMapN'+inttostr(id);
        MainToolbarItem.ShortCut:=HotKey;
        MainToolbarItem.ImageIndex:= VIcon18Index;
        MainToolbarItem.Caption:=name;
        MainToolbarItem.OnAdjustFont:=AdjustFont;
        MainToolbarItem.OnClick:=TBmap1Click;

        TBFillingItem:=TTBXItem.Create(TBFillingTypeMap);
        FTBFillingItemList.Add(VMapType.GUID, TBFillingItem);
        TBFillingItem.name:='TBMapFM'+inttostr(id);
        TBFillingItem.ImageIndex:=VIcon18Index;
        TBFillingItem.Caption:=name;
        TBFillingItem.OnAdjustFont:=AdjustFont;
        TBFillingItem.OnClick:=TBfillMapAsMainClick;
        TBFillingTypeMap.Add(TBFillingItem);

        if asLayer then begin
          NDwnItem:=TMenuItem.Create(nil);
          FNDwnItemList.Add(VMapType.GUID, NDwnItem);
          NDwnItem.Caption:=name;
          NDwnItem.ImageIndex:=VIcon18Index;
          NDwnItem.OnClick:=N21Click;
          ldm.Add(NDwnItem);
          NDelItem:=TMenuItem.Create(nil);
          FNDelItemList.Add(VMapType.GUID, NDelItem);
          NDelItem.Caption:=name;
          NDelItem.ImageIndex:=VIcon18Index;
          NDelItem.OnClick:=NDelClick;
          dlm.Add(NDelItem);
          NLayerParamsItem:=TTBXItem.Create(NLayerParams);
          FNLayerParamsItemList.Add(VMapType.GUID, NLayerParamsItem);
          NLayerParamsItem.Caption:=name;
          NLayerParamsItem.ImageIndex:=VIcon18Index;
          NLayerParamsItem.OnClick:=NMapParamsClick;
          NLayerParams.Add(NLayerParamsItem);
          NDwnItem.Tag:=longint(VMapType);
          NDelItem.Tag:=longint(VMapType);
          NLayerParamsItem.Tag:=longint(VMapType);
        end;
        if (asLayer)and(GState.ViewState.IsHybrGUIDSelected(GUID)) then begin
          MainToolbarItem.Checked:=true;
        end;
        if separator then begin
          MainToolbarItem.Parent.Add(TTBXSeparatorItem.Create(TBSMB));
          TBFillingItem.Parent.Add(TTBXSeparatorItem.Create(TBFillingItem.Parent));
        end;
        MainToolbarItem.Tag:=Longint(VMapType);
        TBFillingItem.Tag:=Longint(VMapType);
      end;
    end;
  end;
  VMapType := GState.ViewState.GetCurrentMap;
  MainToolbarItem := TTBXItem(FMainToolbarItemList.GetByGUID(VMapType.GUID));
  MainToolbarItem.Checked:=true;

  TBSMB.ImageIndex := GState.MapTypeIcons24List.GetIconIndexByGUID(VMapType.GUID);
  if GState.Showmapname then begin
    TBSMB.Caption:= VMapType.Name;
  end else begin
    TBSMB.Caption:='';
  end;
end;

function TFmain.GetIgnoredMenuItemsList: TList;
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
end;

function TFmain.GetMarksIterator(AZoom: Byte; ARect: TExtendedRect;
  AShowType: TMarksShowType): TMarksIteratorBase;
var
  VIgnoredID: Integer;
begin
  if (FCurrentOper = ao_edit_line) or (FCurrentOper = ao_edit_poly) then begin
    VIgnoredID := FEditMarkId;
  end else begin
    VIgnoredID := -1;
  end;
  Result := GState.MarksDb.GetMarksIteratorWithIgnore(AZoom, ARect, AShowType, VIgnoredID);
end;

procedure TFmain.FormActivate(Sender: TObject);
var
  i:integer;
  param:string;
  MainWindowMaximized: Boolean;
  VGUID: TGUID;
  VGUIDString: string;
  VScreenCenterPos: TPoint;
  VZoom: Byte;
  VLonLat: TExtendedPoint;
  VConverter: ICoordConverter;
  VMapType: TMapType;
begin
  GState.ScreenSize := Point(Screen.Width, Screen.Height);
  if not ProgramStart then exit;
  VConverter := GState.MapType[0].GeoConvert;
  VZoom := GState.MainIni.ReadInteger('POSITION','zoom_size',1) - 1;
  VConverter.CheckZoom(VZoom);
  VScreenCenterPos.X := VConverter.PixelsAtZoom(VZoom) div 2 + 1;
  VScreenCenterPos.Y := VScreenCenterPos.X;
  VScreenCenterPos := Point(
    GState.MainIni.ReadInteger('POSITION','x',VScreenCenterPos.X),
    GState.MainIni.ReadInteger('POSITION','y',VScreenCenterPos.Y)
  );

  GState.InitViewState(GState.MapType[0], VZoom - 1, VScreenCenterPos, Point(map.Width, map.Height));

  Enabled:=false;
  try
    TBSMB.Images := GState.MapTypeIcons24List.GetImageList;
    TBSMB.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
    TBLayerSel.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
    TBFillingTypeMap.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
    NSMB.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
    NLayerSel.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
    NLayerParams.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
    ldm.SubMenuImages := GState.MapTypeIcons18List.GetImageList;
    dlm.SubMenuImages := GState.MapTypeIcons18List.GetImageList;

    FMainToolbarItemList := TGUIDObjectList.Create(False);
    FMainToolbarSubMenuItemList := TGUIDObjectList.Create(False);
    FTBFillingItemList := TGUIDObjectList.Create(False);
    FNLayerParamsItemList := TGUIDObjectList.Create(False);
    FNDwnItemList := TGUIDObjectList.Create(False);
    FNDelItemList := TGUIDObjectList.Create(False);

    RectWindow := Types.Rect(0, 0, 0, 0);
    FdWhenMovingButton := 5;
    MainWindowMaximized:=GState.MainIni.Readbool('VIEW','Maximized',true);
    TBFullSize.Checked:=GState.FullScrean;
    if GState.FullScrean then begin
      TBFullSizeClick(TBFullSize);
    end else if MainWindowMaximized then begin
      WindowState:=wsMaximized
    end else begin
      Self.SetBounds(
      GState.MainIni.ReadInteger('VIEW','FLeft',Left),
      GState.MainIni.ReadInteger('VIEW','FTop',Top),
      GState.MainIni.ReadInteger('VIEW','FWidth',Width),
      GState.MainIni.ReadInteger('VIEW','FHeight',Height)
      )
    end;

    Fmovepoint:=0;

    GState.MarksDb.LoadMarksFromFile;
    GState.MarksDb.LoadCategoriesFromFile;
    Enabled:=true;
    FnilLastLoad.use:=false;
    Application.OnMessage := DoMessageEvent;
    Application.HelpFile := ExtractFilePath(Application.ExeName)+'help.hlp';
    FLenShow:=true;
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

    NGoToCur.Checked := GState.ZoomingAtMousePos;
    lock_toolbars:=GState.MainIni.ReadBool('PANEL','lock_toolbars',false);

    Label1.Visible:=GState.MainIni.ReadBool('VIEW','time_rendering',false);


    FMainLayer := TMapMainLayer.Create(map, GState.ViewState);
    FLayersList.Add(FMainLayer);
    FWikiLayer := TWikiLayer.Create(map, GState.ViewState);
    FLayersList.Add(FWikiLayer);
    FLayerFillingMap:=TMapFillingLayer.create(map, GState.ViewState);
    FLayersList.Add(FLayerFillingMap);
    FLayerMapMarks:= TMapMarksLayer.Create(map, GState.ViewState);
    FLayersList.Add(FLayerMapMarks);
    FLayerMapGPS:= TMapGPSLayer.Create(map, GState.ViewState);
    FLayersList.Add(FLayerMapGPS);
    FLayerSelection := TSelectionLayer.Create(map, GState.ViewState);
    FLayersList.Add(FLayerSelection);
    FLayerMapNal:=TMapNalLayer.Create(map, GState.ViewState);
    FLayersList.Add(FLayerMapNal);
    FLayerGoto := TGotoLayer.Create(map, GState.ViewState);
    FLayersList.Add(FLayerGoto);
    LayerMapNavToMark := TNavToMarkLayer.Create(map, GState.ViewState);
    FLayersList.Add(LayerMapNavToMark);
    FShowErrorLayer := TTileErrorInfoLayer.Create(map, GState.ViewState);
    FLayersList.Add(FShowErrorLayer);
    FLayerMapScale := TCenterScale.Create(map, GState.ViewState);
    FLayersList.Add(FLayerMapScale);
    FLayerScaleLine := TLayerScaleLine.Create(map, GState.ViewState);
    FLayersList.Add(FLayerScaleLine);
    FLayerStatBar:=TLayerStatBar.Create(map, GState.ViewState);
    FLayersList.Add(FLayerStatBar);
    FLayerMiniMap := TMiniMapLayer.Create(map, GState.ViewState);
    FLayersList.Add(FLayerMiniMap);

    CreateMapUI;
    FSettings.InitMapsList;

    NGShScale10000.Checked := GState.GShScale = 10000;
    NGShScale25000.Checked := GState.GShScale = 25000;
    NGShScale50000.Checked := GState.GShScale = 50000;
    NGShScale100000.Checked := GState.GShScale = 100000;
    NGShScale200000.Checked := GState.GShScale = 200000;
    NGShScale500000.Checked := GState.GShScale = 500000;
    NGShScale1000000.Checked := GState.GShScale = 1000000;
    NGShScale0.Checked := GState.GShScale = 0;

    Ninvertcolor.Checked:=GState.InvertColor;
//    TBGPSconn.Checked := GState.GPSpar.GPS_enab;
//    if GState.GPSpar.GPS_enab then TBGPSconnClick(TBGPSconn);
    TBGPSPath.Checked:=GState.GPSpar.GPS_ShowPath;
    tbitmGPSTrackShow.Checked:=GState.GPSpar.GPS_ShowPath;
    TBGPSToPoint.Checked:=GState.GPSpar.GPS_MapMove;
    tbitmGPSCenterMap.Checked:=GState.GPSpar.GPS_MapMove;
    TBGPSToPointCenter.Checked:=GState.GPSpar.GPS_MapMoveCentered;
    tbitmGPSToPointCenter.Checked:=GState.GPSpar.GPS_MapMoveCentered;
    Nbackload.Checked:=GState.UsePrevZoom;
    NbackloadLayer.Checked:=GState.UsePrevZoomLayer;
    Nanimate.Checked:=GState.AnimateZoom;

    if not(FileExists(GState.MainConfigFileName)) then begin
      TBEditPath.Floating:=true;
      TBEditPath.MoveOnScreen(true);
      TBEditPath.FloatingPosition:=Point(Left+map.Left+30,Top+map.Top+70);
    end;

    NMainToolBarShow.Checked:=TBMainToolBar.Visible;
    NZoomToolBarShow.Checked:=ZoomToolBar.Visible;
    NsrcToolBarShow.Checked:=SrcToolbar.Visible;
    NGPSToolBarShow.Checked:=GPSToolBar.Visible;
    NMarksBarShow.Checked:=TBMarksToolBar.Visible;

    TBFullSize.Checked:=GState.FullScrean;

    map.Color:=GState.BGround;

    FMapPosChangeListener := TChangePosListenerOfMainForm.Create(Self);
    GState.ViewState.PosChangeNotifier.Add(FMapPosChangeListener);

    FMainMapChangeListener := TMainMapChangeListenerOfMainForm.Create(Self);
    GState.ViewState.MapChangeNotifier.Add(FMainMapChangeListener);

    FHybrChangeListener := THybrChangeListenerOfMainForm.Create(Self);
    GState.ViewState.HybrChangeNotifier.Add(FHybrChangeListener);

    FMapLayersVsibleChangeListener := TMapLayersVisibleChange.Create(Self);
    FLayerStatBar.VisibleChangeNotifier.Add(FMapLayersVsibleChangeListener);
    FLayerMiniMap.VisibleChangeNotifier.Add(FMapLayersVsibleChangeListener);
    FLayerScaleLine.VisibleChangeNotifier.Add(FMapLayersVsibleChangeListener);
    FMainLayer.UseDownloadChangeNotifier.Add(FMapLayersVsibleChangeListener);
    FLayerFillingMap.SourceMapChangeNotifier.Add(FMapLayersVsibleChangeListener);

    FGPSConntectListener := TGPSConnected.Create(Self);
    GState.GPSpar.GPSModele.ConnectNotifier.Add(FGPSConntectListener);
    FGPSDisconntectListener := TGPSDisonnected.Create(Self);
    GState.GPSpar.GPSModele.DisconnectNotifier.Add(FGPSDisconntectListener);
    FGPSConntectErrorListener := TGPSConnectError.Create(Self);
    GState.GPSpar.GPSModele.ConnectErrorNotifier.Add(FGPSConntectErrorListener);
    FGPSTimeOutListener := TGPSTimeOut.Create(Self);
    GState.GPSpar.GPSModele.TimeOutNotifier.Add(FGPSTimeOutListener);
    FGPSReceiveListener := TGPSReceive.Create(Self);
    GState.GPSpar.GPSModele.DataReciveNotifier.Add(FGPSReceiveListener);

    GState.ViewState.LoadViewPortState(GState.MainConfigProvider);

    FLayersList.LoadConfig(GState.MainConfigProvider);
    FLayersList.StartThreads;
    ProgramStart:=false;

    GState.ViewState.LockWrite;
    GState.ViewState.ChangeZoomAndUnlock(VZoom, VScreenCenterPos);

    if ParamCount > 1 then begin
      try
        param:=paramstr(1);
        if param<>'' then begin
          VGUID := StringToGUID(param);
          VMapType := GState.GetMapFromID(VGUID);
          if VMapType <> nil then begin
            GState.ViewState.ChangeMainMapAtCurrentPoint(VMapType);
          end;
        end;
        if  (paramstr(2)<>'') and (paramstr(3)<>'')and(paramstr(4)<>'') then begin
          GState.ViewState.LockWrite;
          VZoom := strtoint(paramstr(2)) - 1;
          GState.ViewState.GetCurrentCoordConverter.CheckZoom(VZoom);
          VLonLat.X := str2r(paramstr(3));
          VLonLat.Y := str2r(paramstr(4));
          GState.ViewState.GetCurrentCoordConverter.CheckLonLatPos(VLonLat);
          GState.ViewState.ChangeZoomAndUnlock(VZoom, VLonLat);
        end else if paramstr(2)<>'' then begin
          VZoom := strtoint(paramstr(2)) - 1;
          GState.ViewState.ChangeZoomWithFreezeAtCenter(VZoom);
        end;
      except
      end;
    end;
    InitSearchers;
    FMapMoving:=false;

    SetProxy;


    case GState.SrchType of
      stGoogle:  TBXSelectYandexSrchClick(TBXSelectGoogleSrch);
      stYandex: TBXSelectYandexSrchClick(TBXSelectYandexSrch);
    end;

    if GState.WebReportToAuthor then begin
      WebBrowser1.Navigate('http://sasgis.ru/stat/index.html');
    end;

    GState.ViewState.ChangeViewSize(Point(map.Width, map.Height));

    GState.StartThreads;
    FMainLayer.Visible := True;
    FLayerMapMarks.Visible := GState.show_point <> mshNone;
  finally
    Enabled:=true;
    map.SetFocus;
    if (FLogo<>nil)and(FLogo.Visible) then FLogo.Timer1.Enabled:=true;
  end;
  TBXMainMenu.ProcessShortCuts:=true;
end;


procedure TFmain.zooming(ANewZoom:byte;move:boolean);
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
    Scale: Extended;
    VZoom: Byte;
begin
  TBZoom_Out.Enabled := False;
  TBZoomIn.Enabled := False;
  NZoomIn.Enabled := False;
  NZoomOut.Enabled := False;
  RxSlider1.Value:=ANewZoom;
  VZoom := GState.ViewState.GetCurrentZoom;
  if (FMapZoomAnimtion)or(FMapMoving)or(ANewZoom>23) then exit;
  FMapZoomAnimtion:=True;

  if (abs(ANewZoom-VZoom)=1)and(GState.AnimateZoom) then begin
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
        GState.ViewState.ScaleTo(Scale, MouseCursorPos);
        FMainLayer.ScaleTo(Scale, MouseCursorPos);
        FLayerMapMarks.ScaleTo(Scale, MouseCursorPos);
        FLayerMapGPS.ScaleTo(Scale, MouseCursorPos);
        FWikiLayer.ScaleTo(Scale, MouseCursorPos);
        FLayerFillingMap.ScaleTo(Scale, MouseCursorPos);
        FLayerMapNal.ScaleTo(Scale, MouseCursorPos);
        FLayerGoto.ScaleTo(Scale, MouseCursorPos);
        FShowErrorLayer.ScaleTo(Scale, MouseCursorPos);
        LayerMapNavToMark.ScaleTo(Scale, MouseCursorPos);
      end else begin
        GState.ViewState.ScaleTo(Scale);
        FMainLayer.ScaleTo(Scale);
        FLayerMapMarks.ScaleTo(Scale);
        FLayerMapGPS.ScaleTo(Scale);
        FWikiLayer.ScaleTo(Scale);
        FLayerFillingMap.ScaleTo(Scale);
        FLayerMapNal.ScaleTo(Scale);
        FLayerGoto.ScaleTo(Scale);
        FShowErrorLayer.ScaleTo(Scale);
        LayerMapNavToMark.ScaleTo(Scale);
      end;
      finally
        map.EndUpdate;
        map.Invalidate;
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
    GState.ViewState.ChangeZoomWithFreezeAtVisualPoint(ANewZoom, MouseCursorPos);
  end else begin
    GState.ViewState.ChangeZoomWithFreezeAtCenter(ANewZoom);
  end;
  FMapZoomAnimtion:=False;
end;

procedure TFmain.NzoomInClick(Sender: TObject);
begin
 zooming(GState.ViewState.GetCurrentZoom + 1, false);
end;

procedure TFmain.NZoomOutClick(Sender: TObject);
begin
 zooming(GState.ViewState.GetCurrentZoom - 1, false);
end;

procedure TFmain.FormCreate(Sender: TObject);
begin
 Application.Title:=Caption;
 TBConfigProviderLoadPositions(Self, GState.MainConfigProvider.GetSubItem('PANEL'));
 TBEditPath.Visible:=false;
 Caption:=Caption+' '+SASVersion;
 ProgramStart:=true;
end;

procedure TFmain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i:integer;
begin
  ProgramClose:=true;
  GState.GPSpar.GPSModele.ConnectNotifier.Remove(FGPSConntectListener);
  GState.GPSpar.GPSModele.DisconnectNotifier.Remove(FGPSDisconntectListener);
  GState.GPSpar.GPSModele.ConnectErrorNotifier.Remove(FGPSConntectErrorListener);
  GState.GPSpar.GPSModele.TimeOutNotifier.Remove(FGPSTimeOutListener);
  GState.GPSpar.GPSModele.DataReciveNotifier.Remove(FGPSReceiveListener);
  if GState.ViewState <> nil then begin
    GState.ViewState.PosChangeNotifier.Remove(FMapPosChangeListener);
    GState.ViewState.MapChangeNotifier.Remove(FMainMapChangeListener);
    GState.ViewState.HybrChangeNotifier.Remove(FHybrChangeListener);
  end;
  FLayerStatBar.VisibleChangeNotifier.Remove(FMapLayersVsibleChangeListener);
  FLayerMiniMap.VisibleChangeNotifier.Remove(FMapLayersVsibleChangeListener);
  FLayerScaleLine.VisibleChangeNotifier.Remove(FMapLayersVsibleChangeListener);
  FMainLayer.UseDownloadChangeNotifier.Remove(FMapLayersVsibleChangeListener);
  FLayerFillingMap.SourceMapChangeNotifier.Remove(FMapLayersVsibleChangeListener);
  FMapLayersVsibleChangeListener := nil;
  //останавливаем GPS
  GState.SendTerminateToThreads;
  for i := 0 to Screen.FormCount - 1 do begin
    if (Screen.Forms[i]<>Application.MainForm)and(Screen.Forms[i].Visible) then begin
      Screen.Forms[i].Close;
    end;
  end;
  FLayersList.SendTerminateToThreads;
  Application.ProcessMessages;
  if length(GState.MapType)<>0 then FSettings.Save(GState.MainConfigProvider);
  FSearchPresenter := nil;
  FGoogleGeoCoder := nil;
  FYandexGeoCoder := nil;
  FMapPosChangeListener := nil;
  FMainMapChangeListener := nil;
  FHybrChangeListener := nil;
  Application.ProcessMessages;
  FreeAndNil(FLayersList);
  FreeAndNil(FShortCutManager);
  FMainToolbarItemList := nil;
  FMainToolbarSubMenuItemList := nil;
  FTBFillingItemList := nil;
  FNLayerParamsItemList := nil;
  FNDwnItemList := nil;
  FNDelItemList := nil;
end;

procedure TFmain.TBmoveClick(Sender: TObject);
begin
 setalloperationfalse(ao_movemap);
end;

procedure TFmain.TBZoom_outClick(Sender: TObject);
begin
 zooming(GState.ViewState.GetCurrentZoom - 1, false);
end;

procedure TFmain.TBZoomInClick(Sender: TObject);
begin
 zooming(GState.ViewState.GetCurrentZoom + 1, false);
end;

procedure TFmain.WMGetMinMaxInfo(var msg:TWMGetMinMaxInfo);
begin
 inherited;
 with msg.MinMaxInfo^.ptMaxTrackSize do begin
  X:=GetDeviceCaps(Canvas.handle,HORZRES)+(Width-ClientWidth);
  Y:=GetDeviceCaps(Canvas.handle,VERTRES)+(Height-ClientHeight);
 end;
end;

procedure TFmain.TBFullSizeClick(Sender:TObject);
begin
 NFoolSize.Checked:=TBFullSize.Checked;
 TBexit.Visible:=TBFullSize.Checked;
 GState.FullScrean:=TBFullSize.Checked;
 TBDock.Parent:=Self;
 TBDockLeft.Parent:=Self;
 TBDockBottom.Parent:=Self;
 TBDockRight.Parent:=Self;
 TBDock.Visible:=not(TBFullSize.Checked);
 TBDockLeft.Visible:=not(TBFullSize.Checked);
 TBDockBottom.Visible:=not(TBFullSize.Checked);
 TBDockRight.Visible:=not(TBFullSize.Checked);
 if TBFullSize.Checked then
  begin
   RectWindow:=Self.BoundsRect;
   SetBounds(Left-ClientOrigin.X,Top-ClientOrigin.Y,GetDeviceCaps(Canvas.handle,
   HORZRES)+(Width-ClientWidth),GetDeviceCaps(Canvas.handle,VERTRES)+(Height-ClientHeight));
  end
  else Self.BoundsRect:=RectWindow;
end;

procedure TextToWebBrowser(Text: string; var WB: TEmbeddedWB);
var
  Document: IHTMLDocument2;
  V: OleVariant;
begin
  if WB.Document = nil then WB.Navigate('about:blank');
  while WB.Document = nil do
    Application.ProcessMessages;
  Document := WB.Document as IHtmlDocument2;
  try
   V:=VarArrayCreate([0, 0], varVariant);
   V[0]:=Text;
   Document.Write(PSafeArray(TVarData(v).VArray));
  finally
   Document.Close;
  end;
end;

procedure TFmain.ZoomToolBarDockChanging(Sender: TObject; Floating: Boolean; DockingTo: TTBDock);
begin
 if (DockingTo=TBDockLeft)or(DockingTo=TBDockRight)
   then begin
         RxSlider1.Orientation:=RxSlider.soVertical;
         TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoom_out),4);
         TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoomin),0);
        end
   else begin
         RxSlider1.Orientation:=RxSlider.soHorizontal;
         TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoom_out),0);
         TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoomin),4);
        end
end;


procedure TFmain.RxSlider1Change(Sender: TObject);
begin
 labZoom.Caption:=inttostr(RxSlider1.Value+1)+'x';
end;

procedure TFmain.RxSlider1Changed(Sender: TObject);
begin
 zooming(RxSlider1.Value, false);
 SetFocusedControl(map);
end;

procedure TFmain.NMainToolBarShowClick(Sender: TObject);
begin
 TBMainToolBar.Visible:=NMainToolBarShow.Checked;
end;

procedure TFmain.NGPSToolBarShowClick(Sender: TObject);
begin
 GPSToolBar.Visible:=NGPSToolBarShow.Checked;
end;

procedure TFmain.NZoomToolBarShowClick(Sender: TObject);
begin
 ZoomToolBar.Visible:=NZoomToolBarShow.Checked;
end;

procedure TFmain.NsrcToolBarShowClick(Sender: TObject);
begin
 SrcToolbar.Visible:=NsrcToolBarShow.Checked;
end;

procedure TFmain.NCalcRastClick(Sender: TObject);
begin
 TBCalcRas.Checked:=true;
 TBCalcRasClick(self);
end;

procedure TFmain.NFoolSizeClick(Sender: TObject);
begin
 TBFullSize.Checked:=NFoolSize.Checked;
 TBFullSizeClick(Sender);
end;

procedure TFmain.N6Click(Sender: TObject);
begin
 close;
end;

procedure TFmain.TBmap1Click(Sender: TObject);
var
  VMapType: TMapType;
begin
  VMapType := TMapType(TTBXItem(sender).tag);
  if not(VMapType.asLayer) then begin
    if (VMapType.showinfo)and(VMapType.MapInfo<>'') then begin
      ShowMessage(VMapType.MapInfo);
      VMapType.showinfo:=false;
    end;
    GState.ViewState.ChangeMainMapAtCurrentPoint(VMapType);
  end else begin
    GState.ViewState.ChangeSelectHybrByGUID(VMapType.GUID);
  end;
end;

procedure TFmain.N8Click(Sender: TObject);
begin
 fsettings.ShowModal;
end;

procedure TFmain.NbackloadClick(Sender: TObject);
begin
 GState.UsePrevZoom := Nbackload.Checked;
 generate_im;
end;

procedure TFmain.NbackloadLayerClick(Sender: TObject);
begin
 GState.UsePrevZoomLayer := NbackloadLayer.Checked;
 generate_im;
end;

procedure TFmain.NaddPointClick(Sender: TObject);
begin
  if AddNewPointModal(GState.ViewState.VisiblePixel2LonLat(FMouseUpPoint)) then begin
    setalloperationfalse(ao_movemap);
    generate_im;
  end;
end;

procedure TFmain.N20Click(Sender: TObject);
var
  btm:TBitmap32;
  btm1:TBitmap;
  VPoint: TPoint;
  VZoomCurr: Byte;
  VConverter: ICoordConverter;
  VMap: TMapType;
begin
  GState.ViewState.LockRead;
  try
    VPoint := GState.ViewState.VisiblePixel2MapPixel(FMouseDownPoint);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VMap := GState.ViewState.GetCurrentMap;
    VConverter := GState.ViewState.GetCurrentCoordConverter;
  finally
    GState.ViewState.UnLockRead;
  end;
  VConverter.CheckPixelPosStrict(VPoint, VZoomCurr, True);
  VPoint := VConverter.PixelPos2TilePos(VPoint, VZoomCurr);
  btm:=TBitmap32.Create;
  try
    if VMap.LoadTile(btm, VPoint, VZoomCurr, false) then begin
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

procedure TFmain.N30Click(Sender: TObject);
var
  ll:TExtendedPoint;
begin
  ll := GState.ViewState.VisiblePixel2LonLat(FMouseDownPoint);
  if GState.FirstLat then CopyStringToClipboard(lat2str(ll.y, GState.llStrType)+' '+lon2str(ll.x, GState.llStrType))
             else CopyStringToClipboard(lon2str(ll.x, GState.llStrType)+' '+lat2str(ll.y, GState.llStrType));
end;

procedure TFmain.N15Click(Sender: TObject);
var
  VPoint: TPoint;
  VZoomCurr: Byte;
  VConverter: ICoordConverter;
  VMap: TMapType;
begin
  GState.ViewState.LockRead;
  try
    VPoint := GState.ViewState.VisiblePixel2MapPixel(FMouseDownPoint);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VMap := GState.ViewState.GetCurrentMap;
    VConverter := GState.ViewState.GetCurrentCoordConverter;
  finally
    GState.ViewState.UnLockRead;
  end;
  if VMap.TileStorage.GetIsStoreFileCache then begin
    VConverter.CheckPixelPosStrict(VPoint, VZoomCurr, True);
    VPoint := VConverter.PixelPos2TilePos(VPoint, VZoomCurr);
   // Копирование в имени файла в буффер обмена. Заменить на обобщенное имя тайла.
   CopyStringToClipboard(VMap.GetTileFileName(VPoint, VZoomCurr));
  end else begin
    ShowMessage('Это не тайловый кеш, невозможно получить имя файла с тайлом.');
  end;
end;

procedure TFmain.N21Click(Sender: TObject);
var
  path:string;
  VMapType:TMapType;
  VMapMain: TMapType;
  VLoadPoint: TPoint;
  VZoomCurr: Byte;
  VPoint: TPoint;
begin
  VMapType := nil;
  if TMenuItem(sender).Tag<>0 then begin
    VMapType := TMapType(TMenuItem(sender).Tag);
  end;
  GState.ViewState.LockRead;
  try
    VMapMain := GState.ViewState.GetCurrentMap;
    if VMapType = nil then begin
      VMapType := GState.ViewState.GetCurrentMap;
    end;
    VPoint := GState.ViewState.VisiblePixel2MapPixel(FMouseUpPoint);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
  finally
    GState.ViewState.UnLockRead;
  end;

  VMapMain.GeoConvert.CheckPixelPosStrict(VPoint, VZoomCurr, False);
  VLoadPoint := VMapMain.GeoConvert.PixelPos2OtherMap(VPoint, VZoomCurr, VMapType.GeoConvert);
  VLoadPoint := VMapType.GeoConvert.PixelPos2TilePos(VLoadPoint, VZoomCurr);
  path := VMapType.GetTileShowName(VLoadPoint, VZoomCurr);

  if ((not(VMapType.tileExists(VLoadPoint, VZoomCurr)))or
      (MessageBox(handle,pchar(Format(SAS_MSG_FileExists, [path])),pchar(SAS_MSG_coution),36)=IDYES))
  then begin
    TTileDownloaderUIOneTile.Create(VLoadPoint, VZoomCurr, VMapType);
  end;
end;

procedure TFmain.N11Click(Sender: TObject);
//var
//  WindirP: PChar;
//  btm_ex:TBitmap;
//  path: string;
begin
  ShowMessage('Временно не работает');
//  WinDirP:=StrAlloc(MAX_PATH);
//  GetWindowsDirectory(WinDirP, MAX_PATH);
//  path := IncludeTrailingPathDelimiter(StrPas(WinDirP))+'SASwallpaper.bmp';
//  btm_ex:=TBitmap.Create;
//  try
//    btm_ex.Assign(MainLayerMap.bitmap);
//    btm_ex.SaveToFile(path);
//  finally
//    btm_ex.Free;
//  end;
//  with TRegIniFile.Create('Control Panel') do
//   begin
//    WriteString('desktop', 'Wallpaper', IncludeTrailingPathDelimiter(StrPas(WinDirP))+'SASwallpaper.bmp');
//    WriteString('desktop', 'TileWallpaper', '0');
//    free;
//   end;
//  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE);
end;

procedure TFmain.NopendirClick(Sender: TObject);
var
  VPoint: TPoint;
  VZoomCurr: Byte;
  VConverter: ICoordConverter;
  VMap: TMapType;
begin
  GState.ViewState.LockRead;
  try
    VPoint := GState.ViewState.VisiblePixel2MapPixel(MouseCursorPos);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VMap := GState.ViewState.GetCurrentMap;
    VConverter := GState.ViewState.GetCurrentCoordConverter;
  finally
    GState.ViewState.UnLockRead;
  end;
  if VMap.TileStorage.GetIsStoreFileCache then begin
    VConverter.CheckPixelPosStrict(VPoint, VZoomCurr, True);
    VPoint := VConverter.PixelPos2TilePos(VPoint, VZoomCurr);
    // Открыть файл в просмотрщике. Заменить на проверку возможности сделать это или дописать экспорт во временный файл.
    ShellExecute(0,'open',PChar(VMap.GetTileFileName(VPoint, VZoomCurr)),nil,nil,SW_SHOWNORMAL);
  end else begin
    ShowMessage('Это не тайловый кеш, невозможно получить имя файла с тайлом.');
  end;
end;

procedure TFmain.N25Click(Sender: TObject);
var s:string;
  VPoint: TPoint;
  VZoomCurr: Byte;
  VConverter: ICoordConverter;
  VMap: TMapType;
begin
  GState.ViewState.LockRead;
  try
    VPoint := GState.ViewState.VisiblePixel2MapPixel(MouseCursorPos);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VMap := GState.ViewState.GetCurrentMap;
    VConverter := GState.ViewState.GetCurrentCoordConverter;
  finally
    GState.ViewState.UnLockRead;
  end;
  if VMap.TileStorage.GetIsStoreFileCache then begin
    VConverter.CheckPixelPosStrict(VPoint, VZoomCurr, True);
    VPoint := VConverter.PixelPos2TilePos(VPoint, VZoomCurr);
    s:=VMap.GetTileFileName(VPoint, VZoomCurr);
    s := ExtractFilePath(s);
    ShellExecute(0,'open',PChar(s),nil,nil,SW_SHOWNORMAL);
  end else begin
    ShowMessage('Это не тайловый кеш, невозможно получить имя файла с тайлом.');
  end;
end;

procedure TFmain.NDelClick(Sender: TObject);
var
  s:string;
  VMapType:TMapType;
  VMapMain: TMapType;
  VLoadPoint: TPoint;
  VZoomCurr: Byte;
  VPoint: TPoint;
begin
  VMapType := nil;
  if TMenuItem(sender).Tag<>0 then begin
    VMapType:=TMapType(TMenuItem(sender).Tag);
  end;

  GState.ViewState.LockRead;
  try
    VMapMain := GState.ViewState.GetCurrentMap;
    if VMapType = nil then begin
      VMapType := GState.ViewState.GetCurrentMap;
    end;
    VPoint := GState.ViewState.VisiblePixel2MapPixel(FMouseUpPoint);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
  finally
    GState.ViewState.UnLockRead;
  end;
  if VMapMain.GeoConvert.CheckPixelPosStrict(VPoint, VZoomCurr, False) then begin
    VLoadPoint := VMapMain.GeoConvert.PixelPos2OtherMap(VPoint, VZoomCurr, VMapType.GeoConvert);
    VMapType.GeoConvert.CheckPixelPosStrict(VLoadPoint, VZoomCurr, False);
    VLoadPoint := VMapType.GeoConvert.PixelPos2TilePos(VPoint, VZoomCurr);
    s:=VMapType.GetTileShowName(VLoadPoint, VZoomCurr);
    if (MessageBox(handle,pchar(SAS_MSG_youasure+' '+s+'?'),pchar(SAS_MSG_coution),36)=IDYES) then begin
      VMapType.DeleteTile(VLoadPoint, VZoomCurr);
      generate_im;
    end;
  end;
end;

procedure TFmain.NSRCinetClick(Sender: TObject);
begin
  FMainLayer.UseDownload := TTileSource(TTBXItem(Sender).Tag);
end;

procedure TFmain.N16Click(Sender: TObject);
begin
 fabout.ShowModal;
end;

procedure TFmain.TBREGIONClick(Sender: TObject);
begin
 TBRectSave.ImageIndex:=9;
 TBRectSave.Checked:=true;
 setalloperationfalse(ao_reg);
end;

procedure TFmain.TBRECTClick(Sender: TObject);
begin
 TBRectSave.ImageIndex:=6;
 TBRectSave.Checked:=true;
 setalloperationfalse(ao_rect);
end;

procedure TFmain.TBRectSaveClick(Sender: TObject);
begin
  if TBRectSave.ImageIndex=6 then begin
    setalloperationfalse(ao_rect);
  end else begin
    setalloperationfalse(ao_reg);
  end;
end;

procedure TFmain.TBPreviousClick(Sender: TObject);
var
  VZoom: Byte;
  VPolygon: TExtendedPointArray;
begin
  VZoom := GState.LastSelectionInfo.Zoom;
  VPolygon := Copy(GState.LastSelectionInfo.Polygon);
  if length(VPolygon)>0 then begin
    fsaveas.Show_(VZoom, VPolygon);
  end else begin
    showmessage(SAS_MSG_NeedHL);
  end;
end;

//карта заполнения в основном окне
procedure TFmain.NFillMapClick(Sender: TObject);
var
  VZoom: Integer;
begin
  VZoom := FLayerFillingMap.SourceZoom;
  if VZoom > -1 then begin
    TBXToolPalette1.SelectedCell:=Point((VZoom + 1) mod 5,(VZoom + 1) div 5);
  end else begin
    TBXToolPalette1.SelectedCell:=Point(0,0);
  end;
end;

procedure TFmain.TBXToolPalette1CellClick(Sender: TTBXCustomToolPalette; var ACol, ARow: Integer; var AllowChange: Boolean);
var
  Vzoom_mapzap: integer;
begin
  Vzoom_mapzap:=((5*ARow)+ACol)-1;
  FLayerFillingMap.SetSourceMap(FLayerFillingMap.SourceSelected, Vzoom_mapzap);
end;
//X-карта заполнения в основном окне

procedure TFmain.TBCalcRasClick(Sender: TObject);
begin
  setalloperationfalse(ao_line);
end;

procedure TFmain.N012Click(Sender: TObject);
var
  VZoom: Byte;
  VLonLat: TExtendedPoint;
begin
  VZoom := TMenuItem(sender).tag - 1;
  VLonLat := GState.ViewState.GetCenterLonLat;
  topos(VLonLat,VZoom,true);
end;

procedure TFmain.N29Click(Sender: TObject);
begin
  ShellExecute(0,'open',PChar(GState.HelpFileName),nil,nil,SW_SHOWNORMAL);
end;

procedure TFmain.EditGoogleSrchAcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
var
  VResult: IGeoCodeResult;
begin
  VResult := FGoogleGeoCoder.GetLocations(Trim(NewText), GState.ViewState.GetCenterLonLat);
  FSearchPresenter.ShowSearchResults(VResult, GState.ViewState.GetCurrentZoom);
end;

procedure TFmain.TBEditItem1AcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
var
  VResult: IGeoCodeResult;
begin
  VResult := FYandexGeoCoder.GetLocations(Trim(NewText), GState.ViewState.GetCenterLonLat);
  FSearchPresenter.ShowSearchResults(VResult, GState.ViewState.GetCurrentZoom);
end;

procedure TFmain.TBSubmenuItem1Click(Sender: TObject);
var
  VResult: IGeoCodeResult;
  VZoom: Byte;
begin
  if frmGoTo.ShowGeocodeModal(VResult, VZoom) then begin
    FSearchPresenter.ShowSearchResults(VResult, VZoom);
  end;
end;

procedure TFmain.TBMainToolBarClose(Sender: TObject);
begin
 if sender=TBMainToolBar then NMainToolBarShow.Checked:=false;
 if sender=SrcToolbar then NsrcToolBarShow.Checked:=false;
 if sender=ZoomToolBar then NZoomToolBarShow.Checked:=false;
 if sender=GPSToolBar then NGPSToolBarShow.Checked:=false;
 if sender=TBMarksToolBar then NMarksBarShow.Checked:=false;
end;

procedure TFmain.N000Click(Sender: TObject);
begin
 GState.TileGridZoom:=TMenuItem(Sender).Tag;
 generate_im;
end;

procedure TFmain.NShowGranClick(Sender: TObject);
var
  i:integer;
  VZoom: Byte;
begin
 if GState.TileGridZoom=0 then NShowGran.Items[0].Checked:=true;
 if GState.TileGridZoom=99 then NShowGran.Items[1].Checked:=true;
 VZoom := GState.ViewState.GetCurrentZoom;
 NShowGran.Items[1].Caption:=SAS_STR_activescale+' (х'+inttostr(VZoom + 1)+')';
 for i:=2 to 7 do
  if VZoom+i-1<24 then begin
                            NShowGran.Items[i].Caption:=SAS_STR_for+' х'+inttostr(VZoom+i-1);
                            NShowGran.Items[i].Visible:=true;
                            NShowGran.Items[i].Tag:=VZoom+i-1;
                            if NShowGran.Items[i].Tag=GState.TileGridZoom then NShowGran.Items[i].Checked:=true
                                                                else NShowGran.Items[i].Checked:=false;
                           end
                      else NShowGran.Items[i].Visible:=false;
end;

procedure TFmain.TBItem2Click(Sender: TObject);
begin
 close;
end;

procedure TFmain.TBGPSconnClick(Sender: TObject);
begin
  TTBXitem(sender).Enabled := False;
  TTBXitem(sender).Checked := GState.GPSpar.GPSModele.IsConnected;
  if TTBXitem(sender).Checked then begin
    GState.GPSpar.GPSModele.Disconnect;
  end else begin
    GState.GPSpar.GPSModele.Connect;
  end;
end;

procedure TFmain.TBGPSPathClick(Sender: TObject);
begin
 tbitmGPSTrackShow.Checked:=TTBXitem(sender).Checked;
 TBGPSPath.Checked:=TTBXitem(sender).Checked;
 GState.GPSpar.GPS_ShowPath:=TBGPSPath.Checked;
end;

procedure TFmain.TBGPSToPointClick(Sender: TObject);
begin
 tbitmGPSCenterMap.Checked:=TTBXitem(sender).Checked;
 TBGPSToPoint.Checked:=TTBXitem(sender).Checked;
 GState.GPSpar.GPS_MapMove:=TBGPSToPoint.Checked;
end;

procedure TFmain.TBCOORDClick(Sender: TObject);
var
  Poly: TExtendedPointArray;
  VSelLonLat: TFSelLonLat;
  VLonLatRect: TExtendedRect;
begin
  VSelLonLat:= TFSelLonLat.Create(Self);
  Try
    Poly := GState.LastSelectionInfo.Polygon;
    GetMinMax(VLonLatRect, Poly);
    if VSelLonLat.Execute(VLonLatRect) Then Begin
      Poly := PolygonFromRect(VLonLatRect);
      fsaveas.Show_(GState.ViewState.GetCurrentZoom, Poly);
      Poly := nil;
    End;
  Finally
    VSelLonLat.Free;
  End;
  TBmoveClick(Sender);
end;

procedure TFmain.ShowstatusClick(Sender: TObject);
begin
  FLayerStatBar.Visible := TTBXItem(Sender).Checked;
end;

procedure TFmain.ShowMiniMapClick(Sender: TObject);
begin
  FLayerMiniMap.Visible := TTBXItem(Sender).Checked;
end;

procedure TFmain.ShowLineClick(Sender: TObject);
begin
  FLayerScaleLine.Visible := TTBXItem(Sender).Checked;
end;

procedure TFmain.N32Click(Sender: TObject);
begin
 FLayerMapScale.Visible := TTBXItem(Sender).Checked;
end;

procedure TFmain.TBItem3Click(Sender: TObject);
var F:TextFile;
    i:integer;
    SaveDlg: TSaveDialog;
    VAllPoints: TExtendedPointArray;
begin
  Fprogress2.Visible:=true;
  try
    fprogress2.MemoInfo.Lines[0]:=SAS_STR_savetreck;
    Fprogress2.ProgressBar1.Max:=100;
    SaveDlg := TSaveDialog.Create(nil);
    try
      SaveDlg.DefaultExt:='*.kml';
      SaveDlg.Filter:='KML|*.kml';
      Fprogress2.ProgressBar1.Progress1:=0;
      if (SaveDlg.Execute)and(SaveDlg.FileName<>'') then begin
        AssignFile(f,SaveDlg.FileName);
        rewrite(f);
        Fprogress2.ProgressBar1.Progress1:=10;
        VAllPoints := GState.GPSpar.GPSRecorder.GetAllPoints;
        Writeln(f,'<?xml version="1.0" encoding="UTF-8"?>');
        Writeln(f,'<kml xmlns="http://earth.google.com/kml/2.1">');
        Writeln(f,'<Folder>');
        Fprogress2.ProgressBar1.Progress1:=20;
        Writeln(f,'	<name>'+ ExtractFileName(SaveDlg.FileName)+'</name>');
        Writeln(f,'	<open>0</open>');
        Writeln(f,'<Style>');
        Writeln(f,'	<ListStyle>');
        Fprogress2.ProgressBar1.Progress1:=30;
        Writeln(f,'		<listItemType>checkHideChildren</listItemType>');
        Writeln(f,'		<bgColor>00ffffff</bgColor>');
        Writeln(f,'	</ListStyle>');
        Fprogress2.ProgressBar1.Progress1:=40;
        Writeln(f,'</Style>');
        Writeln(f,'<Placemark>');
        Writeln(f,'<name>'+ExtractFileName(SaveDlg.FileName)+'</name>');
        Fprogress2.ProgressBar1.Progress1:=50;
        Writeln(f,'<Style>');
        Writeln(f,'	<LineStyle>');
        Writeln(f,'		<color>ff000000</color>');
        Fprogress2.ProgressBar1.Progress1:=60;
        Writeln(f,'	</LineStyle>');
        Writeln(f,'</Style>');
        Writeln(f,'<LineString>');
        Fprogress2.ProgressBar1.Progress1:=70;
        Writeln(f,'	<tessellate>1</tessellate>');
        Writeln(f,'	<altitudeMode>absolute</altitudeMode>');
        Writeln(f,' <coordinates>');
        Fprogress2.ProgressBar1.Progress1:=80;
        for i:=0 to length(VAllPoints)-1 do
          Writeln(f,R2strPoint(VAllPoints[i].x),',',R2strPoint(VAllPoints[i].y),',0');
        Writeln(f,' </coordinates>');
        Fprogress2.ProgressBar1.Progress1:=90;
        Writeln(f,'</LineString>');
        Writeln(f,'</Placemark>');
        Writeln(f,'</Folder>'+#13#10+'</kml>');
        CloseFile(f);
      end;
      Fprogress2.ProgressBar1.Progress1:=100;
    finally
      SaveDlg.Free;
    end;
  finally
    Fprogress2.Visible:=false;
  end;
end;

procedure TFmain.TBItem5Click(Sender: TObject);
var
  VAllPoints: TExtendedPointArray;
begin
  VAllPoints := GState.GPSpar.GPSRecorder.GetAllPoints;
  if length(VAllPoints)>1 then begin
    if SaveLineModal(-1, VAllPoints, '') then begin
      setalloperationfalse(ao_movemap);
      generate_im;
    end;
  end else begin
    ShowMessage(SAS_ERR_Nopoints);
  end;
end;

procedure TFmain.Google1Click(Sender: TObject);
var
  VLonLat:tExtendedPoint;
  VZoomCurr: Byte;
begin
  GState.ViewState.LockRead;
  try
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VLonLat := GState.ViewState.GetCenterLonLat;
  finally
    GState.ViewState.UnLockRead;
  end;
  CopyStringToClipboard('http://maps.google.com/?ie=UTF8&ll='+R2StrPoint(VLonLat.y)+','+R2StrPoint(VLonLat.x)+'&spn=57.249013,100.371094&t=h&z='+inttostr(VZoomCurr));
end;

procedure TFmain.YaLinkClick(Sender: TObject);
var
  Vpos:tExtendedPoint;
  VExtRect: TExtendedRect;
begin
  GState.ViewState.LockRead;
  try
    Vpos := GState.ViewState.GetCenterLonLat;
    VExtRect := GState.ViewState.GetViewLonLatRect;
  finally
    GState.ViewState.UnLockRead;
  end;
  CopyStringToClipboard(
    'http://beta-maps.yandex.ru/?ll='+
    R2StrPoint(round(Vpos.x*100000)/100000)+'%2C'+
    R2StrPoint(round(Vpos.y*100000)/100000)+
    '&spn='+R2StrPoint(abs(VExtRect.Left-VExtRect.Right))+'%2C'+
    R2StrPoint(abs(VExtRect.Top-VExtRect.Bottom))+'&l=sat'
  );
end;

procedure TFmain.kosmosnimkiru1Click(Sender: TObject);
var
  VLonLat:tExtendedPoint;
  VZoomCurr: Byte;
begin
  GState.ViewState.LockRead;
  try
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VLonLat := GState.ViewState.GetCenterLonLat;
  finally
    GState.ViewState.UnLockRead;
  end;
  CopyStringToClipboard('http://kosmosnimki.ru/?x='+R2StrPoint(VLonLat.x)+'&y='+R2StrPoint(VLonLat.y)+'&z='+inttostr(VZoomCurr)+'&fullscreen=false&mode=satellite');
end;

procedure TFmain.mapResize(Sender: TObject);
begin
  if (not ProgramClose)and(not ProgramStart)then begin
    GState.ViewState.ChangeViewSize(Point(map.Width, map.Height));
    FMainLayer.Resize;
    FLayerStatBar.Resize;
    FLayerScaleLine.Resize;
    FLayerMapNal.Resize;
    FLayerMapMarks.Resize;
    FLayerMapGPS.Resize;
    FLayerMapScale.Resize;
    FWikiLayer.Resize;
    FLayerFillingMap.Resize;
    FLayerGoto.Resize;
    FShowErrorLayer.Resize;
    LayerMapNavToMark.Resize;
    FLayerMiniMap.Resize;
    FLayerStatBar.Redraw;
  end;
end;

procedure TFmain.TBLoadSelFromFileClick(Sender: TObject);
begin
  if (OpenDialog1.Execute) then begin
    Fsaveas.LoadSelFromFile(OpenDialog1.FileName);
  end
end;

function TFmain.GetStreamFromURL(var ms:TMemoryStream;url:string;conttype:string):integer;
var par,ty:string;
    err:boolean;
    Buffer:array [1..64535] of char;
    BufferLen:LongWord;
    hSession,hFile:Pointer;
    dwtype: array [1..20] of char;
    dwindex, dwcodelen,dwReserv: dword;
begin
 hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
 if Assigned(hSession)
  then begin
        hFile:=InternetOpenURL(hSession,PChar(URL),PChar(par),length(par), INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_RELOAD,0);
        if Assigned(hFile)then
         begin
          dwcodelen:=150; dwReserv:=0; dwindex:=0;
          if HttpQueryInfo(hFile,HTTP_QUERY_STATUS_CODE,@dwtype, dwcodelen, dwReserv)
           then dwindex:=strtoint(pchar(@dwtype));
          if (dwindex=HTTP_STATUS_PROXY_AUTH_REQ) then
           begin
            if (not GState.InetConnect.userwinset)and(GState.InetConnect.uselogin) then
             begin
              InternetSetOption (hFile, INTERNET_OPTION_PROXY_USERNAME,PChar(GState.InetConnect.loginstr), length(GState.InetConnect.loginstr));
              InternetSetOption (hFile, INTERNET_OPTION_PROXY_PASSWORD,PChar(GState.InetConnect.passstr), length(GState.InetConnect.Passstr));
              HttpSendRequest(hFile, nil, 0,Nil, 0);
             end;
            dwcodelen:=150; dwReserv:=0; dwindex:=0;
            if HttpQueryInfo(hFile,HTTP_QUERY_STATUS_CODE,@dwtype, dwcodelen, dwReserv)
             then dwindex:=strtoint(pchar(@dwtype));
            if (dwindex=HTTP_STATUS_PROXY_AUTH_REQ) then //Неверные пароль логин
             begin
            	result:=-3;
              InternetCloseHandle(hFile);
              InternetCloseHandle(hSession);
              exit
             end;
           end;
          result:=0;
          dwindex:=0; dwcodelen:=150; ty:='';
          fillchar(dwtype,sizeof(dwtype),0);
          if HttpQueryInfo(hfile,HTTP_QUERY_CONTENT_TYPE, @dwtype,dwcodelen,dwindex)
           then ty:=PChar(@dwtype);
          if (PosEx(conttype,ty,1)>0) then
          repeat
           err:=not(internetReadFile(hFile,@Buffer,SizeOf(Buffer),BufferLen));
           ms.Write(Buffer,BufferLen);
           inc(result,BufferLen)
          until (BufferLen=0)and(BufferLen<SizeOf(Buffer))and(err=false)
          else result:=-1;
         end
        else result:=0;
       end
  else result:=0;
  ms.Position:=0;
end;

procedure TFmain.PopupMenu1Popup(Sender: TObject);
var
  i:Integer;
  VMapType: TMapType;
  VLayerIsActive: Boolean;
begin
  ldm.Visible:=false;
  dlm.Visible:=false;
  For i:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[i];
    if (VMapType.asLayer) then begin
      VLayerIsActive := GState.ViewState.IsHybrGUIDSelected(VMapType.GUID);
      TMenuItem(FNDwnItemList.GetByGUID(VMapType.GUID)).Visible := VLayerIsActive;
      TMenuItem(FNDelItemList.GetByGUID(VMapType.GUID)).Visible := VLayerIsActive;
      if VLayerIsActive then begin
        ldm.Visible:=true;
        dlm.Visible:=true;
      end
    end;
  end;
end;

procedure TFmain.NinvertcolorClick(Sender: TObject);
begin
 GState.InvertColor:=Ninvertcolor.Checked;
 generate_im;
end;

procedure TFmain.mapDblClick(Sender: TObject);
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
    GState.ViewState.ChangeMapPixelToVisualPoint(r);
  end;
end;

procedure TFmain.TBAdd_PointClick(Sender: TObject);
begin
 setalloperationfalse(ao_add_point);
end;

procedure TFmain.TBAdd_LineClick(Sender: TObject);
begin
 setalloperationfalse(ao_add_Line);
end;

procedure TFmain.TBAdd_PolyClick(Sender: TObject);
begin
 setalloperationfalse(ao_add_poly);
end;

procedure TFmain.NMarkEditClick(Sender: TObject);
var
  VMark: TMarkFull;
begin
  FEditMarkId:=strtoint(FPWL.numid);
  VMark := GState.MarksDb.GetMarkByID(FEditMarkId);
  if VMark <> nil then begin
    try
      if VMark.IsPoint then begin
        if EditMarkModal(VMark) then begin
          GState.MarksDb.WriteMark(VMark);
          GState.MarksDb.SaveMarks2File;
        end;
      end else if VMark.IsPoly then begin
        Fadd_line_arr:=VMark.Points;
        setalloperationfalse(ao_edit_poly);
      end else if VMark.IsLine then begin
        Fadd_line_arr:=VMark.Points;
        setalloperationfalse(ao_edit_line);
      end;
    finally
      VMark.Free;
    end;
    generate_im;
  end;
end;

procedure TFmain.NMarkDelClick(Sender: TObject);
begin
 FWikiLayer.MouseOnReg(FPWL,FmoveTrue);
 if DeleteMarkModal(StrToInt(FPWL.numid),Handle) then
  generate_im;
end;

procedure TFmain.NMarksBarShowClick(Sender: TObject);
begin
 TBMarksToolBar.Visible:=NMarksBarShow.Checked;
end;

procedure TFmain.NMarkOperClick(Sender: TObject);
var
  VId: Integer;
  VMark: TMarkFull;
begin
  FWikiLayer.MouseOnReg(FPWL,FmoveTrue);
  VId := strtoint(FPWL.numid);
  VMark := GState.MarksDb.GetMarkByID(VId);
  if VMark <> nil then begin
    try
      OperationMark(VMark);
    finally
      VMark.Free;
    end;
  end;
end;

procedure TFmain.livecom1Click(Sender: TObject);
var
  VLonLat:tExtendedPoint;
  VZoomCurr: Byte;
begin
  GState.ViewState.LockRead;
  try
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VLonLat := GState.ViewState.GetCenterLonLat;
  finally
    GState.ViewState.UnLockRead;
  end;
  CopyStringToClipboard('http://maps.live.com/default.aspx?v=2&cp='+R2StrPoint(VLonLat.y)+'~'+R2StrPoint(VLonLat.x)+'&style=h&lvl='+inttostr(VZoomCurr));
end;

procedure TFmain.N13Click(Sender: TObject);
var
  VPoint: TPoint;
  VZoomCurr: Byte;
  VMap: TMapType;
begin
  GState.ViewState.LockRead;
  try
    VMap := GState.ViewState.GetCurrentMap;
    VPoint := GState.ViewState.VisiblePixel2MapPixel(FMouseDownPoint);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
  finally
    GState.ViewState.UnLockRead;
  end;
  VMap.GeoConvert.CheckPixelPosStrict(VPoint, VZoomCurr, True);
  VPoint := VMap.GeoConvert.PixelPos2TilePos(VPoint, VZoomCurr);
  CopyStringToClipboard(VMap.GetLink(VPoint, VZoomCurr));
end;

procedure TFmain.ImageAtlas1Click(Sender: TObject);
var
  VPos: TExtendedPoint;
  VZoomCurr: Byte;
begin
  GState.ViewState.LockRead;
  try
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VPos:=GState.ViewState.GetCenterLonLat;
  finally
    GState.ViewState.UnLockRead;
  end;
  CopyStringToClipboard(
    'http://imageatlas.digitalglobe.com/ia-webapp/?lat='+
    R2StrPoint(VPos.y)+'&lon='+R2StrPoint(VPos.x)+
    '&zoom='+inttostr(VZoomCurr)
  );
end;

procedure TFmain.DigitalGlobe1Click(Sender: TObject);
var
  VPos: TExtendedPoint;
  VSize: TPoint;
begin
  GState.ViewState.LockRead;
  try
    VSize := GState.ViewState.GetViewSizeInVisiblePixel;
    VPos:=GState.ViewState.VisiblePixel2LonLat(FmoveTrue);
  finally
    GState.ViewState.UnLockRead;
  end;
  FDGAvailablePic.setup(VPos, VSize);
end;

procedure TFmain.mapMouseLeave(Sender: TObject);
begin
 if (FHintWindow<>nil) then
  begin
   FHintWindow.ReleaseHandle;
   FreeAndNil(FHintWindow);
  end;
end;

procedure TFmain.GPSReceiverDisconnect;
begin
  if GState.GPSpar.GPS_SensorsAutoShow then TBXSensorsBar.Visible:=false;
  if TBXSignalStrengthBar.Visible then UpdateGPSSatellites;
  tbitmGPSConnect.Enabled := True;
  TBGPSconn.Enabled := True;
  FLayerMapGPS.Visible:=false;
  tbitmGPSConnect.Checked:=false;
  TBGPSconn.Checked:=false;
end;

procedure TFmain.GPSReceiverReceive;
var
  VPointCurr: TExtendedPoint;
  VPointPrev: TExtendedPoint;
  VPointDelta: TExtendedPoint;
  VPosition: IGPSPosition;
begin
  VPosition := GState.GPSpar.GPSModele.Position;
  if FSettings.Visible then FSettings.SatellitePaint;
  if TBXSignalStrengthBar.Visible then UpdateGPSSatellites;
  if (VPosition.IsFix=0) then exit;
  if not((FMapMoving)or(FMapZoomAnimtion))and(Screen.ActiveForm=Self) then begin
    if (GState.GPSpar.GPS_MapMove) then begin
      if GState.GPSpar.GPS_MapMoveCentered then begin
        VPointCurr := GState.GPSpar.GPSRecorder.GetLastPoint;
        GState.ViewState.LockWrite;
        GState.ViewState.ChangeLonLatAndUnlock(VPointCurr);
      end else begin
        if GState.GPSpar.GPSRecorder.GetTwoLastPoints(VPointCurr, VPointPrev) then begin
          GState.ViewState.LockWrite;
          VPointDelta:=GState.ViewState.GetCenterLonLat;
          VPointDelta:=ExtPoint(VPointDelta.x+VPointCurr.x-VPointPrev.x,
                                VPointDelta.y+VPointCurr.y-VPointPrev.y);
          if PointInRect(VPointCurr, GState.ViewState.GetViewLonLatRect) then  begin
            GState.ViewState.ChangeLonLatAndUnlock(VPointDelta);
          end else begin
            GState.ViewState.UnLockWrite;
          end;
        end;
      end;
    end else begin
      FLayerStatBar.Redraw;
      FLayerMapGPS.Redraw;
    end;
   end;
  UpdateGPSsensors;
end;

procedure TFmain.GPSReceiverConnect;
begin
  tbitmGPSConnect.Enabled := True;
  TBGPSconn.Enabled := True;
  FLayerMapGPS.Visible:=True;
  tbitmGPSConnect.Checked:=True;
  TBGPSconn.Checked:=True;
  if GState.GPSpar.GPS_SensorsAutoShow then TBXSensorsBar.Visible:=true;
end;

procedure TFmain.GPSReceiverConnectError;
begin
  ShowMessage(SAS_ERR_PortOpen);
end;

procedure TFmain.GPSReceiverTimeout;
begin
  tbitmGPSConnect.Enabled := True;
  TBGPSconn.Enabled := True;
  ShowMessage(SAS_ERR_Communication);
end;

procedure TFmain.NMapParamsClick(Sender: TObject);
var
  VMapType: TMapType;
begin
  if TTBXItem(sender).Tag=0 then begin
    VMapType := GState.ViewState.GetCurrentMap;
  end else begin
    VMapType := TMapType(TTBXItem(sender).Tag);
  end;
  if FEditMap.EditMapModadl(VMapType) then begin
    CreateMapUI;
    generate_im;
  end;
end;

procedure TFmain.mapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  i:integer;
  VSelectionRect: TExtendedRect;
  VClickLonLat: TExtendedPoint;
  VClickRect: TRect;
  VClickLonLatRect: TExtendedRect;
  VPoly:  TExtendedPointArray;
begin
  if (FHintWindow<>nil) then begin
    FHintWindow.ReleaseHandle;
    FreeAndNil(FHintWindow);
  end;
  if (Layer <> nil) then begin
    exit;
  end;
  if (ssDouble in Shift)or(FMapZoomAnimtion)or(button=mbMiddle)or(HiWord(GetKeyState(VK_DELETE))<>0)
  or(HiWord(GetKeyState(VK_INSERT))<>0)or(HiWord(GetKeyState(VK_F5))<>0) then exit;
  Screen.ActiveForm.SetFocusedControl(map);
  GState.ViewState.LockRead;
  try
    VClickLonLat := GState.ViewState.VisiblePixel2LonLat(Point(x, y));
    VClickRect.Left := x - 5;
    VClickRect.Top := y - 5;
    VClickRect.Right := x + 5;
    VClickRect.Bottom := y + 5;
    VClickLonLatRect.TopLeft := GState.ViewState.VisiblePixel2LonLat(VClickRect.TopLeft);
    VClickLonLatRect.BottomRight := GState.ViewState.VisiblePixel2LonLat(VClickRect.BottomRight);
  finally
    GState.ViewState.UnLockRead;
  end;
  if (Button=mbLeft)and(FCurrentOper<>ao_movemap) then begin
    if (FCurrentOper=ao_line)then begin
      setlength(Flength_arr,length(Flength_arr)+1);
      Flength_arr[length(Flength_arr)-1]:= VClickLonLat;
      TBEditPath.Visible:=(length(Flength_arr)>1);
      FLayerMapNal.DrawLineCalc(Flength_arr, FLenShow);
    end;
    if (FCurrentOper=ao_Reg) then begin
      setlength(Freg_arr,length(Freg_arr)+1);
      Freg_arr[length(Freg_arr)-1]:= VClickLonLat;
      TBEditPath.Visible:=(length(Freg_arr)>1);
      FLayerMapNal.DrawReg(Freg_arr);
    end;
    if (FCurrentOper=ao_rect)then begin
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
      FLayerMapNal.DrawSelectionRect(VSelectionRect);
      if (Frect_p2) then begin
        SetLength(VPoly, 5);
        VPoly[0] := VSelectionRect.TopLeft;
        VPoly[1] := ExtPoint(VSelectionRect.Right, VSelectionRect.Top);
        VPoly[2] := VSelectionRect.BottomRight;
        VPoly[3] := ExtPoint(VSelectionRect.Left, VSelectionRect.Bottom);
        VPoly[4] := VSelectionRect.TopLeft;
        fsaveas.Show_(GState.ViewState.GetCurrentZoom, VPoly);
        FLayerMapNal.DrawNothing;
        VPoly := nil;
        Frect_p2:=false;
      end;
    end;
    if (FCurrentOper=ao_add_point) then begin
      if(AddNewPointModal(VClickLonLat)) then begin
        setalloperationfalse(ao_movemap);
        generate_im;
      end;
    end;
    if (FCurrentOper in [ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly]) then begin
      for i:=0 to length(Fadd_line_arr)-1 do begin
        if (VClickLonLatRect.Left < Fadd_line_arr[i].X) and
          (VClickLonLatRect.Top > Fadd_line_arr[i].Y) and
          (VClickLonLatRect.Right > Fadd_line_arr[i].X) and
          (VClickLonLatRect.Bottom < Fadd_line_arr[i].Y)
        then begin
          Fmovepoint:=i+1;
          Flastpoint:=i;
          TBEditPath.Visible:=(length(Fadd_line_arr)>1);
          FLayerMapNal.DrawNewPath(Fadd_line_arr, (FCurrentOper=ao_add_poly)or(FCurrentOper=ao_edit_poly), Flastpoint);
          exit;
        end;
      end;
      inc(Flastpoint);
      Fmovepoint:=Flastpoint + 1;
      insertinpath(Flastpoint, VClickLonLat);
      TBEditPath.Visible:=(length(Fadd_line_arr)>1);
      FLayerMapNal.DrawNewPath(Fadd_line_arr, (FCurrentOper=ao_add_poly)or(FCurrentOper=ao_edit_poly), Flastpoint);
    end;
    exit;
  end;
  if FMapMoving then exit;
  if (Button=mbright)and(FCurrentOper=ao_movemap) then begin
    FMouseUpPoint:=point(x,y);
    FPWL.find:=false;
    FPWL.S:=0;
    if FLayerMapMarks.Visible then begin
      MouseOnMyReg(FPWL,Point(x,y));
    end;  
    NMarkEdit.Visible:=FPWL.find;
    NMarkDel.Visible:=FPWL.find;
    NMarkSep.Visible:=FPWL.find;
    NMarkOper.Visible:=FPWL.find;
    NMarkNav.Visible:=FPWL.find;
    if (FPWL.find)and(FPWL.type_<>ROTpoint) then begin
      NMarksCalcsSq.Visible:=(FPWL.type_=ROTPoly);
      NMarksCalcsPer.Visible:=(FPWL.type_=ROTPoly);
      NMarksCalcsLen.Visible:=(FPWL.type_=ROTline);
      NMarksCalcs.Visible:=true;
    end else begin
      NMarksCalcs.Visible:=false;
    end;
    if (LayerMapNavToMark.Visible)and(LayerMapNavToMark.id=strtoint(FPWL.numid)) then begin
      NMarkNav.Checked:=true
    end else begin
      NMarkNav.Checked:=false;
    end;
    map.PopupMenu:=PopupMenu1;
  end else begin
    FMapMoving:=true;
    map.PopupMenu:=nil;
  end;
  FMouseDownPoint:=Point(x,y);
end;

procedure TFmain.mapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  VPWL:TResObj;
  stw:String;
  VPoint: TPoint;
  VZoomCurr: Byte;
  VSelectionRect: TExtendedRect;
  VMapMoving: Boolean;
  VMap: TMapType;
  VValidPoint: Boolean;
  VConverter: ICoordConverter;
  VTile: TPoint;
  VLonLat: TExtendedPoint;
  VVisibleSizeInPixel: TPoint;
begin
  if (Layer <> nil) then begin
    exit;
  end;
 if (ssDouble in Shift) then exit;
 VMapMoving := FMapMoving;
 FMapMoving:=false;

 GState.ViewState.LockRead;
 try
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VPoint := GState.ViewState.VisiblePixel2MapPixel(Point(x, y));
    VLonLat := GState.ViewState.VisiblePixel2LonLat(FmoveTrue);
    VMap := GState.ViewState.GetCurrentMap;
    VConverter := GState.ViewState.GetCurrentCoordConverter;
    VVisibleSizeInPixel := GState.ViewState.GetViewSizeInVisiblePixel;
 finally
   GState.ViewState.UnLockRead;
 end;
 VValidPoint := VConverter.CheckPixelPosStrict(VPoint, VZoomCurr, False);
 VTile := VConverter.PixelPos2TilePos(VPoint, VZoomCurr);
 if HiWord(GetKeyState(VK_DELETE))<>0 then begin
  if VValidPoint then begin
   VMap.DeleteTile(VTile, VZoomCurr);
   generate_im;
  end;
  exit;
 end;
 if HiWord(GetKeyState(VK_INSERT))<>0 then begin
  if VValidPoint then begin
    TTileDownloaderUIOneTile.Create(VTile, VZoomCurr, VMap);
  end;
  exit;
 end;
 if HiWord(GetKeyState(VK_F6))<>0 then
  begin
    FDGAvailablePic.setup(VLonLat, VVisibleSizeInPixel);
   exit;
  end;

 if Fmovepoint>0 then begin
  Fmovepoint:=0;
 end;
 if (((FCurrentOper<>ao_movemap)and(Button=mbLeft))or
     ((FCurrentOper=ao_movemap)and(Button=mbRight))) then exit;
 if (FMapZoomAnimtion) then exit;
 map.Enabled:=false;
 map.Enabled:=true;
 if button=mbMiddle then
   begin
    TBFullSize.Checked:=not(TBFullSize.Checked);
    TBFullSizeClick(Sender);
    exit;
   end;

 if VMapMoving then begin
   GState.ViewState.ChangeMapPixelByDelta(Point(FMouseDownPoint.x-x, FMouseDownPoint.y-y));
 end;

 FMouseUpPoint:=Point(x,y);
 if (y=FMouseDownPoint.y)and(x=FMouseDownPoint.x) then
  begin
   FLayerStatBar.Redraw;
   FLayerScaleLine.Redraw;
   if FCurrentOper=ao_line then begin
    TBEditPath.Visible:=(length(Flength_arr)>1);
    FLayerMapNal.DrawLineCalc(Flength_arr, FLenShow);
   end;
   if FCurrentOper=ao_reg then begin
    TBEditPath.Visible:=(length(Freg_arr)>1);
    FLayerMapNal.DrawReg(Freg_arr);
   end;
   if FCurrentOper=ao_rect then begin
     VSelectionRect := FSelectionRect;
     PrepareSelectionRect(Shift, VSelectionRect);
     FLayerMapNal.DrawSelectionRect(VSelectionRect);
   end;
   if GState.GPSpar.GPSModele.IsConnected then begin
     UpdateGPSsensors;
   end;
   if FCurrentOper in [ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly] then begin
    TBEditPath.Visible:=(length(Fadd_line_arr)>1);
    FLayerMapNal.DrawNewPath(Fadd_line_arr, (FCurrentOper=ao_add_poly)or(FCurrentOper=ao_edit_poly), Flastpoint);
   end;
  end;
 if (y=FMouseDownPoint.y)and(x=FMouseDownPoint.x)and(FCurrentOper=ao_movemap)and(button=mbLeft) then
  begin
    VPWL.S:=0;
    VPWL.find:=false;
    if (FWikiLayer.Visible) then
     FWikiLayer.MouseOnReg(VPWL, Point(x,y));
    if (FLayerMapMarks.Visible) then
     MouseOnMyReg(VPWL,Point(x,y));
    if VPWL.find then
     begin
      stw:='<HTML><BODY>';
      stw:=VPWL.descr;
      stw:=stw+'</BODY></HTML>';
      TextToWebBrowser(stw,Fbrowser.EmbeddedWB1);
      Fbrowser.Visible:=true;
     end;
    exit;
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


procedure TFmain.mapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  i,j:integer;
  nms:string;
  hintrect:TRect;
  CState: Integer;
  VPoint: TPoint;
  VZoomCurr: Byte;
  VSelectionRect: TExtendedRect;
  VConverter: ICoordConverter;
  VLonLat: TExtendedPoint;
begin
  if ProgramClose then begin
    exit;
  end;
  if (Layer <> nil) then begin
    FmoveTrue:=point(x,y);
    exit;
  end;
 if (FMapZoomAnimtion)or(
    (ssDouble in Shift)or(HiWord(GetKeyState(VK_DELETE))<>0)or(HiWord(GetKeyState(VK_INSERT))<>0))
    or(HiWord(GetKeyState(VK_F6))<>0)
   then begin
         FmoveTrue:=point(x,y);
         exit;
        end;
 CState:=ShowCursor(True);
 while CState < 0 do begin
  CState:= ShowCursor(true);
 end;
 sleep(5);
 GState.ViewState.LockRead;
 try
  VConverter := GState.ViewState.GetCurrentCoordConverter;
  VZoomCurr := GState.ViewState.GetCurrentZoom;
  VPoint := GState.ViewState.VisiblePixel2MapPixel(Point(x,y));
  VLonLat := GState.ViewState.VisiblePixel2LonLat(Point(x,y));
 finally
  GState.ViewState.UnLockRead;
 end;
 VConverter.CheckPixelPosStrict(VPoint, VZoomCurr, False);
 if Fmovepoint>0 then
  begin
   Fadd_line_arr[Fmovepoint-1]:=VLonLat;
   TBEditPath.Visible:=(length(Fadd_line_arr)>1);
   FLayerMapNal.DrawNewPath(Fadd_line_arr, (FCurrentOper=ao_add_poly)or(FCurrentOper=ao_edit_poly), Flastpoint);
   exit;
  end;
 if (FCurrentOper=ao_rect)and(Frect_dwn)and(not(ssRight in Shift))
         then begin
               FSelectionRect.BottomRight:=VLonLat;
               VSelectionRect := FSelectionRect;
               PrepareSelectionRect(Shift,VSelectionRect);
               FLayerMapNal.DrawSelectionRect(VSelectionRect);
              end;
 if GState.FullScrean then begin
                       if y<10 then begin
                                     TBDock.Parent:=map;
                                     TBDock.Visible:=true;
                                    end
                               else begin
                                     TBDock.Visible:=false;
                                     TBDock.Parent:=Self;
                                    end;
                       if x<10 then begin
                                     TBDockLeft.Parent:=map;
                                     TBDockLeft.Visible:=true;
                                    end
                               else begin
                                     TBDockLeft.Visible:=false;
                                     TBDockLeft.Parent:=Self;
                                    end;
                       if y>Map.Height-10 then begin
                                     TBDockBottom.Parent:=map;
                                     TBDockBottom.Visible:=true;
                                    end
                               else begin
                                     TBDockBottom.Visible:=false;
                                     TBDockBottom.Parent:=Self;
                                    end;
                       if x>Map.Width-10 then begin
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
              GState.ViewState.MoveTo(Point(FMouseDownPoint.X-x, FMouseDownPoint.Y-y));
              FMainLayer.MoveTo(Point(FMouseDownPoint.X-x, FMouseDownPoint.Y-y));
              FLayerMapNal.MoveTo(Point(FMouseDownPoint.X-x, FMouseDownPoint.Y-y));
              FLayerMapMarks.MoveTo(Point(FMouseDownPoint.X-x, FMouseDownPoint.Y-y));
              FWikiLayer.MoveTo(Point(FMouseDownPoint.X-x, FMouseDownPoint.Y-y));
              FLayerMapGPS.MoveTo(Point(FMouseDownPoint.X-x, FMouseDownPoint.Y-y));
              FLayerFillingMap.MoveTo(Point(FMouseDownPoint.X-x, FMouseDownPoint.Y-y));
              FLayerGoto.MoveTo(Point(FMouseDownPoint.X-x, FMouseDownPoint.Y-y));
              FShowErrorLayer.MoveTo(Point(FMouseDownPoint.X-x, FMouseDownPoint.Y-y));
              LayerMapNavToMark.MoveTo(Point(FMouseDownPoint.X-x, FMouseDownPoint.Y-y));
              finally
                map.EndUpdate;
                map.Invalidate;
              end;
             end
        else MouseCursorPos:=point(x,y);
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
 if not(FMapMoving)and((FmoveTrue.x<>X)or(FmoveTrue.y<>y))and(GState.ShowHintOnMarks) then
  begin
   FPWL.S:=0;
   FPWL.find:=false;
   if (FWikiLayer.Visible) then
     FWikiLayer.MouseOnReg(FPWL,Point(x,y));
   if (FLayerMapMarks.Visible) then
     MouseOnMyReg(FPWL,Point(x,y));
   if (FPWL.find) then
    begin
     if FHintWindow<>nil then FHintWindow.ReleaseHandle;
     if (length(FPWL.name)>0) then
      begin
       if System.Pos('<',FPWL.name)>0 then nms:=HTML2Txt(FPWL.name)
                                     else nms:=FPWL.name;
      end;
     if (length(FPWL.descr)>0) then
      begin
       if length(nms)>0 then nms:=nms+#13#10;
       if System.Pos('<',FPWL.descr)>0 then nms:=nms+HTML2Txt(FPWL.descr)
                                      else nms:=nms+FPWL.descr;
      end;
     i:=1;
     j:=0;
     while (i<length(nms))and(i<>0) do
      begin
       inc(j);
       if (nms[i]=#13)or(nms[i]=#10) then j:=0;
       if (j>40)and(nms[i]=' ')and(length(nms)-i>5)then
        begin
         if i>500 then
          begin
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
     if nms<>'' then
     begin
      if FHintWindow=nil then
       begin
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
 FmoveTrue:=point(x,y);
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

procedure TFmain.N35Click(Sender: TObject);
var
  VLonLat:TExtendedPoint;
  param:string;
  VZoomCurr: Byte;
  VMap: TMapType;
begin
  if SaveLink.Execute then begin
    GState.ViewState.LockRead;
    try
      VZoomCurr := GState.ViewState.GetCurrentZoom;
      VLonLat := GState.ViewState.GetCenterLonLat;
      VMap := GState.ViewState.GetCurrentMap;
    finally
      GState.ViewState.UnLockRead;
    end;
    param:=' '+VMap.GUIDString+' '+inttostr(VZoomCurr + 1)+' '+floattostr(VLonLat.x)+' '+floattostr(VLonLat.y);
    CreateLink(ParamStr(0),SaveLink.filename, '', param)
  end;
end;

procedure TFmain.TBItemDelTrackClick(Sender: TObject);
begin
  GState.GPSpar.GPSRecorder.ClearTrack;
  GState.GPSpar.maxspeed:=0;
end;

procedure TFmain.NGShScale01Click(Sender: TObject);
begin
 GState.GShScale:=TTBXItem(sender).Tag;

 if GState.GShScale >= 1000000 then begin
  GState.GShScale := 1000000;
 end else if GState.GShScale >= 500000 then begin
  GState.GShScale := 500000;
 end else if GState.GShScale >= 200000 then begin
  GState.GShScale := 200000;
 end else if GState.GShScale >= 100000 then begin
  GState.GShScale := 100000;
 end else if GState.GShScale >= 50000 then begin
  GState.GShScale := 50000;
 end else if GState.GShScale >= 25000 then begin
  GState.GShScale := 25000;
 end else if GState.GShScale >= 10000 then begin
  GState.GShScale := 10000;
 end else begin
  GState.GShScale := 0;
 end;

 generate_im;
end;

procedure TFmain.TBEditPathDelClick(Sender: TObject);
begin
 case FCurrentOper of
  ao_line: begin
         if length(Flength_arr)>0 then setlength(Flength_arr,length(Flength_arr)-1);
         TBEditPath.Visible:=(length(Flength_arr)>1);
         FLayerMapNal.DrawLineCalc(Flength_arr, FLenShow);
        end;
  ao_Reg : begin
         if length(Freg_arr)>0 then setlength(Freg_arr,length(Freg_arr)-1);
         TBEditPath.Visible:=(length(Freg_arr)>1);
         FLayerMapNal.DrawReg(Freg_arr);
        end;
  ao_add_poly,ao_add_line,ao_edit_line,ao_edit_poly:
        if Flastpoint>0 then
        begin
         if length(Fadd_line_arr)>0 then delfrompath(Flastpoint);
         TBEditPath.Visible:=(length(Fadd_line_arr)>1);
         FLayerMapNal.DrawNewPath(Fadd_line_arr, (FCurrentOper=ao_add_poly)or(FCurrentOper=ao_edit_poly), Flastpoint);
        end;
 end;
end;

procedure TFmain.TBEditPathLabelClick(Sender: TObject);
begin
  if FCurrentOper = ao_line then begin
    FLenShow:=not(FLenShow);
    FLayerMapNal.DrawLineCalc(Flength_arr, FLenShow);
  end;
end;

procedure TFmain.TBEditPathSaveClick(Sender: TObject);
var result:boolean;
begin
  result := false;
  case FCurrentOper of
    ao_add_Poly: begin
      result:=SavePolyModal(-1, Fadd_line_arr);
    end;
    ao_edit_poly: begin
      result:=SavePolyModal(FEditMarkId, Fadd_line_arr);
    end;
    ao_add_Line: begin
      result:=SaveLineModal(-1, Fadd_line_arr, FMarshrutComment);
    end;
    ao_edit_line: begin
      result:=SaveLineModal(FEditMarkId, Fadd_line_arr, '');
    end;
  end;
  if result then begin
    setalloperationfalse(ao_movemap);
    generate_im;
  end;
end;

procedure TFmain.TBEditPathClose(Sender: TObject);
begin
 setalloperationfalse(ao_movemap);
end;

procedure TFmain.NGoToForumClick(Sender: TObject);
begin
  OpenUrlInBrowser('http://sasgis.ru/forum');
end;

procedure TFmain.NGoToSiteClick(Sender: TObject);
begin
  OpenUrlInBrowser('http://sasgis.ru/');
end;

procedure TFmain.TBItem6Click(Sender: TObject);
begin
 FMarksExplorer.ShowModal;
 generate_im;
end;

procedure TFmain.NSRTM3Click(Sender: TObject);
var
  VLonLat:TExtendedPoint;
begin
  VLonLat := GState.ViewState.VisiblePixel2LonLat(FMouseDownPoint);
  TextToWebBrowser(SAS_STR_WiteLoad, Fbrowser.EmbeddedWB1);
  Fbrowser.Visible := true;
  Fbrowser.EmbeddedWB1.Navigate('http://ws.geonames.org/srtm3?lat='+R2StrPoint(VLonLat.y)+'&lng='+R2StrPoint(VLonLat.x));
end;

procedure TFmain.NGTOPO30Click(Sender: TObject);
var
  VLonLat:TExtendedPoint;
begin
  VLonLat := GState.ViewState.VisiblePixel2LonLat(FMouseDownPoint);
  TextToWebBrowser(SAS_STR_WiteLoad,Fbrowser.EmbeddedWB1);
  Fbrowser.Visible:=true;
  Fbrowser.EmbeddedWB1.Navigate('http://ws.geonames.org/gtopo30?lat='+R2StrPoint(VLonLat.y)+'&lng='+R2StrPoint(VLonLat.x));
end;

procedure TFmain.NMarkNavClick(Sender: TObject);
var
  LL:TExtendedPoint;
  id:integer;
  VMark: TMarkFull;
begin
 FWikiLayer.MouseOnReg(FPWL, FmoveTrue);
 if (not NMarkNav.Checked) then begin
   id:=strtoint(FPWL.numid);
   VMark := GState.MarksDb.GetMarkByID(id);
   if VMark = nil then Exit;
   try
     LL := VMark.GetGoToLonLat;
     LayerMapNavToMark.StartNav(LL, Id);
   finally
    VMark.Free;
   end;
  end else  begin
    LayerMapNavToMark.Visible := false;
  end;
end;

function SecondToTime(const Seconds: Cardinal): Double;
const
  SecPerDay = 86400;
  SecPerHour = 3600;
  SecPerMinute = 60;
var
  ms, ss, mm, hh, dd: Cardinal; 
begin 
  dd := Seconds div SecPerDay;
  hh := (Seconds mod SecPerDay) div SecPerHour; 
  mm := ((Seconds mod SecPerDay) mod SecPerHour) div SecPerMinute; 
  ss := ((Seconds mod SecPerDay) mod SecPerHour) mod SecPerMinute; 
  ms := 0; 
  Result := dd + EncodeTime(hh, mm, ss, ms); 
end; 

procedure TFmain.TBEditPathMarshClick(Sender: TObject);
var ms:TMemoryStream;
    pathstr,timeT1:string;
    url:string;
    i,posit,posit2,endpos,dd,seconds,meters:integer;
    Buffer:array [1..64535] of char;
    BufferLen:LongWord;
    dateT1:TDateTime;
begin
 ms:=TMemoryStream.Create;
 case TTBXItem(Sender).tag of
  1:url:='http://maps.mail.ru/stamperx/getPath.aspx?mode=distance';
  2:url:='http://maps.mail.ru/stamperx/getPath.aspx?mode=time';
  3:url:='http://maps.mail.ru/stamperx/getPath.aspx?mode=deftime';
 end;
 for i:=0 to length(Fadd_line_arr)-1 do
  url:=url+'&x'+inttostr(i)+'='+R2StrPoint(Fadd_line_arr[i].x)+'&y'+inttostr(i)+'='+R2StrPoint(Fadd_line_arr[i].y);
 if GetStreamFromURL(ms,url,'text/javascript; charset=utf-8')>0 then
  begin
   ms.Position:=0;
   pathstr:='';
   repeat
    BufferLen:=ms.Read(Buffer,SizeOf(Buffer));
    pathstr:=pathstr+Buffer;
   until (BufferLen=0)or(BufferLen<SizeOf(Buffer));

   SetLength(Fadd_line_arr,0);
   meters:=0;
   seconds:=0;

   try
   posit:=PosEx('"totalLength"',pathstr,1);
   While (posit>0) do
    begin
     try
      posit2:=PosEx('"',pathstr,posit+17);
      meters:=meters+strtoint(copy(pathstr,posit+17,posit2-(posit+17)));
      posit:=PosEx('"totalTime"',pathstr,posit);
      posit2:=PosEx('"',pathstr,posit+15);
      seconds:=seconds+strtoint(copy(pathstr,posit+15,posit2-(posit+15)));
     except
     end;
     posit:=PosEx('"points"',pathstr,posit);
     endpos:=PosEx(']',pathstr,posit);
      while (posit>0)and(posit<endpos) do
       try
        SetLength(Fadd_line_arr,length(Fadd_line_arr)+1);
        posit:=PosEx('"x" : "',pathstr,posit);
        posit2:=PosEx('", "y" : "',pathstr,posit);
        Fadd_line_arr[length(Fadd_line_arr)-1].X:=str2r(copy(pathstr,posit+7,posit2-(posit+7)));
        posit:=PosEx('"',pathstr,posit2+10);
        Fadd_line_arr[length(Fadd_line_arr)-1].y:=str2r(copy(pathstr,posit2+10,posit-(posit2+10)));
        posit:=PosEx('{',pathstr,posit);
       except
        SetLength(Fadd_line_arr,length(Fadd_line_arr)-1);
        dec(Flastpoint);
       end;
     posit:=PosEx('"totalLength"',pathstr,posit);
    end;
   except
   end;

   Flastpoint:=length(Fadd_line_arr)-1;
   if meters>1000 then FMarshrutComment:=SAS_STR_MarshLen+RoundEx(meters/1000,2)+' '+SAS_UNITS_km
                  else FMarshrutComment:=SAS_STR_MarshLen+inttostr(meters)+' '+SAS_UNITS_m;
   DateT1:=SecondToTime(seconds);
   dd:=DaysBetween(0,DateT1);
   timeT1:='';
   if dd>0 then timeT1:=inttostr(dd)+' дней, ';
   timeT1:=timeT1+TimeToStr(DateT1);
   FMarshrutComment:=FMarshrutComment+#13#10+SAS_STR_Marshtime+timeT1;
  end
 else ShowMessage('Connect error!');
 TBEditPath.Visible:=(length(Fadd_line_arr)>1);
 FLayerMapNal.DrawNewPath(Fadd_line_arr, (FCurrentOper=ao_add_poly)or(FCurrentOper=ao_edit_poly), Flastpoint);
end;

procedure TFmain.AdjustFont(Item: TTBCustomItem;
  Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
begin
 if TTBXItem(Item).Checked then TTBXItem(Item).FontSettings.Bold:=tsTrue
                           else TTBXItem(Item).FontSettings.Bold:=tsDefault;
end;

procedure TFmain.NParamsClick(Sender: TObject);
var
  i:Integer;
  VMapType: TMapType;
  VLayerIsActive: Boolean;
begin
  NLayerParams.Visible:=false;
  For i:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[i];
    if (VMapType.asLayer) then begin
      VLayerIsActive := GState.ViewState.IsHybrGUIDSelected(VMapType.GUID);
      TTBXItem(FNLayerParamsItemList.GetByGUID(VMapType.GUID)).Visible := VLayerIsActive;
      if VLayerIsActive then begin
        NLayerParams.Visible:=true;
      end
    end;
  end;
end;

procedure TFmain.TBfillMapAsMainClick(Sender: TObject);
var
  VItem: TTBXItem;
  VFillingMapType: TMapType;
begin
  VItem := Sender as TTBXItem;
  if VItem.Tag = 0 then begin
    VFillingMapType := nil;
  end else begin
    VFillingMapType := TMapType(VItem.Tag);
  end;
  FLayerFillingMap.SetSourceMap(VFillingMapType, FLayerFillingMap.SourceZoom);
end;

procedure TFmain.NMarksCalcsLenClick(Sender: TObject);
var
  VId: Integer;
  VMark: TMarkFull;
  VLen: Extended;
  VMessage: string;
begin
  VId := strtoint(FPWL.numid);
  VMark := GState.MarksDb.GetMarkByID(VId);
  if VMark <> nil then begin
    try
      VLen := GetMarkLength(VMark);
      VMessage := SAS_STR_L+' - '+DistToStrWithUnits(VLen, GState.num_format);
      MessageBox(Self.Handle, pchar(VMessage), pchar(FPWL.name),0);
    finally
      VMark.Free;
    end;
  end;
end;

procedure TFmain.NMarksCalcsSqClick(Sender: TObject);
var
  VId: Integer;
  VMark: TMarkFull;
  VArea: Extended;
  VMessage: string;
begin
  VId := strtoint(FPWL.numid);
  VMark := GState.MarksDb.GetMarkByID(VId);
  if VMark <> nil then begin
    try
      VArea := GetMarkSq(VMark);
      if VArea < 0.1 then begin
        VMessage := SAS_STR_S+' - '+RoundEx(VArea * 1000000,2)+' '+SAS_UNITS_m2;
      end else begin
        VMessage := SAS_STR_S+' - '+RoundEx(VArea,2)+' '+SAS_UNITS_km2;
      end;
      MessageBox(Handle,pchar(VMessage),pchar(FPWL.name),0);
    finally
      VMark.Free;
    end;
  end;
end;

procedure TFmain.NMarksCalcsPerClick(Sender: TObject);
var
  VId: Integer;
  VMark: TMarkFull;
  VLen: Extended;
  VMessage: string;
begin
  VId := strtoint(FPWL.numid);
  VMark := GState.MarksDb.GetMarkByID(VId);
  if VMark <> nil then begin
    try
      VLen := GetMarkLength(VMark);
      VMessage := SAS_STR_P+' - '+DistToStrWithUnits(VLen, GState.num_format);
      MessageBox(Self.Handle, pchar(VMessage), pchar(FPWL.name),0);
    finally
      VMark.Free;
    end;
  end;
end;

procedure TFmain.TBEditPathOkClick(Sender: TObject);
begin
  case FCurrentOper of
   ao_reg: begin
         SetLength(Freg_arr,length(Freg_arr)+1);
         Freg_arr[length(Freg_arr)-1]:=Freg_arr[0];
         FLayerMapNal.DrawNothing;
         Fsaveas.Show_(GState.ViewState.GetCurrentZoom,Freg_arr);
         setalloperationfalse(ao_movemap);
        end;
  end;
end;

procedure TFmain.TBItem1Click(Sender: TObject);
begin
 case TTBXItem(Sender).tag of
  0:GState.LanguageManager.SetCurrentLanguage('ru');
  1:GState.LanguageManager.SetCurrentLanguage('en');
 end;
end;

procedure TFmain.NMapInfoClick(Sender: TObject);
var
  VMap: TMapType;
begin
 VMap := GState.ViewState.GetCurrentMap;
 ShowMessage('Файл: '+VMap.zmpfilename+#13#10+VMap.MapInfo);
end;

procedure TFmain.WebBrowser1Authenticate(Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName, szPassWord: WideString; var Rezult: HRESULT);
begin
 if GState.InetConnect.uselogin then
  begin
   szUserName:=GState.InetConnect.loginstr;
   szPassWord:=GState.InetConnect.passstr;
  end;
end;

procedure TFmain.NanimateClick(Sender: TObject);
begin
  GState.AnimateZoom := Nanimate.Checked;
end;

procedure TFmain.SaveWindowConfigToIni(AProvider: IConfigDataWriteProvider);
var
  lock_tb_b:boolean;
begin
  GState.MainIni.Writebool('VIEW','Maximized',WindowState=wsMaximized);
  GState.MainIni.WriteInteger('VIEW','FLeft',Left);
  GState.MainIni.WriteInteger('VIEW','FTop',Top);
  GState.MainIni.WriteInteger('VIEW','FWidth',Width);
  GState.MainIni.WriteInteger('VIEW','FHeight',Height);

  FLayersList.SaveConfig(AProvider);

  GState.MainIni.WriteBool('PANEL','lock_toolbars',lock_toolbars);
  lock_tb_b:=lock_toolbars;
  lock_toolbars:=false;
  TBConfigProviderSavePositions(Self, GState.MainConfigProvider.GetOrCreateSubItem('PANEL'));
  lock_toolbars:=lock_tb_b;
end;

procedure TFmain.SBClearSensorClick(Sender: TObject);
begin
 if (MessageBox(handle,pchar(SAS_MSG_youasurerefrsensor+'?'),pchar(SAS_MSG_coution),36)=IDYES) then begin
   case TSpeedButton(sender).Tag of
    1: GState.GPSpar.sspeed:=0;
    2: GState.GPSpar.len:=0;
    3: GState.GPSpar.Odometr:=0;
    4: GState.GPSpar.maxspeed:=0;
    5: GState.GPSpar.Odometr2:=0;
   end;
   UpdateGPSsensors;
 end;
end;

procedure TFmain.TBXSensorsBarVisibleChanged(Sender: TObject);
begin
  UpdateGPSsensors;
  TTBXItem(FindComponent('N'+copy(TTBXToolWindow(sender).Name,4,length(TTBXItem(sender).Name)-3))).Checked:=TTBXToolWindow(sender).Visible;
end;

procedure TFmain.NSensorsBarClick(Sender: TObject);
begin
  TTBXToolWindow(FindComponent('TBX'+copy(TTBXItem(sender).Name,2,length(TTBXItem(sender).Name)-1))).Visible:=TTBXItem(sender).Checked;
end;

procedure TFmain.TBXItem1Click(Sender: TObject);
var ms:TMemoryStream;
    url:string;
    i:integer;
    kml:TKmlInfoSimple;
    s,l:integer;
    conerr:boolean;
    add_line_arr_b:TExtendedPointArray;
begin
 ms:=TMemoryStream.Create;
 case TTBXItem(sender).Tag of
    1: url:='http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=1&layer=mapnik';
   11: url:='http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=0&layer=mapnik';
    2: url:='http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=1&layer=mapnik';
   22: url:='http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=0&layer=mapnik';
 end;
 conerr:=false;
 for i:=0 to length(Fadd_line_arr)-2 do begin
 if conerr then Continue;
 url:=url+'&flat='+R2StrPoint(Fadd_line_arr[i].y)+'&flon='+R2StrPoint(Fadd_line_arr[i].x)+
          '&tlat='+R2StrPoint(Fadd_line_arr[i+1].y)+'&tlon='+R2StrPoint(Fadd_line_arr[i+1].x);
 if GetStreamFromURL(ms,url,'text/xml')>0 then
  begin
   kml:=TKmlInfoSimple.Create;
   GState.KmlLoader.LoadFromStream(ms, kml);
   ms.SetSize(0);
   if (length(kml.Data)>0)and(length(kml.Data[0].coordinates)>0) then begin
     s:=length(add_line_arr_b);
     l:=length(kml.Data[0].coordinates);
     SetLength(add_line_arr_b,(s+l));
     Move(kml.Data[0].coordinates[0],add_line_arr_b[s],l*sizeof(TExtendedPoint));
   end;
   kml.Free;
  end
 else conerr:=true;
 end;
 ms.Free;
 if conerr then ShowMessage('Connect error!');
 if (not conerr)and(length(add_line_arr_b)>0) then begin
   Fadd_line_arr:=add_line_arr_b;
   SetLength(add_line_arr_b,0);
   Flastpoint:=length(Fadd_line_arr)-1;
 end;
 TBEditPath.Visible:=(length(Fadd_line_arr)>1);
  FLayerMapNal.DrawNewPath(Fadd_line_arr, (FCurrentOper=ao_add_poly)or(FCurrentOper=ao_edit_poly), Flastpoint);
end;

procedure TFmain.TBXItem5Click(Sender: TObject);
begin
  if GState.GPSpar.GPSModele.IsConnected then begin
    if AddNewPointModal(GState.GPSpar.GPSRecorder.GetLastPoint) then begin
      setalloperationfalse(ao_movemap);
      generate_im;
    end;
  end;
end;

procedure TFmain.TBXSelectYandexSrchClick(Sender: TObject);
begin
 TTBXItem(Sender).Checked:=true;
 GState.SrchType:=TSrchType(TTBXItem(Sender).tag);
 TBXSelectSrchType.Caption:=TTBXItem(Sender).Caption;
end;

procedure TFmain.TBXSearchEditAcceptText(Sender: TObject;
  var NewText: String; var Accept: Boolean);
begin
 case GState.SrchType of
  stGoogle: EditGoogleSrchAcceptText(Sender,NewText, Accept);
  stYandex: TBEditItem1AcceptText(Sender,NewText, Accept);
 end;
end;

procedure TFmain.tbitmPositionByGSMClick(Sender: TObject);
var
  PosFromGPS:TPosFromGPS;
begin
 PosFromGPS:=TPosFromGPS.Create;
 PosFromGPS.Port:='\\.\'+GState.GSMpar.Port;
 PosFromGPS.BaundRate:=GState.GSMpar.BaudRate;
 PosFromGPS.OnToPos:=topos;
 PosFromGPS.GetPos;
end;

procedure TFmain.TBXItem6Click(Sender: TObject);
var
  VLog: TLogForTaskThread;
  VSimpleLog: ILogSimple;
  VThreadLog:ILogForTaskThread;
  VThread: TThreadDownloadTiles;
begin
  if (OpenSessionDialog.Execute)and(FileExists(OpenSessionDialog.FileName)) then begin
    if ExtractFileExt(OpenSessionDialog.FileName)='.sls' then begin
      VLog := TLogForTaskThread.Create(5000, 0);
      VSimpleLog := VLog;
      VThreadLog := VLog;
      VThread := TThreadDownloadTiles.Create(VSimpleLog, OpenSessionDialog.FileName, GState.SessionLastSuccess);
      TFProgress.Create(Application, VThread, VThreadLog);
    end else begin
      if (ExtractFileExt(OpenSessionDialog.FileName)='.kml')or
         (ExtractFileExt(OpenSessionDialog.FileName)='.kmz')or
         (ExtractFileExt(OpenSessionDialog.FileName)='.plt') then begin
        FImport.ImportFile(OpenSessionDialog.FileName);
      end else begin
        if ExtractFileExt(OpenSessionDialog.FileName)='.hlg' then begin
          Fsaveas.LoadSelFromFile(OpenSessionDialog.FileName);
        end;
      end;
    end;
  end;
end;

procedure TFmain.TBGPSToPointCenterClick(Sender: TObject);
begin
 tbitmGPSToPointCenter.Checked:=TTBXitem(sender).Checked;
 TBGPSToPointCenter.Checked:=TTBXitem(sender).Checked;
 GState.GPSpar.GPS_MapMoveCentered:=TTBXitem(sender).Checked;
end;

procedure TFmain.NShowSelectionClick(Sender: TObject);
begin
  FLayerSelection.Visible := TTBXItem(sender).Checked;
end;

procedure TFmain.MouseOnMyReg(var APWL: TResObj; xy: TPoint);
var
  j:integer;
  arLL: TPointArray;
  poly:TExtendedPointArray;
  VLonLatRect: TExtendedRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VMarkLonLatRect: TExtendedRect;
  VPixelPos: TPoint;
  VZoom: Byte;
  VMarksIterator: TMarksIteratorBase;
  VMark: TMarkFull;
begin
  if GState.show_point = mshNone then exit;

  VRect.Left := xy.X - 8;
  VRect.Top := xy.Y - 16;
  VRect.Right := xy.X + 8;
  VRect.Bottom := xy.Y + 16;

  GState.ViewState.LockRead;
  try
    VLonLatRect.TopLeft := GState.ViewState.VisiblePixel2LonLat(VRect.TopLeft);
    VLonLatRect.BottomRight := GState.ViewState.VisiblePixel2LonLat(VRect.BottomRight);
    VConverter := GState.ViewState.GetCurrentCoordConverter;
    VZoom := GState.ViewState.GetCurrentZoom;
    VPixelPos := GState.ViewState.VisiblePixel2MapPixel(xy);
  finally
    GState.ViewState.UnLockRead;
  end;
  VMarksIterator := GetMarksIterator(VZoom, VLonLatRect, GState.show_point);
  try
    While VMarksIterator.Next do begin
      VMark := VMarksIterator.Current;
      VMarkLonLatRect := VMark.LLRect;

      if((VLonLatRect.Right>VMarkLonLatRect.Left)and(VLonLatRect.Left<VMarkLonLatRect.Right)and
      (VLonLatRect.Bottom<VMarkLonLatRect.Top)and(VLonLatRect.Top>VMarkLonLatRect.Bottom))then begin
        if VMark.IsPoint then begin
          APWL.name:=VMark.name;
          APWL.descr:=VMark.Desc;
          APWL.numid:=IntToStr(VMark.id);
          APWL.find:=true;
          APWL.type_:=ROTpoint;
          exit;
        end else begin
          poly := VMark.Points;
          arLL := VConverter.LonLatArray2PixelArray(poly, VZoom);
          if VMark.IsLine then begin
            j:=1;
            while (j<length(poly)) do begin
              if CursorOnLinie(VPixelPos.x,VPixelPos.Y,arLL[j-1].x,arLL[j-1].y,arLL[j].x,arLL[j].y,(VMark.Scale1 div 2)+1)
              then begin
                APWL.name:=VMark.name;
                APWL.descr:=VMark.Desc;
                APWL.numid:=IntToStr(VMark.id);
                APWL.find:=true;
                APWL.type_:=ROTline;
                exit;
              end;
              inc(j);
            end
          end else begin
            if (PtInRgn(arLL,VPixelPos)) then begin
              if ((not(APWL.find))or((PolygonSquare(arLL)<APWL.S)and(APWL.S <> 0))) then begin
                APWL.S:=PolygonSquare(arLL);
                APWL.name:=VMark.name;
                APWL.descr:=VMark.Desc;
                APWL.numid:=IntToStr(VMark.id);
                APWL.find:=true;
                APWL.type_:=ROTPoly;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    VMarksIterator.Free;
 end;
end;

procedure TFmain.NGoToCurClick(Sender: TObject);
begin
  GState.ZoomingAtMousePos := (Sender as TTBXItem).Checked
end;

procedure TFmain.InitSearchers;
var
  VGoto: IMapViewGoto;
  VProxy: IProxySettings;
begin
  VGoto := TMapViewGotoOnFMain.Create;
  FSearchPresenter := TSearchResultPresenterWithForm.Create(VGoto);
  VProxy := TProxySettingsFromTInetConnect.Create(GState.InetConnect);
  FGoogleGeoCoder := TGeoCoderByGoogle.Create(VProxy);
  FYandexGeoCoder := TGeoCoderByYandex.Create(VProxy);
end;

procedure TFmain.TBXItem8Click(Sender: TObject);
begin
  OpenUrlInBrowser('http://z.sasgis.ru/show_zmp/sas.zmp');
end;

procedure TFmain.TBXItem9Click(Sender: TObject);
begin
  OpenUrlInBrowser('http://z.sasgis.ru/show_zmp/plus.zmp');
end;

end.

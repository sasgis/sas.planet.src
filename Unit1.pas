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
  ComCtrls,
  ShlObj,
  ComObj,
  Graphics,
  StdCtrls,
  OleCtrls,
  Controls,
  Buttons,
  DB,
  DBClient,
  WinInet,
  Dialogs,
  ExtDlgs,
  ImgList,
  GR32,
  GR32_Resamplers,
  GR32_Layers,
  GR32_Filters,
  GR32_Image,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  RXSlider,
  EmbeddedWB,
  TB2ExtItems,
  SHDocVw_EWB,
  TB2ToolWindow,
  TBXToolPals,
  EwbCore,
  TBX,
  TBXControls,
  TBXExtItems,
  ZylGPSReceiver,
  ZylCustomGPSReceiver,
  MidasLib,
  i_JclNotify,
  i_IGUIDList,
  i_IMapChangeMessage,
  i_IHybrChangeMessage,
  i_IPosChangeMessage,
  t_LoadEvent,
  u_GeoToStr,
  t_CommonTypes,
  Ugeofun,
  u_MapLayerWiki,
  ULogo,
  UMapType,
  UResStrings,
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
  u_TileDownloaderUI,
  u_SelectionLayer,
  u_GeoSearcher,
  t_GeoTypes;

type
  TTileSource = (tsInternet,tsCache,tsCacheInternet);

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

  TFmain = class(TForm)
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
    DataSource1: TDataSource;
    NMarkDel: TMenuItem;
    NMarkEdit: TMenuItem;
    NMarkSep: TMenuItem;
    NMarkOper: TMenuItem;
    livecom1: TMenuItem;
    N13: TMenuItem;
    ImageAtlas1: TMenuItem;
    SaveDialog1: TSaveDialog;
    N26: TMenuItem;
    N27: TMenuItem;
    DigitalGlobe1: TMenuItem;
    ldm: TMenuItem;
    dlm: TMenuItem;
    GPSReceiver: TZylGPSReceiver;
    SaveLink: TSaveDialog;
    DSKategory: TDataSource;
    CDSKategory: TClientDataSet;
    CDSmarks: TClientDataSet;
    CDSKategoryid: TAutoIncField;
    CDSKategoryname: TStringField;
    CDSKategoryvisible: TBooleanField;
    CDSKategoryAfterScale: TSmallintField;
    CDSKategoryBeforeScale: TSmallintField;
    CDSmarksid: TAutoIncField;
    CDSmarksname: TStringField;
    CDSmarksdescr: TMemoField;
    CDSmarksscale1: TIntegerField;
    CDSmarksscale2: TIntegerField;
    CDSmarkslonlatarr: TBlobField;
    CDSmarkslonL: TFloatField;
    CDSmarkslatT: TFloatField;
    CDSmarksLonR: TFloatField;
    CDSmarksLatB: TFloatField;
    CDSmarkscolor1: TIntegerField;
    CDSmarkscolor2: TIntegerField;
    CDSmarksvisible: TBooleanField;
    CDSmarkspicname: TStringField;
    CDSmarkscategoryid: TIntegerField;
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
    NGPS: TTBXSubmenuItem;
    NParams: TTBXSubmenuItem;
    NLayerParams: TTBXSubmenuItem;
    NHelp: TTBXSubmenuItem;
    NMarksCalcs: TMenuItem;
    NMarksCalcsLen: TMenuItem;
    NMarksCalcsSq: TMenuItem;
    NMarksCalcsPer: TMenuItem;
    WebBrowser1: TEmbeddedWB;
    ImageList1: TImageList;
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
    TBGPSToPoint: TTBXItem;
    TBSrc: TTBXSubmenuItem;
    TBSMB: TTBXSubmenuItem;
    TBLayerSel: TTBXSubmenuItem;
    ImagesSrc24: TTBImageList;
    MapIcons24: TTBImageList;
    MapIcons18: TTBImageList;
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
    NGPSconn: TTBXItem;
    NGPSPath: TTBXItem;
    NGPSToPoint: TTBXItem;
    NSaveTreck: TTBXItem;
    N36: TTBXItem;
    N39: TTBXItem;
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
    TBXSeparatorItem13: TTBXSeparatorItem;
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
    TBXSeparatorItem18: TTBXSeparatorItem;
    TBXItem7: TTBXItem;
    TBXItem6: TTBXItem;
    OpenSessionDialog: TOpenDialog;
    NShowSelection: TTBXItem;
    TBRECT: TTBXItem;
    TBREGION: TTBXItem;
    TBCOORD: TTBXItem;
    TBPrevious: TTBXItem;
    TBLoadSelFromFile: TTBXItem;
    TBXSignalStrengthBar: TTBXToolWindow;
    TBXSignalStrength: TTBXLabel;
    TBXLabel5: TTBXLabel;
    NSignalStrengthBar: TTBXItem;
    TBXSeparatorItem19: TTBXSeparatorItem;
    TBXItem8: TTBXItem;
    TBXItem9: TTBXItem;
    TBControlItem3: TTBControlItem;
    Label1: TLabel;
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
    procedure GPSReceiver1SatellitesReceive(Sender: TObject);
    procedure GPSReceiverReceive(Sender: TObject);
    procedure GPSReceiverDisconnect(Sender: TObject; const Port: TCommPort);
    procedure GPSReceiverConnect(Sender: TObject; const Port: TCommPort);
    procedure GPSReceiverTimeout(Sender: TObject);
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
    procedure TBXItem7Click(Sender: TObject);
    procedure TBXItem6Click(Sender: TObject);
    procedure NShowSelectionClick(Sender: TObject);
    procedure NGoToCurClick(Sender: TObject);
    procedure TBXItem8Click(Sender: TObject);
    procedure TBXItem9Click(Sender: TObject);
  private
    nilLastLoad: TLastLoad;
    ShowActivHint: boolean;
    HintWindow: THintWindow;
    Flock_toolbars: boolean;
    rect_dwn: Boolean;
    rect_p2: boolean;
    FTileSource: TTileSource;
    FMainLayer: TMapMainLayer;
    LayerStatBar: TLayerStatBar;
    FShowErrorLayer: TTileErrorInfoLayer;
    FWikiLayer: TWikiLayer;
    dWhenMovingButton: integer;
    LenShow: boolean;
    RectWindow: TRect;
    FUIDownLoader: TTileDownloaderUI;
    marshrutcomment: string;
    movepoint: integer;
    lastpoint: integer;
    FSelectionRect: TExtendedRect;
    length_arr: TExtendedPointArray;
    add_line_arr: TExtendedPointArray;
    reg_arr: TExtendedPointArray;
    FPWL: TResObj;
    LayerScaleLine: TLayerScaleLine;
    LayerMapNal: TMapNalLayer;
    LayerMapGPS: TMapGPSLayer;
    LayerGoto: TGotoLayer;
    ProgramStart: Boolean;
    ProgramClose: Boolean;
    FMapPosChangeListener: IJclListener;
    FMainMapChangeListener: IJclListener;
    FHybrChangeListener: IJclListener;

    FMainToolbarItemList: IGUIDObjectList; //Пункт списка в главном тулбаре
    FMainToolbarSubMenuItemList: IGUIDObjectList; //Подпункт списка в главном тулбаре
    FTBFillingItemList: IGUIDObjectList; //Пункт главного меню Вид/Карта заполнения/Формировать для
    FNLayerParamsItemList: IGUIDObjectList; //Пункт гланого меню Параметры/Параметры слоя
    FNDwnItemList: IGUIDObjectList; //Пункт контекстного меню Загрузить тайл слоя
    FNDelItemList: IGUIDObjectList; //Пункт контекстного меню Удалить тайл слоя

    procedure DoMessageEvent(var Msg: TMsg; var Handled: Boolean);
    procedure WMGetMinMaxInfo(var msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure Set_lock_toolbars(const Value: boolean);
    procedure Set_TileSource(const Value: TTileSource);
    procedure MouseOnMyReg(var APWL:TResObj;xy:TPoint);
    procedure InitSearchers;
    procedure zooming(ANewZoom: byte; move: boolean);
    function PrepareSelectionRect(Shift: TShiftState; var ASelectedLonLat: TExtendedRect): Boolean;
    procedure insertinpath(pos: integer);
    procedure delfrompath(pos: integer);
    procedure ProcessPosChangeMessage(AMessage: IPosChangeMessage);
    procedure ProcessMapChangeMessage(AMessage: IMapChangeMessage);
    procedure ProcessHybrChangeMessage(AMessage: IHybrChangeMessage);
    procedure CopyBtmToClipboard(btm: TBitmap);
    function GetStreamFromURL(var ms: TMemoryStream; url: string; conttype: string): integer;
  public
    FGoogleSearch: TGeoSearcher;
    FYandexSerach: TGeoSearcher;
    FFillingMap: TMapFillingLayer;
    LayerMapMarks: TMapMarksLayer;
    LayerMapNavToMark: TNavToMarkLayer;
    LayerMapScale: TCenterScale;
    LayerSelection: TSelectionLayer;
    FMiniMapLayer: TMiniMapLayer;
    MouseDownPoint: TPoint;
    MouseUpPoint: TPoint;
    m_m: Tpoint;
    moveTrue: Tpoint;
    MapMoving: Boolean;
    MapZoomAnimtion: Integer;
    change_scene: boolean;
    aoper: TAOperation;
    GPSpar: TGPSpar;
    EditMarkId:integer;
    property lock_toolbars: boolean read Flock_toolbars write Set_lock_toolbars;
    property TileSource: TTileSource read FTileSource write Set_TileSource;
    procedure generate_im(lastload: TLastLoad; err: string); overload;
    procedure generate_im; overload;
    procedure topos(LL: TExtendedPoint; zoom_: byte; draw: boolean);
    procedure selectMap(AMapType: TMapType);
    procedure OpenUrlInBrowser(URL: string);
    procedure CreateMapUI;
    procedure ShowErrScript(DATA: string);
    procedure setalloperationfalse(newop: TAOperation);
    procedure SetStatusBarVisible();
    procedure SetLineScaleVisible(visible: boolean);
    procedure SetMiniMapVisible(visible: boolean);
    procedure UpdateGPSsensors;
    procedure CopyStringToClipboard(s: Widestring);
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
  Unit2,
  UAbout,
  Usettings,
  USaveas,
  UProgress,
  Unit4,
  USelLonLat,
  UImgFun,
  UEditMap,
  Ubrowser,
  UMarksExplorer,
  UFDGAvailablePic,
  u_TileDownloaderUIOneTile,
  u_LogForTaskThread,
  i_ILogSimple,
  i_ILogForTaskThread,
  i_ICoordConverter,
  u_KmlInfoSimple,
  i_IMapViewGoto,
  u_MapViewGotoOnFMain,
  i_ISearchResultPresenter,
  frm_SearchResults,
  i_GeoCoder,
  i_IProxySettings,
  u_ProxySettingsFromTInetConnect,
  u_GeoCoderByGoogle,
  u_GeoCoderByYandex,
  u_MarksSimple,
  u_MarksReadWriteSimple,
  UTrAllLoadMap,
  UGSM,
  UImport, 
  u_MapViewPortState,
  UShortcutEditor;

{$R *.dfm}

type

{ TListenerOfMainForm }

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

function GetClipboardText(Wnd: HWND; var Str: string): Boolean;
var
  hData: HGlobal;
begin
  Result := True;
  if OpenClipboard(Wnd) then
  begin
    try
      hData := GetClipboardData(CF_TEXT);
      if hData <> 0 then
      begin
        try
          SetString(Str, PChar(GlobalLock(hData)), GlobalSize(hData));
        finally
          GlobalUnlock(hData);
        end;
      end
      else
        Result := False;
      Str := PChar(@Str[1]);
    finally
      CloseClipboard;
    end;
  end
  else
    Result := False;
end;

procedure TFmain.ProcessHybrChangeMessage(AMessage: IHybrChangeMessage);
var
  i:integer;
  VMapType: TMapType;
begin
  for i:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[i];
    if VMapType.asLayer then begin
      TTBXItem(FMainToolbarItemList.GetByGUID(VMapType.GUID)).Checked := GState.ViewState.IsHybrGUIDSelected(VMapType.GUID);
    end;
  end;
  generate_im;
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
  TBSMB.ImageIndex := VMainToolbarItem.ImageIndex;
  VMainToolbarItem.Checked:=true;
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
  labZoom.caption:=' '+inttostr(VZoomCurr + 1)+'x ';
  change_scene:=true;
  LayerStatBar.Redraw;
  LayerScaleLine.Redraw;

  FMainLayer.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
  FFillingMap.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
  LayerSelection.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
  LayerMapMarks.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
  LayerMapNal.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
  FWikiLayer.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
  LayerMapGPS.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
  LayerGoto.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
  LayerMapNavToMark.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
  FShowErrorLayer.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
  FMiniMapLayer.SetScreenCenterPos(VPoint, VZoomCurr, VConverter);
  QueryPerformanceCounter(ts3);
  QueryPerformanceFrequency(fr);
  Label1.caption :=FloatToStr((ts3-ts2)/(fr/1000));
end;

procedure TFMain.Set_TileSource(const Value: TTileSource);
begin
 FTileSource:=Value;
 TBSrc.ImageIndex:=integer(Value);
 case Value of
  tsInternet: NSRCinet.Checked:=true;
  tsCache: NSRCesh.Checked:=true;
  tsCacheInternet: NSRCic.Checked:=true;
 end;
 change_scene:=true
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

procedure TFmain.insertinpath(pos: integer);
begin
 SetLength(add_line_arr,length(add_line_arr)+1);
 CopyMemory(Pointer(integer(@add_line_arr[pos])+sizeOf(TExtendedPoint)),@add_line_arr[pos],(length(add_line_arr)-pos-1)*sizeOf(TExtendedPoint));
end;

procedure TFmain.delfrompath(pos: integer);
begin
 CopyMemory(@add_line_arr[pos],Pointer(integer(@add_line_arr[pos])+sizeOf(TExtendedPoint)),(length(add_line_arr)-pos-1)*sizeOf(TExtendedPoint));
 SetLength(add_line_arr,length(add_line_arr)-1);
 Dec(lastpoint);
end;

procedure TFmain.setalloperationfalse(newop: TAOperation);
begin
 if aoper=newop then newop:=ao_movemap;
 LayerMapNal.DrawNothing;
 marshrutcomment:='';
 LayerMapNal.Visible:=newop<>ao_movemap;
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
 rect_dwn:=false;
 setlength(length_arr,0);
 setlength(add_line_arr,0);
 setlength(reg_arr,0);
 rect_p2:=false;
 lastpoint:=-1;
 case newop of
  ao_movemap:  map.Cursor:=crDefault;
  ao_line:     map.Cursor:=2;
  ao_reg,ao_rect: map.Cursor:=crDrag;
  ao_Add_Point,ao_Add_Poly,ao_Add_Line,ao_edit_Line,ao_edit_poly: map.Cursor:=4;
 end;
 if (aoper=ao_edit_line)or(aoper=ao_edit_poly) then begin
   EditMarkId:=-1;
   LayerMapMarks.Redraw;
 end;
 aoper:=newop;
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
begin

 if Active then
  case Msg.message of
   WM_MOUSEWHEEL: if MapZoomAnimtion=0 then
                 begin
                  m_m:=moveTrue;
                  if GState.MouseWheelInv then z:=-1 else z:=1;
                  VZoom := GState.ViewState.GetCurrentZoom;
                  if Msg.wParam<0 then zooming(VZoom-(1*z),GState.ZoomingAtMousePos)
                                  else zooming(VZoom+(1*z),GState.ZoomingAtMousePos);
                 end;
   WM_KEYFIRST: begin
                 if (dWhenMovingButton<35) then begin
                  inc(dWhenMovingButton);
                 end;
                 dWMB:=trunc(Power(dWhenMovingButton,1.5));
                 if Msg.wParam=VK_RIGHT then GState.ViewState.ChangeMapPixelByDelta(Point(dWMB, 0));
                 if Msg.wParam=VK_Left then GState.ViewState.ChangeMapPixelByDelta(Point(-dWMB, 0));
                 if Msg.wParam=VK_Down then GState.ViewState.ChangeMapPixelByDelta(Point(0, dWMB));
                 if Msg.wParam=VK_Up then GState.ViewState.ChangeMapPixelByDelta(Point(0, -dWMB));
                end;
   WM_KEYUP: begin
             dWhenMovingButton:=5;
             if (Msg.wParam=VK_Delete)and(aoper=ao_line) then
               begin
                if length(length_arr)>0 then setlength(length_arr,length(length_arr)-1);
                TBEditPath.Visible:=(length(length_arr)>1);
                LayerMapNal.DrawLineCalc(length_arr, LenShow);
               end;
             if (Msg.wParam=VK_Delete)and(aoper=ao_reg) then
               begin
                if length(reg_arr)>0 then setlength(reg_arr,length(reg_arr)-1);
                TBEditPath.Visible:=(length(reg_arr)>1);
                LayerMapNal.DrawReg(reg_arr);
               end;
             if (Msg.wParam=VK_Delete)and(aoper in [ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly]) then
              if length(add_line_arr)>0 then
               begin
                delfrompath(lastpoint);
                TBEditPath.Visible:=(length(add_line_arr)>1);
                LayerMapNal.DrawNewPath(add_line_arr, (aoper=ao_add_poly)or(aoper=ao_edit_poly), lastpoint);
               end;
             if (Msg.wParam=VK_ESCAPE)and(aoper=ao_Reg) then
              if length(reg_arr)=0 then TBmoveClick(self)
                                   else begin
                                         setlength(reg_arr,0);
                                         TBEditPath.Visible:=(length(reg_arr)>1);
                                         LayerMapNal.DrawReg(reg_arr);
                                        end;
             if (Msg.wParam=VK_ESCAPE)and(aoper=ao_line) then
              if length(length_arr)=0 then TBmoveClick(self)
                                      else begin
                                            setlength(length_arr,0);
                                            TBEditPath.Visible:=(length(length_arr)>1);
                                            LayerMapNal.DrawLineCalc(length_arr, LenShow);
                                           end;
             if (Msg.wParam=VK_ESCAPE)and(aoper=ao_rect) then
              begin
               if rect_dwn then begin
                                 setalloperationfalse(ao_movemap);
                                 setalloperationfalse(ao_rect);
                                end
                           else setalloperationfalse(ao_movemap);
              end;
             if (Msg.wParam=VK_ESCAPE)and(aoper=ao_Add_Point) then setalloperationfalse(ao_movemap);
             if (Msg.wParam=VK_ESCAPE)and(aoper in [ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly]) then begin
               setalloperationfalse(ao_movemap)
             end;
             if (Msg.wParam=13)and(aoper in [ao_add_Poly,ao_add_line,ao_edit_Poly,ao_edit_line])and(length(add_line_arr)>1) then begin
               TBEditPathSaveClick(FMain);
             end;
            end;
  end;
end;

function TFmain.PrepareSelectionRect(Shift: TShiftState;
  var ASelectedLonLat: TExtendedRect): Boolean;
var
  VZoomCurr: Byte;
  VSelectedPixels: TRect;
  bxy: Integer;
  VSelectedTiles: TRect;
  VTileGridZoom: byte;
  VSelectedRelative: TExtendedRect;
  zLonR,zLatR: extended;
  Poly:  TExtendedPointArray;
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
    case GState.GShScale of
      1000000: begin zLonR:=6; zLatR:=4; end;
       500000: begin zLonR:=3; zLatR:=2; end;
       200000: begin zLonR:=1; zLatR:=0.66666666666666666666666666666667; end;
       100000: begin zLonR:=0.5; zLatR:=0.33333333333333333333333333333333; end;
        50000: begin zLonR:=0.25; zLatR:=0.1666666666666666666666666666665; end;
        25000: begin zLonR:=0.125; zLatR:=0.08333333333333333333333333333325; end;
        10000: begin zLonR:=0.0625; zLatR:=0.041666666666666666666666666666625; end;
      else begin zLonR:=6; zLatR:=4; end
    end;

    ASelectedLonLat.Left := ASelectedLonLat.Left-(round(ASelectedLonLat.Left*GSHprec) mod round(zLonR*GSHprec))/GSHprec;
    if ASelectedLonLat.Left < 0 then ASelectedLonLat.Left := ASelectedLonLat.Left-zLonR;

    ASelectedLonLat.Top := ASelectedLonLat.Top-(round(ASelectedLonLat.Top*GSHprec) mod round(zLatR*GSHprec))/GSHprec;
    if ASelectedLonLat.Top > 0 then ASelectedLonLat.Top := ASelectedLonLat.Top+zLatR;

    ASelectedLonLat.Right := ASelectedLonLat.Right-(round(ASelectedLonLat.Right*GSHprec) mod round(zLonR*GSHprec))/GSHprec;
    if ASelectedLonLat.Right >= 0 then ASelectedLonLat.Right := ASelectedLonLat.Right+zLonR;

    ASelectedLonLat.Bottom := ASelectedLonLat.Bottom-(round(ASelectedLonLat.Bottom*GSHprec) mod round(zLatR*GSHprec))/GSHprec;
    if ASelectedLonLat.Bottom <= 0 then ASelectedLonLat.Bottom := ASelectedLonLat.Bottom-zLatR;
  end;
  if (rect_p2) then begin
    SetLength(Poly, 5);
    Poly[0] := ASelectedLonLat.TopLeft;
    Poly[1] := ExtPoint(ASelectedLonLat.Right, ASelectedLonLat.Top);
    Poly[2] := ASelectedLonLat.BottomRight;
    Poly[3] := ExtPoint(ASelectedLonLat.Left, ASelectedLonLat.Bottom);
    Poly[4] := ASelectedLonLat.TopLeft;
    fsaveas.Show_(GState.ViewState.GetCurrentZoom, Poly);
    LayerSelection.Redraw;
    Poly := nil;
    rect_p2:=false;
    Result := False;
  end else begin
    Result := True;
  end;
end;

procedure TFmain.UpdateGPSsensors;
var
    s_len,n_len: string;
    sps: _SYSTEM_POWER_STATUS;
begin
 try
   //скорость
   TBXSensorSpeed.Caption:=RoundEx(GPSpar.speed,2);
   //средняя скорость
   TBXSensorSpeedAvg.Caption:=RoundEx(GPSpar.sspeed,2);
   //максимальная скорость
   TBXSensorSpeedMax.Caption:=RoundEx(GPSpar.maxspeed,2);
   //высота
   TBXSensorAltitude.Caption:=RoundEx(GPSpar.altitude,2);
   //пройденный путь
   s_len := DistToStrWithUnits(GPSpar.len, GState.num_format);
   TBXOdometrNow.Caption:=s_len;
   //расстояние до метки
   if (LayerMapNavToMark<>nil)and(LayerMapNavToMark.Visible) then begin
     n_len:=DistToStrWithUnits(LayerMapNavToMark.GetDistToMark, GState.num_format);
     TBXSensorLenToMark.Caption:=n_len;
   end else begin
     TBXSensorLenToMark.Caption:='-';
   end;
   //одометр
   TBXSensorOdometr.Caption:=DistToStrWithUnits(GPSpar.Odometr, GState.num_format);
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
   TBXSensorAzimut.Caption:=RoundEx(GPSpar.azimut,2)+'°';
   //Сила сигнала, кол-во спутников
   TBXSignalStrength.Caption:=RoundEx(GPSpar.SignalStrength,2)+' ('+inttostr(GPSpar.SatCount)+')'
 except
 end;
end;

procedure TFmain.topos(LL:TExtendedPoint;zoom_:byte;draw:boolean);
begin
  GState.ViewState.LockWrite;
  GState.ViewState.ChangeZoomAndUnlock(zoom_, LL);
  zooming(zoom_,false);
  if draw then begin
    LayerGoto.ShowGotoIcon(LL);
  end;
  generate_im;
end;

procedure TFmain.generate_im;
begin
  generate_im(nilLastLoad, '');
end;

procedure TFmain.generate_im(LastLoad:TLastLoad;err:string);
var
  Leyi:integer;
  ts2,ts3,fr:int64;
  VWikiLayersVisible: Boolean;
  VSelectionRect: TExtendedRect;
  VMapType: TMapType;
begin
  QueryPerformanceCounter(ts2);

  if not(lastload.use) then change_scene:=true;

  FMainLayer.Redraw;
  LayerScaleLine.Redraw;
  VWikiLayersVisible := False;
  for Leyi:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[Leyi];
    if (VMapType.asLayer)then begin
      if VMapType.IsKmlTiles then begin
        if GState.ViewState.IsHybrGUIDSelected(VMapType.GUID) then begin
          VWikiLayersVisible := True;
        end;
      end;
    end;
  end;
  LayerMapMarks.Redraw;
  FWikiLayer.Redraw;
  FWikiLayer.Visible := VWikiLayersVisible;

  if (lastload.use)and(err<>'') then begin
    FShowErrorLayer.ShowError(lastload.TilePos, lastload.Zoom, lastload.mt, err);
  end else begin
    FShowErrorLayer.Visible := False;
  end;

  if not(lastload.use) then begin
    if aoper=ao_line then begin
      TBEditPath.Visible:=(length(length_arr)>1);
      LayerMapNal.DrawLineCalc(length_arr, LenShow);
    end;
    if aoper=ao_reg then begin
      TBEditPath.Visible:=(length(reg_arr)>1);
      LayerMapNal.DrawReg(reg_arr);
    end;
    if aoper=ao_rect then begin
      LayerMapNal.DrawNothing;
      VSelectionRect := FSelectionRect;
      if PrepareSelectionRect([], VSelectionRect) then begin
        LayerMapNal.DrawSelectionRect(VSelectionRect);
      end;
    end;
    if GState.GPS_enab then begin
       LayerMapGPS.Redraw;
       UpdateGPSsensors;
    end;
    if aoper in [ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly] then begin
      TBEditPath.Visible:=(length(add_line_arr)>1);
      LayerMapNal.DrawNewPath(add_line_arr, (aoper=ao_add_poly)or(aoper=ao_edit_poly), lastpoint);
    end;
    try
      LayerMapMarks.Visible := GState.show_point <> mshNone;
    except
    end;
  end;
  LayerStatBar.Redraw;
  QueryPerformanceCounter(ts3);
  QueryPerformanceFrequency(fr);
  Label1.caption :=FloatToStr((ts3-ts2)/(fr/1000));
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

begin
  MapIcons24.Clear;
  MapIcons18.Clear;
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
            MainToolbarSubMenuItem.Images:=MapIcons18;
            if asLayer then begin
              TBLayerSel.Add(MainToolbarSubMenuItem)
            end else begin
              TBSMB.Add(MainToolbarSubMenuItem);
            end;
          end;
          MainToolbarSubMenuItem := TTBXSubmenuItem(FMainToolbarSubMenuItemList.GetByGUID(GState.MapType[j].GUID));
          MainToolbarSubMenuItem.Add(MainToolbarItem);
        end;
        MapIcons24.AddMasked(bmp24,RGB(255,0,255));
        MapIcons18.AddMasked(bmp18,RGB(255,0,255));
        MainToolbarItem.Name:='TBMapN'+inttostr(id);
        MainToolbarItem.ShortCut:=HotKey;
        MainToolbarItem.ImageIndex:=i;
        MainToolbarItem.Caption:=name;
        MainToolbarItem.OnAdjustFont:=AdjustFont;
        MainToolbarItem.OnClick:=TBmap1Click;

        TBFillingItem:=TTBXItem.Create(TBFillingTypeMap);
        FTBFillingItemList.Add(VMapType.GUID, TBFillingItem);
        TBFillingItem.name:='TBMapFM'+inttostr(id);
        TBFillingItem.ImageIndex:=i;
        TBFillingItem.Caption:=name;
        TBFillingItem.OnAdjustFont:=AdjustFont;
        TBFillingItem.OnClick:=TBfillMapAsMainClick;
        TBFillingTypeMap.Add(TBFillingItem);

        if asLayer then begin
          NDwnItem:=TMenuItem.Create(nil);
          FNDwnItemList.Add(VMapType.GUID, NDwnItem);
          NDwnItem.Caption:=name;
          NDwnItem.ImageIndex:=i;
          NDwnItem.OnClick:=N21Click;
          ldm.Add(NDwnItem);
          NDelItem:=TMenuItem.Create(nil);
          FNDelItemList.Add(VMapType.GUID, NDelItem);
          NDelItem.Caption:=name;
          NDelItem.ImageIndex:=i;
          NDelItem.OnClick:=NDelClick;
          dlm.Add(NDelItem);
          NLayerParamsItem:=TTBXItem.Create(NLayerParams);
          FNLayerParamsItemList.Add(VMapType.GUID, NLayerParamsItem);
          NLayerParamsItem.Caption:=name;
          NLayerParamsItem.ImageIndex:=i;
          NLayerParamsItem.OnClick:=NMapParamsClick;
          NLayerParams.Add(NLayerParamsItem);
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
        if asLayer then begin
          NDwnItem.Tag:=longint(VMapType);
          NDelItem.Tag:=longint(VMapType);
          NLayerParamsItem.Tag:=longint(VMapType);
        end;
      end;
    end;
  end;
  MainToolbarItem := TTBXItem(FMainToolbarItemList.GetByGUID(GState.ViewState.GetCurrentMap.GUID));
  TBSMB.ImageIndex := MainToolbarItem.ImageIndex;
  MainToolbarItem.Checked:=true;
  if GState.Showmapname then begin
    TBSMB.Caption:=MainToolbarItem.Caption;
  end else begin
    TBSMB.Caption:='';
  end;
end;

procedure TFmain.FormActivate(Sender: TObject);
var
     i:integer;
     param:string;
     MainWindowMaximized: Boolean;
     VGUID: TGUID;
     VGUIDString: string;
  VFillingmaptype: TMapType;
  Vzoom_mapzap: integer;
  VScreenCenterPos: TPoint;
  VZoom: Byte;
  VLonLat: TExtendedPoint;
  VConverter: ICoordConverter;
begin
 GState.ScreenSize := Point(Screen.Width, Screen.Height);
 if ProgramStart=false then exit;

 if length(GState.MapType)=0 then
  begin
   ShowMessage(SAS_ERR_NoMaps);
   Close;
   exit;
  end;

  FMainToolbarItemList := TGUIDObjectList.Create(False);
  FMainToolbarSubMenuItemList := TGUIDObjectList.Create(False);
  FTBFillingItemList := TGUIDObjectList.Create(False);
  FNLayerParamsItemList := TGUIDObjectList.Create(False);
  FNDwnItemList := TGUIDObjectList.Create(False);
  FNDelItemList := TGUIDObjectList.Create(False);

 RectWindow := Types.Rect(0, 0, 0, 0);
 Enabled:=false;
 dWhenMovingButton := 5;
 MainWindowMaximized:=GState.MainIni.Readbool('VIEW','Maximized',true);
 GState.FullScrean:=GState.MainIni.Readbool('VIEW','FullScreen',false);
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

 movepoint:=-1;

 LoadMarksFromFile;
 LoadCategoriesFromFile;
 Enabled:=true;
 nilLastLoad.use:=false;
 Application.OnMessage := DoMessageEvent;
 Application.HelpFile := ExtractFilePath(Application.ExeName)+'help.hlp';
 LenShow:=true;
 Screen.Cursors[1]:=LoadCursor(HInstance, 'SEL');
 Screen.Cursors[2]:=LoadCursor(HInstance, 'LEN');
 Screen.Cursors[3]:=LoadCursor(HInstance, 'HAND');
 Screen.Cursors[4]:=LoadCursor(HInstance, 'SELPOINT');
 Map.Cursor:=crDefault;

 MouseDownPoint := point(0,0);
 MouseUpPoint := point(0,0);
 MapZoomAnimtion:=0;

 GState.TilesOut:=GState.MainIni.readInteger('VIEW','TilesOut',0);

 setlength(GState.LastSelectionPolygon,0);

 FSettings.FShortcutEditor.LoadShortCuts(TBXMainMenu.Items, 'HOTKEY');

 GState.InetConnect.userwinset:=GState.MainIni.Readbool('INTERNET','userwinset',true);
 GState.InetConnect.uselogin:=GState.MainIni.Readbool('INTERNET','uselogin',false);
 GState.InetConnect.Proxyused:=GState.MainIni.Readbool('INTERNET','used_proxy',false);
 GState.InetConnect.proxystr:=GState.MainIni.Readstring('INTERNET','proxy','');
 GState.InetConnect.loginstr:=GState.MainIni.Readstring('INTERNET','login','');
 GState.InetConnect.passstr:=GState.MainIni.Readstring('INTERNET','password','');
 GState.SaveTileNotExists:=GState.MainIni.ReadBool('INTERNET','SaveTileNotExists', True);
 GState.IgnoreTileNotExists:=GState.MainIni.ReadBool('INTERNET','IgnoreTileNotExists',false);

 GState.TwoDownloadAttempt:=GState.MainIni.ReadBool('INTERNET','DblDwnl',true);
 GState.GoNextTileIfDownloadError:=GState.MainIni.ReadBool('INTERNET','GoNextTile',false);
 GState.InetConnect.TimeOut:=GState.MainIni.ReadInteger('INTERNET','TimeOut',40000);
 GState.SessionLastSuccess:=GState.MainIni.ReadBool('INTERNET','SessionLastSuccess',false);

 GState.ShowMapName:=GState.MainIni.readBool('VIEW','ShowMapNameOnPanel',true);
 GState.ZoomingAtMousePos:=GState.MainIni.readBool('VIEW','ZoomingAtMousePos',true);
 NGoToCur.Checked := GState.ZoomingAtMousePos;
 GState.show_point := TMarksShowType(GState.MainIni.readinteger('VIEW','ShowPointType',2));
 GState.CacheConfig.DefCache:=GState.MainIni.readinteger('VIEW','DefCache',2);
 Vzoom_mapzap:=GState.MainIni.readinteger('VIEW','MapZap',-1);
 GState.TileGridZoom:=GState.MainIni.readinteger('VIEW','grid',0);
 GState.MouseWheelInv:=GState.MainIni.readbool('VIEW','invert_mouse',false);
 TileSource:=TTileSource(GState.MainIni.Readinteger('VIEW','TileSource',1));
 GState.num_format:= TDistStrFormat(GState.MainIni.Readinteger('VIEW','NumberFormat',0));
 GState.Resampling := TTileResamplingType(GState.MainIni.Readinteger('VIEW','ResamlingType',1));
 GState.llStrType:=TDegrShowFormat(GState.MainIni.Readinteger('VIEW','llStrType',0));
 GState.FirstLat:=GState.MainIni.ReadBool('VIEW','FirstLat',false);
 GState.BorderAlpha:=GState.MainIni.Readinteger('VIEW','BorderAlpha',150);
 GState.BorderColor:=GState.MainIni.Readinteger('VIEW','BorderColor',$FFFFFF);
 GState.ShowBorderText:=GState.MainIni.ReadBool('VIEW','BorderText',true);
 GState.UsePrevZoom := GState.MainIni.Readbool('VIEW','back_load',true);
 GState.UsePrevZoomLayer := GState.MainIni.Readbool('VIEW','back_load_layer',true);
 GState.AnimateZoom:=GState.MainIni.Readbool('VIEW','animate',true);
 GState.GShScale:=GState.MainIni.Readinteger('VIEW','GShScale',0);
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

 GState.MapZapColor:=GState.MainIni.Readinteger('VIEW','MapZapColor',clBlack);
 GState.MapZapShowTNE:=GState.MainIni.ReadBool('VIEW','MapZapShowTNE', True);
 GState.MapZapTneColor:=GState.MainIni.Readinteger('VIEW','MapZapTneColor',clRed);
 GState.MapZapAlpha:=GState.MainIni.Readinteger('VIEW','MapZapAlpha',110);
 lock_toolbars:=GState.MainIni.ReadBool('VIEW','lock_toolbars',false);

 GState.LastSelectionColor:=GState.MainIni.Readinteger('VIEW','LastSelectionColor',clBlack);
 GState.LastSelectionAlfa:=GState.MainIni.Readinteger('VIEW','LastSelectionAlpha',210);

 GState.CacheElemensMaxCnt:=GState.MainIni.ReadInteger('VIEW','TilesOCache',150);
 Label1.Visible:=GState.MainIni.ReadBool('VIEW','time_rendering',false);
 GState.ShowHintOnMarks:=GState.MainIni.ReadBool('VIEW','ShowHintOnMarks',true);
 GState.SrchType:=TSrchType(GState.MainIni.ReadInteger('VIEW','SearchType',0));
 GState.BGround:=GState.MainIni.ReadInteger('VIEW','Background',clSilver);
 GState.WikiMapMainColor:=GState.MainIni.Readinteger('Wikimapia','MainColor',$FFFFFF);
 GState.ShowStatusBar := GState.MainIni.readbool('VIEW','statusbar',true);
 GState.WikiMapFonColor:=GState.MainIni.Readinteger('Wikimapia','FonColor',$000001);

 GState.GammaN:=GState.MainIni.Readinteger('COLOR_LEVELS','gamma',50);
 GState.ContrastN:=GState.MainIni.Readinteger('COLOR_LEVELS','contrast',0);
 GState.InvertColor:=GState.MainIni.ReadBool('COLOR_LEVELS','InvertColor',false);
 GState.GPS_COM:=GState.MainIni.ReadString('GPS','com','COM0');
 GState.GPS_BaudRate:=GState.MainIni.ReadInteger('GPS','BaudRate',4800);
 GState.GPS_TimeOut:=GState.MainIni.ReadInteger('GPS','timeout',15);
 GState.GPS_Delay:=GState.MainIni.ReadInteger('GPS','update',1000);
 GState.GPS_enab:=GState.MainIni.ReadBool('GPS','enbl',false);
 GState.GPS_WriteLog:=GState.MainIni.Readbool('GPS','log',true);
 GState.GPS_NMEALog:=GState.MainIni.Readbool('GPS','NMEAlog',false);
 GState.GPS_ArrowSize:=GState.MainIni.ReadInteger('GPS','SizeStr',25);
 GState.GPS_TrackWidth:=GState.MainIni.ReadInteger('GPS','SizeTrack',5);
 GState.GPS_ArrowColor:=GState.MainIni.ReadInteger('GPS','ColorStr',clRed);
 GState.GPS_Correction:=extpoint(str2r(GState.MainIni.ReadString('GPS','popr_lon','0')),str2r(GState.MainIni.ReadString('GPS','popr_lat','0')));
 GState.GPS_ShowPath:=GState.MainIni.ReadBool('GPS','path',true);
 GState.GPS_MapMove:=GState.MainIni.ReadBool('GPS','go',true);
 GPSpar.Odometr:=str2r(GState.MainIni.ReadString('GPS','Odometr','0'));
 GState.GPS_SensorsAutoShow:=GState.MainIni.ReadBool('GPS','SensorsAutoShow',true);
 GState.GPS_NumTrackPoints:=GState.MainIni.ReadInteger('GPS','NumShowTrackPoints',5000);

 GState.GSMpar.Port:=GState.MainIni.ReadString('GSM','port','COM1');
 GState.GSMpar.BaudRate:=GState.MainIni.ReadInteger('GSM','BaudRate',4800);
 GState.GSMpar.auto:=GState.MainIni.ReadBool('GSM','Auto',true);
 GState.GSMpar.WaitingAnswer:=GState.MainIni.ReadInteger('GSM','WaitingAnswer',200);

 GState.CacheConfig.OldCpath:=GState.MainIni.Readstring('PATHtoCACHE','GMVC','cache_old' + PathDelim);
 GState.CacheConfig.NewCpath:=GState.MainIni.Readstring('PATHtoCACHE','SASC','cache' + PathDelim);
 GState.CacheConfig.ESCpath:=GState.MainIni.Readstring('PATHtoCACHE','ESC','cache_ES' + PathDelim);
 GState.CacheConfig.GMTilesPath:=GState.MainIni.Readstring('PATHtoCACHE','GMTiles','cache_gmt' + PathDelim);
 GState.CacheConfig.GECachePath:=GState.MainIni.Readstring('PATHtoCACHE','GECache','cache_GE' + PathDelim);

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

  FMainLayer := TMapMainLayer.Create(map, VScreenCenterPos);
  FMainLayer.Visible := True;
  FWikiLayer := TWikiLayer.Create(map, VScreenCenterPos);
  FFillingMap:=TMapFillingLayer.create(map, VScreenCenterPos);
  LayerMapMarks:= TMapMarksLayer.Create(map, VScreenCenterPos);
  LayerMapGPS:= TMapGPSLayer.Create(map, VScreenCenterPos);
  LayerSelection := TSelectionLayer.Create(map, VScreenCenterPos);
  LayerMapNal:=TMapNalLayer.Create(map, VScreenCenterPos);
  LayerMapNal.Visible:=false;


  LayerGoto := TGotoLayer.Create(map, VScreenCenterPos);
  LayerMapNavToMark := TNavToMarkLayer.Create(map, VScreenCenterPos);
  FShowErrorLayer := TTileErrorInfoLayer.Create(map, VScreenCenterPos);
  LayerMapScale := TCenterScale.Create(map);
  LayerScaleLine := TLayerScaleLine.Create(map);
  LayerStatBar:=TLayerStatBar.Create(map);

  FMiniMapLayer := TMiniMapLayer.Create(map, VScreenCenterPos, MapIcons18);
  FMiniMapLayer.Visible := True;

  CreateMapUI;
  FSettings.InitMapsList;

 try
  VGUIDString := GState.MainIni.ReadString('VIEW','FillingMap','');
  if VGUIDString <> '' then begin
    VGUID := StringToGUID(VGUIDString);
    VFillingmaptype:=GetMapFromID(VGUID);
  end else begin
    VFillingmaptype := nil;
  end;
 except
  VFillingmaptype := nil;
 end;
 if VFillingmaptype<>nil then begin
   TTBXItem(FTBFillingItemList.GetByGUID(Vfillingmaptype.GUID)).Checked:=true
 end else TBfillMapAsMain.Checked:=true;

 FFillingMap.SetSourceMap(VFillingmaptype, Vzoom_mapzap);

 i:=1;
 while str2r(GState.MainIni.ReadString('HIGHLIGHTING','pointx_'+inttostr(i),'2147483647'))<>2147483647 do
  begin
   setlength(GState.LastSelectionPolygon,i);
   GState.LastSelectionPolygon[i-1].x:=str2r(GState.MainIni.ReadString('HIGHLIGHTING','pointx_'+inttostr(i),'2147483647'));
   GState.LastSelectionPolygon[i-1].y:=str2r(GState.MainIni.ReadString('HIGHLIGHTING','pointy_'+inttostr(i),'2147483647'));
   inc(i);
  end;
 if length(GState.LastSelectionPolygon)>0 then GState.poly_zoom_save:=GState.MainIni.Readinteger('HIGHLIGHTING','zoom',1);
 LayerSelection.Visible := GState.MainIni.readbool('VIEW','showselection',false);

 LayerMapScale.Visible:=GState.MainIni.readbool('VIEW','showscale',false);
 SetMiniMapVisible(GState.MainIni.readbool('VIEW','minimap',true));
 SetLineScaleVisible(GState.MainIni.readbool('VIEW','line',true));
 SetStatusBarVisible();

 TTBXItem(FindComponent('NGShScale'+IntToStr(GState.GShScale))).Checked:=true;
 N32.Checked:=LayerMapScale.Visible;
 NShowSelection.Checked := LayerSelection.Visible;
 Ninvertcolor.Checked:=GState.InvertColor;
 TBGPSconn.Checked := GState.GPS_enab;
 if GState.GPS_enab then TBGPSconnClick(TBGPSconn);
 TBGPSPath.Checked:=GState.GPS_ShowPath;
 NGPSPath.Checked:=GState.GPS_ShowPath;
 TBGPSToPoint.Checked:=GState.GPS_MapMove;
 NGPSToPoint.Checked:=GState.GPS_MapMove;
 Nbackload.Checked:=GState.UsePrevZoom;
 NbackloadLayer.Checked:=GState.UsePrevZoomLayer;
 Nanimate.Checked:=GState.AnimateZoom;

 if not(FileExists(GState.MainConfigFileName)) then
  begin
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

 ProgramStart:=false;

 if Vzoom_mapzap<>-1 then TBMapZap.Caption:='x'+inttostr(vzoom_mapzap + 1)
                     else TBMapZap.Caption:='';
 selectMap(GState.ViewState.GetCurrentMap);

 map.Color:=GState.BGround;

 FMapPosChangeListener := TChangePosListenerOfMainForm.Create(Self);
 GState.ViewState.PosChangeNotifier.Add(FMapPosChangeListener);

 FMainMapChangeListener := TMainMapChangeListenerOfMainForm.Create(Self);
 GState.ViewState.MapChangeNotifier.Add(FMainMapChangeListener);

 FHybrChangeListener := THybrChangeListenerOfMainForm.Create(Self);
 GState.ViewState.HybrChangeNotifier.Add(FHybrChangeListener);

 GState.ViewState.LoadViewPortState;

 GState.ViewState.LockWrite;
 GState.ViewState.ChangeZoomAndUnlock(VZoom, VScreenCenterPos);

 if ParamCount > 1 then begin
  try
    param:=paramstr(1);
    if param<>'' then begin
      VGUID := StringToGUID(param);
      for i:=0 to length(GState.MapType)-1 do begin
        if IsEqualGUID(GState.MapType[i].GUID, VGUID)then begin
          GState.ViewState.ChangeMainMapAtCurrentPoint(GState.MapType[i]);
        end;
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

 zooming(GState.ViewState.GetCurrentZoom,false);
 MapMoving:=false;
 Fsaveas.PageControl1.ActivePageIndex:=0;

 SetProxy;

 case GState.SrchType of
  stGoogle:  TBXSelectYandexSrchClick(TBXSelectGoogleSrch);
  stYandex: TBXSelectYandexSrchClick(TBXSelectYandexSrch);
 end;

 LayerMapMarks.Visible := True;
 if GState.WebReportToAuthor then WebBrowser1.Navigate('http://sasgis.ru/stat/index.html');
 Enabled:=true;
 SetFocus;
 if (FLogo<>nil)and(FLogo.Visible) then FLogo.Timer1.Enabled:=true;
 FUIDownLoader := TTileDownloaderUI.Create;

 InitSearchers;
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
 if ANewZoom<=1  then TBZoom_Out.Enabled:=false
          else TBZoom_Out.Enabled:=true;
 if ANewZoom>=24 then TBZoomIn.Enabled:=false
          else TBZoomIn.Enabled:=true;
 NZoomIn.Enabled:=TBZoomIn.Enabled;
 NZoomOut.Enabled:=TBZoom_Out.Enabled;
 RxSlider1.Value:=ANewZoom;
 VZoom := GState.ViewState.GetCurrentZoom;
 if (MapZoomAnimtion=1)or(MapMoving)or(ANewZoom>23) then exit;
 MapZoomAnimtion:=1;
 steps:=11;

 if (abs(ANewZoom-VZoom)=1)and(GState.AnimateZoom) then begin
   for i:=0 to steps-1 do begin
     QueryPerformanceCounter(ts1);
      if VZoom>ANewZoom then begin
        //Scale := 1 - (i/(steps - 1))/2;
        Scale := 1/(1 + i/(steps - 1));
      end else begin
        //Scale := 1 + i/(steps - 1);
        Scale := 3 - (1/(1+i/(steps - 1)))*2;
      end;
      if move then begin
        FMainLayer.ScaleTo(Scale, m_m);
        LayerSelection.ScaleTo(Scale, m_m);
        LayerMapMarks.ScaleTo(Scale, m_m);
        LayerMapGPS.ScaleTo(Scale, m_m);
        FWikiLayer.ScaleTo(Scale, m_m);
        FFillingMap.ScaleTo(Scale, m_m);
        LayerMapNal.ScaleTo(Scale, m_m);
        LayerGoto.ScaleTo(Scale, m_m);
        FShowErrorLayer.ScaleTo(Scale, m_m);
        LayerMapNavToMark.ScaleTo(Scale, m_m);
      end else begin
        FMainLayer.ScaleTo(Scale);
        LayerSelection.ScaleTo(Scale);
        LayerMapMarks.ScaleTo(Scale);
        LayerMapGPS.ScaleTo(Scale);
        FWikiLayer.ScaleTo(Scale);
        FFillingMap.ScaleTo(Scale);
        LayerMapNal.ScaleTo(Scale);
        LayerGoto.ScaleTo(Scale);
        FShowErrorLayer.ScaleTo(Scale);
        LayerMapNavToMark.ScaleTo(Scale);
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
    GState.ViewState.ChangeZoomWithFreezeAtVisualPoint(ANewZoom, m_m);
  end else begin
    GState.ViewState.ChangeZoomWithFreezeAtCenter(ANewZoom);
  end;
 MapZoomAnimtion:=0;
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
 TBiniLoadPositions(Self,GState.MainIni,'PANEL_');
 TBEditPath.Visible:=false;
 Caption:=Caption+' '+SASVersion;
 ProgramStart:=true;
end;

procedure TFmain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  VWaitResult: DWORD;
  i:integer;
begin
  ProgramClose:=true;
  GState.ViewState.PosChangeNotifier.Remove(FMapPosChangeListener);
  GState.ViewState.MapChangeNotifier.Remove(FMainMapChangeListener);
  GState.ViewState.HybrChangeNotifier.Remove(FHybrChangeListener);
  //останавливаем GPS
  GPSReceiver.OnDisconnect:=nil;
  GPSReceiver.Close;
  FUIDownLoader.Terminate;
  GState.StopAllThreads;
  for i := 0 to Screen.FormCount - 1 do begin
    if (Screen.Forms[i]<>Application.MainForm)and(Screen.Forms[i].Visible) then begin
      Screen.Forms[i].Close;
    end;
  end;
  Application.ProcessMessages;
  VWaitResult := WaitForSingleObject(FUIDownLoader.Handle, 10000);
  if VWaitResult = WAIT_TIMEOUT then begin
    TerminateThread(FUIDownLoader.Handle, 0);
  end;
  if length(GState.MapType)<>0 then FSettings.Save;
  FreeAndNil(FGoogleSearch);
  FreeAndNil(FYandexSerach);
  FMapPosChangeListener := nil;
  FMainMapChangeListener := nil;
  FHybrChangeListener := nil;
  FreeAndNil(FFillingMap);
  FreeAndNil(FWikiLayer);
  Application.ProcessMessages;
  FreeAndNil(FUIDownLoader);
  FreeAndNil(LayerMapScale);
  FreeAndNil(LayerStatBar);
  FreeAndNil(LayerScaleLine);
  FreeAndNil(LayerMapGPS);
  FreeAndNil(LayerMapMarks);
  FreeAndNil(LayerSelection);
  FreeAndNil(LayerGoto);
  FreeAndNil(FShowErrorLayer);
  FreeAndNil(LayerMapNal);
  FreeAndNil(LayerMapNavToMark);
  FreeAndNil(FMainLayer);
  FreeAndNil(FMiniMapLayer);
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

procedure TextToHTMLDoc(Text: string; var Document: IHTMLDocument2);
var
  V: OleVariant;
begin
  try
   V:=VarArrayCreate([0, 0], varVariant);
   V[0]:=Text;
   Document.Write(PSafeArray(TVarData(v).VArray));
  finally
   Document.Close;
  end;
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
 labZoom.Caption:=' '+inttostr(RxSlider1.Value+1)+'x ';
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
begin
 selectMap(TMapType(TTBXItem(sender).tag));
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
  if AddNewPointModal(GState.ViewState.VisiblePixel2LonLat(MouseUpPoint)) then begin
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
    VPoint := GState.ViewState.VisiblePixel2MapPixel(MouseDownPoint);
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
  ll := GState.ViewState.VisiblePixel2LonLat(MouseDownPoint);
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
    VPoint := GState.ViewState.VisiblePixel2MapPixel(MouseDownPoint);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VMap := GState.ViewState.GetCurrentMap;
    VConverter := GState.ViewState.GetCurrentCoordConverter;
  finally
    GState.ViewState.UnLockRead;
  end;
  if VMap.IsStoreFileCache then begin
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
    VPoint := GState.ViewState.VisiblePixel2MapPixel(MouseUpPoint);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
  finally
    GState.ViewState.UnLockRead;
  end;

  VMapMain.GeoConvert.CheckPixelPosStrict(VPoint, VZoomCurr, False);
  VLoadPoint := VMapMain.GeoConvert.Pos2OtherMap(VPoint, VZoomCurr + 8, VMapType.GeoConvert);
  VLoadPoint := VMapType.GeoConvert.PixelPos2TilePos(VLoadPoint, VZoomCurr);
  path := VMapType.GetTileShowName(VLoadPoint, VZoomCurr);

  if ((not(VMapType.tileExists(VLoadPoint, VZoomCurr)))or
      (MessageBox(handle,pchar(SAS_STR_file+' '+path+' '+SAS_MSG_FileExists),pchar(SAS_MSG_coution),36)=IDYES))
  then begin
    TTileDownloaderUIOneTile.Create(VLoadPoint, VZoomCurr, VMapType);
  end;
end;

procedure TFmain.N11Click(Sender: TObject);
var
  WindirP: PChar;
  btm_ex:TBitmap;
  path: string;
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
    VPoint := GState.ViewState.VisiblePixel2MapPixel(m_m);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VMap := GState.ViewState.GetCurrentMap;
    VConverter := GState.ViewState.GetCurrentCoordConverter;
  finally
    GState.ViewState.UnLockRead;
  end;
  if VMap.IsStoreFileCache then begin
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
    VPoint := GState.ViewState.VisiblePixel2MapPixel(m_m);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VMap := GState.ViewState.GetCurrentMap;
    VConverter := GState.ViewState.GetCurrentCoordConverter;
  finally
    GState.ViewState.UnLockRead;
  end;
  if VMap.IsStoreFileCache then begin
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
    VPoint := GState.ViewState.VisiblePixel2MapPixel(MouseUpPoint);
    VZoomCurr := GState.ViewState.GetCurrentZoom;
  finally
    GState.ViewState.UnLockRead;
  end;
  if VMapMain.GeoConvert.CheckPixelPosStrict(VPoint, VZoomCurr, False) then begin
    VLoadPoint := VMapMain.GeoConvert.Pos2OtherMap(VPoint, VZoomCurr + 8, VMapType.GeoConvert);
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
 Tilesource:=TTileSource(TTBXItem(Sender).Tag);
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
 if TBRectSave.ImageIndex=6 then setalloperationfalse(ao_rect)
                            else setalloperationfalse(ao_reg)
end;

procedure TFmain.TBPreviousClick(Sender: TObject);
begin
 if length(GState.LastSelectionPolygon)>0 then fsaveas.Show_(GState.poly_zoom_save - 1, GState.LastSelectionPolygon)
                        else showmessage(SAS_MSG_NeedHL);
 LayerSelection.Redraw;
end;

//карта заполнения в основном окне
procedure TFmain.NFillMapClick(Sender: TObject);
begin
  if FFillingMap.SourceZoom > -1 then begin
    TBXToolPalette1.SelectedCell:=Point((FFillingMap.SourceZoom + 1) mod 5,(FFillingMap.SourceZoom + 1) div 5);
  end else begin
    TBXToolPalette1.SelectedCell:=Point(0,0);
  end;
end;

procedure TFmain.TBXToolPalette1CellClick(Sender: TTBXCustomToolPalette; var ACol, ARow: Integer; var AllowChange: Boolean);
var
  Vzoom_mapzap: integer;
begin
 Vzoom_mapzap:=((5*ARow)+ACol)-1;
 if Vzoom_mapzap>-1 then begin
  TBMapZap.Caption:='x'+inttostr(Vzoom_mapzap+1);
  Vzoom_mapzap := Vzoom_mapzap;
  end else begin
   TBMapZap.Caption:='';
  end;
 FFillingMap.SetSourceMap(FFillingMap.SourceSelected, Vzoom_mapzap);
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

procedure TFmain.selectMap(AMapType: TMapType);
begin
  if not(AMapType.asLayer) then begin
    if (AMapType.showinfo)and(AMapType.MapInfo<>'') then begin
      ShowMessage(AMapType.MapInfo);
      AMapType.showinfo:=false;
    end;
    GState.ViewState.ChangeMainMapAtCurrentPoint(AMapType);
  end else begin
    GState.ViewState.ChangeSelectHybrByGUID(AMapType.GUID);
  end;
end;

procedure TFmain.EditGoogleSrchAcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
begin
  FGoogleSearch.ModalSearch(NewText, GState.ViewState.GetCenterLonLatDlb);
end;

procedure TFmain.TBSubmenuItem1Click(Sender: TObject);
begin
 FGoTo.ShowModal;
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
 try
 NGPSconn.Checked:=TTBXitem(sender).Checked;
 TBGPSconn.Checked:=TTBXitem(sender).Checked;
 LayerMapGPS.Visible:=NGPSconn.Checked;
 GState.GPS_enab := TBGPSconn.Checked;
 if GState.GPS_enab then
  begin
   GPSReceiver.NMEALog:=GState.GPS_NMEALog;
   GPSReceiver.LogFile:=GState.TrackLogPath+inttostr(YearOf(Date))+'.'+inttostr(MonthOf(Date))+'.'+inttostr(DayOf(Date))
     +'-'+inttostr(HourOf(GetTime))+'-'+inttostr(MinuteOf(GetTime))+'-'+inttostr(SecondOf(GetTime))+'.nmea';
   GPSReceiver.Delay:=GState.GPS_Delay;
   GPSReceiver.ConnectionTimeout:=GState.GPS_TimeOut;
   GPSReceiver.Port :=  GPSReceiver.StringToCommPort(GState.GPS_COM);
   if GPSReceiver.BaudRate<>GPSReceiver.IntToBaudRate(GState.GPS_BaudRate) then
     GPSReceiver.BaudRate:=GPSReceiver.IntToBaudRate(GState.GPS_BaudRate);
   GPSReceiver.NeedSynchronization:=true;
   try
    GPSReceiver.Open;
   except
    ShowMessage(SAS_ERR_PortOpen);
    GPSReceiver.Close;
   end;
  end
  else GPSReceiver.Close;
 except
 end;
end;

procedure TFmain.TBGPSPathClick(Sender: TObject);
begin
 NGPSPath.Checked:=TTBXitem(sender).Checked;
 TBGPSPath.Checked:=TTBXitem(sender).Checked;
 GState.GPS_ShowPath:=TBGPSPath.Checked;
end;

procedure TFmain.TBGPSToPointClick(Sender: TObject);
begin
 NGPSToPoint.Checked:=TTBXitem(sender).Checked;
 TBGPSToPoint.Checked:=TTBXitem(sender).Checked;
 GState.GPS_MapMove:=TBGPSToPoint.Checked;
end;

procedure TFmain.TBCOORDClick(Sender: TObject);
var
  Poly: TExtendedPointArray;
begin
 FSelLonLat:= TFSelLonLat.Create(Self);
        Try
          if FSelLonLat.Execute Then
             Begin
              SetLength(Poly, 5);
              Poly[0] := ExtPoint(FSelLonLat._lon_k,FSelLonLat._lat_k);
              Poly[1] := ExtPoint(FSelLonLat.lon_k,FSelLonLat._lat_k);
              Poly[2] := ExtPoint(FSelLonLat.lon_k,FSelLonLat.lat_k);
              Poly[3] := ExtPoint(FSelLonLat._lon_k,FSelLonLat.lat_k);
              Poly[4] := ExtPoint(FSelLonLat._lon_k,FSelLonLat._lat_k);
              fsaveas.Show_(GState.ViewState.GetCurrentZoom, Poly);
              LayerSelection.Redraw;
              Poly := nil;
             End;
        Finally
          FSelLonLat.Free;
        End;
 TBmoveClick(Sender);
end;

procedure TFmain.SetStatusBarVisible();
begin
 LayerStatBar.Visible:=GState.ShowStatusBar;
 mapResize(nil);
 Showstatus.Checked:=GState.ShowStatusBar;
end;

procedure TFmain.SetLineScaleVisible(visible:boolean);
begin
  LayerScaleLine.Visible := visible;
  LayerScaleLine.Redraw;
  ShowLine.Checked:=visible;
end;

procedure TFmain.SetMiniMapVisible(visible:boolean);
begin
  FMiniMapLayer.Visible := visible;
  ShowMiniMap.Checked:=visible;
end;

procedure TFmain.ShowstatusClick(Sender: TObject);
begin
  GState.ShowStatusBar := Showstatus.Checked;
  SetStatusBarVisible();
end;

procedure TFmain.ShowMiniMapClick(Sender: TObject);
begin
 SetMiniMapVisible(ShowMiniMap.Checked);
end;

procedure TFmain.ShowLineClick(Sender: TObject);
begin
 SetLineScaleVisible(ShowLine.Checked)
end;

procedure TFmain.N32Click(Sender: TObject);
begin
 LayerMapScale.Visible:=TTBXItem(sender).Checked;
end;

procedure TFmain.TBItem3Click(Sender: TObject);
var F:TextFile;
    i:integer;
    SaveDlg: TSaveDialog;
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
        for i:=0 to length(GState.GPS_TrackPoints)-1 do
          Writeln(f,R2strPoint(GState.GPS_TrackPoints[i].x),',',R2strPoint(GState.GPS_TrackPoints[i].y),',0');
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
begin
  if length(GState.GPS_TrackPoints)>1 then begin
    if SaveLineModal(-1, GState.GPS_TrackPoints, marshrutcomment) then begin
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
 if (ProgramClose<>true)and(not(ProgramStart))then
  begin
   GState.ViewState.ChangeViewSize(Point(map.Width, map.Height));
   FMainLayer.Resize;
   LayerStatBar.Resize;
   LayerScaleLine.Resize;
   LayerSelection.Resize;
   LayerMapNal.Resize;
   LayerMapMarks.Resize;
   LayerMapGPS.Resize;
   LayerMapScale.Resize;
   FWikiLayer.Resize;
   FFillingMap.Resize;
   LayerGoto.Resize;
   FShowErrorLayer.Resize;
   LayerMapNavToMark.Resize;
   FMiniMapLayer.Resize;
   LayerStatBar.Redraw;
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

procedure TFmain.TBEditItem1AcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
begin
  FYandexSerach.ModalSearch(NewText, GState.ViewState.GetCenterLonLatDlb);
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

procedure TFmain.ShowErrScript(DATA:string);
begin
 ShowMessage(data);
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
  MapMoving:=false;
  if (aoper=ao_movemap) then begin
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
var arr:TExtendedPointArray;
    op:TAOperation;
begin
 EditMarkId:=strtoint(FPWL.numid);
 op:=EditMarkF(EditMarkId,arr);
 if op=ao_edit_line then begin
   setalloperationfalse(ao_edit_line);
   add_line_arr:=arr;
 end;
 if op=ao_edit_poly then begin
   setalloperationfalse(ao_edit_poly);
   add_line_arr:=arr;
 end;
 generate_im;
end;

procedure TFmain.NMarkDelClick(Sender: TObject);
begin
 FWikiLayer.MouseOnReg(FPWL,moveTrue);
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
  FWikiLayer.MouseOnReg(FPWL,moveTrue);
  VId := strtoint(FPWL.numid);
  VMark := GetMarkByID(VId);
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
    VPoint := GState.ViewState.VisiblePixel2MapPixel(MouseDownPoint);
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
    VSize := GState.ViewState.GetVisibleSizeInPixel;
    VPos:=GState.ViewState.VisiblePixel2LonLat(moveTrue);
  finally
    GState.ViewState.UnLockRead;
  end;
  FDGAvailablePic.setup(VPos, VSize);
end;

procedure TFmain.mapMouseLeave(Sender: TObject);
begin
 if (HintWindow<>nil) then
  begin
   HintWindow.ReleaseHandle;
   FreeAndNil(HintWindow);
  end;
end;

procedure TFmain.GPSReceiver1SatellitesReceive(Sender: TObject);
begin
 if FSettings.Visible then FSettings.PaintBox1.Repaint;
end;

procedure TFmain.GPSReceiverReceive(Sender: TObject);
var s2f,sb:string;
    len:integer;
    xYear, xMonth, xDay, xHr, xMin, xSec, xMSec: word;
    VConverter: ICoordConverter;
begin
 if (GPSReceiver.IsFix=0) then exit;
 setlength(GState.GPS_TrackPoints,length(GState.GPS_TrackPoints)+1);
 len:=length(GState.GPS_TrackPoints);
 GState.GPS_TrackPoints[len-1]:=ExtPoint(GPSReceiver.GetLongitudeAsDecimalDegrees+GState.GPS_Correction.x,GPSReceiver.GetLatitudeAsDecimalDegrees+GState.GPS_Correction.y);
 if (GState.GPS_TrackPoints[len-1].x<>0)or(GState.GPS_TrackPoints[len-1].y<>0) then
  begin
    VConverter := GState.ViewState.GetCurrentCoordConverter;
  setlength(GState.GPS_ArrayOfSpeed,len);
  GPSpar.speed:=GPSReceiver.GetSpeed_KMH;
  if GPSpar.maxspeed<GPSpar.speed then GPSpar.maxspeed:=GPSpar.speed;
  inc(GPSpar.sspeednumentr);
  GPSpar.allspeed:=GPSpar.allspeed+GPSpar.speed;
  GPSpar.sspeed:=GPSpar.allspeed/GPSpar.sspeednumentr;
  GState.GPS_ArrayOfSpeed[len-1]:=GPSReceiver.GetSpeed_KMH;
  GPSpar.altitude:=GPSReceiver.GetAltitude;
  GPSpar.SignalStrength:=GPSReceiver.GetReceiverStatus.SignalStrength;
  GPSpar.SatCount:=GPSReceiver.GetSatelliteCount;
  if len>1 then begin
    GPSpar.len:=GPSpar.len+VConverter.CalcDist(GState.GPS_TrackPoints[len-2], GState.GPS_TrackPoints[len-1]);
    GPSpar.Odometr:=GPSpar.Odometr+VConverter.CalcDist(GState.GPS_TrackPoints[len-2], GState.GPS_TrackPoints[len-1]);
    GPSpar.azimut:=RadToDeg(ArcTan2(GState.GPS_TrackPoints[len-2].y-GState.GPS_TrackPoints[len-1].y,GState.GPS_TrackPoints[len-1].x-GState.GPS_TrackPoints[len-2].x))+90;
  end;

  if not((MapMoving)or(MapZoomAnimtion=1))and(Screen.ActiveForm=FMain) then
   begin
    if (GState.GPS_MapMove)
     then begin
            GState.ViewState.LockWrite;
            GState.ViewState.ChangeLonLatAndUnlock(GState.GPS_TrackPoints[len-1]);
          end
     else begin
           LayerStatBar.Redraw;
          end;
   end;
  UpdateGPSsensors;
  if GState.GPS_WriteLog then
   begin
    if length(GState.GPS_TrackPoints)=1 then sb:='1' else sb:='0';
    DecodeDate(Date, xYear, xMonth, xDay);
    DecodeTime(GetTime, xHr, xMin, xSec, xMSec);
    s2f:=R2StrPoint(round(GState.GPS_TrackPoints[len-1].y*10000000)/10000000)+','+R2StrPoint(round(GState.GPS_TrackPoints[len-1].x*10000000)/10000000)+','+sb+','+R2StrPoint(GPSReceiver.MetersToFeet(GPSpar.altitude))+','+
                    floattostr(Double(Date))+'.'+inttostr(round(Double(GetTime)*1000000))+','+inttostr(xDay)+'.'+inttostr(xMonth)+'.'+inttostr(xYear)+','+
                    inttostr(xHr)+':'+inttostr(xMin)+':'+inttostr(xSec);
    Writeln(GState.GPS_LogFile,s2f);
   end;
  end
  else setlength(GState.GPS_TrackPoints,length(GState.GPS_TrackPoints)-1);
end;

procedure TFmain.GPSReceiverDisconnect(Sender: TObject;
  const Port: TCommPort);
begin
 try
 if GState.GPS_WriteLog then CloseFile(GState.GPS_LogFile);
 if GState.GPS_SensorsAutoShow then TBXSensorsBar.Visible:=false;
 GState.GPS_enab:=false;
 LayerMapGPS.Visible:=false;
 NGPSconn.Checked:=false;
 TBGPSconn.Checked:=false;
 except
 end;
end;

procedure TFmain.GPSReceiverConnect(Sender: TObject; const Port: TCommPort);
var S:string;
begin
 GPSpar.allspeed:=0;
 GPSpar.sspeed:=0;
 GPSpar.speed:=0;
 GPSpar.maxspeed:=0;
 GPSpar.sspeednumentr:=0;
 if GState.GPS_SensorsAutoShow then TBXSensorsBar.Visible:=true;
 if GState.GPS_WriteLog then
 try
  CreateDir(GState.TrackLogPath);
  s:=GState.TrackLogPath+inttostr(YearOf(Date))+'.'+inttostr(MonthOf(Date))+'.'+inttostr(DayOf(Date))
     +'-'+inttostr(HourOf(GetTime))+'-'+inttostr(MinuteOf(GetTime))+'-'+inttostr(SecondOf(GetTime))+'.plt';
  AssignFile(GState.GPS_LogFile,s);
  rewrite(GState.GPS_LogFile);
  Write(GState.GPS_LogFile,'OziExplorer Track Point File Version 2.0'+#13#10+'WGS 84'+#13#10+'Altitude is in Feet'+#13#10+'Reserved 3'+#13#10+'0,2,255,Track Log File - '+DateTimeToStr(Now)+',1'+#13#10+'0'+#13#10)
 except
  GState.GPS_WriteLog:=false;
 end;
end;

procedure TFmain.GPSReceiverTimeout(Sender: TObject);
begin
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
begin
  if (HintWindow<>nil) then begin
    HintWindow.ReleaseHandle;
    FreeAndNil(HintWindow);
  end;
  if (Layer <> nil) then begin
    exit;
  end;
  if (ssDouble in Shift)or(MapZoomAnimtion=1)or(button=mbMiddle)or(HiWord(GetKeyState(VK_DELETE))<>0)
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
  if (Button=mbLeft)and(aoper<>ao_movemap) then begin
    if (aoper=ao_line)then begin
      setlength(length_arr,length(length_arr)+1);
      length_arr[length(length_arr)-1]:= VClickLonLat;
      TBEditPath.Visible:=(length(length_arr)>1);
      LayerMapNal.DrawLineCalc(length_arr, LenShow);
    end;
    if (aoper=ao_Reg) then begin
      setlength(reg_arr,length(reg_arr)+1);
      reg_arr[length(reg_arr)-1]:= VClickLonLat;
      TBEditPath.Visible:=(length(reg_arr)>1);
      LayerMapNal.DrawReg(reg_arr);
    end;
    if (aoper=ao_rect)then begin
      if rect_dwn then begin
        FSelectionRect.BottomRight:= VClickLonLat;
        rect_p2:=true;
      end else begin
        FSelectionRect.TopLeft:= VClickLonLat;
        FSelectionRect.BottomRight:=FSelectionRect.TopLeft
      end;
      rect_dwn:=not(rect_dwn);
      LayerMapNal.DrawNothing;
      VSelectionRect := FSelectionRect;
      if PrepareSelectionRect(Shift, VSelectionRect) then begin
        LayerMapNal.DrawSelectionRect(VSelectionRect);
      end;
    end;
    if (aoper=ao_add_point) then begin
      if(AddNewPointModal(VClickLonLat)) then begin
        setalloperationfalse(ao_movemap);
        generate_im;
      end;
    end;
    if (aoper in [ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly]) then begin
      for i:=0 to length(add_line_arr)-1 do begin
        if (VClickLonLatRect.Left < add_line_arr[i].X) and
          (VClickLonLatRect.Top > add_line_arr[i].Y) and
          (VClickLonLatRect.Right > add_line_arr[i].X) and
          (VClickLonLatRect.Bottom < add_line_arr[i].Y)
        then begin
          movepoint:=i;
          lastpoint:=i;
          TBEditPath.Visible:=(length(add_line_arr)>1);
          LayerMapNal.DrawNewPath(add_line_arr, (aoper=ao_add_poly)or(aoper=ao_edit_poly), lastpoint);
          exit;
        end;
      end;
      inc(lastpoint);
      movepoint:=lastpoint;
      insertinpath(lastpoint);
      add_line_arr[lastpoint]:=VClickLonLat;
      TBEditPath.Visible:=(length(add_line_arr)>1);
      LayerMapNal.DrawNewPath(add_line_arr, (aoper=ao_add_poly)or(aoper=ao_edit_poly), lastpoint);
    end;
    exit;
  end;
  if MapMoving then exit;
  if (Button=mbright)and(aoper=ao_movemap) then begin
    MouseUpPoint:=point(x,y);
    FPWL.find:=false;
    FPWL.S:=0;
    if LayerMapMarks.Visible then begin
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
    MapMoving:=true;
    map.PopupMenu:=nil;
  end;
  MouseDownPoint:=Point(x,y);
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
 VMapMoving := MapMoving;
 MapMoving:=false;

 GState.ViewState.LockRead;
 try
    VZoomCurr := GState.ViewState.GetCurrentZoom;
    VPoint := GState.ViewState.VisiblePixel2MapPixel(Point(x, y));
    VLonLat := GState.ViewState.VisiblePixel2LonLat(moveTrue);
    VMap := GState.ViewState.GetCurrentMap;
    VConverter := GState.ViewState.GetCurrentCoordConverter;
    VVisibleSizeInPixel := GState.ViewState.GetVisibleSizeInPixel;
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

 if movepoint>-1 then begin
  movepoint:=-1;
 end;
 if (((aoper<>ao_movemap)and(Button=mbLeft))or
     ((aoper=ao_movemap)and(Button=mbRight))) then exit;
 if (MapZoomAnimtion=1) then exit;
 map.Enabled:=false;
 map.Enabled:=true;
 if button=mbMiddle then
   begin
    TBFullSize.Checked:=not(TBFullSize.Checked);
    TBFullSizeClick(Sender);
    exit;
   end;

 if VMapMoving then begin
   GState.ViewState.ChangeMapPixelByDelta(Point(MouseDownPoint.x-x, MouseDownPoint.y-y));
 end;

 MouseUpPoint:=Point(x,y);
 if (y=MouseDownPoint.y)and(x=MouseDownPoint.x) then
  begin
   LayerStatBar.Redraw;
   LayerScaleLine.Redraw;
   if aoper=ao_line then begin
    TBEditPath.Visible:=(length(length_arr)>1);
    LayerMapNal.DrawLineCalc(length_arr, LenShow);
   end;
   if aoper=ao_reg then begin
    TBEditPath.Visible:=(length(reg_arr)>1);
    LayerMapNal.DrawReg(reg_arr);
   end;
   if aoper=ao_rect then begin
     LayerMapNal.DrawNothing;
     VSelectionRect := FSelectionRect;
     if PrepareSelectionRect([], VSelectionRect) then begin
       LayerMapNal.DrawSelectionRect(VSelectionRect);
     end;
   end;
   if GState.GPS_enab then begin
     LayerMapGPS.Redraw;
     UpdateGPSsensors;
   end;
   if aoper in [ao_add_line,ao_add_poly,ao_edit_line,ao_edit_poly] then begin
    TBEditPath.Visible:=(length(add_line_arr)>1);
    LayerMapNal.DrawNewPath(add_line_arr, (aoper=ao_add_poly)or(aoper=ao_edit_poly), lastpoint);
   end;
  end;
 if (y=MouseDownPoint.y)and(x=MouseDownPoint.x)and(aoper=ao_movemap)and(button=mbLeft) then
  begin
    VPWL.S:=0;
    VPWL.find:=false;
    if (FWikiLayer.Visible) then
     FWikiLayer.MouseOnReg(VPWL, Point(x,y));
    if (LayerMapMarks.Visible) then
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
    moveTrue:=point(x,y);
    exit;
  end;
 if (MapZoomAnimtion>0)or(
    (ssDouble in Shift)or(HiWord(GetKeyState(VK_DELETE))<>0)or(HiWord(GetKeyState(VK_INSERT))<>0))
    or(HiWord(GetKeyState(VK_F6))<>0)
   then begin
         moveTrue:=point(x,y);
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
 if movepoint>-1 then
  begin
   add_line_arr[movepoint]:=VLonLat;
   TBEditPath.Visible:=(length(add_line_arr)>1);
   LayerMapNal.DrawNewPath(add_line_arr, (aoper=ao_add_poly)or(aoper=ao_edit_poly), lastpoint);
   exit;
  end;
 if (aoper=ao_rect)and(rect_dwn)and(not(ssRight in Shift))
         then begin
               FSelectionRect.BottomRight:=VLonLat;
               LayerMapNal.DrawNothing;
               VSelectionRect := FSelectionRect;
               if PrepareSelectionRect(Shift,VSelectionRect) then begin
                 LayerMapNal.DrawSelectionRect(VSelectionRect);
               end;
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
 if MapZoomAnimtion=1 then exit;
 if MapMoving then begin
              FMainLayer.MoveTo(Point(MouseDownPoint.X-x, MouseDownPoint.Y-y));
              LayerSelection.MoveTo(Point(MouseDownPoint.X-x, MouseDownPoint.Y-y));
              LayerMapNal.MoveTo(Point(MouseDownPoint.X-x, MouseDownPoint.Y-y));
              LayerMapMarks.MoveTo(Point(MouseDownPoint.X-x, MouseDownPoint.Y-y));
              FWikiLayer.MoveTo(Point(MouseDownPoint.X-x, MouseDownPoint.Y-y));
              LayerMapGPS.MoveTo(Point(MouseDownPoint.X-x, MouseDownPoint.Y-y));
              FFillingMap.MoveTo(Point(MouseDownPoint.X-x, MouseDownPoint.Y-y));
              LayerGoto.MoveTo(Point(MouseDownPoint.X-x, MouseDownPoint.Y-y));
              FShowErrorLayer.MoveTo(Point(MouseDownPoint.X-x, MouseDownPoint.Y-y));
              LayerMapNavToMark.MoveTo(Point(MouseDownPoint.X-x, MouseDownPoint.Y-y));
             end
        else m_m:=point(x,y);
 if not(MapMoving) then begin
    LayerStatBar.Redraw;
 end;

 if (not ShowActivHint) then begin
   if (HintWindow<>nil) then begin
     HintWindow.ReleaseHandle;
     FreeAndNil(HintWindow);
    end;
  end;
 ShowActivHint:=false;
 if not(MapMoving)and((moveTrue.x<>X)or(moveTrue.y<>y))and(GState.ShowHintOnMarks) then
  begin
   FPWL.S:=0;
   FPWL.find:=false;
   if (FWikiLayer.Visible) then
     FWikiLayer.MouseOnReg(FPWL,Point(x,y));
   if (LayerMapMarks.Visible) then
     MouseOnMyReg(FPWL,Point(x,y));
   if (FPWL.find) then
    begin
     if HintWindow<>nil then HintWindow.ReleaseHandle;
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
      if HintWindow=nil then
       begin
        HintWindow:=THintWindow.Create(Self);
        HintWindow.Brush.Color:=clInfoBk;
        HintWindow.Font.Charset:=RUSSIAN_CHARSET;
       end;
      hintrect:=HintWindow.CalcHintRect(Screen.Width, nms, nil);
      HintWindow.ActivateHint(Bounds(Mouse.CursorPos.x+13,Mouse.CursorPos.y-13,abs(hintrect.Right-hintrect.Left),abs(hintrect.Top-hintrect.Bottom)),nms);
      HintWindow.Repaint;
     end;
     ShowActivHint:=true;
    end;
  end;
 moveTrue:=point(x,y);
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
 setlength(GState.GPS_ArrayOfSpeed,0);
 setlength(GState.GPS_TrackPoints,0);
 GPSpar.maxspeed:=0;
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
 case aoper of
  ao_line: begin
         if length(length_arr)>0 then setlength(length_arr,length(length_arr)-1);
         TBEditPath.Visible:=(length(length_arr)>1);
         LayerMapNal.DrawLineCalc(length_arr, LenShow);
        end;
  ao_Reg : begin
         if length(reg_arr)>0 then setlength(reg_arr,length(reg_arr)-1);
         TBEditPath.Visible:=(length(reg_arr)>1);
         LayerMapNal.DrawReg(reg_arr);
        end;
  ao_add_poly,ao_add_line,ao_edit_line,ao_edit_poly:
        if lastpoint>0 then
        begin
         if length(add_line_arr)>0 then delfrompath(lastpoint);
         TBEditPath.Visible:=(length(add_line_arr)>1);
         LayerMapNal.DrawNewPath(add_line_arr, (aoper=ao_add_poly)or(aoper=ao_edit_poly), lastpoint);
        end;
 end;
end;

procedure TFmain.TBEditPathLabelClick(Sender: TObject);
begin
  if aoper = ao_line then begin
    LenShow:=not(LenShow);
    LayerMapNal.DrawLineCalc(length_arr, LenShow);
  end;
end;

procedure TFmain.TBEditPathSaveClick(Sender: TObject);
var result:boolean;
begin
  result := false;
  case aoper of
    ao_add_Poly: begin
      result:=SavePolyModal(-1, add_line_arr);
    end;
    ao_edit_poly: begin
      result:=SavePolyModal(EditMarkId, add_line_arr);
    end;
    ao_add_Line: begin
      result:=SaveLineModal(-1, add_line_arr, marshrutcomment);
    end;
    ao_edit_line: begin
      result:=SaveLineModal(EditMarkId, add_line_arr, '');
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
 Self.Enabled:=false;
 FMarksExplorer.ShowModal;
 Self.Enabled:=true;
 generate_im;
end;

procedure TFmain.NSRTM3Click(Sender: TObject);
var
  VLonLat:TExtendedPoint;
begin
  VLonLat := GState.ViewState.VisiblePixel2LonLat(MouseDownPoint);
  TextToWebBrowser(SAS_STR_WiteLoad, Fbrowser.EmbeddedWB1);
  Fbrowser.Visible := true;
  Fbrowser.EmbeddedWB1.Navigate('http://ws.geonames.org/srtm3?lat='+R2StrPoint(VLonLat.y)+'&lng='+R2StrPoint(VLonLat.x));
end;

procedure TFmain.NGTOPO30Click(Sender: TObject);
var
  VLonLat:TExtendedPoint;
begin
  VLonLat := GState.ViewState.VisiblePixel2LonLat(MouseDownPoint);
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
 FWikiLayer.MouseOnReg(FPWL, moveTrue);
 if (not NMarkNav.Checked) then begin
   id:=strtoint(FPWL.numid);
   VMark := GetMarkByID(id);
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
 for i:=0 to length(add_line_arr)-1 do
  url:=url+'&x'+inttostr(i)+'='+R2StrPoint(add_line_arr[i].x)+'&y'+inttostr(i)+'='+R2StrPoint(add_line_arr[i].y);
 if GetStreamFromURL(ms,url,'text/javascript; charset=utf-8')>0 then
  begin
   ms.Position:=0;
   pathstr:='';
   repeat
    BufferLen:=ms.Read(Buffer,SizeOf(Buffer));
    pathstr:=pathstr+Buffer;
   until (BufferLen=0)or(BufferLen<SizeOf(Buffer));

   SetLength(add_line_arr,0);
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
        SetLength(add_line_arr,length(add_line_arr)+1);
        posit:=PosEx('"x" : "',pathstr,posit);
        posit2:=PosEx('", "y" : "',pathstr,posit);
        add_line_arr[length(add_line_arr)-1].X:=str2r(copy(pathstr,posit+7,posit2-(posit+7)));
        posit:=PosEx('"',pathstr,posit2+10);
        add_line_arr[length(add_line_arr)-1].y:=str2r(copy(pathstr,posit2+10,posit-(posit2+10)));
        posit:=PosEx('{',pathstr,posit);
       except
        SetLength(add_line_arr,length(add_line_arr)-1);
        dec(lastpoint);
       end;
     posit:=PosEx('"totalLength"',pathstr,posit);
    end;
   except
   end;

   lastpoint:=length(add_line_arr)-1;
   if meters>1000 then marshrutcomment:=SAS_STR_MarshLen+RoundEx(meters/1000,2)+' '+SAS_UNITS_km
                  else marshrutcomment:=SAS_STR_MarshLen+inttostr(meters)+' '+SAS_UNITS_m;
   DateT1:=SecondToTime(seconds);
   dd:=DaysBetween(0,DateT1);
   timeT1:='';
   if dd>0 then timeT1:=inttostr(dd)+' дней, ';
   timeT1:=timeT1+TimeToStr(DateT1);
   marshrutcomment:=marshrutcomment+#13#10+SAS_STR_Marshtime+timeT1;
  end
 else ShowMessage('Connect error!');
 TBEditPath.Visible:=(length(add_line_arr)>1);
 LayerMapNal.DrawNewPath(add_line_arr, (aoper=ao_add_poly)or(aoper=ao_edit_poly), lastpoint);
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
  VFillingMapType: TMapType;
begin
  VFillingMapType := FFillingMap.SourceSelected;
  if TTBXItem(sender).Tag=0 then begin
    if Vfillingmaptype<>nil then begin
      TTBXItem(FTBFillingItemList.GetByGUID(VFillingMapType.GUID)).Checked:=false;
      Vfillingmaptype:=nil;
    end;
    TBfillMapAsMain.Checked:=true;
  end else begin
    TBfillMapAsMain.Checked:=false;
    if Vfillingmaptype<>nil then begin
      TTBXItem(FTBFillingItemList.GetByGUID(VFillingMapType.GUID)).Checked:=false;
    end;
    Vfillingmaptype:=TMapType(TTBXItem(sender).Tag);
    TTBXItem(FTBFillingItemList.GetByGUID(VFillingMapType.GUID)).Checked:=true;
  end;
  FFillingMap.SetSourceMap(VFillingMapType, FFillingMap.SourceZoom);
end;

procedure TFmain.NMarksCalcsLenClick(Sender: TObject);
var
  VId: Integer;
  VMark: TMarkFull;
  VLen: Extended;
  VMessage: string;
begin
  VId := strtoint(FPWL.numid);
  VMark := GetMarkByID(VId);
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
  VMark := GetMarkByID(VId);
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
  VMark := GetMarkByID(VId);
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
  case aoper of
   ao_reg: begin
         SetLength(reg_arr,length(reg_arr)+1);
         reg_arr[length(reg_arr)-1]:=reg_arr[0];
         LayerMapNal.DrawNothing;
         Fsaveas.Show_(GState.ViewState.GetCurrentZoom,reg_arr);
         setalloperationfalse(ao_movemap);
         LayerSelection.Redraw;
        end;
  end;
end;

procedure TFmain.TBItem1Click(Sender: TObject);
begin
 if ((GState.Localization<>LANG_RUSSIAN)and(TTBXItem(Sender).tag=0))or
    ((GState.Localization<>LANG_ENGLISH)and(TTBXItem(Sender).tag=1)) then ShowMessage(SAS_MSG_need_reload_application);
 case TTBXItem(Sender).tag of
  0:GState.Localization:=LANG_RUSSIAN;
  1:GState.Localization:=LANG_ENGLISH;
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

procedure TFmain.SBClearSensorClick(Sender: TObject);
begin
 if (MessageBox(handle,pchar(SAS_MSG_youasurerefrsensor+'?'),pchar(SAS_MSG_coution),36)=IDYES) then begin
   case TSpeedButton(sender).Tag of
    1: GPSpar.sspeed:=0;
    2: GPSpar.len:=0;
    3: GPSpar.Odometr:=0;
    4: GPSpar.maxspeed:=0;
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
 for i:=0 to length(add_line_arr)-2 do begin
 if conerr then Continue;
 url:=url+'&flat='+R2StrPoint(add_line_arr[i].y)+'&flon='+R2StrPoint(add_line_arr[i].x)+
          '&tlat='+R2StrPoint(add_line_arr[i+1].y)+'&tlon='+R2StrPoint(add_line_arr[i+1].x);
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
   add_line_arr:=add_line_arr_b;
   SetLength(add_line_arr_b,0);
   lastpoint:=length(add_line_arr)-1;
 end;
 TBEditPath.Visible:=(length(add_line_arr)>1);
  LayerMapNal.DrawNewPath(add_line_arr, (aoper=ao_add_poly)or(aoper=ao_edit_poly), lastpoint);
end;

procedure TFmain.TBXItem5Click(Sender: TObject);
begin
  if GState.GPS_enab then begin
    if AddNewPointModal(GState.GPS_TrackPoints[length(GState.GPS_TrackPoints)-1]) then begin
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

procedure TFmain.TBXItem7Click(Sender: TObject);
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
  VThread: ThreadAllLoadMap;
begin
  if (OpenSessionDialog.Execute)and(FileExists(OpenSessionDialog.FileName)) then begin
    if ExtractFileExt(OpenSessionDialog.FileName)='.sls' then begin
      Fmain.Enabled:=true;
      VLog := TLogForTaskThread.Create(5000, 0);
      VSimpleLog := VLog;
      VThreadLog := VLog;
      VThread := ThreadAllLoadMap.Create(VSimpleLog, OpenSessionDialog.FileName, GState.SessionLastSuccess);
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

procedure TFmain.NShowSelectionClick(Sender: TObject);
begin
 LayerSelection.Visible:=TTBXItem(sender).Checked;
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
  marksFilter:string;
  VCategoryFilter: string;
  VMarksIterator: TMarksIteratorVisibleInRectIgnoreEdit;
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
  VMarksIterator := TMarksIteratorVisibleInRectIgnoreEdit.Create(VZoom, VLonLatRect);
  try
    While VMarksIterator.Next do begin
      VMark := VMarksIterator.Current;
      VMarkLonLatRect := VMark.LLRect;

      if((VLonLatRect.Right>VMarkLonLatRect.Left)and(VLonLatRect.Left<VMarkLonLatRect.Right)and
      (VLonLatRect.Bottom<VMarkLonLatRect.Top)and(VLonLatRect.Top>VMarkLonLatRect.Bottom))then begin
        poly := VMark.Points;
        if length(poly)=1 then begin
          APWL.name:=VMark.name;
          APWL.descr:=VMark.Desc;
          APWL.numid:=IntToStr(VMark.id);
          APWL.find:=true;
          APWL.type_:=ROTpoint;
          exit;
        end else begin
          arLL := VConverter.PoligonProject(VZoom + 8, poly);
          if (poly[0].x<>poly[length(poly)-1].x)or
          (poly[0].y<>poly[length(poly)-1].y)then begin
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
  VPresenter: ISearchResultPresenter;
  VGeoCoder: IGeoCoder;
  VProxy: IProxySettings;
begin
  VGoto := TMapViewGotoOnFMain.Create;
  VPresenter := TSearchResultPresenterWithForm.Create(VGoto);
  VProxy := TProxySettingsFromTInetConnect.Create(GState.InetConnect);

  VGeoCoder := TGeoCoderByGoogle.Create(VProxy);
  FGoogleSearch := TGeoSearcher.Create(VGeoCoder, VPresenter);

  VGeoCoder := TGeoCoderByYandex.Create(VProxy);
  FYandexSerach := TGeoSearcher.Create(VGeoCoder, VPresenter);
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

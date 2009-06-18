unit Unit1;
interface
uses
  Windows, Registry, Messages, SysUtils, Forms, GR32, GR32_Resamplers,GR32_Layers,
  GR32_Polygons, TB2Item,  TB2Dock, TB2Toolbar, math, ShellApi, inifiles,
  UTrAllLoadMap,  Classes, Menus,  RXSlider, UThreadScleit, Ugeofun, UWikiLayer,
  MSHTML,variants, ActiveX, ComCtrls, ShlObj, ComObj, GR32_Filters,
  EmbeddedWB, TB2ExtItems, midaslib, ULogo, UMapType, Graphics, StdCtrls,
  DB, UThreadExport, GR32_Image, UResStrings, WinInet, SHDocVw_EWB, DBClient,
  MXPMenu, ZylCustomGPSReceiver, ZylGPSReceiver, BMSearch, EwbCore, ImgList,
  Dialogs, Controls, OleCtrls, UFillingMap;

type
  TlastLoad = record
    x,y:longint;
    z:byte;
    mt:Longint;
    use:boolean;
  end;

  TArrLL = array [0..0] of TExtendedPoint;
  PArrLL = ^TArrLL;

  TAOperation = (
    movemap,
    add_line ,
    add_poly,
    add_point,
    line,
    rect,
    reg
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
    MainMenu: TMainMenu;
    N2: TMenuItem;
    NZoomIn: TMenuItem;
    NZoomOut: TMenuItem;   
    N12: TMenuItem;
    N14: TMenuItem;
    NCalcRast: TMenuItem;
    N5: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    N6: TMenuItem;
    N1: TMenuItem;
    N4: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N011: TMenuItem;
    N021: TMenuItem;
    N031: TMenuItem;
    N041: TMenuItem;
    N051: TMenuItem;
    N061: TMenuItem;
    N071: TMenuItem;
    N081: TMenuItem;
    N19: TMenuItem;
    NFoolSize: TMenuItem;
    NGoToCur: TMenuItem;
    Nbackload: TMenuItem;
    Nanimate: TMenuItem;
    NShowGran: TMenuItem;
    N10: TMenuItem;
    NSRCinet: TMenuItem;
    NSRCesh: TMenuItem;
    NSRCic: TMenuItem;
    NSMB: TMenuItem;
    N3: TMenuItem;
    N16: TMenuItem;
    NCiclMap: TMenuItem;
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
    N29: TMenuItem;
    NMainToolBarShow: TMenuItem;
    NZoomToolBarShow: TMenuItem;
    NsrcToolBarShow: TMenuItem;
    EmbeddedWB1_: TEmbeddedWB;
    N000: TMenuItem;
    N001: TMenuItem;
    N002: TMenuItem;
    N003: TMenuItem;
    N004: TMenuItem;
    N005: TMenuItem;
    N006: TMenuItem;
    N007: TMenuItem;
    NGPSToolBarShow: TMenuItem;
    N30: TMenuItem;
    N31: TMenuItem;
    Showstatus: TMenuItem;
    ShowMiniMap: TMenuItem;
    ShowLine: TMenuItem;
    PopupMSmM: TPopupMenu;
    NMMtype_0: TMenuItem;
    N32: TMenuItem;
    ImagesSrc24: TImageList;
    N37: TMenuItem;
    N38: TMenuItem;
    NRECT: TMenuItem;
    NREGION: TMenuItem;
    N41: TMenuItem;
    N42: TMenuItem;
    Google1: TMenuItem;
    N43: TMenuItem;
    TBDockLeft: TTBDock;
    TBDock: TTBDock;
    TBMainToolBar: TTBToolbar;
    TBmove: TTBItem;
    TBRectSave: TTBSubmenuItem;
    TBRECT: TTBItem;
    TBREGION: TTBItem;
    TBItem4: TTBItem;
    TBItem1: TTBItem;
    TBCalcRas: TTBItem;
    TBSeparatorItem1: TTBSeparatorItem;
    TBMapZap: TTBSubmenuItem;
    TBMapZap0: TTBItem;
    TBMapZap1: TTBItem;
    TBMapZap2: TTBItem;
    TBMapZap3: TTBItem;
    TBMapZap4: TTBItem;
    TBMapZap5: TTBItem;
    TBMapZap6: TTBItem;
    TBMapZap7: TTBItem;
    TBMapZap8: TTBItem;
    TBGoTo: TTBSubmenuItem;
    EditGoogleSrch: TTBEditItem;
    TBSeparatorItem3: TTBSeparatorItem;
    TBFullSize: TTBItem;
    SrcToolbar: TTBToolbar;
    TBSrc: TTBSubmenuItem;
    TBinet: TTBItem;
    TBcache: TTBItem;
    TBcin: TTBItem;
    TBSMB: TTBSubmenuItem;
    TBExit: TTBToolbar;
    TBItem2: TTBItem;
    GPSToolbar: TTBToolbar;
    TBGPSconn: TTBItem;
    TBGPSPath: TTBSubmenuItem;
    TBItem3: TTBItem;
    TBGPSToPoint: TTBItem;
    TBDockBottom: TTBDock;
    TBDockRight: TTBDock;
    TBImageList2: TTBImageList;
    TBLoadSelFromFile: TTBItem;
    NLoadSelFromFile: TMenuItem;
    TBEditItem1: TTBEditItem;
    GPS1: TMenuItem;
    NGPSconn: TMenuItem;
    NGPSPath: TMenuItem;
    NGPSToPoint: TMenuItem;
    N48: TMenuItem;
    NSaveTreck: TMenuItem;
    OpenDialog1: TOpenDialog;
    YaLink: TMenuItem;
    kosmosnimkiru1: TMenuItem;
    N51: TMenuItem;
    TBLayerSel: TTBSubmenuItem;
    MapIcons24: TImageList;
    NLayerSel: TMenuItem;
    MapIcons18: TImageList;
    Ninvertcolor: TMenuItem;
    TBMarksToolbar: TTBToolbar;
    TBAdd_Poly: TTBItem;
    TBAdd_Line: TTBItem;
    TBAdd_Point: TTBItem;
    DataSource1: TDataSource;
    TBItem5: TTBItem;
    NMarkDel: TMenuItem;
    NMarkEdit: TMenuItem;
    NMarkSep: TMenuItem;
    NMarksBarShow: TMenuItem;
    NMarkOper: TMenuItem;
    livecom1: TMenuItem;
    N13: TMenuItem;
    ImageList1: TImageList;
    ImageAtlas1: TMenuItem;
    SaveDialog1: TSaveDialog;
    N26: TMenuItem;
    N27: TMenuItem;
    DigitalGlobe1: TMenuItem;
    NSubMenuSmItem: TMenuItem;
    ldm: TMenuItem;
    dlm: TMenuItem;
    TBControlItem3: TTBControlItem;
    Label1: TLabel;
    GPSReceiver: TZylGPSReceiver;
    N33: TMenuItem;
    NMapParams: TMenuItem;
    N34: TMenuItem;
    N35: TMenuItem;
    SaveLink: TSaveDialog;
    TBEditItem2: TTBEditItem;
    TBItemDelTrack: TTBItem;
    N36: TMenuItem;
    N39: TMenuItem;
    N40: TMenuItem;
    NGShScale0: TMenuItem;
    NGShScale1000000: TMenuItem;
    NGShScale500000: TMenuItem;
    NGShScale200000: TMenuItem;
    NGShScale100000: TMenuItem;
    NGShScale50000: TMenuItem;
    NGShScale25000: TMenuItem;
    NGShScale10000: TMenuItem;
    TBEditPath: TTBToolbar;
    TBEditPathSave: TTBItem;
    TBEditPathLabel: TTBItem;
    TBEditPathDel: TTBItem;
    WebBrowser1: TEmbeddedWB;
    ZoomToolBar: TTBToolbar;
    TBZoom_out: TTBItem;
    TBControlItem1: TTBControlItem;
    TBZoomIn: TTBItem;
    TBSeparatorItemtr: TTBSeparatorItem;
    TBControlItem2: TTBControlItem;
    labZoom: TLabel;
    RxSlider1: TRxSlider;
    N44: TMenuItem;
    NGoToForum: TMenuItem;
    NGoToSite: TMenuItem;
    DSKategory: TDataSource;
    TBItem6: TTBItem;
    TBSeparatorItem24: TTBSeparatorItem;
    N45: TMenuItem;
    N46: TMenuItem;
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
    procedure N18Click(Sender: TObject);
    procedure N17Click(Sender: TObject);
    procedure ThreadDone(Sender: TObject);
    procedure NSRCinetClick(Sender: TObject);
    procedure N16Click(Sender: TObject);
    procedure TBRECTClick(Sender: TObject);
    procedure TBRectSaveClick(Sender: TObject);
    procedure TBItem1Click(Sender: TObject);
    procedure TBMapZapPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure TBMapZap1Click(Sender: TObject);
    procedure TBCalcRasClick(Sender: TObject);
    procedure NCiclMapClick(Sender: TObject);
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
    procedure TBItem4Click(Sender: TObject);
    procedure ShowstatusClick(Sender: TObject);
    procedure ShowMiniMapClick(Sender: TObject);
    procedure ShowLineClick(Sender: TObject);
    procedure NMMtype_0Click(Sender: TObject);
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
    procedure GPSReceiverDisconnect(Sender: TObject;
      const Port: TCommPort);
    procedure GPSReceiverConnect(Sender: TObject; const Port: TCommPort);
    procedure GPSReceiverTimeout(Sender: TObject);
    procedure NMapParamsClick(Sender: TObject);
    procedure mapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure mapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure mapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure N35Click(Sender: TObject);
    procedure TBEditItem2AcceptText(Sender: TObject; var NewText: String;
      var Accept: Boolean);
    procedure TBItemDelTrackClick(Sender: TObject);
    procedure NGShScale0Click(Sender: TObject);
    procedure TBEditPathDelClick(Sender: TObject);
    procedure TBEditPathLabelClick(Sender: TObject);
    procedure TBEditPathSaveClick(Sender: TObject);
    procedure TBEditPathClose(Sender: TObject);
    procedure NGoToForumClick(Sender: TObject);
    procedure NGoToSiteClick(Sender: TObject);
    procedure TBItem6Click(Sender: TObject);
    procedure NSRTM3Click(Sender: TObject);
    procedure NGTOPO30Click(Sender: TObject);
  private
   procedure DoMessageEvent(var Msg: TMsg; var Handled: Boolean);
   procedure WMGetMinMaxInfo(var msg: TWMGetMinMaxInfo);message WM_GETMINMAXINFO;
  public
   FillingMap:TFillingMap;
   XPMenu:TMXPMenu;
   procedure createdirif(path:string);
   procedure generate_im(lastload:TLastLoad;err:string);
   procedure sm_im_reset(x,y:integer);
   function  toSh:string;
   function  X2AbsX(Ax:integer;Azoom:byte):integer;
   function  X2Lon(X:integer):extended;
   function  Y2Lat(Y:integer):extended;
   function  Lon2X(Lon:real):integer;
   function  Lat2Y(lat:real):integer;
   function  Lon2Xf(Lon:real):real;
   function  Lat2Yf(lat:real):real;
   procedure topos(lat,lon:real;zoom_:byte;draw:boolean);
   function  R2ShortStr(r:real;z:byte;s1,s2:string):string;
   procedure zooming(x:byte;move:boolean);
   function  find_length(StartLat,EndLat,StartLong,EndLong:double):real;
   function  timezone(lon,lat:real):TDateTime;
   procedure drawLineCalc;
   procedure drawPath(pathll:array of TExtendedPoint;new:boolean;color1,color2:TColor32;linew:integer;poly:boolean);
   procedure drawReg;
   function  loadpre(var spr:TBitmap32;x,y:integer;Azoom:byte;Amap:PMapType):boolean;
   procedure generate_mapzap;
   procedure draw_point;
   function  str2r(inp:string):real;
   procedure ThreadSclDone(Sender: TObject);
   procedure paint_Line;
   procedure selectMap(num:PMapType);
   function lon2str(Alon:real):string;
   function lat2str(Alat:real):string;
   function kb2KbMbGb(kb:real):string;
   procedure generate_granica;
   procedure drawLineGPS;
   procedure sm_im_reset_type2(x,y:integer);
   procedure ShowCaptcha(URL:string);
   procedure drawRect(Shift:TShiftState);
   procedure ShowErrScript(DATA:string);
   function mouseXY2Pos(Pnt:TPoint):TPoint;
   procedure setalloperationfalse(newop:TAOperation);
   procedure insertinpath(pos:integer);
   procedure delfrompath(pos:integer);
   procedure ThreadExportDone(Sender: TObject);
   procedure generate_lonlan(dx,dy:integer);
   procedure DrawGenShBorders;
   procedure LayerMinMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
   procedure LayerMinMapMouseUP(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
   procedure LayerMinMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  end;

  TName = record
   FileURL, FileName: String;
  end;

  Tsm_map = record
   size_dw,zooming,m_dwn:boolean;
   width,height,dx,dy:integer;
   pos:TPoint;
   alpha,zoom,z1mz2:byte;
   maptype:PMapType;
   PlusButton,MinusButton,SmMapBitmap:TBitmap32;
  end;

  TWikim_set = record
   MainColor:TColor;
   FonColor:TColor;
  end;

  TGPSpar = record
   speed:real;
   len:extended;
   sspeed:real;
   maxspeed:real;
   nap:integer;
  end;

  TInetConnect = record
    proxyused,userwinset,uselogin:boolean;
    proxystr,loginstr,passstr:string;
  end;

  TNavOnMark = class
   use:boolean;
   id:integer;
   ll:TExtendedPoint;
   width:integer;
   public
   procedure draw;
  end;

const
  SASVersion='90617';
  ENU=LANG_ENGLISH;
  RUS=LANG_RUSSIAN;// $00000419;
  MerkElipsK=0.0000001;
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы
  R2D: Double = 57.295779513082320876798154814105; // Константа для преобразования радиан в градусы
  zoom:array [1..24] of longint = (256,512,1024,2048,4096,8192,16384,32768,65536,
                                   131072,262144,524288,1048576,2097152,4194304,
                                   8388608,16777216,33554432,67108864,134217728,
                                   268435456,536870912,1073741824,2147483647);
  GSHprec=100000000;
  Codes64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

var
  Fmain:TFmain;
  PWL:TResObj;
  //exct,ra,rb:extended;
  zoom_size,zoom_mapzap,source,num_format,show_point,zoom_line,
  poly_zoom_save,resampling,llStrType:byte;
  All_Dwn_Kb:Currency;
  ShowActivHint:boolean;
  GPS_enab:boolean;
  mx,my,All_Dwn_Tiles,gamman,contrastn,vo_ves_ecr,anim_zoom, GShScale,
    zoom_in,mWd2,mHd2,yhgpx,xhgpx,hg_x,hg_y,pr_x,pr_y,GPS_timeout,GPS_update,GPS_SizeTrack:integer;
  move,m_up,m_m,POS,oldPOS,moveTrue:Tpoint;
  notpaint,invertcolor,dwn,start,close_,vo_ves_ecran,ShowMapName, GoNextTile, FirstLat,backload,animate,BorderText,
    mouse_inv,sparam,ban_pg_ld,LenShow,CiclMap,Maximized,GPS_path,GPS_go,sizing,dblDwnl,SaveTileNotExists:boolean;
  spr,mapbuf:TBitmap32;
  DefCache:byte;
  BorderColor,MapZapColor:TColor;
  BorderAlpha,MapZapAlpha:byte;
  sat_map_both:PMapType;
  marksicons:TStringList;
  movepoint,lastpoint:integer;
  //COMportpar
  TilesOut,BaudRate:integer;
  Find_Zoom:byte;
  rect_arr:array [0..1] of TextendedPoint;
  rect_dwn,rect_p2:boolean;
  InetConnect:TInetConnect;
  //x
  aoper:TAOperation;
  Deg:real;
  NewCPath_,OldCPath_,ESCpath_,GMTilespath_,dwnlstr,GPS_COM:string;
  GPS_arr_speed:array of real;
  length_arr,add_line_arr,GPS_arr:array of TExtendedPoint;
  GPS_popr:TExtendedPoint;
  GPS_Log:boolean;
  GPS_SizeStr:integer;
  GPS_colorStr:TColor;
  GPS_LogFile:TextFile;
  reg_arr,poly_save:array of TExtendedPoint;
  Names:TName;
  sm_map:Tsm_map;
  RectWindow:TRect=(Left:0;Top:0;Right:0;Bottom:0);
  THLoadMap1: ThreadAllLoadMap;
  LayerMap,LayerMapWiki,LayerMapMarks,LayerMapScale,layerLineM,LayerMinMap,LayerStatBar,LayerMapNal,LayerMapGPS: TBitmapLayer;
  h: THintWindow;
  oldLayerIndex:integer;
  curBuf:TCursor;
  OldFormWH:TPoint;
  Wikim_set:TWikim_set;
  TilesLoad:TStringList;
  nilLastLoad:TLastLoad;
  change_scene:boolean;
  GPSpar:TGPSpar;
  GOToSelIcon:TBitmap32;
  localization:integer;
  ProgrammPath:string;
  NavOnMark:TNavOnMark;
  function  ffpath(x,y:longint;Azoom:byte;AMap:TMapType;short:boolean):string;
  procedure Gamma(Bitmap: TBitmap32);
  function c_GetTempPath: string;
  procedure CopyStringToClipboard(s: Widestring);
  procedure CopyBtmToClipboard(btm:TBitmap);

implementation
uses Unit2,UAbout,Usettings,USaveas,UProgress,UaddPoint,Unit4,
  USelLonLat, StrUtils,UImgFun, UtimeZones, PNGimage,
  UaddLine, UaddPoly, DateUtils, UEditMap, Ubrowser, Types, UMarksExplorer;
{$R *.dfm}

procedure TNavOnMark.draw;
var Polygon:TPolygon32;
    ke,ks:TExtendedPoint;
    dl:integer;
    r,TanOfAngle,D,Angle:Extended;

begin
 Polygon := TPolygon32.Create;
 Polygon.Antialiased := true;
 polygon.AntialiasMode:=am4times;

  ke:=extpoint(Fmain.Lon2Xf(ll.x),Fmain.lat2Yf(ll.y));
  ke:=extPoint(ke.X+(pr_x-mWd2),ke.y+(pr_y-mHd2));
  ks:=extPoint(pr_x,pr_y);
  dl:=GPS_SizeStr;
  //R:=sqrt(sqr(ks.X-ke.X)+sqr(ks.Y-ke.Y))/2-(dl div 2);
  if ks.x=ke.x then TanOfAngle:=MaxExtended/100 * Sign(ks.Y-ke.Y)
               else TanOfAngle:=(ks.Y-ke.Y)/(ks.X-ke.X);
  D:=Sqrt(Sqr(ks.X-ke.X)+Sqr(ks.Y-ke.Y));
  r:=D/2-(dl div 2);
  if mWd2>mHd2 then if R>mHd2 then r:=mHd2-(dl div 2) else
               else if R>mWd2 then r:=mWd2-(dl div 2);
  //ke.x:=ke.X+(ke.X-ks.X);
  //ke.y:=ke.y+(ke.y-ks.y);
  ke.x:=Round((R*kE.x+(D-R)*kS.X)/D);
  ke.y:=Round((R*kE.y+(D-R)*kS.Y)/D);
  Polygon.Add(FixedPoint(round(ke.X),round(ke.Y)));
  Angle:=ArcTan(TanOfAngle)+0.28;
  if ks.X < ke.X then Angle:=Angle+Pi;
  Polygon.Add(FixedPoint(round(ke.x) + Round(dl*Cos(Angle)),round(ke.Y) + Round(dl*Sin(Angle))));
  Angle:=ArcTan(TanOfAngle)-0.28;
  if ks.X < ke.X then Angle:=Angle+Pi;
  Polygon.Add(FixedPoint(round(ke.X) + Round(dl*Cos(Angle)),round(ke.Y) + Round(dl*Sin(Angle))));
  Polygon.DrawFill(LayerMap.Bitmap, SetAlpha(Color32(GPS_colorStr), 150));

 Polygon.Free;
end;

function DigitToHex(Digit: Integer): Char;
begin
  case Digit of
    0..9: Result := Chr(Digit + Ord('0'));
    10..15: Result := Chr(Digit - 10 + Ord('A'));
    else Result := '0';
  end;
end; // DigitToHex

function EncodeDG(S: string): string;
var i:integer;
begin
 result:=S;
 for i:=1 to length(s) do
  if ord(s[i]) mod 2 = 0 then result[i]:=chr(ord(s[i])+1)
                         else result[i]:=chr(ord(s[i])-1);
end;

function Encode64(S: string): string;
var i,a,x,b: Integer;
begin
 Result:='';
 a:=0;
 b:=0;
 for i := 1 to Length(s) do
  begin
   x:=Ord(s[i]);
   b:=b*256+x;
   a:=a+8;
   while a >= 6 do
    begin
     a := a-6;
     x := b div (1 shl a);
     b := b mod (1 shl a);
     Result := Result + Codes64[x + 1];
    end;
  end;
 if a>0 then Result:=Result+Codes64[(b shl (6-a))+1];
end;

function Decode64(S: string): string;
var i,a,x,b: Integer;
begin
 Result := '';
 a := 0;
 b := 0;
 for i := 1 to Length(s) do
  begin
   x := System.Pos(s[i], codes64) - 1;
   if x>=0 then begin
                 b := b * 64 + x;
                 a := a + 6;
                 if a >= 8 then
                  begin
                   a := a - 8;
                   x := b shr a;
                   b := b mod (1 shl a);
                   x := x mod 256;
                   Result := Result + chr(x);
                  end;
               end
           else Exit;
   end;
end;

function URLEncode(const S: string): string;
var i, idx, len: Integer;
begin
  len := 0;
  for i := 1 to Length(S) do
   if ((S[i] >= '0') and (S[i] <= '9')) or
      ((S[i] >= 'A') and (S[i] <= 'Z')) or
      ((S[i] >= 'a') and (S[i] <= 'z')) or (S[i] = ' ') or
      (S[i] = '_') or (S[i] = '*') or (S[i] = '-') or (S[i] = '.')
      then len := len + 1
      else len := len + 3;
  SetLength(Result, len);
  idx := 1;
  for i := 1 to Length(S) do
    if S[i] = ' ' then
    begin
      Result[idx] := '+';
      idx := idx + 1;
    end
    else if ((S[i] >= '0') and (S[i] <= '9')) or
    ((S[i] >= 'A') and (S[i] <= 'Z')) or
    ((S[i] >= 'a') and (S[i] <= 'z')) or
    (S[i] = '_') or (S[i] = '*') or (S[i] = '-') or (S[i] = '.') then
    begin
      Result[idx] := S[i];
      idx := idx + 1;
    end
    else
    begin
      Result[idx] := '%';
      Result[idx + 1] := DigitToHex(Ord(S[i]) div 16);
      Result[idx + 2] := DigitToHex(Ord(S[i]) mod 16);
      idx := idx + 3;
    end;
end; // URLEncode

function URLDecode(const S: string): string;
var i, idx, len, n_coded: Integer;
  function WebHexToInt(HexChar: Char): Integer;
  begin
    if HexChar < '0' then
      Result := Ord(HexChar) + 256 - Ord('0')
    else if HexChar <= Chr(Ord('A') - 1) then
      Result := Ord(HexChar) - Ord('0')
    else if HexChar <= Chr(Ord('a') - 1) then
      Result := Ord(HexChar) - Ord('A') + 10
    else Result := Ord(HexChar) - Ord('a') + 10;
  end;
begin
  len := 0;
  n_coded := 0;
  for i := 1 to Length(S) do
    if n_coded >= 1 then
    begin
      n_coded := n_coded + 1;
      if n_coded >= 3 then n_coded := 0;
    end
    else
    begin
      len := len + 1;
      if S[i] = '%' then n_coded := 1;
    end;
  SetLength(Result, len);
  idx := 0;
  n_coded := 0;
  for i := 1 to Length(S) do
    if n_coded >= 1 then
    begin
      n_coded := n_coded + 1;
      if n_coded >= 3 then
      begin
        Result[idx] := Chr((WebHexToInt(S[i - 1]) * 16 + WebHexToInt(S[i])) mod 256);
        n_coded := 0;
      end;
    end
    else
    begin
      idx := idx + 1;
      if S[i] = '%' then n_coded := 1;
      if S[i] = '+' then Result[idx] := ' '
                    else Result[idx] := S[i];
    end;
end;

procedure CopyBtmToClipboard(btm:TBitmap);
var hSourcDC, hDestDC, hBM, hbmOld: THandle;
begin
  hSourcDC := btm.Canvas.Handle;
  hDestDC := CreateCompatibleDC(hSourcDC);
  hBM := CreateCompatibleBitmap(hSourcDC, btm.width, btm.height);
  hbmold:= SelectObject(hDestDC, hBM);
  BitBlt(hDestDC, 0, 0, btm.width, btm.height, hSourcDC, 0, 0, SRCCopy);
  OpenClipBoard(fmain.handle);
  EmptyClipBoard;
  SetClipBoardData(CF_Bitmap, hBM);
  CloseClipBoard;
  SelectObject(hDestDC,hbmold);
  DeleteObject(hbm);
  DeleteDC(hDestDC);
  DeleteDC(hSourcDC);
end;

procedure CopyStringToClipboard(s: Widestring);
var hg: THandle;
    P: PChar;
begin
  if OpenClipboard(FMain.Handle) then
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

procedure TFmain.insertinpath(pos:integer);
begin
 SetLength(add_line_arr,length(add_line_arr)+1);
 CopyMemory(Pointer(integer(@add_line_arr[pos])+sizeOf(TExtendedPoint)),@add_line_arr[pos],(length(add_line_arr)-pos-1)*sizeOf(TExtendedPoint));
end;

function c_GetTempPath: string;
var Buffer: array[0..1023] of Char;
begin
  SetString(Result, Buffer, GetTempPath(Sizeof(Buffer) - 1,Buffer));
end;

procedure TFmain.delfrompath(pos:integer);
begin
 CopyMemory(@add_line_arr[pos],Pointer(integer(@add_line_arr[pos])+sizeOf(TExtendedPoint)),(length(add_line_arr)-pos-1)*sizeOf(TExtendedPoint));
 SetLength(add_line_arr,length(add_line_arr)-1);
 Dec(lastpoint);
end;

function LoadPNGintoBitmap32(destBitmap: TBitmap32; filename: String): boolean;
var PNGObject: TPNGObject;
    TransparentColor: TColor32;
    PixelPtr: PColor32;
    AlphaPtr: PByte;
    X, Y: Integer;
begin
  PNGObject := nil;
  try
    result := false;
    PNGObject:=TPngObject.Create;
    PNGObject.LoadFromFile(filename);
    destBitmap.Assign(PNGObject);
    destBitmap.ResetAlpha;
    case PNGObject.TransparencyMode of
      ptmPartial:
          if (PNGObject.Header.ColorType in [COLOR_GRAYSCALEALPHA,COLOR_RGBALPHA]) then
          begin
            PixelPtr := PColor32(@destBitmap.Bits[0]);
            for Y := 0 to destBitmap.Height - 1 do
            begin
              AlphaPtr := PByte(PNGObject.AlphaScanline[Y]);
              for X := 0 to destBitmap.Width - 1 do
              begin
                PixelPtr^:=(PixelPtr^ and $00FFFFFF) or (TColor32(AlphaPtr^) shl 24);
                Inc(PixelPtr);
                Inc(AlphaPtr);
              end;
            end;
          end;
      ptmBit:
        begin
          TransparentColor := Color32(PNGObject.TransparentColor);
          PixelPtr := PColor32(@destBitmap.Bits[0]);
          for X := 0 to (destBitmap.Height - 1) * (destBitmap.Width - 1) do
          begin
            if PixelPtr^ = TransparentColor then
              PixelPtr^ := PixelPtr^ and $00FFFFFF;
            Inc(PixelPtr);
          end;
        end;
    end;
    result := true;
  finally
    if Assigned(PNGObject) then FreeAndNil(PNGObject);
  end;
end;

procedure TFmain.setalloperationfalse(newop:TAOperation);
begin
 if aoper=newop then newop:=movemap;
 LayerMapNal.Bitmap.Clear(clBlack);
 LayerMapNal.Visible:=newop<>movemap;
 TBmove.Checked:=newop=movemap;
 TBCalcRas.Checked:=newop=line;
 TBRectSave.Checked:=(newop=reg)or(newop=rect);
 TBAdd_Point.Checked:=newop=Add_Point;
 TBAdd_Line.Checked:=newop=Add_line;
 TBAdd_Poly.Checked:=newop=Add_Poly;
 TBEditPath.Visible:=false;
 TBEditPathSave.Visible:=(newop=Add_line)or(newop=Add_Poly);
 TBEditPathLabel.Visible:=(newop=line);
 rect_dwn:=false;
 setlength(length_arr,0);
 setlength(add_line_arr,0);
 setlength(reg_arr,0);
 rect_p2:=false;
 lastpoint:=-1;
 case newop of
  movemap:  map.Cursor:=crDefault;
  line:     map.Cursor:=2;
  reg,rect: map.Cursor:=crDrag;
  Add_Point,Add_Poly,Add_Line: map.Cursor:=4;
 end;
 aoper:=newop;
end;

function ffpath(x,y:longint;Azoom:byte;AMap:TMapType;short:boolean):string;
function full(int,z:integer):string;
var s,s1:string;
    i:byte;
begin
 result:='';
 s:=inttostr(int);
 s1:=inttostr(zoom[z] div 256);
 for i:=length(s) to length(s1)-1 do result:=result+'0';
 result:=result+s;
end;
var os,prer:TPoint;
    i,ct:byte;
    sbuf,name:String;
begin
 if (AMap.CacheType=0) then ct:=DefCache
                       else ct:=AMap.CacheType;
 if x>=0 then x:=x mod zoom[Azoom]
         else x:=zoom[Azoom]+(x mod zoom[Azoom]);
 case ct of
 1:
 begin
   sbuf:=Format('%.*d', [2, Azoom]);
   if not(short) then result:=OldCpath_
                 else result:='';
   result:=result+AMap.NameInCache+'\'+sbuf+'\t';
   os.X:=zoom[Azoom]shr 1;
   os.Y:=zoom[Azoom]shr 1;
   prer:=os;
   for i:=2 to Azoom do
    begin
    prer.X:=prer.X shr 1;
    prer.Y:=prer.Y shr 1;
    if x<os.X
     then begin
           os.X:=os.X-prer.X;
           if y<os.y then begin
                            os.Y:=os.Y-prer.Y;
                            result:=result+'q';
                           end
                      else begin
                            os.Y:=os.Y+prer.Y;
                            result:=result+'t';
                           end;
          end
     else begin
           os.X:=os.X+prer.X;
           if y<os.y then begin
                           os.Y:=os.Y-prer.Y;
                           result:=result+'r';
                          end
                     else begin
                           os.Y:=os.Y+prer.Y;
                           result:=result+'s';
                          end;
         end;
    end;
  result:=result+AMap.ext;
 end;
 2:
 begin
  if not(short) then result:=NewCpath_
                else result:='';
  x:=x shr 8;
  y:=y shr 8;
  result:=result+AMap.NameInCache+format('\z%d\%d\x%d\%d\y%d',[Azoom,x shr 10,x,y shr 10,y])+AMap.ext;
 end;
 3:
 begin
   sbuf:=Format('%.*d', [2, Azoom]);
   if not(short) then result:=ESCpath_
                 else result:='';
   name:=sbuf+'-'+full(x shr 8,Azoom)+'-'+full(y shr 8,Azoom);
   if Azoom<7
    then result:=result+AMap.NameInCache+'\'+sbuf+'\'
    else if Azoom<11
          then result:=result+AMap.NameInCache+'\'+sbuf+'\'+Chr(59+Azoom)+
                       full((x shr 8)shr 5,Azoom-5)+full((y shr 8)shr 5,Azoom-5)+'\'
          else result:=result+AMap.NameInCache+'\'+'10'+'-'+full((x shr (Azoom-10))shr 8,10)+'-'+
                       full((y shr (Azoom-10))shr 8,10)+'\'+sbuf+'\'+Chr(59+Azoom)+
                       full((x shr 8)shr 5,Azoom-5)+full((y shr 8)shr 5,Azoom-5)+'\';
   result:=result+name+AMap.ext;
 end;
 4,41:
 begin
  if not(short) then result:=GMTilespath_
                else result:='';
  x:=x shr 8;
  y:=y shr 8;
  if ct=4 then result:=result+AMap.NameInCache+format('\z%d\%d\%d'+AMap.ext,[Azoom-1,Y,X])
          else result:=result+AMap.NameInCache+format('\z%d\%d_%d'+AMap.ext,[Azoom-1,Y,X]);
 end;
 end;
 if not(short)and(result[2]<>'\')and(system.pos(':',result)=0)
   then result:=ProgrammPath+result;
end;

function TFmain.mouseXY2Pos(Pnt:TPoint):TPoint;
begin
 Result:=Point(Pos.x-(mWd2-Pnt.X),Pos.y-(mHd2-Pnt.y));
end;


procedure rotate(var x,y:integer;c_x,c_y:integer;a_:real);
var s_,c_,r_:extended;
begin
 s_:=0; c_:=0;
 r_ := sqrt(sqr(y - c_y) + sqr(x - c_x));
 SinCos(a_ + arctan2((x - c_x), (y - c_y)), s_, c_);
 y:= round(c_y + r_ * c_);
 x:= round(c_x + r_ * s_);
end;

procedure Perspective(const X, Y, Z, Height, Basis: Double; var XP, YP: Double);
var Den: Double;
begin
  Den := Y + Basis;
  if Abs(Den) < 1E-100 then
    Den := 1E-100;
  XP := Basis * X / Den;
  YP := (Basis * Z + Height * Y) / Den;
end;

procedure TFmain.ShowCaptcha(URL:string);
begin
 ShellExecute(Handle, nil, PChar(URL), nil, nil, SW_RESTORE);
end;

//Обработка нажатий кнопоки и калесика
procedure TFmain.DoMessageEvent(var Msg: TMsg; var Handled: Boolean);
var z:integer;
begin

 if Fmain.Active then
  case Msg.message of
   WM_MOUSEWHEEL:if anim_zoom=0 then
                 begin
                  m_m:=moveTrue;
                  if mouse_inv then z:=-1 else z:=1;
                  if Msg.wParam<0 then Fmain.zooming(Zoom_size-(1*z),NGoToCur.Checked)
                                  else Fmain.zooming(Zoom_size+(1*z),NGoToCur.Checked);
                 end;
   WM_KEYFIRST: begin
                 if Msg.wParam=VK_RIGHT then pos.x:=pos.x+128;
                 if Msg.wParam=VK_Left then pos.x:=pos.x-128;
                 if Msg.wParam=VK_Down then pos.y:=pos.y+128;
                 if Msg.wParam=VK_Up then pos.y:=pos.y-128;
                 if (Msg.wParam=VK_RIGHT)or(Msg.wParam=VK_Left)or
                    (Msg.wParam=VK_Down)or(Msg.wParam=VK_Up)then
                    generate_im(nilLastLoad,'');
                end;
   WM_KEYUP:begin
             if (Msg.wParam=VK_Delete)and(aoper=line) then
               begin
                if length(length_arr)>0 then setlength(length_arr,length(length_arr)-1);
                drawLineCalc;
               end;
             if (Msg.wParam=VK_Delete)and(aoper=reg) then
               begin
                if length(reg_arr)>0 then setlength(reg_arr,length(reg_arr)-1);
                drawReg;
               end;
             if (Msg.wParam=VK_Delete)and(aoper in [add_line,add_poly]) then
              if length(add_line_arr)>0 then
               begin
                setlength(add_line_arr,length(add_line_arr)-1);
                dec(lastpoint);
                drawPath(add_line_arr,true,SetAlpha(ClRed32, 150),SetAlpha(ClWhite32, 50),3,aoper=add_poly);
               end;
             if (Msg.wParam=VK_ESCAPE)and(aoper=Reg) then
              if length(reg_arr)=0 then TBmoveClick(Fmain)
                                   else begin
                                         setlength(reg_arr,0);
                                         drawreg;
                                        end;
             if (Msg.wParam=VK_ESCAPE)and(aoper=line) then
              if length(length_arr)=0 then TBmoveClick(Fmain)
                                      else begin
                                            setlength(length_arr,0);
                                            drawLineCalc;
                                           end;
             if (Msg.wParam=VK_ESCAPE)and(aoper=rect) then
              begin
               if rect_dwn then begin
                                 setalloperationfalse(movemap);
                                 setalloperationfalse(rect);
                                end
                           else setalloperationfalse(movemap);     
              end;
             if (Msg.wParam=VK_ESCAPE)and(aoper=Add_Point) then setalloperationfalse(movemap);
             if (Msg.wParam=VK_ESCAPE)and(aoper in [add_line,add_poly]) then
               if length(add_line_arr)=0 then setalloperationfalse(movemap)
                                         else begin
                                               setlength(add_line_arr,0);
                                               lastpoint:=-1;
                                               drawPath(add_line_arr,true,setalpha(clRed32,150),setalpha(clWhite32,50),3,aoper=add_poly);
                                              end;
             if (Msg.wParam=13)and(aoper=add_Poly)and(length(add_line_arr)>1) then
              begin
               if FaddPoly.show_(add_line_arr,true) then
                begin
                 setalloperationfalse(movemap);
                 generate_im(nilLastLoad,'');
                end; 
              end;
             if (Msg.wParam=13)and(aoper=add_line)and(length(add_line_arr)>1) then
              begin
               if FaddLine.show_(add_line_arr,true) then
                begin
                 setalloperationfalse(movemap);
                 generate_im(nilLastLoad,'');
                end;
              end;
            end;
  end;
end;


function Float2Str(r:real):string;
begin
 if Frac(r)>0 then result:=inttostr(Trunc(r))+'.'+copy(floattostr(Frac(r)),3,1)
              else result:=inttostr(Trunc(r));
end;                          

function TFmain.kb2KbMbGb(kb:real):string;
begin
 result:=float2str(kb)+' '+SAS_UNITS_kb;
 if kb>1024 then result:=float2str(kb/1024)+' '+SAS_UNITS_mb;
 if kb>1048576 then result:=float2str(kb/1048576)+' '+SAS_UNITS_gb;
end;

procedure Highlight(aSource:TBitmap32; r,g,b:byte);
const
  trr = 0.33;
  trg = 0.35;
  trb = 0.31;
var i, j, rc,gc,bc,rgbc: Integer;
    rp,gp,bp:single;
    s: PColor32Array;
begin
   rc:=0;
   gc:=0;
   bc:=0;
   for i := 0 to aSource.Height - 1 do
   begin
     s := ASource.Scanline[i];
     for j := 0 to aSource.Width - 1 do
     begin
       rc:=rc+RedComponent(s^[0]);
       gc:=gc+GreenComponent(s^[0]);
       bc:=bc+BlueComponent(s^[0]);
       inc(s);
     end;
   end;
   rgbc:=rc+gc+bc;
   rp:=1+trr-(rc/rgbc);
   gp:=1+trg-(gc/rgbc);
   bp:=1+trb-(bc/rgbc);
   for i := 0 to aSource.Height - 1 do
   begin
     s := ASource.Scanline[i];
     for j := 0 to aSource.Width - 1 do
     begin
       s^[0]:=GR32.Color32(round(rp*RedComponent(s^[0])) mod 256,
                           round(gp*GreenComponent(s^[0])) mod 256,
                           round(bp*BlueComponent(s^[0])) mod 256);
       inc(s);
     end;
   end;
end;

procedure Contrast(Bitmap: TBitmap32; Value: double);
 function BLimit(B:Integer):Byte;
  begin
   if B<0 then Result:=0
          else if B>255 then Result:=255
                        else Result:=B;
  end;
var Dest: PColor32Array;
    x,y,mr,mg,mb,W,H:Integer;
    vd: Double;
begin
  if Value=0 then Exit;
  Value:=Value/10;
  W:= Bitmap.Width-1;
  H:= Bitmap.Height-1;
  mR:=128;
  mG:=128;
  mB:=128;
  if Value>0 then vd:=1+(Value/10)
             else vd:=1-(Sqrt(-Value)/10);
  for y:=0 to H do
   begin
    Dest:=Bitmap.ScanLine[y];
    for x:=0 to W do
     begin
      Dest^[0]:=GR32.Color32(BLimit(mR+Trunc((RedComponent(dest^[0])-mR)*vd)),
                             BLimit(mG+Trunc((GreenComponent(dest^[0])-mG)*vd)),
                             BLimit(mB+Trunc((BlueComponent(dest^[0])-mB)*vd)));
      Inc(Dest);
     end;
   end;
end;

procedure InvertBitmap(Bitmap: TBitmap32);
begin
 if invertcolor then InvertRGB(Bitmap,Bitmap);
end;

procedure Gamma(Bitmap: TBitmap32);
  function Power(Base, Exponent: Extended): Extended;
  begin
    Result := Exp(Exponent * Ln(Base));
  end;
var Dest: PColor32Array;
    X,Y: integer;
    GT: array[0..255] of Byte;
    L:Double;
begin
  Contrast(Bitmap, contrastn);
  InvertBitmap(Bitmap);
//  Highlight(Bitmap, 1,1,1);
//  Highlight(Bitmap,1,1,1);
  if gamman<>50 then
   begin
    if gamman<50 then L:=(gamman*2)/100
                 else L:=(gamman-40)/10;
    GT[0]:=0;
    for X := 1 to 255 do GT[X]:=Round(255*Power(X/255,1/L));
    for Y := 0 to Bitmap.Height-1 do
     begin
      Dest:=Bitmap.ScanLine[y];
      for X := 0 to Bitmap.Width-1 do
       begin
        Dest^[0]:= GR32.Color32(GT[RedComponent(dest^[0])],GT[GreenComponent(dest^[0])],GT[BlueComponent(dest^[0])]);
        Inc(Dest);
       end;
     end;
   end;
end;

function TFmain.X2AbsX(Ax:integer;Azoom:byte):integer;
begin
 if Ax>=0 then result:=Ax mod zoom[Azoom]
          else result:=zoom[Azoom]+(Ax mod zoom[Azoom])
end;

function TFmain.str2r(inp:string):real;
var p:integer;
begin
{ p:=System.pos('.',inp);
 if p=0 then p:=System.pos(',',inp);
 if p>0 then inp[p]:=DecimalSeparator;     }
 p:=System.pos(DecimalSeparator,inp);
 if p=0 then begin
              if DecimalSeparator='.' then p:=System.pos(',',inp)
                                      else p:=System.pos('.',inp);
              inp[p]:=DecimalSeparator;
             end;
 result:=strtofloat(inp);
end;

procedure TFmain.createdirif(path:string);
begin
 path:=copy(path, 1, LastDelimiter('\', path));
 if not(DirectoryExists(path)) then ForceDirectories(path);
end;

procedure TFmain.ThreadSclDone(Sender: TObject);
begin
 ThreadScleit(sender):=nil;
end;

procedure TFmain.ThreadExportDone(Sender: TObject);
begin
 ThreadExport(sender):=nil;
end;

procedure TFmain.ThreadDone(Sender: TObject);
begin
  ThreadAllLoadMap(sender).closeSession;
  if ThreadAllLoadMap(sender)=THLoadMap1 then
       begin
        THLoadMap1:=nil;
        exit;
       end;
   //ThreadAllLoadMap(sender):=nil;
{   if ThreadAllLoadMap(sender)=THLdMap_RightCl_1 then
     THLdMap_RightCl_1:=nil;
   if ThreadAllLoadMap(sender)=THLdMap_RightCl_2 then
     THLdMap_RightCl_2:=nil;}
   if ThreadAllLoadMap(sender).typeRect in [2,3] then
    begin
     //ThreadAllLoadMap(sender)._FProgress.Free;
     ThreadAllLoadMap(sender):=nil;
     if not((dwn)or(anim_zoom=1)) then
       begin
        move.X:=m_up.x;
        Fmain.generate_im(nilLastLoad,'');
       end; 
     exit;
    end;
   ThreadAllLoadMap(sender):=nil;
end;

procedure TFmain.drawRect(Shift:TShiftState);
var i,d256,kz,jj,j,bxy:integer;
    xy1,xy2,pxy1,pxy2:TPoint;
    zLonR,zLatR:extended;
    LonLatLT,LonLatRD:TExtendedPoint;
begin
  xy1:=Point(Lon2X(rect_arr[0].x),Lat2Y(rect_arr[0].y) );
  xy2:=Point(Lon2X(rect_arr[1].x),Lat2Y(rect_arr[1].y) );
  LayerMapNal.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
  LayerMapNal.Bitmap.Clear(clBlack);
  if (zoom_line in [99,0])or(zoom_line<zoom_size)
   then d256:=256
   else d256:=256 div round(power(2,zoom_line-zoom_size));
  if xy1.x>xy2.x then begin
                       bxy:=xy2.x;
                       xy2.x:=xy1.x;
                       xy1.x:=bxy;
                      end;
  if xy1.y>xy2.y then begin
                       bxy:=xy2.y;
                       xy2.y:=xy1.y;
                       xy1.y:=bxy;
                      end;
  pxy1.x:=(pos.X-(mWd2-xy1.x));
  pxy1.y:=(pos.y-(mHd2-xy1.y));
  pxy2.x:=(pos.X-(mWd2-xy2.x));
  pxy2.y:=(pos.y-(mHd2-xy2.y));
  if ssCtrl in Shift then
   begin
    pxy1.x:=(pxy1.x-(pxy1.x mod d256));
    xy1.x:=mWd2-(pos.x-pxy1.x);
    pxy1.y:=(pxy1.y-(pxy1.y mod d256));
    xy1.y:=mHd2-(pos.y-pxy1.y);
    pxy2.x:=((pxy2.x+d256)-((pxy2.x+d256) mod d256));
    xy2.x:=mWd2-(pos.x-pxy2.x);
    pxy2.y:=((pxy2.y+d256)-((pxy2.y+d256) mod d256));
    xy2.y:=mHd2-(pos.y-pxy2.y);
   end;
  if (ssShift in Shift)and(GShScale>0) then
   begin
    case GShScale of
      1000000: begin zLonR:=6; zLatR:=4; end;
       500000: begin zLonR:=3; zLatR:=2; end;
       200000: begin zLonR:=1; zLatR:=0.66666666666666666666666666666667; end;
       100000: begin zLonR:=0.5; zLatR:=0.33333333333333333333333333333333; end;
        50000: begin zLonR:=0.25; zLatR:=0.1666666666666666666666666666665; end;
        25000: begin zLonR:=0.125; zLatR:=0.08333333333333333333333333333325; end;
        10000: begin zLonR:=0.0625; zLatR:=0.041666666666666666666666666666625; end;
    end;
    LonLatLT:=GPos2LonLat(pxy1,zoom_size,sat_map_both);
    LonLatLT.X:=LonLatLT.X-(round(LonLatLT.X*GSHprec) mod round(zLonR*GSHprec))/GSHprec;
    LonLatLT.Y:=LonLatLT.Y-(round(LonLatLT.Y*GSHprec) mod round(zLatR*GSHprec))/GSHprec;
    if LonLatLT.X<0 then LonLatLT.X:=LonLatLT.X-zLonR;
    if LonLatLT.Y>0 then LonLatLT.Y:=LonLatLT.Y+zLatR;
    xy1:=GLonLat2Pos(LonLatLT,zoom_size,sat_map_both);
    xy1:=Point(mWd2-(pos.x-xy1.x),mHd2-(pos.y-xy1.y));

    LonLatRD:=GPos2LonLat(pxy2,zoom_size,sat_map_both);
    LonLatRD.X:=LonLatRD.X-(round(LonLatRD.X*GSHprec) mod round(zLonR*GSHprec))/GSHprec;
    LonLatRD.Y:=LonLatRD.Y-(round(LonLatRD.Y*GSHprec) mod round(zLatR*GSHprec))/GSHprec;
    if LonLatRD.X>=0 then LonLatRD.X:=LonLatRD.X+zLonR;
    if LonLatRD.Y<=0 then LonLatRD.Y:=LonLatRD.Y-zLatR;
    xy2:=GLonLat2Pos(LonLatRD,zoom_size,sat_map_both);
    xy2:=Point(mWd2-(pos.x-xy2.x),mHd2-(pos.y-xy2.y));
    if (rect_p2) then
     begin
      fsaveas.Show_(zoom_size,[LonLatLT,extPoint(LonLatRD.X,LonLatLT.Y),LonLatRD,extPoint(LonLatLT.X,LonLatRD.Y),LonLatLT]);
      rect_p2:=false;
      exit;
     end;
   end;
  if (rect_p2) then
   begin
    fsaveas.Show_(zoom_size,[GPos2LonLat(pxy1,zoom_size,sat_map_both),
                             GPos2LonLat(Point(pxy2.X,pxy1.Y),zoom_size,sat_map_both),
                             GPos2LonLat(pxy2,zoom_size,sat_map_both),
                             GPos2LonLat(Point(pxy1.X,pxy2.Y),zoom_size,sat_map_both),
                             GPos2LonLat(pxy1,zoom_size,sat_map_both)]);
    rect_p2:=false;
    exit;
   end;
  if not(rect_dwn) then exit;
  xy1.x:=xy1.x-(mWd2-pr_x)+1;
  xy1.y:=xy1.y-(mHd2-pr_y)+1;
  xy2.x:=xy2.x-(mWd2-pr_x)-1;
  xy2.y:=xy2.y-(mHd2-pr_y)-1;
  LayerMapNal.Bitmap.FillRectS(xy1.x,xy1.y,xy2.x,xy2.y,SetAlpha(clWhite32,20));
  LayerMapNal.Bitmap.FrameRectS(xy1.x,xy1.y,xy2.x,xy2.y,SetAlpha(clBlue32,150));
  LayerMapNal.Bitmap.FrameRectS(xy1.x-1,xy1.y-1,xy2.x+1,xy2.y+1,SetAlpha(clBlue32,150));
  kz:=256;
  while kz>=32 do
   begin
    i:=xy1.x-kz; while ((pos.X-(pr_x-i))mod kz)<>0 do inc(i);
    j:=xy1.y-kz; while ((pos.Y-(pr_y-j))mod kz)<>0 do inc(j);
    jj:= round(Log2(kz))-5;
    LayerMapNal.Bitmap.FrameRectS(i-jj,j-jj,i+((xy2.x+kz-i)div kz)*kz+jj,j+((xy2.y+kz-j)div kz)*kz+jj,SetAlpha(RGB(kz-1,kz-1,kz-1),255));
    LayerMapNal.Bitmap.Font.Size:=11;
    LayerMapNal.Bitmap.RenderText(xy2.x-((xy2.x-xy1.x)div 2)-42+jj*26,xy2.y-((xy2.y-xy1.y)div 2)-6,'x'+inttostr(zoom_size+3-jj),3,SetAlpha(RGB(kz-1,kz-1,kz-1),255));
    kz:=kz div 2;
   end;
end;

procedure TFmain.drawReg;
var i:integer;
    k1,k2:TPoint;
    Polygon: TPolygon32;
//    Outline: TPolygon32;
begin
 LayerMapNal.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
 Polygon := TPolygon32.Create;
 Polygon.Antialiased := true;
 Polygon.AntialiasMode := am32times;
 Polygon.FillMode := pfAlternate;
 with LayerMapNal.Bitmap do
  begin
   Clear(clBlack);
   Canvas.Pen.Style:=psSolid;
   Canvas.Brush.Color:=ClWhite;
   Canvas.Pen.Width:=1;
   if length(reg_arr)=1 then
    begin
     k1:=point(Lon2X(reg_arr[0].x),lat2Y(reg_arr[0].y));
     k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
     LayerMapNal.Bitmap.FillRectS(k1.X-3,k1.Y-3,k1.X+3,k1.Y+3,SetAlpha(ClGreen32,255));
    end;
   for i:=0 to length(reg_arr)-1 do
    begin
     k1:=point(Lon2X(reg_arr[i].x),lat2Y(reg_arr[i].y));
     k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
     Polygon.add(FixedPoint(k1.x, k1.Y));
    end;
 end;
 with Polygon.Outline do
  begin
   FillMode := pfWinding;
   with Grow(Fixed(2 / 2), 0.5) do
    begin
     DrawFill(LayerMapNal.Bitmap, SetAlpha(clBlue32, 180));
     free;
    end;
   free;
  end;
// Outline := Polygon.Outline.Grow(Fixed(2 / 2), 0.5);
// Outline.FillMode := pfWinding;
 Polygon.DrawFill(LayerMapNal.Bitmap, SetAlpha(clWhite32, 40));
// Outline.DrawFill(LayerMapNal.Bitmap, SetAlpha(clBlue32, 180));
 if length(reg_arr)>1 then
  begin
   k1:=point(Lon2X(reg_arr[0].x),lat2Y(reg_arr[0].y));
   k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
   LayerMapNal.Bitmap.FillRectS(k1.X-3,k1.Y-3,k1.X+3,k1.Y+3,SetAlpha(ClGreen32,255));
   k2:=point(Lon2X(reg_arr[length(reg_arr)-1].x),lat2Y(reg_arr[length(reg_arr)-1].y));
   k2:=Point(k2.X+(pr_x-mWd2),k2.y+(pr_y-mHd2));
   LayerMapNal.Bitmap.FillRectS(k2.X-3,k2.Y-3,k2.X+3,k2.Y+3,SetAlpha(ClRed32,255));
  end;
 if length(reg_arr)>2 then
  begin
   k1:=point(Lon2X(reg_arr[0].x),lat2Y(reg_arr[0].y));
   k2:=point(Lon2X(reg_arr[length(reg_arr)-1].x),Lat2Y(reg_arr[length(reg_arr)-1].y));
   k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
   k2:=Point(k2.X+(pr_x-mWd2),k2.y+(pr_y-mHd2));
   if (k1.X<k2.X+5)and(k1.X>k2.X-5)and(k1.y<k2.y+5)and(k1.y>k2.y-5)
    then begin
          reg_arr[length(reg_arr)-1]:=reg_arr[0];
          LayerMapNal.Bitmap.Clear(clBlack);
          Fsaveas.Show_(zoom_size,reg_arr);
         end;
  end;
 //LayerMapNal.BringToFront;
 Polygon.Free;
end;

procedure TFmain.drawLineGPS;
var i,j,speed,adp,SizeTrackd2:integer;
    k1,k2,k4:TPoint;
    ke,ks,k3:TExtendedPoint;
    TanOfAngle,Angle,D,R: Currency;
    dl: integer;
    Polygon: TPolygon32;
    s_speed_w,s_avgspeed_w,s_len_w,s_speed_h,s_avgspeed_h,s_len_h:integer;
    s_speed,s_avgspeed,s_len:string;
    polygon_line,Polygon_line2,Polygon_lineOutline: TPolygon32;
  //  Clr:TColor32;
begin
 Polygon := TPolygon32.Create;
 Polygon.Antialiased := true;
 polygon.AntialiasMode:=am4times;
 Polygon_line := TPolygon32.Create;
 Polygon_line.Antialiased := true;
 Polygon_line.AntialiasMode := am4times;
 polygon_line.Closed:=false;
// map.Bitmap.BeginUpdate;
 LayerMapGPS.Bitmap.Canvas.Pen.Style:=psSolid;
 LayerMapGPS.Bitmap.Canvas.Pen.Color:=clBlue;
 LayerMapGPS.Bitmap.Clear(clBlack);
 if length(GPS_arr_speed)>3 then
  begin
   GPSpar.speed:=GPS_arr_speed[length(GPS_arr_speed)-1];
   GPSpar.sspeed:=0;
   GPSpar.maxspeed:=GPS_arr_speed[1];
   for i:=3 to length(GPS_arr_speed)-1 do
    begin
     GPSpar.sspeed:=GPSpar.sspeed+GPS_arr_speed[i];
     if GPS_arr_speed[i]>GPSpar.maxspeed then GPSpar.maxspeed:=GPS_arr_speed[i];
    end;
   GPSpar.sspeed:=GPSpar.sspeed/(length(GPS_arr_speed)-3);
  end;

 with LayerMapGPS.Bitmap do
 if GPS_path then
 begin
{  for i:=0 to length(GPS_arr)-1 do
   begin
    k1:=point(Lon2X(GPS_arr[i].x)+(pr_x-mWd2),Lat2Y(GPS_arr[i].y)+(pr_y-mHd2));
    if (k1.x<32767)and(k1.x>-32767)and(k1.y<32767)and(k1.y>-32767) then
      polygon_line.Add(FixedPoint(k1));
    if i<length(GPS_arr)-1 then
     begin
      k2:=point(Lon2X(GPS_arr[i+1].x)+(pr_x-mWd2),Lat2Y(GPS_arr[i+1].y)+(pr_y-mHd2));
      if (k2.x-k1.x)>(k2.y-k1.y) then adp:=(k2.x-k1.x)div 32767+2
                                 else adp:=(k2.y-k1.y)div 32767+2;
      k3:=extPoint(((k2.X-k1.x)/adp),((k2.y-k1.y)/adp));
      if adp>2 then
       for j:=1 to adp-1 do
        begin
         k4:=Point(round(k1.x+k3.x*j),round(k1.Y+k3.y*j));
         if(k4.x<32767)and(k4.x>-32767)and(k4.y<32767)and(k4.y>-32767)then polygon_line.Add(FixedPoint(k4.x,k4.y));
        end;
     end;
    end;   }
  for i:=0 to length(GPS_arr)-2 do
   begin
    k1:=point(Lon2X(GPS_arr[i].x),lat2Y(GPS_arr[i].y));
    k2:=point(Lon2X(GPS_arr[i+1].x),Lat2Y(GPS_arr[i+1].y));
    //k4:=point(Lon2X(GPS_arr[i+2].x),Lat2Y(GPS_arr[i+2].y));
    k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
    k2:=Point(k2.X+(pr_x-mWd2),k2.y+(pr_y-mHd2));
    //k4:=Point(k4.X+(pr_x-mWd2),k4.y+(pr_y-mHd2));
    if GPS_arr_speed[i]>0 then speed:= round(255/(GPSpar.maxspeed/GPS_arr_speed[i]))
                          else speed:=0;
    //Clr:=Color32(speed,0,256-speed,0);
    if (k1.x<32767)and(k1.x>-32767)and(k1.y<32767)and(k1.y>-32767) then
      begin
       polygon_line.Add(FixedPoint(k1));
       polygon_line.Add(FixedPoint(k2));
       //polygon_line.Add(FixedPoint(k4));
      end;

    with Polygon_line.Outline do
     begin
      with Grow(Fixed(GPS_SizeTrack / 2), 0.5) do
       begin
        DrawFill(LayerMapGPS.Bitmap, SetAlpha(Color32(speed,0,256-speed,0),150));
        free;
       end;
      free;
     end;
    Polygon_line.Clear;
    {Polygon_lineOutline:=Polygon_line.Outline;
    Polygon_line2:=Polygon_lineOutline.Grow(Fixed(GPS_SizeTrack / 2), 0.5);
    FreeAndNil(Polygon_lineOutline);
    Polygon_line2.DrawFill(LayerMapGPS.Bitmap, SetAlpha(Color32(speed,0,256-speed,0),150));
    FreeAndNil(Polygon_line2);}

    {with Polygon_line.Outline.Grow(Fixed(GPS_SizeTrack / 2), 0.5) do
     begin
      //FillMode:=pfWinding;
      //DrawFill(LayerMapGPS.Bitmap, SetAlpha(Color32(speed,0,256-speed,0),150));
      Free;
     end;}
  //  LineAS(k1.X,k1.Y,k2.X,k2.Y,SetAlpha(Clr,180));
   end;
 end;

 if length(GPS_arr)>1 then
 begin
  ke:=extpoint(Lon2Xf(GPS_arr[length(GPS_arr)-1].x),lat2Yf(GPS_arr[length(GPS_arr)-1].y));
  ke:=extPoint(ke.X+(pr_x-mWd2),ke.y+(pr_y-mHd2));
  ks:=extpoint(Lon2Xf(GPS_arr[length(GPS_arr)-2].x),Lat2Yf(GPS_arr[length(GPS_arr)-2].y));
  ks:=extPoint(ks.X+(pr_x-mWd2),ks.y+(pr_y-mHd2));
  dl:=GPS_SizeStr;
  R:=sqrt(sqr(ks.X-ke.X)+sqr(ks.Y-ke.Y))/2-(dl div 2);
  if ks.x=ke.x then TanOfAngle:=MaxExtended/100 * Sign(ks.Y-ke.Y)
               else TanOfAngle:=(ks.Y-ke.Y)/(ks.X-ke.X);
  D:=Sqrt(Sqr(ks.X-ke.X)+Sqr(ks.Y-ke.Y));
  ke.x:=ke.X+(ke.X-ks.X);
  ke.y:=ke.y+(ke.y-ks.y);
  ke.x:=Round((R*ks.x+(D-R)*kE.X)/D);
  ke.y:=Round((R*ks.y+(D-R)*kE.Y)/D);
  Polygon.Add(FixedPoint(round(ke.X),round(ke.Y)));
  Angle:=ArcTan(TanOfAngle)+0.28;
  if ks.X < ke.X then Angle:=Angle+Pi;
  Polygon.Add(FixedPoint(round(ke.x) + Round(dl*Cos(Angle)),round(ke.Y) + Round(dl*Sin(Angle))));
  Angle:=ArcTan(TanOfAngle)-0.28;
  if ks.X < ke.X then Angle:=Angle+Pi;
  Polygon.Add(FixedPoint(round(ke.X) + Round(dl*Cos(Angle)),round(ke.Y) + Round(dl*Sin(Angle))));
  Polygon.DrawFill(LayerMapGPS.Bitmap, SetAlpha(Color32(GPS_colorStr), 150));
 end;

 if length(GPS_arr)>0 then
  begin
   ke:=extpoint(Lon2Xf(GPS_arr[length(GPS_arr)-1].x),lat2Yf(GPS_arr[length(GPS_arr)-1].y));
   ke:=extPoint(ke.X+(pr_x-mWd2),ke.y+(pr_y-mHd2));
   SizeTrackd2:=GPS_SizeStr div 6;
   LayerMapGPS.Bitmap.FillRectS(round(ke.x-SizeTrackd2),round(ke.y-SizeTrackd2),round(ke.x+SizeTrackd2),round(ke.y+SizeTrackd2),SetAlpha(clRed32, 200));
  end;

 s_speed:=RoundEx(GPSpar.speed,2)+' ('+RoundEx(GPSpar.sspeed,1)+') '+SAS_UNITS_kmperh;
 LayerStatBar.Bitmap.FillRectS(10,-40,10,-20,SetAlpha(clWhite32, 140));
 LayerMapGPS.Bitmap.FillRectS((pr_x-mWd2)+5,(pr_y-mHd2)+5,(pr_x-mWd2)+round(LayerMapGPS.Bitmap.TextWidthW(s_speed)*1.3),(pr_y-mHd2)+52,SetAlpha(clWhite32, 140));
 LayerMapGPS.Bitmap.Font.Size:=8;
 LayerMapGPS.Bitmap.RenderText((pr_x-mWd2)+10,(pr_y-mHd2)+10,SAS_STR_Speed+':', 0, clBlack32);
 LayerMapGPS.Bitmap.Font.Size:=16;
 LayerMapGPS.Bitmap.RenderText((pr_x-mWd2)+10,(pr_y-mHd2)+24,s_speed, 4, clBlack32);
 s_len:=R2ShortStr(GPSpar.len,4,' '+SAS_UNITS_km+' ',' '+SAS_UNITS_m);
 LayerMapGPS.Bitmap.FillRectS((pr_x-mWd2)+5,(pr_y-mHd2)+59,(pr_x-mWd2)+round(LayerMapGPS.Bitmap.TextWidthW(s_len)*1.3)+5,(pr_y-mHd2)+106,SetAlpha(clWhite32, 140));
 LayerMapGPS.Bitmap.Font.Size:=8;
 LayerMapGPS.Bitmap.RenderText((pr_x-mWd2)+10,(pr_y-mHd2)+64,SAS_STR_LenPath+':', 0, clBlack32);
 LayerMapGPS.Bitmap.Font.Size:=16;
 LayerMapGPS.Bitmap.RenderText((pr_x-mWd2)+10,(pr_y-mHd2)+78,s_len, 4, clBlack32);

 LayerMapGPS.BringToFront;
 FreeAndNil(Polygon);
 FreeAndNil(Polygon_line);
 toSh;
end;

procedure TFmain.drawPath(pathll:array of TExtendedPoint;new:boolean;color1,color2:TColor32;linew:integer;poly:boolean);
var i,adp,j:integer;
    k1,k2,k4:TPoint;
    k3:TextendedPoint;
    polygon: TPolygon32;
begin
 try
 if new then
  begin
   LayerMapNal.Bitmap.Clear(clBlack);
   TBEditPath.Visible:=(new)and(length(pathll)>1);
  end;
 polygon:=TPolygon32.Create;
 polygon.Antialiased:=true;
 polygon.AntialiasMode:=am4times;
 polygon.Closed:=poly;
 map.Bitmap.BeginUpdate;
 if length(pathll)>0 then
 with LayerMap.Bitmap do
 begin
  for i:=0 to length(pathll)-1 do
   begin
    k1:=point(Lon2X(pathll[i].x)+(pr_x-mWd2),Lat2Y(pathll[i].y)+(pr_y-mHd2));
    if (k1.x<32767)and(k1.x>-32767)and(k1.y<32767)and(k1.y>-32767) then
      polygon.Add(FixedPoint(k1));
    if i<length(pathll)-1 then
     begin
      k2:=point(Lon2X(pathll[i+1].x)+(pr_x-mWd2),Lat2Y(pathll[i+1].y)+(pr_y-mHd2));
      if (k2.x-k1.x)>(k2.y-k1.y) then adp:=(k2.x-k1.x)div 32767+2
                                 else adp:=(k2.y-k1.y)div 32767+2;
      k3:=extPoint(((k2.X-k1.x)/adp),((k2.y-k1.y)/adp));
      if adp>2 then
       for j:=1 to adp-1 do
        begin
         k4:=Point(round(k1.x+k3.x*j),round(k1.Y+k3.y*j));
         if(k4.x<32767)and(k4.x>-32767)and(k4.y<32767)and(k4.y>-32767)then polygon.Add(FixedPoint(k4.x,k4.y));
        end;
     end;
   end;
  if poly then if new then Polygon.DrawFill(LayerMapNal.Bitmap, color2)
                      else Polygon.DrawFill(LayerMapMarks.Bitmap, color2);
  with Polygon.Outline do
   begin
    with Grow(Fixed(linew / 2), 0.5) do
     begin
      FillMode := pfWinding;
      if new then DrawFill(LayerMapNal.Bitmap, color1)
             else DrawFill(LayerMapMarks.Bitmap, color1);
      free;
     end;
    free;
   end;
//  with Polygon.Outline.Grow(Fixed(linew / 2), 0.5) do
//   begin
//    FillMode:=pfWinding;
//    if new then DrawFill(LayerMapNal.Bitmap, color1)
//           else DrawFill(LayerMap.Bitmap, color1);
//   end;
  polygon.Free;
  if new then
  for i:=0 to length(pathll)-1 do
   begin
    k1:=point(Lon2X(pathll[i].x),Lat2Y(pathll[i].y));
    k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
    LayerMapNal.Bitmap.FillRectS(k1.X-4,k1.y-4,k1.X+4,k1.y+4,SetAlpha(clYellow32,150));
   end;
 end;
 if new then
  begin
   k1:=point(Lon2X(pathll[0].x),lat2Y(pathll[0].y));
   k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
   LayerMapNal.Bitmap.FillRectS(k1.x-4,k1.y-4,k1.X+4,k1.y+4,SetAlpha(ClGreen32,255));
   k1:=point(Lon2X(pathll[lastpoint].x),lat2Y(pathll[lastpoint].y));
   k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
   LayerMapNal.Bitmap.FillRectS(k1.x-4,k1.y-4,k1.X+4,k1.y+4,SetAlpha(ClRed32,255));
  end;
{ if (new)and(length(pathll)>1) then
  begin
   if lastpoint=-1 then k1:=point(Lon2X(pathll[length(pathll)-1].x),Lat2Y(pathll[length(pathll)-1].y))
                   else k1:=point(Lon2X(pathll[lastpoint].x),Lat2Y(pathll[lastpoint].y));
   k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
   LayerMapNal.Bitmap.FillRectS(k1.x+5,k1.y,k1.X+32,k1.y+15,SetAlpha(ClWhite32,120));
//   btn_ok.Left:=k1.X+7+round(LayerMap.Location.Left);
//   btn_ok.top:=k1.Y+2+round(LayerMap.Location.Top);
//   btn_delpath.Left:=k1.X+20+round(LayerMap.Location.Left);
//   btn_delpath.top:=k1.Y+2+round(LayerMap.Location.Top);
  end;}
 map.Bitmap.endUpdate;
 map.Bitmap.Changed;
 except
 end;
end;


procedure TFmain.drawLineCalc;
var i,j,textW,l,adp:integer;
    k1,k2,k4:TPoint;
    k3:TExtendedPoint;
    len:real;
    text:string;
    polygon: TPolygon32;
begin
 try
 polygon:=TPolygon32.Create;
 polygon.Antialiased:=true;
 polygon.AntialiasMode:=am4times;
 polygon.Closed:=false;
 LayerMapNal.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
 map.Bitmap.BeginUpdate;
 TBEditPath.Visible:=(length(length_arr)>1);
 LayerMapNal.Bitmap.Font.Name:='Tahoma';
 LayerMapNal.Bitmap.Clear(clBlack);
 if length(length_arr)>0 then
 with LayerMapNal.Bitmap do
 begin
  for i:=0 to length(length_arr)-1 do
   begin
    k1:=point(Lon2X(length_arr[i].x)+(pr_x-mWd2),Lat2Y(length_arr[i].y)+(pr_y-mHd2));
    if (k1.x<32767)and(k1.x>-32767)and(k1.y<32767)and(k1.y>-32767) then
      polygon.Add(FixedPoint(k1));
    if i<length(length_arr)-1 then
     begin
      k2:=point(Lon2X(length_arr[i+1].x)+(pr_x-mWd2),Lat2Y(length_arr[i+1].y)+(pr_y-mHd2));
      if (k2.x-k1.x)>(k2.y-k1.y) then adp:=(k2.x-k1.x)div 32767+2
                                 else adp:=(k2.y-k1.y)div 32767+2;
      k3:=extPoint(((k2.X-k1.x)/adp),((k2.y-k1.y)/adp));
      if adp>2 then
       for j:=1 to adp-1 do
        begin
         k4:=Point(round(k1.x+k3.x*j),round(k1.Y+k3.y*j));
         if(k4.x<32767)and(k4.x>-32767)and(k4.y<32767)and(k4.y>-32767)then polygon.Add(FixedPoint(k4.x,k4.y));
        end;
     end;
   end;
  with Polygon.Outline do
   begin
    with Grow(Fixed(2.5 / 2), 0.5) do
     begin
      FillMode := pfWinding;
      DrawFill(LayerMapNal.Bitmap, SetAlpha(ClRed32, 150));
      free;
     end;
    free;
   end;
//  with Polygon.Outline.Grow(Fixed(2.5 / 2), 0.5) do
//   begin
//    FillMode:=pfWinding;
//    DrawFill(LayerMapNal.Bitmap, SetAlpha(ClRed32, 150));
//   end;
  polygon.Free;
  for i:=0 to length(length_arr)-2 do
   begin
    k1:=point(Lon2X(length_arr[i].x),lat2Y(length_arr[i].y));
    k2:=point(Lon2X(length_arr[i+1].x),Lat2Y(length_arr[i+1].y));
    k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
    k2:=Point(k2.X+(pr_x-mWd2),k2.y+(pr_y-mHd2));
    if not((k2.x>0)and(k2.y>0))and((k2.x<xhgpx)and(k2.y<yhgpx))then continue;
    FrameRectS(k2.x-3,k2.y-3,k2.X+3,k2.Y+3,SetAlpha(ClRed32,150));
    FillRectS(k1.x-2,k1.y-2,k1.X+2,k1.y+2,SetAlpha(ClWhite32,150));
    if i=length(length_arr)-2 then
     begin
      len:=0;
      for j:=0 to i do len:=len+find_length(length_arr[j].y,length_arr[j+1].y,length_arr[j].X,length_arr[j+1].x);
      text:=SAS_STR_Whole+': '+R2ShortStr(len,2,SAS_UNITS_km+'. ',SAS_UNITS_m+'.');
      Font.Size:=9;
      textW:=TextWidth(text)+11;
      FillRectS(k2.x+12,k2.y,k2.X+textW,k2.y+15,SetAlpha(ClWhite32,110));
      RenderText(k2.X+15,k2.y,text,3,clBlack32);
     end
    else
     if LenShow then
      begin
       text:=R2ShortStr(find_length(length_arr[i].y,length_arr[i+1].y,length_arr[i].x,length_arr[i+1].x),2,SAS_UNITS_km+'. ',SAS_UNITS_m+'.');
       LayerMapNal.Bitmap.Font.Size:=7;
       textW:=TextWidth(text)+11;
       FillRectS(k2.x+5,k2.y+5,k2.X+textW,k2.y+16,SetAlpha(ClWhite32,110));
       RenderText(k2.X+8,k2.y+5,text,0,clBlack32);
      end;
   end;
  k1:=point(Lon2X(length_arr[0].x),lat2Y(length_arr[0].y));
  k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
  FillRectS(k1.x-3,k1.y-3,k1.X+3,k1.y+3,SetAlpha(ClGreen32,255));
  k1:=point(Lon2X(length_arr[length(length_arr)-1].x),lat2Y(length_arr[length(length_arr)-1].y));
  k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
  FillRectS(k1.x-3,k1.y-3,k1.X+3,k1.y+3,SetAlpha(ClRed32,255));
 end;
 map.Bitmap.endUpdate;
 map.Bitmap.Changed;
 except
 end;
end;

procedure TFmain.draw_point;
var lon_l,lon_r,lat_t,lat_d:real;
    i:integer;
    xy:Tpoint;
    btm:TBitmap32;
    TestArrLenP1,TestArrLenP2:TPoint;
    arrLL:PArrLL;
    buf_line_arr:array of TExtendedPoint;
    ms:TMemoryStream;
    indexmi:integer;
    imw,texth:integer;
begin
 if (show_point=3) then
  begin
   LayerMapMarks.Visible:=false;
   exit;
  end;
 btm:=TBitmap32.Create;
 btm.DrawMode:=dmBlend;
 btm.Resampler:=TKernelResampler.Create;
 TKernelResampler(btm.Resampler).Kernel:=TCubicKernel.Create;
 lon_l:=X2Lon(-(pr_x-mWd2));
 lon_r:=X2Lon(pr_x+mWd2);
 lat_t:=Y2Lat(-(pr_y-mHd2));
 lat_d:=Y2Lat(pr_y+mHd2);
 CDSmarks.Filtered:=false;
 CDSmarks.Filter:='(not( LonR<'+floattostr(lon_l)+' or LonL>'+floattostr(lon_R)+
                  ' or LatB>'+floattostr(lat_t)+' or LatT<'+floattostr(lat_d)+'))';
 if show_point=2 then
 begin
 CDSKategory.Filter:='visible = false or ( AfterScale > '+inttostr(zoom_size)+' or BeforeScale < '+inttostr(zoom_size)+' )';
 CDSKategory.Filtered:=true;
 CDSmarks.Filter:=CDSmarks.Filter+' and visible=1';
 CDSKategory.First;
 if not(CDSKategory.Eof) then
  begin
   CDSmarks.Filter:=CDSmarks.Filter+' and (';
   while not(CDSKategory.Eof) do
    begin
     CDSmarks.Filter:=CDSmarks.Filter+'categoryid<>'+CDSKategory.fieldbyname('id').AsString;
     CDSKategory.Next;
     if not(CDSKategory.Eof) then CDSmarks.Filter:=CDSmarks.Filter+' and ';
    end;
   CDSmarks.Filter:=CDSmarks.Filter+')';
  end;
 CDSKategory.Filtered:=false;
 end;
 CDSmarks.Filtered:=true;
 CDSmarks.First;
 if CDSmarks.Eof then LayerMapMarks.Visible:=false
                 else begin
                       LayerMapMarks.Bitmap.Clear(clBlack);
                       LayerMapMarks.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
                       LayerMapMarks.Visible:=true;
                      end;
 While not(CDSmarks.Eof) do
  begin
     texth:=CDSmarkscategoryid.AsInteger;
     ms:=TMemoryStream.Create;
     TBlobField(CDSmarksLonLatArr).SaveToStream(ms);
     ms.Position:=0;
     GetMem(arrLL,ms.size);
     ms.ReadBuffer(arrLL^,ms.size);
     if (ms.size)>24 then
      begin
       {if (arrLL^[0].x=arrLL^[(ms.size div 24)-1].x)and(arrLL^[0].y=arrLL^[(ms.size div 24)-1].y) then
        begin
         if (CDSmarks.FieldByName('LonL').AsFloat<lon_l)and
            (CDSmarks.FieldByName('LonR').AsFloat>lon_r)and
            (CDSmarks.FieldByName('LatT').AsFloat>lat_t)and
            (CDSmarks.FieldByName('LatB').AsFloat<lat_d) then
             begin
              ms.free;
              FreeMem(arrLL);
              CDSmarks.Next;
              continue;
             end;
        end;}
       TestArrLenP1:=GLonLat2Pos(ExtPoint(CDSmarksLonL.AsFloat,CDSmarksLatT.AsFloat),zoom_size,sat_map_both);
       TestArrLenP2:=GLonLat2Pos(ExtPoint(CDSmarksLonR.AsFloat,CDSmarksLatB.AsFloat),zoom_size,sat_map_both);
       if (abs(TestArrLenP1.X-TestArrLenP2.X)>CDSmarksScale1.AsInteger+2)or(abs(TestArrLenP1.Y-TestArrLenP2.Y)>CDSmarksScale1.AsInteger+2) then
        begin
         SetLength(buf_line_arr,(ms.size div 24));
         for i:=0 to (ms.size div 24)-1 do buf_line_arr[i]:=arrLL^[i];
         drawPath(buf_line_arr,false,TColor32(CDSmarksColor1.AsInteger),TColor32(CDSmarksColor2.AsInteger),CDSmarksScale1.asInteger,
                 (buf_line_arr[0].x=buf_line_arr[length(buf_line_arr)-1].x)and(buf_line_arr[0].y=buf_line_arr[length(buf_line_arr)-1].y));
         SetLength(buf_line_arr,0);
        end;
      end;
     if (ms.size)=24 then
      begin
       xy:=Point(Lon2X(arrLL^[0].x)+(pr_x-mWd2),lat2Y(arrLL^[0].y)+(pr_y-mHd2));
       imw:=CDSmarks.FieldByName('Scale2').AsInteger;
       indexmi:=marksicons.IndexOf(CDSmarks.FieldByName('picname').AsString);
       if(indexmi=-1)and(marksicons.Count>0) then indexmi:=0;
       if(indexmi>-1)then begin
                           PNGintoBitmap32(btm,TPNGObject(marksicons.Objects[indexmi]));
                           LayerMapMarks.Bitmap.Draw(bounds(xy.x-(imw div 2),xy.y-imw,imw,imw),bounds(0,0,btm.Width,btm.Height),btm);
                          end;
       if CDSmarks.FieldByName('Scale1').AsInteger>0 then
        begin
         LayerMapMarks.Bitmap.Font.Size:=CDSmarksScale1.AsInteger;
         texth:=LayerMap.Bitmap.TextHeight(CDSmarksname.asString) div 2;
         LayerMapMarks.Bitmap.RenderText(xy.x+(imw div 2)+2,xy.y-(imw div 2)-texth+1,CDSmarksname.AsString,1,TColor32(CDSmarksColor2.AsInteger));
         LayerMapMarks.Bitmap.RenderText(xy.x+(imw div 2)+1,xy.y-(imw div 2)-texth,CDSmarksname.AsString,1,TColor32(CDSmarksColor1.AsInteger));
        end;
      end;
     ms.free;
     FreeMem(arrLL);
     CDSmarks.Next;
  end;
{ CDSmarks.First;
 While not(CDSmarks.Eof) do
  begin
     ms:=TMemoryStream.Create;
     TBlobField(CDSmarksLonLatArr).SaveToStream(ms);
     ms.Position:=0;
     GetMem(arrLL,ms.size);
     ms.ReadBuffer(arrLL^,ms.size);
     if (ms.size)=24 then
      begin
       xy:=Point(Lon2X(arrLL^[0].x)+(pr_x-mWd2),lat2Y(arrLL^[0].y)+(pr_y-mHd2));
       imw:=CDSmarksScale2.AsInteger;
       indexmi:=marksicons.IndexOf(CDSmarkspicname.AsString);
       if(indexmi=-1)and(marksicons.Count>0) then indexmi:=0;
       if(indexmi>-1)then begin
                           PNGintoBitmap32(btm,TPNGObject(marksicons.Objects[indexmi]));
                           LayerMapMarks.Bitmap.Draw(bounds(xy.x-(imw div 2),xy.y-imw,imw,imw),bounds(0,0,btm.Width,btm.Height),btm);
                          end;
       if CDSmarksScale1.AsInteger>0 then
        begin
         LayerMapMarks.Bitmap.Font.Size:=CDSmarksScale1.AsInteger;
         texth:=LayerMap.Bitmap.TextHeight(CDSmarksname.asString) div 2;
         LayerMapMarks.Bitmap.RenderText(xy.x+(imw div 2)+2,xy.y-(imw div 2)-texth+1,CDSmarksname.AsString,1,TColor32(CDSmarksColor2.AsInteger));
         LayerMapMarks.Bitmap.RenderText(xy.x+(imw div 2)+1,xy.y-(imw div 2)-texth,CDSmarksname.AsString,1,TColor32(CDSmarksColor1.AsInteger));
        end;
      end;
    ms.free;
    FreeMem(arrLL);
    CDSmarks.Next;
  end;       }
 CDSmarks.Filtered:=false;
 btm.Free;
end;

function TFmain.timezone(lon,lat:real):TDateTime;
var prH,prM:integer;
    tz:real;
    st:TSystemTime;
begin
 tz:=GetTZ_(ExtPoint(Lon,Lat));
 GetSystemTime(st);
 prH:=trunc(tz);
 prM:=round(60*frac(TZ));
 result:=EncodeTime(abs(st.wHour+prH+24)mod 24,abs(st.wMinute+prM+60)mod 60,0,0);// StrToTime(inttostr(abs(st.wHour+prH+24)mod 24)+':'+inttostr(abs(st.wMinute+prM+60)mod 60));
end;

function TFmain.X2Lon(X:integer):extended;
begin
 result:=((POS.x-(map.Width/2-X))-zoom[zoom_size]/2)/(zoom[zoom_size]/360);
end;

function TFmain.Y2Lat(Y:integer):extended;
var Zum1,Zu,yy:extended;
begin
 case sat_map_both.projection of
  1: begin
      result:=((POS.y-(map.Height/2-Y))-zoom[zoom_size]/2) /-(zoom[zoom_size]/(2*Pi));
      result:=(2*arctan(exp(result))-Pi/2)*180/Pi;
     end;
  2: begin
      if ((POS.y-(map.Height/2-Y))>(zoom[zoom_size]/2))
       then yy:=(zoom[zoom_size] div 2) - ((POS.y-((map.Height div 2)-Y)) mod (zoom[zoom_size] div 2))
       else yy:=POS.y-(map.Height/2-Y);
  //    yy:=POS.y-((map.Height/2)-Y);
      result:=((yy)-zoom[zoom_size]/2) /-(zoom[zoom_size]/(2*Pi));
      result:=(2*arctan(exp(result))-Pi/2)*180/Pi;
      Zu:=result/(180/Pi);
      yy:=((yy)-zoom[zoom_size]/2);
      repeat
       Zum1:=Zu;
       Zu:=arcsin(1-((1+Sin(Zum1))*power(1-sat_map_both.exct*sin(Zum1),sat_map_both.exct))/(exp((2*yy)/-(zoom[zoom_size]/(2*Pi)))*power(1+sat_map_both.exct*sin(Zum1),sat_map_both.exct)));
      until ((abs(Zum1-Zu)<MerkElipsK) or (isNAN(Zu)));
      if not(isNAN(Zu)) then
       if ((POS.y-(map.Height/2-Y))>(zoom[zoom_size]/2))
         then result:=-zu*180/Pi
         else result:=zu*180/Pi;
     end;
  3: result:=-((POS.y-(map.Height/2-Y))-zoom[zoom_size]/2)/(zoom[zoom_size]/360);
 end; 
{ if result<-84.99 then result:=-84.99;
 if result>84.99 then result:=84.99;   }
end;

function TFmain.Lon2X(lon:real):integer;
begin
 result:=mWd2-(POS.x-round(zoom[zoom_size]/2+Lon*(zoom[zoom_size]/360)));
end;

function TFmain.Lon2Xf(lon:real):real;
begin
 result:=mWd2-(POS.x-(zoom[zoom_size]/2+Lon*(zoom[zoom_size]/360)));
end;

function TFmain.Lat2Y(lat:real):integer;
var z,c:real;
begin
 case sat_map_both.projection of
  1: begin
      z:=sin(Lat*deg);
      c:=(zoom[zoom_size]/(2*Pi));
      result:=mHd2-(POS.y-round(zoom[zoom_size]/2-0.5*ln((1+z)/(1-z))*c));
     end;
  2: begin
      z:=sin(Lat*deg);
      c:=(zoom[zoom_size]/(2*Pi));
      result:=mHd2-(POS.y-round(zoom[zoom_size]/2-c*(ArcTanh(z)-sat_map_both.exct*ArcTanh(sat_map_both.exct*z)) ) )
     end;
  3: result:=(mHd2-(POS.y-round(zoom[zoom_size]/2-Lat*(zoom[zoom_size]/360))));
 end;
end;

function TFmain.Lat2Yf(lat:real):real;
var z,c:real;
begin
 case sat_map_both.projection of
  1: begin
      z:=sin(Lat*deg);
      c:=(zoom[zoom_size]/(2*Pi));
      result:=mHd2-(POS.y-(zoom[zoom_size]/2-0.5*ln((1+z)/(1-z))*c));
     end;
  2: begin
      z:=sin(Lat*deg);
      c:=(zoom[zoom_size]/(2*Pi));
      result:=mHd2-(POS.y-(zoom[zoom_size]/2-c*(ArcTanh(z)-sat_map_both.exct*ArcTanh(sat_map_both.exct*z)) ) )
     end;
  3: result:=(mWd2-(POS.y-(zoom[zoom_size]/2+Lat*(zoom[zoom_size]/360))));
 end;
end;

function TFmain.find_length(StartLat,EndLat,StartLong,EndLong:double):real;
var fPhimean,fdLambda,fdPhi,fAlpha,fRho,fNu,fR,fz,fTemp,a,e2:Double;
const
//  e2: Double = 0.006739496742337; // Квадрат эксцентричности эллипсоида
  f: Double = 0.003352810664747; // Выравнивание эллипсоида
begin
  result:=0;
  e2:=sat_map_both.exct*sat_map_both.exct;
  a:=PMapType(sat_map_both).radiusa;
  if (StartLong=EndLong)and(StartLat=EndLat)then exit;
  fdLambda := (StartLong - EndLong) * D2R;
  fdPhi := (StartLat - EndLat) * D2R;
  fPhimean := ((StartLat + EndLat) / 2.0) * D2R;
  fTemp := 1 - e2 * (Power(Sin(fPhimean), 2));
  fRho := (a * (1 - e2)) / Power(fTemp, 1.5);
  fNu := a / (Sqrt(1 - e2 * (Sin(fPhimean) * Sin(fPhimean))));
  fz:=Sqrt(Power(Sin(fdPhi/2),2)+Cos(EndLat*D2R)*Cos(StartLat*D2R)*Power(Sin(fdLambda/2),2));
  fz := 2*ArcSin(fz);
  fAlpha := Cos(EndLat * D2R) * Sin(fdLambda) * 1 / Sin(fz);
  fAlpha := ArcSin(fAlpha);
  fR:=(fRho*fNu)/((fRho*Power(Sin(fAlpha),2))+(fNu*Power(Cos(fAlpha),2)));
  result := (fz * fR);
end;

procedure TFmain.topos(lat,lon:real;zoom_:byte;draw:boolean);
begin
 POS:=GLonLat2pos(ExtPoint(lon,lat),zoom_,sat_map_both);
 zoom_size:=zoom_;
 zooming(zoom_,false);
 TilesLoad.Clear;
 if draw then LayerMap.Bitmap.Draw(pr_x-7,pr_y-6,GOToSelIcon);
end;                     

function TFmain.R2ShortStr(r:real;z:byte;s1,s2:string):string;
var s:string;
begin
 case num_format of
 0: begin
     result:='';
     s:=floattostr(int(r));
     result:=copy(s,1,length(s)-3);
     if length(result)>0 then result:=result+s1;
     result:=result+inttostr(strtoint(copy(s,length(s)-2,3)));
     s:=floattostr(frac(r));
     result:=result+','+copy(s,3,2)+s2;
    end;
 1: if r<10000 then result:=floattostr(int(r))+','+copy(floattostr(frac(r)),3,2)+s2
               else result:=floattostr(int(r/1000))+','+copy(floattostr(frac(r/1000)),3,2)+s1+'/пикс.';
 end;
end;

procedure TFmain.paint_Line;
var rnum,len_p,textstrt,textwidth:integer;
    s,se:string;
    temp,num:real;
begin
 num:=106/((zoom[zoom_size]/(2*PI))/(sat_map_both.radiusa*cos(y2Lat(mHd2)*deg)));
 if num>10000 then begin
                    num:=num/1000;
                    se:=' '+SAS_UNITS_km+'.';
                   end
              else
 if num<10    then begin
                    num:=num*100;
                    se:=' '+SAS_UNITS_sm+'.';
                   end
              else se:=' '+SAS_UNITS_m+'.';
 rnum:=round(num);
 temp:=power(5,(length(inttostr(rnum))-1));
 if ((rnum/temp)<1.25) then rnum:=round(temp)
                      else if ((rnum/temp)>=3.75)then rnum:=5*round(temp)
                                                 else rnum:=round(2.5*temp);
 len_p:=round(106/(num/rnum));
 s:=inttostr(rnum)+se;
 textwidth:=LayerLineM.bitmap.TextWidth(s);
 while (len_p<textwidth+15)and(not(len_p=0)) do
  begin
   rnum:=rnum*2;
   len_p:=round(106/(num/rnum));
  end;
 s:=inttostr(rnum)+se;
 len_p:=round(106/(num/rnum));
 textwidth:=LayerLineM.bitmap.TextWidth(s);
 LayerLineM.Bitmap.Width:=len_p;
 With LayerLineM do Location:=floatrect(bounds(round(Location.left),round(Location.top),len_p,15));
 LayerLineM.Bitmap.Clear(SetAlpha(clWhite32,135));
 LayerLineM.bitmap.LineS(0,0,0,15,SetAlpha(clBlack32,256));
 LayerLineM.bitmap.LineS(LayerLineM.bitmap.Width-1,0,LayerLineM.bitmap.Width-1,15,SetAlpha(clBlack32,256));
 textstrt:=(len_p div 2)-(textwidth div 2);
 LayerLineM.bitmap.RenderText(textstrt,0,s, 2, clBlack32);
end;

function TFmain.lon2str(Alon:real):string;
var num:real;
begin
 if ALon>0 then if llStrType<3 then result:='E' else
           else if llStrType<3 then result:='W' else result:='-';
 Alon:=abs(Alon);
 case (llStrType mod 3) of
  0:begin
     result:=result+R2StrPoint(int(ALon))+'°'; num:=Frac(ALon)*60;
     result:=result+R2StrPoint(int(num))+''''+Copy(R2StrPoint(Frac(Num)*60),1,5)+'"';
    end;
  1:result:=result+R2StrPoint(int(ALon))+'°'+Copy(R2StrPoint(Frac(ALon)*60),1,7)+'''';
  2:result:=result+Copy(R2StrPoint(ALon),1,9)+'°';
 end;
end;

function TFmain.lat2str(Alat:real):string;
var num:real;
begin
 if Alat>0 then if llStrType<3 then result:='N' else
           else if llStrType<3 then result:='S' else result:='-';
 Alat:=abs(Alat);
 case (llStrType mod 3) of
  0:begin
     result:=result+R2StrPoint(int(Alat))+'°'; num:=Frac(Alat)*60;
     result:=result+R2StrPoint(int(num))+''''+Copy(R2StrPoint(Frac(Num)*60),1,5)+'"';
    end;
  1:result:=result+R2StrPoint(int(Alat))+'°'+Copy(R2StrPoint(Frac(Alat)*60),1,7)+'''';
  2:result:=result+Copy(R2StrPoint(Alat),1,9)+'°';
 end;
end;

function TFmain.toSh:string;
var ll:TextendedPoint;
    subs2:string;
    posnext:integer;
    TameTZ:TDateTime;
begin
 labZoom.caption:=' '+inttostr(zoom_size)+'x ';
 ll:=GPos2LonLat(mouseXY2Pos(Point(m_m.X,m_m.Y)),zoom_size,sat_map_both);
 if FirstLat then result:=lat2str(ll.y)+' '+lon2str(ll.x)
             else result:=lon2str(ll.x)+' '+lat2str(ll.y);
 LayerStatBar.Bitmap.Width:=map.Width;
 LayerStatBar.Bitmap.Clear(SetAlpha(clWhite32,160));
 LayerStatBar.Bitmap.Line(0,0,map.Width,0,SetAlpha(clBlack32,256));
 LayerStatBar.bitmap.RenderText(4,1,inttostr(zoom_size)+'x', 0, clBlack32);
 LayerStatBar.bitmap.RenderText(29,1,'| '+SAS_STR_coordinates+' '+result, 0, clBlack32);

 TameTZ:=timezone(ll.x,ll.y);
 subs2:=R2ShortStr(1/((zoom[zoom_size]/(2*PI))/(PMapType(sat_map_both).radiusa*cos(ll.y*deg))),4,' '+SAS_UNITS_km+'.',' '+SAS_UNITS_mperp);
 LayerStatBar.bitmap.RenderText(278,1,' | '+SAS_STR_Scale+' '+subs2, 0, clBlack32);
 posnext:=273+LayerStatBar.Bitmap.TextWidth(subs2)+70;
 LayerStatBar.bitmap.RenderText(posnext,1,' | '+SAS_STR_time+' '+ TimeToStr(TameTZ), 0, clBlack32);
 posnext:=posnext+LayerStatBar.Bitmap.TextWidth(SAS_STR_time+' '+TimeToStr(TameTZ))+10;
 subs2:=ffpath(X2absX(pos.x-(mWd2-m_m.x),zoom_size),pos.y-(mHd2-m_m.y),zoom_size,sat_map_both^,false);
 LayerStatBar.bitmap.RenderText(posnext,1,' | '+SAS_STR_load+' '+inttostr(All_Dwn_Tiles)+' ('+kb2KbMbGb(All_Dwn_Kb)+') | '+SAS_STR_file+' '+subs2, 0, clBlack32);

 LayerStatBar.BringToFront;
 //if LayerMinMap.Visible then LayerMinMap.BringToFront;
end;


function TFmain.loadpre(var spr:TBitmap32;x,y:integer;Azoom:byte;Amap:PMapType):boolean;
var i,c_x,c_y,c_d:integer;
    ss:string;
    bmp,bmp2:TBitmap32;
begin
 result:=false;
 if not(Nbackload.Checked) then
  begin
   spr.Clear(clSilver);
   exit;
  end;
 for i:=(Azoom-1) downto 1 do
  begin
   c_d:=round(power(2,Azoom-i));
   ss:=ffpath(x div c_d,y div c_d,i,AMap^,false);
   if TileExists(ss) then break;
  end;
 if not(tileExists(ss))or(c_d>=512) then
  begin
   spr.Clear(clSilver);
   exit;
  end;
 bmp:=TBitmap32.Create;
 if resampling=1
  then bmp.Resampler:=TLinearResampler.Create
  else begin
        bmp.Resampler:=TKernelResampler.Create;
        case resampling of
         0: TKernelResampler(bmp.Resampler).Kernel:=TBoxKernel.Create;
         2: TKernelResampler(bmp.Resampler).Kernel:=TCosineKernel.Create;
         3: TKernelResampler(bmp.Resampler).Kernel:=TSplineKernel.Create;
         4: TKernelResampler(bmp.Resampler).Kernel:=TMitchellKernel.Create;
         5: TKernelResampler(bmp.Resampler).Kernel:=TCubicKernel.Create;
         6: TKernelResampler(bmp.Resampler).Kernel:=THermiteKernel.Create;
         7: TKernelResampler(bmp.Resampler).Kernel:=TLanczosKernel.Create;
         8: TKernelResampler(bmp.Resampler).Kernel:=TGaussianKernel.Create;
         9: TKernelResampler(bmp.Resampler).Kernel:=TBlackmanKernel.Create;
         10:TKernelResampler(bmp.Resampler).Kernel:=THannKernel.Create;
         11:TKernelResampler(bmp.Resampler).Kernel:=THammingKernel.Create;
         12:TKernelResampler(bmp.Resampler).Kernel:=TSinshKernel.Create;
        end;
       end;
 if not(LoadTilefromCache(bmp,ss))then
  begin
   bmp.SetSize(256,256);
   bmp.Clear(Color32(clSilver) xor $00000000);
  end;
 c_x:=((x-(x mod 256))div c_d)mod 256;
 c_y:=((y-(y mod 256))div c_d)mod 256;
 bmp2:=TBitmap32.Create;
 bmp2.SetSize(256,256);
 try
 bmp2.Draw(bounds(-c_x*c_d,-c_y*c_d,256*c_d,256*c_d),bounds(0,0,256,256),bmp);
 spr.Assign(bmp2);
 except
 end;
 bmp.Free;
 bmp2.Free;
 result:=true;
end;

procedure TFmain.generate_lonlan(dx,dy:integer);
var xyDraw:TPoint;
    xx,yy,xx1,yy1,fxy,dxy:extended;
    i,j:integer;
    xy1,xy2,xy1res:TExtendedPoint;
    src:TRect;
    d2562,x2,x1,y1,zl:integer;
begin
 xy1:=GPos2LonLat(Point((pos.x-pr_x+dx),(pos.y-pr_y)+dy),zoom_size,sat_map_both);
 xy2:=GPos2LonLat(Point((pos.x+pr_x+dx),(pos.y+pr_y)+dy),zoom_size,sat_map_both);
 yy:=xy1.y-xy2.y;
 fxy:=yy/10;
 i:=0;
 While (yy)<100 do
  begin
   yy:=yy * 10;
   inc(i);
  end;
 if i=0 then dxy:=1
        else dxy:=power(10,i);
// fxy:=round(fxy*(dxy*100))/(dxy*100);
// xy1.Y:=round(xy1.Y * (dxy))/(dxy);
 for i:=1 to 10 do
  begin
   xy1.Y:=xy1.Y-fxy;
   xy1res:=ExtPoint(xy1.X,ROUND(xy1.y*dxy)/dxy);
   xyDraw:=GLonLat2Pos(xy1res,zoom_size,sat_map_both);
   xyDraw.y:=pr_y-(pos.y-xyDraw.y);
   LayerMap.Bitmap.Textout(pr_x,xyDraw.Y-10,floattostr(xy1res.y));
   LayerMap.bitmap.LineAS(0,xyDraw.Y,pr_x*2,xyDraw.Y,SetAlpha(Color32(BorderColor),BorderAlpha));
  end;
end;






procedure TFmain.DrawGenShBorders;
var i,j:integer;
    LonLatLT,LonLatRD:TExtendedPoint;
    bufLonLT:Extended;
    PosLT,pos10k,pos1k,pos05k,pos025k,posCurr:TPoint;
    zLonR,zLatR:extended;
    x2,x1,y1,y2,X1b,twidth,theight:integer;
    ListName:WideString;
begin
 if GShScale=0 then exit;
 case GShScale of
  1000000: begin zLonR:=6; zLatR:=4; end;
   500000: begin zLonR:=3; zLatR:=2; end;
   200000: begin zLonR:=1; zLatR:=0.66666666666666666666666666666667; end;
   100000: begin zLonR:=0.5; zLatR:=0.33333333333333333333333333333333; end;
    50000: begin zLonR:=0.25; zLatR:=0.1666666666666666666666666666665; end;
    25000: begin zLonR:=0.125; zLatR:=0.08333333333333333333333333333325; end;
    10000: begin zLonR:=0.0625; zLatR:=0.041666666666666666666666666666625; end;
 end;

 LonLatRD:=GPos2LonLat(Point(pos.x+pr_x,pos.y+pr_y),zoom_size,sat_map_both);
 LonLatRD:=ExtPoint(LonLatRD.x+zLonR,LonLatRD.y-zLatR);
 if LonLatRD.y<-90 then LonLatRD.y:=-90;
 if LonLatRD.x>180 then LonLatRd.x:=180;
 LonLatLT:=GPos2LonLat(Point(pos.x-pr_x,pos.y-pr_y),zoom_size,sat_map_both);
 LonLatLT:=ExtPoint(LonLatLT.X-zLonR,LonLatLT.Y+zLatR);
 if LonLatLT.y>90 then LonLatLT.y:=90;
 if LonLatLT.x<-180 then LonLatLT.x:=-180;
 LonLatLT.X:=LonLatLT.X-(round(LonLatLT.X*GSHprec) mod round(zLonR*GSHprec))/GSHprec;
 LonLatLT.Y:=LonLatLT.Y-(round(LonLatLT.Y*GSHprec) mod round(zLatR*GSHprec))/GSHprec;

 PosLT:=GLonLat2Pos(LonLatLT,zoom_size,sat_map_both);
 if GLonLat2Pos(ExtPoint(LonLatLT.x+zLonR,LonLatLT.y+zLatR),zoom_size,sat_map_both).x-PosLT.x<4 then exit;
 bufLonLT:=LonLatLT.X;

 Y1:=pr_Y-(Pos.Y-PosLT.Y);
 while (LonLatLT.Y>LonLatRD.y) do
  begin
   LonLatLT.Y:=LonLatLT.Y-zLatR;
   LonLatLT.X:=bufLonLT;
   PosLT:=GLonLat2Pos(LonLatLT,zoom_size,sat_map_both);
   Y2:=pr_Y-(Pos.Y-PosLT.Y);
   X1:=pr_x-(Pos.X-PosLT.X);
   X1b:=X1;
   while (LonLatLT.X+zLonR/2<LonLatRD.x) do
    begin
     LonLatLT.X:=LonLatLT.X+zLonR;
     PosLT:=GLonLat2Pos(LonLatLT,zoom_size,sat_map_both);
     X2:=pr_x-(Pos.X-PosLT.X);
     LayerMap.bitmap.LineAS(x1,y1,x1,y2,SetAlpha(Color32(BorderColor),BorderAlpha));
     if ((x2-x1>30)and(y2-y1>7))and(BorderText) then
      begin
       ListName:=LonLat2GShListName(ExtPoint(LonLatLT.X-zLonR/2,LonLatLT.Y+zLatR/2),GShScale,GSHprec);
 {      pos10k:=Point(round((LonLatLT.X+180)*GSHprec-zLon/2)div (6*GSHprec),round(abs(LonLatLT.Y*GSHprec+zLat/2))div (4*GSHprec));
       if LonLatLT.Y+zLatR/2<0 then ListName:='x'+chr(65+pos10k.y)+'-'+inttostr(pos10k.x+1)
                               else ListName:=chr(65+pos10k.y)+'-'+inttostr(pos10k.x+1);
       case GShScale of
        500000: begin
                 posCurr:=Point(round((LonLatLT.X+180-zLonR/2)*GSHprec)div zLon,round(abs(LonLatLT.Y+zLatR/2)*GSHprec)div zLat);
                 if LonLatLT.Y+zLatR/2>0 then ListName:=ListName+'-'+chr(192+(posCurr.x mod 2)+(1-(posCurr.y mod 2))*2)
                                         else ListName:=ListName+'-'+chr(192+(posCurr.x mod 2)+(posCurr.y mod 2)*2);
                end;
        200000: begin
                 posCurr:=Point(round((LonLatLT.X+180-zLonR/2)*GSHprec)div zLon,round(abs(LonLatLT.Y+zLatR/2)*GSHprec)div zLat);
                 if LonLatLT.Y+zLatR/2>0 then ListName:=ListName+'-'+ArabicToRoman(1+(posCurr.x mod 6)+(5-(posCurr.y mod 6))*6)
                                         else ListName:=ListName+'-'+ArabicToRoman(1+(posCurr.x mod 6)+(posCurr.y mod 6)*6);
                end;
        else
         if GShScale<=100000
          then begin
                posCurr:=Point(round((LonLatLT.X+180-zLonR/2)*GSHprec)div round((6/12)*GSHprec),round(abs(LonLatLT.Y+zLatR/2)*GSHprec)div round((4/12)*GSHprec));
                if LonLatLT.Y+zLatR/2>0 then ListName:=ListName+'-'+inttostr(1+(posCurr.x mod 12)+(11-(posCurr.y mod 12))*12)
                                        else ListName:=ListName+'-'+inttostr(1+(posCurr.x mod 12)+(posCurr.y mod 12)*12);
                if GShScale<=50000
                 then begin
                       posCurr:=Point(round((LonLatLT.X+180-zLonR/2)*GSHprec)div round((6/24)*GSHprec),round(abs(LonLatLT.Y+zLatR/2)*GSHprec)div round((4/24)*GSHprec));
                       if LonLatLT.Y+zLatR/2>0 then ListName:=ListName+'-'+chr(192+((posCurr.x mod 24) mod 2)+((23-(posCurr.y mod 24))mod 2)*2)
                                                  else ListName:=ListName+'-'+chr(192+((posCurr.x mod 24) mod 2)+((posCurr.y mod 24) mod 2)*2);
                       if GShScale<=25000
                        then begin
                              posCurr:=Point(round((LonLatLT.X+180-zLonR/2)*GSHprec)div round((6/48)*GSHprec),round(abs(LonLatLT.Y+zLatR/2)*GSHprec)div round((4/48)*GSHprec));
                              if LonLatLT.Y+zLatR/2>0 then ListName:=ListName+'-'+chr(224+((posCurr.x mod 48) mod 2)+((47-(posCurr.y mod 48))mod 2)*2)
                                                          else ListName:=ListName+'-'+chr(224+((posCurr.x mod 48) mod 2)+((posCurr.y mod 48) mod 2)*2);
                              if GShScale<=10000
                               then begin
                                     posCurr:=Point(round((LonLatLT.X+180-zLonR/2)*GSHprec)div round((6/96)*GSHprec),round(abs(LonLatLT.Y+zLatR/2)*GSHprec)div round((4/96)*GSHprec));
                                     if LonLatLT.Y+zLatR/2>0 then ListName:=ListName+'-'+inttostr(1+((posCurr.x mod 96) mod 2)+((95-(posCurr.y mod 96))mod 2)*2)
                                                             else ListName:=ListName+'-'+inttostr(1+((posCurr.x mod 96) mod 2)+((posCurr.y mod 96) mod 2)*2);
                                    end;
                             end;
                      end;
               end;
        end;  }
       twidth:=LayerMap.bitmap.TextWidth(ListName);
       theight:=LayerMap.bitmap.TextHeight(ListName);
       if (twidth+4<x2-x1)and(theight+4<y2-y1) then
        LayerMap.bitmap.RenderTextW(x1+(x2-x1)div 2-(twidth div 2),y1+(y2-y1)div 2-(theight div 2),ListName,0,SetAlpha(Color32(BorderColor),BorderAlpha));
      end;
     X1:=X2;
    end;
   LayerMap.bitmap.LineAS(x1b,y1,x2,y1,SetAlpha(Color32(BorderColor),BorderAlpha));
   LonLatLT.X:=LonLatLT.X+zLonR;
   PosLT:=GLonLat2Pos(LonLatLT,zoom_size,sat_map_both);
   LayerMap.bitmap.LineAS(x1,y1,x1,y2,SetAlpha(Color32(BorderColor),BorderAlpha));
   y1:=y2;
  end;

{ while (PosLT.Y<pos.y+pr_y)and(LonLatLT.Y>-90)and(LonLatLT.Y<90)
        and((PosLT.Y<zoom[zoom_size])) do
  begin
  //if ((PosLT.Y<zoom[zoom_size])and(PosLT.Y>0)) then
   begin
    Y1:=pr_Y-(Pos.Y-PosLT.Y);
    if (pos.X-pr_x)<0 then x1:=abs(pos.X-pr_x) else x1:=0;
    if (pos.X+pr_x)>zoom[zoom_size] then x2:=xhgpx-((pos.X+pr_x)-zoom[zoom_size]) else x2:=xhgpx;
    LayerMap.bitmap.LineAS(x1,y1,x2,y1,SetAlpha(Color32(BorderColor),BorderAlpha));
   end;
   LonLatLT.Y:=LonLatLT.Y-zLat/10000;
   PosLT:=GLonLat2Pos(LonLatLT,zoom_size,sat_map_both);
  end;
 while (PosLT.X<pos.x+pr_x)and(PosLT.X<zoom[zoom_size]) do
  begin
 // if ((PosLT.X<zoom[zoom_size])and(PosLT.X>0)) then
   begin
    X1:=pr_x-(Pos.X-PosLT.X);
    if (pos.y-pr_y)<0 then y1:=abs(pos.y-pr_y) else y1:=0;
    if (pos.y+pr_y)>zoom[zoom_size] then y2:=yhgpx-((pos.y+pr_y)-zoom[zoom_size]) else y2:=yhgpx;
    LayerMap.bitmap.LineAS(x1,y1,x1,y2,SetAlpha(Color32(BorderColor),BorderAlpha));
   end;
   LonLatLT.X:=LonLatLT.X+zLon/10000;
   PosLT:=GLonLat2Pos(LonLatLT,zoom_size,sat_map_both);
  end;     }
end;

procedure TFmain.generate_granica;
var y_draw,x_draw,xx,yy,xx1,yy1:longint;
    i,j:integer;
    src:TRect;
    textoutx,textouty:string;
    d2562,x2,x1,y1,zl,twidthx,twidthy,theight:integer;
begin
 if zoom_line=99 then zl:=zoom_size
                 else zl:=zoom_line;
 if (zl<zoom_size)or(zl-zoom_size>5) then exit;
 x2:=trunc(power(2,zl-zoom_size));
 d2562:=256 div x2;
 src:=bounds(0,0,d2562,d2562);
 y_draw:=(256+((pos.y-pr_y)mod 256))mod 256;
 x_draw:=(256+((pos.x-pr_x)mod 256))mod 256;
 if (pos.x-pr_x)>0 then xx1:=((pos.x-pr_x)-((pos.x-pr_x)mod 256))*x2
                   else xx1:=((pos.x-pr_x)-256-((pos.x-pr_x)mod 256))*x2;
 if (pos.y-pr_y)>0 then yy1:=((pos.y-pr_y)-((pos.y-pr_y)mod 256))*x2
                   else yy1:=((pos.y-pr_y)-256-((pos.y-pr_y)mod 256))*x2;
 for i:=0 to hg_x*(x2)+(x_draw div d2562) do
  for j:=0 to hg_y*(x2)+(y_draw div d2562) do
    begin
     xx:=xx1+(i shl 8);
     yy:=yy1+(j shl 8);
     if (xx<0)or(yy<0)or(yy>=zoom[zl])or(xx>=zoom[zl]) then Continue;
     x1:=(i*d2562)-x_draw;
     y1:=(j*d2562)-y_draw;
     LayerMap.bitmap.LineAS(x1,y1,x1+d2562,y1,SetAlpha(Color32(BorderColor),BorderAlpha));
     LayerMap.bitmap.LineAS(x1+d2562,y1,x1+d2562,y1+d2562,SetAlpha(Color32(BorderColor),BorderAlpha));
     if BorderText then
       begin
        LayerMap.bitmap.Font.Size:=8;
        LayerMap.bitmap.Font.Color:=BorderColor;
        LayerMap.bitmap.Font.Name:='Arial';
        textoutx:='x='+inttostr(((pos.x-pr_x+x1)*x2)div 256);
        textouty:='y='+inttostr(((pos.y-pr_y+y1)*x2)div 256);
        twidthx:=LayerMap.bitmap.TextWidth(textoutx);
        twidthy:=LayerMap.bitmap.TextWidth(textouty);
        if ((twidthx+6)<d2562)and((twidthy+6)<d2562) then
         begin
          theight:=LayerMap.bitmap.TextHeight(textoutx);
          LayerMap.bitmap.Textout((x1+d2562 div 2)-tWidthx div 2,(y1+d2562 div 2)-theight,textoutx);
          LayerMap.bitmap.Textout((x1+d2562 div 2)-tWidthy div 2,(y1+d2562 div 2),textouty);
         end;
       end;
    end;
end;

procedure TFmain.generate_mapzap;
begin
 if (zoom_mapzap<=zoom_size)or(zoom_mapzap-zoom_size>8) then begin
                                                              FillingMap.stop:=true;
                                                              FillingMap.LayerMap.Visible:=false;
                                                             end
                                                        else begin
                                                              FillingMap.stop:=false;
                                                              FillingMap.needRepaint:=true;
                                                              FillingMap.Suspended:=false;
                                                             end
end;

procedure BadDraw(var spr:TBitmap32);
begin
 spr.Clear(Color32(clSilver) xor $00000000);
 spr.Canvas.Brush.Color:=Color32(clSilver) xor $00000000;
 spr.Canvas.Rectangle(0,0,256,256);
 spr.Textout(87,120,SAS_ERR_BadFile)
end;

procedure TFmain.generate_im(LastLoad:TLastLoad;err:string);
var Path:String;
    y_draw,x_draw,y_drawN,x_drawN,xx,yy,x_,x_1,y_,y_1,xmov,ymov,size,ii,jj:longint;
    i,j:byte;
    Leyi:integer;
    AcrBuf:Tcursor;
    posN,opmp:TPoint;
    ATilesLoad:TStringList;
    ts:Cardinal;
    lok:string;
begin
 if notpaint then exit;
 ts:=GetTickCount;
 if not(lastload.use) then change_scene:=true;
 ATilesLoad:=TStringList.Create;
 AcrBuf:=map.Cursor;
 if not(lastload.use) then map.Cursor:=crAppStart;
 y_draw:=(256+((pos.y-pr_y)mod 256))mod 256;
 x_draw:=(256+((pos.x-pr_x)mod 256))mod 256;
 xmov:=(OldPos.x-Pos.x);
 ymov:=(OldPos.y-Pos.y);
 if (xmov=0)and(ymov=0) then TilesLoad.Clear;
 opmp:=Point(0,0);
 if ((OldPos.x-pr_x>0)and(Pos.x-pr_x<0)) then opmp.x:=256;
 if ((OldPos.x-pr_x<0)and(Pos.x-pr_x>0)) then opmp.x:=-256;
 if ((OldPos.y-pr_y>0)and(Pos.y-pr_y<0)) then opmp.y:=256;
 if ((OldPos.y-pr_y<0)and(Pos.y-pr_y>0)) then opmp.y:=-256;
 mapbuf.roll(((((OldPos.x-(pr_x)) div 256)-((Pos.x-(pr_x)) div 256))*256)+opmp.x,
             ((((OldPos.y-(pr_y)) div 256)-((Pos.y-(pr_y)) div 256))*256)+opmp.y,false,clBlack32);
// LayerMap.bitmap.roll(xmov,ymov,false,clBlack32);
 if not(lastload.use) then generate_mapzap;
 LayerMap.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
 if aoper<>movemap then LayerMapNal.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
 if GPS_enab then LayerMapGPS.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
 LayerMapWiki.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
 LayerMapWiki.Bitmap.Clear(clBlack);
 destroyWL;
 for i:=0 to hg_x do
  for j:=0 to hg_y do
   begin
    if CiclMap then xx:=X2AbsX(pos.x-pr_x+(i shl 8),zoom_size)
               else xx:=pos.x-pr_x+(i shl 8);
    yy:=pos.y-pr_y+(j shl 8);
    if xx>=0 then xx:=xx-(xx mod 256)
             else xx:=xx-(256-(abs(xx) mod 256));
    if yy>=0 then yy:=yy-(yy mod 256)
             else yy:=yy-(256-(abs(yy) mod 256));
    if (xx<0)or(yy<0)or(yy>=zoom[zoom_size])or(xx>=zoom[zoom_size]) then
      begin
        spr.Clear(Color32(clSilver) xor $00000000);
        mapbuf.Draw((i shl 8),(j shl 8), spr);
        spr.Clear;
        continue;
      end;
    Path:=ffpath(xx,yy,zoom_size,sat_map_both^,false);
    lok:=inttostr(xx)+'-'+inttostr(yy)+'-'+inttostr(zoom_size)+'-'+inttostr(sat_map_both.id);
    {if ((i<>0)and(j<>0)and(i<>hg_x)and(j<>hg_y)) then }ATilesLoad.Add(' '+lok);
    if (TilesLoad.IndexOf(lok)>-1)and(LastLoad.use=false) then continue;
    if (lastload.use)and((lastload.x<>xx)or(lastload.z<>zoom_size)or
       ((lastload.y<yy-128)or(lastload.y>yy+128))) then continue;
    if TileExists(path)
     then begin
           if LoadTilefromCache(spr,path)
             then begin
                   if sat_map_both.DelAfterShow then delFile(path)
                  end
             else BadDraw(spr);
          end
     else loadpre(spr,xx,yy,zoom_size,sat_map_both);
    Gamma(spr);
    mapbuf.Draw((i shl 8),(j shl 8),bounds(0,0,256,256),spr);
    if err<>'' then mapbuf.Textout((i shl 8)+15,(j shl 8)+124,err);
   end;
  spr.SetSize(256,256);
  for Leyi:=0 to length(MapType)-1 do
   if (MapType[Leyi].asLayer)and(MapType[Leyi].active) then
    begin
    if MapType[Leyi].ext='.kml' then
     begin
      loadWL(@MapType[Leyi]);
      continue;
     end;
   posN:=ConvertPosM2M(POS,zoom_size,sat_map_both,@MapType[Leyi]);
   y_drawN:=(((256+((posN.y-pr_y)mod 256)) mod 256)-y_draw);
   x_drawN:=(((256+((posN.x-pr_x)mod 256)) mod 256)-x_draw);
   for i:=0 to hg_x do
    for j:=0 to hg_y do
      begin
       if CiclMap then xx:=X2AbsX(posN.x-pr_x+(i shl 8),zoom_size)
                  else xx:=posN.x-pr_x+(i shl 8);
       yy:=posN.y-pr_y+(j shl 8);
       xx:=xx-(abs(xx) mod 256); yy:=yy-(abs(yy) mod 256);
       if  (xx<0)or(yy<0)or(yy>=zoom[zoom_size])or(xx>=zoom[zoom_size]) then continue;
       Path:=ffpath(xx,yy,zoom_size,MapType[Leyi],false);
       if CiclMap then xx:=X2AbsX(pos.x-pr_x+(i shl 8),zoom_size)
                  else xx:=pos.x-pr_x+(i shl 8);
       yy:=pos.y-pr_y+(j shl 8);
       xx:=xx-(abs(xx) mod 256); yy:=yy-(abs(yy) mod 256);
       lok:=inttostr(xx)+'-'+inttostr(yy)+'-'+inttostr(zoom_size)+'-'+inttostr(MapType[Leyi].id);
       if (sat_map_both.projection=MapType[Leyi].projection)or((j>0)and(j<hg_y)) then ATilesLoad.Add(' '+lok);
       if (TilesLoad.IndexOf(lok)>-1)and(lastload.use=false) then continue;
       if (lastload.use)and(
          (lastload.x<>xx)or(lastload.z<>zoom_size)or((lastload.y<yy-256)and(lastload.y>yy+256)) ) then continue;
       spr.Draw(0,0,bounds((i shl 8)-x_drawN,(j shl 8)-y_drawN,256,256),mapbuf);
       InvertBitmap(spr);
       if TileExists(Path)then
        begin
          //if MapType[Leyi].ext='.png' then LoadPNGintoBitmap32(spr,Path)
          //                            else LoadTilefromCache(spr,Path);
         if LoadTilefromCache(spr,Path)
          then begin
                if MapType[Leyi].DelAfterShow then delFile(path);
                spr.ResetAlpha;
               end
          else BadDraw(spr);
         InvertBitmap(spr);
         mapbuf.Draw((i shl 8)-x_drawN,(j shl 8)-y_drawN, spr);
        end
      end;
    end;
 LayerMap.bitmap.Draw(-x_draw,-y_draw, mapbuf);
 TilesLoad.Clear;
 if ATilesLoad.Count>1 then
 for i:=0 to ATilesLoad.Count-1 do TilesLoad.Add(ATilesLoad.ValueFromIndex[i]);
 ATilesLoad.Free;
 generate_granica;
 DrawGenShBorders;
 if not(lastload.use) then
 begin
 paint_Line;
 if NavOnMark<>nil then NavOnMark.draw;
 if aoper=line then drawLineCalc;
 if aoper=reg then drawReg;
 if aoper=rect then drawRect([]);
 if GPS_enab then drawLineGPS;
 if aoper in [add_line,add_poly] then drawPath(add_line_arr,true,setalpha(clRed32,150),setalpha(clWhite32,50),3,aoper=add_poly);
 try
 draw_point;
 except
 end;
 sm_im_reset(sm_map.width div 2,sm_map.height div 2);
 end;
 m_up.x:=move.X;
 m_up.y:=move.y;
 toSh;
 Label1.caption := IntToStr(GetTickCount-ts);
 map.Cursor:=AcrBuf;
 OldPos:=Pos;
end;


procedure loadMarksIcons;
var SearchRec: TSearchRec;
    startdir:string;
    i:integer;
begin
 marksicons:=TStringList.Create;
 i:=0;
 startdir:=extractfilepath(paramstr(0))+'marksicons\';
 if FindFirst(startdir+'*.png', faAnyFile, SearchRec) = 0 then
    repeat //Application.ProcessMessages;
     if (SearchRec.Attr and faDirectory) <> faDirectory then
      begin
        marksicons.AddObject(SearchRec.Name,TPNGObject.Create);
        TPNGObject(marksicons.Objects[i]).LoadFromFile(startdir+SearchRec.Name);
        inc(i);
      end;
    until FindNext(SearchRec) <> 0;
 FindClose(SearchRec);
end;

procedure TFmain.FormActivate(Sender: TObject);
var  Ini: TMeminifile;
     i,j,r:integer;
     xy,xy1:Tpoint;
     param:string;
begin
 if start=false then exit;
 Fmain.Enabled:=false;
 Ini:=TMeminiFile.Create(copy(paramstr(0),1,length(paramstr(0))-4)+'.ini');
 Maximized:=Ini.Readbool('VIEW','Maximized',true);
 vo_ves_ecran:=Ini.Readbool('VIEW','FullScreen',false);
 TBFullSize.Checked:=vo_ves_ecran;
  if vo_ves_ecran then TBFullSizeClick(TBFullSize)
                  else if Maximized
                        then Fmain.WindowState:=wsMaximized
                        else begin
                              Fmain.SetBounds(
                              ini.ReadInteger('VIEW','FLeft',Fmain.Left),
                              ini.ReadInteger('VIEW','FTop',Fmain.Top),
                              ini.ReadInteger('VIEW','FWidth',Fmain.Width),
                              ini.ReadInteger('VIEW','FHeight',Fmain.Height)
                              )
                             end;

 movepoint:=-1;
 LoadMaps;
 loadMarksIcons;
 if length(MapType)=0 then
  begin
   ShowMessage(SAS_ERR_NoMaps);
   Fmain.Close;
   exit;
  end;
 if FileExists(extractfilepath(paramstr(0))+'marks.sml')
  then begin
        CDSMarks.LoadFromFile(extractfilepath(paramstr(0))+'marks.sml');
        if CDSMarks.RecordCount>0 then
         CDSmarks.SaveToFile(extractfilepath(paramstr(0))+'marks.~sml',dfXMLUTF8);
       end;
 if FileExists(extractfilepath(paramstr(0))+'Categorymarks.sml')
  then begin
        CDSKategory.LoadFromFile(extractfilepath(paramstr(0))+'Categorymarks.sml');
        if CDSKategory.RecordCount>0 then
         CDSKategory.SaveToFile(extractfilepath(paramstr(0))+'Categorymarks.~sml',dfXMLUTF8);
       end;
 Fmain.Enabled:=true;
// CDSMarks.Open;
// CDSKategory.Open;
 TilesLoad:=TStringList.Create;
 nilLastLoad.use:=false;
 notpaint:=true;
 SetDefoultMap;
 Application.OnMessage := DoMessageEvent;
 Application.HelpFile := ExtractFilePath(Application.ExeName)+'help.hlp';
 LenShow:=true;
 ban_pg_ld:=true;
 mWd2:=map.Width shr 1;
 mHd2:=map.Height shr 1;
 All_Dwn_Kb:=0;
 All_Dwn_Tiles:=0;
 Screen.Cursors[1]:=LoadCursor(HInstance, 'SEL');
 Screen.Cursors[2]:=LoadCursor(HInstance, 'LEN');
 Screen.Cursors[3]:=LoadCursor(HInstance, 'HAND');
 Screen.Cursors[4]:=LoadCursor(HInstance, 'SELPOINT');
 move:=point(0,0);
 m_up:=point(0,0);
 anim_zoom:=0;

 TilesOut:=Ini.readInteger('VIEW','TilesOut',0);

 hg_x:=round(Screen.Width / 256)+(integer((Screen.Width mod 256)>0))+TilesOut;
 hg_y:=round(Screen.Height / 256)+(integer((Screen.Height mod 256)>0))+TilesOut;
 pr_x:=(256*hg_x)div 2;
 pr_y:=(256*hg_y)div 2;
 yhgpx:=256*hg_y;
 xhgpx:=256*hg_x;

 vo_ves_ecr:=-3;
 Deg:=pi/180;
 spr:=TBitmap32.Create;
 spr.Width:=256;
 spr.Height:=256;
 setlength(poly_save,0);

 Map.Cursor:=crDefault;
 map.Color:=TColor32(clSilver);
 LayerMap:=TBitmapLayer.Create(map.Layers);
 LayerMap.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
 LayerMap.Bitmap.Width:=xhgpx;
 LayerMap.Bitmap.Height:=yhgpx;
 LayerMap.bitmap.Font.Charset:=RUSSIAN_CHARSET;
// LayerMap.bitmap.Resampler:=TDraftResampler.Create;
 mapbuf:=TBitmap32.Create;
 mapbuf.Width:=xhgpx+256;
 mapbuf.Height:=yhgpx+256;

 LayerMapScale:=TBitmapLayer.Create(map.Layers);
 LayerMapScale.location:=floatrect(bounds(mWd2-145,mHd2-145,290,290));
 LayerMapScale.Bitmap.Width:=290;
 LayerMapScale.Bitmap.Height:=290;
 LayerMapScale.Bitmap.DrawMode:=dmBlend;
 LayerMapScale.Bitmap.CombineMode:=cmMerge;
 LayerMapScale.bitmap.Font.Charset:=RUSSIAN_CHARSET;
 i:=0;
 LayerMapScale.Bitmap.Clear(clBlack);
 LayerMapScale.Bitmap.Canvas.Pen.Color:=clRed;
 LayerMapScale.Bitmap.Font.Size:=6;
 While i<360 do
  begin
   LayerMapScale.Bitmap.Font.Size:=6;
   if (i mod 90) = 0 then begin
                           r:=0;
                           LayerMapScale.Bitmap.Font.Size:=10;
                          end
    else if (i mod 45) = 0 then begin
                                 r:=80;
                                 LayerMapScale.Bitmap.Font.Size:=8;
                                end
     else r:=110;
   xy.x := round(145 + 120 * cos(i*(Pi/180)));
   xy.y := round(145 + 120 * sin(i*(Pi/180)));
   xy1.x := round(145 + r * cos(i*(Pi/180)));
   xy1.y := round(145 + r * sin(i*(Pi/180)));
   LayerMapScale.Bitmap.LineFS(xy.x,xy.y,xy1.x,xy1.y,SetAlpha(clRed32,180));
   if (i mod 15) = 0 then
    begin
     xy1.x := round(145 + 132* cos(i*(Pi/180)))-LayerMapScale.Bitmap.TextWidth(inttostr((i+90)mod 360)+'°')div 2;
     xy1.y := round(145 + 132* sin(i*(Pi/180)))-2-LayerMapScale.Bitmap.Font.size div 2;
     LayerMapScale.Bitmap.RenderText(xy1.x+1,xy1.y+1,inttostr((i+90)mod 360)+'°',3,SetAlpha(clWhite32,250) );
     LayerMapScale.Bitmap.RenderText(xy1.x,xy1.y,inttostr((i+90)mod 360)+'°',3,SetAlpha(clBlue32,250) );
    end;
   inc(i,5);
  end;

 FillingMap:=TFillingMap.create(true);
 FillingMap.FreeOnTerminate:=true;
 FillingMap.Priority:=tpLowest;

 LayerMapNal:=TBitmapLayer.Create(map.Layers);
 LayerMapNal.Bitmap.Width:=xhgpx;
 LayerMapNal.Bitmap.Height:=yhgpx;
 LayerMapNal.Bitmap.DrawMode:=dmBlend;
 LayerMapNal.Bitmap.CombineMode:=cmMerge;
 LayerMapNal.bitmap.Font.Charset:=RUSSIAN_CHARSET;
 LayerMapNal.Visible:=false;

 LayerMapMarks:=TBitmapLayer.Create(map.Layers);
 LayerMapMarks.Bitmap.Width:=xhgpx;
 LayerMapMarks.Bitmap.Height:=yhgpx;
 LayerMapMarks.Bitmap.DrawMode:=dmBlend;
 LayerMapMarks.Bitmap.CombineMode:=cmMerge;
 LayerMapMarks.bitmap.Font.Charset:=RUSSIAN_CHARSET;
 LayerMapMarks.Bitmap.Font.Name:='Tahoma';
 LayerMapMarks.Bitmap.Font.Style:=[];
 LayerMapMarks.Visible:=false;

 LayerMapWiki:=TBitmapLayer.Create(map.Layers);
 LayerMapWiki.Bitmap.Width:=xhgpx;
 LayerMapWiki.Bitmap.Height:=yhgpx;
 LayerMapWiki.Bitmap.DrawMode:=dmTransparent;//dmBlend;
 LayerMapWiki.bitmap.Font.Charset:=RUSSIAN_CHARSET;

 LayerMapGPS:=TBitmapLayer.Create(map.Layers);
 LayerMapGPS.Bitmap.Width:=xhgpx;
 LayerMapGPS.Bitmap.Height:=yhgpx;
 LayerMapGPS.Bitmap.DrawMode:=dmBlend;
 LayerMapGPS.Bitmap.CombineMode:=cmMerge;
 LayerMapGPS.bitmap.Font.Charset:=RUSSIAN_CHARSET;
 LayerMapGPS.Visible:=false;

 layerLineM:=TBitmapLayer.Create(map.Layers);
 layerLineM.location:=floatrect(bounds(6,map.Height-23,128,15));
 layerLineM.Bitmap.Width:=128;
 layerLineM.Bitmap.Height:=15;
 layerLineM.bitmap.DrawMode := dmBlend;
 LayerLineM.bitmap.Font.Charset:=RUSSIAN_CHARSET;
 LayerLineM.bitmap.Font.Name := 'Arial';
 LayerLineM.bitmap.Font.Size := 10;

 LayerMinMap:=TBitmapLayer.Create(map.Layers);
 sm_map.SmMapBitmap:=TBitmap32.Create;
 Sm_Map.SmMapBitmap.CombineMode:=cmMerge;
 Sm_Map.SmMapBitmap.MasterAlpha:=100;
 Sm_Map.SmMapBitmap.Resampler:=TKernelResampler.Create;
 TKernelResampler(Sm_Map.SmMapBitmap.Resampler).Kernel:=TLinearKernel.Create;
 LayerMinMap.bitmap.Font.Charset:=RUSSIAN_CHARSET;
 LayerMinMap.Cursor:=crHandPoint;
 LayerMinMap.OnMouseDown:=LayerMinMapMouseDown;
 LayerMinMap.OnMouseUp:=LayerMinMapMouseUp;
 LayerMinMap.OnMouseMove:=LayerMinMapMouseMove;

 LayerStatBar:=TBitmapLayer.Create(map.Layers);
 LayerStatBar.Location:=floatrect(0,map.Height-17,map.Width,map.Height);
 LayerStatBar.Bitmap.Width:=map.Width;
 LayerStatBar.Bitmap.Height:=17;
 LayerStatBar.Bitmap.DrawMode:=dmBlend;
 LayerStatBar.bitmap.Font.Charset:=RUSSIAN_CHARSET;
 LayerStatBar.bitmap.Font.Name := 'arial';
 LayerStatBar.bitmap.Font.Size := 10;

 ShowMapName:=Ini.readBool('VIEW','ShowMapNameOnPanel',true);
 sm_map.width:=Ini.readInteger('VIEW','SmMapW',160);
 sm_map.height:=Ini.readInteger('VIEW','SmMapH',160);
 sm_map.z1mz2:=Ini.readInteger('VIEW','SmMapDifference',4);
 sm_map.Alpha:=Ini.readInteger('VIEW','SmMapAlpha',220);
 show_point:=Ini.readinteger('VIEW','ShowPointType',2);
 Zoom_Size:=Ini.ReadInteger('POSITION','zoom_size',1);
 DefCache:=Ini.readinteger('VIEW','DefCache',2);
 zoom_mapzap:=Ini.readinteger('VIEW','MapZap',0);
 zoom_line:=Ini.readinteger('VIEW','grid',0);
 mouse_inv:=Ini.readbool('VIEW','invert_mouse',false);
 source:=Ini.Readinteger('VIEW','SourceType',2);
 num_format:=Ini.Readinteger('VIEW','NumberFormat',0);
 CiclMap:=Ini.Readbool('VIEW','CiclMap',false);
 resampling:=Ini.Readinteger('VIEW','ResamlingType',1);
 llStrType:=Ini.Readinteger('VIEW','llStrType',0);
 FirstLat:=Ini.ReadBool('VIEW','FirstLat',false);
 BorderAlpha:=Ini.Readinteger('VIEW','BorderAlpha',150);
 BorderColor:=Ini.Readinteger('VIEW','BorderColor',$FFFFFF);
 BorderText:=Ini.ReadBool('VIEW','BorderText',true);
 GShScale:=Ini.Readinteger('VIEW','GShScale',0);
 MapZapColor:=Ini.Readinteger('VIEW','MapZapColor',clBlack);
 MapZapAlpha:=Ini.Readinteger('VIEW','MapZapAlpha',110);
 Wikim_set.MainColor:=Ini.Readinteger('Wikimapia','MainColor',$FFFFFF);
 Wikim_set.FonColor:=Ini.Readinteger('Wikimapia','FonColor',$000001);

 gamman:=Ini.Readinteger('COLOR_LEVELS','gamma',50);
 contrastn:=Ini.Readinteger('COLOR_LEVELS','contrast',0);
 invertcolor:=Ini.ReadBool('COLOR_LEVELS','InvertColor',false);
 GPS_COM:=Ini.ReadString('GPS','com','COM0');
 BaudRate:=Ini.ReadInteger('GPS','BaudRate',4800);
 GPS_timeout:=Ini.ReadInteger('GPS','timeout',15);
 GPS_update:=Ini.ReadInteger('GPS','update',1000);
 GPS_enab:=Ini.ReadBool('GPS','enbl',false);
 GPS_Log:=Ini.Readbool('GPS','log',true);
 GPS_SizeStr:=Ini.ReadInteger('GPS','SizeStr',25);
 GPS_SizeTrack:=Ini.ReadInteger('GPS','SizeTrack',5);
 GPS_colorStr:=Ini.ReadInteger('GPS','ColorStr',-16776961); //clBlue32
 GPS_popr:=extpoint(Ini.ReadFloat('GPS','popr_lon',0),Ini.ReadFloat('GPS','popr_lat',0));
 GPS_path:=Ini.ReadBool('GPS','path',true);
 GPS_go:=Ini.ReadBool('GPS','go',true);
 OldCpath_:=Ini.Readstring('PATHtoCACHE','GMVC','cache_old\');
 NewCpath_:=Ini.Readstring('PATHtoCACHE','SASC','cache\');
 ESCpath_:=Ini.Readstring('PATHtoCACHE','ESC','cache_ES\');
 GMTilesPath_:=Ini.Readstring('PATHtoCACHE','GMTiles','cache_gmt\');
 POS.x:=Ini.ReadInteger('POSITION','x',zoom[zoom_size]div 2 +1);
 POS.y:=Ini.ReadInteger('POSITION','y',zoom[zoom_size]div 2 +1);
 oldPOS:=pos;
 InetConnect.userwinset:=Ini.Readbool('INTERNET','userwinset',true);
 InetConnect.uselogin:=Ini.Readbool('INTERNET','uselogin',true);
 InetConnect.Proxyused:=Ini.Readbool('INTERNET','used_proxy',false);
 InetConnect.proxystr:=Ini.Readstring('INTERNET','proxy','');
 InetConnect.loginstr:=Ini.Readstring('INTERNET','login','');
 InetConnect.passstr:=Ini.Readstring('INTERNET','password','');
 SaveTileNotExists:=Ini.ReadBool('INTERNET','SaveTileNotExists',false);
 dblDwnl:=Ini.ReadBool('INTERNET','DblDwnl',true);
 GoNextTile:=Ini.ReadBool('INTERNET','GoNextTile',false);
 backload:=Ini.Readbool('VIEW','back_load',true);
 animate:=Ini.Readbool('VIEW','animate',true);
 sparam:=Ini.ReadBool('NPARAM','stat',true);
 i:=1;
 while Ini.ReadFloat('HIGHLIGHTING','pointx_'+inttostr(i),2147483647)<>2147483647 do
  begin
   setlength(poly_save,i);
   poly_save[i-1].x:=Ini.ReadFloat('HIGHLIGHTING','pointx_'+inttostr(i),2147483647);
   poly_save[i-1].y:=Ini.ReadFloat('HIGHLIGHTING','pointy_'+inttostr(i),2147483647);
   inc(i);
  end;
 if length(poly_save)>0 then poly_zoom_save:=Ini.Readinteger('HIGHLIGHTING','zoom',1);

 ShowLine.Checked:=Ini.readbool('VIEW','line',true);
 LayerMapScale.Visible:=Ini.readbool('VIEW','showscale',false);
 ShowMiniMap.Checked:=Ini.readbool('VIEW','minimap',true);
 ShowStatus.Checked:=Ini.readbool('VIEW','statusbar',true);
 LayerMapScale.Visible:=Ini.readbool('VIEW','showscale',false);

 NzoomIn.ShortCut:=Ini.Readinteger('HOTKEY','ZoomIn',33);
 NzoomOut.ShortCut:=Ini.Readinteger('HOTKEY','ZoomOut',34);
 N14.ShortCut:=Ini.Readinteger('HOTKEY','GoTo',16455);
 NCalcRast.ShortCut:=Ini.Readinteger('HOTKEY','CalcRast',16460);
 NRECT.ShortCut:=Ini.Readinteger('HOTKEY','Rect',32850);
 NRegion.ShortCut:=Ini.Readinteger('HOTKEY','Polyg',32848);
 N41.ShortCut:=Ini.Readinteger('HOTKEY','Coord',16451);
 N42.ShortCut:=Ini.Readinteger('HOTKEY','Previous',16450);
 NSRCinet.ShortCut:=Ini.Readinteger('HOTKEY','inet',32841);
 NSRCesh.ShortCut:=Ini.Readinteger('HOTKEY','Cache',32835);
 NSRCic.ShortCut:=Ini.Readinteger('HOTKEY','CachInet',32834);
 Showstatus.ShortCut:=Ini.Readinteger('HOTKEY','Showstatus',32851);
 ShowLine.ShortCut:=Ini.Readinteger('HOTKEY','ShowLine',32844);
 ShowMiniMap.ShortCut:=Ini.Readinteger('HOTKEY','ShowMiniMap',32845);
 NFoolSize.ShortCut:=Ini.Readinteger('HOTKEY','FoolSize',122);
 NGoToCur.ShortCut:=Ini.Readinteger('HOTKEY','GoToCur',49219);
 Nbackload.ShortCut:=Ini.Readinteger('HOTKEY','backload',49218);
 Nanimate.ShortCut:=Ini.Readinteger('HOTKEY','animate',49217);
 NCiclMap.ShortCut:=Ini.Readinteger('HOTKEY','CiclMap',49242);
 N32.ShortCut:=Ini.Readinteger('HOTKEY','ShowScale',49235);
 NGPSconn.ShortCut:=Ini.Readinteger('HOTKEY','GPSconn',49223);
 NGPSPath.ShortCut:=Ini.Readinteger('HOTKEY','GPSPath',49236);
 NGPSToPoint.ShortCut:=Ini.Readinteger('HOTKEY','GPSToPoint',0);
 NSaveTreck.ShortCut:=Ini.Readinteger('HOTKEY','SaveTreck',16467);
 Ninvertcolor.ShortCut:=Ini.Readinteger('HOTKEY','InvertColor',32846);
 NLoadSelFromFile.ShortCut:=Ini.Readinteger('HOTKEY','LoadSelFromFile',0);

 TMenuItem(FindComponent('NGShScale'+IntToStr(GShScale))).Checked:=true;
 N32.Checked:=LayerMapScale.Visible;
 Ninvertcolor.Checked:=invertcolor;
 TBGPSconn.Checked:=GPS_enab;
 if GPS_enab then TBGPSconnClick(TBGPSconn);
 TBGPSPath.Checked:=GPS_path;
 NGPSPath.Checked:=GPS_path;
 TBGPSToPoint.Checked:=GPS_go;
 NGPSToPoint.Checked:=GPS_go;
 Nbackload.Checked:=backload;
 Nanimate.Checked:=animate;

 if not(FileExists(copy(paramstr(0),1,length(paramstr(0))-4)+'.ini')) then
  begin
   //ZoomToolBar.Dock(TBDockLeft,bounds(0,0,1,1));
   TBEditPath.Floating:=true;
   TBEditPath.MoveOnScreen(true);
   TBEditPath.FloatingPosition:=Point(Fmain.Left+map.Left+30,Fmain.Top+map.Top+70);
  end;  

 NMainToolBarShow.Checked:=TBMainToolBar.Visible;
 NZoomToolBarShow.Checked:=ZoomToolBar.Visible;
 NsrcToolBarShow.Checked:=SrcToolbar.Visible;
 NGPSToolBarShow.Checked:=GPSToolBar.Visible;
 NGPSToolBarShow.Checked:=GPSToolBar.Visible;
 NMarksBarShow.Checked:=TBMarksToolBar.Visible;

 TBFullSize.Checked:=vo_ves_ecran;
 NCiclMap.Checked:=CiclMap;

 toSh;
 start:=false;

 Ini.Free;
 case source of
   1: NSRCinetClick(NSRCinet);
   2: NSRCinetClick(NSRCesh);
   3: NSRCinetClick(NSRCic);
 end;
 if zoom_mapzap<>0 then TBMapZap.Caption:='x'+inttostr(zoom_mapzap)
                   else TBMapZap.Caption:='';
 selectMap(sat_map_both);
 ShowLine.OnClick(sender);
 ShowMiniMap.OnClick(sender);
 ShowStatus.OnClick(sender);
 RxSlider1.Value:=Zoom_size-1;
 notpaint:=false;

 if ParamCount > 1 then
 begin
  param:=paramstr(1);
  if param<>'' then For i:=0 to length(MapType)-1 do if MapType[i].guids=param then sat_map_both:=@MapType[i];
  if paramstr(2)<>'' then zoom_size:=strtoint(paramstr(2));
  if (paramstr(3)<>'')and(paramstr(4)<>'') then POS:=GLonLat2Pos(ExtPoint(str2r(paramstr(3)),str2r(paramstr(4))),zoom_size,sat_map_both);
 end;

 zooming(Zoom_size,false);
 dwn:=false;
 Fsaveas.PageControl1.ActivePageIndex:=0;
 if InetConnect.proxyused then
  try
   EmbeddedWB1_.ProxySettings.Address:=copy(InetConnect.proxystr,1,PosEx(':',InetConnect.proxystr)-1);
   EmbeddedWB1_.ProxySettings.Port:=strtoint(copy(InetConnect.proxystr,PosEx(':',InetConnect.proxystr)+1,length(InetConnect.proxystr)-PosEx(':',InetConnect.proxystr)));
  except
   ShowMessage(SAS_ERR_ProxyStrFormat);
  end;
 if sparam then EmbeddedWB1_.Navigate('http://sasgis.ru/stat/index.html');
 Fmain.Enabled:=true;
 Fmain.SetFocus;
 if (FLogo<>nil)and(FLogo.Visible) then FLogo.Timer1.Enabled:=true;

end;

procedure TFmain.sm_im_reset_type2(x,y:integer);
var bm:TBitmap32;
    pos_sm,d:TPoint;
    path:string;
    x128,y128,ilay:integer;
    m_t:PMapType;
begin
 bm:=TBitmap32.Create;
 bm.Width:=256;
 bm.Height:=256;
 Sm_Map.SmMapBitmap.Width:=256;
 Sm_Map.SmMapBitmap.Height:=256;
 Sm_Map.SmMapBitmap.Clear(Color32(clSilver) xor $00000000);
// pos_sm:=Point(pos.X div round(power(2,(zoom_size-sm_map.zoom))),pos.y div round(power(2,zoom_size-sm_map.zoom)));
 pos_sm:=Point(pos.X shr (zoom_size-sm_map.zoom),pos.y shr (zoom_size-sm_map.zoom));
 if longint(sm_map.maptype)=0 then m_t:=sat_map_both
                              else m_t:=sm_map.maptype;
 Pos_sm:=ConvertPosM2M(Pos_sm,sm_map.zoom,sat_map_both,m_t);
 d:=Point((pos_sm.X-128),(pos_sm.y-128));
 if d.x<0 then d.x:=256+d.x;
 if d.y<0 then d.y:=256+d.y;
 d:=Point((d.x mod 256),(d.y mod 256));

 x128:=-128;
 while (x128<=128) do
  begin
   y128:=-128;
   if (CiclMap)or((pos_sm.X+x128<=zoom[sm_map.zoom])and(pos_sm.X+x128>=0)) then
   while (y128<=128) do
    begin
     if (pos_sm.y+y128<=zoom[sm_map.zoom])and(pos_sm.y+y128>=0) then
      begin
       path:=ffpath(pos_sm.X+x128,pos_sm.y+y128,sm_map.zoom,m_t^,false);
       bm.Clear(Color32(clSilver) xor $00000000);
       if (tileexists(path))
        then begin
              if not(LoadTilefromCache(bm,path))
               then bm.Clear(Color32(clSilver) xor $00000000);
             end
        else loadpre(bm,pos_sm.x+x128,pos_sm.y+y128,sm_map.zoom,m_t);
       Sm_Map.SmMapBitmap.Draw((128+x128)-d.x,(128+y128)-d.y,bm);
      end;
     inc(y128,256);
    end;
   inc(x128,256);
  end;

 for iLay:=0 to length(MapType)-1 do
  if (MapType[iLay].asLayer)and(MapType[iLay].ShowOnSmMap)and(MapType[iLay].ext<>'.kml') then
  begin
   //pos_sm:=Point(Pos.X div round(power(2,(zoom_size-sm_map.zoom))),Pos.y div round(power(2,zoom_size-sm_map.zoom)));
   pos_sm:=Point(pos.X shr (zoom_size-sm_map.zoom),pos.y shr (zoom_size-sm_map.zoom));
   Pos_sm:=ConvertPosM2M(Pos_sm,sm_map.zoom,sat_map_both,@MapType[iLay]);
   d:=Point((pos_sm.X-128),(pos_sm.y-128));
   if d.x<0 then d.x:=256+d.x;
   if d.y<0 then d.y:=256+d.y;
   d:=Point((d.x mod 256),(d.y mod 256));
   x128:=-128;
   while (x128<=128) do
    begin
     y128:=-128;
     if (CiclMap)or((pos_sm.X+x128<=zoom[sm_map.zoom])and(pos_sm.X+x128>=0)) then
     while (y128<=128) do
      begin
       if (pos_sm.y+y128<=zoom[sm_map.zoom])and(pos_sm.y+y128>=0) then
        begin
         path:=ffpath(pos_sm.X+x128,pos_sm.y+y128,sm_map.zoom,MapType[iLay],false);
         bm.Clear(Color32(clSilver) xor $00000000);
         bm.Draw(0,0,bounds((128+x128)-d.x,(128+y128)-d.y,256,256),Sm_Map.SmMapBitmap);
         if (tileexists(path))and(not((pos_sm.Y-y128<0)or(pos_sm.Y+y128>zoom[sm_map.zoom])) )
          then LoadTilefromCache(bm,path);
         Sm_Map.SmMapBitmap.Draw((128+x128)-d.x,(128+y128)-d.y,bm);
        end;
       inc(y128,256);
      end;
     inc(x128,256);
    end;
  end;
  bm.Free;
end;

procedure TFmain.sm_im_reset(x,y:integer);
var Polygon: TPolygon32;
    iLay:integer;
begin
 if LayerMinMap.Visible=false then exit;
 if LayerStatBar.Visible then LayerMinMap.location:=floatrect(bounds(map.Width-sm_map.width-5,map.Height-sm_map.height-17,sm_map.width+5,sm_map.height))
                         else LayerMinMap.location:=floatrect(bounds(map.Width-sm_map.width-5,map.Height-sm_map.height,sm_map.width+5,sm_map.height));
 LayerMinMap.Bitmap.Width:=sm_map.width+5;
 LayerMinMap.Bitmap.Height:=sm_map.height+5;
 sm_map.zoom:=zoom_size-sm_map.z1mz2;
 if zoom_size-sm_map.z1mz2<=0 then sm_map.zoom:=1;
 if sm_map.zoom>1
  then begin
        if ((x=sm_map.width div 2)and(y=sm_map.height div 2))and(not sm_map.size_dw) then sm_im_reset_type2(x,y);
        sm_map.dx:=round((sm_map.width/256)*((Fmain.map.Width/zoom[zoom_size])*zoom[sm_map.zoom]) );
        sm_map.dy:=round((sm_map.height/256)*((Fmain.map.Height/zoom[zoom_size])*zoom[sm_map.zoom]) );
        sm_map.pos:=Point(x,y);
       end
  else begin
         if (longint(sm_map.maptype)=0)
           then begin
                 if not(LoadTilefromCache(Sm_Map.SmMapBitmap,ffpath(128,128,1,sat_map_both^,false)))
                  then Sm_Map.SmMapBitmap.Assign(DefoultMap);
                end
           else if not(LoadTilefromCache(Sm_Map.SmMapBitmap,ffpath(128,128,1,sm_map.maptype^,false)))
                 then Sm_Map.SmMapBitmap.Assign(DefoultMap);
         for iLay:=0 to length(MapType)-1 do
          if (MapType[iLay].asLayer)and(MapType[iLay].ShowOnSmMap)and(MapType[iLay].ext<>'.kml') then
            if not(LoadTilefromCache(Sm_Map.SmMapBitmap,ffpath(128,128,1,@MapType[iLay],false)))
              then Sm_Map.SmMapBitmap.Assign(DefoultMap);
        if (x=sm_map.width div 2)and(y=sm_map.height div 2)
         then sm_map.pos:=Point(round(pos.x*(sm_map.width/zoom[zoom_size])),round(pos.y*(sm_map.height/zoom[zoom_size])))
         else sm_map.pos:=Point(x,y);
        sm_map.dx:=round(sm_map.width*(map.Width/zoom[zoom_size]));
        sm_map.dy:=round(sm_map.height*(map.Height/zoom[zoom_size]));
       end;
 LayerMinMap.bitmap.Draw(bounds(5,5,sm_map.width,sm_map.height),bounds(0,0,256,256),Sm_Map.SmMapBitmap);
 gamma(LayerMinMap.bitmap);

 Polygon := TPolygon32.Create;
 Polygon.Antialiased:=true;
 Polygon.Add(FixedPoint((sm_map.pos.x-sm_map.dx div 2)+4-2,(sm_map.pos.y-sm_map.dy div 2)+4-2));
 Polygon.Add(FixedPoint((sm_map.pos.x-sm_map.dx div 2)+sm_map.dx+4+2,(sm_map.pos.y-sm_map.dy div 2)+4-2));
 Polygon.Add(FixedPoint((sm_map.pos.x-sm_map.dx div 2)+sm_map.dx+4+2,(sm_map.pos.y-sm_map.dy div 2)+sm_map.dy+4+2));
 Polygon.Add(FixedPoint((sm_map.pos.x-sm_map.dx div 2)+4-2,(sm_map.pos.y-sm_map.dy div 2)+sm_map.dy+4+2));
  with Polygon.Outline do
   begin
    with Grow(Fixed(3.2 / 2), 0.5) do
     begin
      FillMode := pfWinding;
      DrawFill(LayerMinMap.bitmap,SetAlpha(clNavy32,(zoom_size-sm_map.zoom)*43));
      free;
     end;
    free;
   end;
///  with Polygon.Outline.Grow(Fixed(3.2/2),0.5) do
//  begin
//   FillMode:=pfWinding;
//   DrawFill(LayerMinMap.bitmap,SetAlpha(clNavy32,(zoom_size-sm_map.zoom)*43));
//  end;
 Polygon.DrawFill(LayerMinMap.bitmap,SetAlpha(clWhite32,(zoom_size-sm_map.zoom)*35));
 Polygon.Free;

 LayerMinMap.bitmap.Canvas.brush.Color:=$e0e0e0;
 LayerMinMap.bitmap.Canvas.Pen.Color:=ClBlack;
 LayerMinMap.bitmap.Canvas.Polygon([point(0,sm_map.height+5),point(0,0),point(sm_map.width+5,0),point(sm_map.width+5,4),point(4,4),point(4,sm_map.height+5){point(34,0),point(38,4),point(38,smm-4),point(sm_map.width+5,smm-4),point(sm_map.width+5,smm),point(4,smm),point(4,sm_map.height+smm),point(0,sm_map.height+smm)}]);
 LayerMinMap.bitmap.Canvas.Pixels[2,((sm_map.height+5) div 2)-6]:=clBlack;
 LayerMinMap.bitmap.Canvas.Pixels[2,((sm_map.height+5) div 2)-2]:=clBlack;
 LayerMinMap.bitmap.Canvas.Pixels[2,((sm_map.height+5) div 2)+2]:=clBlack;
 LayerMinMap.bitmap.Canvas.Pixels[2,((sm_map.height+5) div 2)+6]:=clBlack;
 LayerMinMap.bitmap.DrawMode:=dmBlend;
 LayerMinMap.bitmap.ResetAlpha(sm_map.alpha);
 if sm_map.z1mz2>1 then LayerMinMap.bitmap.Draw(6,6,Sm_Map.PlusButton);
 if zoom_size-sm_map.z1mz2>1 then LayerMinMap.bitmap.Draw(19,6,Sm_Map.MinusButton);
 LayerMinMap.BringToFront;
end;

procedure TFmain.zooming(x:byte;move:boolean);
var w,w1,i:integer;
begin
 if x<=1  then TBZoom_Out.Enabled:=false
          else TBZoom_Out.Enabled:=true;
 if x>=24 then TBZoomIn.Enabled:=false
          else TBZoomIn.Enabled:=true;
 NZoomIn.Enabled:=TBZoomIn.Enabled;
 NZoomOut.Enabled:=TBZoom_Out.Enabled;
 if (anim_zoom=1){or(x=zoom_size)}or(dwn)or(x<1)or(x>24) then exit;
 anim_zoom:=1;
 labZoom.caption:=' '+inttostr(zoom_size)+'x ';
 labZoom.caption:=' '+inttostr(x)+'x ';
 RxSlider1.Value:=x-1;
 if zoom_size>x
  then begin
         zoom_in:=-1;
         w:=-20;
         w1:=-10;
         POS:=Point(trunc(pos.x/power(2,zoom_size-x)),trunc(pos.y/power(2,zoom_size-x)));
         if (move)and(abs(x-zoom_size)=1) then
           POS:=Point(pos.x+(mWd2-m_m.X)div 2,pos.y+(mHd2-m_m.y)div 2);
       end
  else begin
         zoom_in:=1;
         w:=10;
         w1:=5;
         POS:=Point(trunc(pos.x*power(2,x-zoom_size)),trunc(pos.y*power(2,x-zoom_size)));
         if (move)and(abs(x-zoom_size)=1) then
           POS:=Point(pos.x-(mWd2-m_m.X),pos.y-(mHd2-m_m.y));
       end;
 LayerMapNal.Bitmap.Clear(clBlack);
 LayerMapgps.Bitmap.Clear(clBlack);
 LayerMapWiki.Visible:=false;
 if (abs(x-zoom_size)=1)and(Nanimate.Checked)
   then for i:=0 to 10 do
         begin
          if (move)and(Nanimate.Checked) then
           LayerMap.Location:=
              floatrect(bounds(mWd2-pr_x-round(((xhgpx shr 1)/w)*i)+round(((mWd2-m_m.X)/w1/2)*i),
                               mHd2-pr_y-round(((yhgpx shr 1)/w)*i)+round(((mHd2-m_m.y)/w1/2)*i),
                               xhgpx+round(((xhgpx shr 1)/w)*i*2),yhgpx+round(((yhgpx shr 1)/w)*i*2)))
           else
             if (Nanimate.Checked)and(not(move)) then
              LayerMap.Location:=
              floatrect(bounds(mWd2-pr_x-round(((xhgpx shr 1)/w)*i),mHd2-pr_y-round(((yhgpx shr 1)/w)*i),
                               xhgpx+round(((xhgpx shr 1)/w)*i*2),yhgpx+round(((yhgpx shr 1)/w)*i*2)));
          if (FillingMap.LayerMap<>nil)and(FillingMap.LayerMap.Visible) then FillingMap.LayerMap.Location:=LayerMap.Location;
          if (LayerMapMarks.Visible) then LayerMapMarks.Location:=LayerMap.Location;
          application.ProcessMessages;
         end;
 zoom_size:=x;
 generate_im(nilLastLoad,'');
 anim_zoom:=0;
 toSh;
end;

procedure TFmain.NzoomInClick(Sender: TObject);
begin
 zooming(zoom_size+1,false);
end;

procedure TFmain.NZoomOutClick(Sender: TObject);
begin
 zooming(zoom_size-1,false);
end;

procedure TFmain.FormCreate(Sender: TObject);
begin
 ProgrammPath:=ExtractFilePath(ParamStr(0));
 Application.Title:=Fmain.Caption;
 TBiniLoadPositions(Fmain,copy(paramstr(0),1,length(paramstr(0))-4)+'.ini','PANEL_');
 TBEditPath.Visible:=false;
 Fmain.Caption:=Fmain.Caption+' '+SASVersion;
 start:=true;
 XPMenu:=TMXPMenu.Create(Fmain);
 XPMenu.Active:=false;
 XPMenu.IconBackColor:=clBtnFace;
 XPMenu.BackColor:=clWhite;
end;

procedure TFmain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if GPSReceiver.IsConnected <> spNone then GPSReceiver.Close();
 FreeAndNil(XPMenu);
 close_:=true;
 if length(MapType)<>0 then FSettings.Save;
 marksicons.free;
 TilesLoad.Free;
end;

procedure TFmain.TBmoveClick(Sender: TObject);
begin
 setalloperationfalse(movemap);
// setalloperationfalse(newop:TAOperation);
end;

procedure TFmain.TBZoom_outClick(Sender: TObject);
begin
 zooming(Zoom_size-1,false);
end;

procedure TFmain.TBZoomInClick(Sender: TObject);
begin
 zooming(Zoom_size+1,false);
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
 vo_ves_ecran:=TBFullSize.Checked;
 TBDock.Visible:=not(TBFullSize.Checked);
 TBDockLeft.Visible:=not(TBFullSize.Checked);
 TBDockBottom.Visible:=not(TBFullSize.Checked);
 TBDockRight.Visible:=not(TBFullSize.Checked);
 if TBFullSize.Checked then
  begin
   RectWindow:=BoundsRect;
   SetBounds(Left-ClientOrigin.X,Top-ClientOrigin.Y,GetDeviceCaps(Canvas.handle,
   HORZRES)+(Width-ClientWidth),GetDeviceCaps(Canvas.handle,VERTRES)+(Height-ClientHeight));
  end
  else BoundsRect:=RectWindow;
// generate_im(nilLastLoad,'');
end;

procedure TextToHTMLDoc(Text: string; var Document: IHTMLDocument2);
var
  //Document: IHTMLDocument2;
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
         TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoom_out),2);
         TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoomin),0);
        end
   else begin
         RxSlider1.Orientation:=RxSlider.soHorizontal;
         TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoom_out),0);
         TTBToolBar(sender).Items.Move(TTBToolBar(sender).Items.IndexOf(TBZoomin),2);
        end
end;


procedure TFmain.RxSlider1Change(Sender: TObject);
begin
 labZoom.Caption:=' '+inttostr(RxSlider1.Value+1)+'x ';
end;

procedure TFmain.RxSlider1Changed(Sender: TObject);
begin
 zooming(RxSlider1.Value+1,false);
 fmain.SetFocusedControl(map);
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
 if sender is TTBItem then selectMap(PMapType(TMenuItem(sender).tag))
                      else selectMap(PMapType(TMenuItem(sender).tag));
end;

procedure TFmain.N8Click(Sender: TObject);
begin
 fsettings.ShowModal;
end;

procedure TFmain.NbackloadClick(Sender: TObject);
begin
 generate_im(nilLastLoad,'');
end;

procedure TFmain.NaddPointClick(Sender: TObject);
begin
 if FAddPoint.show_(extPoint(X2Lon(m_up.x),Y2Lat(m_up.y)),true) then
  generate_im(nilLastLoad,'');
end;

procedure TFmain.N20Click(Sender: TObject);
var btm:TBitmap32;
    btm1:TBitmap;
begin
 btm:=TBitmap32.Create;
 if LoadTilefromCache(btm,ffpath(X2absX(pos.x-(mWd2-move.x),zoom_size),pos.y-(mHd2-move.y),zoom_size,sat_map_both^,false))
  then begin
        btm1:=TBitmap.Create;
        btm1.Width:=256; btm1.Height:=256;
        btm.DrawTo(btm1.Canvas.Handle,0,0);
        CopyBtmToClipboard(btm1);
        btm1.Free;
       end;
 btm.Free;
end;

procedure TFmain.N30Click(Sender: TObject);
var ll:TExtendedPoint;
begin
 ll:=GPos2LonLat(mouseXY2Pos(Point(move.X,move.Y)),zoom_size,sat_map_both);
 if FirstLat then CopyStringToClipboard(lat2str(ll.y)+' '+lon2str(ll.x))
             else CopyStringToClipboard(lon2str(ll.x)+' '+lat2str(ll.y));
end;

procedure TFmain.N15Click(Sender: TObject);
begin
 CopyStringToClipboard(ffpath(X2AbsX(pos.x-(mWd2-move.x),zoom_size),pos.y-(mHd2-move.y),zoom_size,sat_map_both^,false));
end;

procedure TFmain.N21Click(Sender: TObject);
var path:string;
    APos:TPoint;
    AMapType:PMapType;
begin
 if TMenuItem(sender).Tag=0 then AMapType:=sat_map_both
                            else AMapType:=PMapType(TMenuItem(sender).Tag);
 APos:=ConvertPosM2M(pos,zoom_size,sat_map_both,AMapType);
 path:=ffpath(APos.x-(mWd2-m_up.x),APos.y-(mHd2-m_up.y),zoom_size,AMapType^,false);
 if ((not(tileExists(path)))or(MessageBox(handle,pchar(SAS_STR_file+' '+path+' '+SAS_MSG_FileExists),pchar(SAS_MSG_coution),36)=IDYES)) then
   with ThreadAllLoadMap.Create(False,[Point(Apos.x-(mWd2-m_up.x),Apos.y-(mHd2-m_up.y))],1,true,false,false,true,zoom_size,AMapType,date) do
   begin
    FreeOnTerminate:=true;
    OnTerminate:=ThreadDone;
   end;
end;

procedure TFmain.N11Click(Sender: TObject);
var WindirP: PChar;
begin
  WinDirP:=StrAlloc(MAX_PATH);
  GetWindowsDirectory(WinDirP, MAX_PATH);
  SaveTileInCache(LayerMap.bitmap,StrPas(WinDirP)+'\SASwallpaper.bmp');
  with TRegIniFile.Create('Control Panel') do
   begin
    WriteString('desktop', 'Wallpaper', StrPas(WinDirP)+'\SASwallpaper.bmp');
    WriteString('desktop', 'TileWallpaper', '0');
    free;
   end;
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE);
end;

procedure TFmain.NopendirClick(Sender: TObject);
begin
 ShellExecute(0,'open',PChar(ffpath(pos.x-(mWd2-m_m.x),pos.y-(mHd2-m_m.y),zoom_size,sat_map_both^,false)),nil,nil,SW_SHOWNORMAL);
end;

procedure TFmain.N25Click(Sender: TObject);
var s:string;
    i:integer;
begin
 s:=ffpath(pos.x-(mWd2-m_m.x),pos.y-(mHd2-m_m.y),zoom_size,sat_map_both^,false);
 for i:=length(s) downto 0 do if s[i]='\'then break;
 ShellExecute(0,'open',PChar(copy(s,1,i)),nil,nil,SW_SHOWNORMAL);
end;

procedure TFmain.NDelClick(Sender: TObject);
var s:string;
    AMapType:PMapType;
    APos:TPoint;
begin
 if TMenuItem(sender).Tag=0 then AMapType:=sat_map_both
                            else AMapType:=PMapType(TMenuItem(sender).Tag);
 APos:=ConvertPosM2M(pos,zoom_size,sat_map_both,AMapType);
 s:=ffpath(APos.x-(mWd2-m_up.x),APos.y-(mHd2-m_up.y),zoom_size,AMapType^,false);
 if (MessageBox(handle,pchar(SAS_MSG_youasure+' '+s+'?'),pchar(SAS_MSG_coution),36)=IDYES)
  then begin
        if TileExists(s) then DelFile(s);
        generate_im(nilLastLoad,'');
       end;
       
end;

procedure TFmain.NSRCinetClick(Sender: TObject);
begin
 if (sender is TMenuItem)
  then source:=TMenuItem(Sender).Tag
  else source:=TTBItem(Sender).Tag;
 TBSrc.ImageIndex:=source-1;
 case source of
   1: NSRCinet.Checked:=true;
   2: NSRCesh.Checked:=true;
   3: NSRCic.Checked:=true;
 end;
 if THLoadMap1<>nil then THLoadMap1.Terminate;
 if source<>2 then
  begin
   While THLoadMap1<>nil do Application.ProcessMessages;
   change_scene:=true;
   THLoadMap1:=ThreadAllLoadMap.Create(False,[],4,NSRCinet.Checked,false,false,true,zoom_size,sat_map_both,date);
   THLoadMap1.FreeOnTerminate:=true;
   THLoadMap1.Priority:=tpLower;
   THLoadMap1.OnTerminate:=ThreadDone;
  end
end;
             
procedure TFmain.N16Click(Sender: TObject);
begin
 fabout.ShowModal;
end;

procedure TFmain.TBREGIONClick(Sender: TObject);
begin
 TBRectSave.ImageIndex:=9;
 TBRectSave.Checked:=true;
 setalloperationfalse(reg);
end;

procedure TFmain.TBRECTClick(Sender: TObject);
begin
 TBRectSave.ImageIndex:=6;
 TBRectSave.Checked:=true;
 setalloperationfalse(rect);
end;

procedure TFmain.TBRectSaveClick(Sender: TObject);
begin
 if TBRectSave.ImageIndex=6 then setalloperationfalse(rect)
                            else setalloperationfalse(reg)
end;

procedure TFmain.TBItem1Click(Sender: TObject);
begin
 if length(poly_save)>0 then fsaveas.Show_(poly_zoom_save,poly_save)
                        else showmessage(SAS_MSG_NeedHL);
end;

//карта заполнения в основном окне
procedure TFmain.TBMapZapPopup(Sender: TTBCustomItem; FromLink: Boolean);
var i:integer;
begin
 if zoom_mapzap=0 then TBMapZap.Items[0].Checked:=true;
 for i:=1 to 8 do
  if zoom_size+i<24 then begin
                          TBMapZap.Items[i].Caption:=SAS_STR_InMainWindowFor+' х'+inttostr(zoom_size+i);
                          TBMapZap.Items[i].Visible:=true;
                          TBMapZap.Items[i].Tag:=zoom_size+i;
                          if TBMapZap.Items[i].Tag=zoom_mapzap then TBMapZap.Items[i].Checked:=true
                                                               else TBMapZap.Items[i].Checked:=false;
                         end
                    else TBMapZap.Items[i].Visible:=false;
end;

procedure TFmain.N17Click(Sender: TObject);
var i:byte;
begin
 if zoom_mapzap=0 then N17.Items[0].Checked:=true;
 for i:=1 to 8 do
  if zoom_size+i<24 then begin
                          N17.Items[i].Caption:=SAS_STR_for+' х'+inttostr(zoom_size+i);
                          N17.Items[i].Visible:=true;
                          N17.Items[i].Tag:=zoom_size+i;
                          if N17.Items[i].Tag=zoom_mapzap then N17.Items[i].Checked:=true
                                                          else N17.Items[i].Checked:=false;
                         end
                    else N17.Items[i].Visible:=false;
end;

procedure TFmain.TBMapZap1Click(Sender: TObject);
begin
 zoom_mapzap:=TTBItem(Sender).Tag;
 if TTBItem(Sender).Tag<>0 then TBMapZap.Caption:='x'+inttostr(TTBItem(Sender).Tag)
                           else TBMapZap.Caption:='';
 generate_im(nilLastLoad,'');
end;

procedure TFmain.N18Click(Sender: TObject);
begin
 zoom_mapzap:=TMenuItem(sender).tag;
 if TMenuItem(Sender).Tag<>0 then TBMapZap.Caption:='x'+inttostr(TMenuItem(Sender).Tag)
                             else TBMapZap.Caption:='';
 generate_im(nilLastLoad,'');
end;
//X-карта заполнения в основном окне

procedure TFmain.TBCalcRasClick(Sender: TObject);
begin
 setalloperationfalse(line);
end;

procedure TFmain.NCiclMapClick(Sender: TObject);
begin
 ciclmap:=NCiclMap.Checked;
 generate_im(nilLastLoad,'');
end;

procedure TFmain.N012Click(Sender: TObject);
begin
 topos(Y2Lat(m_up.Y),X2Lon(m_up.X),TMenuItem(sender).tag,true);
end;

procedure TFmain.N29Click(Sender: TObject);
begin
 ShellExecute(0,'open',PChar(ExtractFilePath(ParamStr(0))+'help.chm'),nil,nil,SW_SHOWNORMAL);
end;

procedure TFmain.selectMap(num:PMapType);
var lat,lon:real;
    i:integer;
begin
 lat:=Y2Lat(mHd2);
 Lon:=X2Lon(mWd2);
 if not(PMapType(num).asLayer) then
  begin
   PMapType(sat_map_both).TBItem.Checked:=false;
   PMapType(sat_map_both).NItem.Checked:=false;
   PMapType(sat_map_both).active:=false;
   sat_map_both:=num;
   TBSMB.ImageIndex:=TMapType(Pointer(sat_map_both)^).TBItem.ImageIndex;
   PMapType(sat_map_both).TBItem.Checked:=true;
   PMapType(sat_map_both).NItem.Checked:=true;
   PMapType(sat_map_both).active:=true;
   if Showmapname then TBSMB.Caption:=PMapType(sat_map_both).name
                  else TBSMB.Caption:='';
  end else
  begin
   PMapType(num).active:=not(PMapType(num).active);
   For i:=0 to length(MapType)-1 do
    if maptype[i].asLayer then
    begin
     MapType[i].TBItem.Checked:=MapType[i].active;
     MapType[i].NItem.Checked:=MapType[i].active;
    end;
  end;
 topos(lat,lon,zoom_size,false);
end;

procedure TFmain.EditGoogleSrchAcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
var s,slat,slon,par:string;
    i,j:integer;
    err:boolean;
    lat,lon:real;
    Buffer:array [1..64535] of char;
    BufferLen:LongWord;
    hSession,hFile:Pointer;
    dwindex, dwcodelen,dwReserv: dword;
    strr:string;
begin
 s:='';
 if InetConnect.userwinset
  then hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0)
  else if InetConnect.proxyused
        then hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_PROXY,PChar(InetConnect.proxystr),nil,0)
        else hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_DIRECT,nil,nil,0);
 if Assigned(hSession)
  then begin
        for i:=1 to length(NewText) do
         if NewText[i]=' ' then NewText[i]:='+';
//        strr:='qweqwe';

        strr:='http://maps.google.com/maps/geo?q='+URLEncode(AnsiToUtf8(NewText))+'&output=xml&hl=ru&key=';
        hFile:=InternetOpenUrl(hSession,PChar(strr),PChar(par),length(par),INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_RELOAD,0);
        dwcodelen:=SizeOf(dwindex);
        if not(InternetQueryOption(hFile, INTERNET_OPTION_HANDLE_TYPE,@dwindex, dwcodelen)) then
         begin
        	ShowMessage(SAS_ERR_Noconnectionstointernet);
          InternetCloseHandle(hFile);
          InternetCloseHandle(hSession);
          exit;
         end;
        if (not InetConnect.userwinset)and(InetConnect.uselogin) then
         begin
          err:=InternetSetOption (hFile, INTERNET_OPTION_PROXY_USERNAME,PChar(InetConnect.loginstr), length(InetConnect.loginstr));
          err:=InternetSetOption (hFile, INTERNET_OPTION_PROXY_PASSWORD,PChar(InetConnect.passstr), length(InetConnect.Passstr));
          if (not(err))or(HttpSendRequest(hFile, nil, 0,Nil, 0)) then //Неверные пароль логин
           begin
           	ShowMessage(SAS_ERR_Authorization);
            InternetCloseHandle(hFile);
            InternetCloseHandle(hSession);
            exit;
           end;
          HttpQueryInfo(hFile,HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER,@dwindex, dwcodelen, dwReserv);
          if (dwindex = HTTP_STATUS_PROXY_AUTH_REQ) then
           begin
           	ShowMessage(SAS_ERR_Authorization);
            InternetCloseHandle(hFile);
            InternetCloseHandle(hSession);
            exit;
           end;
         end;
        err:=false;
        if Assigned(hFile)then
         begin
          repeat
           err:=not(internetReadFile(hFile,@Buffer,SizeOf(Buffer),BufferLen));
           s:=s+Buffer;
          until (BufferLen=0)and(BufferLen<SizeOf(Buffer))and(err=false);

          if PosEx(AnsiToUtf8('Placemark'),s)<1 then
           begin
            ShowMessage(SAS_STR_notfound);
            exit;
           end;
          i:=PosEx('<address>',s);
          j:=PosEx('</address>',s);
          strr:=Utf8ToAnsi(Copy(s,i+9,j-(i+9)));
          i:=PosEx('<coordinates>',s);
          j:=PosEx(',',s,i+13);
          slon:=Copy(s,i+13,j-(i+13));
          i:=PosEx(',0</coordinates>',s,j);
          slat:=Copy(s,j+1,i-(j+1));
          if slat[1]='\' then delete(slat,1,1);
          if slon[1]='\' then delete(slon,1,1);
          try
           lat:=Fmain.str2r(slat);
           lon:=Fmain.str2r(slon);
          except
           ShowMessage('Ошибка при конвертации координат!'+#13#10+'Возможно отсутствует подключение к интернету,'+#13#10+'или Яндекс изменил формат.');
           exit;
          end;
          Fmain.toPos(lat,lon,zoom_size,true);
          ShowMessage(SAS_STR_foundplace+' "'+strr+'"');
         end
        else ShowMessage(SAS_ERR_Noconnectionstointernet);
       end
  else ShowMessage(SAS_ERR_Noconnectionstointernet);
{ ld:=1;
 if InetConnect.proxyused then
  try
   EmbeddedWB1_.ProxySettings.Address:=copy(InetConnect.proxystr,1,PosEx(':',InetConnect.proxystr)-1);
   EmbeddedWB1_.ProxySettings.Port:=strtoint(copy(InetConnect.proxystr,PosEx(':',InetConnect.proxystr)+1,length(InetConnect.proxystr)-PosEx(':',InetConnect.proxystr)));
  except
   ShowMessage(SAS_ERR_ProxyStrFormat);
  end;
  asd
 EmbeddedWB1_.Navigate('http://maps.google.ru/maps?f=q&hl=ru&geocode=&q='+NewText);  }
end;

procedure TFmain.TBSubmenuItem1Click(Sender: TObject);
begin
 FGoTo.ShowModal;
// Visible:=true;
// Fmain.Enabled:=false;
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
 zoom_line:=TMenuItem(Sender).Tag;
 generate_im(nilLastLoad,'');
end;

procedure TFmain.NShowGranClick(Sender: TObject);
var i:integer;
begin
 if zoom_line=0 then NShowGran.Items[0].Checked:=true;
 if zoom_line=99 then NShowGran.Items[1].Checked:=true;
 NShowGran.Items[1].Caption:=SAS_STR_activescale+' (х'+inttostr(zoom_size)+')';
 for i:=2 to 7 do
  if zoom_size+i-2<24 then begin
                            NShowGran.Items[i].Caption:=SAS_STR_for+' х'+inttostr(zoom_size+i-2);
                            NShowGran.Items[i].Visible:=true;
                            NShowGran.Items[i].Tag:=zoom_size+i-2;
                            if NShowGran.Items[i].Tag=zoom_line then NShowGran.Items[i].Checked:=true
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
 if sender is TTBitem then NGPSconn.Checked:=TTBitem(sender).Checked
                      else TBGPSconn.Checked:=TMenuItem(sender).Checked;
 LayerMapGPS.Visible:=NGPSconn.Checked;
 GPS_enab:=TBGPSconn.Checked;
 if GPS_enab then
  begin
   GPSReceiver.Delay:=GPS_update;
   GPSReceiver.ConnectionTimeout:=GPS_timeout;
   GPSReceiver.Port :=  GPSReceiver.StringToCommPort(GPS_COM);
   GPSReceiver.BaudRate := GPSReceiver.IntToBaudRate(BaudRate);
   GPSReceiver.NeedSynchronization:=true;
   try
    GPSReceiver.Open;
   except
    ShowMessage(SAS_ERR_PortOpen);
    GPSReceiverDisconnect(nil,TCommPort(nil));
   end;
  end
  else GPSReceiver.Close;
end;

procedure TFmain.TBGPSPathClick(Sender: TObject);
begin
 if sender is TTBSubMenuitem then NGPSPath.Checked:=TTBitem(sender).Checked
                             else TBGPSPath.Checked:=TMenuItem(sender).Checked;
 GPS_path:=TBGPSPath.Checked;
// if not(GPS_path) then setlength(GPS_arr,0);
end;

procedure TFmain.TBGPSToPointClick(Sender: TObject);
begin
 if sender is TTBitem then NGPSToPoint.Checked:=TTBitem(sender).Checked
                      else TBGPSToPoint.Checked:=TMenuItem(sender).Checked;
 GPS_go:=TBGPSToPoint.Checked;
end;

procedure TFmain.TBItem4Click(Sender: TObject);
begin
 FSelLonLat:= TFSelLonLat.Create(Self);
        Try
          if FSelLonLat.Execute Then
             Begin
              fsaveas.Show_(zoom_size,[ExtPoint(FSelLonLat._lon_k,FSelLonLat._lat_k),ExtPoint(FSelLonLat.lon_k,FSelLonLat._lat_k),ExtPoint(FSelLonLat.lon_k,FSelLonLat.lat_k),ExtPoint(FSelLonLat._lon_k,FSelLonLat.lat_k),ExtPoint(FSelLonLat._lon_k,FSelLonLat._lat_k)]);
             End;
        Finally
          FSelLonLat.Free;
        End;
 TBmoveClick(Sender);
end;

procedure TFmain.ShowstatusClick(Sender: TObject);
begin
 LayerStatBar.Visible:= Showstatus.Checked;
 mapResize(Sender);
end;

procedure TFmain.ShowMiniMapClick(Sender: TObject);
begin
 LayerMinMap.Visible:= ShowMiniMap.Checked;
 if LayerMinMap.Visible then LayerMinMap.BringToFront
                        else LayerMinMap.SendToBack;
 sm_im_reset(sm_map.width div 2,sm_map.height div 2);
end;

procedure TFmain.ShowLineClick(Sender: TObject);
begin
 LayerLineM.visible:=ShowLine.Checked;
end;

procedure TFmain.NMMtype_0Click(Sender: TObject);
begin
 if TMenuItem(sender).Tag=0 then begin
                                  sm_map.MapType.ShowOnSmMap:=false;
                                  sm_map.MapType.NSmItem.Checked:=false;
                                  sm_map.maptype:=0;
                                  NMMtype_0.Checked:=true;
                                 end
 else
 if PMapType(TMenuItem(sender).Tag).asLayer then
   begin
    PMapType(TMenuItem(sender).Tag).ShowOnSmMap:=not(PMapType(TMenuItem(sender).Tag).ShowOnSmMap);
    TMenuItem(sender).Checked:=PMapType(TMenuItem(sender).Tag).ShowOnSmMap;
   end else
   begin
    NMMtype_0.Checked:=false;
    if longint(sm_map.maptype)<>0 then begin
                                        sm_map.MapType.ShowOnSmMap:=false;
                                        sm_map.MapType.NSmItem.Checked:=false;
                                       end;
    sm_map.maptype:=PMapType(TMenuItem(sender).Tag);
    sm_map.maptype.NSmItem.Checked:=true;
    sm_map.maptype.ShowOnSmMap:=true;
   end;
 sm_im_reset(sm_map.width div 2,sm_map.height div 2);
end;

procedure TFmain.N32Click(Sender: TObject);
begin
 LayerMapScale.Visible:=TMenuItem(sender).Checked;
 if LayerMapScale.Visible then LayerMapScale.BringToFront
                          else LayerMapScale.SendToBack;
end;

procedure TFmain.TBItem3Click(Sender: TObject);
var F:TextFile;
    i:integer;
    SaveDlg: TSaveDialog;
begin
Fprogress2.Visible:=true;
fprogress2.MemoInfo.Lines[0]:=SAS_STR_savetreck;
Fprogress2.ProgressBar1.Max:=100;
SaveDlg := TSaveDialog.Create(Fprogress2);
SaveDlg.DefaultExt:='*.kml';
SaveDlg.Filter:='KML|*.kml';
Fprogress2.ProgressBar1.Progress1:=0;
if (SaveDlg.Execute)and(SaveDlg.FileName<>'') then
 begin
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
  for i:=0 to length(GPS_arr)-1 do
   Writeln(f,R2strPoint(GPS_arr[i].x),',',R2strPoint(GPS_arr[i].y),',0');
  Writeln(f,' </coordinates>');
  Fprogress2.ProgressBar1.Progress1:=90;
	Writeln(f,'</LineString>');
  Writeln(f,'</Placemark>');
	Writeln(f,'</Folder>'+#13#10+'</kml>');
  CloseFile(f);
 end;
 Fprogress2.ProgressBar1.Progress1:=100;
 Fprogress2.Visible:=false;
 SaveDlg.Free;
end;

procedure TFmain.TBItem5Click(Sender: TObject);
begin
 if length(GPS_arr)>1 then begin
                            if FaddLine.show_(GPS_arr,true) then
                             begin
                              setalloperationfalse(movemap);
                              generate_im(nilLastLoad,'');
                             end; 
                           end
                      else ShowMessage(SAS_ERR_Nopoints);
end;

procedure TFmain.Google1Click(Sender: TObject);
var Apos:tExtendedPoint;
begin
 Apos:=GPos2LonLat(pos,zoom_size,sat_map_both);
 CopyStringToClipboard('http://maps.google.com/?ie=UTF8&ll='+R2StrPoint(Apos.y)+','+R2StrPoint(Apos.x)+'&spn=57.249013,100.371094&t=h&z='+inttostr(zoom_size-1));
end;

procedure TFmain.YaLinkClick(Sender: TObject);
var Apos,AposLT,AposRD:tExtendedPoint;
begin
 Apos:=GPos2LonLat(pos,zoom_size,sat_map_both);
 AposLT:=GPos2LonLat(Point(pos.X-mWd2,pos.Y-mHd2),zoom_size,sat_map_both);
 AposRD:=GPos2LonLat(Point(pos.X+mWd2,pos.Y+mHd2),zoom_size,sat_map_both);
 CopyStringToClipboard('http://beta-maps.yandex.ru/?ll='+R2StrPoint(round(Apos.x*100000)/100000)+'%2C'+R2StrPoint(round(Apos.y*100000)/100000)+'&spn='+R2StrPoint(abs(AposLT.x-AposRD.x))+'%2C'+R2StrPoint(abs(AposLT.y-AposRD.y))+'&l=sat');
end;

procedure TFmain.kosmosnimkiru1Click(Sender: TObject);
var Apos:tExtendedPoint;
    s:string;
begin
 Apos:=GPos2LonLat(pos,zoom_size,sat_map_both);
 CopyStringToClipboard('http://kosmosnimki.ru/?x='+R2StrPoint(Apos.x)+'&y='+R2StrPoint(Apos.y)+'&z='+inttostr(zoom_size-1)+'&fullscreen=false&mode=satellite');
end;

procedure TFmain.mapResize(Sender: TObject);
begin
 if (close_<>true)and(not(start))then
  begin
   OldFormWH:=point(mWd2,mHd2);
   mWd2:=map.Width shr 1;
   mHd2:=map.Height shr 1;
   LayerStatBar.Location:=floatrect(0,map.Height-17,map.Width,map.Height);
   if LayerStatBar.Visible
    then begin
          LayerMinMap.Location:=floatrect(map.Width-sm_map.width-5,map.Height-sm_map.height-17,map.Width,map.Height-17);
          with LayerLineM do location:=floatrect(location.left,map.Height-23-17,location.right,map.Height-8-17);
         end
    else begin
          LayerMinMap.Location:=floatrect(map.Width-sm_map.width-5,map.Height-sm_map.height,map.Width,map.Height);
          with LayerLineM do location:=floatrect(location.left,map.Height-23,location.right,map.Height-8);
         end;
   LayerMap.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
   FillingMap.LayerMap.Location:=LayerMap.Location;
   LayerMapNal.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
   LayerMapMarks.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
   LayerMapGPS.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
   LayerMapWiki.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
   LayerMapScale.location:=floatrect(bounds(mWd2-145,mHd2-145,290,290));
   toSh;
   sm_im_reset(sm_map.width div 2,sm_map.height div 2)
  end;
end;

procedure TFmain.TBLoadSelFromFileClick(Sender: TObject);
var ini:TIniFile;
    i:integer;
begin
 fsaveas.Visible:=true;
 if (OpenDialog1.Execute)and(OpenDialog1.FileName<>'') then
  begin
   ini:=TIniFile.Create(OpenDialog1.FileName);
   fprogress2.MemoInfo.Lines[0]:=SAS_STR_loadhl;
   i:=1;
   while Ini.ReadFloat('HIGHLIGHTING','PointLon_'+inttostr(i),2147483647)<>2147483647 do
    begin
     setlength(poly_save,i);
     poly_save[i-1].x:=Ini.ReadFloat('HIGHLIGHTING','PointLon_'+inttostr(i),2147483647);
     poly_save[i-1].y:=Ini.ReadFloat('HIGHLIGHTING','PointLat_'+inttostr(i),2147483647);
     inc(i);
    end;
   if length(poly_save)>0 then
    begin
     poly_zoom_save:=Ini.Readinteger('HIGHLIGHTING','zoom',1);
     fsaveas.Show_(poly_zoom_save,poly_save);
    end;
  end
 else fsaveas.Visible:=false;
end;

procedure TFmain.TBEditItem1AcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
var s,slat,slon,par:string;
    i,j:integer;
    err:boolean;
    lat,lon:real;
    Buffer:array [1..64535] of char;
    BufferLen:LongWord;
    hSession,hFile:Pointer;
    dwindex, dwcodelen,dwReserv: dword;
begin
 s:='';
 if InetConnect.userwinset
  then hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0)
  else if InetConnect.proxyused
        then hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_PROXY,PChar(InetConnect.proxystr),nil,0)
        else hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_DIRECT,nil,nil,0);
 if Assigned(hSession)
  then begin
        hFile:=InternetOpenURL(hSession,PChar('http://maps.yandex.ru/?text='+URLEncode(AnsiToUtf8(NewText))),PChar(par),length(par),INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_RELOAD,0);
        dwcodelen:=SizeOf(dwindex);
        if not(InternetQueryOption(hFile, INTERNET_OPTION_HANDLE_TYPE,@dwindex, dwcodelen)) then
         begin
        	ShowMessage(SAS_ERR_Noconnectionstointernet);
          InternetCloseHandle(hFile);
          InternetCloseHandle(hSession);
          exit;
         end;
        if (not InetConnect.userwinset)and(InetConnect.uselogin) then
         begin
          err:=InternetSetOption (hFile, INTERNET_OPTION_PROXY_USERNAME,PChar(InetConnect.loginstr), length(InetConnect.loginstr));
          err:=InternetSetOption (hFile, INTERNET_OPTION_PROXY_PASSWORD,PChar(InetConnect.passstr), length(InetConnect.Passstr));
          if (not(err))or(HttpSendRequest(hFile, nil, 0,Nil, 0)) then //Неверные пароль логин
           begin
           	ShowMessage(SAS_ERR_Authorization);
            InternetCloseHandle(hFile);
            InternetCloseHandle(hSession);
            exit;
           end;
          HttpQueryInfo(hFile,HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER,@dwindex, dwcodelen, dwReserv);
          if (dwindex = HTTP_STATUS_PROXY_AUTH_REQ) then
           begin
           	ShowMessage(SAS_ERR_Authorization);
            InternetCloseHandle(hFile);
            InternetCloseHandle(hSession);
            exit;
           end;
         end;
        err:=false;
        if Assigned(hFile)then
         begin
          repeat
           err:=not(internetReadFile(hFile,@Buffer,SizeOf(Buffer),BufferLen));
           s:=s+Buffer;
          until (BufferLen=0)and(BufferLen<SizeOf(Buffer))and(err=false);

          if PosEx(AnsiToUtf8('Искомая комбинация'),s)>0 then
           begin
            ShowMessage(SAS_STR_notfound);
            exit;
           end;
          i:=PosEx('this,[',s);
          j:=PosEx(',',s,i+6);
          slon:=Copy(s,i+6,j-(i+6));
          i:=PosEx(']',s,j);
          slat:=Copy(s,j+1,i-(j+1));
          if slat[1]='\' then delete(slat,1,1);
          if slon[1]='\' then delete(slon,1,1);
          try
           lat:=Fmain.str2r(slat);
           lon:=Fmain.str2r(slon);
          except
           ShowMessage('Ошибка при конвертации координат!'+#13#10+'Возможно отсутствует подключение к интернету,'+#13#10+'или Яндекс изменил формат.');
           exit;
          end;
          Fmain.toPos(lat,lon,zoom_size,true);
          ShowMessage(SAS_STR_foundplace+' "'+NewText+'"');
         end
        else ShowMessage(SAS_ERR_Noconnectionstointernet);
       end
  else ShowMessage(SAS_ERR_Noconnectionstointernet);
end;

procedure TFmain.PopupMenu1Popup(Sender: TObject);
var i:Integer;
begin
 ldm.Visible:=false;
 dlm.Visible:=false;
 For i:=0 to length(MapType)-1 do
  if (MapType[i].asLayer) then
   begin
    MapType[i].NDwnItem.Visible:=MapType[i].active;
    MapType[i].NDelItem.Visible:=MapType[i].active;
    if MapType[i].active then begin
                                ldm.Visible:=true;
                                dlm.Visible:=true;
                              end
   end;
end;

procedure TFmain.ShowErrScript(DATA:string);
begin
 ShowMessage(data);
end;

procedure TFmain.NinvertcolorClick(Sender: TObject);
begin
 invertcolor:=Ninvertcolor.Checked;
 generate_im(nilLastLoad,''); 
end;

procedure TFmain.mapDblClick(Sender: TObject);
var r:TPoint;
begin
 dwn:=false;
 if (aoper=movemap){and(dblclik=true)} then
  begin
   r:=map.ScreenToClient(Mouse.CursorPos);
   POS:=Point(pos.x+(r.x-mWd2),pos.y+(r.y-mHd2));
   generate_im(nilLastLoad,'');
  end;
// map.Enabled:=false;
// map.Enabled:=true;
// Screen.ActiveForm.SetFocusedControl(map);

// dwn:=false;
// map.Refresh;
// dblclik:=true;
end;

procedure TFmain.TBAdd_PointClick(Sender: TObject);
begin
 setalloperationfalse(add_point);
end;

procedure TFmain.TBAdd_LineClick(Sender: TObject);
begin
 setalloperationfalse(add_Line);
end;

procedure TFmain.TBAdd_PolyClick(Sender: TObject);
begin
 setalloperationfalse(add_poly);
end;

procedure TFmain.NMarkEditClick(Sender: TObject);
begin
 MouseOnReg(PWL,Point(moveTrue.x+(pr_x-mWd2),moveTrue.y+(pr_y-mHd2)));
 if EditMark(strtoint(PWL.numid)) then generate_im(nilLastLoad,'');
end;

procedure TFmain.NMarkDelClick(Sender: TObject);
begin
 MouseOnReg(PWL,Point(moveTrue.x+(pr_x-mWd2),moveTrue.y+(pr_y-mHd2)));
 if DeleteMark(StrToInt(PWL.numid),Fmain.Handle) then
  generate_im(nilLastLoad,'');
end;

procedure TFmain.NMarksBarShowClick(Sender: TObject);
begin
 TBMarksToolBar.Visible:=NMarksBarShow.Checked;
end;

procedure TFmain.NMarkOperClick(Sender: TObject);
begin
 MouseOnReg(PWL,Point(moveTrue.x+(pr_x-mWd2),moveTrue.y+(pr_y-mHd2)));
 OperationMark(strtoint(PWL.numid));
end;

procedure TFmain.livecom1Click(Sender: TObject);
var Apos:tExtendedPoint;
begin
 Apos:=GPos2LonLat(pos,zoom_size,sat_map_both);
 CopyStringToClipboard('http://maps.live.com/default.aspx?v=2&cp='+R2StrPoint(Apos.y)+'~'+R2StrPoint(Apos.x)+'&style=h&lvl='+inttostr(zoom_size-1));
end;

procedure TFmain.N13Click(Sender: TObject);
begin
 CopyStringToClipboard(sat_map_both.GetLink(X2absX(pos.x-(mWd2-move.x),zoom_size),pos.y-(mHd2-move.y),zoom_size));
end;

procedure TFmain.ImageAtlas1Click(Sender: TObject);
var Apos:tExtendedPoint;
begin
 Apos:=GPos2LonLat(pos,zoom_size,sat_map_both);
 CopyStringToClipboard('http://imageatlas.digitalglobe.com/ia-webapp/?lat='+R2StrPoint(Apos.y)+'&lon='+R2StrPoint(Apos.x)+'&zoom='+inttostr(zoom_size-1));
end;

procedure TFmain.DigitalGlobe1Click(Sender: TObject);
{var s,slat,slon,par:string;
    i,j,ls:integer;
    err:boolean;
    lat,lon:real;
    Buffer:array [1..64535] of char;
    BufferLen:LongWord;
    hSession,hFile:Pointer;
    dwindex, dwcodelen,dwReserv: dword;
    Apos:tExtendedPoint;
    mpp:extended;
    hi,wi:integer;
begin
 Apos:=GPos2LonLat(Point(pos.x-(mWd2-move.x),pos.y-(mHd2-move.y)),zoom_size,sat_map_both);
 hi:=20;
 wi:=20;
 mpp:=1;
 TextToWebBrowser(SAS_STR_WiteLoad,Fbrowser.EmbeddedWB1);
 Fbrowser.Visible:=true;

 s:='';

 if InetConnect.userwinset
  then hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0)
  else if InetConnect.proxyused
        then hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_PROXY,PChar(InetConnect.proxystr),nil,0)
        else hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_DIRECT,nil,nil,0);
 if Assigned(hSession)
  then
   for ls:=1 to 53 do
    if lsAndid[ls,2]<>0 then
        begin
        s:=s+'<BR><b>Для параметра l = '+inttostr(lsAndid[ls,1])+' и id = '+inttostr(lsAndid[ls,2])+'</b><BR>';
        hFile:=InternetOpenURL(hSession,PChar('http://image.globexplorer.com/gexservlets/gex?cmd=info&id='+inttostr(lsAndid[ls,2])+'&appid=020100S&ls='+inttostr(lsAndid[ls,1])+'&xc='+R2StrPoint(Apos.x)+'&yc='+R2StrPoint(Apos.y)+'&mpp='+R2StrPoint(mpp)+'&iw='+inttostr(wi)+'&ih='+inttostr(hi)+'&extentset=all'),PChar(par),length(par),INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_RELOAD,0);
        dwcodelen:=SizeOf(dwindex);
        if not(InternetQueryOption(hFile, INTERNET_OPTION_HANDLE_TYPE,@dwindex, dwcodelen)) then
         begin
        	ShowMessage(SAS_ERR_Noconnectionstointernet);
          InternetCloseHandle(hFile);
          InternetCloseHandle(hSession);
          exit;
         end;
        if (not InetConnect.userwinset)and(InetConnect.uselogin) then
         begin
          err:=InternetSetOption (hFile, INTERNET_OPTION_PROXY_USERNAME,PChar(InetConnect.loginstr), length(InetConnect.loginstr));
          err:=InternetSetOption (hFile, INTERNET_OPTION_PROXY_PASSWORD,PChar(InetConnect.passstr), length(InetConnect.Passstr));
          if (not(err))or(HttpSendRequest(hFile, nil, 0,Nil, 0)) then //Неверные пароль логин
           begin
           	ShowMessage(SAS_ERR_Authorization);
            InternetCloseHandle(hFile);
            InternetCloseHandle(hSession);
            exit;
           end;
          HttpQueryInfo(hFile,HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER,@dwindex, dwcodelen, dwReserv);
          if (dwindex = HTTP_STATUS_PROXY_AUTH_REQ) then
           begin
           	ShowMessage(SAS_ERR_Authorization);
            InternetCloseHandle(hFile);
            InternetCloseHandle(hSession);
            exit;
           end;
         end;
        err:=false;
        if Assigned(hFile)then
         begin
          repeat
           err:=not(internetReadFile(hFile,@Buffer,SizeOf(Buffer),BufferLen));
           s:=s+copy(Buffer,0,BufferLen);
          until (BufferLen=0)and(BufferLen<SizeOf(Buffer))and(err=false);
         end
        else ShowMessage(SAS_ERR_Noconnectionstointernet);
       end  else
  else ShowMessage(SAS_ERR_Noconnectionstointernet);
 TextToWebBrowser(s,Fbrowser.EmbeddedWB1); }
var Apos:tExtendedPoint;
    mpp:extended;
    hi,wi:integer;
    encrypt:string;
begin
 Apos:=GPos2LonLat(Point(pos.x-(mWd2-move.x),pos.y-(mHd2-move.y)),zoom_size,sat_map_both);
 mpp:=1/((zoom[zoom_size]/(2*PI))/(PMapType(sat_map_both).radiusa*cos(APos.y*deg)));
 hi:=round(mpp*15);
 wi:=round(mpp*15);
 if hi>3500 then hi:=3500;
 if wi>3500 then wi:=3500;
 if hi<mHd2*2 then hi:=256;
 if wi<mWd2*2 then wi:=256;
 if mpp>8 then mpp:=8;
 TextToWebBrowser(SAS_STR_WiteLoad,Fbrowser.EmbeddedWB1);
 Fbrowser.Visible:=true;
 encrypt:= Encode64(EncodeDG('cmd=info&id=ca4046dd-bba5-425c-8966-0a553e0deb3a&appid=020100S&ls=49&xc='+R2StrPoint(Apos.x)+'&yc='+R2StrPoint(Apos.y)+'&mpp='+R2StrPoint(mpp)+'&iw='+inttostr(wi)+'&ih='+inttostr(hi)+'&extentset=all'));
 Fbrowser.EmbeddedWB1.Navigate('http://image.globexplorer.com/gexservlets/gex?encrypt='+encrypt);
 //Fbrowser.EmbeddedWB1.Navigate('http://image.globexplorer.com/gexservlets/gex?cmd=info&id=227400001&appid=020100S&ls=19&xc='+R2StrPoint(Apos.x)+'&yc='+R2StrPoint(Apos.y)+'&mpp='+R2StrPoint(mpp)+'&iw='+inttostr(wi)+'&ih='+inttostr(hi)+'&extentset=all');
 Fbrowser.EmbeddedWB1.DocumentSourceText;
end;

procedure TFmain.mapMouseLeave(Sender: TObject);
begin
 if (h<>nil) then
  begin
   H.ReleaseHandle;
   FreeAndNil(H);
//   h:=nil;
   oldLayerIndex:=0;
  end;
end;

procedure TFmain.GPSReceiver1SatellitesReceive(Sender: TObject);
begin
 if FSettings.Visible then FSettings.PaintBox1.Repaint;
end;

procedure TFmain.GPSReceiverReceive(Sender: TObject);
var s2f,sb:string;
    len:integer;
    bPos:TPoint;
begin
  if (GPSReceiver.IsFix=0)or((GPSReceiver.GetLongitudeAsDecimalDegrees=0)and(GPSReceiver.GetLatitudeAsDecimalDegrees=0)) then exit;
  setlength(GPS_arr,length(GPS_arr)+1);
  len:=length(GPS_arr);
  GPS_arr[len-1]:=ExtPoint(GPSReceiver.GetLongitudeAsDecimalDegrees+GPS_popr.x,GPSReceiver.GetLatitudeAsDecimalDegrees+GPS_popr.y);
  setlength(GPS_arr_speed,len);
  GPS_arr_speed[len-1]:=FMain.GPSReceiver.GetSpeed_KMH;
  if len>1 then
    GPSpar.len:=GPSpar.len+Fmain.find_length(GPS_arr[len-2].y,GPS_arr[len-1].y,
                GPS_arr[length(GPS_arr)-2].x,GPS_arr[len-1].x);
  if not((dwn)or(anim_zoom=1))and(Fmain.Active) then
   begin
    bPOS:=GLonLat2pos(ExtPoint(GPS_arr[len-1].X,GPS_arr[len-1].Y),zoom_size,sat_map_both);
    if (GPS_go)and((bpos.X<>pos.x)or(bpos.y<>pos.y))
     then begin
           POS:=bpos;
           generate_im(nilLastLoad,'');
          end
     else begin
           drawLineGPS;
           toSh;
          end;
   end;
  if GPS_Log then
   begin
    if length(GPS_arr)=1 then sb:='1' else sb:='0';
    s2f:=R2StrPoint(round(GPS_arr[len-1].y*10000000)/10000000)+','+R2StrPoint(round(GPS_arr[len-1].x*10000000)/10000000)+','+sb+','+'-777'+','+
                    floattostr(Double(Date))+'.'+inttostr(round(Double(GetTime)*1000000))+','+DateToStr(Date)+','+TimeToStr(GetTime);
    Writeln(GPS_logfile,s2f);
   end;
end;

procedure TFmain.GPSReceiverDisconnect(Sender: TObject;
  const Port: TCommPort);
begin
 try
 if GPS_Log then CloseFile(GPS_LogFile);
 except
 end;
 //setlength(GPS_arr,0);
 //setlength(GPS_arr_speed,0);
 LayerMapGPS.Bitmap.Clear(clBlack);
 GPS_enab:=false;
 LayerMapGPS.Visible:=false;
 NGPSconn.Checked:=false;
 TBGPSconn.Checked:=false;
end;

procedure TFmain.GPSReceiverConnect(Sender: TObject; const Port: TCommPort);
var S:string;
begin
 if GPS_Log then
 try
  TimeSeparator:='-';
  DateSeparator:='-';
  CreateDir(ExtractFilePath(paramstr(0))+'TRECKLOG');
  s:=ExtractFilePath(paramstr(0))+'TRECKLOG\'+DateToStr(Date)+'-'+TimeToStr(GetTime)+'.plt';
  AssignFile(GPS_LogFile,s);
  rewrite(GPS_LogFile);
  TimeSeparator:=':';
  DateSeparator:='.';
 except
  GPS_Log:=false;
 end;
end;

procedure TFmain.GPSReceiverTimeout(Sender: TObject);
begin
 ShowMessage(SAS_ERR_Communication);
// GPSReceiver.close;
// GPSReceiverDisconnect(nil,TCommPort(nil));
end;

procedure TFmain.NMapParamsClick(Sender: TObject);
begin
 FEditMap.AmapType:=sat_map_both;
 FEditMap.ShowModal;
end;

procedure TFmain.LayerMinMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ll,lt:integer;
begin
 map.PopupMenu:=nil;
 case button of
   mbRight: map.PopupMenu:=PopupMSmM;
   mbLeft: begin
            ll:=round(LayerMinMap.Location.Left);
            lt:=round(LayerMinMap.Location.top);
            if (x<ll+5)
             then sm_map.size_dw:=true
             else if (x>ll+6)and(x<ll+17)and(y>lt+5)and(y<lt+15)
                   then begin
                         sm_map.zooming:=true;
                         if sm_map.z1mz2>1 then dec(sm_map.z1mz2);
                         sm_im_reset(sm_map.width div 2,sm_map.height div 2);
                        end
                   else if (x>ll+19)and(x<ll+33)and(y>lt+5)and(y<lt+15)
                         then begin
                               sm_map.zooming:=true;
                               if zoom_size-sm_map.z1mz2>1 then inc(sm_map.z1mz2);
                               sm_im_reset(sm_map.width div 2,sm_map.height div 2);
                              end
                         else if (x>ll+5)and(y>lt) then
                               begin
                                sm_map.m_dwn:=true;
                                sm_im_reset(round(x-(LayerMinMap.Location.Left+5)),round(y-(LayerMinMap.Location.top)));
                               end;
           end;
 end;
end;

procedure TFmain.LayerMinMapMouseUp(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
 if (anim_zoom=1) then exit;
 sm_map.m_dwn:=false;
 sm_map.size_dw:=false;
 if (not(sm_map.size_dw))and(not(sm_map.zooming))and((x>LayerMinMap.Location.Left+5)and(y>LayerMinMap.Location.Top))
  then begin
        if sm_map.zoom>1 then
         pos:=Point(pos.x+round((-(128-(sm_map.pos.x*(256/sm_map.width))))*power(2,zoom_size-sm_map.zoom)),
              pos.y+round((-(128-(sm_map.pos.y*(256/sm_map.height))))*power(2,zoom_size-sm_map.zoom)))
         else pos:=Point(round(sm_map.pos.X*(256/sm_map.height)*power(2,zoom_size-sm_map.zoom)),round((sm_map.pos.Y*(256/sm_map.height))*power(2,zoom_size-sm_map.zoom)));
        Fmain.generate_im(nilLastLoad,'');
       end
  else sm_map.zooming:=false;
 toSh;
end;

procedure TFmain.LayerMinMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (x<LayerMinMap.Location.Left+5)and(map.Cursor<>crSizeNWSE) then LayerMinMap.Cursor:=crSizeNWSE
                                                               else LayerMinMap.Cursor:=crHandPoint;
 if (sm_map.size_dw)and((map.Width-x-5)>40)
  then begin
        sm_map.width:=(map.Width-x-5);
        sm_map.height:=(map.Width-x-5);
        sm_im_reset(sm_map.width div 2,sm_map.height div 2)
       end;
 if (sm_map.m_dwn)and(x>LayerMinMap.Location.Left+5)and(y>LayerMinMap.Location.top+5)
  then sm_im_reset(round(x-(LayerMinMap.Location.Left+5)),round(y-(LayerMinMap.Location.top)));
end;

procedure TFmain.mapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var ll,lt,i:integer;
begin
 if (layer=LayerMinMap) then exit;
 if (ssDouble in Shift)or(anim_zoom=1)or(button=mbMiddle)or(HiWord(GetKeyState(VK_DELETE))<>0)
    or(HiWord(GetKeyState(VK_INSERT))<>0) then exit;
 Screen.ActiveForm.SetFocusedControl(map);
 Layer.Cursor:=curBuf;
 if (Button=mbLeft)and(aoper<>movemap) then
  begin
   if (aoper=line)then begin
                  setlength(length_arr,length(length_arr)+1);
                  length_arr[length(length_arr)-1]:=extpoint(X2Lon(X),Y2Lat(Y));
                  drawLineCalc;
                 end;
   if (aoper=Reg) then begin
                  setlength(reg_arr,length(reg_arr)+1);
                  reg_arr[length(reg_arr)-1]:=extpoint(X2Lon(X),Y2Lat(Y));
                  drawReg;
                 end;
   if (aoper=rect)then begin
                  if rect_dwn then begin
                                    rect_arr[1]:=extPoint(X2Lon(x),Y2Lat(y));
                                    rect_p2:=true;
                                   end
                              else begin
                                    rect_arr[0]:=extPoint(X2Lon(x),Y2Lat(y));
                                    rect_arr[1]:=rect_arr[0];
                                   end;
                  rect_dwn:=not(rect_dwn);
                  drawRect(Shift);
                 end;
   if (aoper=add_point)and(FAddPoint.show_(extPoint(X2Lon(x),Y2Lat(y)),true)) then generate_im(nilLastLoad,'');
   if (aoper in [add_line,add_poly]) then
      begin
        for i:=0 to length(add_line_arr)-1 do
         if (X<Lon2x(add_line_arr[i].x)+5)and(X>Lon2x(add_line_arr[i].x)-5)and
            (Y<Lat2y(add_line_arr[i].y)+5)and(Y>Lat2y(add_line_arr[i].y)-5)
            then begin
                  movepoint:=i;
                  lastpoint:=i;
                  drawPath(add_line_arr,true,SetAlpha(ClRed32, 150),SetAlpha(ClWhite32, 50),3,aoper=add_poly);
                  exit;
                 end;
        inc(lastpoint);
        movepoint:=lastpoint;
        insertinpath(lastpoint);
        add_line_arr[lastpoint]:=extpoint(X2Lon(X),Y2Lat(Y));
        drawPath(add_line_arr,true,SetAlpha(ClRed32, 150),SetAlpha(ClWhite32, 50),3,aoper=add_poly);
      end;
   exit;
  end;
 if dwn then exit;
 if (Button=mbright)and(aoper=movemap)and(not dwn)
  then begin
        m_up:=point(x,y);
        PWL.find:=false;
        PWL.S:=0;
        MouseOnMyReg(PWL,Point(x,y));
        NMarkEdit.Visible:=PWL.find;
        NMarkDel.Visible:=PWL.find;
        NMarkSep.Visible:=PWL.find;
        NMarkOper.Visible:=PWL.find;
        map.PopupMenu:=PopupMenu1;
       end
  else begin
        dwn:=true;
        map.PopupMenu:=nil;
       end;
 move:=Point(x,y);
end;

procedure TFmain.mapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var PWL:TResObj;
    stw,path:String;
begin
 if (layer=LayerMinMap) then exit;
 if (ssDouble in Shift) then exit;
 dwn:=false;
 if HiWord(GetKeyState(VK_DELETE))<>0 then
  begin
   DelFile(ffpath(Pos.x-(mWd2-X),Pos.y-(mHd2-y),zoom_size,sat_map_both^,false));
   generate_im(nilLastLoad,'');
   exit;
  end;
 if HiWord(GetKeyState(VK_INSERT))<>0 then
  begin
   path:=ffpath(Pos.x-(mWd2-x),Pos.y-(mHd2-y),zoom_size,sat_map_both^,false);
  // if ((not(tileExists(path)))or(MessageBox(handle,pchar(SAS_STR_file+' '+path+' '+SAS_MSG_FileExists),pchar(SAS_MSG_coution),36)=IDYES)) then
    with ThreadAllLoadMap.Create(False,[Point(pos.x-(mWd2-x),pos.y-(mHd2-y))],1,true,false,false,true,zoom_size,sat_map_both,date) do
     begin
      FreeOnTerminate:=true;
      OnTerminate:=ThreadDone;
     end;
   exit;
  end;
// btn_visib.Visible:=aoper=line;
// btn_close.Visible:=aoper=line;
// btn_ok.Visible:=(aoper in [add_line,add_poly])and(length(add_line_arr)>1);
// btn_delpath.Visible:=(aoper in [add_line,add_poly])and(length(add_line_arr)>1);

 if movepoint>-1 then begin movepoint:=-1; end;
 if (((aoper<>movemap)and(Button=mbLeft))or
     ((aoper=movemap)and(Button=mbRight))) then exit;
 m_up:=move;
 if (anim_zoom=1) then exit;
 map.Enabled:=false;
 map.Enabled:=true;
 if button=mbMiddle then
   begin
    TBFullSize.Checked:=not(TBFullSize.Checked);
    TBFullSizeClick(Sender);
    layer.Cursor:=curBuf;
    exit;
   end;
 POS:=Point(pos.x+(move.x-x),pos.y+(move.y-y));
 m_up:=Point(x,y);
 layer.Cursor:=curBuf;
 if ((move.x<>m_up.x)or(move.y<>m_up.y))
  then generate_im(nilLastLoad,'');
// map.Refresh;
 if (y=move.y)and(x=move.x) then
  begin
   toSh;
   paint_Line;
   if aoper=line then drawLineCalc;
   if aoper=reg then drawReg;
   if aoper=rect then drawRect([]);
   if GPS_enab then drawLineGPS;
   if aoper in [add_line,add_poly] then drawPath(add_line_arr,true,setalpha(clRed32,150),setalpha(clWhite32,50),3,aoper=add_poly);
  end;
 if (y=move.y)and(x=move.x)and(aoper=movemap)and(button=mbLeft) then
  begin
    layer.Cursor:=curBuf;
    PWL.S:=0;
    PWL.find:=false;
    if (LayerMapWiki.Visible) then
     MouseOnReg(PWL,Point(x+(pr_x-mWd2),y+(pr_y-mHd2)));
    MouseOnMyReg(PWL,Point(x,y));
    if pwl.find then
     begin
      stw:='<HTML><BODY>';
      stw:=pwl.descr;
      stw:=stw+'</BODY></HTML>';
      TextToWebBrowser(stw,Fbrowser.EmbeddedWB1);
      Fbrowser.Visible:=true;
     end;
    exit;
  end;
end;

function HTML2Char(const s: String): Char; 
var i: Integer; 
begin 
 Result := #0;
  if s <> '' then
  begin i := Length(s);
    if (i > 1) and (i < 7) then 
    begin 
      if s = 'quot'   then Result := '"'; 
      if s = 'amp'    then Result := '&'; 
      if s = 'lt'     then Result := '<'; 
      if s = 'gt'     then Result := '>'; 
      if s = 'nbsp'   then Result := #32; 
      if s = 'iexcl'  then Result := 'Ў'; 
      if s = 'cent'   then Result := 'ў'; 
      if s = 'pound'  then Result := 'Ј'; 
      if s = 'curren' then Result := '¤'; 
      if s = 'yen'    then Result := 'Ґ'; 
      if s = 'brvbar' then Result := '¦'; 
      if s = 'sect'   then Result := '§'; 
      if s = 'uml'    then Result := 'Ё'; 
      if s = 'copy'   then Result := '©'; 
      if s = 'ordf'   then Result := 'Є'; 
      if s = 'laquo'  then Result := '«'; 
      if s = 'not'    then Result := '¬'; 
      if s = 'shy'    then Result := '­'; 
      if s = 'reg'    then Result := '®'; 
      if s = 'macr'   then Result := 'Ї'; 
      if s = 'deg'    then Result := '°'; 
      if s = 'plusmn' then Result := '±'; 
      if s = 'sup2'   then Result := 'І'; 
      if s = 'sup3'   then Result := 'і'; 
      if s = 'acute'  then Result := 'ґ'; 
      if s = 'micro'  then Result := 'µ'; 
      if s = 'para'   then Result := '¶'; 
      if s = 'middot' then Result := '·'; 
      if s = 'cedil'  then Result := 'ё'; 
      if s = 'sup1'   then Result := '№'; 
      if s = 'ordm'   then Result := 'є'; 
      if s = 'raquo'  then ResulT := '»'; 
      if s = 'frac14' then Result := 'ј'; 
      if s = 'frac12' then Result := 'Ѕ'; 
      if s = 'frac34' then Result := 'ѕ'; 
      if s = 'iquest' then Result := 'ї'; 
      if s = 'Agrave' then Result := 'А'; 
      if s = 'Aacute' then Result := 'Б'; 
      if s = 'Acirc'  then Result := 'В'; 
      if s = 'Atilde' then Result := 'Г'; 
      if s = 'Auml'   then Result := 'Д'; 
      if s = 'Aring'  then Result := 'Е'; 
      if s = 'Aelig'  then Result := 'Ж'; 
      if s = 'Ccedil' then Result := 'З'; 
      if s = 'Egrave' then Result := 'И'; 
      if s = 'Eacute' then Result := 'Й'; 
      if s = 'Ecirc'  then Result := 'К'; 
      if s = 'Euml'   then Result := 'Л'; 
      if s = 'Igrave' then Result := 'М'; 
      if s = 'Iacute' then Result := 'Н'; 
      if s = 'Icirc'  then Result := 'О'; 
      if s = 'Iuml'   then Result := 'П'; 
      if s = 'Eth'    then Result := 'Р'; 
      if s = 'Ntilde' then Result := 'С'; 
      if s = 'Ograve' then Result := 'Т'; 
      if s = 'Oacute' then Result := 'У'; 
      if s = 'Ocirc'  then Result := 'Ф'; 
      if s = 'Otilde' then Result := 'Х'; 
      if s = 'Ouml'   then Result := 'Ц'; 
      if s = 'times'  then Result := 'Ч'; 
      if s = 'Oslash' then Result := 'Ш'; 
      if s = 'Ugrave' then Result := 'Щ'; 
      if s = 'Uacute' then Result := 'Ъ'; 
      if s = 'Ucirc'  then Result := 'Ы'; 
      if s = 'Uuml'   then Result := 'Ь'; 
      if s = 'Yacute' then Result := 'Э'; 
      if s = 'thorn'  then Result := 'Ю'; 
      if s = 'szlig'  then Result := 'Я'; 
      if s = 'agrave' then Result := 'а'; 
      if s = 'aacute' then Result := 'б'; 
      if s = 'acirc'  then Result := 'в'; 
      if s = 'atilde' then Result := 'г'; 
      if s = 'auml'   then Result := 'д'; 
      if s = 'aring'  then Result := 'е'; 
      if s = 'aelig'  then Result := 'ж'; 
      if s = 'ccedil' then Result := 'з'; 
      if s = 'egrave' then Result := 'и'; 
      if s = 'eacute' then Result := 'й'; 
      if s = 'ecirc'  then Result := 'к'; 
      if s = 'euml'   then Result := 'л'; 
      if s = 'igrave' then Result := 'м'; 
      if s = 'iacute' then Result := 'н'; 
      if s = 'icirc'  then Result := 'о'; 
      if s = 'iuml'   then Result := 'п'; 
      if s = 'eth'    then Result := 'р'; 
      if s = 'ntilde' then Result := 'с'; 
      if s = 'ograve' then Result := 'т'; 
      if s = 'oacute' then Result := 'у'; 
      if s = 'ocirc'  then Result := 'ф'; 
      if s = 'otilde' then Result := 'х'; 
      if s = 'ouml'   then Result := 'ц'; 
      if s = 'divide' then Result := 'ч'; 
      if s = 'oslash' then Result := 'ш'; 
      if s = 'ugrave' then Result := 'щ'; 
      if s = 'uacute' then Result := 'ъ'; 
      if s = 'ucirc'  then Result := 'ы'; 
      if s = 'uuml'   then Result := 'ь'; 
      if s = 'yacute' then Result := 'э'; 
      if s = 'thorn'  then Result := 'ю'; 
      if s = 'yuml'   then Result := 'я'; 
    end; 
  end; 
end;

function HTML2Text(const HTML: String): String;
const Forbidden: Set of Char = [#0, #10, #13, '&']; 
var i, p, f, d: Integer; 
    s, HtmlSymbol: String;
    HtmlChar: Char;
begin
 Result := '';
 i := 0;
 p := 1;
 d := 0;
  if HTML <> '' then
  begin
    SetLength(s, Length(HTML) + 1);
    repeat
      inc(i);
      if HTML[i] = '<' then
      begin
        for f := p to i -1 do
        begin
          inc(d);
          s[d] := HTML[f];
        end;
        repeat
          inc(i);
        until (HTML[i] = #0) or (HTML[i] = '>');
        p := i + 1;
      end else
      begin
        p := i + 1;
        inc(d);
        s[d] := HTML[i];
      end;
    until HTML[i] = #0;

    if s <> '' then
    begin // entferne HTML-Sonderzeichen
      SetLength(Result, Length(s) + 1);
      i := 0;
      d := 0;
      repeat
        inc(i);
        if s[i] = '&' then
        begin
         p := i;
         inc(i);
         repeat
           if s[i] = ';' then
           begin
             HtmlSymbol := Copy( s, p + 1, i - p - 1);
             HtmlChar   := HTML2Char(HtmlSymbol);
             if HtmlChar <> #0 then
             begin // HTML-Sonderzeichen
               for f := p to i - (Length(HtmlSymbol) + 3) do // Kopiere
               begin
                 inc(d);
                 Result[d] := s[f];
               end;
               inc(d);
               Result[d] := HtmlChar; // Ersetze
             end else
             begin // kein HTML-Sonderzeichen
               for f := p to i -1 do
               begin
                 inc(d);
                 Result[d] := s[f];
               end;
             end;
             Break;
           end;
           inc(i);
         until s[i] in Forbidden;
        end else
        begin
          inc(d);
          Result[d] := s[i];
        end;
      until s[i] = #0;
    end; // s <> ''
   end;
end;

procedure TFmain.mapMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
var i,j:integer;
    nms:string;
    hintrect:TRect;
    Document: IHTMLDocument2;
    CState: Integer;
begin
 if (Layer=LayerMinMap)or(anim_zoom>0)or(
    (ssDouble in Shift)or(HiWord(GetKeyState(VK_DELETE))<>0)or(HiWord(GetKeyState(VK_INSERT))<>0))
   then begin
         moveTrue:=point(x,y);
         exit;
        end;
 CState:=ShowCursor(True);
 while CState < 0 do CState:= ShowCursor(true);
 sleep(1);
 if movepoint>-1 then
  begin
   add_line_arr[movepoint]:=extpoint(X2Lon(X),Y2Lat(Y));
   drawPath(add_line_arr,true,SetAlpha(ClRed32, 150),SetAlpha(ClWhite32, 50),3,aoper=add_poly);
   exit;
  end;
 if (aoper=rect)and(rect_dwn)and(not(ssRight in Shift))and(layer<>LayerMinMap)
         then begin
               rect_arr[1]:=extPoint(X2Lon(x),Y2Lat(y));
               drawRect(Shift);
              end;
 if dwn then layer.Cursor:=3;

 if vo_ves_ecran then begin
                       TBDock.Visible:=y<10;
                       TBDockLeft.Visible:=x<10;
                       TBDockBottom.Visible:=y>Map.Height-10;
                       TBDockRight.Visible:=x>Map.Width-10;
                      end;
 if anim_zoom=1 then exit;
 if dwn then begin
              LayerMap.Location:=floatrect(bounds(mWd2-pr_x-(move.X-x),mHd2-pr_y-(move.Y-y),hg_x shl 8,hg_y shl 8));
              if (FillingMap.LayerMap<>nil)and(FillingMap.LayerMap.Visible) then FillingMap.LayerMap.Location:=LayerMap.Location;
              if (LayerMapNal.Visible)and(aoper<>movemap) then LayerMapNal.Location:=floatrect(bounds(mWd2-pr_x-(move.X-x),mHd2-pr_y-(move.Y-y),hg_x shl 8,hg_y shl 8));
              if (LayerMapMarks.Visible) then LayerMapMarks.Location:=floatrect(bounds(mWd2-pr_x-(move.X-x),mHd2-pr_y-(move.Y-y),hg_x shl 8,hg_y shl 8));
              if (LayerMapGPS.Visible)and(GPS_enab) then LayerMapGPS.Location:=floatrect(bounds(mWd2-pr_x-(move.X-x),mHd2-pr_y-(move.Y-y),hg_x shl 8,hg_y shl 8));
              if LayerMapWiki.Visible then LayerMapWiki.Location:=floatrect(bounds(mWd2-pr_x-(move.X-x),mHd2-pr_y-(move.Y-y),hg_x shl 8,hg_y shl 8));
             end
        else m_m:=point(x,y);
 if not(dwn) then toSh;

 if (h<>nil)and(not ShowActivHint) then
  begin
   H.ReleaseHandle;
   FreeAndNil(H);
   oldLayerIndex:=0;
   Layer.Cursor:=curBuf;
   //ShowActivHint:=false;
  end;
 ShowActivHint:=false;
 if not(dwn)and((moveTrue.x<>X) or (moveTrue.y<>y)) then
  begin
   PWL.S:=0;
   PWL.find:=false;
   if (LayerMapWiki.Visible) then
     MouseOnReg(PWL,Point(x+(pr_x-mWd2),y+(pr_y-mHd2)));
   MouseOnMyReg(PWL,Point(x,y));
   if (PWL.find) then
    begin
     if h<>nil then H.ReleaseHandle;
     nms:='<HTML><BODY>'+PWL.name;
     if PWL.descr<>'' then nms:=nms+'<BR>'+PWL.descr;
     nms:=nms+'<HTML><BODY>';
     TextToWebBrowser(nms,EmbeddedWB1_);
     Document:=EmbeddedWB1_.Document as IHtmlDocument2;
     nms:=(Document.all.Item(NULL, 0) as IHTMLElement).outerText;
     //nms:=HTML2Text(nms);
     i:=1;
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
     oldLayerIndex:=layer.Index;
     if h=nil then
      begin
       H:=THintWindow.Create(Fmain);
       H.Brush.Color:=clInfoBk;
       H.Font.Charset:=RUSSIAN_CHARSET;
      end;
     layer.Cursor:=crHandPoint;
     hintrect:=H.CalcHintRect(Screen.Width, nms, nil);
     H.ActivateHint(Bounds(Mouse.CursorPos.x+13,Mouse.CursorPos.y-13,abs(hintrect.Right-hintrect.Left),abs(hintrect.Top-hintrect.Bottom)),nms);
     H.Repaint;
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
var Apos:TExtendedPoint;
    param:string;
    i:integer;
begin
Fprogress2.Visible:=True;
if SaveLink.Execute then
 begin
   Apos:=GPos2LonLat(Point(pos.x,pos.y),zoom_size,sat_map_both);
   param:=' '+sat_map_both.guids+' '+inttostr(zoom_size)+' '+floattostr(Apos.x)+' '+floattostr(Apos.y);
   CreateLink(ParamStr(0),SaveLink.filename, '', param)
 end;
Fprogress2.Visible:=false;
end;

procedure TFmain.TBEditItem2AcceptText(Sender: TObject;
  var NewText: String; var Accept: Boolean);
var SearchBM:TSearchBM;
begin
SearchBM:=TSearchBM.Create;
//SearchBM.
//
end;

procedure TFmain.TBItemDelTrackClick(Sender: TObject);
begin
 setlength(GPS_arr_speed,0);
 setlength(GPS_arr,0);
 GPSpar.len:=0;
 GPSpar.speed:=0;
 GPSpar.sspeed:=0;
end;

procedure TFmain.NGShScale0Click(Sender: TObject);
begin
 GShScale:=TMenuItem(sender).Tag;
 generate_im(nilLastLoad,'');
end;

procedure TFmain.TBEditPathDelClick(Sender: TObject);
begin
 case aoper of
  line: begin
         if length(length_arr)>0 then setlength(length_arr,length(length_arr)-1);
         drawLineCalc;
        end;
  add_poly,add_line:
        begin
         if length(add_line_arr)>0 then delfrompath(lastpoint);
         drawPath(add_line_arr,true,SetAlpha(ClRed32, 150),SetAlpha(ClWhite32, 50),3,aoper=add_poly);
        end;
 end;
end;

procedure TFmain.TBEditPathLabelClick(Sender: TObject);
begin
 LenShow:=not(LenShow);
 drawLineCalc;
end;

procedure TFmain.TBEditPathSaveClick(Sender: TObject);
var result:boolean;
begin
 case aoper of
  add_Poly: result:=FaddPoly.show_(add_line_arr,true);
  add_Line: result:=FaddLine.show_(add_line_arr,true);
 end;
 if result then
  begin
   setalloperationfalse(movemap);
   Fmain.generate_im(nilLastLoad,'');
  end;
end;

procedure TFmain.TBEditPathClose(Sender: TObject);
begin
 setalloperationfalse(movemap);
end;

procedure TFmain.NGoToForumClick(Sender: TObject);
begin
 Fmain.ShowCaptcha('http://sasgis.ru/forum');
end;

procedure TFmain.NGoToSiteClick(Sender: TObject);
begin
 Fmain.ShowCaptcha('http://sasgis.ru/');
end;

procedure TFmain.TBItem6Click(Sender: TObject);
begin
 Fmain.Enabled:=false;
 FMarksExplorer.ShowModal;
 Fmain.Enabled:=true;
 generate_im(nilLastLoad,'');
end;

procedure TFmain.NSRTM3Click(Sender: TObject);
var Apos:TExtendedPoint;
begin
 Apos:=GPos2LonLat(Point(pos.x-(mWd2-move.x),pos.y-(mHd2-move.y)),zoom_size,sat_map_both);
 TextToWebBrowser(SAS_STR_WiteLoad,Fbrowser.EmbeddedWB1);
 Fbrowser.Visible:=true;
 Fbrowser.EmbeddedWB1.Navigate('http://ws.geonames.org/srtm3?lat='+R2StrPoint(Apos.y)+'&lng='+R2StrPoint(Apos.x));
end;

procedure TFmain.NGTOPO30Click(Sender: TObject);
var Apos:TExtendedPoint;
begin
 Apos:=GPos2LonLat(Point(pos.x-(mWd2-move.x),pos.y-(mHd2-move.y)),zoom_size,sat_map_both);
 TextToWebBrowser(SAS_STR_WiteLoad,Fbrowser.EmbeddedWB1);
 Fbrowser.Visible:=true;
 Fbrowser.EmbeddedWB1.Navigate('http://ws.geonames.org/gtopo30?lat='+R2StrPoint(Apos.y)+'&lng='+R2StrPoint(Apos.x));
end;

end.

unit Umain;
interface
uses
  Windows, Registry, Messages, SysUtils, Forms, GR32, GR32_Resamplers,GR32_Layers,
  GR32_Image,GR32_Polygons, TB2Item, StdCtrls,TB2Dock, TB2Toolbar, math, ShellApi, inifiles,
  UTrAllLoadMap,  Classes, Menus,jpeg, RXSlider, UThreadScleit,
  ImgList, Controls, ExtCtrls, Clipbrd, Dialogs,Ugeofun, UWikiLayer,Buttons,uGPS,
  IdFTP,IdFTPCommon,UKMLmap, SHDocVw, MSHTML,GR32_Transforms, variants,ActiveX,
  ComCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, EwbCore,
  SHDocVw_EWB, EmbeddedWB,  OleCtrls, TB2ExtItems, XPMenu, midaslib, IdHTTP, Graphics, UMapType,
  DB, DBClient;
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
    BmpList: TBitmap32List;
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
    WebBrowser1: TEmbeddedWB;
    FTPcl: TIdFTP;
    NKMLShow: TMenuItem;
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
    XPMenu1: TXPMenu;
    TBLoadSelFromFile: TTBItem;
    NLoadSelFromFile: TMenuItem;
    TBEditItem1: TTBEditItem;
    IdHTTP_YaSrch: TIdHTTP;
    KML1: TMenuItem;
    KMLexplorer1: TMenuItem;
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
    TBKMLShow: TTBItem;
    MapIcons24: TImageList;
    NLayerSel: TMenuItem;
    NKMLShow1: TMenuItem;
    TBSeparatorItem4: TTBSeparatorItem;
    N7: TMenuItem;
    MapIcons18: TImageList;
    TBControlItem3: TTBControlItem;
    Label1: TLabel;
    btn_visib: TImage;
    Ninvertcolor: TMenuItem;
    TBMarksToolbar: TTBToolbar;
    TBAdd_Poly: TTBItem;
    TBAdd_Line: TTBItem;
    TBAdd_Point: TTBItem;
    CDSmarks: TClientDataSet;
    CDSmarksid: TAutoIncField;
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
    DataSource1: TDataSource;
    btn_ok: TImage;
    CDSmarksname: TStringField;
    TBItem5: TTBItem;
    NMarkDel: TMenuItem;
    NMarkEdit: TMenuItem;
    NMarkSep: TMenuItem;
    NMarksBarShow: TMenuItem;
    ZoomToolBar: TTBToolbar;
    TBZoom_out: TTBItem;
    TBControlItem1: TTBControlItem;
    TBZoomIn: TTBItem;
    TBSeparatorItem2: TTBSeparatorItem;
    TBControlItem2: TTBControlItem;
    labZoom: TLabel;
    RxSlider1: TRxSlider;
    btn_Close: TImage;
    btn_delpath: TImage;
    NMarkOper: TMenuItem;
    livecom1: TMenuItem;
    N13: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure NzoomInClick(Sender: TObject);
    procedure NZoomOutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TBZoom_outClick(Sender: TObject);
    procedure TBZoomInClick(Sender: TObject);
    procedure TBmoveClick(Sender: TObject);
    procedure TBFullSizeClick(Sender: TObject);
    procedure mapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure mapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure mapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure RxSlider1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
    procedure btn_CloseClick(Sender: TObject);
    procedure btn_visibClick(Sender: TObject);
    procedure TBCalcRasClick(Sender: TObject);
    procedure NCiclMapClick(Sender: TObject);
    procedure N012Click(Sender: TObject);
    procedure N29Click(Sender: TObject);
    procedure NMainToolBarShowClick(Sender: TObject);
    procedure NZoomToolBarShowClick(Sender: TObject);
    procedure NsrcToolBarShowClick(Sender: TObject);
    procedure EditGoogleSrchAcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
    procedure EmbeddedWB1_DocumentComplete(ASender: TObject; const pDisp: IDispatch; var URL: OleVariant);
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
    procedure TBKMLShowClick(Sender: TObject);
    procedure TBItem3Click(Sender: TObject);
    procedure Google1Click(Sender: TObject);
    procedure mapResize(Sender: TObject);
    procedure TBLoadSelFromFileClick(Sender: TObject);
    procedure TBEditItem1AcceptText(Sender: TObject; var NewText: String; var Accept: Boolean);
    procedure KMLexplorer1Click(Sender: TObject);
    procedure YaLinkClick(Sender: TObject);
    procedure kosmosnimkiru1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure NinvertcolorClick(Sender: TObject);
    procedure mapDblClick(Sender: TObject);
    procedure TBAdd_PointClick(Sender: TObject);
    procedure TBAdd_LineClick(Sender: TObject);
    procedure TBAdd_PolyClick(Sender: TObject);
    procedure btn_okClick(Sender: TObject);
    procedure TBItem5Click(Sender: TObject);
    procedure NMarkEditClick(Sender: TObject);
    procedure NMarkDelClick(Sender: TObject);
    procedure NMarksBarShowClick(Sender: TObject);
    procedure btn_delpathClick(Sender: TObject);
    procedure NMarkOperClick(Sender: TObject);
    procedure livecom1Click(Sender: TObject);
    procedure N13Click(Sender: TObject);
  private
   procedure DoMessageEvent(var Msg: TMsg; var Handled: Boolean);
   procedure WMGetMinMaxInfo(var msg: TWMGetMinMaxInfo);message WM_GETMINMAXINFO;
  public
   GPS_enab:boolean;
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
   function  timezone(lon,lat:real):TTime;
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
   procedure toGPSpos(lon,lat:real);
   procedure drawLineGPS;
   procedure ThreadGPSDone(Sender: TObject);
   procedure sm_im_reset_type2(x,y:integer);
   function SetFTPconnect:boolean;
   procedure ShowCaptcha(URL:string);
   procedure drawRect(Shift:TShiftState);
   procedure ShowErrScript(DATA:string);
   function mouseXY2Pos(Pnt:TPoint):TPoint;
   procedure setalloperationfalse(newop:TAOperation);
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


const
  MerkElipsK=0.0000001;
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы
  R2D: Double = 57.295779513082320876798154814105; // Константа для преобразования радиан в градусы
  zoom:array [1..24] of longint = (256,512,1024,2048,4096,8192,16384,32768,65536,
                                   131072,262144,524288,1048576,2097152,4194304,
                                   8388608,16777216,33554432,67108864,134217728,
                                   268435456,536870912,1073741824,2147483647);
var
  Fmain:TFmain;
  KML_Path:string;
  PWL:TResObj;
  dblclik:boolean = true;
  //exct,ra,rb:extended;
  zoom_size,zoom_mapzap,source,num_format,show_point,zoom_line,
  poly_zoom_save,alpha_mapzap,resampling,llStrType:byte;
  All_Dwn_Kb:Currency;
  ShowActivHint:boolean;
  mx,my,All_Dwn_Tiles,gamman,contrastn,vo_ves_ecr,anim_zoom,
    zoom_in,mWd2,mHd2,yhgpx,xhgpx,hg_x,hg_y,pr_x,pr_y,GPS_timeout,GPS_update,FTP_port,FTP_trtype:integer;
  move,m_up,m_m,POS,oldPOS,moveTrue:Tpoint;
  notpaint,invertcolor,dwn,start,close_,vo_ves_ecran,generate_rocess,KMLshow,ShowMapName,
    mouse_inv,sparam,ban_pg_ld,LenShow,CiclMap,Maximized,GPS_path,GPS_go,sizing,FTP_read,FTP_use,FTP_write:boolean;
  spr,sprb,mapbuf:TBitmap32;
  sprP:TPicture;
  DefCache:byte;
  sat_map_both:PMapType;
  marksicons:TStringList;
  //COMportpar
  TilesOut,BaudRate:integer;
  ByteSize,Parity,StopBits,Find_Zoom:byte;
  setparams:boolean;
  rect_arr:array [0..1] of TextendedPoint;
  rect_dwn,rect_p2:boolean;
  InetConnect:TInetConnect;
  //x
  aoper:TAOperation;
  Deg:real;
  NewCPath_,OldCPath_,ESCpath_,dwnlstr,GPS_COM,FTP_login,FTP_pass,FTP_path,FTP_Short_path:string;
  GPS_arr_speed:array of real;
  length_arr,add_line_arr,GPS_arr:array of TExtendedPoint;
  GPS_popr:TExtendedPoint;
  GPS_Log:boolean;
  GPS_SizeStr:integer;
  GPS_colorStr:TColor;
  reg_arr,poly_save:array of TExtendedPoint;
  Names:TName;
  sm_map:Tsm_map;
  RectWindow:TRect=(Left:0;Top:0;Right:0;Bottom:0);
  Dwn_list:TStringList;
  THLoadMap1,THLdMap_RightCl_1,THLdMap_RightCl_2,THLdMap_RightCl_3: ThreadAllLoadMap;
  THGPS: ThreadGPS;
  LayerMap,LayerMapWiki,LayerMapZap,LayerMapScale,layerLineM,LayerMinMap,LayerStatBar,LayerMapNal,LayerMapGPS: TBitmapLayer;
  h: THintWindow;
  oldLayerIndex:integer;
  curBuf:TCursor;
  OldFormWH:TPoint;
  Wikim_set:TWikim_set;
  TilesLoad:TStringList;
  nilLastLoad:TLastLoad;
  change_scene:boolean;
  GPSpar:TGPSpar;
  function  ffpath(x,y:longint;Azoom:byte;AMap:PMapType;short:boolean):string;


implementation
uses Unit2,Unit7,Usettings,USaveas,UProgress,UaddPoint,Unit4,
  USelLonLat, StrUtils,UImgFun, UKMLExplorer, UtimeZones, PNGimage,Ubrowser,
  UaddLine, UaddPoly;
{$R *.dfm}

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
    if Assigned(PNGObject) then PNGObject.Free;
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
 btn_visib.Visible:=false;
 btn_close.Visible:=false;
 btn_ok.Visible:=false;
 btn_delpath.Visible:=false;
 rect_dwn:=false;
 setlength(length_arr,0);
 setlength(add_line_arr,0);
 setlength(reg_arr,0);
 rect_p2:=false;
 case newop of
  movemap:  map.Cursor:=crDefault;
  line:     map.Cursor:=2;
  reg,rect: map.Cursor:=crDrag;
  Add_Point,Add_Poly,Add_Line:  map.Cursor:=4;
 end;
 aoper:=newop;
end;

function ffpath(x,y:longint;Azoom:byte;AMap:PMapType;short:boolean):string;
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
    i:byte;
    sbuf,name:String;
begin
 if x>=0 then x:=x mod zoom[Azoom]
         else x:=zoom[Azoom]+(x mod zoom[Azoom]);
 result:='';
 if Azoom>9 then sbuf:=inttostr(Azoom)
            else sbuf:='0'+inttostr(Azoom);
 if (PMapType(AMap).CacheType=1)or((PMapType(AMap).CacheType=0)and(DefCache=1))
  then
  begin
   if not(short) then result:=OldCpath_;
   result:=result+PMapType(AMap).NameInCache+'\'+sbuf+'\t';
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
  result:=result+PMapType(AMap).ext;
 end;
 if (PMapType(AMap).CacheType=2)or((PMapType(AMap).CacheType=0)and(DefCache=2)) then
 begin
  if not(short) then result:=NewCpath_;
  x:=x div 256;
  y:=y div 256;
  result:=result+PMapType(AMap).NameInCache+'\z'+inttostr(Azoom)+'\'+inttostr(x div 1024)+'\x'+inttostr(x)+'\'+
                                    inttostr(y div 1024)+'\y'+inttostr(y)+PMapType(AMap).ext;
 end;
 if (PMapType(AMap).CacheType=3)or((PMapType(AMap).CacheType=0)and(DefCache=3)) then
 begin
   if not(short) then result:=ESCpath_;
   name:=sbuf+'-'+full(x div 256,Azoom)+'-'+full(y div 256,Azoom);
   if Azoom<7
    then result:=result+PMapType(AMap).NameInCache+'\'+sbuf+'\'+name+PMapType(AMap).ext
    else if Azoom<11
          then result:=result+PMapType(AMap).NameInCache+'\'+sbuf+'\'+Chr(59+Azoom)+
                       full((x div 256)div 32,Azoom-5)+full((y div 256)div 32,Azoom-5)+'\'+name+PMapType(AMap).ext
          else result:=result+PMapType(AMap).NameInCache+'\'+'10'+'-'+full((x div round(power(2,Azoom-10)))div  256,10)+'-'+
                       full((y div round(power(2,Azoom-10)))div 256,10)+'\'+sbuf+'\'+Chr(59+Azoom)+
                       full((x div 256)div 32,Azoom-5)+full((y div 256)div 32,Azoom-5)+'\'+name+PMapType(AMap).ext;
 end;
 if not(short)and(copy(result,2,2)<>':\')and(copy(result,1,2)<>'\\')and(copy(result,1,3)<>'ftp')
   then result:=ExtractFilePath(ParamStr(0))+result;
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
   WM_MOUSEWHEEL:begin
                  if mouse_inv then z:=-1 else z:=1;
                  if Msg.wParam<0 then Fmain.zooming(Zoom_size-(1*z),NGoToCur.Checked)
                                  else Fmain.zooming(Zoom_size+(1*z),NGoToCur.Checked);
                 end;
   WM_KEYUP:begin
             if Msg.wParam=VK_RIGHT then pos.x:=pos.x+128;
             if Msg.wParam=VK_Left then pos.x:=pos.x-128;
             if Msg.wParam=VK_Down then pos.y:=pos.y+128;
             if Msg.wParam=VK_Up then pos.y:=pos.y-128;
             if (Msg.wParam=VK_RIGHT)or(Msg.wParam=VK_Left)or
                (Msg.wParam=VK_Down)or(Msg.wParam=VK_Up)then
               begin
                generate_im(nilLastLoad,'');
               end;
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
               begin
                if length(add_line_arr)>0 then setlength(add_line_arr,length(add_line_arr)-1);
                drawPath(add_line_arr,true,SetAlpha(ClRed32, 150),SetAlpha(ClWhite32, 50),3,aoper=add_poly);
               end;
             if (Msg.wParam=VK_ESCAPE)and(aoper=Reg) then
               begin
                setlength(reg_arr,0);
                drawreg;
               end;
             if (Msg.wParam=VK_ESCAPE)and(aoper=line) then
               begin
                setlength(length_arr,0);
                drawLineCalc;
               end;
             if (Msg.wParam=VK_ESCAPE)and((aoper=rect)or(aoper in [add_line,add_poly])) then
               begin
                TBmoveClick(Fmain);
               end;
             if (Msg.wParam=13)and(aoper=add_Poly)and(length(add_line_arr)>1) then
               FaddPoly.show_(self,add_line_arr,true);
             if (Msg.wParam=13)and(aoper=add_line)and(length(add_line_arr)>1) then
               FaddLine.show_(self,add_line_arr,true);
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
 result:=float2str(kb)+' Кб';
 if kb>1024 then result:=float2str(kb/1024)+' Мб';
 if kb>1048576 then result:=float2str(kb/1048576)+' Гб';
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
var x,y:Integer;
    Dest:PColor32Array;
begin
 if invertcolor then
 for y:=0 to Bitmap.Height-1 do
  begin
   Dest:=Bitmap.ScanLine[y];
   for x:=0 to Bitmap.Width-1 do
    begin
     Dest^[0]:=not(Dest^[0]) xor $FF000000;
     Inc(Dest);
    end;
  end;
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
        Dest^[0]:=GR32.Color32(GT[RedComponent(dest^[0])],GT[GreenComponent(dest^[0])],GT[BlueComponent(dest^[0])]);
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
 p:=System.pos('.',inp);
 if (p>0)and(DecimalSeparator=',') then
   begin
    inp[p]:=',';
    result:=strtofloat(inp);
    exit;
   end;
 p:=System.pos(',',inp);
 if (p>0)and(DecimalSeparator='.') then
   begin
    inp[p]:='.';
    result:=strtofloat(inp);
    exit;
   end;
 result:=strtofloat(inp);
end;

procedure TFmain.createdirif(path:string);
var i:integer;
begin
 for i:=length(path) downto 0 do if path[i]='\'then break;
 path:=copy(path,1,i);
 if not(DirectoryExists(path)) then ForceDirectories(path);
end;

procedure TFmain.ThreadSclDone(Sender: TObject);
begin
 ThreadScleit(sender):=nil;
 Fmain.TBRectSave.Enabled:=true;
 FProgress.Close;
end;

procedure TFmain.ThreadGPSDone(Sender: TObject);
begin
  if ThreadGPS(sender).err_open then begin
                                      TBGPSconn.Checked:=false;
                                      NGPSconn.Checked:=false;
                                     end;
  if ThreadGPS(sender)<>nil then
     begin
      if ThreadGPS(sender).log then  CloseFile(ThreadGPS(sender).F);
      CloseHandle(ThreadGPS(sender).hCom);
      ThreadGPS(sender):=nil;
      setlength(GPS_arr,0);
      setlength(GPS_arr_speed,0); 
      LayerMapGPS.Bitmap.Clear(clBlack);
     end;
 GPS_enab:=false;
 LayerMapGPS.Visible:=false;
end;

procedure TFmain.ThreadDone(Sender: TObject);
begin
  ThreadAllLoadMap(sender).closeSession;
  if ThreadAllLoadMap(sender)=THLoadMap1 then
     begin
      if (THLoadMap1.res<=0) then
       begin
        THLoadMap1:=nil;
        exit;
       end;
      THLoadMap1:=nil;
     end;
   if ThreadAllLoadMap(sender)=THLdMap_RightCl_1 then
     THLdMap_RightCl_1:=nil;
   if ThreadAllLoadMap(sender)=THLdMap_RightCl_2 then
     THLdMap_RightCl_2:=nil;
   if ThreadAllLoadMap(sender).typeRect in [2,3] then
    begin
     ThreadAllLoadMap(sender):=nil;
     if not((dwn)or(generate_rocess)or(anim_zoom=1)) then
       begin
        move.X:=m_up.x;
        Fmain.generate_im(nilLastLoad,'');
       end; 
     exit;
    end;
end;

procedure TFmain.drawRect(Shift:TShiftState);
var i,d256,kz,jj,j,bxy:integer;
    xy1,xy2,pxy1,pxy2:TPoint;
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
    Outline: TPolygon32;
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
 Outline := Polygon.Outline.Grow(Fixed(2 / 2), 0.5);
 Outline.FillMode := pfWinding;
 Polygon.DrawFill(LayerMapNal.Bitmap, SetAlpha(clWhite32, 40));
 Outline.DrawFill(LayerMapNal.Bitmap, SetAlpha(clBlue32, 180));
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
var i,speed:integer;
    k1,k2:TPoint;
    ke,ks:TExtendedPoint;
    TanOfAngle,Angle,D,R: Currency;
    dl: integer;
    Polygon: TPolygon32;
    Clr:TColor;
begin
 Polygon := TPolygon32.Create;
 Polygon.Antialiased := true;
 Polygon.AntialiasMode := am32times;
 map.Bitmap.BeginUpdate;
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
  for i:=0 to length(GPS_arr)-2 do
   begin
    k1:=point(Lon2X(GPS_arr[i].x),lat2Y(GPS_arr[i].y));
    k2:=point(Lon2X(GPS_arr[i+1].x),Lat2Y(GPS_arr[i+1].y));
    k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
    k2:=Point(k2.X+(pr_x-mWd2),k2.y+(pr_y-mHd2));
    if GPS_arr_speed[i]>0 then speed:= round(255/(GPSpar.maxspeed/GPS_arr_speed[i]))
                          else speed:=0;
    Clr:=Color32(speed,0,256-speed,0);
    LineAS(k1.X,k1.Y,k2.X,k2.Y,SetAlpha(Clr,180));
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
   LayerMapGPS.Bitmap.FillRectS(round(ke.x-2),round(ke.y-2),round(ke.x+2),round(ke.y+2),SetAlpha(clRed32, 200));
  end;
 map.Bitmap.endUpdate;
 map.Bitmap.Changed;
 LayerMapGPS.BringToFront;
 Polygon.Free;
 toSh;
end;

procedure TFmain.drawPath(pathll:array of TExtendedPoint;new:boolean;color1,color2:TColor32;linew:integer;poly:boolean);
var i:integer;
    k1:TPoint;
    polygon: TPolygon32;
begin
 try
 if new then LayerMapNal.Bitmap.Clear(clBlack);
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
    k1:=point(Lon2X(pathll[i].x),Lat2Y(pathll[i].y));
    k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
    polygon.Add(FixedPoint(k1));
   end;
  if poly then if new then Polygon.DrawFill(LayerMapNal.Bitmap, color2)
                      else Polygon.DrawFill(LayerMap.Bitmap, color2);
  with Polygon.Outline.Grow(Fixed(linew / 2), 0.5) do
   begin
    FillMode:=pfWinding;
    if new then DrawFill(LayerMapNal.Bitmap, color1)
           else DrawFill(LayerMap.Bitmap, color1);
   end;
  polygon.Free;
 end;
 if new then
  begin
   btn_ok.Visible:=length(pathll)>1;
   btn_delpath.Visible:=length(pathll)>1;
   k1:=point(Lon2X(pathll[0].x),lat2Y(pathll[0].y));
   k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
   LayerMapNal.Bitmap.FillRectS(k1.x-3,k1.y-3,k1.X+3,k1.y+3,SetAlpha(ClGreen32,255));
   k1:=point(Lon2X(pathll[length(pathll)-1].x),lat2Y(pathll[length(pathll)-1].y));
   k1:=Point(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2));
   LayerMapNal.Bitmap.FillRectS(k1.x-3,k1.y-3,k1.X+3,k1.y+3,SetAlpha(ClRed32,255));
  end;
 if (new)and(length(pathll)>1) then
  begin
   LayerMapNal.Bitmap.FillRectS(k1.x+5,k1.y,k1.X+32,k1.y+15,SetAlpha(ClWhite32,120));
   btn_ok.Left:=k1.X+7+round(LayerMap.Location.Left);
   btn_ok.top:=k1.Y+2+round(LayerMap.Location.Top);
   btn_delpath.Left:=k1.X+20+round(LayerMap.Location.Left);
   btn_delpath.top:=k1.Y+2+round(LayerMap.Location.Top);
  end;
 map.Bitmap.endUpdate;
 map.Bitmap.Changed;
 except
 end;
end;


procedure TFmain.drawLineCalc;
var i,j,textW,l:integer;
    k1,k2:TPoint;
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
 btn_visib.Visible:=false;
 btn_close.Visible:=false;
 LayerMapNal.Bitmap.Font.Name:='Tahoma';
 LayerMapNal.Bitmap.Clear(clBlack);
 if length(length_arr)>0 then
 with LayerMapNal.Bitmap do
 begin
  for i:=0 to length(length_arr)-1 do
   begin
    k1:=point(Lon2X(length_arr[i].x),Lat2Y(length_arr[i].y));
    polygon.Add(FixedPoint(k1.X+(pr_x-mWd2),k1.y+(pr_y-mHd2)));
   end;
  with Polygon.Outline.Grow(Fixed(2.5 / 2), 0.5) do
   begin
    FillMode:=pfWinding;
    DrawFill(LayerMapNal.Bitmap, SetAlpha(ClRed32, 150));
   end;
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
      text:='Всего: '+R2ShortStr(len,2,'км. ','м.');
      Font.Size:=9;
      textW:=TextWidth(text)+40;
      FillRectS(k2.x+12,k2.y,k2.X+textW,k2.y+15,SetAlpha(ClWhite32,110));
      RenderText(k2.X+15,k2.y,text,3,clBlack32);
      btn_close.Left:=k2.X+textW-14+round(LayerMapNal.Location.Left);
      btn_close.top:=k2.Y+2+round(LayerMapNal.Location.Top);
      btn_visib.Left:=k2.X+textW-27+round(LayerMapNal.Location.Left);
      btn_visib.top:=k2.Y+2+round(LayerMapNal.Location.Top);
     end
    else
     if LenShow then
      begin
       text:=R2ShortStr(find_length(length_arr[i].y,length_arr[i+1].y,length_arr[i].x,length_arr[i+1].x),2,'км. ','м.');
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
 btn_visib.Visible:=(length(length_arr)>1);
 btn_close.Visible:=(length(length_arr)>1);
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
    ks:TKernelResampler;
begin
 if (show_point=3) then exit;
 LayerMap.Bitmap.Font.Name:='Tahoma';
 LayerMap.Bitmap.Font.Style:=[];
 lon_l:=X2Lon(-(pr_x-mWd2));
 lon_r:=X2Lon(pr_x+mWd2);
 lat_t:=Y2Lat(-(pr_y-mHd2));
 lat_d:=Y2Lat(pr_y+mHd2);
 CDSmarks.Filter:='(not( LonR<'+floattostr(lon_l)+' or LonL>'+floattostr(lon_R)+
                  ' or LatB>'+floattostr(lat_t)+' or LatT<'+floattostr(lat_d)+'))';
 if show_point=2 then CDSmarks.Filter:=CDSmarks.Filter+' and visible=true';
 CDSmarks.Filtered:=true;
 CDSmarks.First;
 While not(CDSmarks.Eof) do
  begin
     ms:=TMemoryStream.Create;
     TBlobField(CDSmarksLonLatArr).SaveToStream(ms);
     ms.Position:=0;
     GetMem(arrLL,ms.size);
     ms.ReadBuffer(arrLL^,ms.size);
     if (ms.size)>24 then
      begin
       TestArrLenP1:=GLonLat2Pos(ExtPoint(CDSmarksLonL.AsFloat,CDSmarksLatT.AsFloat),zoom_size,sat_map_both);
       TestArrLenP2:=GLonLat2Pos(ExtPoint(CDSmarksLonR.AsFloat,CDSmarksLatB.AsFloat),zoom_size,sat_map_both);
       if (abs(TestArrLenP1.X-TestArrLenP2.X)>CDSmarksScale1.AsInteger+2)and(abs(TestArrLenP1.Y-TestArrLenP2.Y)>CDSmarksScale1.AsInteger+2) then
        begin
         SetLength(buf_line_arr,(ms.size div 24));
         for i:=0 to (ms.size div 24)-1 do
          buf_line_arr[i]:=arrLL^[i];
         drawPath(buf_line_arr,false,TColor32(CDSmarksColor1.AsInteger),TColor32(CDSmarksColor2.AsInteger),CDSmarksScale1.asInteger,
                 (buf_line_arr[0].x=buf_line_arr[length(buf_line_arr)-1].x)and(buf_line_arr[0].y=buf_line_arr[length(buf_line_arr)-1].y));
         SetLength(buf_line_arr,0);
        end;
      end;
    ms.free;
    FreeMem(arrLL);
    CDSmarks.Next;
  end;
 CDSmarks.First;
 btm:=TBitmap32.Create;
 btm.DrawMode:=dmBlend;
 ks:=TKernelResampler.Create;
 ks.Kernel:=TCubicKernel.Create;
 btm.Resampler:=ks;
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
                           LayerMap.Bitmap.Draw(bounds(xy.x-(imw div 2),xy.y-imw,imw,imw),bounds(0,0,btm.Width,btm.Height),btm);
                          end;
       if CDSmarksScale1.AsInteger>0 then
        begin
         LayerMap.Bitmap.Font.Size:=CDSmarksScale1.AsInteger;
         texth:=LayerMap.Bitmap.TextHeight(CDSmarksname.asString) div 2;
         LayerMap.Bitmap.RenderText(xy.x+(imw div 2)+2,xy.y-(imw div 2)-texth+1,CDSmarksname.AsString,1,TColor32(CDSmarksColor2.AsInteger));
         LayerMap.Bitmap.RenderText(xy.x+(imw div 2)+1,xy.y-(imw div 2)-texth,CDSmarksname.AsString,1,TColor32(CDSmarksColor1.AsInteger));
        end;
      end;
    ms.free;
    FreeMem(arrLL);
    CDSmarks.Next;
  end;
 btm.Free;
end;

function TFmain.timezone(lon,lat:real):TTime;
var prH,prM:integer;
    tz:real;
    st:TSystemTime;
begin
 tz:=GetTZ_(ExtPoint(Lon,Lat));
 GetSystemTime(st);
 prH:=trunc(tz);
 prM:=round(60*frac(TZ));
 result:=StrToTime(inttostr(abs(st.wHour+prH+24)mod 24)+':'+inttostr(abs(st.wMinute+prM+60)mod 60));
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
      result:=((POS.y-(map.Height/2-Y))-zoom[zoom_size]/2) /-(zoom[zoom_size]/(2*Pi));
      result:=(2*arctan(exp(result))-Pi/2)*180/Pi;
      Zu:=result/(180/Pi);
      yy:=((POS.y-(map.Height/2-Y))-zoom[zoom_size]/2);
      repeat
       Zum1:=Zu;
       Zu:=arcsin(1-((1+Sin(Zum1))*power(1-sat_map_both.exct*sin(Zum1),sat_map_both.exct))/(exp((2*yy)/-(zoom[zoom_size]/(2*Pi)))*power(1+sat_map_both.exct*sin(Zum1),sat_map_both.exct)));
      until (abs(Zum1-Zu)<MerkElipsK)or(isNAN(Zu));
      if not(isNAN(Zu)) then result:=zu*180/Pi;
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
 while (dwn)or(generate_rocess)or(anim_zoom=1) do Application.ProcessMessages;
 zooming(zoom_,false);
 POS:=GLonLat2pos(ExtPoint(lon,lat),zoom_,sat_map_both);
 generate_im(nilLastLoad,'');
 TilesLoad.Clear;
if draw then LayerMap.Bitmap.Draw(pr_x-7,pr_y-6,bmpList.Bitmap[3]);
end;

procedure TFmain.toGPSpos(lon,lat:real);
begin
 if GPS_go then POS:=GLonLat2pos(ExtPoint(lon,lat),zoom_size,sat_map_both);
 generate_im(nilLastLoad,'');
// drawLineGPS;
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
                    se:=' км.';
                   end
              else
 if num<10    then begin
                    num:=num*100;
                    se:=' см.';
                   end
              else se:=' м.';
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
begin
 labZoom.caption:=' '+inttostr(zoom_size)+'x ';
 ll:=GPos2LonLat(mouseXY2Pos(Point(m_m.X,m_m.Y)),zoom_size,sat_map_both);
 result:=lon2str(ll.x)+' ';
 result:=result+lat2str(ll.y);
 LayerStatBar.Bitmap.Width:=map.Width;
 subs2:=ffpath(X2absX(pos.x-(mWd2-m_m.x),zoom_size),pos.y-(mHd2-m_m.y),zoom_size,sat_map_both,false);
 LayerStatBar.Bitmap.Clear(SetAlpha(clWhite32,160));
 LayerStatBar.Bitmap.Line(0,0,map.Width,0,SetAlpha(clBlack32,256));
 LayerStatBar.bitmap.RenderText(4,1,'Координаты '+result, 0, clBlack32);
 if GPS_enab then
  begin
   LayerStatBar.bitmap.RenderText(248,1,' | '+'Скорость '+R2ShortStr(GPSpar.speed,1,' ',' км/час')
   +' (Ср.: '+ R2ShortStr(GPSpar.sspeed,1,' ',' км/час;')
   +' Макс.: '+R2ShortStr(GPSpar.maxspeed,1,' ',' км/час)')
   +' Дл. пути: '+R2ShortStr(GPSpar.len,4,' км ',' м'), 0, clBlack32);
  end
  else
  begin
//   LayerStatBar.bitmap.RenderText(248,1,' | '+'Масштаб '+inttostr(integer(LayerMap.Bitmap.Canvas.Pixels[m_m.x,m_m.y])), 0, clBlack32);
   LayerStatBar.bitmap.RenderText(248,1,' | '+'Масштаб '+R2ShortStr(1/((zoom[zoom_size]/(2*PI))/(PMapType(sat_map_both).radiusa*cos(ll.y*deg))),4,' км.',' м./пикс.'), 0, clBlack32);
   LayerStatBar.bitmap.RenderText(448,1,' | '+'Время~ '+ TimeToStr(timezone(ll.x,ll.y)), 0, clBlack32);
   LayerStatBar.bitmap.RenderText(573,1,' | '+'Скачано '+inttostr(All_Dwn_Tiles)+' ('+kb2KbMbGb(All_Dwn_Kb)+') | '+'Файл '+subs2, 0, clBlack32);
  end;
 LayerStatBar.BringToFront;
end;


function TFmain.loadpre(var spr:TBitmap32;x,y:integer;Azoom:byte;Amap:PMapType):boolean;
var i,c_x,c_y,c_d:integer;
    ss:string;
    bmp,bmp2:TBitmap32;
    ks:TKernelResampler;
begin
 result:=false;
 if not(Nbackload.Checked) then exit;
 for i:=(Azoom-1) downto 1 do
  begin
   c_d:=round(power(2,Azoom-i));
   ss:=ffpath(x div c_d,y div c_d,i,AMap,false);
   if TileExists(ss) then break;
  end;
 if not(tileExists(ss))or(c_d>=512) then exit;
 bmp:=TBitmap32.Create;
 ks:=TKernelResampler.Create;
 case resampling of
  0: ks.Kernel:=TBoxKernel.Create;
  1: ks.Kernel:=TLinearKernel.Create;
  2: ks.Kernel:=TCosineKernel.Create;
  3: ks.Kernel:=TSplineKernel.Create;
  4: ks.Kernel:=TMitchellKernel.Create;
  5: ks.Kernel:=TCubicKernel.Create;
  6: ks.Kernel:=THermiteKernel.Create;
  7: ks.Kernel:=TLanczosKernel.Create;
  8: ks.Kernel:=TGaussianKernel.Create;
  9:ks.Kernel:=TBlackmanKernel.Create;
  10:ks.Kernel:=THannKernel.Create;
  11:ks.Kernel:=THammingKernel.Create;
  12:ks.Kernel:=TSinshKernel.Create;
 end;
 try
  LoadTilefromCache(bmp,ss);
 except
  bmp.Width:=256;
  bmp.Height:=256;
  bmp.Clear(Color32(clSilver) xor $00000000);
 end;
 c_x:=((x-(x mod 256))div c_d)mod 256;
 c_y:=((y-(y mod 256))div c_d)mod 256;
 bmp2:=TBitmap32.Create;
 bmp2.Width:=256; bmp2.Height:=256;
 bmp.Resampler:=ks;
 bmp2.Draw(bounds(-c_x*c_d,-c_y*c_d,256*c_d,256*c_d),bounds(0,0,256,256),bmp);
 spr.Assign(bmp2);
 bmp.Free;
 bmp2.Free;
 result:=true;
end;

procedure TFmain.generate_granica;
var y_draw,x_draw,xx,yy,xx1,yy1:longint;
    i,j:integer;
    src:TRect;
    d2562,x2,x1,y1,zl:integer;
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
 LayerMap.bitmap.Canvas.pen.Color:=clWhite;
 for i:=0 to hg_x*(x2)+(x_draw div d2562) do
  for j:=0 to hg_y*(x2)+(y_draw div d2562) do
    begin
     xx:=xx1+(i shl 8);
     yy:=yy1+(j shl 8);
     if (xx<0)or(yy<0)or(yy>=zoom[zl])or(xx>=zoom[zl]) then Continue;
     x1:=(i*d2562)-x_draw;
     y1:=(j*d2562)-y_draw;
     LayerMap.bitmap.HorzLineS(x1,y1,x1+d2562,SetAlpha(ClWhite,30));
     LayerMap.bitmap.VertLineS(x1+d2562,y1,y1+d2562,SetAlpha(ClWhite,30));
     LayerMap.bitmap.HorzLineS(x1+d2562,y1+d2562,x1,SetAlpha(ClWhite,30));
     LayerMap.bitmap.VertLineS(x1,y1+d2562,y1,SetAlpha(ClWhite,30));
    end;
end;

procedure TFmain.generate_mapzap;
var Path:String;
    y_draw,x_draw,x1,y1,xx,yy,imd256:longint;
    i,j,ii,jj,Ahg_x,Ahg_y,Apr_x,Apr_y,ppaprx,ppapry:integer;
    ClMZ:TColor32;
    fn,fo:string;
    bo,bb:boolean;
    d2562,x2:integer;
begin
 LayerMapZap.Visible:=zoom_mapzap>0;
 if (zoom_mapzap<=zoom_size)or(zoom_mapzap-zoom_size>8) then exit;
 LayerMapZap.bitmap.Clear(clBlack);
 x2:=trunc(power(2,zoom_mapzap-zoom_size));
 ClMZ:=SetAlpha(clBlack32,alpha_mapzap);
 d2562:=256 div x2;
 Ahg_x:=(map.Width div d2562)+1;
 Ahg_y:=(map.Height div d2562)+1;
 Apr_x:=(d2562*Ahg_x)div 2;
 Apr_y:=(d2562*Ahg_y)div 2;
 x_draw:=((d2562+((pos.x-Apr_x)mod d2562))mod d2562)-((pr_x-Apr_x));
 y_draw:=((d2562+((pos.y-Apr_y)mod d2562))mod d2562)-((pr_y-Apr_y));
 if (zoom_mapzap-zoom_size)>3 then
  begin
   Fprogress2.ProgressBar1.Max:=((Ahg_x+1)*(Ahg_y+1));
   Fprogress2.Caption:='Идет обработка...';
   fprogress2.ProgrInfo.Lines[0]:='Идет обработка...';
   Fprogress2.ProgressBar1.Progress1:=0;
   fprogress2.ProgrInfo.Lines[1]:='Обработано '+inttostr(Fprogress2.ProgressBar1.Progress1);
   Fprogress2.Visible:=true;
   Fmain.Enabled:=false;
  end;
 ppaprx:=pos.x-Apr_x;
 ppapry:=pos.y-Apr_y;
 for i:=0 to Ahg_x do
  begin
   imd256:=i*d2562;
   xx:=(ppaprx+imd256)*x2;
   if (xx<0)or(xx>=zoom[zoom_mapzap]) then Continue;
   for j:=0 to Ahg_y do
    begin
     yy:=(ppapry+(j*d2562))*x2;
     if (yy<0)or(yy>=zoom[zoom_mapzap]) then Continue;
     Path:=ffpath(xx,yy,zoom_mapzap,sat_map_both,false);
     fn:=ExtractFilePath(path);
     if fn=fo then if bo then bb:=TileExists(path)
                         else bb:=false
              else begin
                    bo:=DirectoryExists(fn);
                    if bo then bb:=TileExists(path)
                          else bb:=false;
                   end;
     fo:=fn;
     if not(bb)
      then begin
            x1:=imd256-x_draw; y1:=(j*d2562)-y_draw;
            for ii:=x1 to x1+(d2562-1) do
             for jj:=y1 to y1+(d2562-1) do
              LayerMapZap.Bitmap.PixelS[ii,jj]:=clMZ;
           end;
    end;
   if ((i mod 15) = 0)and((zoom_mapzap-zoom_size)>3) then
    begin
     LayerMapZap.Update;
     Fprogress2.ProgressBar1.Progress1:=i*Ahg_y;
     fprogress2.ProgrInfo.Lines[1]:='Обработано '+inttostr(Fprogress2.ProgressBar1.Progress1);
     Application.ProcessMessages;
     if (Fprogress2.Visible=false)and((zoom_mapzap-zoom_size)>3) then exit;
    end;
  end;
 LayerMapZap.Update;
 if (zoom_mapzap-zoom_size)>3 then Fprogress2.Close;
end;

procedure BadDraw(var spr:TBitmap32);
begin
 spr.Clear(Color32(clSilver) xor $00000000);
 spr.Canvas.Brush.Color:=Color32(clSilver) xor $00000000;
 spr.Canvas.Rectangle(0,0,256,256);
 spr.Textout(87,120,'Файл испорчен!')
end;

procedure TFmain.generate_im(LastLoad:TLastLoad;err:string);
var Path:String;
    y_draw,x_draw,y_drawN,x_drawN,xx,yy,x_,x_1,y_,y_1,xmov,ymov,size,ii,jj:longint;
    i,j:byte;
    Leyi:integer;
    AcrBuf:Tcursor;
    posN,opmp:TPoint;
    ATilesLoad:TStringList;
    Dest:PColor32Array;
    ts:Cardinal;
    lok:string;
    alphaarr:array [0..255,0..255] of byte;
begin
 if (generate_rocess)or(notpaint) then exit;
 generate_rocess:=true;
 ts:=GetTickCount;
 if not(lastload.use) then change_scene:=true;
 ATilesLoad:=TStringList.Create;
 AcrBuf:=map.Cursor;
 map.Cursor:=crAppStart;
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
 LayerMap.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
 if zoom_mapzap>0 then LayerMapZap.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
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
//    xx:=xx-(abs(xx) mod 256);
//    xx:=xx-(abs(xx) mod 256);
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
    Path:=ffpath(xx,yy,zoom_size,sat_map_both,false);
    lok:=inttostr(xx)+'-'+inttostr(yy)+'-'+inttostr(zoom_size)+'-'+inttostr(sat_map_both.id);
    {if ((i<>0)and(j<>0)and(i<>hg_x)and(j<>hg_y)) then }ATilesLoad.Add(' '+lok);
    if (TilesLoad.IndexOf(lok)>-1)and(LastLoad.use=false) then continue;
    if (lastload.use)and((lastload.x<>xx)or(lastload.z<>zoom_size)or
       ((lastload.y<yy-128)or(lastload.y>yy+128))) then continue;
    SynFtp(path,ffpath(xx,yy,zoom_size,sat_map_both,true));
    if TileExists(path) then
     begin
      try
       LoadTilefromCache(spr,path);
      except
       BadDraw(spr);
      end;
     end
    else begin
          if loadpre(spr,xx,yy,zoom_size,sat_map_both)
            then begin
                 end
            else begin
                  spr.Clear(Color32(clSilver) xor $00000000);
                 end;
         end;
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
       Path:=ffpath(xx,yy,zoom_size,@MapType[Leyi],false);
       SynFtp(path,ffpath(xx,yy,zoom_size,@MapType[Leyi],true));
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
         try
          //if MapType[Leyi].ext='.png' then LoadPNGintoBitmap32(spr,Path)
          //                            else LoadTilefromCache(spr,Path);
          LoadTilefromCache(spr,Path);
          spr.ResetAlpha;
         except
          BadDraw(spr);
         end;
         InvertBitmap(spr);
         mapbuf.Draw((i shl 8)-x_drawN,(j shl 8)-y_drawN, spr);
        end
      end;
    end;
  LayerMap.bitmap.Draw(-x_draw,-y_draw, mapbuf);
//  LayerMap.bitmap.Draw(500,0,mapbuf);
{
       if not(sat_map_both in [mtYaSat,mtYaSat,mtKSat]) then
       if ((i>0)and(j>0)and(i<hg_x)and(j<hg_y)) then ATilesLoad.Add(' '+lok);
       if (sat_map_both in [mtYaSat,mtYaSat,mtKSat]) then
       if ((i>1)and(j>1)and(i<hg_x-1)and(j<hg_y-1)) then ATilesLoad.Add(' '+lok);
       if (TilesLoad.IndexOf(lok)>-1)and(lastload.use=false) then continue;
       if (lastload.use)and((lastload.x<>xx)or(lastload.z<>zoom_size)or
          ((lastload.y<yy-256)and(lastload.y>yy+256))) then continue;
  }
 TilesLoad.Clear;
 if ATilesLoad.Count>1 then
 for i:=0 to ATilesLoad.Count-1 do TilesLoad.Add(ATilesLoad.ValueFromIndex[i]);
 ATilesLoad.Free;
 generate_granica;
 paint_Line;
 if aoper=line then drawLineCalc;
 if aoper=reg then drawReg;
 if aoper=rect then drawRect([]);
 if GPS_enab then drawLineGPS;
 if aoper in [add_line,add_poly] then drawPath(add_line_arr,true,setalpha(clRed32,150),setalpha(clWhite32,50),3,aoper=add_poly);
 draw_point;
 if not(lastload.use) then generate_mapzap;
 m_up.x:=move.X;
 m_up.y:=move.y;
 toSh;
 if KMLShow then ShowKML;
 sm_im_reset(sm_map.width div 2,sm_map.height div 2);
 Label1.caption := IntToStr(GetTickCount-ts);
 map.Cursor:=AcrBuf;
 OldPos:=Pos;
 generate_rocess:=false;
end;

function TFmain.SetFTPconnect:boolean;
var Apos:integer;
begin
 try
 Apos:=PosEx('/',Ftp_path,7);
 if (FTPcl.Connected) then FTPcl.Disconnect;
 if not(FTPcl.Connected) then
  begin
   FTPcl.Host:=copy(Ftp_path,7,Apos-7);
   FTPcl.Username:=Ftp_login;
   FTPcl.Password:=Ftp_pass;
   FTPcl.Port:=Ftp_port;
   if Ftp_trtype=0 then Fmain.FTPcl.TransferType:=ftBinary
                   else Fmain.FTPcl.TransferType:=ftASCII;
   FTPcl.Connect;
  end;
 FTP_short_path:=copy(Ftp_path,Apos+1,length(Ftp_path)-Apos);
 Fmain.FTPcl.ChangeDir('/');
 except
  ShowMessage('Ошибка подключения к FTP серверу');
  FTP_use:=false;
 end;
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
// marksicons.free;
end;

procedure TFmain.FormActivate(Sender: TObject);
var  Ini: Tinifile;
     i,r:integer;
     ks:TKernelResampler;
     xy,xy1:Tpoint;
begin
 if start=false then exit;
 loadMarksIcons;
 LoadMaps;
 if FileExists(extractfilepath(paramstr(0))+'marks.xml')
  then CDSMarks.LoadFromFile(extractfilepath(paramstr(0))+'marks.xml');
 CDSMarks.Open;
 TilesLoad:=TStringList.Create;
 nilLastLoad.use:=false;
 notpaint:=true;
 Ini:=TiniFile.Create(extractfilepath(paramstr(0))+'SASPlanet.ini');
 SetDefoultMap;
 Application.OnMessage := DoMessageEvent;
 Application.HelpFile := ExtractFilePath(Application.ExeName)+'help.hlp';
 LenShow:=true;
 ban_pg_ld:=true;
 mWd2:=map.Width shr 1;
 mHd2:=map.Height shr 1;
 Dwn_list:=TStringList.Create;
 All_Dwn_Kb:=0;
 All_Dwn_Tiles:=0;
 generate_rocess:=false;
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
 sprb:=TBitmap32.Create;
 sprP:=TPicture.Create;
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

 LayerMapZap:=TBitmapLayer.Create(map.Layers);
 LayerMapZap.Bitmap.Width:=xhgpx;
 LayerMapZap.Bitmap.Height:=yhgpx;
 LayerMapZap.bitmap.DrawMode  := dmBlend;
 LayerMapZap.Visible:=false;

 LayerMapNal:=TBitmapLayer.Create(map.Layers);
 LayerMapNal.Bitmap.Width:=xhgpx;
 LayerMapNal.Bitmap.Height:=yhgpx;
 LayerMapNal.Bitmap.DrawMode:=dmBlend;
 LayerMapNal.Bitmap.CombineMode:=cmMerge;
 LayerMapNal.bitmap.Font.Charset:=RUSSIAN_CHARSET;
 LayerMapNal.Visible:=false;

 LayerMapWiki:=TBitmapLayer.Create(map.Layers);
 LayerMapWiki.Bitmap.Width:=xhgpx;
 LayerMapWiki.Bitmap.Height:=yhgpx;
 LayerMapWiki.Bitmap.DrawMode:=dmTransparent;//dmBlend;
// LayerMapWiki.Bitmap.CombineMode:=cmBlend;
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

 ks:=TKernelResampler.Create;
 ks.Kernel:=TLinearKernel.Create;
 LayerMinMap:=TBitmapLayer.Create(map.Layers);
 bmplist.Bitmap[2].Resampler:=ks;
 LayerMinMap.bitmap.Font.Charset:=RUSSIAN_CHARSET;
 LayerMinMap.Cursor:=crHandPoint;

 LayerStatBar:=TBitmapLayer.Create(map.Layers);
 LayerStatBar.Location:=floatrect(0,map.Height-17,map.Width,map.Height);
 LayerStatBar.Bitmap.Width:=map.Width;
 LayerStatBar.Bitmap.Height:=17;
 LayerStatBar.Bitmap.DrawMode:=dmBlend;
 LayerStatBar.bitmap.Font.Charset:=RUSSIAN_CHARSET;
 LayerStatBar.bitmap.Font.Name := 'arial';
 LayerStatBar.bitmap.Font.Size := 10;
// LayerStatBar.bitmap.Font.Style:=[fsBold];

 ShowMapName:=Ini.readBool('VIEW','ShowMapNameOnPanel',false);
 sm_map.width:=Ini.readInteger('VIEW','SmMapW',160);
 sm_map.height:=Ini.readInteger('VIEW','SmMapH',160);
 sm_map.z1mz2:=Ini.readInteger('VIEW','SmMapDifference',4);
 sm_map.Alpha:=Ini.readInteger('VIEW','SmMapAlpha',220);
 show_point:=Ini.readinteger('VIEW','ShowPointType',1);
 Zoom_Size:=Ini.ReadInteger('POSITION','zoom_size',1);
 KMLShow:=not(Ini.Readbool('POSITION','KMLShow',false));
 TBKMLShow.Checked:=KMLShow;
 NKMLShow.Checked:=KMLShow;
 ShowLine.Checked:=Ini.readbool('VIEW','line',true);
 LayerMapScale.Visible:=Ini.readbool('VIEW','showscale',false);
 N32.Checked:=LayerMapScale.Visible;
 ShowMiniMap.Checked:=Ini.readbool('VIEW','minimap',true);
 DefCache:=Ini.readinteger('VIEW','DefCache',2);
 ShowStatus.Checked:=Ini.readbool('VIEW','statusbar',true);
 zoom_mapzap:=Ini.readinteger('VIEW','MapZap',0);
 alpha_mapzap:=Ini.readinteger('VIEW','MapZapAlpha',110);
 zoom_line:=Ini.readinteger('VIEW','grid',0);
 mouse_inv:=Ini.readbool('VIEW','invert_mouse',false);
 source:=Ini.Readinteger('VIEW','SourceType',2);
 vo_ves_ecran:=Ini.Readbool('VIEW','FullScreen',false);
 Maximized:=Ini.Readbool('VIEW','Maximized',true);
 num_format:=Ini.Readinteger('VIEW','NumberFormat',0);
 CiclMap:=Ini.Readbool('VIEW','CiclMap',false);
 resampling:=Ini.Readinteger('VIEW','ResamlingType',1);
 llStrType:=Ini.Readinteger('VIEW','llStrType',0);
 Wikim_set.MainColor:=Ini.Readinteger('Wikimapia','MainColor',clWhite);
 Wikim_set.FonColor:=Ini.Readinteger('Wikimapia','FonColor',$000001);
 NKMLShow.ShortCut:=Ini.Readinteger('HOTKEY','KML',16459);
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

 gamman:=Ini.Readinteger('COLOR_LEVELS','gamma',50);
 contrastn:=Ini.Readinteger('COLOR_LEVELS','contrast',0);
 invertcolor:=Ini.ReadBool('COLOR_LEVELS','InvertColor',false);
 Ninvertcolor.Checked:=invertcolor;
 FTP_path:=Ini.ReadString('FTP','path','');
 FTP_login:=Ini.ReadString('FTP','login','');
 FTP_pass:=Ini.ReadString('FTP','pass','');
 FTP_port:=Ini.ReadInteger('FTP','port',21);
 FTP_trtype:=Ini.ReadInteger('FTP','TransfType',0);
 FTP_Read:=Ini.ReadBool('FTP','Read',true);
 FTP_Write:=Ini.ReadBool('FTP','Write',true);
 FTP_Use:=Ini.ReadBool('FTP','use',false);
 if FTP_use then Fmain.SetFTPconnect;
 GPS_COM:=Ini.ReadString('GPS','com','COM0');
 BaudRate:=Ini.ReadInteger('GPS','BaudRate',4800);
 ByteSize:=Ini.ReadInteger('GPS','ByteSize',4);
 Parity:=Ini.ReadInteger('GPS','Parity',0);
 StopBits:=Ini.ReadInteger('GPS','StopBits',0);
 setparams:=Ini.ReadBool('GPS','SetComParams',false);
 GPS_timeout:=Ini.ReadInteger('GPS','timeout',15);
 GPS_update:=Ini.ReadInteger('GPS','update',1000);
 GPS_enab:=Ini.ReadBool('GPS','enbl',false);
 GPS_Log:=Ini.Readbool('GPS','log',true);
 GPS_SizeStr:=Ini.ReadInteger('GPS','SizeStr',25);
 GPS_colorStr:=Ini.ReadInteger('GPS','ColorStr',clBlue32);
 GPS_popr:=extpoint(Ini.ReadFloat('GPS','popr_lon',0),Ini.ReadFloat('GPS','popr_lat',0));
 TBGPSconn.Checked:=GPS_enab;
 if GPS_enab then TBGPSconnClick(TBGPSconn);
 GPS_path:=Ini.ReadBool('GPS','path',true);
 TBGPSPath.Checked:=GPS_path;
 NGPSPath.Checked:=GPS_path;
 GPS_go:=Ini.ReadBool('GPS','go',true);
 TBGPSToPoint.Checked:=GPS_go;
 NGPSToPoint.Checked:=GPS_go;
 OldCpath_:=Ini.Readstring('PATHtoCACHE','GMVC','cache_old\');
 NewCpath_:=Ini.Readstring('PATHtoCACHE','SASC','cache\');
 ESCpath_:=Ini.Readstring('PATHtoCACHE','ESC','cache_ES\');
 POS.x:=Ini.ReadInteger('POSITION','x',zoom[zoom_size]div 2 +1);
 POS.y:=Ini.ReadInteger('POSITION','y',zoom[zoom_size]div 2 +1);
 oldPOS:=pos;
 InetConnect.userwinset:=Ini.Readbool('INTERNET','userwinset',true);
 InetConnect.uselogin:=Ini.Readbool('INTERNET','uselogin',true);
 InetConnect.Proxyused:=Ini.Readbool('INTERNET','used_proxy',false);
 InetConnect.proxystr:=Ini.Readstring('INTERNET','proxy','');
 InetConnect.loginstr:=Ini.Readstring('INTERNET','login','');
 InetConnect.passstr:=Ini.Readstring('INTERNET','password','');
 Nbackload.Checked:=Ini.Readbool('VIEW','back_load',true);
 Nanimate.Checked:=Ini.Readbool('VIEW','animate',true);
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

 KML_Path:=Ini.Readstring('KML','path','KML\');

 if not(FileExists(extractfilepath(paramstr(0))+'SASPlanet.ini')) then
  ZoomToolBar.Dock(TBDockLeft,bounds(0,0,1,1)); 
 TBiniLoadPositions(Fmain,extractfilepath(paramstr(0))+'SASPlanet.ini','PANEL_');

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
 TBKMLShowClick(TBKMLShow);
 notpaint:=false;
 zooming(Zoom_size,false);
 BmpList.Bitmap[0].CombineMode:=cmBlend;
 dwn:=false;
 Fsaveas.PageControl1.ActivePageIndex:=0;
 if InetConnect.proxyused then
  try
   EmbeddedWB1_.ProxySettings.Address:=copy(InetConnect.proxystr,1,PosEx(':',InetConnect.proxystr)-1);
   EmbeddedWB1_.ProxySettings.Port:=strtoint(copy(InetConnect.proxystr,PosEx(':',InetConnect.proxystr)+1,length(InetConnect.proxystr)-PosEx(':',InetConnect.proxystr)));
  except
   ShowMessage('Неверный формат записи прокси-сервера');
  end;
 if sparam then EmbeddedWB1_.Navigate('http://sasgis.ru/stat/index.html');
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
 Fmain.bmplist.Bitmap[2].Width:=256;
 Fmain.bmplist.Bitmap[2].Height:=256;
 Fmain.bmplist.Bitmap[2].Clear(Color32(clSilver) xor $00000000);
 pos_sm:=Point(pos.X div round(power(2,(zoom_size-sm_map.zoom))),pos.y div round(power(2,zoom_size-sm_map.zoom)));
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
       path:=ffpath(pos_sm.X+x128,pos_sm.y+y128,sm_map.zoom,m_t,false);
       bm.Clear(Color32(clSilver) xor $00000000);
       if (tileexists(path))
        then try
              LoadTilefromCache(bm,path);
             except
              bm.Clear(Color32(clSilver) xor $00000000);
             end
        else loadpre(bm,pos_sm.x+x128,pos_sm.y+y128,sm_map.zoom,m_t);
       Fmain.bmplist.Bitmap[2].Draw((128+x128)-d.x,(128+y128)-d.y,bm);
      end;
     inc(y128,256);
    end;
   inc(x128,256);
  end;

 for iLay:=0 to length(MapType)-1 do
  if (MapType[iLay].asLayer)and(MapType[iLay].ShowOnSmMap) then
  begin
   pos_sm:=Point(Pos.X div round(power(2,(zoom_size-sm_map.zoom))),Pos.y div round(power(2,zoom_size-sm_map.zoom)));
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
         path:=ffpath(pos_sm.X+x128,pos_sm.y+y128,sm_map.zoom,@MapType[iLay],false);
         bm.Clear(Color32(clSilver) xor $00000000);
         bm.Draw(0,0,bounds((128+x128)-d.x,(128+y128)-d.y,256,256),Fmain.bmplist.Bitmap[2]);
         if (tileexists(path))and(not((pos_sm.Y-y128<0)or(pos_sm.Y+y128>zoom[sm_map.zoom])) )
          then try
                LoadTilefromCache(bm,path);
               except
               end;
         Fmain.bmplist.Bitmap[2].Draw((128+x128)-d.x,(128+y128)-d.y,bm);
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
        try
         if (longint(sm_map.maptype)=0)
           then LoadTilefromCache(bmplist.Bitmap[2],ffpath(128,128,1,sat_map_both,false))
           else LoadTilefromCache(bmplist.Bitmap[2],ffpath(128,128,1,sm_map.maptype,false));
         for iLay:=0 to length(MapType)-1 do
          if (MapType[iLay].asLayer)and(MapType[iLay].ShowOnSmMap) then
            LoadTilefromCache(bmplist.Bitmap[2],ffpath(128,128,1,@MapType[iLay],false));
        except
         bmplist.Bitmap[2].Assign(DefoultMap);
        end;
        if (x=sm_map.width div 2)and(y=sm_map.height div 2)
         then sm_map.pos:=Point(round(pos.x*(sm_map.width/zoom[zoom_size])),round(pos.y*(sm_map.height/zoom[zoom_size])))
         else sm_map.pos:=Point(x,y);
        sm_map.dx:=round(sm_map.width*(map.Width/zoom[zoom_size]));
        sm_map.dy:=round(sm_map.height*(map.Height/zoom[zoom_size]));
       end;
 LayerMinMap.bitmap.Draw(bounds(5,5,sm_map.width,sm_map.height),bounds(0,0,256,256),bmplist.Bitmap[2]);
 gamma(LayerMinMap.bitmap);

 Polygon := TPolygon32.Create;
 Polygon.Antialiased:=true;
 Polygon.Add(FixedPoint((sm_map.pos.x-sm_map.dx div 2)+4-2,(sm_map.pos.y-sm_map.dy div 2)+4-2));
 Polygon.Add(FixedPoint((sm_map.pos.x-sm_map.dx div 2)+sm_map.dx+4+2,(sm_map.pos.y-sm_map.dy div 2)+4-2));
 Polygon.Add(FixedPoint((sm_map.pos.x-sm_map.dx div 2)+sm_map.dx+4+2,(sm_map.pos.y-sm_map.dy div 2)+sm_map.dy+4+2));
 Polygon.Add(FixedPoint((sm_map.pos.x-sm_map.dx div 2)+4-2,(sm_map.pos.y-sm_map.dy div 2)+sm_map.dy+4+2));
 with Polygon.Outline.Grow(Fixed(3.2/2),0.5) do
  begin
   FillMode:=pfWinding;
   DrawFill(LayerMinMap.bitmap,SetAlpha(clNavy32,(zoom_size-sm_map.zoom)*43));
  end;
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
 if sm_map.z1mz2>1 then LayerMinMap.bitmap.Draw(6,6,bmplist.Bitmap[4]);
 if zoom_size-sm_map.z1mz2>1 then LayerMinMap.bitmap.Draw(19,6,bmplist.Bitmap[5]);
 LayerMinMap.BringToFront;
end;

procedure TFmain.zooming(x:byte;move:boolean);
var w,w1,i:integer;
begin
 if generate_rocess then exit;
 if x<=1  then TBZoom_Out.Enabled:=false
          else TBZoom_Out.Enabled:=true;
 if x>=24 then TBZoomIn.Enabled:=false
          else TBZoomIn.Enabled:=true;
 NZoomIn.Enabled:=TBZoomIn.Enabled;
 NZoomOut.Enabled:=TBZoom_Out.Enabled;
 if (anim_zoom=1)or(generate_rocess){or(x=zoom_size)}or(dwn)or(x<1)or(x>24) then exit;
 generate_rocess:=true;
 anim_zoom:=1;
 labZoom.caption:=' '+inttostr(zoom_size)+'x ';
 labZoom.caption:=' '+inttostr(x)+'x ';
 RxSlider1.Value:=x-1;
 if zoom_size>x
  then begin
         zoom_in:=-1;
         w:=-24;
         w1:=-12;
         POS:=Point(trunc(pos.x/power(2,zoom_size-x)),trunc(pos.y/power(2,zoom_size-x)));
         if (move)and(abs(x-zoom_size)=1) then
           POS:=Point(pos.x+(mWd2-m_m.X)div 2,pos.y+(mHd2-m_m.y)div 2);
       end
  else begin
         zoom_in:=1;
         w:=12;
         w1:=6;
         POS:=Point(trunc(pos.x*power(2,x-zoom_size)),trunc(pos.y*power(2,x-zoom_size)));
         if (move)and(abs(x-zoom_size)=1) then
          begin
           POS:=Point(pos.x-(mWd2-m_m.X),pos.y-(mHd2-m_m.y));
          end;
       end;
 LayerMapNal.Bitmap.Clear(clBlack);
 LayerMapgps.Bitmap.Clear(clBlack);
 LayerMapZap.Bitmap.Clear(clBlack);
 LayerMapWiki.Visible:=false;
 btn_visib.Visible:=false;
 btn_close.Visible:=false;
 btn_ok.Visible:=false;
 btn_delpath.Visible:=false;
 if (abs(x-zoom_size)=1)and(Nanimate.Checked)
   then for i:=0 to 12 do
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
          application.ProcessMessages;
         end;
 btn_visib.Visible:=aoper=line;
 btn_close.Visible:=aoper=line;
 btn_ok.Visible:=aoper in [add_line,add_poly];
 btn_delpath.Visible:=aoper in [add_line,add_poly];
 zoom_size:=x;
 generate_rocess:=false;
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
 start:=true;
end;

procedure TFmain.FormClose(Sender: TObject; var Action: TCloseAction);
var Ini: Tinifile;
    i:integer;
begin
 close_:=true;
 SaveMaps;

 Ini:=TiniFile.Create(extractfilepath(paramstr(0))+'SASPlanet.ini');
 try
 Ini.WriteBool('VIEW','ShowMapNameOnPanel',ShowMapName);
 Ini.WriteInteger('POSITION','zoom_size',Zoom_Size);
 Ini.WriteInteger('POSITION','x',POS.x);
 Ini.WriteInteger('POSITION','y',POS.y);
 Ini.WriteInteger('POSITION','y',POS.y);
 Ini.Writebool('POSITION','KMLShow',KMLShow);
 Ini.Writebool('VIEW','line',ShowLine.Checked);
 Ini.Writeinteger('VIEW','DefCache',DefCache);
 ini.WriteInteger('VIEW','cachetype',MapType[0].cachetype);
 Ini.Writebool('VIEW','minimap',ShowMiniMap.Checked);
 Ini.Writebool('VIEW','statusbar',Showstatus.Checked);
 Ini.WriteInteger('VIEW','TilesOut',TilesOut);
 Ini.Writeinteger('VIEW','grid',zoom_line);
 Ini.Writebool('VIEW','invert_mouse',mouse_inv);
 Ini.Writebool('VIEW','back_load',Nbackload.Checked);
 Ini.Writebool('VIEW','animate',Nanimate.Checked);
 Ini.Writebool('VIEW','FullScreen',vo_ves_ecran);
 ini.WriteInteger('VIEW','FLeft',Fmain.Left);
 ini.WriteInteger('VIEW','FTop',Fmain.Top);
 ini.WriteInteger('VIEW','FWidth',Fmain.Width);
 ini.WriteInteger('VIEW','FHeight',Fmain.Height);
 Ini.WriteInteger('VIEW','SourceType',source);
 Ini.WriteInteger('VIEW','SmMapW',sm_map.width);
 Ini.WriteInteger('VIEW','SmMapH',sm_map.height);
 Ini.Writebool('VIEW','showscale',LayerMapScale.Visible);
 Ini.WriteInteger('VIEW','SmMapDifference',sm_map.z1mz2);
 Ini.WriteInteger('VIEW','SmMapAlpha',sm_map.alpha);
 Ini.WriteInteger('VIEW','ShowPointType',show_point);
 Ini.Writeinteger('VIEW','MapZap',zoom_mapzap);
 Ini.Writeinteger('VIEW','MapZapAlpha',alpha_mapzap);
 Ini.Writeinteger('VIEW','NumberFormat',num_format);
 Ini.Writebool('VIEW','Maximized',Fmain.WindowState=wsMaximized);
 Ini.Writebool('VIEW','CiclMap',CiclMap);
 Ini.Writeinteger('VIEW','ResamlingType',resampling);
 Ini.Writeinteger('VIEW','llStrType',llStrType);
 Ini.Writeinteger('Wikimapia','MainColor',Wikim_set.MainColor);
 Ini.Writeinteger('Wikimapia','FonColor',Wikim_set.FonColor);
 Ini.Writeinteger('HOTKEY','KML',NKMLShow.ShortCut);
 Ini.Writeinteger('HOTKEY','ZoomIn',NzoomIn.ShortCut);
 Ini.Writeinteger('HOTKEY','ZoomOut',NzoomOut.ShortCut);
 Ini.Writeinteger('HOTKEY','GoTo',N14.ShortCut);
 Ini.Writeinteger('HOTKEY','CalcRast',NCalcRast.ShortCut);
 Ini.Writeinteger('HOTKEY','Rect',NRECT.ShortCut);
 Ini.Writeinteger('HOTKEY','Polyg',NRegion.ShortCut);
 Ini.Writeinteger('HOTKEY','Coord',N41.ShortCut);
 Ini.Writeinteger('HOTKEY','Previous',N42.ShortCut);
 Ini.Writeinteger('HOTKEY','inet',NSRCinet.ShortCut);
 Ini.Writeinteger('HOTKEY','Cache',NSRCesh.ShortCut);
 Ini.Writeinteger('HOTKEY','CachInet',NSRCic.ShortCut);
 Ini.Writeinteger('HOTKEY','Showstatus',Showstatus.ShortCut);
 Ini.Writeinteger('HOTKEY','ShowLine',ShowLine.ShortCut);
 Ini.Writeinteger('HOTKEY','ShowMiniMap',ShowMiniMap.ShortCut);
 Ini.Writeinteger('HOTKEY','FoolSize',NFoolSize.ShortCut);
 Ini.Writeinteger('HOTKEY','GoToCur',NGoToCur.ShortCut);
 Ini.Writeinteger('HOTKEY','backload',Nbackload.ShortCut);
 Ini.Writeinteger('HOTKEY','animate',Nanimate.ShortCut);
 Ini.Writeinteger('HOTKEY','CiclMap',NCiclMap.ShortCut);
 Ini.Writeinteger('HOTKEY','ShowScale',N32.ShortCut);
 Ini.Writeinteger('HOTKEY','GPSconn',NGPSconn.ShortCut);
 Ini.Writeinteger('HOTKEY','GPSPath',NGPSPath.ShortCut);
 Ini.Writeinteger('HOTKEY','GPSToPoint',NGPSToPoint.ShortCut);
 Ini.Writeinteger('HOTKEY','SaveTreck',NSaveTreck.ShortCut);
 Ini.Writeinteger('HOTKEY','LoadSelFromFile',NLoadSelFromFile.ShortCut);
 Ini.Writeinteger('HOTKEY','InvertColor',Ninvertcolor.ShortCut);

 Ini.Writeinteger('COLOR_LEVELS','gamma',gamman);
 Ini.Writeinteger('COLOR_LEVELS','contrast',contrastn);
 Ini.WriteBool('COLOR_LEVELS','InvertColor',invertcolor);
 Ini.WriteString('FTP','login',FTP_login);
 Ini.WriteString('FTP','pass',FTP_pass);
 Ini.WriteInteger('FTP','port',FTP_port);
 Ini.WriteInteger('FTP','TransfType',FTP_trtype);
 Ini.WriteString('FTP','path',FTP_path);
 Ini.WriteBool('FTP','Read',FTP_Read);
 Ini.WriteBool('FTP','Write',FTP_Write);
 Ini.WriteBool('FTP','use',FTP_Use);

 if GPS_enab then Ini.WriteBool('GPS','enbl',true)
                else Ini.WriteBool('GPS','enbl',false);
 Ini.WriteBool('GPS','path',GPS_path);
 Ini.WriteBool('GPS','go',GPS_go);
 Ini.WriteString('GPS','COM',GPS_com);
 Ini.WriteInteger('GPS','BaudRate',BaudRate);
 Ini.WriteInteger('GPS','ByteSize',ByteSize);
 Ini.WriteInteger('GPS','Parity',Parity);
 Ini.WriteInteger('GPS','StopBits',StopBits);
 Ini.WriteBool('GPS','SetComParams',setparams);
 Ini.WriteFloat('GPS','popr_lon',GPS_popr.x);
 Ini.WriteFloat('GPS','popr_lat',GPS_popr.y);
 Ini.Writeinteger('GPS','update',GPS_update);
 Ini.WriteBool('GPS','log',GPS_Log);
 Ini.WriteInteger('GPS','SizeStr',GPS_SizeStr);
 Ini.WriteInteger('GPS','ColorStr',GPS_colorStr);
 Ini.Writestring('PATHtoCACHE','GMVC',OldCpath_);
 Ini.Writestring('PATHtoCACHE','SASC',NewCpath_);
 Ini.Writestring('PATHtoCACHE','ESC',ESCpath_);
 Ini.Writebool('INTERNET','userwinset',InetConnect.userwinset);
 Ini.Writebool('INTERNET','uselogin',InetConnect.uselogin);
 Ini.Writebool('INTERNET','used_proxy',InetConnect.Proxyused);
 Ini.Writestring('INTERNET','proxy',InetConnect.proxystr);
 Ini.Writestring('INTERNET','login',InetConnect.loginstr);
 Ini.Writestring('INTERNET','password',InetConnect.passstr);
 Ini.Writebool('NPARAM','stat',sparam);

 i:=1;
 while Ini.ReadFloat('HIGHLIGHTING','pointx_'+inttostr(i),2147483647)<>2147483647 do
  begin
   Ini.DeleteKey('HIGHLIGHTING','pointx_'+inttostr(i));
   Ini.DeleteKey('HIGHLIGHTING','pointy_'+inttostr(i));
   inc(i);
  end;
 if length(poly_save)>0 then
  begin
   Ini.WriteInteger('HIGHLIGHTING','zoom',poly_zoom_save);
   for i:=1 to length(poly_save) do
    begin
     Ini.WriteFloat('HIGHLIGHTING','pointx_'+inttostr(i),poly_save[i-1].x);
     Ini.WriteFloat('HIGHLIGHTING','pointy_'+inttostr(i),poly_save[i-1].y);
    end;
  end;
 Ini.Writestring('KML','path',KML_Path);
 TBiniSavePositions(Fmain,extractfilepath(paramstr(0))+'SASPlanet.ini','PANEL_');

 except
 end;
 Ini.Free;
 if THGPS<>nil then THGPS.Terminate;
// ShowMessage('4');
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
 generate_im(nilLastLoad,'');
end;

procedure TFmain.mapMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var ll,lt:integer;
begin
 if (layer<>LayerMinMap)and(Button=mbLeft)and(aoper<>movemap) then
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
   if (aoper=add_point) then FAddPoint.show_(self,extPoint(X2Lon(x),Y2Lat(y)),true);
   if (aoper in [add_line,add_poly]) then begin
                             setlength(add_line_arr,length(add_line_arr)+1);
                             add_line_arr[length(add_line_arr)-1]:=extpoint(X2Lon(X),Y2Lat(Y));
                             drawPath(add_line_arr,true,SetAlpha(ClRed32, 150),SetAlpha(ClWhite32, 50),3,aoper=add_poly);
                            end;
   exit;
  end;
 if dwn then exit;
// if (not(layer is TWikiLayer))and(not(layer is TKMLLayer)) then curBuf:=layer.Cursor;
 if (button=mbRight)and(layer=LayerMinMap) then
  begin
   map.PopupMenu:=PopupMSmM;
   exit;
  end;
 map.PopupMenu:=nil;
 if (layer=LayerMinMap)and(layer.Visible)and(button=mbLeft) then
  begin
   dblclik:=false;
   move:=Point(x,y);
   ll:=round(TBitmapLayer(Layer).Location.Left);
   lt:=round(TBitmapLayer(Layer).Location.top);
   if (x<ll+5)
    then sm_map.size_dw:=true
    else
   if (x>ll+6)and(x<ll+17)and(y>lt+5)and(y<lt+15)
    then begin
          sm_map.zooming:=true;
          if sm_map.z1mz2>1 then dec(sm_map.z1mz2);
          sm_im_reset(sm_map.width div 2,sm_map.height div 2);
         end
    else
   if (x>ll+19)and(x<ll+33)and(y>lt+5)and(y<lt+15)
    then begin
          sm_map.zooming:=true;
          if zoom_size-sm_map.z1mz2>1 then inc(sm_map.z1mz2);
          sm_im_reset(sm_map.width div 2,sm_map.height div 2);
         end
    else
   if (x>ll+5)and(y>lt) then
         begin
          sm_map.m_dwn:=true;
          sm_im_reset(round(x-(TBitmapLayer(Layer).Location.Left+5)),round(y-(TBitmapLayer(Layer).Location.top)));
          exit;
         end;
   exit;
  end;
 if (anim_zoom=1)or(button=mbMiddle) then exit;
 if (Button=mbright)and(aoper=movemap)and(not dwn)
  then begin
        m_up:=point(x,y);
        MouseOnMyReg(PWL,ExtPoint(X2Lon(x),Y2Lat(y)));
        NMarkEdit.Visible:=(PWL.num>0);
        NMarkDel.Visible:=(PWL.num>0);
        NMarkSep.Visible:=(PWL.num>0);
        NMarkOper.Visible:=(PWL.num>0);
        map.PopupMenu:=PopupMenu1;
       end
  else begin
        dwn:=true;
        btn_close.Visible:=false;
        btn_visib.Visible:=false;
        btn_ok.Visible:=false;
        btn_delpath.Visible:=false;
       end;
 move:=Point(x,y);
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
  V := VarArrayCreate([0, 0], varVariant);
  V[0] := Text;
  Document.Write(PSafeArray(TVarData(v).VArray));
  Document.Close;
end;

procedure TFmain.mapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var i:integer;
    PWL:TResObj;
    stw:String;
begin
 dwn:=false;
 if (((aoper<>movemap)and(Button=mbLeft))or
     ((aoper=movemap)and(Button=mbRight)))and(layer<>LayerMinMap) then exit;
 m_up:=move;
 if (anim_zoom=1) then exit;
 if (layer=LayerMinMap)and(layer.Visible) then
   begin
    sm_map.m_dwn:=false;
    if (not(sm_map.size_dw))and(not(sm_map.zooming))and((x>LayerMinMap.Location.Left+5)and(y>LayerMinMap.Location.Top))
     then begin
           if sm_map.zoom>1 then
            pos:=Point(pos.x+round((-(128-(sm_map.pos.x*(256/sm_map.width))))*power(2,zoom_size-sm_map.zoom)),
                 pos.y+round((-(128-(sm_map.pos.y*(256/sm_map.height))))*power(2,zoom_size-sm_map.zoom)))
            else pos:=Point(round(sm_map.pos.X*(256/sm_map.height)*power(2,zoom_size-sm_map.zoom)),round((sm_map.pos.Y*(256/sm_map.height))*power(2,zoom_size-sm_map.zoom)));
           m_up:=Point(x,y);
           Fmain.generate_im(nilLastLoad,'');
           //sm_im_reset(sm_map.width div 2,sm_map.height div 2);
          end
     else begin
           sm_map.size_dw:=false;
           sm_map.zooming:=false;
          end;
    toSh;
    if aoper=reg then drawReg;
    if aoper=line then drawLineCalc;
    if aoper=rect then drawrect(Shift);
    //layer.Cursor:=curBuf;
    exit;
   end;

{ if rect then
   begin
    map.Refresh;
    TBmoveClick(sender);
    if (zoom_line in [99,0])or(zoom_line<zoom_size)
     then d256:=256
     else d256:=256 div round(power(2,zoom_line-zoom_size));
    if Shift = [ssCtrl] then
      begin
       move.X:=(pos.X-(mWd2-move.X));
       move.X:=mWd2-(pos.x-(move.X-(move.X mod d256)));
       move.y:=(pos.y-(mHd2-move.y));
       move.y:=mHd2-(pos.y-(move.y-(move.y mod d256)));
       X:=(pos.X-(mWd2-X));
       X:=mWd2-(pos.x-((X+d256)-((X+d256) mod d256)));
       y:=(pos.y-(mHd2-y));
       y:=mHd2-(pos.y-((y+d256)-((y+d256) mod d256)));
      end;
    if x<move.X then x:=move.X;
    if y<move.Y then y:=move.Y;
    fsaveas.Show_(zoom_size,[ExtPoint(X2Lon(move.x),Y2Lat(move.y)),ExtPoint(X2Lon(x),Y2Lat(move.y)),ExtPoint(X2Lon(x),Y2Lat(y)),ExtPoint(X2Lon(move.x),Y2Lat(y)),ExtPoint(X2Lon(move.x),Y2Lat(move.y))]);
    toSh;
    layer.Cursor:=curBuf;
    exit;
   end; }
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
 if {(not(reg or line or rect))and}((move.x<>m_up.x)or(move.y<>m_up.y))
  then generate_im(nilLastLoad,'');
 if aoper=reg then drawReg;
 if aoper=line then drawLineCalc;
 if aoper=rect then drawrect(Shift);
 toSh;
 map.Refresh;
 if (y=move.y)and(x=move.x)and(LayerMapWiki.Visible)and(aoper=movemap)and(button=mbLeft) then
  begin
    layer.Cursor:=curBuf;
    PWL.num:=0;
    PWL.width:=0;
    MouseOnReg(PWL,Point(x+(pr_x-mWd2),y+(pr_y-mHd2)));
    MouseOnRegKML(PWL,Point(x+(pr_x-mWd2),y+(pr_y-mHd2)));
    if pwl.num>0 then
     begin
      stw:='<HTML><BODY>';
      for i:=0 to pwl.num-1 do stw:=stw+pwl.descr[i]+'<HR>';
      stw:=stw+'</BODY></HTML>';
      TextToWebBrowser(stw,Fbrowser.EmbeddedWB1);
      Fbrowser.Visible:=true;
     end;
    exit;
  end;
end;

procedure TFmain.mapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var i:integer;
    nms:string;
    hinth:integer;
begin
 if generate_rocess then exit;
 sleep(1);
 if (aoper=rect)and(rect_dwn)and(not(ssRight in Shift))and(layer<>LayerMinMap)
         then begin
               rect_arr[1]:=extPoint(X2Lon(x),Y2Lat(y));
               drawRect(Shift);
              end;
 if dwn then layer.Cursor:=3;
 if (layer=LayerMinMap)and(layer.Visible) then
  begin
   if (x<TBitmapLayer(Layer).Location.Left+5)and(map.Cursor<>crSizeNWSE) then LayerMinMap.Cursor:=crSizeNWSE
                                                                        else LayerMinMap.Cursor:=crHandPoint;
   if (sm_map.size_dw)
    then if (map.Width-x-5)>40 then
         begin
          sm_map.width:=(map.Width-x-5);
          sm_map.height:=(map.Width-x-5);
          sm_im_reset(sm_map.width div 2,sm_map.height div 2)
         end else
    else if (x>TBitmapLayer(Layer).Location.Left+5)and(y>TBitmapLayer(Layer).Location.top+5)and(sm_map.m_dwn)
          then sm_im_reset(round(x-(TBitmapLayer(Layer).Location.Left+5)),round(y-(TBitmapLayer(Layer).Location.top)));
   exit;
  end;
 if vo_ves_ecran then begin
                       TBDock.Visible:=y<10;
                       TBDockLeft.Visible:=x<10;
                       TBDockBottom.Visible:=y>Map.Height-10;
                       TBDockRight.Visible:=x>Map.Width-10;
                      end;
 if anim_zoom=1 then exit;
 if dwn then begin
              LayerMap.Location:=floatrect(bounds(mWd2-pr_x-(move.X-x),mHd2-pr_y-(move.Y-y),hg_x shl 8,hg_y shl 8));
              if (LayerMapZap.Visible)and(zoom_mapzap>0) then LayerMapZap.Location:=floatrect(bounds(mWd2-pr_x-(move.X-x),mHd2-pr_y-(move.Y-y),hg_x shl 8,hg_y shl 8));
              if (LayerMapNal.Visible)and(aoper<>movemap) then LayerMapNal.Location:=floatrect(bounds(mWd2-pr_x-(move.X-x),mHd2-pr_y-(move.Y-y),hg_x shl 8,hg_y shl 8));
              if (LayerMapGPS.Visible)and(GPS_enab) then LayerMapGPS.Location:=floatrect(bounds(mWd2-pr_x-(move.X-x),mHd2-pr_y-(move.Y-y),hg_x shl 8,hg_y shl 8));
              if LayerMapWiki.Visible then LayerMapWiki.Location:=floatrect(bounds(mWd2-pr_x-(move.X-x),mHd2-pr_y-(move.Y-y),hg_x shl 8,hg_y shl 8));
             end
        else m_m:=point(x,y);
 if not(dwn) then toSh;

 if (h<>nil)and(not ShowActivHint) then
  begin
   H.ReleaseHandle;
   oldLayerIndex:=0;
   Layer.Cursor:=curBuf;
   ShowActivHint:=false;
  end;
 ShowActivHint:=false;
 if not(dwn)and(LayerMapWiki.Visible)and((moveTrue.x<>X) or (moveTrue.y<>y)) then
  begin
   PWL.num:=0;
   PWL.width:=0;
   MouseOnReg(PWL,Point(x+(pr_x-mWd2),y+(pr_y-mHd2)));
   MouseOnRegKML(PWL,Point(x+(pr_x-mWd2),y+(pr_y-mHd2)));
   if (PWL.num>0) then
    begin
     if h<>nil then H.ReleaseHandle;
     nms:='';
     for i:=0 to PWL.num-1 do begin
                               if i>0 then nms:=nms+#13#10;
                               nms:=nms+PWL.name[i];
                              end;
     oldLayerIndex:=layer.Index;
     H:=THintWindow.Create(Fmain);
     H.Brush.Color:=clInfoBk;
     H.Font.Charset:=RUSSIAN_CHARSET;
     layer.Cursor:=crHandPoint;
     H.ActivateHint(Bounds(Mouse.CursorPos.x+13,Mouse.CursorPos.y-13,PWL.width,14*PWL.num),nms);
     ShowActivHint:=true;
    end;
  end;
 if not(dwn)and(show_point<>3)and((moveTrue.x<>X) or (moveTrue.y<>y)) then
  begin
   PWL.num:=0;
   PWL.width:=0;
   MouseOnMyReg(PWL,ExtPoint(X2Lon(x),Y2Lat(y)));
   if (PWL.num>0) then
    begin
     if h<>nil then H.ReleaseHandle;
     nms:='';
     if PWL.descr[0]<>'' then hinth:=28*PWL.num else hinth:=14*PWL.num;
     nms:=PWL.name[0]+#13#10+PWL.descr[0];
     PWL.width:=TBitmap32.Create.TextWidthW(PWL.name[0])+10;
     if PWL.width<TBitmap32.Create.TextWidthW(PWL.descr[0])+10 then
        PWL.width:=TBitmap32.Create.TextWidthW(PWL.descr[0])+10;
     oldLayerIndex:=layer.Index;
     H:=THintWindow.Create(Fmain);
     H.Brush.Color:=clInfoBk;
     H.Font.Charset:=RUSSIAN_CHARSET;
     layer.Cursor:=crHandPoint;
     H.ActivateHint(Bounds(Mouse.CursorPos.x+13,Mouse.CursorPos.y-13,PWL.width,hinth),nms);
     ShowActivHint:=true;
    end;
  end;
 moveTrue:=point(x,y);
end;

procedure TFmain.ZoomToolBarDockChanging(Sender: TObject; Floating: Boolean; DockingTo: TTBDock);
begin
 if (DockingTo=TBDockLeft)or(DockingTo=TBDockRight)
   then RxSlider1.Orientation:=RxSlider.soVertical
   else RxSlider1.Orientation:=RxSlider.soHorizontal;
end;


procedure TFmain.RxSlider1MouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
 zooming(RxSlider1.Value+1,false);
end;

procedure TFmain.RxSlider1Change(Sender: TObject);
begin
 labZoom.Caption:=' '+inttostr(RxSlider1.Value+1)+'x ';
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
 FAddPoint.show_(self,extPoint(X2Lon(m_up.x),Y2Lat(m_up.y)),true);
end;

procedure TFmain.N20Click(Sender: TObject);
var s:string;
    jp:TBitmap32;
begin
 s:=ffpath(X2absX(pos.x-(mWd2-move.x),zoom_size),pos.y-(mHd2-move.y),zoom_size,sat_map_both,false);
 jp:=TBitmap32.Create;
 LoadTilefromCache(jp,s);
 Clipboard.Assign(jp);
end;

procedure TFmain.N30Click(Sender: TObject);
var ll:TExtendedPoint;
begin
 ll:=GPos2LonLat(mouseXY2Pos(Point(move.X,move.Y)),zoom_size,sat_map_both);
 Clipboard.AsText:=lon2str(ll.x)+' ';
 Clipboard.AsText:=Clipboard.AsText+lat2str(ll.y);
end;

procedure TFmain.N15Click(Sender: TObject);
var s:string;
begin
 s:=ffpath(X2AbsX(pos.x-(mWd2-move.x),zoom_size),pos.y-(mHd2-move.y),zoom_size,sat_map_both,false);
 Clipboard.AsText:=s
end;

procedure TFmain.N21Click(Sender: TObject);
var path:string;
    APos:TPoint;
    AMapType:PMapType;
begin
 if TMenuItem(sender).Tag=0 then AMapType:=sat_map_both
                            else AMapType:=PMapType(TMenuItem(sender).Tag);
 APos:=ConvertPosM2M(pos,zoom_size,sat_map_both,AMapType);
 path:=ffpath(APos.x-(mWd2-m_up.x),APos.y-(mHd2-m_up.y),zoom_size,AMapType,false);
 if (THLdMap_RightCl_1=nil)and((not(tileExists(path)))or
    (MessageBox(handle,pchar('Файл '+path+' уже есть у вас в кеше.'+#13#10+'Заменить этот файл вновь скачанным?'),pchar('Внимание!'),36)=IDYES)) then
   begin
    THLdMap_RightCl_1:=ThreadAllLoadMap.Create(False,[Point(Apos.x-(mWd2-m_up.x),Apos.y-(mHd2-m_up.y))],1,true,false,false,zoom_size,AMapType,date);
    THLdMap_RightCl_1.FreeOnTerminate:=true;
    THLdMap_RightCl_1.OnTerminate:=ThreadDone;
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
   end;
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE);
end;

procedure TFmain.NopendirClick(Sender: TObject);
begin
 ShellExecute(0,'open',PChar(ffpath(pos.x-(mWd2-m_m.x),pos.y-(mHd2-m_m.y),zoom_size,sat_map_both,false)),nil,nil,SW_SHOWNORMAL);
end;

procedure TFmain.N25Click(Sender: TObject);
var s:string;
    i:integer;
begin
 s:=ffpath(pos.x-(mWd2-m_m.x),pos.y-(mHd2-m_m.y),zoom_size,sat_map_both,false);
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
 s:=ffpath(APos.x-(mWd2-m_up.x),APos.y-(mHd2-m_up.y),zoom_size,AMapType,false);
 if (MessageBox(handle,pchar('Вы действительно хотите удалить изображение '+s+'?'),pchar('Внимание!'),36)=IDYES)
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
   THLoadMap1:=ThreadAllLoadMap.Create(False,[],4,NSRCinet.Checked,false,false,zoom_size,sat_map_both,date);
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
 TBRectSaveClick(self);
end;

procedure TFmain.TBRECTClick(Sender: TObject);
begin
 TBRectSave.ImageIndex:=6;
 TBRectSave.Checked:=true;
 TBRectSaveClick(self);
end;

procedure TFmain.TBRectSaveClick(Sender: TObject);
begin
 if TBRectSave.ImageIndex=6 then setalloperationfalse(rect)
                            else setalloperationfalse(reg)
end;

procedure TFmain.TBItem1Click(Sender: TObject);
begin
 if length(poly_save)>0 then fsaveas.Show_(poly_zoom_save,poly_save)
                        else showmessage('Сначала необходимо выделить хотябы один раз область!');
end;

//карта заполнения в основном окне
procedure TFmain.TBMapZapPopup(Sender: TTBCustomItem; FromLink: Boolean);
var i:integer;
begin
 if zoom_mapzap=0 then TBMapZap.Items[0].Checked:=true;
 for i:=1 to 8 do
  if zoom_size+i<24 then begin
                          TBMapZap.Items[i].Caption:='В главном окне для х'+inttostr(zoom_size+i);
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
                          N17.Items[i].Caption:='Для х'+inttostr(zoom_size+i);
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

procedure TFmain.btn_CloseClick(Sender: TObject);
begin
 if length(length_arr)>0 then setlength(length_arr,length(length_arr)-1);
 drawLineCalc;
end;

procedure TFmain.btn_visibClick(Sender: TObject);
begin
 LenShow:=not(LenShow);
 drawLineCalc;
end;

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
 if generate_rocess then exit;
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
begin
 ld:=1;
 if InetConnect.proxyused then
  try
   EmbeddedWB1_.ProxySettings.Address:=copy(InetConnect.proxystr,1,PosEx(':',InetConnect.proxystr)-1);
   EmbeddedWB1_.ProxySettings.Port:=strtoint(copy(InetConnect.proxystr,PosEx(':',InetConnect.proxystr)+1,length(InetConnect.proxystr)-PosEx(':',InetConnect.proxystr)));
  except
   ShowMessage('Неверный формат записи прокси-сервера');
  end;
 EmbeddedWB1_.Navigate('http://maps.google.ru/maps?f=q&hl=ru&geocode=&q='+NewText);
end;

procedure TFmain.EmbeddedWB1_DocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
var s,slat,slon,sname:string;
    i,j,k:integer;
    lat,lon:real;
begin
 if ld<2 then begin
                inc(ld);
                exit;
              end;
 ld:=1;
 s:=EmbeddedWB1_.DocumentSource;
 for i:=1 to length(s) do
  if copy(s,i,17)='viewport:{center:'
   then begin
         j:=i;
         Break;
        end;
 j:=j+22;
 slat:='';
 while s[j]<>',' do
  begin
   slat:=slat+s[j];
   inc(j);
  end;
 j:=j+5;
 slon:='';
 while s[j]<>'}' do
  begin
   slon:=slon+s[j];
   inc(j);
  end;
 try
  lat:=Fmain.str2r(slat);
  lon:=Fmain.str2r(slon);
 except
  ShowMessage('Ошибка при конвертации координат!'+#13#10+'Возможно отсутствует подключение к интернету,'+#13#10+'или google изменил формат.');
  FGoto.Enabled:=true;
  exit;
 end;
 j:=length(s);
 for i:=1 to length(s) do
  if copy(s,i,7)='laddr:"'
   then begin
         j:=i;
         Break;
        end;
 for i:=j+6 to length(s) do
  if copy(s,i,2)='",'
   then begin
         k:=i;
         Break;
        end;
 if j=length(s) then
  begin
   ShowMessage('Ничего не найдено');
   FGoto.Enabled:=true;
   exit;
  end;
 sname:=copy(s,j+6,k-j-5);
 FGoto.Enabled:=true;
 FGoto.Close;
 Fmain.toPos(lat,lon,zoom_size{CBzoom.ItemIndex+1},true);
 ShowMessage(sname);
end;

procedure TFmain.TBSubmenuItem1Click(Sender: TObject);
begin
 FGoto.Visible:=true;
 Fmain.Enabled:=false;
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
 NShowGran.Items[1].Caption:='Активный масштаб (х'+inttostr(zoom_size)+')';
 for i:=2 to 7 do
  if zoom_size+i-2<24 then begin
                            NShowGran.Items[i].Caption:='Для х'+inttostr(zoom_size+i-2);
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
   THGPS:=ThreadGPS.Create(False,GPS_COM);
   THGPS.FreeOnTerminate:=true;
   THGPS.OnTerminate:=ThreadGPSDone;
   THGPS.Priority:=tpLower;
  end
  else if THGPS<>nil then THGPS.Terminate;
end;

procedure TFmain.TBGPSPathClick(Sender: TObject);
begin
 if sender is TTBSubMenuitem then NGPSPath.Checked:=TTBitem(sender).Checked
                             else TBGPSPath.Checked:=TMenuItem(sender).Checked;
 GPS_path:=TBGPSPath.Checked;
 if not(GPS_path) then setlength(GPS_arr,0);
end;

procedure TFmain.TBGPSToPointClick(Sender: TObject);
begin
 if sender is TTBitem then NGPSToPoint.Checked:=TTBitem(sender).Checked
                      else TBGPSToPoint.Checked:=TMenuItem(sender).Checked;
 GPS_go:=TBGPSToPoint.Checked;
end;

procedure TFmain.TBItem4Click(Sender: TObject);
begin
 TBmoveClick(Sender);
 Fmain.Enabled:=false;
 FSelLonLat.Visible:=true;
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

procedure TFmain.TBKMLShowClick(Sender: TObject);
begin
 KMLShow:=not(KMLShow);
 NKMLShow.Checked:=KMLShow;
 NKMLShow1.Checked:=KMLShow;
 TBKMLShow.Checked:=KMLShow;
 KMLexplorer1.Enabled:=KMLShow;
 if KMLShow then loadKML
            else FKMLExplorer.Hide;
 generate_im(nilLastLoad,'');
end;

procedure TFmain.TBItem3Click(Sender: TObject);
var F:TextFile;
    i:integer;
    SaveDlg: TSaveDialog;
begin
Fprogress2.Visible:=true;
fprogress2.ProgrInfo.Lines[0]:='Сожранение трека...';
Fprogress2.ProgressBar1.Max:=100;
SaveDlg := TSaveDialog.Create(Fprogress2);
SaveDlg.DefaultExt:='*.kml';
SaveDlg.Filter:='Формат KML|*.kml';
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
 if length(GPS_arr)>1 then FaddLine.show_(self,GPS_arr,true)
                      else ShowMessage('Необходимый набор точек отсутствует!');
end;

procedure TFmain.Google1Click(Sender: TObject);
var Apos:tExtendedPoint;
begin
 Apos:=GPos2LonLat(pos,zoom_size,sat_map_both);
 Clipboard.AsText:='http://maps.google.com/?ie=UTF8&ll='+R2StrPoint(Apos.y)+','+R2StrPoint(Apos.x)+'&spn=57.249013,100.371094&t=h&z='+inttostr(zoom_size-1);
end;

procedure TFmain.YaLinkClick(Sender: TObject);
var Apos,AposLT,AposRD:tExtendedPoint;
begin
 Apos:=GPos2LonLat(pos,zoom_size,sat_map_both);
 AposLT:=GPos2LonLat(Point(pos.X-mWd2,pos.Y-mHd2),zoom_size,sat_map_both);
 AposRD:=GPos2LonLat(Point(pos.X+mWd2,pos.Y+mHd2),zoom_size,sat_map_both);
 Clipboard.AsText:='http://beta-maps.yandex.ru/?ll='+R2StrPoint(round(Apos.x*100000)/100000)+'%2C'+R2StrPoint(round(Apos.y*100000)/100000)+'&spn='+R2StrPoint(abs(AposLT.x-AposRD.x))+'%2C'+R2StrPoint(abs(AposLT.y-AposRD.y))+'&l=sat';
end;

procedure TFmain.kosmosnimkiru1Click(Sender: TObject);
var Apos:tExtendedPoint;
begin
 Apos:=GPos2LonLat(pos,zoom_size,sat_map_both);
 Clipboard.AsText:='http://kosmosnimki.ru/?x='+R2StrPoint(Apos.x)+'&y='+R2StrPoint(Apos.y)+'&z='+inttostr(zoom_size-1)+'&fullscreen=false&mode=satellite';
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
   LayerMapZap.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
   LayerMapNal.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
   LayerMapGPS.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
   LayerMapWiki.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
   LayerMapScale.location:=floatrect(bounds(mWd2-145,mHd2-145,290,290));
   btn_close.Visible:=false;
   btn_visib.Visible:=false;
   btn_ok.Visible:=false;
   btn_delpath.Visible:=false;
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
   fprogress2.ProgrInfo.Lines[0]:='Загрузка выделения...';
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
var s,slat,slon:string;
    i,j:integer;
    lat,lon:real;
begin
 if InetConnect.proxyused then
  try
   IdHTTP_YaSrch.ProxyParams.ProxyServer:=copy(InetConnect.proxystr,1,PosEx(':',InetConnect.proxystr)-1);
   IdHTTP_YaSrch.ProxyParams.ProxyPort:=strtoint(copy(InetConnect.proxystr,PosEx(':',InetConnect.proxystr)+1,length(InetConnect.proxystr)-PosEx(':',InetConnect.proxystr)));
  except
   ShowMessage('Неверный формат записи прокси-сервера');
  end;
 i:=PosEx(' ',NewText);
 while i<>0 do
  begin
   NewText[i]:='+';
   i:=PosEx(' ',NewText,i);
  end;           //'http://wikimapia.org/sys/search4/?q=
 s:=IdHTTP_YaSrch.Get('http://beta-maps.yandex.ru/?text='+AnsiToUtf8(NewText)+'"' );
 if PosEx(AnsiToUtf8('Искомая комбинация'),s)>0 then
  begin
   ShowMessage('Искомая комбинация на карте не встречается.');
   exit;
  end;
 i:=PosEx('this,{lng:',s);
 j:=PosEx(',lat:',s,i);
 slon:=Copy(s,i+10,j-(i+10));
 i:=PosEx('}',s,j);
 slat:=Copy(s,j+5,i-(j+5));

 try
  lat:=Fmain.str2r(slat);
  lon:=Fmain.str2r(slon);
 except
  ShowMessage('Ошибка при конвертации координат!'+#13#10+'Возможно отсутствует подключение к интернету,'+#13#10+'или Яндекс изменил формат.');
  exit;
 end;
 Fmain.toPos(lat,lon,zoom_size,true);
 ShowMessage('Место найденное по запросу "'+NewText+'"');
end;

procedure TFmain.KMLexplorer1Click(Sender: TObject);
begin
 FKMLExplorer.Show;
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
 if (aoper=movemap)and(dblclik=true) then
  begin
   r:=TImage32(Sender).ScreenToClient(Mouse.CursorPos);
   POS:=Point(pos.x+(r.x-mWd2),pos.y+(r.y-mHd2));
   generate_im(nilLastLoad,'');
  end;
 dblclik:=true;
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

procedure TFmain.btn_okClick(Sender: TObject);
begin
 if aoper=add_Poly
    then FaddPoly.show_(self,add_line_arr,true)
    else FaddLine.show_(self,add_line_arr,true);
end;


procedure TFmain.NMarkEditClick(Sender: TObject);
var arrLL:PArrLL;
    arLL:array of TExtendedPoint;
    ms:TMemoryStream;
    i:integer;
begin
 CDSmarks.Filter:='id='+PWL.numid[0];
 CDSmarks.Filtered:=true;
 CDSmarks.First;
 if not(CDSmarks.Eof) then
  begin
   ms:=TMemoryStream.Create;
   TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
   GetMem(arrLL,ms.size);
   SetLength(arLL,ms.size div 24);
   ms.Position:=0;
   ms.ReadBuffer(arrLL^,ms.size);
   for i:=0 to length(arLL)-1 do
    arLL[i]:=arrLL^[i];
   if ms.Size=24 then FaddPoint.show_(self,arLL[0],false);
   if (ms.Size>24) then
    if compare2EP(arLL[0],arLL[length(arLL)-1]) then FaddPoly.show_(self,arLL,false)
                                                else FaddLine.show_(self,arLL,false);
   freeMem(arrLL);
   SetLength(arLL,0);
   ms.Free;
  end;
end;

procedure TFmain.NMarkDelClick(Sender: TObject);
begin
 CDSmarks.Filter:='id='+PWL.numid[0];
 CDSmarks.Filtered:=true;
 CDSmarks.First;
 if not(CDSmarks.Eof) then
  begin
   if MessageBox(handle,pchar('Вы действительно хотите это удалить?'),pchar('Внимание!'),36)=IDNO
    then exit;
   CDSmarks.Delete;
   CDSmarks.ApplyRange;
   CDSmarks.MergeChangeLog;
   CDSmarks.SaveToFile(extractfilepath(paramstr(0))+'marks.xml');
  end;
 generate_im(nilLastLoad,'');
end;

procedure TFmain.NMarksBarShowClick(Sender: TObject);
begin
 TBMarksToolBar.Visible:=NMarksBarShow.Checked;
end;

procedure TFmain.btn_delpathClick(Sender: TObject);
begin
 if length(add_line_arr)>0 then setlength(add_line_arr,length(add_line_arr)-1);
 drawPath(add_line_arr,true,SetAlpha(ClRed32, 150),SetAlpha(ClWhite32, 50),3,aoper=add_poly);
end;

procedure TFmain.NMarkOperClick(Sender: TObject);
var arrLL:PArrLL;
    arLL:array of TExtendedPoint;
    ms:TMemoryStream;
    i:integer;
begin
 CDSmarks.Filter:='id='+PWL.numid[0];
 CDSmarks.Filtered:=true;
 CDSmarks.First;
 if not(CDSmarks.Eof) then
  begin
   ms:=TMemoryStream.Create;
   TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
   GetMem(arrLL,ms.size);
   SetLength(arLL,ms.size div 24);
   ms.Position:=0;
   ms.ReadBuffer(arrLL^,ms.size);
   for i:=0 to length(arLL)-1 do
    arLL[i]:=arrLL^[i];
   if (ms.Size>24)and(compare2EP(arLL[0],arLL[length(arLL)-1]))
     then Fsaveas.Show_(zoom_size,arLL)
     else ShowMessage('В данной версии функция доступна только для полигонов');
   freeMem(arrLL);
   SetLength(arLL,0);
   ms.Free;
  end;
end;

procedure TFmain.livecom1Click(Sender: TObject);
var Apos:tExtendedPoint;
begin
 Apos:=GPos2LonLat(pos,zoom_size,sat_map_both);
 Clipboard.AsText:='http://maps.live.com/default.aspx?v=2&cp='+R2StrPoint(Apos.y)+'~'+R2StrPoint(Apos.x)+'&style=h&lvl='+inttostr(zoom_size-1);
end;

procedure TFmain.N13Click(Sender: TObject);
begin
 Clipboard.AsText:=sat_map_both.GetLink(X2absX(pos.x-(mWd2-move.x),zoom_size),pos.y-(mHd2-move.y),zoom_size);;
end;

end.

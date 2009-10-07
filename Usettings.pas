unit Usettings;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Graphics,
  inifiles,
  filectrl,
  ExtCtrls,
  ComCtrls,
  Dialogs,
  Spin,
  strutils,
  DBCtrlsEh,
  IEConst,
  Mask,
  urlmon,
  wininet,
  GR32,
  XPMan,
  ZylGPSReceiver,
  TB2Dock,
  rxToolEdit,
  rxCurrEdit,
  Ugeofun,
  UMapType,
  UResStrings;

type
  TFSettings = class(TForm)
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet1: TTabSheet;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    TabSheet3: TTabSheet;
    GroupBox1: TGroupBox;
    ScrolInvert: TCheckBox;
    TabSheet4: TTabSheet;
    Button3: TButton;
    Label15: TLabel;
    OldCpath: TEdit;
    NewCpath: TEdit;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Label1: TLabel;
    EScPath: TEdit;
    Button8: TButton;
    Button9: TButton;
    Bevel1: TBevel;
    Label3: TLabel;
    ComboBox1: TComboBox;
    Bevel2: TBevel;
    TrBarGamma: TTrackBar;
    XPManifest1: TXPManifest;
    LabelGamma: TLabel;
    TrBarContrast: TTrackBar;
    LabelContrast: TLabel;
    TabSheet5: TTabSheet;
    ComboBoxCOM: TComboBox;
    Label4: TLabel;
    Label6: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Bevel3: TBevel;
    Label14: TLabel;
    smmapdif: TSpinEdit;
    Bevel4: TBevel;
    Label16: TLabel;
    ComboBox2: TComboBox;
    SpinEditMiniMap: TSpinEdit;
    Bevel5: TBevel;
    Label17: TLabel;
    Bevel6: TBevel;
    GroupBox2: TGroupBox;
    Label21: TLabel;
    Label22: TLabel;
    lat_ns: TComboBox;
    Lon_we: TComboBox;
    lat2: TCurrencyEdit;
    lat3: TCurrencyEdit;
    lon1: TCurrencyEdit;
    lon2: TCurrencyEdit;
    lon3: TCurrencyEdit;
    Lat1: TCurrencyEdit;
    Bevel7: TBevel;
    Bevel12: TBevel;
    SpinEdit3: TSpinEdit;
    Label69: TLabel;
    CB_GPSlog: TCheckBox;
    TabSheet8: TTabSheet;
    CBWMainColor: TColorBox;
    Label76: TLabel;
    Label77: TLabel;
    CBWFonColor: TColorBox;
    Label78: TLabel;
    CB_llstrType: TComboBox;
    Bevel8: TBevel;
    Label84: TLabel;
    Bevel13: TBevel;
    CBShowmapname: TCheckBox;
    RadioGroup1: TRadioGroup;
    CBinvertcolor: TCheckBox;
    SESizeStr: TSpinEdit;
    Label11: TLabel;
    Label10: TLabel;
    ColorBoxGPSstr: TColorBox;
    Label12: TLabel;
    Label13: TLabel;
    RBWinCon: TRadioButton;
    GroupBox4: TGroupBox;
    CBProxyused: TCheckBox;
    EditPass: TEdit;
    EditLogin: TEdit;
    Label25: TLabel;
    CBLogin: TCheckBox;
    EditIP: TEdit;
    RBMyCon: TRadioButton;
    Label27: TLabel;
    Label28: TLabel;
    SpinEditBorderAlpha: TSpinEdit;
    ColorBoxBorder: TColorBox;
    CBDblDwnl: TCheckBox;
    Bevel14: TBevel;
    CkBGoNextTile: TCheckBox;
    TabSheet9: TTabSheet;
    Button11: TButton;
    Button12: TButton;
    Button15: TButton;
    MapList: TListView;
    GroupBox5: TGroupBox;
    ScrollBox3: TScrollBox;
    Label38: TLabel;
    Label39: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label58: TLabel;
    Label57: TLabel;
    Label59: TLabel;
    Label5: TLabel;
    HotKey12: THotKey;
    HotKey13: THotKey;
    HotKey15: THotKey;
    HotKey16: THotKey;
    HotKey17: THotKey;
    HotKey18: THotKey;
    HotKey19: THotKey;
    HotKey20: THotKey;
    HotKey21: THotKey;
    HotKey22: THotKey;
    HotKey23: THotKey;
    ScrollBox4: TScrollBox;
    Label75: TLabel;
    Label74: TLabel;
    Label73: TLabel;
    Label72: TLabel;
    Label71: TLabel;
    Label60: TLabel;
    Label54: TLabel;
    Label53: TLabel;
    Label52: TLabel;
    Label51: TLabel;
    Label50: TLabel;
    Label49: TLabel;
    Label48: TLabel;
    Label47: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    HotKey39: THotKey;
    HotKey38: THotKey;
    HotKey37: THotKey;
    HotKey36: THotKey;
    HotKey35: THotKey;
    HotKey32: THotKey;
    HotKey31: THotKey;
    HotKey30: THotKey;
    HotKey29: THotKey;
    HotKey28: THotKey;
    HotKey27: THotKey;
    HotKey26: THotKey;
    HotKey25: THotKey;
    HotKey24: THotKey;
    HotKey40: THotKey;
    CBoxLocal: TComboBox;
    Label8: TLabel;
    Bevel10: TBevel;
    ChBoxFirstLat: TCheckBox;
    Label19: TLabel;
    GMTilesPath: TEdit;
    Button13: TButton;
    Button14: TButton;
    Label20: TLabel;
    SESizeTrack: TSpinEdit;
    ComboBoxBoudRate: TComboBox;
    Label65: TLabel;
    Button16: TButton;
    GroupBox3: TGroupBox;
    PaintBox1: TPaintBox;
    CBSaveTileNotExists: TCheckBox;
    CBBorderText: TCheckBox;
    Label23: TLabel;
    Label24: TLabel;
    Label26: TLabel;
    MapZapColorBox: TColorBox;
    Label29: TLabel;
    MapZapAlphaEdit: TSpinEdit;
    HotKey41: THotKey;
    Label18: TLabel;
    Bevel9: TBevel;
    CBlock_toolbars: TCheckBox;
    Label30: TLabel;
    SETilesOCache: TSpinEdit;
    Bevel11: TBevel;
    CBShowHintOnMarks: TCheckBox;
    GECachePath: TEdit;
    Button10: TButton;
    Button17: TButton;
    Label31: TLabel;
    Button18: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrBarGammaChange(Sender: TObject);
    procedure TrBarContrastChange(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure MapListCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure MapListCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
  private
  public
    procedure Save;
  end;

var
  FSettings: TFSettings;
  activ:boolean=true;
  procedure SetProxy;

implementation

uses
  Math,
  u_GlobalState,
  u_GeoToStr,
  Uimgfun,
  Unit1,
  UEditMap,
  UFillingMap,
  Ubrowser,
  Unit2,
  UAbout,
  USaveas,
  USearchResult,
  UImport,
  UAddCategory,
  UFDGAvailablePic,
  UaddPoint,
  Unit4,
  UaddLine,
  u_MiniMap,
  UaddPoly;

{$R *.dfm}

procedure TFSettings.Save;
var
    i:integer;
    lock_tb_b:boolean;
begin
 try
 SaveMaps;
 GState.MainIni.WriteBool('VIEW','ShowMapNameOnPanel',GState.ShowMapName);
 GState.MainIni.WriteInteger('POSITION','zoom_size',GState.Zoom_Size);
 GState.MainIni.WriteInteger('POSITION','x',FMain.POS.x);
 GState.MainIni.WriteInteger('POSITION','y',FMain.POS.y);
 GState.MainIni.WriteInteger('POSITION','y',FMain.POS.y);
 GState.MainIni.Writebool('VIEW','line',Fmain.ShowLine.Checked);
 GState.MainIni.Writeinteger('VIEW','DefCache',GState.DefCache);
 GState.MainIni.Writebool('VIEW','minimap',Fmain.ShowMiniMap.Checked);
 GState.MainIni.Writebool('VIEW','statusbar',Fmain.Showstatus.Checked);
 GState.MainIni.WriteInteger('VIEW','TilesOut',GState.TilesOut);
 GState.MainIni.Writeinteger('VIEW','grid',zoom_line);
 GState.MainIni.Writebool('VIEW','invert_mouse',GState.MouseWheelInv);
 GState.MainIni.Writebool('VIEW','back_load',GState.UsePrevZoom);
 GState.MainIni.Writebool('VIEW','animate',GState.AnimateZoom);
 GState.MainIni.Writebool('VIEW','FullScreen',GState.FullScrean);
 GState.MainIni.WriteInteger('VIEW','FLeft',Fmain.Left);
 GState.MainIni.WriteInteger('VIEW','FTop',Fmain.Top);
 GState.MainIni.WriteInteger('VIEW','FWidth',Fmain.Width);
 GState.MainIni.WriteInteger('VIEW','FHeight',Fmain.Height);
 GState.MainIni.WriteInteger('VIEW','TileSource',integer(Fmain.TileSource));
 GState.MainIni.WriteInteger('VIEW','SmMapW',GMiniMap.width);
 GState.MainIni.WriteInteger('VIEW','SmMapH',GMiniMap.height);
 if LayerMapScale<>nil then GState.MainIni.Writebool('VIEW','showscale',LayerMapScale.Visible);
 GState.MainIni.WriteInteger('VIEW','SmMapDifference',GMiniMap.z1mz2);
 GState.MainIni.WriteInteger('VIEW','SmMapAlpha',GMiniMap.alpha);
 GState.MainIni.WriteInteger('VIEW','ShowPointType',Byte(GState.show_point));
 GState.MainIni.Writeinteger('VIEW','MapZap',GState.zoom_mapzap);
 GState.MainIni.Writeinteger('VIEW','NumberFormat',byte(GState.num_format));
 GState.MainIni.Writebool('VIEW','Maximized',Fmain.WindowState=wsMaximized);
 GState.MainIni.Writebool('VIEW','CiclMap',GState.CiclMap);
 GState.MainIni.Writeinteger('VIEW','ResamlingType',byte(GState.resampling));
 GState.MainIni.Writeinteger('VIEW','llStrType',byte(GState.llStrType));
 GState.MainIni.WriteBool('VIEW','FirstLat',GState.FirstLat);
 GState.MainIni.Writeinteger('VIEW','BorderAlpha',GState.BorderAlpha);
 GState.MainIni.Writeinteger('VIEW','BorderColor',GState.BorderColor);
 GState.MainIni.WriteBool('VIEW','BorderText',GState.ShowBorderText);
 GState.MainIni.Writeinteger('VIEW','localization',GState.Localization);
 GState.MainIni.Writeinteger('VIEW','GShScale',GState.GShScale);
 GState.MainIni.Writeinteger('VIEW','MapZapColor',GState.MapZapColor);
 GState.MainIni.Writeinteger('VIEW','MapZapAlpha',GState.MapZapAlpha);
 GState.MainIni.WriteBool('VIEW','lock_toolbars',Fmain.lock_toolbars);
 GState.MainIni.WriteInteger('VIEW','TilesOCache', GState.MainFileCache.CacheElemensMaxCnt);
 GState.MainIni.WriteBool('VIEW','ShowHintOnMarks', GState.ShowHintOnMarks);

 if Fillingmaptype=nil then GState.MainIni.WriteString('VIEW','FillingMap','0')
                       else GState.MainIni.WriteString('VIEW','FillingMap',Fillingmaptype.guids);
 GState.MainIni.Writeinteger('Wikimapia','MainColor',Wikim_set.MainColor);
 GState.MainIni.Writeinteger('Wikimapia','FonColor',Wikim_set.FonColor);
 GState.MainIni.Writeinteger('HOTKEY','ZoomIn',Fmain.NzoomIn.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','ZoomOut',Fmain.NzoomOut.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','GoTo',Fmain.N14.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','CalcRast',Fmain.NCalcRast.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','Rect',Fmain.TBRECT.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','Polyg',Fmain.TBRegion.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','Coord',Fmain.TBCOORD.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','Previous',Fmain.TBPrevious.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','inet',Fmain.NSRCinet.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','Cache',Fmain.NSRCesh.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','CachInet',Fmain.NSRCic.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','Showstatus',Fmain.Showstatus.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','ShowLine',Fmain.ShowLine.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','ShowMiniMap',Fmain.ShowMiniMap.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','FoolSize',Fmain.NFoolSize.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','GoToCur',Fmain.NGoToCur.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','backload',Fmain.Nbackload.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','animate',Fmain.Nanimate.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','CiclMap',Fmain.NCiclMap.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','ShowScale',Fmain.N32.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','GPSconn',Fmain.NGPSconn.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','GPSPath',Fmain.NGPSPath.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','GPSToPoint',Fmain.NGPSToPoint.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','SaveTreck',Fmain.NSaveTreck.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','LoadSelFromFile',Fmain.TBLoadSelFromFile.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','InvertColor',Fmain.Ninvertcolor.ShortCut);
 GState.MainIni.Writeinteger('HOTKEY','MapParams',Fmain.NMapParams.ShortCut);

 GState.MainIni.Writeinteger('COLOR_LEVELS','gamma', GState.GammaN);
 GState.MainIni.Writeinteger('COLOR_LEVELS','contrast',GState.ContrastN);
 GState.MainIni.WriteBool('COLOR_LEVELS','InvertColor',GState.InvertColor);

 if GState.GPS_enab then GState.MainIni.WriteBool('GPS','enbl',true)
                else GState.MainIni.WriteBool('GPS','enbl',false);
 GState.MainIni.WriteBool('GPS','path',GState.GPS_ShowPath);
 GState.MainIni.WriteBool('GPS','go',GState.GPS_MapMove);
 GState.MainIni.WriteString('GPS','COM',GState.GPS_COM);
 GState.MainIni.WriteInteger('GPS','BaudRate',GState.GPS_BaudRate);
 GState.MainIni.WriteFloat('GPS','popr_lon',GState.GPS_Correction.x);
 GState.MainIni.WriteFloat('GPS','popr_lat',GState.GPS_Correction.y);
 GState.MainIni.Writeinteger('GPS','update',GState.GPS_Delay);
 GState.MainIni.WriteBool('GPS','log',GState.GPS_WriteLog);
 GState.MainIni.WriteInteger('GPS','SizeStr',GState.GPS_ArrowSize);
 GState.MainIni.WriteInteger('GPS','SizeTrack',GState.GPS_TrackWidth);
 GState.MainIni.WriteInteger('GPS','ColorStr',GState.GPS_ArrowColor);
 GState.MainIni.Writestring('PATHtoCACHE','GMVC',GState.OldCpath_);
 GState.MainIni.Writestring('PATHtoCACHE','SASC',GState.NewCpath_);
 GState.MainIni.Writestring('PATHtoCACHE','ESC',GState.ESCpath_);
 GState.MainIni.Writestring('PATHtoCACHE','GMTiles',GState.GMTilesPath_);
 GState.MainIni.Writestring('PATHtoCACHE','GECache',GState.GECachePath_);
 GState.MainIni.Writebool('INTERNET','userwinset',GState.InetConnect.userwinset);
 GState.MainIni.Writebool('INTERNET','uselogin',GState.InetConnect.uselogin);
 GState.MainIni.Writebool('INTERNET','used_proxy',GState.InetConnect.Proxyused);
 GState.MainIni.Writestring('INTERNET','proxy',GState.InetConnect.proxystr);
 GState.MainIni.Writestring('INTERNET','login',GState.InetConnect.loginstr);
 GState.MainIni.Writestring('INTERNET','password',GState.InetConnect.passstr);
 GState.MainIni.WriteBool('INTERNET','SaveTileNotExists',GState.SaveTileNotExists);
 GState.MainIni.WriteBool('INTERNET','DblDwnl',GState.TwoDownloadAttempt);
 GState.MainIni.Writebool('INTERNET','GoNextTile',GState.GoNextTileIfDownloadError);
 GState.MainIni.Writebool('NPARAM','stat',GState.WebReportToAuthor);

 i:=1;
 while GState.MainIni.ReadString('HIGHLIGHTING','pointx_'+inttostr(i),'2147483647')<>'2147483647' do
  begin
   GState.MainIni.DeleteKey('HIGHLIGHTING','pointx_'+inttostr(i));
   GState.MainIni.DeleteKey('HIGHLIGHTING','pointy_'+inttostr(i));
   inc(i);
  end;
 if length(poly_save)>0 then
  begin
   GState.MainIni.WriteInteger('HIGHLIGHTING','zoom',poly_zoom_save);
   for i:=1 to length(poly_save) do
    begin
     GState.MainIni.WriteFloat('HIGHLIGHTING','pointx_'+inttostr(i),poly_save[i-1].x);
     GState.MainIni.WriteFloat('HIGHLIGHTING','pointy_'+inttostr(i),poly_save[i-1].y);
    end;
  end;
 GState.MainIni.UpdateFile;
 lock_tb_b:=Fmain.lock_toolbars;
 Fmain.lock_toolbars:=false;
 TBiniSavePositions(Fmain,GState.MainIni,'PANEL_');
 Fmain.lock_toolbars:=lock_tb_b;
 except
 end;
end;

procedure TFSettings.Button1Click(Sender: TObject);
begin
 Close
end;


procedure SetProxy;
var PIInfo : PInternetProxyInfo;
begin
 New (PIInfo) ;
 if not(GState.InetConnect.userwinset) then
  begin
   if GState.InetConnect.proxyused then
    begin
     PIInfo^.dwAccessType := INTERNET_OPEN_TYPE_PROXY ;
     PIInfo^.lpszProxy := PChar(GState.InetConnect.proxystr);
     PIInfo^.lpszProxyBypass := nil;
     UrlMkSetSessionOption(INTERNET_OPTION_PROXY, piinfo, SizeOf(Internet_Proxy_Info), 0);
    end
   else
    begin
     PIInfo^.dwAccessType := INTERNET_OPEN_TYPE_DIRECT;
     PIInfo^.lpszProxy := nil;
     PIInfo^.lpszProxyBypass := nil;
     UrlMkSetSessionOption(INTERNET_OPTION_PROXY, piinfo, SizeOf(Internet_Proxy_Info), 0);
    end;
   UrlMkSetSessionOption(INTERNET_OPTION_SETTINGS_CHANGED, nil, 0, 0);
  end;
 Dispose (PIInfo) ;
end;

procedure TFSettings.Button3Click(Sender: TObject);
var i,k,j:integer;
    MTb:TMapType;
begin
 For i:=0 to MapList.Items.Count-1 do
  begin
   TMapType(MapList.Items.Item[i].data).id:=i+1;
  end;
  k := length(MapType) shr 1;
 while k>0 do
  begin
   for i:=0 to length(MapType)-k-1 do
    begin
      j:=i;
      while (j>=0)and(MapType[j].id>MapType[j+k].id) do
      begin
        MTb:=MapType[j];
        MapType[j]:=MapType[j+k];
        MapType[j+k]:=MTb;
        if j>k then Dec(j,k)
               else j:=0;
      end;
    end;
   k:=k shr 1;
  end;
 GState.ShowHintOnMarks:=CBShowHintOnMarks.checked;
 GState.MainFileCache.CacheElemensMaxCnt:=SETilesOCache.value;
 GState.MapZapColor:=MapZapColorBox.Selected;
 GState.MapZapAlpha:=MapZapAlphaEdit.Value;
 GState.FirstLat:=ChBoxFirstLat.Checked;
 GState.TwoDownloadAttempt:=CBDblDwnl.Checked;
 GState.GoNextTileIfDownloadError:=CkBGoNextTile.Checked;
 GState.GPS_ArrowColor:=ColorBoxGPSstr.selected;
 GState.InvertColor:=CBinvertcolor.Checked;
 GState.BorderColor:=ColorBoxBorder.Selected;
 GState.BorderAlpha:=SpinEditBorderAlpha.Value;
 GState.ShowBorderText:=CBBorderText.Checked;
 GState.DefCache:=RadioGroup1.itemindex+1;
 GState.ShowMapName:=CBShowmapname.Checked;
 GState.llStrType:=TDegrShowFormat(CB_llstrType.ItemIndex);
 GMiniMap.alpha:=SpinEditMiniMap.Value;
 GState.Resampling:= TTileResamplingType(ComboBox2.ItemIndex);

 GState.GPS_ArrowSize:=SESizeStr.Value;
 GState.GPS_TrackWidth:=SESizeTrack.Value;
 GState.GPS_TimeOut:=SpinEdit2.Value;
 GState.GPS_WriteLog:=CB_GPSlog.Checked;
 GState.GPS_Delay:=SpinEdit1.Value;
 FMain.lock_toolbars:=CBlock_toolbars.Checked;
 GState.GPS_COM:=ComboBoxCOM.Text;
 GState.GPS_BaudRate:=StrToint(ComboBoxBoudRate.Text);
 GMiniMap.z1mz2:=smmapdif.Value;
 if (RBWinCon.Checked)and(not GState.InetConnect.userwinset) then ShowMessage(SAS_MSG_need_reload_application_curln);
 GState.InetConnect.userwinset:=RBWinCon.Checked;
 GState.InetConnect.proxyused:=CBProxyused.Checked;
 GState.InetConnect.uselogin:=CBLogin.Checked;
 GState.InetConnect.proxystr:=EditIP.Text;
 GState.InetConnect.loginstr:=EditLogin.Text;
 GState.InetConnect.passstr:=EditPass.Text;
 GState.SaveTileNotExists:=CBSaveTileNotExists.Checked;
 GState.MouseWheelInv:=ScrolInvert.Checked;
 GState.NewCPath_:=IncludeTrailingPathDelimiter(NewCPath.Text);
 GState.OldCPath_:=IncludeTrailingPathDelimiter(OldCPath.Text);
 GState.ESCPath_:=IncludeTrailingPathDelimiter(EScPath.Text);
 GState.GMTilesPath_:=IncludeTrailingPathDelimiter(GMTilesPath.Text);
 GState.GECachePath_:=IncludeTrailingPathDelimiter(GECachePath.Text);
 GState.GammaN:=TrBarGamma.Position;
 GState.ContrastN:=TrBarContrast.Position;
 GState.num_format := TDistStrFormat(ComboBox1.ItemIndex);
 Wikim_set.MainColor:=CBWMainColor.Selected;
 Wikim_set.FonColor:=CBWFonColor.Selected;
 With Fmain do
 begin
 NzoomIn.ShortCut:=HotKey12.HotKey;
 NzoomOut.ShortCut:=HotKey13.HotKey;
 N14.ShortCut:=HotKey15.HotKey;
 NCalcRast.ShortCut:=HotKey16.HotKey;
 TBRECT.ShortCut:=HotKey17.HotKey;
 TBRegion.ShortCut:=HotKey18.HotKey;
 TBCOORD.ShortCut:=HotKey19.HotKey;
 TBPREVIOUS.ShortCut:=HotKey20.HotKey;
 NSRCinet.ShortCut:=HotKey21.HotKey;
 NSRCesh.ShortCut:=HotKey22.HotKey;
 NSRCic.ShortCut:=HotKey23.HotKey;
 Showstatus.ShortCut:=HotKey24.HotKey;
 ShowLine.ShortCut:=HotKey25.HotKey;
 ShowMiniMap.ShortCut:=HotKey26.HotKey;
 NFoolSize.ShortCut:=HotKey27.HotKey;
 NGoToCur.ShortCut:=HotKey28.HotKey;
 Nbackload.ShortCut:=HotKey29.HotKey;
 Nanimate.ShortCut:=HotKey30.HotKey;
 NCiclMap.ShortCut:=HotKey31.HotKey;
 N32.ShortCut:=HotKey32.HotKey;
 NGPSconn.ShortCut:=HotKey35.HotKey;
 NGPSPath.ShortCut:=HotKey36.HotKey;
 NGPSToPoint.ShortCut:=HotKey37.HotKey;
 NSaveTreck.ShortCut:=HotKey38.HotKey;
 TBLoadSelFromFile.ShortCut:=HotKey39.HotKey;
 Ninvertcolor.ShortCut:=HotKey40.HotKey;
 NMapParams.ShortCut:=HotKey41.HotKey;
 end;

 if ((GState.Localization<>LANG_RUSSIAN)and(CBoxLocal.ItemIndex=0))or
    ((GState.Localization<>LANG_ENGLISH)and(CBoxLocal.ItemIndex=1)) then ShowMessage(SAS_MSG_need_reload_application);
 case CBoxLocal.ItemIndex of
  0:GState.Localization:=LANG_RUSSIAN;
  1:GState.Localization:=LANG_ENGLISH;
 end;

 GState.GPS_Correction:=Extpoint(DMS2G(lon1.Value,lon2.Value,lon3.Value,Lon_we.ItemIndex=1),
                      DMS2G(lat1.Value,lat2.Value,lat3.Value,Lat_ns.ItemIndex=1));

 GState.TilesOut:=SpinEdit3.Value;
 hg_x:=(Screen.Width div 256)+(integer((Screen.Width mod 256)>0))+GState.TilesOut;
 hg_y:=(Screen.Height div 256)+(integer((Screen.Height mod 256)>0))+GState.TilesOut;

 if GState.TilesOut=0 then begin
   yhgpx:=Screen.Height;
   xhgpx:=Screen.Width;
 end else begin
   yhgpx:=256*hg_y;
   xhgpx:=256*hg_x;
 end;
 pr_x:=(xhgpx)div 2;
 pr_y:=(yhgpx)div 2;

 LayerMap.Bitmap.Width:=xhgpx;
 LayerMap.Bitmap.Height:=yhgpx;
 LayerMapNal.Bitmap.Width:=xhgpx;
 LayerMapNal.Bitmap.Height:=yhgpx;
 LayerMapMarks.Bitmap.Width:=xhgpx;
 LayerMapMarks.Bitmap.Height:=yhgpx;
 LayerMapWiki.Bitmap.Height:=yhgpx;
 LayerMapWiki.Bitmap.Width:=xhgpx;
 LayerMapGPS.Bitmap.Height:=yhgpx;
 LayerMapGPS.Bitmap.Width:=xhgpx;

 SetProxy;

{ New (PIInfo) ;
 PIInfo^.dwAccessType := INTERNET_OPEN_TYPE_PROXY ;
 PIInfo^.lpszProxy := PChar('192.168.50.2:3128'); // ага, здесь пишем прокси.
 PIInfo^.lpszProxyBypass := PChar(''); // а тут адреса, доступ к которым возможен, минуя прокси
 UrlMkSetSessionOption(INTERNET_OPTION_PROXY, piinfo, SizeOf(Internet_Proxy_Info), 0);
 Dispose (PIInfo) ;
 UrlMkSetSessionOption(INTERNET_OPTION_PROXY_USERNAME, PChar('nord\az'), SizeOf('nord\az'), 0);
 UrlMkSetSessionOption(INTERNET_OPTION_PROXY_PASSWORD, PChar('678727'), SizeOf('678727'), 0);    }
{ if not(InetConnect.userwinset) then
  if InetConnect.proxyused then
   try
    Fmain.EmbeddedWB1_.ProxySettings.Address:=copy(InetConnect.proxystr,1,PosEx(':',InetConnect.proxystr)-1);
    Fmain.EmbeddedWB1_.ProxySettings.Port:=strtoint(copy(InetConnect.proxystr,PosEx(':',InetConnect.proxystr)+1,length(InetConnect.proxystr)-PosEx(':',InetConnect.proxystr)));
    Fbrowser.EmbeddedWB1.ProxySettings.Address:=copy(InetConnect.proxystr,1,PosEx(':',InetConnect.proxystr)-1);
    Fbrowser.EmbeddedWB1.ProxySettings.Port:=strtoint(copy(InetConnect.proxystr,PosEx(':',InetConnect.proxystr)+1,length(InetConnect.proxystr)-PosEx(':',InetConnect.proxystr)));
    if InetConnect.uselogin then
     begin
      Fmain.EmbeddedWB1_.ProxySettings.UserName:=InetConnect.loginstr;
      Fmain.EmbeddedWB1_.ProxySettings.Password:=InetConnect.passstr;
      Fbrowser.EmbeddedWB1.ProxySettings.UserName:=InetConnect.loginstr;
      Fbrowser.EmbeddedWB1.ProxySettings.Password:=InetConnect.passstr;
     end;
   except
    ShowMessage(SAS_ERR_ProxyStrFormat);
   end;    }

 if sender=Button2 then
  begin
   Fmain.Enabled:=true;
   Fsettings.Visible := false;
   Close
  end;
 save;
 if MapsEdit then
  begin
   CreateMapUI;
  end;
 Fmain.selectMap(sat_map_both);
end;

procedure TFSettings.Button4Click(Sender: TObject);
begin
 if (sender as TButton).Tag=1 then OldCpath.Text:='cache_old\';
 if (sender as TButton).Tag=2 then NewCpath.Text:='cache\';
 if (sender as TButton).Tag=3 then NewCpath.Text:='cache_es\';
 if (sender as TButton).Tag=4 then GMTilespath.Text:='cache_gmt\';
 if (sender as TButton).Tag=5 then GECachepath.Text:='cache_ge\';
end;

procedure TFSettings.Button5Click(Sender: TObject);
var  TempPath: string;
begin
  if SelectDirectory('', '', TempPath) then
  begin
    if (sender as TButton).Tag=1 then OldCpath.Text:=String(TempPath)+'\';
    if (sender as TButton).Tag=2 then NewCpath.Text:=String(TempPath)+'\';
    if (sender as TButton).Tag=3 then ESCpath.Text:=String(TempPath)+'\';
    if (sender as TButton).Tag=4 then GMTilesPath.Text:=String(TempPath)+'\';
    if (sender as TButton).Tag=5 then GECachePath.Text:=String(TempPath)+'\';
  end;
end;

procedure TFSettings.FormShow(Sender: TObject);
var DMS:TDMS;
begin
 MapsEdit:=false;
 case GState.Localization  of
  LANG_RUSSIAN:CBoxLocal.ItemIndex:=0;
  LANG_ENGLISH:CBoxLocal.ItemIndex:=1;
 end;
 CBShowHintOnMarks.Checked:=GState.ShowHintOnMarks;
 SETilesOCache.Value:=GState.MainFileCache.CacheElemensMaxCnt;
 MapZapColorBox.Selected:=GState.MapZapColor;
 MapZapAlphaEdit.Value:=GState.MapZapAlpha;
 CBDblDwnl.Checked:=GState.TwoDownloadAttempt;
 ChBoxFirstLat.Checked:=GState.FirstLat;
 CBlock_toolbars.Checked:=FMain.lock_toolbars;
 CkBGoNextTile.Checked:=GState.GoNextTileIfDownloadError;
 RBWinCon.Checked:=GState.InetConnect.userwinset;
 RBMyCon.Checked:=not(GState.InetConnect.userwinset);
 CBProxyused.Checked:=GState.InetConnect.proxyused;
 CBLogin.Checked:=GState.InetConnect.uselogin;
 CBSaveTileNotExists.Checked:=GState.SaveTileNotExists;
 EditIP.Text:=GState.InetConnect.proxystr;
 EditLogin.Text:=GState.InetConnect.loginstr;
 EditPass.Text:=GState.InetConnect.passstr;
 ColorBoxGPSstr.Selected:=GState.GPS_ArrowColor;
 CBinvertcolor.Checked:=GState.InvertColor;
 PageControl1.ActivePageIndex:=0;
 ColorBoxBorder.Selected:=GState.BorderColor;
 SpinEditBorderAlpha.Value:=GState.BorderAlpha;
 CBBorderText.Checked:=GState.ShowBorderText;
 RadioGroup1.ItemIndex:=GState.DefCache-1;
 CBShowmapname.Checked:=GState.ShowMapName;
 CB_llstrType.ItemIndex:=byte(GState.llStrType);
 SpinEditMiniMap.Value:=GMiniMap.alpha;
 OldCPath.text:=GState.OldCPath_;
 NewCPath.text:=GState.NewCPath_;
 ESCPath.text:=GState.ESCPath_;
 GMTilesPath.text:=GState.GMTilesPath_;
 GECachePath.text:=GState.GECachePath_;
 SpinEdit2.Value:=GState.GPS_TimeOut;
 CB_GPSlog.Checked:=GState.GPS_WriteLog;
 SpinEdit1.Value:=GState.GPS_Delay;
 SESizeStr.Value:=GState.GPS_ArrowSize;
 SESizeTrack.Value:=GState.GPS_TrackWidth;
 ScrolInvert.Checked:=GState.MouseWheelInv;
 smmapdif.Value:=GMiniMap.z1mz2;
 ComboBox2.ItemIndex:=byte(GState.Resampling);
 ComboBoxCOM.Text:=GState.GPS_COM;
 ComboBoxBoudRate.Text:=inttostr(GState.GPS_BaudRate);
 TrBarGamma.Position:=GState.GammaN;
 if GState.GammaN<50 then LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((GState.GammaN*2)/100)+')'
              else LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((GState.GammaN-40)/10)+')';
 TrBarcontrast.Position:=GState.ContrastN;
 LabelContrast.Caption:=SAS_STR_Contrast+' ('+inttostr(GState.ContrastN)+')';
 ComboBox1.ItemIndex := byte(GState.num_format);
 CBWMainColor.Selected:=Wikim_set.MainColor;
 CBWFonColor.Selected:=Wikim_set.FonColor;
 With Fmain do
 begin
 HotKey12.HotKey:=NzoomIn.ShortCut;
 HotKey13.HotKey:=NzoomOut.ShortCut;
 HotKey15.HotKey:=N14.ShortCut;
 HotKey16.HotKey:=NCalcRast.ShortCut;
 HotKey17.HotKey:=TBRECT.ShortCut;
 HotKey18.HotKey:=TBRegion.ShortCut;
 HotKey19.HotKey:=TBCOORD.ShortCut;
 HotKey20.HotKey:=TBPREVIOUS.ShortCut;
 HotKey21.HotKey:=NSRCinet.ShortCut;
 HotKey22.HotKey:=NSRCesh.ShortCut;
 HotKey23.HotKey:=NSRCic.ShortCut;
 HotKey24.HotKey:=Showstatus.ShortCut;
 HotKey25.HotKey:=ShowLine.ShortCut;
 HotKey26.HotKey:=ShowMiniMap.ShortCut;
 HotKey27.HotKey:=NFoolSize.ShortCut;
 HotKey28.HotKey:=NGoToCur.ShortCut;
 HotKey29.HotKey:=Nbackload.ShortCut;
 HotKey30.HotKey:=Nanimate.ShortCut;
 HotKey31.HotKey:=NCiclMap.ShortCut;
 HotKey32.HotKey:=N32.ShortCut;
 HotKey35.HotKey:=NGPSconn.ShortCut;
 HotKey36.HotKey:=NGPSconn.ShortCut;
 HotKey37.HotKey:=NGPSToPoint.ShortCut;
 HotKey38.HotKey:=NSaveTreck.ShortCut;
 HotKey39.HotKey:=TBLoadSelFromFile.ShortCut;
 HotKey40.HotKey:=Ninvertcolor.ShortCut;
 HotKey41.HotKey:=NMapParams.ShortCut;
 end;
 DMS:=D2DMS(GState.GPS_Correction.Y);
 lat1.Value:=DMS.D; lat2.Value:=DMS.M; lat3.Value:=DMS.S;
 if DMS.N then Lat_ns.ItemIndex:=1 else Lat_ns.ItemIndex:=0;
 DMS:=D2DMS(GState.GPS_Correction.X);
 lon1.Value:=DMS.D; lon2.Value:=DMS.M; lon3.Value:=DMS.S;
 if DMS.N then Lon_we.ItemIndex:=1 else Lon_we.ItemIndex:=0;

 SpinEdit3.Value:=GState.TilesOut;
end;

procedure TFSettings.FormCreate(Sender: TObject);
var i:integer;
begin
  ComboBoxCOM.Items.Clear;
  for i:=1 to 64 do ComboBoxCOM.Items.Add('COM'+inttostr(i));
end;

procedure TFSettings.TrBarGammaChange(Sender: TObject);
begin
 if TrBarGamma.Position<50 then LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position*2)/100)+')'
                           else LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position-40)/10)+')';
end;

procedure TFSettings.TrBarContrastChange(Sender: TObject);
begin
 LabelContrast.Caption:=SAS_STR_Contrast+' ('+inttostr(TrBarcontrast.Position)+')';
end;

procedure ExchangeItems(lv: TListView; const i, j: Integer);
var tempLI: TListItem;
begin
 lv.Items.BeginUpdate;
 try
  tempLI := TListItem.Create(lv.Items);
  tempLI.Assign(lv.Items.Item[i]);
  lv.Items.Item[i].Assign(lv.Items.Item[j]);
  lv.Items.Item[j].Assign(tempLI);
  lv.Items.Item[j].Selected:=true;
  tempLI.Free;
 finally
  lv.Items.EndUpdate
 end;
end;

procedure TFSettings.Button12Click(Sender: TObject);
begin
 MapsEdit:=true;
 If (MapList.Selected<>nil)and(MapList.Selected.Index>0) then
  ExchangeItems(MapList, MapList.Selected.Index,MapList.Selected.Index-1);
end;

procedure TFSettings.Button11Click(Sender: TObject);
begin
 MapsEdit:=true;
 If (MapList.Selected<>nil)and(MapList.Selected.Index<MapList.Items.Count-1) then
  ExchangeItems(MapList, MapList.Selected.Index,MapList.Selected.Index+1)
end;

procedure TFSettings.Button15Click(Sender: TObject);
begin
 FEditMap.AmapType:=TMapType(MapList.Selected.Data);
 FEditMap.ShowModal;
end;

procedure TFSettings.MapListCustomDrawItem(Sender:TCustomListView; Item:TListItem; State:TCustomDrawState; var DefaultDraw:Boolean);
begin
  sender.Canvas.Font.Height:=8;
end;

procedure TFSettings.MapListCustomDrawSubItem(Sender:TCustomListView; Item:TListItem; SubItem:Integer; State:TCustomDrawState; var DefaultDraw:Boolean);
begin
 if item = nil then EXIT;
 if TMapType(Item.Data).separator then
  begin
   sender.canvas.Pen.Color:=clGray;
   sender.canvas.MoveTo(2,Item.DisplayRect(drBounds).Bottom-1);
   sender.canvas.LineTo(sender.Column[0].Width,Item.DisplayRect(drBounds).Bottom-1);
  end;
 if Item.Index mod 2 = 1 then sender.canvas.brush.Color:=cl3DLight
                         else sender.canvas.brush.Color:=clwhite;
end;

procedure TFSettings.PaintBox1Paint(Sender: TObject);
begin
 Fmain.GPSReceiver.DrawSatellites(TPaintBox(Sender).Canvas, TPaintBox(Sender).Width div 2,
    TPaintBox(Sender).Parent.Brush.Color, clBlack);
end;

procedure TFSettings.Button16Click(Sender: TObject);
var
  pPort: TCommPort;
  pBaudRate: TBaudRate;
begin
 if Fmain.GPSReceiver.FastDetectGPS(pPort, pBaudRate) then
  begin
   ComboBoxCOM.Text := Fmain.GPSReceiver.CommPortToString(pPort);
   ComboBoxBoudRate.Text := IntToStr( Fmain.GPSReceiver.BaudRateToInt(pBaudRate));
   ShowMessage('Ok');
  end
  else ShowMessage(SAS_MSG_NoGPSdetected);
end;

procedure TFSettings.Button18Click(Sender: TObject);
begin
 showMessage(TMapType(MapList.Selected.Data).info);
end;

end.


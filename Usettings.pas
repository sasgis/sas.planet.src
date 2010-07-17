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
  filectrl,
  ExtCtrls,
  ComCtrls,
  Dialogs,
  Buttons,
  Spin,
  Mask,
  urlmon,
  wininet,
  GR32,
  XPMan,
  ZylGPSReceiver,
  TB2Dock,
  TBX,
  rxToolEdit,
  rxCurrEdit,
  Ugeofun,
  UMapType,
  UResStrings,
  UShortcutEditor;

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
    CBSensorsBarAutoShow: TCheckBox;
    SBGetComNum: TSpeedButton;
    Label32: TLabel;
    SETimeOut: TSpinEdit;
    TabSheet6: TTabSheet;
    Label33: TLabel;
    CBGSMComPort: TComboBox;
    Label34: TLabel;
    CBGSMBaundRate: TComboBox;
    RBGSMAuto: TRadioButton;
    RBGSMManual: TRadioButton;
    Bevel15: TBevel;
    Label35: TLabel;
    ColorBoxBackGround: TColorBox;
    CBLastSuccess: TCheckBox;
    Bevel16: TBevel;
    Label36: TLabel;
    SEWaitingAnswer: TSpinEdit;
    Label37: TLabel;
    CBCacheType: TComboBox;
    List: TListBox;
    Label40: TLabel;
    Label55: TLabel;
    Label5: TLabel;
    SE_NumTrackPoints: TSpinEdit;
    CB_GPSlogNmea: TCheckBox;
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
    procedure Button18Click(Sender: TObject);
    procedure SBGetComNumClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    FShortcutEditor: TShortcutEditor;
    procedure Save;
  end;

var
  FSettings: TFSettings;
  procedure SetProxy;

implementation

uses
  Types,
  TB2Item,
  u_GlobalState,
  u_GeoToStr,
  Uimgfun,
  Unit1,
  UEditMap;

{$R *.dfm}

procedure TFSettings.Save;
var
    i:integer;
    lock_tb_b:boolean;
    VZoom: Byte;
    VScreenCenterPos: TPoint;
begin
  GState.ViewState.LockRead;
  try
    VZoom := GState.ViewState.GetCurrentZoom;
    VScreenCenterPos := GState.ViewState.GetCenterMapPixel;
  finally
    GState.ViewState.UnLockRead;
  end;

 try
 SaveMaps;
 GState.ViewState.SaveViewPortState;
 GState.MainIni.WriteBool('VIEW','ShowMapNameOnPanel',GState.ShowMapName);
 GState.MainIni.WriteBool('VIEW','ZoomingAtMousePos',GState.ZoomingAtMousePos);
 GState.MainIni.WriteInteger('POSITION','zoom_size',VZoom + 1);
 GState.MainIni.WriteInteger('POSITION','x',VScreenCenterPos.x);
 GState.MainIni.WriteInteger('POSITION','y',VScreenCenterPos.y);
 GState.MainIni.Writebool('VIEW','line',Fmain.ShowLine.Checked);
 GState.MainIni.Writeinteger('VIEW','DefCache',GState.DefCache);
 GState.MainIni.Writebool('VIEW','minimap',Fmain.ShowMiniMap.Checked);
 GState.MainIni.Writebool('VIEW','statusbar',Fmain.Showstatus.Checked);
 GState.MainIni.WriteInteger('VIEW','TilesOut',GState.TilesOut);
 GState.MainIni.Writeinteger('VIEW','grid', GState.TileGridZoom);
 GState.MainIni.Writebool('VIEW','invert_mouse',GState.MouseWheelInv);
 GState.MainIni.Writebool('VIEW','back_load',GState.UsePrevZoom);
 GState.MainIni.Writebool('VIEW','back_load_layer',GState.UsePrevZoomLayer);
 GState.MainIni.Writebool('VIEW','animate',GState.AnimateZoom);
 GState.MainIni.Writebool('VIEW','FullScreen',GState.FullScrean);
 GState.MainIni.WriteInteger('VIEW','FLeft',Fmain.Left);
 GState.MainIni.WriteInteger('VIEW','FTop',Fmain.Top);
 GState.MainIni.WriteInteger('VIEW','FWidth',Fmain.Width);
 GState.MainIni.WriteInteger('VIEW','FHeight',Fmain.Height);
 GState.MainIni.WriteInteger('VIEW','TileSource',integer(Fmain.TileSource));
 if FMain.LayerMapScale<>nil then GState.MainIni.Writebool('VIEW','showscale', FMain.LayerMapScale.Visible);
 GState.MainIni.Writebool('VIEW','showselection', FMain.LayerSelection.Visible);
 Fmain.FMiniMapLayer.WriteIni;
 GState.MainIni.WriteInteger('VIEW','ShowPointType',Byte(GState.show_point));
 GState.MainIni.Writeinteger('VIEW','MapZap', Fmain.FFillingMap.SourceZoom);
 GState.MainIni.Writeinteger('VIEW','NumberFormat',byte(GState.num_format));
 GState.MainIni.Writebool('VIEW','Maximized',Fmain.WindowState=wsMaximized);
 GState.MainIni.Writeinteger('VIEW','ResamlingType',byte(GState.resampling));
 GState.MainIni.Writeinteger('VIEW','llStrType',byte(GState.llStrType));
 GState.MainIni.WriteBool('VIEW','FirstLat',GState.FirstLat);
 GState.MainIni.Writeinteger('VIEW','BorderAlpha',GState.BorderAlpha);
 GState.MainIni.Writeinteger('VIEW','BorderColor',GState.BorderColor);
 GState.MainIni.WriteBool('VIEW','BorderText',GState.ShowBorderText);
 GState.MainIni.Writeinteger('VIEW','localization',GState.Localization);
 GState.MainIni.Writeinteger('VIEW','GShScale',GState.GShScale);
 GState.MainIni.Writeinteger('VIEW','MapZapColor',GState.MapZapColor);
 GState.MainIni.WriteBool('VIEW','MapZapShowTNE',GState.MapZapShowTNE);
 GState.MainIni.Writeinteger('VIEW','MapZapTneColor',GState.MapZapTneColor);
 GState.MainIni.Writeinteger('VIEW','MapZapAlpha',GState.MapZapAlpha);
 GState.MainIni.WriteBool('VIEW','lock_toolbars',Fmain.lock_toolbars);
 GState.MainIni.WriteInteger('VIEW','TilesOCache', GState.CacheElemensMaxCnt);
 GState.MainIni.WriteBool('VIEW','ShowHintOnMarks', GState.ShowHintOnMarks);
 GState.MainIni.Writeinteger('VIEW','LastSelectionColor',GState.LastSelectionColor);
 GState.MainIni.Writeinteger('VIEW','LastSelectionAlfa',GState.LastSelectionAlfa);

 if FMain.FFillingMap.SourceSelected=nil then GState.MainIni.WriteString('VIEW','FillingMap','')
                       else GState.MainIni.WriteString('VIEW','FillingMap',FMain.FFillingMap.SourceSelected.GUIDString);
 GState.MainIni.WriteInteger('VIEW','SearchType',integer(GState.SrchType));
 GState.MainIni.WriteInteger('VIEW','Background',GState.BGround);
 GState.MainIni.Writeinteger('Wikimapia','MainColor',GState.WikiMapMainColor);
 GState.MainIni.Writeinteger('Wikimapia','FonColor',GState.WikiMapFonColor);

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
 GState.MainIni.WriteBool('GPS','NMEALog',GState.GPS_NMEALog);
 GState.MainIni.WriteInteger('GPS','SizeStr',GState.GPS_ArrowSize);
 GState.MainIni.WriteInteger('GPS','SizeTrack',GState.GPS_TrackWidth);
 GState.MainIni.WriteInteger('GPS','ColorStr',GState.GPS_ArrowColor);
 GState.MainIni.WriteFloat('GPS','Odometr',FMain.GPSpar.Odometr);
 GState.MainIni.WriteBool('GPS','SensorsAutoShow',GState.GPS_SensorsAutoShow);
 GState.MainIni.WriteInteger('GPS','NumShowTrackPoints',GState.GPS_NumTrackPoints);

 GState.MainIni.WriteString('GSM','port',GState.GSMpar.Port);
 GState.MainIni.WriteInteger('GSM','BaudRate',GState.GSMpar.BaudRate);
 GState.MainIni.WriteBool('GSM','Auto',GState.GSMpar.auto);
 GState.MainIni.WriteInteger('GSM','WaitingAnswer',GState.GSMpar.WaitingAnswer);

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
 GState.MainIni.WriteBool('INTERNET','IgnoreTileNotExists',GState.IgnoreTileNotExists);
 GState.MainIni.WriteBool('INTERNET','DblDwnl',GState.TwoDownloadAttempt);
 GState.MainIni.Writebool('INTERNET','GoNextTile',GState.GoNextTileIfDownloadError);
 GState.MainIni.WriteInteger('INTERNET','TimeOut',GState.InetConnect.TimeOut);
 GState.MainIni.WriteBool('INTERNET','SessionLastSuccess',GState.SessionLastSuccess);

 GState.MainIni.Writebool('NPARAM','stat',GState.WebReportToAuthor);

 i:=1;
 while GState.MainIni.ReadString('HIGHLIGHTING','pointx_'+inttostr(i),'2147483647')<>'2147483647' do
  begin
   GState.MainIni.DeleteKey('HIGHLIGHTING','pointx_'+inttostr(i));
   GState.MainIni.DeleteKey('HIGHLIGHTING','pointy_'+inttostr(i));
   inc(i);
  end;
 if length(GState.LastSelectionPolygon)>0 then
  begin
   GState.MainIni.WriteInteger('HIGHLIGHTING','zoom',GState.poly_zoom_save);
   for i:=1 to length(GState.LastSelectionPolygon) do
    begin
     GState.MainIni.WriteFloat('HIGHLIGHTING','pointx_'+inttostr(i),GState.LastSelectionPolygon[i-1].x);
     GState.MainIni.WriteFloat('HIGHLIGHTING','pointy_'+inttostr(i),GState.LastSelectionPolygon[i-1].y);
    end;
  end;
 lock_tb_b:=Fmain.lock_toolbars;
 Fmain.lock_toolbars:=false;
 TBiniSavePositions(Fmain,GState.MainIni,'PANEL_');
 FShortcutEditor.Save;
 Fmain.lock_toolbars:=lock_tb_b;
 GState.MainIni.UpdateFile;
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
  k := length(GState.MapType) shr 1;
 while k>0 do
  begin
   for i:=0 to length(GState.MapType)-k-1 do
    begin
      j:=i;
      while (j>=0)and(GState.MapType[j].id>GState.MapType[j+k].id) do
      begin
        MTb:=GState.MapType[j];
        GState.MapType[j]:=GState.MapType[j+k];
        GState.MapType[j+k]:=MTb;
        if j>k then Dec(j,k)
               else j:=0;
      end;
    end;
   k:=k shr 1;
  end;

 GState.SessionLastSuccess:=CBLastSuccess.Checked;
 GState.BGround:=ColorBoxBackGround.Selected;
 FMain.map.Color:=GState.BGround;
 GState.GSMpar.BaudRate:=strtoint(CBGSMBaundRate.text);
 GState.GSMpar.Port:=CBGSMComPort.Text;
 GState.GSMpar.auto:=RBGSMAuto.Checked;
 GState.GSMpar.WaitingAnswer:=SEWaitingAnswer.Value;
 GState.ShowHintOnMarks:=CBShowHintOnMarks.checked;
 GState.CacheElemensMaxCnt:=SETilesOCache.value;
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
 if CBCacheType.ItemIndex >= 0 then begin
  GState.DefCache := CBCacheType.ItemIndex+1;
 end else begin
  GState.DefCache := 2;
 end;
 GState.ShowMapName:=CBShowmapname.Checked;
 GState.llStrType:=TDegrShowFormat(CB_llstrType.ItemIndex);
 GState.Resampling:= TTileResamplingType(ComboBox2.ItemIndex);

 GState.GPS_ArrowSize:=SESizeStr.Value;
 GState.GPS_TrackWidth:=SESizeTrack.Value;
 GState.GPS_TimeOut:=SpinEdit2.Value;
 GState.GPS_WriteLog:=CB_GPSlog.Checked;
 GState.GPS_NMEALog:=CB_GPSlogNmea.Checked;
 GState.GPS_Delay:=SpinEdit1.Value;
 FMain.lock_toolbars:=CBlock_toolbars.Checked;
 GState.GPS_COM:=ComboBoxCOM.Text;
 GState.GPS_BaudRate:=StrToint(ComboBoxBoudRate.Text);
 GState.GPS_SensorsAutoShow:=CBSensorsBarAutoShow.Checked;
 GState.GPS_NumTrackPoints:=SE_NumTrackPoints.Value;
 if (RBWinCon.Checked)and(not GState.InetConnect.userwinset) then ShowMessage(SAS_MSG_need_reload_application_curln);
 GState.InetConnect.userwinset:=RBWinCon.Checked;
 GState.InetConnect.proxyused:=CBProxyused.Checked;
 GState.InetConnect.uselogin:=CBLogin.Checked;
 GState.InetConnect.proxystr:=EditIP.Text;
 GState.InetConnect.loginstr:=EditLogin.Text;
 GState.InetConnect.passstr:=EditPass.Text;
 if (GState.InetConnect.TimeOut<>SETimeOut.Value) then ShowMessage(SAS_MSG_need_reload_application_curln);
 GState.InetConnect.TimeOut:=SETimeOut.Value;

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
 GState.WikiMapMainColor:=CBWMainColor.Selected;
 GState.WikiMapFonColor:=CBWFonColor.Selected;

 if ((GState.Localization<>LANG_RUSSIAN)and(CBoxLocal.ItemIndex=0))or
    ((GState.Localization<>LANG_ENGLISH)and(CBoxLocal.ItemIndex=1)) then ShowMessage(SAS_MSG_need_reload_application);
 case CBoxLocal.ItemIndex of
  0:GState.Localization:=LANG_RUSSIAN;
  1:GState.Localization:=LANG_ENGLISH;
 end;

 GState.GPS_Correction:=Extpoint(DMS2G(lon1.Value,lon2.Value,lon3.Value,Lon_we.ItemIndex=1),
                      DMS2G(lat1.Value,lat2.Value,lat3.Value,Lat_ns.ItemIndex=1));

 GState.TilesOut:=SpinEdit3.Value;

 SetProxy;

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
 Fmain.selectMap(GState.ViewState.GetCurrentMap);
end;

procedure TFSettings.Button4Click(Sender: TObject);
begin
 if (sender as TButton).Tag=1 then OldCpath.Text:='cache_old' + PathDelim;
 if (sender as TButton).Tag=2 then NewCpath.Text:='cache' + PathDelim;
 if (sender as TButton).Tag=3 then NewCpath.Text:='cache_es' + PathDelim;
 if (sender as TButton).Tag=4 then GMTilespath.Text:='cache_gmt' + PathDelim;
 if (sender as TButton).Tag=5 then GECachepath.Text:='cache_ge' + PathDelim;
end;

procedure TFSettings.Button5Click(Sender: TObject);
var  TempPath: string;
begin
  if SelectDirectory('', '', TempPath) then
  begin
    if (sender as TButton).Tag=1 then OldCpath.Text:= IncludeTrailingPathDelimiter(TempPath);
    if (sender as TButton).Tag=2 then NewCpath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (sender as TButton).Tag=3 then ESCpath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (sender as TButton).Tag=4 then GMTilesPath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (sender as TButton).Tag=5 then GECachePath.Text:=IncludeTrailingPathDelimiter(TempPath);
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

 CBLastSuccess.Checked:=GState.SessionLastSuccess;
 ColorBoxBackGround.Selected:=GState.BGround;
 CBGSMBaundRate.text:=inttostr(GState.GSMpar.BaudRate);
 CBGSMComPort.Text:=GState.GSMpar.Port;
 RBGSMAuto.Checked:=GState.GSMpar.auto;
 RBGSMManual.Checked:=not GState.GSMpar.auto;
 SEWaitingAnswer.Value:=GState.GSMpar.WaitingAnswer;
 SETimeOut.Value:=GState.InetConnect.TimeOut;
 CBShowHintOnMarks.Checked:=GState.ShowHintOnMarks;
 SETilesOCache.Value:=GState.CacheElemensMaxCnt;
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
 CBCacheType.ItemIndex:=GState.DefCache-1;
 CBShowmapname.Checked:=GState.ShowMapName;
 CB_llstrType.ItemIndex:=byte(GState.llStrType);
 OldCPath.text:=GState.OldCPath_;
 NewCPath.text:=GState.NewCPath_;
 ESCPath.text:=GState.ESCPath_;
 GMTilesPath.text:=GState.GMTilesPath_;
 GECachePath.text:=GState.GECachePath_;
 SpinEdit2.Value:=GState.GPS_TimeOut;
 CB_GPSlog.Checked:=GState.GPS_WriteLog;
 CB_GPSlogNmea.Checked:=GState.GPS_NMEALog;
 SpinEdit1.Value:=GState.GPS_Delay;
 SESizeStr.Value:=GState.GPS_ArrowSize;
 SESizeTrack.Value:=GState.GPS_TrackWidth;
 SE_NumTrackPoints.Value:=GState.GPS_NumTrackPoints;
 CBSensorsBarAutoShow.Checked:=GState.GPS_SensorsAutoShow;
 ScrolInvert.Checked:=GState.MouseWheelInv;
 ComboBox2.ItemIndex:=byte(GState.Resampling);
 ComboBoxCOM.Text:=GState.GPS_COM;
 ComboBoxBoudRate.Text:=inttostr(GState.GPS_BaudRate);
 TrBarGamma.Position:=GState.GammaN;
 if GState.GammaN<50 then LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((GState.GammaN*2)/100)+')'
              else LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((GState.GammaN-40)/10)+')';
 TrBarcontrast.Position:=GState.ContrastN;
 LabelContrast.Caption:=SAS_STR_Contrast+' ('+inttostr(GState.ContrastN)+')';
 ComboBox1.ItemIndex := byte(GState.num_format);
 CBWMainColor.Selected:=GState.WikiMapMainColor;
 CBWFonColor.Selected:=GState.WikiMapFonColor;

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
  for i:=1 to 64 do begin
    CBGSMComPort.Items.Add('COM'+inttostr(i));
    ComboBoxCOM.Items.Add('COM'+inttostr(i));
  end;
  FShortcutEditor := TShortcutEditor.Create(List);
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
 FEditMap.FMapType := TMapType(MapList.Selected.Data);
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

procedure TFSettings.Button18Click(Sender: TObject);
begin
 showMessage(TMapType(MapList.Selected.Data).MapInfo);
end;

procedure TFSettings.SBGetComNumClick(Sender: TObject);
var
  pPort: TCommPort;
  pBaudRate: TBaudRate;
begin
 SBGetComNum.Enabled:=false;
 if Fmain.GPSReceiver.FastDetectGPS(pPort, pBaudRate) then
  begin
   ComboBoxCOM.Text := Fmain.GPSReceiver.CommPortToString(pPort);
   ComboBoxBoudRate.Text := IntToStr( Fmain.GPSReceiver.BaudRateToInt(pBaudRate));
   ShowMessage('Ok');
  end
  else ShowMessage(SAS_MSG_NoGPSdetected);
 SBGetComNum.Enabled:=true;
end;

procedure TFSettings.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FShortcutEditor);
end;

end.


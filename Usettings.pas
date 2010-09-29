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
  u_CommonFormAndFrameParents,
  UMapType,
  UResStrings,
  u_ShortcutManager, fr_ShortCutList;

type
  TFSettings = class(TCommonFormParent)
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet1: TTabSheet;
    Label2: TLabel;
    btnCancel: TButton;
    btnOk: TButton;
    TabSheet3: TTabSheet;
    GroupBox1: TGroupBox;
    ScrolInvert: TCheckBox;
    TabSheet4: TTabSheet;
    btnApply: TButton;
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
    Label3: TLabel;
    ComboBox1: TComboBox;
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
    Label14: TLabel;
    smmapdif: TSpinEdit;
    Label16: TLabel;
    ComboBox2: TComboBox;
    SpinEditMiniMap: TSpinEdit;
    Label17: TLabel;
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
    Label84: TLabel;
    CBShowmapname: TCheckBox;
    CBinvertcolor: TCheckBox;
    SESizeStr: TSpinEdit;
    Label11: TLabel;
    Label10: TLabel;
    ColorBoxGPSstr: TColorBox;
    Label12: TLabel;
    GroupBox4: TGroupBox;
    CBProxyused: TCheckBox;
    EditPass: TEdit;
    EditLogin: TEdit;
    Label25: TLabel;
    CBLogin: TCheckBox;
    EditIP: TEdit;
    Label27: TLabel;
    Label28: TLabel;
    SpinEditBorderAlpha: TSpinEdit;
    ColorBoxBorder: TColorBox;
    CBDblDwnl: TCheckBox;
    CkBGoNextTile: TCheckBox;
    TabSheet9: TTabSheet;
    Button11: TButton;
    Button12: TButton;
    Button15: TButton;
    MapList: TListView;
    GroupBox5: TGroupBox;
    CBoxLocal: TComboBox;
    Label8: TLabel;
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
    CBlock_toolbars: TCheckBox;
    Label30: TLabel;
    SETilesOCache: TSpinEdit;
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
    Label35: TLabel;
    ColorBoxBackGround: TColorBox;
    CBLastSuccess: TCheckBox;
    Label36: TLabel;
    SEWaitingAnswer: TSpinEdit;
    Label37: TLabel;
    CBCacheType: TComboBox;
    Label5: TLabel;
    SE_NumTrackPoints: TSpinEdit;
    CB_GPSlogNmea: TCheckBox;
    pnlBottomButtons: TPanel;
    pnlMapsRightButtons: TPanel;
    flwpnlMemCache: TFlowPanel;
    grdpnlCache: TGridPanel;
    pnlProxyUrl: TPanel;
    lblUseProxy: TLabel;
    lblProxyLogin: TLabel;
    flwpnlProxyAuth: TFlowPanel;
    chkUseIEProxy: TCheckBox;
    pnlUseIEProxy: TPanel;
    pnlDownloadParams: TPanel;
    flwpnlDownloadTimeOut: TFlowPanel;
    pnlNumbersFormat: TPanel;
    pnlCoordFormat: TPanel;
    pnlUILeft: TPanel;
    pnlLonLatFormat: TPanel;
    flwpnlMiniMap: TFlowPanel;
    pnlImageProcess: TPanel;
    pnlResize: TPanel;
    flwpnlTileBorders: TFlowPanel;
    pnlTileBorders: TPanel;
    pnlUIRight: TPanel;
    flwpnlMiniMapAlfa: TFlowPanel;
    flwpnlTileBorder: TFlowPanel;
    pnlShowMapName: TPanel;
    pnlLang: TPanel;
    pnlFillMap: TPanel;
    flwpnlFillMap: TFlowPanel;
    pnlLockToolbars: TPanel;
    pnlShowPointDescr: TPanel;
    pnlBgColor: TPanel;
    grdpnlUI: TGridPanel;
    pnlGPSLeft: TPanel;
    flwpnlGpsPort: TFlowPanel;
    flwpnlGpsParams: TFlowPanel;
    pnlGpsTrackSave: TPanel;
    pnlGpsSensors: TPanel;
    pnlGpsRight: TPanel;
    grdpnlWiki: TGridPanel;
    chkPosFromGSM: TCheckBox;
    pnlGSM: TPanel;
    flwpnlGSM: TFlowPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
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
    procedure chkUseIEProxyClick(Sender: TObject);
    procedure CBProxyusedClick(Sender: TObject);
    procedure CBLoginClick(Sender: TObject);
    procedure chkPosFromGSMClick(Sender: TObject);
    procedure CBoxLocalChange(Sender: TObject);
  private
    FMapsEdit: boolean;
    frShortCutList: TfrShortCutList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save;
    procedure InitMapsList;
    procedure RefreshTranslation; override;
  end;

var
  FSettings: TFSettings;
  procedure SetProxy;

implementation

uses
  Types,
  Menus,
  TB2Item,
  u_GlobalState,
  u_GeoToStr,
  Uimgfun,
  Unit1,
  UEditMap;

{$R *.dfm}

procedure TFSettings.Save;
begin
  try
    GState.SaveMaps;
    GState.ViewState.SaveViewPortState;
    GState.SaveMainParams;
    Fmain.ShortCutManager.Save(GState.MainIni, 'HOTKEY');
    Fmain.SaveWindowConfigToIni;
    GState.MainIni.UpdateFile;
  except
  end;
end;

procedure TFSettings.btnCancelClick(Sender: TObject);
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

procedure TFSettings.btnApplyClick(Sender: TObject);
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
 GState.GSMpar.auto:=chkPosFromGSM.Checked;
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
  GState.CacheConfig.DefCache := CBCacheType.ItemIndex+1;
 end else begin
  GState.CacheConfig.DefCache := 2;
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
 if (chkUseIEProxy.Checked)and(not GState.InetConnect.userwinset) then ShowMessage(SAS_MSG_need_reload_application_curln);
 GState.InetConnect.userwinset:=chkUseIEProxy.Checked;
 GState.InetConnect.proxyused:=CBProxyused.Checked;
 GState.InetConnect.uselogin:=CBLogin.Checked;
 GState.InetConnect.proxystr:=EditIP.Text;
 GState.InetConnect.loginstr:=EditLogin.Text;
 GState.InetConnect.passstr:=EditPass.Text;
 if (GState.InetConnect.TimeOut<>SETimeOut.Value) then ShowMessage(SAS_MSG_need_reload_application_curln);
 GState.InetConnect.TimeOut:=SETimeOut.Value;

 GState.SaveTileNotExists:=CBSaveTileNotExists.Checked;
 GState.MouseWheelInv:=ScrolInvert.Checked;
 GState.CacheConfig.NewCPath:=IncludeTrailingPathDelimiter(NewCPath.Text);
 GState.CacheConfig.OldCPath:=IncludeTrailingPathDelimiter(OldCPath.Text);
 GState.CacheConfig.ESCPath:=IncludeTrailingPathDelimiter(EScPath.Text);
 GState.CacheConfig.GMTilesPath:=IncludeTrailingPathDelimiter(GMTilesPath.Text);
 GState.CacheConfig.GECachePath:=IncludeTrailingPathDelimiter(GECachePath.Text);
 GState.GammaN:=TrBarGamma.Position;
 GState.ContrastN:=TrBarContrast.Position;
 GState.num_format := TDistStrFormat(ComboBox1.ItemIndex);
 GState.WikiMapMainColor:=CBWMainColor.Selected;
 GState.WikiMapFonColor:=CBWFonColor.Selected;

 GState.LanguageManager.SetCurrentLangIndex(CBoxLocal.ItemIndex);

 GState.TilesOut:=SpinEdit3.Value;

 SetProxy;

 save;
 if FMapsEdit then begin
   Fmain.CreateMapUI;
 end;
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

procedure TFSettings.CBLoginClick(Sender: TObject);
var
  VUseAuth: Boolean;
begin
  VUseAuth := CBLogin.Enabled and CBLogin.Checked;
  EditLogin.Enabled := VUseAuth;
  Label25.Enabled := VUseAuth;
  EditPass.Enabled := VUseAuth;
end;

procedure TFSettings.CBoxLocalChange(Sender: TObject);
begin
 GState.LanguageManager.SetCurrentLangIndex(CBoxLocal.ItemIndex);
end;

procedure TFSettings.CBProxyusedClick(Sender: TObject);
var
  VUseProxy: Boolean;
begin
  VUseProxy := CBProxyused.Enabled and CBProxyused.Checked;
  EditIP.Enabled := VUseProxy;
  CBLogin.Enabled := VUseProxy;
  lblProxyLogin.Enabled := VUseProxy;
  CBLoginClick(CBLogin);
end;

procedure TFSettings.chkPosFromGSMClick(Sender: TObject);
var
  VUseGSM: Boolean;
  i: Integer;
  VControl: TControl;
begin
  VUseGSM := chkPosFromGSM.Checked;
  for i := 0 to flwpnlGSM.ControlCount - 1 do begin
    VControl := flwpnlGSM.Controls[i];
    VControl.Enabled := VUseGSM;
  end;
end;

procedure TFSettings.chkUseIEProxyClick(Sender: TObject);
var
  VUseIeProxy: Boolean;
begin
  VUseIeProxy := chkUseIEProxy.Checked;
  CBProxyused.Enabled := not VUseIeProxy;
  lblUseProxy.Enabled := not VUseIeProxy;
  CBProxyusedClick(CBProxyused);
end;

constructor TFSettings.Create(AOwner: TComponent);
begin
  inherited;
  frShortCutList := TfrShortCutList.Create(nil);
  frShortCutList.Parent := GroupBox5;
  PageControl1.ActivePageIndex:=0;
end;

destructor TFSettings.Destroy;
begin
  FreeAndNil(frShortCutList);
  inherited;
end;

procedure TFSettings.FormShow(Sender: TObject);
begin
 FMapsEdit:=false;
 CBoxLocal.Clear;
 GState.LanguageManager.GetLangNames(CBoxLocal.Items);
 CBoxLocal.ItemIndex := GState.LanguageManager.GetCurrentLangIndex;

 CBLastSuccess.Checked:=GState.SessionLastSuccess;
 ColorBoxBackGround.Selected:=GState.BGround;
 CBGSMBaundRate.text:=inttostr(GState.GSMpar.BaudRate);
 CBGSMComPort.Text:=GState.GSMpar.Port;
 chkPosFromGSM.Checked:=GState.GSMpar.auto;
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
 chkUseIEProxy.Checked:=GState.InetConnect.userwinset;
 CBProxyused.Checked:=GState.InetConnect.proxyused;
 CBLogin.Checked:=GState.InetConnect.uselogin;
 CBSaveTileNotExists.Checked:=GState.SaveTileNotExists;
 EditIP.Text:=GState.InetConnect.proxystr;
 EditLogin.Text:=GState.InetConnect.loginstr;
 EditPass.Text:=GState.InetConnect.passstr;
 ColorBoxGPSstr.Selected:=GState.GPS_ArrowColor;
 CBinvertcolor.Checked:=GState.InvertColor;
 ColorBoxBorder.Selected:=GState.BorderColor;
 SpinEditBorderAlpha.Value:=GState.BorderAlpha;
 CBBorderText.Checked:=GState.ShowBorderText;
 CBCacheType.ItemIndex:=GState.CacheConfig.DefCache-1;
 CBShowmapname.Checked:=GState.ShowMapName;
 CB_llstrType.ItemIndex:=byte(GState.llStrType);
 OldCPath.text:=GState.CacheConfig.OldCPath;
 NewCPath.text:=GState.CacheConfig.NewCPath;
 ESCPath.text:=GState.CacheConfig.ESCPath;
 GMTilesPath.text:=GState.CacheConfig.GMTilesPath;
 GECachePath.text:=GState.CacheConfig.GECachePath;
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

 SpinEdit3.Value:=GState.TilesOut;
 chkPosFromGSMClick(chkPosFromGSM);
 chkUseIEProxyClick(chkUseIEProxy);
 frShortCutList.SetShortCutManager(Fmain.ShortCutManager);
end;

procedure TFSettings.FormCreate(Sender: TObject);
var i:integer;
begin
  ComboBoxCOM.Items.Clear;
  for i:=1 to 64 do begin
    CBGSMComPort.Items.Add('COM'+inttostr(i));
    ComboBoxCOM.Items.Add('COM'+inttostr(i));
  end;
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
 FMapsEdit:=true;
 If (MapList.Selected<>nil)and(MapList.Selected.Index>0) then
  ExchangeItems(MapList, MapList.Selected.Index,MapList.Selected.Index-1);
end;

procedure TFSettings.Button11Click(Sender: TObject);
begin
 FMapsEdit:=true;
 If (MapList.Selected<>nil)and(MapList.Selected.Index<MapList.Items.Count-1) then
  ExchangeItems(MapList, MapList.Selected.Index,MapList.Selected.Index+1)
end;

procedure TFSettings.Button15Click(Sender: TObject);
var
  VMapType: TMapType;
begin
  VMapType := TMapType(MapList.Selected.Data);
  if FEditMap.EditMapModadl(VMapType) then begin
    FMapsEdit := True;
  end;
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

procedure TFSettings.RefreshTranslation;
begin
  inherited;
  FormShow(Self);
  frShortCutList.RefreshTranslation;
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

procedure TFSettings.InitMapsList;
var
  i: integer;
  VMapType: TMapType;
begin
  MapList.Clear;
  for i:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[i];
    With VMapType do begin
      MapList.AddItem(VMapType.name,nil);
      MapList.Items.Item[i].Data:=VMapType;
      MapList.Items.Item[i].SubItems.Add(VMapType.TileStorage.CacheConfig.NameInCache);
      if VMapType.asLayer then begin
        MapList.Items.Item[i].SubItems.Add(SAS_STR_Layers+'\'+VMapType.ParentSubMenu);
      end else begin
        MapList.Items.Item[i].SubItems.Add(SAS_STR_Maps+'\'+VMapType.ParentSubMenu);
      end;
      MapList.Items.Item[i].SubItems.Add(ShortCutToText(VMapType.HotKey));
      MapList.Items.Item[i].SubItems.Add(VMapType.ZmpFileName);
    end;
  end;
  if MapList.Items.Count>0 then begin
    MapList.Items.Item[0].Selected:=true;
  end;
end;

end.


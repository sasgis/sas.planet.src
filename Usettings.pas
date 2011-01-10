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
  urlmon,
  wininet,
  GR32,
  GR32_Image,
  GR32_Backends,
  XPMan,
  ZylGPSReceiver,
  u_CommonFormAndFrameParents,
  i_IConfigDataWriteProvider,
  i_IImageResamplerFactory,
  fr_ShortCutList,
  UMapType,
  UResStrings;

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
    Label16: TLabel;
    ComboBox2: TComboBox;
    MiniMapAlphaEdit: TSpinEdit;
    Label17: TLabel;
    TilesOverScreenEdit: TSpinEdit;
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
    GroupBox3: TGroupBox;
    SatellitePaintBox: TImage32;
    PaintBox1: TPaintBox;
    Label13: TLabel;
    Label18: TLabel;
    Label9: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Bevel1: TBevel;
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
    procedure MapListCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure Button18Click(Sender: TObject);
    procedure SBGetComNumClick(Sender: TObject);
    procedure chkUseIEProxyClick(Sender: TObject);
    procedure CBProxyusedClick(Sender: TObject);
    procedure CBLoginClick(Sender: TObject);
    procedure chkPosFromGSMClick(Sender: TObject);
    procedure CBoxLocalChange(Sender: TObject);
    procedure SatellitePaintBoxResize(Sender: TObject);
    procedure TabSheet5Show(Sender: TObject);
  private
    FMapsEdit: boolean;
    frShortCutList: TfrShortCutList;
    procedure InitResamplersList(AList: IImageResamplerFactoryList; ABox: TComboBox);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save(AProvider: IConfigDataWriteProvider);
    procedure InitMapsList;
    procedure RefreshTranslation; override;
    procedure SatellitePaint;
  end;

var
  FSettings: TFSettings;
  procedure SetProxy;

implementation

uses
  Types,
  Menus,
  t_CommonTypes,
  i_IProxySettings,
  i_GPS,
  u_GlobalState,
  Unit1,
  UEditMap;

{$R *.dfm}

procedure TFSettings.Save(AProvider: IConfigDataWriteProvider);
begin
  try
    GState.ViewState.SaveViewPortState(AProvider);
    GState.SaveMainParams;
    Fmain.ShortCutManager.Save(AProvider.GetOrCreateSubItem('HOTKEY'));
    Fmain.SaveWindowConfigToIni(AProvider);
  except
  end;
end;

procedure TFSettings.btnCancelClick(Sender: TObject);
begin
  Close
end;

procedure SetProxy;
var
  PIInfo : PInternetProxyInfo;
  VProxyConfig: IProxyConfig;
  VUseIEProxy: Boolean;
  VUseProxy: Boolean;
  VHost: string;
begin
  VProxyConfig := GState.InetConfig.ProxyConfig;
  VProxyConfig.LockRead;
  try
    VUseIEProxy := VProxyConfig.GetUseIESettings;
    VUseProxy := VProxyConfig.GetUseProxy;
    VHost := VProxyConfig.GetHost;
  finally
    VProxyConfig.UnlockRead;
  end;
  New (PIInfo);
  if not(VUseIEProxy) then begin
    if VUseProxy then begin
      PIInfo^.dwAccessType := INTERNET_OPEN_TYPE_PROXY ;
      PIInfo^.lpszProxy := PChar(VHost);
      PIInfo^.lpszProxyBypass := nil;
      UrlMkSetSessionOption(INTERNET_OPTION_PROXY, piinfo, SizeOf(Internet_Proxy_Info), 0);
    end else  begin
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
var
  i: integer;
  VProxyConfig: IProxyConfig;
  VInetConfig: IInetConfig;
begin
 For i:=0 to MapList.Items.Count-1 do
  begin
   TMapType(MapList.Items.Item[i].data).FSortIndex:=i+1;
  end;
 GState.MapType.SortList;

 Fmain.LayerMiniMap.MasterAlpha:=MiniMapAlphaEdit.Value;

 GState.SessionLastSuccess:=CBLastSuccess.Checked;
 GState.BGround:=ColorBoxBackGround.Selected;
 FMain.map.Color:=GState.BGround;
 GState.GSMpar.LockWrite;
 try
   GState.GSMpar.SetUseGSMByCOM(chkPosFromGSM.Checked);
   GState.GSMpar.SetBaudRate(strtoint(CBGSMBaundRate.text));
   GState.GSMpar.SetPortName(CBGSMComPort.Text);
   GState.GSMpar.SetWaitTime(SEWaitingAnswer.Value);
 finally
   GState.GSMpar.UnlockWrite;
 end;

 GState.ShowHintOnMarks:=CBShowHintOnMarks.checked;
  GState.MainMemCacheConfig.MaxSize := SETilesOCache.value;
 GState.MapZapColor:=MapZapColorBox.Selected;
 GState.MapZapAlpha:=MapZapAlphaEdit.Value;
 GState.TwoDownloadAttempt:=CBDblDwnl.Checked;
 GState.GoNextTileIfDownloadError:=CkBGoNextTile.Checked;
 GState.MainFormConfig.LayersConfig.GPSMarker.MarkerMovedColor := SetAlpha(Color32(ColorBoxGPSstr.selected), 150);
 GState.BitmapPostProcessingConfig.LockWrite;
 try
   GState.BitmapPostProcessingConfig.InvertColor:=CBinvertcolor.Checked;
   GState.BitmapPostProcessingConfig.GammaN:=TrBarGamma.Position;
   GState.BitmapPostProcessingConfig.ContrastN:=TrBarContrast.Position;
 finally
   GState.BitmapPostProcessingConfig.UnlockWrite;
 end;
  GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.LockWrite;
  try
    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.TileGrid.GridColor := SetAlpha(Color32(ColorBoxBorder.Selected),SpinEditBorderAlpha.Value);
    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.TileGrid.ShowText:=CBBorderText.Checked;
  finally
    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.UnlockWrite;
  end;
 if CBCacheType.ItemIndex >= 0 then begin
  GState.CacheConfig.DefCache := CBCacheType.ItemIndex+1;
 end else begin
  GState.CacheConfig.DefCache := 2;
 end;
  GState.ValueToStringConverterConfig.LockWrite;
  try
    GState.ValueToStringConverterConfig.IsLatitudeFirst := ChBoxFirstLat.Checked;
    GState.ValueToStringConverterConfig.DegrShowFormat := TDegrShowFormat(CB_llstrType.ItemIndex);
    GState.ValueToStringConverterConfig.DistStrFormat := TDistStrFormat(ComboBox1.ItemIndex);
  finally
    GState.ValueToStringConverterConfig.UnlockWrite;
  end;
  if ComboBox2.ItemIndex > 0 then begin
    GState.ImageResamplerConfig.ActiveIndex := ComboBox2.ItemIndex;
  end;

  GState.MainFormConfig.LayersConfig.GPSMarker.MarkerMovedSize := SESizeStr.Value;
  GState.MainFormConfig.LayersConfig.GPSTrackConfig.LockWrite;
  try
    GState.MainFormConfig.LayersConfig.GPSTrackConfig.LineWidth := SESizeTrack.Value;
    GState.MainFormConfig.LayersConfig.GPSTrackConfig.LastPointCount := SE_NumTrackPoints.Value;
  finally
    GState.MainFormConfig.LayersConfig.GPSTrackConfig.UnlockWrite;
  end;
 GState.GPSpar.GPSSettings.ConnectionTimeout:=SpinEdit2.Value;
 GState.GPSpar.GPS_WriteLog:=CB_GPSlog.Checked;
 GState.GPSpar.GPSSettings.NMEALog:=CB_GPSlogNmea.Checked;
 GState.GPSpar.GPSSettings.Delay:=SpinEdit1.Value;
 GState.MainFormConfig.ToolbarsLock.SetLock(CBlock_toolbars.Checked);
 GState.GPSpar.GPSSettings.Port := StrToInt(Copy(ComboBoxCOM.Text, 4, 2));
 GState.GPSpar.GPSSettings.BaudRate:=StrToint(ComboBoxBoudRate.Text);
  GState.MainFormConfig.GPSBehaviour.SensorsAutoShow := CBSensorsBarAutoShow.Checked;
  VInetConfig :=GState.InetConfig;
  VInetConfig.LockWrite;
  try
    VProxyConfig := VInetConfig.ProxyConfig;
    VProxyConfig.SetUseIESettings(chkUseIEProxy.Checked);
    VProxyConfig.SetUseProxy(CBProxyused.Checked);
    VProxyConfig.SetHost(EditIP.Text);
    VProxyConfig.SetUseLogin(CBLogin.Checked);
    VProxyConfig.SetLogin(EditLogin.Text);
    VProxyConfig.SetPassword(EditPass.Text);
    VInetConfig.SetTimeOut(SETimeOut.Value);
  finally
    VInetConfig.UnlockWrite;
  end;

 GState.SaveTileNotExists:=CBSaveTileNotExists.Checked;
  GState.MainFormConfig.MainConfig.LockWrite;
  try
    GState.MainFormConfig.MainConfig.ShowMapName := CBShowmapname.Checked;
    GState.MainFormConfig.MainConfig.MouseScrollInvert := ScrolInvert.Checked;
  finally
    GState.MainFormConfig.MainConfig.UnlockWrite;
  end;
 GState.CacheConfig.NewCPath:=IncludeTrailingPathDelimiter(NewCPath.Text);
 GState.CacheConfig.OldCPath:=IncludeTrailingPathDelimiter(OldCPath.Text);
 GState.CacheConfig.ESCPath:=IncludeTrailingPathDelimiter(EScPath.Text);
 GState.CacheConfig.GMTilesPath:=IncludeTrailingPathDelimiter(GMTilesPath.Text);
 GState.CacheConfig.GECachePath:=IncludeTrailingPathDelimiter(GECachePath.Text);
 GState.WikiMapMainColor:=CBWMainColor.Selected;
 GState.WikiMapFonColor:=CBWFonColor.Selected;

 GState.LanguageManager.SetCurrentLangIndex(CBoxLocal.ItemIndex);

 GState.TilesOut:=TilesOverScreenEdit.Value;

 SetProxy;

 save(GState.MainConfigProvider);
 if FMapsEdit then begin
   Fmain.CreateMapUI;
 end;
 ShowMessage(SAS_MSG_need_reload_application_curln);
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
  PageControl1.ActivePageIndex:=0;
end;

destructor TFSettings.Destroy;
begin
  FreeAndNil(frShortCutList);
  inherited;
end;

procedure TFSettings.FormShow(Sender: TObject);
var
  VProxyConfig: IProxyConfig;
  VInetConfig: IInetConfig;
begin
 FMapsEdit:=false;
 CBoxLocal.Clear;
 frShortCutList.Parent := GroupBox5;
 GState.LanguageManager.GetLangNames(CBoxLocal.Items);
 CBoxLocal.ItemIndex := GState.LanguageManager.GetCurrentLangIndex;

 MiniMapAlphaEdit.Value:=Fmain.LayerMiniMap.MasterAlpha;

 CBLastSuccess.Checked:=GState.SessionLastSuccess;
 ColorBoxBackGround.Selected:=GState.BGround;
  GState.GSMpar.LockRead;
  try
    chkPosFromGSM.Checked := GState.GSMpar.GetUseGSMByCOM;
    CBGSMComPort.Text := GState.GSMpar.GetPortName;
    CBGSMBaundRate.text := inttostr(GState.GSMpar.GetBaudRate);
    SEWaitingAnswer.Value := GState.GSMpar.GetWaitTime;
  finally
    GState.GSMpar.UnlockRead;
  end;
  VInetConfig := GState.InetConfig;
  VInetConfig.LockRead;
  try
    SETimeOut.Value := VInetConfig.GetTimeOut;
    VProxyConfig := VInetConfig.ProxyConfig;
    chkUseIEProxy.Checked := VProxyConfig.GetUseIESettings;
    CBProxyused.Checked := VProxyConfig.GetUseProxy;
    CBLogin.Checked := VProxyConfig.GetUseLogin;
    EditIP.Text := VProxyConfig.GetHost;
    EditLogin.Text := VProxyConfig.GetLogin;
    EditPass.Text := VProxyConfig.GetPassword;
  finally
    VInetConfig.UnlockRead;
  end;
 CBShowHintOnMarks.Checked:=GState.ShowHintOnMarks;
  SETilesOCache.Value := GState.MainMemCacheConfig.MaxSize;
 MapZapColorBox.Selected:=GState.MapZapColor;
 MapZapAlphaEdit.Value:=GState.MapZapAlpha;
 CBDblDwnl.Checked:=GState.TwoDownloadAttempt;
 CBlock_toolbars.Checked:=GState.MainFormConfig.ToolbarsLock.GetIsLock;
 CkBGoNextTile.Checked:=GState.GoNextTileIfDownloadError;
 CBSaveTileNotExists.Checked:=GState.SaveTileNotExists;
  ColorBoxGPSstr.Selected := WinColor(GState.MainFormConfig.LayersConfig.GPSMarker.MarkerMovedColor);
  GState.BitmapPostProcessingConfig.LockRead;
  try
    CBinvertcolor.Checked := GState.BitmapPostProcessingConfig.InvertColor;
    TrBarGamma.Position:=GState.BitmapPostProcessingConfig.GammaN;
    TrBarcontrast.Position:=GState.BitmapPostProcessingConfig.ContrastN;
  finally
    GState.BitmapPostProcessingConfig.UnlockRead;
  end;
  if TrBarGamma.Position < 50 then begin
    LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position*2)/100)+')';
  end else begin
    LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position-40)/10)+')';
  end;
  LabelContrast.Caption:=SAS_STR_Contrast+' ('+inttostr(TrBarcontrast.Position)+')';

  GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.LockRead;
  try
    ColorBoxBorder.Selected:=WinColor(GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.TileGrid.GridColor);
    SpinEditBorderAlpha.Value:=AlphaComponent(GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.TileGrid.GridColor);
    CBBorderText.Checked:=GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.TileGrid.ShowText;
  finally
    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.UnlockRead;
  end;

  GState.MainFormConfig.MainConfig.LockRead;
  try
    CBShowmapname.Checked := GState.MainFormConfig.MainConfig.ShowMapName;
    ScrolInvert.Checked := GState.MainFormConfig.MainConfig.MouseScrollInvert;
  finally
    GState.MainFormConfig.MainConfig.UnlockRead;
  end;

 CBCacheType.ItemIndex:=GState.CacheConfig.DefCache-1;
 OldCPath.text:=GState.CacheConfig.OldCPath;
 NewCPath.text:=GState.CacheConfig.NewCPath;
 ESCPath.text:=GState.CacheConfig.ESCPath;
 GMTilesPath.text:=GState.CacheConfig.GMTilesPath;
 GECachePath.text:=GState.CacheConfig.GECachePath;
 SpinEdit2.Value:=GState.GPSpar.GPSSettings.ConnectionTimeout;
 CB_GPSlog.Checked:=GState.GPSpar.GPS_WriteLog;
 CB_GPSlogNmea.Checked:=GState.GPSpar.GPSSettings.NMEALog;
 SpinEdit1.Value:=GState.GPSpar.GPSSettings.Delay;
  SESizeStr.Value:=GState.MainFormConfig.LayersConfig.GPSMarker.MarkerMovedSize;
  GState.MainFormConfig.LayersConfig.GPSTrackConfig.LockRead;
  try
    SESizeTrack.Value := Trunc(GState.MainFormConfig.LayersConfig.GPSTrackConfig.LineWidth);
    SE_NumTrackPoints.Value := GState.MainFormConfig.LayersConfig.GPSTrackConfig.LastPointCount;
  finally
    GState.MainFormConfig.LayersConfig.GPSTrackConfig.UnlockRead;
  end;
  CBSensorsBarAutoShow.Checked := GState.MainFormConfig.GPSBehaviour.SensorsAutoShow;
  InitResamplersList(GState.ImageResamplerConfig.GetList, ComboBox2);
  ComboBox2.ItemIndex := GState.ImageResamplerConfig.ActiveIndex;
 ComboBoxCOM.Text:= 'COM' + IntToStr(GState.GPSpar.GPSSettings.Port);
 ComboBoxBoudRate.Text:=inttostr(GState.GPSpar.GPSSettings.BaudRate);
  GState.ValueToStringConverterConfig.LockRead;
  try
    ChBoxFirstLat.Checked:=GState.ValueToStringConverterConfig.IsLatitudeFirst;
    CB_llstrType.ItemIndex:=byte(GState.ValueToStringConverterConfig.DegrShowFormat);
    ComboBox1.ItemIndex := byte(GState.ValueToStringConverterConfig.DistStrFormat);
  finally
    GState.ValueToStringConverterConfig.UnlockRead;
  end;
 CBWMainColor.Selected:=GState.WikiMapMainColor;
 CBWFonColor.Selected:=GState.WikiMapFonColor;

 TilesOverScreenEdit.Value:=GState.TilesOut;
 chkPosFromGSMClick(chkPosFromGSM);
 chkUseIEProxyClick(chkUseIEProxy);
 frShortCutList.SetShortCutManager(Fmain.ShortCutManager);
 SatellitePaint;
end;

procedure TFSettings.FormCreate(Sender: TObject);
var i:integer;
begin
  SatellitePaintBox.Bitmap.SetSizeFrom(SatellitePaintBox);

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

procedure TFSettings.TabSheet5Show(Sender: TObject);
begin
 pnlGPSLeft.Repaint;
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

procedure TFSettings.SatellitePaint;
var
  i,bar_width,bar_height,bar_x1,bar_dy,Ellipse_d,Ellipse_r,padd:integer;
  Ellipse_XY1,Ellipse_XY2,Ellipse_center:TPoint;
  VPosition: IGPSPosition;
  VSattelite: IGPSSatelliteInfo;
begin
 with SatellitePaintBox.Bitmap do begin
  Clear(clWhite);
  Canvas.Pen.Color:=clBlack;
  Canvas.Brush.Color:=clWhite;
  padd:=20;
  Ellipse_r:=(Width div 2)-padd;
  Ellipse_center:=Point(Width div 2,Ellipse_r+5);
  for I := 0 to 8 do begin
    Ellipse_d:=Ellipse_r-(((Ellipse_r) div 9)*i);
    Ellipse_XY1.x:=round(Ellipse_center.x + Ellipse_d * cos((0) * (Pi/180)));
    Ellipse_XY1.y:=round(Ellipse_center.y + Ellipse_d * sin((270) * (Pi/180)));
    Ellipse_XY2.x:=round(Ellipse_center.x + Ellipse_d * cos((180) * (Pi/180)));
    Ellipse_XY2.y:=round(Ellipse_center.y + Ellipse_d * sin((90) * (Pi/180)));
    Canvas.Ellipse(Ellipse_XY1.x,Ellipse_XY1.y,Ellipse_XY2.x,Ellipse_XY2.y);
  end;
  for I := 0 to 15 do begin
    Ellipse_XY1.x := round(Ellipse_center.x + Ellipse_r * cos((i*22.5) * (Pi / 180)));
    Ellipse_XY1.y := round(Ellipse_center.y + Ellipse_r * sin((i*22.5) * (Pi / 180)));
    Canvas.MoveTo(Ellipse_center.x,Ellipse_center.y);
    Canvas.LineTo(Ellipse_XY1.x,Ellipse_XY1.y);
  end;

  VPosition := GState.GPSpar.GPSModule.Position;

  for I := 0 to VPosition.Satellites.Count-1 do begin
    VSattelite := VPosition.Satellites.Item[i];
    Ellipse_r:=trunc(((Width div 2)-padd)*((90-VSattelite.Elevation)/90));
    Ellipse_XY1.x:=round(Ellipse_center.x + Ellipse_r * cos((VSattelite.Azimuth-90) * (Pi / 180)));
    Ellipse_XY1.y:=round(Ellipse_center.y + Ellipse_r * sin((VSattelite.Azimuth-90) * (Pi / 180)));
    if VSattelite.IsFix then begin
      Canvas.Brush.Color:=clGreen;
    end else begin
      if VSattelite.SignalToNoiseRatio=0 then begin
        Canvas.Brush.Color:=clRed;
      end else begin
        Canvas.Brush.Color:=clYellow;
      end;
    end;
    Canvas.Ellipse(Ellipse_XY1.x-10,Ellipse_XY1.y-10,Ellipse_XY1.x+10,Ellipse_XY1.y+10);
    Canvas.TextOut(Ellipse_XY1.x-5,Ellipse_XY1.y-7,inttostr(VSattelite.PseudoRandomCode));
  end;

  bar_width:=(Width div 16);
  bar_height:=42;
  Canvas.Brush.Color:=clWhite;
  Canvas.Pen.Color:=clBlack;

  bar_dy:=65;
  for I := 0 to 31 do begin
   bar_x1:=(i mod 16)*bar_width;
   if i=16 then begin
     bar_dy:=11;
   end;
   Canvas.TextOut(bar_x1+1,Height-bar_dy-1,inttostr(i+1));
   Canvas.Rectangle(bar_x1+1,Height-bar_dy-bar_height,bar_x1+bar_width-1,Height-bar_dy);
  end;
  for I := 0 to VPosition.Satellites.Count-1 do begin
   VSattelite := VPosition.Satellites.Item[i];
   if VSattelite.PseudoRandomCode>16 then begin
     bar_dy:=12;
   end else begin
     bar_dy:=66;
   end;
   bar_x1:=(bar_width*((VSattelite.PseudoRandomCode-1) mod 16));
   bar_height:=trunc((VSattelite.SignalToNoiseRatio)/2.5);
   if VSattelite.IsFix then begin
      Canvas.Brush.Color:=clGreen;
   end else begin
     if VSattelite.SignalToNoiseRatio=0 then begin
       Canvas.Brush.Color:=clRed;
     end else begin
       Canvas.Brush.Color:=clYellow;
     end;
   end;
   Canvas.Pen.Color:=clWhite;
   Canvas.Rectangle(bar_x1+2,Height-bar_dy-bar_height,bar_x1+bar_width-2,Height-bar_dy);
 end;
 end;
end;

procedure TFSettings.SatellitePaintBoxResize(Sender: TObject);
begin
  SatellitePaintBox.Bitmap.SetSizeFrom(SatellitePaintBox);
  SatellitePaintBox.Bitmap.Clear(clWhite32);
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
 ShowMessage(SAS_MSG_NoGPSdetected);
 SBGetComNum.Enabled:=true;
end;

procedure TFSettings.InitMapsList;
var
  i: integer;
  VMapType: TMapType;
begin
  MapList.Clear;
  for i:=0 to GState.MapType.Count-1 do begin
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

procedure TFSettings.InitResamplersList(AList: IImageResamplerFactoryList; ABox: TComboBox);
var
  i: Integer;
begin
  ABox.Items.Clear;
  for i := 0 to AList.Count - 1 do begin
    ABox.Items.Add(AList.Captions[i]);
  end;
end;

end.


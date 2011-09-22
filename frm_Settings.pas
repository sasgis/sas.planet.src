unit frm_Settings;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Graphics,
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  ExtCtrls,
  ComCtrls,
  Dialogs,
  Buttons,
  Spin,
  urlmon,
  wininet,
  GR32,
  GR32_Image,
  u_CommonFormAndFrameParents,
  i_ConfigDataWriteProvider,
  i_ImageResamplerFactory,
  fr_ShortCutList,
  u_MapType,
  u_ResStrings;

type
  TfrmSettings = class(TCommonFormParent)
    PageControl1: TPageControl;
    tsCache: TTabSheet;
    tsInternet: TTabSheet;
    Label2: TLabel;
    btnCancel: TButton;
    btnOk: TButton;
    tsControl: TTabSheet;
    GroupBox1: TGroupBox;
    ScrolInvert: TCheckBox;
    tsView: TTabSheet;
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
    LabelGamma: TLabel;
    TrBarContrast: TTrackBar;
    LabelContrast: TLabel;
    tsGPS: TTabSheet;
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
    tsWiki: TTabSheet;
    CBWMainColor: TColorBox;
    lblWikiMainColor: TLabel;
    lblWikiBgColor: TLabel;
    CBWFonColor: TColorBox;
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
    tsMaps: TTabSheet;
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
    btnMapInfo: TButton;
    CBSensorsBarAutoShow: TCheckBox;
    Label32: TLabel;
    SETimeOut: TSpinEdit;
    tsGSM: TTabSheet;
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
    pnlDistFormat: TPanel;
    pnlUILeft: TPanel;
    pnlLonLatFormat: TPanel;
    pnlImageProcess: TPanel;
    pnlResize: TPanel;
    flwpnlTileBorders: TFlowPanel;
    pnlTileBorders: TPanel;
    pnlUIRight: TPanel;
    flwpnlMiniMapAlfa: TFlowPanel;
    flwpnlTileBorder: TFlowPanel;
    pnlOptions: TPanel;
    pnlLang: TPanel;
    pnlFillMap: TPanel;
    flwpnlFillMap: TFlowPanel;
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
    lblSatInfoVisible: TLabel;
    lblSatInfoZeroSignal: TLabel;
    lblSatInfoActive: TLabel;
    shpSatInfoActive: TShape;
    shpSatInfoVisible: TShape;
    shpSatInfoZeroSignal: TShape;
    pnlSatInfoLegend: TPanel;
    pnlSatInfoActive: TPanel;
    pnlSatInfoVisible: TPanel;
    pnlSatInfoZeroSignal: TPanel;
    CBMinimizeToTray: TCheckBox;
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
    procedure btnMapInfoClick(Sender: TObject);
    procedure chkUseIEProxyClick(Sender: TObject);
    procedure CBProxyusedClick(Sender: TObject);
    procedure CBLoginClick(Sender: TObject);
    procedure chkPosFromGSMClick(Sender: TObject);
    procedure CBoxLocalChange(Sender: TObject);
    procedure SatellitePaintBoxResize(Sender: TObject);
    procedure tsGPSShow(Sender: TObject);
    procedure MapListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    frShortCutList: TfrShortCutList;
    procedure InitResamplersList(AList: IImageResamplerFactoryList; ABox: TComboBox);
    procedure InitMapsList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save(AProvider: IConfigDataWriteProvider);
    procedure RefreshTranslation; override;
    procedure SatellitePaint;
  end;

var
  frmSettings: TfrmSettings;
  procedure SetProxy;

implementation

uses
  Types,
  Menus,
  t_CommonTypes,
  i_ProxySettings,
  i_InetConfig,
  i_GUIDListStatic,
  u_GlobalState,
  frm_Main,
  frm_IntrnalBrowser,
  frm_MapTypeEdit;

{$R *.dfm}

procedure TfrmSettings.Save(AProvider: IConfigDataWriteProvider);
begin
  try
    GState.SaveMainParams;
    frmMain.SaveWindowConfigToIni(AProvider);
  except
  end;
end;

procedure TfrmSettings.btnCancelClick(Sender: TObject);
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
  if VUseIEProxy then begin
    PIInfo^.dwAccessType := INTERNET_OPEN_TYPE_PRECONFIG;
    PIInfo^.lpszProxy := nil;
    PIInfo^.lpszProxyBypass := nil;
    UrlMkSetSessionOption(INTERNET_OPTION_PROXY, piinfo, SizeOf(Internet_Proxy_Info), 0);
    UrlMkSetSessionOption(INTERNET_OPTION_REFRESH, nil, 0, 0);
  end else begin
    if VUseProxy then begin
      PIInfo^.dwAccessType := INTERNET_OPEN_TYPE_PROXY ;
      PIInfo^.lpszProxy := PChar(VHost);
      PIInfo^.lpszProxyBypass := nil;
    end else  begin
      PIInfo^.dwAccessType := INTERNET_OPEN_TYPE_DIRECT;
      PIInfo^.lpszProxy := nil;
      PIInfo^.lpszProxyBypass := nil;
    end;
    UrlMkSetSessionOption(INTERNET_OPTION_PROXY, piinfo, SizeOf(Internet_Proxy_Info), 0);
    UrlMkSetSessionOption(INTERNET_OPTION_SETTINGS_CHANGED, nil, 0, 0);
  end;
  Dispose (PIInfo) ;
end;

procedure TfrmSettings.btnApplyClick(Sender: TObject);
var
  i: integer;
  VProxyConfig: IProxyConfig;
  VInetConfig: IInetConfig;
  VNeedReboot: boolean;
begin
  VNeedReboot:=false;
  GState.MapType.GUIConfigList.LockWrite;
  try
    For i:=0 to MapList.Items.Count-1 do begin
      TMapType(MapList.Items.Item[i].data).GUIConfig.SortIndex := i+1;
    end;
  finally
    GState.MapType.GUIConfigList.UnlockWrite;
  end;

  GState.MainFormConfig.LayersConfig.MiniMapLayerConfig.MasterAlpha := MiniMapAlphaEdit.Value;

  GState.DownloadConfig.LockWrite;
  try
    GState.DownloadConfig.IsUseSessionLastSuccess := CBLastSuccess.Checked;
    GState.DownloadConfig.IsGoNextTileIfDownloadError := CkBGoNextTile.Checked;
    GState.DownloadConfig.IsSaveTileNotExists := CBSaveTileNotExists.Checked;
  finally
    GState.DownloadConfig.UnlockWrite;
  end;

 GState.ViewConfig.BackGroundColor := ColorBoxBackGround.Selected;
 GState.GSMpar.LockWrite;
 try
   GState.GSMpar.SetUseGSMByCOM(chkPosFromGSM.Checked);
   GState.GSMpar.SetBaudRate(strtoint(CBGSMBaundRate.text));
   GState.GSMpar.SetPortName(CBGSMComPort.Text);
   GState.GSMpar.SetWaitTime(SEWaitingAnswer.Value);
 finally
   GState.GSMpar.UnlockWrite;
 end;
  GState.GlobalAppConfig.IsShowIconInTray := CBMinimizeToTray.Checked;
  GState.MainMemCacheConfig.MaxSize := SETilesOCache.value;

  GState.MainFormConfig.LayersConfig.FillingMapLayerConfig.NoTileColor := SetAlpha(Color32(MapZapColorBox.Selected), MapZapAlphaEdit.Value);

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

  GState.ImageResamplerConfig.ActiveIndex := ComboBox2.ItemIndex;

  GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.LockWrite;
  try
    GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.MarkerColor := SetAlpha(Color32(ColorBoxGPSstr.selected), 150);
    GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.MarkerSize := SESizeStr.Value;
  finally
    GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.UnlockWrite;
  end;
  GState.MainFormConfig.LayersConfig.GPSTrackConfig.LockWrite;
  try
    GState.MainFormConfig.LayersConfig.GPSTrackConfig.LineWidth := SESizeTrack.Value;
    GState.MainFormConfig.LayersConfig.GPSTrackConfig.LastPointCount := SE_NumTrackPoints.Value;
  finally
    GState.MainFormConfig.LayersConfig.GPSTrackConfig.UnlockWrite;
  end;
  GState.GPSConfig.LockWrite;
  try
    GState.GPSConfig.ModuleConfig.ConnectionTimeout:=SpinEdit2.Value;
    GState.GPSConfig.ModuleConfig.NMEALog:=CB_GPSlogNmea.Checked;
    GState.GPSConfig.ModuleConfig.Delay:=SpinEdit1.Value;
    GState.GPSConfig.ModuleConfig.Port := StrToInt(Copy(ComboBoxCOM.Text, 4, 2));
    GState.GPSConfig.ModuleConfig.BaudRate:=StrToint(ComboBoxBoudRate.Text);
    GState.GPSConfig.WriteLog:=CB_GPSlog.Checked;
  finally
    GState.GPSConfig.UnlockWrite;
  end;

  GState.MainFormConfig.ToolbarsLock.SetLock(CBlock_toolbars.Checked);
  GState.MainFormConfig.GPSBehaviour.SensorsAutoShow := CBSensorsBarAutoShow.Checked;
  VInetConfig :=GState.InetConfig;
  VInetConfig.LockWrite;
  try
    VProxyConfig := VInetConfig.ProxyConfig;
    if (chkUseIEProxy.Checked)and(VProxyConfig.GetUseIESettings<>chkUseIEProxy.Checked) then begin
      VNeedReboot:=true;
    end;
    VProxyConfig.SetUseIESettings(chkUseIEProxy.Checked);
    VProxyConfig.SetUseProxy(CBProxyused.Checked);
    VProxyConfig.SetHost(EditIP.Text);
    VProxyConfig.SetUseLogin(CBLogin.Checked);
    VProxyConfig.SetLogin(EditLogin.Text);
    VProxyConfig.SetPassword(EditPass.Text);
    VInetConfig.SetTimeOut(SETimeOut.Value);
    if CBDblDwnl.Checked then begin
      if VInetConfig.DownloadTryCount < 2 then begin
        VInetConfig.DownloadTryCount := 2;
      end;
    end else begin
      VInetConfig.DownloadTryCount := 1;
    end;
    SetProxy;
  finally
    VInetConfig.UnlockWrite;
  end;

  GState.MainFormConfig.MainConfig.LockWrite;
  try
    GState.MainFormConfig.MainConfig.ShowMapName := CBShowmapname.Checked;
    GState.MainFormConfig.MainConfig.MouseScrollInvert := ScrolInvert.Checked;
    GState.MainFormConfig.MainConfig.ShowHintOnMarks := CBShowHintOnMarks.checked;
  finally
    GState.MainFormConfig.MainConfig.UnlockWrite;
  end;
 GState.CacheConfig.NewCPath:=IncludeTrailingPathDelimiter(NewCPath.Text);
 GState.CacheConfig.OldCPath:=IncludeTrailingPathDelimiter(OldCPath.Text);
 GState.CacheConfig.ESCPath:=IncludeTrailingPathDelimiter(EScPath.Text);
 GState.CacheConfig.GMTilesPath:=IncludeTrailingPathDelimiter(GMTilesPath.Text);
 GState.CacheConfig.GECachePath:=IncludeTrailingPathDelimiter(GECachePath.Text);
  GState.MainFormConfig.LayersConfig.KmlLayerConfig.LockWrite;
  try
    GState.MainFormConfig.LayersConfig.KmlLayerConfig.MainColor :=
      SetAlpha(
        Color32(CBWMainColor.Selected),
        AlphaComponent(GState.MainFormConfig.LayersConfig.KmlLayerConfig.MainColor)
      );
    GState.MainFormConfig.LayersConfig.KmlLayerConfig.ShadowColor :=
      SetAlpha(
        Color32(CBWFonColor.Selected),
        AlphaComponent(GState.MainFormConfig.LayersConfig.KmlLayerConfig.ShadowColor)
      );
    GState.MainFormConfig.LayersConfig.KmlLayerConfig.PointColor :=
      SetAlpha(
        Color32(CBWMainColor.Selected),
        AlphaComponent(GState.MainFormConfig.LayersConfig.KmlLayerConfig.PointColor)
      );
  finally
    GState.MainFormConfig.LayersConfig.KmlLayerConfig.UnlockWrite;
  end;

 GState.LanguageManager.SetCurrentLanguageIndex(CBoxLocal.ItemIndex);

 GState.MainFormConfig.DownloadUIConfig.TilesOut := TilesOverScreenEdit.Value;

 save(GState.MainConfigProvider);
 if VNeedReboot then begin
   ShowMessage(SAS_MSG_need_reload_application_curln);
 end;
end;

procedure TfrmSettings.Button4Click(Sender: TObject);
begin
 if (sender as TButton).Tag=1 then OldCpath.Text:='cache_old' + PathDelim;
 if (sender as TButton).Tag=2 then NewCpath.Text:='cache' + PathDelim;
 if (sender as TButton).Tag=3 then NewCpath.Text:='cache_es' + PathDelim;
 if (sender as TButton).Tag=4 then GMTilespath.Text:='cache_gmt' + PathDelim;
 if (sender as TButton).Tag=5 then GECachepath.Text:='cache_ge' + PathDelim;
end;

procedure TfrmSettings.Button5Click(Sender: TObject);
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

procedure TfrmSettings.CBLoginClick(Sender: TObject);
var
  VUseAuth: Boolean;
begin
  VUseAuth := CBLogin.Enabled and CBLogin.Checked;
  EditLogin.Enabled := VUseAuth;
  Label25.Enabled := VUseAuth;
  EditPass.Enabled := VUseAuth;
end;

procedure TfrmSettings.CBoxLocalChange(Sender: TObject);
begin
 GState.LanguageManager.SetCurrentLanguageIndex(CBoxLocal.ItemIndex);
end;

procedure TfrmSettings.CBProxyusedClick(Sender: TObject);
var
  VUseProxy: Boolean;
begin
  VUseProxy := CBProxyused.Enabled and CBProxyused.Checked;
  EditIP.Enabled := VUseProxy;
  CBLogin.Enabled := VUseProxy;
  lblProxyLogin.Enabled := VUseProxy;
  CBLoginClick(CBLogin);
end;

procedure TfrmSettings.chkPosFromGSMClick(Sender: TObject);
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

procedure TfrmSettings.chkUseIEProxyClick(Sender: TObject);
var
  VUseIeProxy: Boolean;
begin
  VUseIeProxy := chkUseIEProxy.Checked;
  CBProxyused.Enabled := not VUseIeProxy;
  lblUseProxy.Enabled := not VUseIeProxy;
  CBProxyusedClick(CBProxyused);
end;

constructor TfrmSettings.Create(AOwner: TComponent);
begin
  inherited;
  frShortCutList := TfrShortCutList.Create(nil);
  PageControl1.ActivePageIndex:=0;
end;

destructor TfrmSettings.Destroy;
begin
  FreeAndNil(frShortCutList);
  inherited;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
var
  VProxyConfig: IProxyConfig;
  VInetConfig: IInetConfig;
  i: Integer;
begin
 InitMapsList;

 CBoxLocal.Clear;
 frShortCutList.Parent := GroupBox5;

  CBoxLocal.Items.Clear;
  for i := 0 to GState.LanguageManager.LanguageList.Count - 1 do begin
    CBoxLocal.Items.Add(GState.LanguageManager.GetLangNameByIndex(i));
  end;
  CBoxLocal.ItemIndex := GState.LanguageManager.GetCurrentLanguageIndex;

 MiniMapAlphaEdit.Value:=GState.MainFormConfig.LayersConfig.MiniMapLayerConfig.MasterAlpha;

  GState.DownloadConfig.LockRead;
  try
    CBLastSuccess.Checked:=GState.DownloadConfig.IsUseSessionLastSuccess;
    CkBGoNextTile.Checked:=GState.DownloadConfig.IsGoNextTileIfDownloadError;
    CBSaveTileNotExists.Checked:=GState.DownloadConfig.IsSaveTileNotExists;
  finally
    GState.DownloadConfig.UnlockRead;
  end;

 ColorBoxBackGround.Selected:=GState.ViewConfig.BackGroundColor;
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
    CBDblDwnl.Checked := (VInetConfig.DownloadTryCount > 1);
  finally
    VInetConfig.UnlockRead;
  end;
  SETilesOCache.Value := GState.MainMemCacheConfig.MaxSize;
  GState.MainFormConfig.LayersConfig.FillingMapLayerConfig.LockRead;
  try
    MapZapColorBox.Selected := WinColor(GState.MainFormConfig.LayersConfig.FillingMapLayerConfig.NoTileColor);
    MapZapAlphaEdit.Value := AlphaComponent(GState.MainFormConfig.LayersConfig.FillingMapLayerConfig.NoTileColor);
  finally
    GState.MainFormConfig.LayersConfig.FillingMapLayerConfig.UnlockRead;
  end;
 CBlock_toolbars.Checked:=GState.MainFormConfig.ToolbarsLock.GetIsLock;
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
    CBShowHintOnMarks.checked := GState.MainFormConfig.MainConfig.ShowHintOnMarks;
  finally
    GState.MainFormConfig.MainConfig.UnlockRead;
  end;

 CBCacheType.ItemIndex:=GState.CacheConfig.DefCache-1;
 OldCPath.text:=GState.CacheConfig.OldCPath;
 NewCPath.text:=GState.CacheConfig.NewCPath;
 ESCPath.text:=GState.CacheConfig.ESCPath;
 GMTilesPath.text:=GState.CacheConfig.GMTilesPath;
 GECachePath.text:=GState.CacheConfig.GECachePath;
  GState.GPSConfig.LockRead;
  try
    SpinEdit2.Value:=GState.GPSConfig.ModuleConfig.ConnectionTimeout;
    CB_GPSlogNmea.Checked:=GState.GPSConfig.ModuleConfig.NMEALog;
    SpinEdit1.Value:=GState.GPSConfig.ModuleConfig.Delay;
    ComboBoxCOM.Text:= 'COM' + IntToStr(GState.GPSConfig.ModuleConfig.Port);
    ComboBoxBoudRate.Text:=inttostr(GState.GPSConfig.ModuleConfig.BaudRate);
    CB_GPSlog.Checked:=GState.GPSConfig.WriteLog;
  finally
    GState.GPSConfig.UnlockRead;
  end;

  GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.LockRead;
  try
    ColorBoxGPSstr.Selected := WinColor(GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.MarkerColor);
    SESizeStr.Value:=GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.MarkerSize;
  finally
    GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.UnlockRead;
  end;

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
  GState.ValueToStringConverterConfig.LockRead;
  try
    ChBoxFirstLat.Checked:=GState.ValueToStringConverterConfig.IsLatitudeFirst;
    CB_llstrType.ItemIndex:=byte(GState.ValueToStringConverterConfig.DegrShowFormat);
    ComboBox1.ItemIndex := byte(GState.ValueToStringConverterConfig.DistStrFormat);
  finally
    GState.ValueToStringConverterConfig.UnlockRead;
  end;
  GState.MainFormConfig.LayersConfig.KmlLayerConfig.LockRead;
  try
    CBWMainColor.Selected:=WinColor(GState.MainFormConfig.LayersConfig.KmlLayerConfig.MainColor);
    CBWFonColor.Selected:=WinColor(GState.MainFormConfig.LayersConfig.KmlLayerConfig.ShadowColor);
  finally
    GState.MainFormConfig.LayersConfig.KmlLayerConfig.UnlockRead;
  end;

  TilesOverScreenEdit.Value := GState.MainFormConfig.DownloadUIConfig.TilesOut;
  CBMinimizeToTray.Checked := GState.GlobalAppConfig.IsShowIconInTray;

 chkPosFromGSMClick(chkPosFromGSM);
 chkUseIEProxyClick(chkUseIEProxy);
 frShortCutList.SetShortCutManager(frmMain.ShortCutManager);
 SatellitePaint;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
var i:integer;
begin
  SatellitePaintBox.Bitmap.SetSizeFrom(SatellitePaintBox);
  ComboBoxCOM.Items.Clear;
  for i:=1 to 64 do begin
    CBGSMComPort.Items.Add('COM'+inttostr(i));
    ComboBoxCOM.Items.Add('COM'+inttostr(i));
  end;
  MapList.DoubleBuffered:=true;
end;

procedure TfrmSettings.TrBarGammaChange(Sender: TObject);
begin
 if TrBarGamma.Position<50 then LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position*2)/100)+')'
                           else LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position-40)/10)+')';
end;

procedure TfrmSettings.tsGPSShow(Sender: TObject);
begin
 pnlGPSLeft.Repaint;
end;

procedure TfrmSettings.TrBarContrastChange(Sender: TObject);
begin
 LabelContrast.Caption:=SAS_STR_Contrast+' ('+inttostr(TrBarcontrast.Position)+')';
end;

procedure ExchangeItems(lv: TListView; const i, j: Integer);
var
  tempLI: TListItem;
begin
  lv.Items.BeginUpdate;
  try
    tempLI := TListItem.Create(lv.Items);
    try
      tempLI.Assign(lv.Items.Item[i]);
      lv.Items.Item[i].Assign(lv.Items.Item[j]);
      lv.Items.Item[j].Assign(tempLI);
      lv.Items.Item[j].Selected:=true;
    finally
      tempLI.Free;
    end;
  finally
    lv.Items.EndUpdate
  end;
end;

procedure TfrmSettings.Button12Click(Sender: TObject);
begin
  If (MapList.Selected<>nil)and(MapList.Selected.Index>0) then begin
    ExchangeItems(MapList, MapList.Selected.Index,MapList.Selected.Index-1);
  end;
end;

procedure TfrmSettings.Button11Click(Sender: TObject);
begin
  If (MapList.Selected<>nil)and(MapList.Selected.Index<MapList.Items.Count-1) then begin
    ExchangeItems(MapList, MapList.Selected.Index,MapList.Selected.Index+1)
  end;
end;

procedure TfrmSettings.Button15Click(Sender: TObject);
var
  VMapType: TMapType;
begin
  VMapType := TMapType(MapList.Selected.Data);
  if frmMapTypeEdit.EditMapModadl(VMapType) then begin
    InitMapsList;
  end;
end;

procedure TfrmSettings.MapListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  VMap: TMapType;
begin
  if Self.Visible then begin
    if Item.Data<>nil then begin
      VMap := TMapType(Item.Data);
      btnMapInfo.Enabled:=VMap.GUIConfig.InfoUrl.Value<>'';
    end;
  end;
end;

procedure TfrmSettings.MapListCustomDrawSubItem(Sender:TCustomListView; Item:TListItem; SubItem:Integer; State:TCustomDrawState; var DefaultDraw:Boolean);
begin
 if item = nil then EXIT;
 if TMapType(Item.Data).GUIConfig.Separator then
  begin
   sender.canvas.Pen.Color:=clGray;
   sender.canvas.MoveTo(2,Item.DisplayRect(drBounds).Bottom-1);
   sender.canvas.LineTo(sender.Column[0].Width,Item.DisplayRect(drBounds).Bottom-1);
  end;
 if Item.Index mod 2 = 1 then sender.canvas.brush.Color:=cl3DLight
                         else sender.canvas.brush.Color:=clwhite;
end;

procedure TfrmSettings.SatellitePaint;
begin
  GState.SkyMapDraw.Draw(SatellitePaintBox.Bitmap, GState.GPSRecorder.CurrentPosition.Satellites);
end;

procedure TfrmSettings.SatellitePaintBoxResize(Sender: TObject);
begin
  SatellitePaintBox.Bitmap.Lock;
  try
    SatellitePaintBox.Bitmap.SetSizeFrom(SatellitePaintBox);
  finally
    SatellitePaintBox.Bitmap.Unlock;
  end;
end;

procedure TfrmSettings.RefreshTranslation;
begin
  inherited;
  FormShow(Self);
  frShortCutList.RefreshTranslation;
end;

procedure TfrmSettings.btnMapInfoClick(Sender: TObject);
var
  VMap: TMapType;
  VUrl: string;
begin
  VMap := TMapType(MapList.Selected.Data);
  VUrl := VMap.GUIConfig.InfoUrl.Value;
  if VUrl <> '' then begin
    VUrl := 'sas://ZmpInfo/' + GUIDToString(VMap.Zmp.GUID) + VUrl;
    frmIntrnalBrowser.Navigate(VMap.Zmp.FileName, VUrl);
  end;
end;

procedure TfrmSettings.InitMapsList;
var
  i: integer;
  VMapType: TMapType;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  MapList.Clear;
  VGUIDList := GState.MapType.GUIConfigList.OrderedMapGUIDList;
  for i := 0 to VGUIDList.Count - 1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := GState.MapType.FullMapsSet.GetMapTypeByGUID(VGUID).MapType;

    MapList.AddItem(VMapType.GUIConfig.Name.Value, nil);
    MapList.Items.Item[i].Data:=VMapType;
    MapList.Items.Item[i].SubItems.Add(VMapType.StorageConfig.NameInCache);
    if VMapType.Abilities.IsLayer then begin
      MapList.Items.Item[i].SubItems.Add(SAS_STR_Layers+'\'+VMapType.GUIConfig.ParentSubMenu.Value);
    end else begin
      MapList.Items.Item[i].SubItems.Add(SAS_STR_Maps+'\'+VMapType.GUIConfig.ParentSubMenu.Value);
    end;
    MapList.Items.Item[i].SubItems.Add(ShortCutToText(VMapType.GUIConfig.HotKey));
    MapList.Items.Item[i].SubItems.Add(VMapType.Zmp.FileName);
    if VMapType.GUIConfig.Enabled then begin
      MapList.Items.Item[i].SubItems.Add(SAS_STR_Yes)
    end else begin
      MapList.Items.Item[i].SubItems.Add(SAS_STR_No)
    end;
  end;
  if MapList.Items.Count > 0 then begin
    MapList.Items.Item[0].Selected := True;
  end;
end;

procedure TfrmSettings.InitResamplersList(AList: IImageResamplerFactoryList; ABox: TComboBox);
var
  i: Integer;
begin
  ABox.Items.Clear;
  for i := 0 to AList.Count - 1 do begin
    ABox.Items.Add(AList.Captions[i]);
  end;
end;

end.


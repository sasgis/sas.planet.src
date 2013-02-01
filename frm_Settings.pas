{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit frm_Settings;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  ExtCtrls,
  ComCtrls,
  Dialogs,
  Spin,
  urlmon,
  wininet,
  GR32,
  u_CommonFormAndFrameParents,
  i_ListenerNotifierLinksList,
  i_ImageResamplerFactory,
  i_MapTypeConfigModalEdit,
  i_LanguageManager,
  u_ShortcutManager,
  fr_MapsList,
  fr_GPSConfig,
  fr_ShortCutList;

type
  TfrmSettings = class(TFormWitghLanguageManager)
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
    OldCPath: TEdit;
    NewCpath: TEdit;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Label1: TLabel;
    ESCPath: TEdit;
    Button8: TButton;
    Button9: TButton;
    Label3: TLabel;
    ComboBox1: TComboBox;
    TrBarGamma: TTrackBar;
    LabelGamma: TLabel;
    TrBarContrast: TTrackBar;
    LabelContrast: TLabel;
    tsGPS: TTabSheet;
    lblResizeMethod: TLabel;
    cbbResizeMethod: TComboBox;
    MiniMapAlphaEdit: TSpinEdit;
    lblMiniMapAlfa: TLabel;
    TilesOverScreenEdit: TSpinEdit;
    Label69: TLabel;
    tsWiki: TTabSheet;
    CBWMainColor: TColorBox;
    lblWikiMainColor: TLabel;
    lblWikiBgColor: TLabel;
    CBWFonColor: TColorBox;
    CB_llstrType: TComboBox;
    Label84: TLabel;
    CBShowmapname: TCheckBox;
    CBinvertcolor: TCheckBox;
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
    SpinEditGenshtabBorderAlpha: TSpinEdit;
    SpinEditDegreeBorderAlpha: TSpinEdit;
    ColorBoxBorder: TColorBox;
    GenshtabBoxBorder: TColorBox;
    DegreeBoxBorder: TColorBox;
    CBDblDwnl: TCheckBox;
    CkBGoNextTile: TCheckBox;
    tsMaps: TTabSheet;
    GroupBox5: TGroupBox;
    CBoxLocal: TComboBox;
    Label8: TLabel;
    ChBoxFirstLat: TCheckBox;
    Label19: TLabel;
    GMTilesPath: TEdit;
    Button13: TButton;
    Button14: TButton;
    CBSaveTileNotExists: TCheckBox;
    CBBorderText: TCheckBox;
    CBGenshtabBorderText: TCheckBox;
    CBDegreeBorderText: TCheckBox;
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
    lblBDBCachePath: TLabel;
    edtBDBCachePath: TEdit;
    btnSetDefBDBCachePath: TButton;
    btnSetBDBCachePath: TButton;
    Label32: TLabel;
    SETimeOut: TSpinEdit;
    tsGSM: TTabSheet;
    Label33: TLabel;
    CBGSMComPort: TComboBox;
    Label34: TLabel;
    CBGSMBaundRate: TComboBox;
    lblBGColor: TLabel;
    ColorBoxBackGround: TColorBox;
    CBLastSuccess: TCheckBox;
    Label36: TLabel;
    SEWaitingAnswer: TSpinEdit;
    pnlBottomButtons: TPanel;
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
    flwpnlGenshtabBorders: TFlowPanel;
    flwpnlDegreeBorders: TFlowPanel;
    pnlTileBorders: TPanel;
    pnlGenshtabBorders: TPanel;
    pnlDegreeBorders: TPanel;
    pnlUIRight: TPanel;
    flwpnlMiniMapAlfa: TFlowPanel;
    flwpnlTileBorder: TFlowPanel;
    pnlOptions: TPanel;
    pnlLang: TPanel;
    pnlFillMap: TPanel;
    flwpnlFillMap: TFlowPanel;
    pnlBgColor: TPanel;
    grdpnlUI: TGridPanel;
    grdpnlWiki: TGridPanel;
    chkPosFromGSM: TCheckBox;
    pnlGSM: TPanel;
    flwpnlGSM: TFlowPanel;
    CBMinimizeToTray: TCheckBox;
    lbGCCachePath: TLabel;
    edtGCCachePath: TEdit;
    btnSetDefGCCachePath: TButton;
    btnSetGCCachePath: TButton;
    pnlImageProcessTop: TPanel;
    btnImageProcessReset: TButton;
    lblImageProcessCaption: TLabel;
    pnlAreaFormat: TPanel;
    lblAreaFormat: TLabel;
    cbbAreaFormat: TComboBox;
    tsGrids: TTabSheet;
    pnlResizeOnload: TPanel;
    lblResizeOnLoad: TLabel;
    cbbResizeOnLoad: TComboBox;
    pnlResizeGetPre: TPanel;
    lblResizeGetPre: TLabel;
    cbbResizeGetPre: TComboBox;
    pnlProjectionChange: TPanel;
    lblProjectionChange: TLabel;
    cbbProjectionChange: TComboBox;
    pnlDownloadResize: TPanel;
    lblDownloadResize: TLabel;
    cbbDownloadResize: TComboBox;
    lblResize: TLabel;
    pnlResizeTileMatrixDraft: TPanel;
    lblResizeTileMatrixDraft: TLabel;
    cbbResizeTileMatrixDraft: TComboBox;
    tsGPSMarker: TTabSheet;
    SESizeStr: TSpinEdit;
    lblGPSMarkerSize: TLabel;
    flwpnlGPSMarker: TFlowPanel;
    ColorBoxGPSstr: TColorBox;
    lblGPSMarkerColor: TLabel;
    lblGPSMarkerRingsCount: TLabel;
    seGPSMarkerRingsCount: TSpinEdit;
    lblGPSMarkerRingRadius: TLabel;
    seGPSMarkerRingRadius: TSpinEdit;
    Label37: TLabel;
    CBCacheType: TComboBox;
    lbDBMSCachePath: TLabel;
    edtDBMSCachePath: TEdit;
    btnSetDefDBMSCachePath: TButton;
    btnSetDBMSCachePath: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrBarGammaChange(Sender: TObject);
    procedure TrBarContrastChange(Sender: TObject);
    procedure chkUseIEProxyClick(Sender: TObject);
    procedure CBProxyusedClick(Sender: TObject);
    procedure CBLoginClick(Sender: TObject);
    procedure chkPosFromGSMClick(Sender: TObject);
    procedure CBoxLocalChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnImageProcessResetClick(Sender: TObject);
  private
    FOnSave: TNotifyEvent;
    FLinksList: IListenerNotifierLinksList;
    frShortCutList: TfrShortCutList;
    frMapsList: TfrMapsList;
    frGPSConfig: TfrGPSConfig;
    FMapTypeEditor: IMapTypeConfigModalEdit;
    procedure InitResamplersList(
      const AList: IImageResamplerFactoryList;
      ABox: TComboBox
    );
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AShortCutManager: TShortcutManager;
      const AMapTypeEditor: IMapTypeConfigModalEdit;
      AOnSave: TNotifyEvent
    ); reintroduce;
    destructor Destroy; override;
    procedure SetProxy;
    procedure ShowGPSSettings;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  c_CacheTypeCodes, // for default path
  t_CommonTypes,
  i_ProxySettings,
  i_InetConfig,
  u_ListenerNotifierLinksList,
  u_GlobalState,
  u_ResStrings;

{$R *.dfm}

constructor TfrmSettings.Create(
  const ALanguageManager: ILanguageManager;
  const AShortCutManager: TShortcutManager;
  const AMapTypeEditor: IMapTypeConfigModalEdit;
  AOnSave: TNotifyEvent
);
begin
  inherited Create(ALanguageManager);
  FMapTypeEditor := AMapTypeEditor;
  FOnSave := AOnSave;
  FLinksList := TListenerNotifierLinksList.Create;
  frShortCutList :=
    TfrShortCutList.Create(
      ALanguageManager,
      AShortCutManager
    );
  frMapsList :=
    TfrMapsList.Create(
      ALanguageManager,
      GState.InternalBrowser,
      GState.MapType.FullMapsSet,
      GState.MapType.GUIConfigList,
      AMapTypeEditor
    );
  frGPSConfig :=
    TfrGPSConfig.Create(
      ALanguageManager,
      GState.GpsSystem,
      GState.SensorList,
      GState.GUISyncronizedTimerNotifier,
      GState.SkyMapDraw,
      GState.MainFormConfig.GPSBehaviour,
      GState.MainFormConfig.LayersConfig.GPSTrackConfig,
      GState.GPSConfig
    );
  PageControl1.ActivePageIndex:=0;
end;

procedure TfrmSettings.btnCancelClick(Sender: TObject);
begin
  frShortCutList.CancelChanges;
  frMapsList.CancelChanges;
  frGPSConfig.CancelChanges;
  Close
end;

procedure TfrmSettings.btnImageProcessResetClick(Sender: TObject);
begin
  TrBarGamma.Position := 50;
  TrBarContrast.Position := 0;
  CBinvertcolor.Checked := False;
end;

procedure TfrmSettings.SetProxy;
var
  PIInfo : PInternetProxyInfo;
  VProxyConfig: IProxyConfig;
  VUseIEProxy: Boolean;
  VUseProxy: Boolean;
  VHost: AnsiString;
begin
  VProxyConfig := GState.Config.InetConfig.ProxyConfig;
  VProxyConfig.LockRead;
  try
    VUseIEProxy := VProxyConfig.GetUseIESettings;
    VUseProxy := VProxyConfig.GetUseProxy;
    VHost := AnsiString(VProxyConfig.GetHost);
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
      PIInfo^.lpszProxy := PAnsiChar(VHost);
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

procedure TfrmSettings.ShowGPSSettings;
begin
  tsGPS.Show;
  ShowModal;
end;

procedure TfrmSettings.btnApplyClick(Sender: TObject);
var
  VProxyConfig: IProxyConfig;
  VInetConfig: IInetConfig;
  VNeedReboot: boolean;
begin
  VNeedReboot:=false;

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
 GState.Config.GsmConfig.LockWrite;
 try
   GState.Config.GsmConfig.SetUseGSMByCOM(chkPosFromGSM.Checked);
   GState.Config.GsmConfig.SetBaudRate(strtoint(CBGSMBaundRate.text));
   GState.Config.GsmConfig.SetPortName(CBGSMComPort.Text);
   GState.Config.GsmConfig.SetWaitTime(SEWaitingAnswer.Value);
 finally
   GState.Config.GsmConfig.UnlockWrite;
 end;
  GState.Config.GlobalAppConfig.IsShowIconInTray := CBMinimizeToTray.Checked;
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

    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.GridColor := SetAlpha(Color32(GenshtabBoxBorder.Selected),SpinEditGenshtabBorderAlpha.Value);
    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.ShowText:=CBGenshtabBorderText.Checked;

    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.GridColor := SetAlpha(Color32(DegreeBoxBorder.Selected),SpinEditDegreeBorderAlpha.Value);
    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.ShowText:=CBDegreeBorderText.Checked;
  finally
    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.UnlockWrite;
  end;

 if CBCacheType.ItemIndex >= 0 then begin
   if CBCacheType.ItemIndex + 3 in [c_File_Cache_Id_RAM] then begin
     // RAM
     GState.CacheConfig.DefCache := CBCacheType.ItemIndex + 3;
   end else if CBCacheType.ItemIndex + 2 in [c_File_Cache_Id_BDB,c_File_Cache_Id_DBMS] then begin
     // BDB or DBMS
     GState.CacheConfig.DefCache := CBCacheType.ItemIndex + 2;
   end else begin
     // other starting items
     GState.CacheConfig.DefCache := CBCacheType.ItemIndex+1;
   end;
 end else begin
   // no selection
   GState.CacheConfig.DefCache := 2;
 end;
 
  GState.Config.ValueToStringConverterConfig.LockWrite;
  try
    GState.Config.ValueToStringConverterConfig.IsLatitudeFirst := ChBoxFirstLat.Checked;
    GState.Config.ValueToStringConverterConfig.DegrShowFormat := TDegrShowFormat(CB_llstrType.ItemIndex);
    GState.Config.ValueToStringConverterConfig.DistStrFormat := TDistStrFormat(ComboBox1.ItemIndex);
    GState.Config.ValueToStringConverterConfig.AreaShowFormat := TAreaStrFormat(cbbAreaFormat.ItemIndex);
  finally
    GState.Config.ValueToStringConverterConfig.UnlockWrite;
  end;

  GState.Config.ImageResamplerConfig.ActiveIndex := cbbResizeMethod.ItemIndex;
  GState.MapType.TileLoadResamplerConfig.ActiveIndex := cbbResizeOnLoad.ItemIndex;
  GState.MapType.TileGetPrevResamplerConfig.ActiveIndex := cbbResizeGetPre.ItemIndex;
  GState.MapType.TileReprojectResamplerConfig.ActiveIndex := cbbProjectionChange.ItemIndex;
  GState.MapType.TileDownloadResamplerConfig.ActiveIndex := cbbDownloadResize.ItemIndex;
  GState.Config.TileMatrixDraftResamplerConfig.ActiveIndex := cbbResizeTileMatrixDraft.ItemIndex;

  GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.LockWrite;
  try
    GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.MarkerColor := SetAlpha(Color32(ColorBoxGPSstr.selected), 150);
    GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.MarkerSize := SESizeStr.Value;
    GState.MainFormConfig.LayersConfig.GPSMarker.MarkerRingsConfig.Count := seGPSMarkerRingsCount.Value;
    GState.MainFormConfig.LayersConfig.GPSMarker.MarkerRingsConfig.StepDistance := seGPSMarkerRingRadius.Value;
  finally
    GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.UnlockWrite;
  end;

  GState.MainFormConfig.ToolbarsLock.SetLock(CBlock_toolbars.Checked);
  VInetConfig :=GState.Config.InetConfig;
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

 GState.CacheConfig.NewCpath.Path:=IncludeTrailingPathDelimiter(NewCpath.Text);
 GState.CacheConfig.OldCPath.Path:=IncludeTrailingPathDelimiter(OldCPath.Text);
 GState.CacheConfig.ESCPath.Path:=IncludeTrailingPathDelimiter(ESCPath.Text);
 GState.CacheConfig.GMTilesPath.Path:=IncludeTrailingPathDelimiter(GMTilesPath.Text);
 GState.CacheConfig.GECachePath.Path:=IncludeTrailingPathDelimiter(GECachePath.Text);
 GState.CacheConfig.BDBCachePath.Path:=IncludeTrailingPathDelimiter(edtBDBCachePath.Text);
 GState.CacheConfig.DBMSCachePath.Path:=edtDBMSCachePath.Text; // do not add delimiter(s)
 GState.CacheConfig.GCCachePath.Path:=IncludeTrailingPathDelimiter(edtGCCachePath.Text);

  GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.LockWrite;
  try
    GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.MainColor :=
      SetAlpha(
        Color32(CBWMainColor.Selected),
        AlphaComponent(GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.MainColor)
      );
    GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.ShadowColor :=
      SetAlpha(
        Color32(CBWFonColor.Selected),
        AlphaComponent(GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.ShadowColor)
      );
    GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.PointColor :=
      SetAlpha(
        Color32(CBWMainColor.Selected),
        AlphaComponent(GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.PointColor)
      );
  finally
    GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.UnlockWrite;
  end;

 GState.Config.LanguageManager.SetCurrentLanguageIndex(CBoxLocal.ItemIndex);

 GState.MainFormConfig.DownloadUIConfig.TilesOut := TilesOverScreenEdit.Value;

 frShortCutList.ApplyChanges;
 frMapsList.ApplyChanges;
 frGPSConfig.ApplyChanges;
  if Assigned(FOnSave) then begin
    FOnSave(nil);
  end;
 if VNeedReboot then begin
   ShowMessage(SAS_MSG_need_reload_application_curln);
 end;
end;

procedure TfrmSettings.Button4Click(Sender: TObject);
begin
 if (Sender as TButton).Tag=1 then OldCPath.Text         := c_File_Cache_Default_GMV + PathDelim;
 if (Sender as TButton).Tag=2 then NewCpath.Text         := c_File_Cache_Default_SAS + PathDelim;
 if (Sender as TButton).Tag=3 then ESCPath.Text          := c_File_Cache_Default_ES + PathDelim;
 if (Sender as TButton).Tag=4 then GMTilesPath.Text      := c_File_Cache_Default_GM + PathDelim;
 if (Sender as TButton).Tag=5 then GECachePath.Text      := c_File_Cache_Default_GE + PathDelim;
 if (Sender as TButton).Tag=6 then edtBDBCachePath.Text  := c_File_Cache_Default_BDB + PathDelim;
 if (Sender as TButton).Tag=7 then edtDBMSCachePath.Text := c_File_Cache_Default_DBMS; // without deliiter(s)
 if (Sender as TButton).Tag=8 then edtGCCachePath.Text   := c_File_Cache_Default_GC + PathDelim;
end;

procedure TfrmSettings.Button5Click(Sender: TObject);
var  TempPath: string;
begin
  if (Sender as TButton).Tag=7 then begin
    // DBMS - select source - not implemented yet
    Exit;
  end;

  if SelectDirectory('', '', TempPath) then
  begin
    if (Sender as TButton).Tag=1 then OldCPath.Text:= IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=2 then NewCpath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=3 then ESCPath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=4 then GMTilesPath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=5 then GECachePath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=6 then edtBDBCachePath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=7 then ;
    if (Sender as TButton).Tag=8 then edtGCCachePath.Text:=IncludeTrailingPathDelimiter(TempPath);
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
 GState.Config.LanguageManager.SetCurrentLanguageIndex(CBoxLocal.ItemIndex);
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

destructor TfrmSettings.Destroy;
begin
  FreeAndNil(frShortCutList);
  FreeAndNil(frMapsList);
  FreeAndNil(frGPSConfig);
  inherited;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
var
  VProxyConfig: IProxyConfig;
  VInetConfig: IInetConfig;
  i: Integer;
begin
  FLinksList.ActivateLinks;

  CBoxLocal.Clear;
  frShortCutList.Parent := GroupBox5;
  frMapsList.Parent := tsMaps;
  frMapsList.Init;
  frGPSConfig.Parent := tsGPS;
  frGPSConfig.Init;

  CBoxLocal.Items.Clear;
  for i := 0 to GState.Config.LanguageManager.LanguageList.Count - 1 do begin
    CBoxLocal.Items.Add(GState.Config.LanguageManager.GetLangNameByIndex(i));
  end;
  CBoxLocal.ItemIndex := GState.Config.LanguageManager.GetCurrentLanguageIndex;

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
  GState.Config.GsmConfig.LockRead;
  try
    chkPosFromGSM.Checked := GState.Config.GsmConfig.GetUseGSMByCOM;
    CBGSMComPort.Text := GState.Config.GsmConfig.GetPortName;
    CBGSMBaundRate.text := inttostr(GState.Config.GsmConfig.GetBaudRate);
    SEWaitingAnswer.Value := GState.Config.GsmConfig.GetWaitTime;
  finally
    GState.Config.GsmConfig.UnlockRead;
  end;
  VInetConfig := GState.Config.InetConfig;
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
    TrBarContrast.Position:=GState.BitmapPostProcessingConfig.ContrastN;
  finally
    GState.BitmapPostProcessingConfig.UnlockRead;
  end;
  if TrBarGamma.Position < 50 then begin
    LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position*2)/100)+')';
  end else begin
    LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position-40)/10)+')';
  end;
  LabelContrast.Caption:=SAS_STR_Contrast+' ('+inttostr(TrBarContrast.Position)+')';

  GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.LockRead;
  try
    ColorBoxBorder.Selected:=WinColor(GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.TileGrid.GridColor);
    SpinEditBorderAlpha.Value:=AlphaComponent(GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.TileGrid.GridColor);
    CBBorderText.Checked:=GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.TileGrid.ShowText;

    GenshtabBoxBorder.Selected:=WinColor(GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.GridColor);
    SpinEditGenshtabBorderAlpha.Value:=AlphaComponent(GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.GridColor);
    CBGenshtabBorderText.Checked:=GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.ShowText;

    DegreeBoxBorder.Selected:=WinColor(GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.GridColor);
    SpinEditDegreeBorderAlpha.Value:=AlphaComponent(GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.GridColor);
    CBDegreeBorderText.Checked:=GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.ShowText;
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

  if GState.CacheConfig.DefCache in [c_File_Cache_Id_RAM] then begin
    // RAM
    CBCacheType.ItemIndex := GState.CacheConfig.DefCache - 3;
  end else if GState.CacheConfig.DefCache in [c_File_Cache_Id_BDB, c_File_Cache_Id_DBMS] then begin
    // BDB or DBMS by default
    CBCacheType.ItemIndex := GState.CacheConfig.DefCache - 2;
  end else if GState.CacheConfig.DefCache > 4 then begin
    // no GE and GC by default
    CBCacheType.ItemIndex := 1;
  end else begin
    // starting items
    CBCacheType.ItemIndex:=GState.CacheConfig.DefCache-1;
  end;

 OldCPath.text:=GState.CacheConfig.OldCPath.Path;
 NewCpath.text:=GState.CacheConfig.NewCpath.Path;
 ESCPath.text:=GState.CacheConfig.ESCPath.Path;
 GMTilesPath.text:=GState.CacheConfig.GMTilesPath.Path;
 GECachePath.text:=GState.CacheConfig.GECachePath.Path;
 edtBDBCachePath.text:=GState.CacheConfig.BDBCachePath.Path;
 edtDBMSCachePath.text:=GState.CacheConfig.DBMSCachePath.Path;
 edtGCCachePath.text:=GState.CacheConfig.GCCachePath.Path;

  GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.LockRead;
  try
    ColorBoxGPSstr.Selected := WinColor(GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.MarkerColor);
    SESizeStr.Value:=GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.MarkerSize;
    seGPSMarkerRingsCount.Value := GState.MainFormConfig.LayersConfig.GPSMarker.MarkerRingsConfig.Count;
    seGPSMarkerRingRadius.Value := Trunc(GState.MainFormConfig.LayersConfig.GPSMarker.MarkerRingsConfig.StepDistance);
  finally
    GState.MainFormConfig.LayersConfig.GPSMarker.MovedMarkerConfig.UnlockRead;
  end;

  InitResamplersList(GState.Config.ImageResamplerConfig.GetList, cbbResizeMethod);
  cbbResizeMethod.ItemIndex := GState.Config.ImageResamplerConfig.ActiveIndex;

  InitResamplersList(GState.MapType.TileLoadResamplerConfig.GetList, cbbResizeOnLoad);
  cbbResizeOnLoad.ItemIndex := GState.MapType.TileLoadResamplerConfig.ActiveIndex;

  InitResamplersList(GState.MapType.TileGetPrevResamplerConfig.GetList, cbbResizeGetPre);
  cbbResizeGetPre.ItemIndex := GState.MapType.TileGetPrevResamplerConfig.ActiveIndex;

  InitResamplersList(GState.MapType.TileReprojectResamplerConfig.GetList, cbbProjectionChange);
  cbbProjectionChange.ItemIndex := GState.MapType.TileReprojectResamplerConfig.ActiveIndex;

  InitResamplersList(GState.MapType.TileDownloadResamplerConfig.GetList, cbbDownloadResize);
  cbbDownloadResize.ItemIndex := GState.MapType.TileDownloadResamplerConfig.ActiveIndex;

  InitResamplersList(GState.Config.TileMatrixDraftResamplerConfig.GetList, cbbResizeTileMatrixDraft);
  cbbResizeTileMatrixDraft.ItemIndex := GState.Config.TileMatrixDraftResamplerConfig.ActiveIndex;

  GState.Config.ValueToStringConverterConfig.LockRead;
  try
    ChBoxFirstLat.Checked:=GState.Config.ValueToStringConverterConfig.IsLatitudeFirst;
    CB_llstrType.ItemIndex:=byte(GState.Config.ValueToStringConverterConfig.DegrShowFormat);
    ComboBox1.ItemIndex := byte(GState.Config.ValueToStringConverterConfig.DistStrFormat);
    cbbAreaFormat.ItemIndex := byte(GState.Config.ValueToStringConverterConfig.AreaShowFormat);
  finally
    GState.Config.ValueToStringConverterConfig.UnlockRead;
  end;
  GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.LockRead;
  try
    CBWMainColor.Selected:=WinColor(GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.MainColor);
    CBWFonColor.Selected:=WinColor(GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.ShadowColor);
  finally
    GState.MainFormConfig.LayersConfig.KmlLayerConfig.DrawConfig.UnlockRead;
  end;

  TilesOverScreenEdit.Value := GState.MainFormConfig.DownloadUIConfig.TilesOut;
  CBMinimizeToTray.Checked := GState.Config.GlobalAppConfig.IsShowIconInTray;

 chkPosFromGSMClick(chkPosFromGSM);
 chkUseIEProxyClick(chkUseIEProxy);
end;

procedure TfrmSettings.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := frGPSConfig.CanClose;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
var i:integer;
begin
  for i:=1 to 64 do begin
    CBGSMComPort.Items.Add('COM'+inttostr(i));
  end;
end;

procedure TfrmSettings.TrBarGammaChange(Sender: TObject);
begin
 if TrBarGamma.Position<50 then LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position*2)/100)+')'
                           else LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position-40)/10)+')';
end;

procedure TfrmSettings.TrBarContrastChange(Sender: TObject);
begin
 LabelContrast.Caption:=SAS_STR_Contrast+' ('+inttostr(TrBarContrast.Position)+')';
end;

procedure TfrmSettings.RefreshTranslation;
begin
  inherited;
  FormShow(Self);
end;

procedure TfrmSettings.InitResamplersList(
  const AList: IImageResamplerFactoryList;
  ABox: TComboBox
);
var
  i: Integer;
begin
  ABox.Items.Clear;
  for i := 0 to AList.Count - 1 do begin
    ABox.Items.Add(AList.Captions[i]);
  end;
end;

end.




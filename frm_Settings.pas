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
  Windows,
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
  fr_GpsSatellites,
  fr_MapsList,
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
    ComboBoxCOM: TComboBox;
    Label4: TLabel;
    Label6: TLabel;
    SpinEdit1: TSpinEdit;
    SE_ConnectionTimeout: TSpinEdit;
    Label16: TLabel;
    ComboBox2: TComboBox;
    MiniMapAlphaEdit: TSpinEdit;
    Label17: TLabel;
    TilesOverScreenEdit: TSpinEdit;
    Label69: TLabel;
    CB_GPSlogPLT: TCheckBox;
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
    Label20: TLabel;
    SESizeTrack: TSpinEdit;
    ComboBoxBoudRate: TComboBox;
    Label65: TLabel;
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
    pnlGPSLeft: TPanel;
    flwpnlGpsPort: TFlowPanel;
    flwpnlGpsParams: TFlowPanel;
    GB_GpsTrackSave: TGroupBox;
    pnlGpsSensors: TPanel;
    pnlGpsRight: TPanel;
    grdpnlWiki: TGridPanel;
    chkPosFromGSM: TCheckBox;
    pnlGSM: TPanel;
    flwpnlGSM: TFlowPanel;
    GroupBox3: TGroupBox;
    CBMinimizeToTray: TCheckBox;
    btnGPSAutodetectCOM: TButton;
    lbGPSDelimiter1: TLabel;
    btnGPSSwitch: TButton;
    CB_GPSAutodetectCOMOnConnect: TCheckBox;
    CB_GPSAutodetectCOMSerial: TCheckBox;
    CB_GPSAutodetectCOMVirtual: TCheckBox;
    CB_GPSAutodetectCOMBluetooth: TCheckBox;
    CB_GPSAutodetectCOMUSBSer: TCheckBox;
    CB_GPSAutodetectCOMOthers: TCheckBox;
    CB_USBGarmin: TCheckBox;
    CB_GPSlogGPX: TCheckBox;
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
    procedure tsGPSShow(Sender: TObject);
    procedure btnGPSAutodetectCOMClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnGPSSwitchClick(Sender: TObject);
    procedure btnImageProcessResetClick(Sender: TObject);
    procedure tsGPSHide(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    FOnSave: TNotifyEvent;
    FLinksList: IListenerNotifierLinksList;
    frShortCutList: TfrShortCutList;
    frMapsList: TfrMapsList;
    frGpsSatellites: TfrGpsSatellites;
    FMapTypeEditor: IMapTypeConfigModalEdit;
    FAutodetecting: Boolean;
    procedure InitResamplersList(
      const AList: IImageResamplerFactoryList;
      ABox: TComboBox
    );
    procedure SaveGPSConfig;
    procedure LoadGPSConfig;
    procedure AutodetectAntiFreeze(Sender: TObject; AThread: TObject);
    function AutodetectCOMFlags: DWORD;
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
  Types,
  t_CommonTypes,
  c_SensorsGUIDSimple,
  i_ProxySettings,
  i_InetConfig,
  i_Sensor,
  i_SensorList,
  vsagps_public_base,
  vsagps_public_tracks,
{$if defined(VSAGPS_AS_DLL)}
  vsagps_public_com_checker,
{$else}
  vsagps_com_checker,
{$ifend}
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
var
  VSensorListEntity: ISensorListEntity;
  VSensor: ISensor;
  VSensorSatellites: ISensorGPSSatellites;
begin
  inherited Create(ALanguageManager);
  FAutodetecting:=FALSE;
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
  VSensorListEntity := GState.SensorList.Get(CSensorGPSSatellitesGUID);
  if VSensorListEntity <> nil then begin
    VSensor := VSensorListEntity.Sensor;
    if Supports(VSensor, ISensorGPSSatellites, VSensorSatellites) then begin
      frGpsSatellites :=
        TfrGpsSatellites.Create(
          ALanguageManager,
          GState.GUISyncronizedTimerNotifier,
          VSensorSatellites,
          GState.SkyMapDraw,
          True
        );
    end;
  end;
  PageControl1.ActivePageIndex:=0;
end;

procedure TfrmSettings.btnCancelClick(Sender: TObject);
begin
  frShortCutList.CancelChanges;
  frMapsList.CancelChanges;
  Close
end;

procedure TfrmSettings.btnGPSAutodetectCOMClick(Sender: TObject);
var
  VObj: TCOMCheckerObject;
  VCancelled: Boolean;
  VFlags: DWORD;
  VPortName: String;
  VPortNumber: SmallInt;
  VPortIndex: Integer;
begin
  if FAutodetecting then
    Exit;
  FAutodetecting:=TRUE;
  VObj:=nil;
  try
    // temp. disable controls
    btnGPSAutodetectCOM.Enabled:=FALSE;
    ComboBoxCOM.Enabled:=FALSE;
    btnGPSSwitch.Enabled:=FALSE;
    // make objects to enum
    VObj:=TCOMCheckerObject.Create;
    // flags (what to enum)
    VFlags:=AutodetectCOMFlags;
    // set timeouts as for real connection
    VObj.SetFullConnectionTimeout(SE_ConnectionTimeout.Value, TRUE);
    // set antifreeze handlers
    VObj.OnThreadFinished:=Self.AutodetectAntiFreeze;
    VObj.OnThreadPending:=Self.AutodetectAntiFreeze;
    // execute
    VPortNumber:=VObj.EnumExecute(nil, VCancelled, VFlags, FALSE);
    if (VPortNumber>=0) then begin
      // port found
      // add new ports to combobox - not implemented yet
      // set first port
      VPortName:='COM'+IntToStr(VPortNumber);
      VPortIndex:=ComboBoxCOM.Items.IndexOf(VPortName);
      if (VPortIndex<>ComboBoxCOM.ItemIndex) then begin
        // select new item
        ComboBoxCOM.ItemIndex:=VPortIndex;
        if Assigned(ComboBoxCOM.OnChange) then
          ComboBoxCOM.OnChange(ComboBoxCOM);
      end;
    end;
  finally
    VObj.Free;
    btnGPSAutodetectCOM.Enabled:=TRUE;
    ComboBoxCOM.Enabled:=TRUE;
    btnGPSSwitch.Enabled:=TRUE;
    FAutodetecting:=FALSE;
  end;
end;

procedure TfrmSettings.btnGPSSwitchClick(Sender: TObject);
begin
  // save config
  SaveGPSConfig;
  // change state
  GState.GPSConfig.GPSEnabled := (not GState.GPSConfig.GPSEnabled);
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

procedure TfrmSettings.AutodetectAntiFreeze(Sender, AThread: TObject);
begin
  Application.ProcessMessages;
end;

function TfrmSettings.AutodetectCOMFlags: DWORD;
var VOptions: TCOMAutodetectOptions;
begin
  VOptions.CheckSerial:=CB_GPSAutodetectCOMSerial.Checked;
  VOptions.CheckVirtual:=CB_GPSAutodetectCOMVirtual.Checked;
  VOptions.CheckBthModem:=CB_GPSAutodetectCOMBluetooth.Checked;
  VOptions.CheckUSBSer:=CB_GPSAutodetectCOMUSBSer.Checked;
  VOptions.CheckOthers:=CB_GPSAutodetectCOMOthers.Checked;
  EncodeCOMDeviceFlags(@VOptions, Result);
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

    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.GridColor := SetAlpha(Color32(GenshtabBoxBorder.Selected),SpinEditGenshtabBorderAlpha.Value);
    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.GenShtabGrid.ShowText:=CBGenshtabBorderText.Checked;

    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.GridColor := SetAlpha(Color32(DegreeBoxBorder.Selected),SpinEditDegreeBorderAlpha.Value);
    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.DegreeGrid.ShowText:=CBDegreeBorderText.Checked;
  finally
    GState.MainFormConfig.LayersConfig.MapLayerGridsConfig.UnlockWrite;
  end;

 if CBCacheType.ItemIndex >= 0 then begin
  if CBCacheType.ItemIndex = 4 then begin
    // BDB
    GState.CacheConfig.DefCache := 6;
  end else begin
    // other starting items
    GState.CacheConfig.DefCache := CBCacheType.ItemIndex+1;
  end;
 end else begin
  // no selection
  GState.CacheConfig.DefCache := 2;
 end;
 
  GState.ValueToStringConverterConfig.LockWrite;
  try
    GState.ValueToStringConverterConfig.IsLatitudeFirst := ChBoxFirstLat.Checked;
    GState.ValueToStringConverterConfig.DegrShowFormat := TDegrShowFormat(CB_llstrType.ItemIndex);
    GState.ValueToStringConverterConfig.DistStrFormat := TDistStrFormat(ComboBox1.ItemIndex);
    GState.ValueToStringConverterConfig.AreaShowFormat := TAreaStrFormat(cbbAreaFormat.ItemIndex);
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

  // save gps config
  SaveGPSConfig;

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

 GState.CacheConfig.NewCpath:=IncludeTrailingPathDelimiter(NewCpath.Text);
 GState.CacheConfig.OldCPath:=IncludeTrailingPathDelimiter(OldCPath.Text);
 GState.CacheConfig.ESCPath:=IncludeTrailingPathDelimiter(ESCPath.Text);
 GState.CacheConfig.GMTilesPath:=IncludeTrailingPathDelimiter(GMTilesPath.Text);
 GState.CacheConfig.GECachePath:=IncludeTrailingPathDelimiter(GECachePath.Text);
 GState.CacheConfig.BDBCachePath:=IncludeTrailingPathDelimiter(edtBDBCachePath.Text);
 GState.CacheConfig.GCCachePath:=IncludeTrailingPathDelimiter(edtGCCachePath.Text);

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

 frShortCutList.ApplyChanges;
 frMapsList.ApplyChanges;
  if Assigned(FOnSave) then begin
    FOnSave(nil);
  end;
 if VNeedReboot then begin
   ShowMessage(SAS_MSG_need_reload_application_curln);
 end;
end;

procedure TfrmSettings.Button4Click(Sender: TObject);
begin
 if (Sender as TButton).Tag=1 then OldCPath.Text:='cache_old' + PathDelim;
 if (Sender as TButton).Tag=2 then NewCpath.Text:='cache' + PathDelim;
 if (Sender as TButton).Tag=3 then ESCPath.Text:='cache_es' + PathDelim;
 if (Sender as TButton).Tag=4 then GMTilesPath.Text:='cache_gmt' + PathDelim;
 if (Sender as TButton).Tag=5 then GECachePath.Text:='cache_ge' + PathDelim;
 if (Sender as TButton).Tag=6 then edtBDBCachePath.Text:='cache_db' + PathDelim;
 if (Sender as TButton).Tag=7 then edtGCCachePath.Text:='cache_gc' + PathDelim;
end;

procedure TfrmSettings.Button5Click(Sender: TObject);
var  TempPath: string;
begin
  if SelectDirectory('', '', TempPath) then
  begin
    if (Sender as TButton).Tag=1 then OldCPath.Text:= IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=2 then NewCpath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=3 then ESCPath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=4 then GMTilesPath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=5 then GECachePath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=6 then edtBDBCachePath.Text:=IncludeTrailingPathDelimiter(TempPath);
    if (Sender as TButton).Tag=7 then edtGCCachePath.Text:=IncludeTrailingPathDelimiter(TempPath);
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

destructor TfrmSettings.Destroy;
begin
  FreeAndNil(frShortCutList);
  FreeAndNil(frMapsList);
  FreeAndNil(frGpsSatellites);
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

 if GState.CacheConfig.DefCache = 6 then begin
   // DBD by default
   CBCacheType.ItemIndex := 4;
 end else if GState.CacheConfig.DefCache > 4 then begin
   // no GE and GC by default
   CBCacheType.ItemIndex := 1;
 end else begin
   // starting items
   CBCacheType.ItemIndex:=GState.CacheConfig.DefCache-1;
 end;

 OldCPath.text:=GState.CacheConfig.OldCPath;
 NewCpath.text:=GState.CacheConfig.NewCpath;
 ESCPath.text:=GState.CacheConfig.ESCPath;
 GMTilesPath.text:=GState.CacheConfig.GMTilesPath;
 GECachePath.text:=GState.CacheConfig.GECachePath;
 edtBDBCachePath.text:=GState.CacheConfig.BDBCachePath;
 edtGCCachePath.text:=GState.CacheConfig.GCCachePath;

  // load gps config
  LoadGPSConfig;

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
    cbbAreaFormat.ItemIndex := byte(GState.ValueToStringConverterConfig.AreaShowFormat);
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
 if tsGPS.Visible then begin
   tsGPSShow(nil);
 end;
end;

procedure TfrmSettings.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=(not FAutodetecting);
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
var i:integer;
begin
  ComboBoxCOM.Items.Clear;
  for i:=1 to 64 do begin
    CBGSMComPort.Items.Add('COM'+inttostr(i));
    ComboBoxCOM.Items.Add('COM'+inttostr(i));
  end;
end;

procedure TfrmSettings.FormHide(Sender: TObject);
begin
  if tsGPS.Visible then begin
    tsGPSHide(nil);
  end;
end;

procedure TfrmSettings.TrBarGammaChange(Sender: TObject);
begin
 if TrBarGamma.Position<50 then LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position*2)/100)+')'
                           else LabelGamma.Caption:=SAS_STR_Gamma+' ('+floattostr((TrBarGamma.Position-40)/10)+')';
end;

procedure TfrmSettings.tsGPSHide(Sender: TObject);
begin
  if frGpsSatellites <> nil then begin
    frGpsSatellites.Parent := nil;
    frGpsSatellites.Hide;
  end;
end;

procedure TfrmSettings.tsGPSShow(Sender: TObject);
begin
  if frGpsSatellites <> nil then begin
    frGpsSatellites.Show;
    frGpsSatellites.Parent := GroupBox3;
  end;
end;

procedure TfrmSettings.TrBarContrastChange(Sender: TObject);
begin
 LabelContrast.Caption:=SAS_STR_Contrast+' ('+inttostr(TrBarContrast.Position)+')';
end;

procedure TfrmSettings.SaveGPSConfig;
begin
  GState.GPSConfig.LockWrite;
  try
    GState.GPSConfig.ModuleConfig.ConnectionTimeout:=SE_ConnectionTimeout.Value;
    GState.GPSConfig.ModuleConfig.NMEALog:=CB_GPSlogNmea.Checked;
    GState.GPSConfig.ModuleConfig.Delay:=SpinEdit1.Value;
    GState.GPSConfig.ModuleConfig.Port := GetCOMPortNumber(ComboBoxCOM.Text);
    GState.GPSConfig.ModuleConfig.BaudRate:=StrToint(ComboBoxBoudRate.Text);
    GState.GPSConfig.WriteLog[ttPLT]:=CB_GPSlogPLT.Checked;
    GState.GPSConfig.WriteLog[ttGPX]:=CB_GPSlogGPX.Checked;
    GState.GPSConfig.ModuleConfig.USBGarmin:=CB_USBGarmin.Checked;
    GState.GPSConfig.ModuleConfig.AutodetectCOMOnConnect:=CB_GPSAutodetectCOMOnConnect.Checked;
    GState.GPSConfig.ModuleConfig.AutodetectCOMFlags:=Self.AutodetectCOMFlags;
  finally
    GState.GPSConfig.UnlockWrite;
  end;
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

procedure TfrmSettings.LoadGPSConfig;
var
  VFlags: DWORD;
  VOptions: TCOMAutodetectOptions;
begin
  GState.GPSConfig.LockRead;
  try
    SE_ConnectionTimeout.Value:=GState.GPSConfig.ModuleConfig.ConnectionTimeout;
    CB_GPSlogNmea.Checked:=GState.GPSConfig.ModuleConfig.NMEALog;
    SpinEdit1.Value:=GState.GPSConfig.ModuleConfig.Delay;
    ComboBoxCOM.Text:= 'COM' + IntToStr(GState.GPSConfig.ModuleConfig.Port);
    ComboBoxBoudRate.Text:=inttostr(GState.GPSConfig.ModuleConfig.BaudRate);
    CB_GPSlogPLT.Checked:=GState.GPSConfig.WriteLog[ttPLT];
    CB_GPSlogGPX.Checked:=GState.GPSConfig.WriteLog[ttGPX];
    CB_USBGarmin.Checked:=GState.GPSConfig.ModuleConfig.USBGarmin;
    CB_GPSAutodetectCOMOnConnect.Checked:=GState.GPSConfig.ModuleConfig.AutodetectCOMOnConnect;
    VFlags:=GState.GPSConfig.ModuleConfig.AutodetectCOMFlags;
  finally
    GState.GPSConfig.UnlockRead;
  end;
  DecodeCOMDeviceFlags(VFlags, @VOptions);
  CB_GPSAutodetectCOMSerial.Checked:=VOptions.CheckSerial;
  CB_GPSAutodetectCOMVirtual.Checked:=VOptions.CheckVirtual;
  CB_GPSAutodetectCOMBluetooth.Checked:=VOptions.CheckBthModem;
  CB_GPSAutodetectCOMUSBSer.Checked:=VOptions.CheckUSBSer;
  CB_GPSAutodetectCOMOthers.Checked:=VOptions.CheckOthers;
end;

end.




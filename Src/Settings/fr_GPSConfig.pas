{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit fr_GPSConfig;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  ComCtrls,
  StdCtrls,
  Spin,
  ExtCtrls,
  i_Listener,
  i_Notifier,
  i_NotifierTime,
  i_LanguageManager,
  i_GpsSystem,
  i_GPSConfig,
  i_MapLayerGPSTrackConfig,
  i_MapLayerGPSMarkerConfig,
  i_MainFormBehaviourByGPSConfig,
  i_SensorList,
  i_SatellitesInViewMapDraw,
  fr_GpsSatellites,
  u_CommonFormAndFrameParents;

type
  TfrGPSConfig = class(TFrame)
    pnlGPSLeft: TPanel;
    pnlGpsConfig: TPanel;
    Label4: TLabel;
    ComboBoxCOM: TComboBox;
    btnGPSAutodetectCOM: TButton;
    Label65: TLabel;
    ComboBoxBoudRate: TComboBox;
    btnGPSSwitch: TButton;
    flwpnlGpsParams: TFlowPanel;
    Label6: TLabel;
    SE_ConnectionTimeout: TSpinEdit;
    Label11: TLabel;
    SpinEdit1: TSpinEdit;
    Label20: TLabel;
    SESizeTrack: TSpinEdit;
    Label5: TLabel;
    SE_NumTrackPoints: TSpinEdit;
    GB_GpsTrackSave: TGroupBox;
    CB_GPSlogPLT: TCheckBox;
    CB_GPSlogNmea: TCheckBox;
    CB_GPSlogGPX: TCheckBox;
    CBSensorsBarAutoShow: TCheckBox;
    pnlGpsRight: TPanel;
    GroupBox3: TGroupBox;
    lblGpsType: TLabel;
    cbbGpsType: TComboBox;
    flwpnlComPort: TFlowPanel;
    flwpnlComPortSpeed: TFlowPanel;
    pnlComParams: TPanel;
    pnlTcpParams: TPanel;
    lblServerAddress: TLabel;
    edServerAddress: TEdit;
    btnServerProbe: TButton;
    lblTcpConnectionState: TLabel;
    grpAutodetectComPort: TGroupBox;
    chkAutodetectAll: TCheckBox;
    chkAutodetectBluetooth: TCheckBox;
    chkAutodetectUsb: TCheckBox;
    chkAutodetectCom: TCheckBox;
    chkAutodetectComVirtual: TCheckBox;
    chkAutodetectOthers: TCheckBox;
    chkUseReplayTrackFile: TCheckBox;
    pgcGps: TPageControl;
    tsCommon: TTabSheet;
    tsTrackAndMarker: TTabSheet;
    pnlConnectionTimeout: TPanel;
    pnlRefreshRate: TPanel;
    grpGpsTrack: TGroupBox;
    pnlTrackPoints: TPanel;
    pnlTrackWidth: TPanel;
    grpGpsMarker: TGroupBox;
    seGPSMarkerRingRadius: TSpinEdit;
    lblGPSMarkerRingRadius: TLabel;
    seGPSMarkerRingsCount: TSpinEdit;
    lblGPSMarkerRingsCount: TLabel;
    ColorBoxGPSstr: TColorBox;
    lblGPSMarkerColor: TLabel;
    SESizeStr: TSpinEdit;
    lblGPSMarkerSize: TLabel;
    pnlMarkerColor: TPanel;
    pnlMarkerPointerSize: TPanel;
    pnlMarker2: TPanel;
    pnlMarker3: TPanel;
    procedure btnGPSAutodetectCOMClick(Sender: TObject);
    procedure btnServerProbeClick(Sender: TObject);
    procedure btnGPSSwitchClick(Sender: TObject);
    procedure OnGpsTypeChange(Sender: TObject);
    procedure OnAutodetectItemClick(Sender: TObject);
  private
    FGpsSystem: IGpsSystem;
    FGPSConfig: IGPSConfig;
    FGPSTrackConfig: IMapLayerGPSTrackConfig;
    FGPSMarkerConfig: IMapLayerGPSMarkerConfig;
    FGPSBehaviour: IMainFormBehaviourByGPSConfig;

    FAutodetecting: Boolean;
    FCancelAutodetect: Boolean;
    FProbing: Boolean;
    FCancelProbe: Boolean;
    FfrGpsSatellites: TfrGpsSatellites;
    FStateListener: IListener;
    function ProbePumpMessages: Boolean;
    procedure UpdateControlsState;
    procedure UpdateConnectionStateLabel(const AState: TGpsConnectionState);
    procedure OnGpsStateChanged;
    procedure UpdateServerAddressControls;
    function GetAutodetectComFlags: DWORD;
    function IsAutodetectComEnabled: Boolean;
    procedure AutodetectAntiFreeze(
      Sender: TObject;
      AThread: TObject
    );
  protected
    procedure RefreshTranslation; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGpsSystem: IGpsSystem;
      const ASensorList: ISensorList;
      const AGUISyncronizedTimerNotifier: INotifierTime;
      const ASkyMapDraw: ISatellitesInViewMapDraw;
      const AGPSBehaviour: IMainFormBehaviourByGPSConfig;
      const AGPSTrackConfig: IMapLayerGPSTrackConfig;
      const AGPSMarkerConfig: IMapLayerGPSMarkerConfig;
      const AGPSConfig: IGPSConfig
    ); reintroduce;
    destructor Destroy; override;
    procedure Init;
    procedure CancelChanges;
    procedure ApplyChanges;
    function CanClose: Boolean;
  end;

implementation

uses
  Math,
  GR32,
  gnugettext,
  vsagps_public_base,
  vsagps_public_tracks,
  vsagps_com_checker,
  vsagps_tcp_checker,
  vsagps_udp_checker,
  c_SensorsGUIDSimple,
  i_Sensor,
  i_GPSModuleByCOMPortSettings,
  u_Dialogs,
  u_ListenerByEvent;

const
  CAutodetectComPortAllTag = 100;

  // the server probe limits
  CServerProbeDataTimeoutSec = 3;
  CServerProbeMaxConnectTimeoutSec = 10;
  CServerProbeSampleMaxLen = 100;

  // how often the gps state change is delivered to the gui
  CStateCheckIntervalMSec = 250;

  // caption of a button that is currently in the "cancel the operation" mode
  CCancelModeCaption = 'X';

  // cbbGpsType.Items order (see the dfm) -> gps module origin
  CGpsTypeOrigins: array [0..5] of TGPSOrigin = (
    gpsoNMEA,
    gpsoGarmin,
    gpsoLocationAPI,
    gpsoFlyOnTrack,
    gpsoTCPNMEA,
    gpsoUDPNMEA
  );

{$R *.dfm}

function GetOriginByIndex(const AIndex: Integer): TGPSOrigin;
begin
  if (AIndex >= Low(CGpsTypeOrigins)) and (AIndex <= High(CGpsTypeOrigins)) then begin
    Result := CGpsTypeOrigins[AIndex];
  end else begin
    Result := gpsoNMEA;
  end;
end;

function GetIndexByOrigin(const AOrigin: TGPSOrigin): Integer;
var
  I: Integer;
begin
  for I := Low(CGpsTypeOrigins) to High(CGpsTypeOrigins) do begin
    if CGpsTypeOrigins[I] = AOrigin then begin
      Result := I;
      Exit;
    end;
  end;
  Result := 0;
end;

procedure SwitchToCancelMode(
  AButton: TButton;
  const ACancelHint: string;
  out AOldCaption: string;
  out AOldHint: string
);
begin
  AOldCaption := AButton.Caption;
  AOldHint := AButton.Hint;
  AButton.Caption := CCancelModeCaption;
  AButton.Hint := ACancelHint;
end;

procedure RestoreFromCancelMode(
  AButton: TButton;
  const AOldCaption: string;
  const AOldHint: string
);
begin
  AButton.Caption := AOldCaption;
  AButton.Hint := AOldHint;
end;

procedure ShowServerAddressHint(const AOrigin: TGPSOrigin);
begin
  if AOrigin = gpsoUDPNMEA then begin
    ShowWarningMessage(_('Enter the address as port or addr:port (e.g. 224.5.1.21:1365)'));
  end else begin
    ShowWarningMessage(_('Enter the address as host:port (e.g. 192.168.1.10:10110)'));
  end;
end;

procedure ShowProbeResult(const AInfo: TTcpProbeInfo);
var
  VMsg: string;
  VSysMsg: string;
begin
  case AInfo.Kind of
    tpr_Gpsd: begin
      ShowInfoMessage(_('Connected: GPSD server detected'));
    end;
    tpr_Nmea: begin
      ShowInfoMessage(_('Connected: NMEA data received'));
    end;
    tpr_NoData: begin
      if AInfo.Sample <> '' then begin
        ShowWarningMessage(Format(
          _('Connected, but no NMEA or GPSD data received in %d sec.'), [CServerProbeDataTimeoutSec]
        ));
      end else begin
        ShowWarningMessage(Format(
          _('Connected, but no data received in %d sec.'), [CServerProbeDataTimeoutSec])
        );
      end;
    end;
    tpr_InvalidParams: begin
      ShowServerAddressHint(gpsoTCPNMEA);
    end;
  else
    begin
      VMsg := Format(_('Connection failed (Winsock error: %d)'), [AInfo.LastError]);
      VSysMsg := Trim(SysErrorMessage(AInfo.LastError));
      if VSysMsg <> '' then begin
        VMsg := VMsg + sLineBreak + sLineBreak + VSysMsg;
      end;
      ShowErrorMessage(VMsg);
    end;
  end;
end;

procedure ShowUdpProbeResult(const AInfo: TUdpProbeInfo);
var
  VMsg: string;
  VSysMsg: string;
begin
  case AInfo.Kind of
    upr_Nmea: begin
      ShowInfoMessage(_('Bound: NMEA data received'));
    end;
    upr_NoData: begin
      if AInfo.Sample <> '' then begin
        ShowWarningMessage(Format(
          _('Bound, but no NMEA data received in %d sec.'), [CServerProbeDataTimeoutSec]
        ));
      end else begin
        ShowWarningMessage(Format(
          _('Bound, but no data received in %d sec.'), [CServerProbeDataTimeoutSec])
        );
      end;
    end;
    upr_InvalidParams: begin
      ShowServerAddressHint(gpsoUDPNMEA);
    end;
  else
    begin
      VMsg := Format(_('Bind failed (Winsock error: %d)'), [AInfo.LastError]);
      VSysMsg := Trim(SysErrorMessage(AInfo.LastError));
      if VSysMsg <> '' then begin
        VMsg := VMsg + sLineBreak + sLineBreak + VSysMsg;
      end;
      ShowErrorMessage(VMsg);
    end;
  end;
end;

function IsActiveConnectionState(const AState: TGpsConnectionState): Boolean; inline;
begin
  Result := AState in [gcsConnecting, gcsConnected, gcsConnectedNoData, gcsDisconnecting];
end;

constructor TfrGPSConfig.Create(
  const ALanguageManager: ILanguageManager;
  const AGpsSystem: IGpsSystem;
  const ASensorList: ISensorList;
  const AGUISyncronizedTimerNotifier: INotifierTime;
  const ASkyMapDraw: ISatellitesInViewMapDraw;
  const AGPSBehaviour: IMainFormBehaviourByGPSConfig;
  const AGPSTrackConfig: IMapLayerGPSTrackConfig;
  const AGPSMarkerConfig: IMapLayerGPSMarkerConfig;
  const AGPSConfig: IGPSConfig
);
var
  VSensorListEntity: ISensorListEntity;
  VSensor: ISensor;
  VSensorSatellites: ISensorGPSSatellites;
begin
  Assert(AGpsSystem <> nil);
  Assert(ASensorList <> nil);
  Assert(AGUISyncronizedTimerNotifier <> nil);
  Assert(ASkyMapDraw <> nil);
  Assert(AGPSBehaviour <> nil);
  Assert(AGPSTrackConfig <> nil);
  Assert(AGPSMarkerConfig <> nil);
  Assert(AGPSConfig <> nil);
  inherited Create(ALanguageManager);
  FGpsSystem := AGpsSystem;
  FGPSConfig := AGPSConfig;
  FGPSTrackConfig := AGPSTrackConfig;
  FGPSMarkerConfig := AGPSMarkerConfig;
  FGPSBehaviour := AGPSBehaviour;

  ComboBoxCOM.Items.Clear;

  FAutodetecting := False;
  FProbing := False;

  FStateListener :=
    TNotifyEventListenerSync.Create(
      AGUISyncronizedTimerNotifier,
      CStateCheckIntervalMSec,
      Self.OnGpsStateChanged
    );

  VSensorListEntity := ASensorList.Get(CSensorGPSSatellitesGUID);
  if VSensorListEntity <> nil then begin
    VSensor := VSensorListEntity.Sensor;
    if Supports(VSensor, ISensorGPSSatellites, VSensorSatellites) then begin
      FfrGpsSatellites :=
        TfrGpsSatellites.Create(
          ALanguageManager,
          AGUISyncronizedTimerNotifier,
          VSensorSatellites,
          ASkyMapDraw,
          True
        );
    end;
  end;

  FGpsSystem.ConnectingNotifier.Add(FStateListener);
  FGpsSystem.ConnectedNotifier.Add(FStateListener);
  FGpsSystem.DisconnectingNotifier.Add(FStateListener);
  FGpsSystem.DisconnectedNotifier.Add(FStateListener);
end;

destructor TfrGPSConfig.Destroy;
begin
  if Assigned(FGpsSystem) and Assigned(FStateListener) then begin
    FGpsSystem.ConnectingNotifier.Remove(FStateListener);
    FGpsSystem.ConnectedNotifier.Remove(FStateListener);
    FGpsSystem.DisconnectingNotifier.Remove(FStateListener);
    FGpsSystem.DisconnectedNotifier.Remove(FStateListener);
  end;
  FGpsSystem := nil;
  FreeAndNil(FfrGpsSatellites);
  inherited;
end;

procedure TfrGPSConfig.CancelChanges;
begin
end;

function TfrGPSConfig.CanClose: Boolean;
begin
  Result := (not FAutodetecting) and (not FProbing);
end;

procedure TfrGPSConfig.ApplyChanges;
var
  VBaudRate: Integer;
begin
  FGPSTrackConfig.LockWrite;
  try
    FGPSTrackConfig.LineWidth := SESizeTrack.Value;
    FGPSTrackConfig.LastPointCount := SE_NumTrackPoints.Value;
  finally
    FGPSTrackConfig.UnlockWrite;
  end;

  FGPSMarkerConfig.MovedMarkerConfig.LockWrite;
  try
    FGPSMarkerConfig.MovedMarkerConfig.MarkerColor := SetAlpha(Color32(ColorBoxGPSstr.selected), 150);
    FGPSMarkerConfig.MovedMarkerConfig.MarkerSize := SESizeStr.Value;
    FGPSMarkerConfig.MarkerRingsConfig.Count := seGPSMarkerRingsCount.Value;
    FGPSMarkerConfig.MarkerRingsConfig.StepDistance := seGPSMarkerRingRadius.Value;
  finally
    FGPSMarkerConfig.MovedMarkerConfig.UnlockWrite;
  end;

  FGPSBehaviour.SensorsAutoShow := CBSensorsBarAutoShow.Checked;

  // the combo box is editable: keep the current value if the text is not a number
  VBaudRate := StrToIntDef(ComboBoxBoudRate.Text, FGPSConfig.ModuleConfig.BaudRate);

  FGPSConfig.LockWrite;
  try
    FGPSConfig.ModuleConfig.GPSOrigin := GetOriginByIndex(cbbGpsType.ItemIndex);
    FGPSConfig.ModuleConfig.ConnectionTimeout := SE_ConnectionTimeout.Value;
    FGPSConfig.ModuleConfig.LowLevelLog := CB_GPSlogNmea.Checked;
    FGPSConfig.ModuleConfig.Delay := SpinEdit1.Value;
    FGPSConfig.ModuleConfig.Port := GetCOMPortNumber(ComboBoxCOM.Text);
    FGPSConfig.ModuleConfig.BaudRate := VBaudRate;
    FGPSConfig.ModuleConfig.ServerAddress := Trim(edServerAddress.Text);
    FGPSConfig.WriteLog[ttPLT] := CB_GPSlogPLT.Checked;
    FGPSConfig.WriteLog[ttGPX] := CB_GPSlogGPX.Checked;
    FGPSConfig.ModuleConfig.AutodetectCOMOnConnect := Self.IsAutodetectComEnabled;
    FGPSConfig.ModuleConfig.AutodetectComFlags := Self.GetAutodetectComFlags;
  finally
    FGPSConfig.UnlockWrite;
  end;
end;

procedure TfrGPSConfig.Init;
var
  VFlags: DWORD;
  VOptions: TCOMAutodetectOptions;
begin
  Assert(cbbGpsType.Items.Count = Length(CGpsTypeOrigins));

  chkAutodetectAll.Tag := CAutodetectComPortAllTag;

  if ComboBoxCOM.Items.Count = 0 then begin
    GetAllComPortsList(ComboBoxCOM.Items, 0);
  end;

  FGPSTrackConfig.LockRead;
  try
    SESizeTrack.Value := Trunc(FGPSTrackConfig.LineWidth);
    SE_NumTrackPoints.Value := FGPSTrackConfig.LastPointCount;
  finally
    FGPSTrackConfig.UnlockRead;
  end;

  FGPSMarkerConfig.MovedMarkerConfig.LockRead;
  try
    ColorBoxGPSstr.Selected := WinColor(FGPSMarkerConfig.MovedMarkerConfig.MarkerColor);
    SESizeStr.Value := FGPSMarkerConfig.MovedMarkerConfig.MarkerSize;
    seGPSMarkerRingsCount.Value := FGPSMarkerConfig.MarkerRingsConfig.Count;
    seGPSMarkerRingRadius.Value := Trunc(FGPSMarkerConfig.MarkerRingsConfig.StepDistance);
  finally
    FGPSMarkerConfig.MovedMarkerConfig.UnlockRead;
  end;

  CBSensorsBarAutoShow.Checked := FGPSBehaviour.SensorsAutoShow;

  if Assigned(FfrGpsSatellites) then begin
    FfrGpsSatellites.Parent := GroupBox3;
  end;

  FGPSConfig.LockRead;
  try
    SE_ConnectionTimeout.Value := FGPSConfig.ModuleConfig.ConnectionTimeout;
    CB_GPSlogNmea.Checked := FGPSConfig.ModuleConfig.LowLevelLog;
    SpinEdit1.Value := FGPSConfig.ModuleConfig.Delay;
    ComboBoxCOM.Text := 'COM' + IntToStr(FGPSConfig.ModuleConfig.Port);
    ComboBoxBoudRate.Text := IntToStr(FGPSConfig.ModuleConfig.BaudRate);
    edServerAddress.Text := FGPSConfig.ModuleConfig.ServerAddress;
    CB_GPSlogPLT.Checked := FGPSConfig.WriteLog[ttPLT];
    CB_GPSlogGPX.Checked := FGPSConfig.WriteLog[ttGPX];
    cbbGpsType.ItemIndex := GetIndexByOrigin(FGPSConfig.ModuleConfig.GPSOrigin);
    VFlags := FGPSConfig.ModuleConfig.AutodetectComFlags;
    chkAutodetectAll.Checked := (VFlags = cCOM_src_All) and FGPSConfig.ModuleConfig.AutodetectCOMOnConnect;
  finally
    FGPSConfig.UnlockRead;
  end;

  DecodeCOMDeviceFlags(VFlags, @VOptions);
  if not chkAutodetectAll.Checked then begin
    chkAutodetectBluetooth.Checked := VOptions.CheckBthModem;
    chkAutodetectUsb.Checked := VOptions.CheckUSBSer;
    chkAutodetectCom.Checked := VOptions.CheckSerial;
    chkAutodetectComVirtual.Checked := VOptions.CheckVirtual;
    chkAutodetectOthers.Checked := VOptions.CheckOthers;
  end;

  OnGpsTypeChange(Self);
end;

procedure TfrGPSConfig.OnAutodetectItemClick(Sender: TObject);
var
  VTag: NativeInt;
begin
  if not (Sender is TCheckBox) then begin
    Exit;
  end;
  VTag := TCheckBox(Sender).Tag;
  if (VTag = CAutodetectComPortAllTag) and chkAutodetectAll.Checked then begin
    chkAutodetectBluetooth.Checked := False;
    chkAutodetectUsb.Checked := False;
    chkAutodetectCom.Checked := False;
    chkAutodetectComVirtual.Checked := False;
    chkAutodetectOthers.Checked := False;
  end else begin
    chkAutodetectAll.Checked := False;
  end;
end;

procedure TfrGPSConfig.UpdateControlsState;
var
  VState: TGpsConnectionState;
  VConnected: Boolean;
  VGpsOn: Boolean;
  VBusy: Boolean;
begin
  if (FGpsSystem = nil) or (FGPSConfig = nil) then begin
    Exit;
  end;

  VState := FGpsSystem.ConnectionState;
  VConnected := IsActiveConnectionState(VState);
  // the gps module is on: a module that is reconnecting has no active
  // connection between the attempts, but it is still on
  VGpsOn := VConnected or FGPSConfig.GPSEnabled;
  VBusy := FProbing or FAutodetecting;

  // the probe opens its own connection to the server, so it is allowed only
  // when the gps module is off
  btnServerProbe.Enabled := FProbing or (not FAutodetecting and not VGpsOn);

  // the server address and the gps type are locked together with the probe
  // button (and while the probe itself is running)
  edServerAddress.Enabled := not (VBusy or VGpsOn);
  cbbGpsType.Enabled := not (VBusy or VGpsOn);

  btnGPSAutodetectCOM.Enabled :=
    FAutodetecting or
    (not FProbing and not VConnected);

  btnGPSSwitch.Enabled := not VBusy;

  CB_GPSlogPLT.Enabled := not VConnected;
  CB_GPSlogNmea.Enabled := not VConnected;
  CB_GPSlogGPX.Enabled := not VConnected;

  UpdateConnectionStateLabel(VState);
end;

procedure TfrGPSConfig.UpdateConnectionStateLabel(const AState: TGpsConnectionState);
const
  cMsgCtxt = 'gps-conn-state';
var
  VText: string;
begin
  if FProbing then begin
    VText := pgettext(cMsgCtxt, 'probing the server...');
  end else begin
    case AState of
      gcsConnecting: begin
        VText := pgettext(cMsgCtxt, 'connecting...');
      end;
      gcsConnected: begin
        VText := pgettext(cMsgCtxt, 'connected');
      end;
      gcsConnectedNoData: begin
        VText := pgettext(cMsgCtxt, 'connected, no data');
      end;
      gcsDisconnecting: begin
        VText := pgettext(cMsgCtxt, 'disconnecting...');
      end;
      gcsWaitingReconnect: begin
        VText := pgettext(cMsgCtxt, 'waiting before auto reconnect');
      end;
    else
      VText := pgettext(cMsgCtxt, 'not connected');
    end;
  end;
  if lblTcpConnectionState.Caption <> VText then begin
    lblTcpConnectionState.Caption := VText;
  end;
end;

procedure TfrGPSConfig.OnGpsStateChanged;
begin
  if FProbing and
     (FGPSConfig.GPSEnabled or (FGpsSystem.ConnectionState <> gcsDisconnected))
  then begin
    FCancelProbe := True;
  end;
  UpdateControlsState;
end;

procedure TfrGPSConfig.AutodetectAntiFreeze(Sender, AThread: TObject);
begin
  Application.ProcessMessages;
  if FCancelAutodetect then begin
    FCancelAutodetect := False;
    (Sender as TCOMCheckerObject).EnumCancel(False);
  end;
end;

function TfrGPSConfig.IsAutodetectComEnabled: Boolean;
begin
  Result := chkAutodetectAll.Checked or (GetAutodetectComFlags <> 0);
end;

function TfrGPSConfig.GetAutodetectComFlags: DWORD;
var
  VOptions: TCOMAutodetectOptions;
begin
  if chkAutodetectAll.Checked then begin
    Result := cCOM_src_All;
  end else begin
    VOptions.CheckBthModem := chkAutodetectBluetooth.Checked;
    VOptions.CheckUSBSer := chkAutodetectUsb.Checked;
    VOptions.CheckSerial := chkAutodetectCom.Checked;
    VOptions.CheckVirtual := chkAutodetectComVirtual.Checked;
    VOptions.CheckOthers := chkAutodetectOthers.Checked;
    EncodeCOMDeviceFlags(@VOptions, Result);
  end;
end;

procedure TfrGPSConfig.btnGPSAutodetectCOMClick(Sender: TObject);
var
  VObj: TCOMCheckerObject;
  VIsCancelled: Boolean;
  VFlags: DWORD;
  VPortName: string;
  VPortNumber: SmallInt;
  VPortIndex: Integer;
  VOldCaption: string;
  VOldHint: string;
begin
  if FAutodetecting then begin
    FCancelAutodetect := True;
    Exit;
  end;
  FAutodetecting := True;
  FCancelAutodetect := False;
  VObj := nil;
  SwitchToCancelMode(btnGPSAutodetectCOM, _('Cancel device port autodetection'), VOldCaption, VOldHint);
  try
    // temp. disable controls
    ComboBoxCOM.Enabled := False;
    UpdateControlsState;
    // make objects to enum
    VObj := TCOMCheckerObject.Create;
    // flags (what to enum)
    VFlags := GetAutodetectComFlags;
    // set timeouts as for real connection
    VObj.SetFullConnectionTimeout(SE_ConnectionTimeout.Value, True);
    // set antifreeze handlers
    VObj.OnThreadFinished := Self.AutodetectAntiFreeze;
    VObj.OnThreadPending := Self.AutodetectAntiFreeze;
    // execute
    VPortNumber := VObj.EnumExecute(nil, VIsCancelled, VFlags, False);
    if VIsCancelled then begin
      Exit;
    end;
    if VPortNumber >= 0 then begin
      // port found
      VPortName := 'COM' + IntToStr(VPortNumber);
      VPortIndex := ComboBoxCOM.Items.IndexOf(VPortName);
      if VPortIndex = -1 then begin
        VPortIndex := ComboBoxCOM.Items.Add(VPortName);
      end;
      if (VPortIndex <> ComboBoxCOM.ItemIndex) then begin
        // select new item
        ComboBoxCOM.ItemIndex := VPortIndex;
        if Assigned(ComboBoxCOM.OnChange) then begin
          ComboBoxCOM.OnChange(ComboBoxCOM);
        end;
      end;
      ShowWarningMessage(Format(_('Device found on the %s port.'), [VPortName]));
    end else begin
      ShowWarningMessage(_('No devices found!'));
    end;
  finally
    VObj.Free;
    RestoreFromCancelMode(btnGPSAutodetectCOM, VOldCaption, VOldHint);
    ComboBoxCOM.Enabled := True;
    FAutodetecting := False;
    UpdateControlsState;
  end;
end;

function TfrGPSConfig.ProbePumpMessages: Boolean;
begin
  Application.ProcessMessages;
  Result := FCancelProbe or Application.Terminated;
end;

procedure TfrGPSConfig.btnServerProbeClick(Sender: TObject);
var
  VProbeInfo: TTcpProbeInfo;
  VUdpProbeInfo: TUdpProbeInfo;
  VOrigin: TGPSOrigin;
  VAddress: string;
  VConnectTimeoutSec: DWORD;
  VOldCaption: string;
  VOldHint: string;
begin
  if FProbing then begin
    FCancelProbe := True;
    Exit;
  end;

  VOrigin := GetOriginByIndex(cbbGpsType.ItemIndex);

  VAddress := Trim(edServerAddress.Text);
  if VAddress = '' then begin
    ShowServerAddressHint(VOrigin);
    Exit;
  end;

  FProbing := True;
  FCancelProbe := False;
  SwitchToCancelMode(btnServerProbe, _('Cancel the server probe'), VOldCaption, VOldHint);
  try
    UpdateControlsState;

    if VOrigin = gpsoUDPNMEA then begin
      // the udp listener: bind the address and wait shortly for any
      // datagram - nothing is sent to the source and there is no
      // connecting phase, so the bind error is reported at once
      UdpProbeExecute(
        VAddress,
        CServerProbeDataTimeoutSec * 1000,
        Self.ProbePumpMessages,
        VUdpProbeInfo
      );

      if VUdpProbeInfo.Cancelled or Application.Terminated then begin
        Exit;
      end;
      ShowUdpProbeResult(VUdpProbeInfo);
    end else begin
      // the connecting phase of the probe is limited: as long as the usual
      // connection timeout allows, but not longer than CServerProbeMaxConnectTimeoutSec
      VConnectTimeoutSec := DWORD(EnsureRange(SE_ConnectionTimeout.Value, 1, CServerProbeMaxConnectTimeoutSec));

      TcpProbeExecute(
        VAddress,
        VConnectTimeoutSec * 1000,
        CServerProbeDataTimeoutSec * 1000,
        Self.ProbePumpMessages,
        VProbeInfo
      );

      if VProbeInfo.Cancelled or Application.Terminated then begin
        Exit;
      end;
      ShowProbeResult(VProbeInfo);
    end;
  finally
    RestoreFromCancelMode(btnServerProbe, VOldCaption, VOldHint);
    FProbing := False;
    UpdateControlsState;
  end;
end;

procedure TfrGPSConfig.btnGPSSwitchClick(Sender: TObject);
var
  VOrigin: TGPSOrigin;
begin
  VOrigin := GetOriginByIndex(cbbGpsType.ItemIndex);
  if not FGPSConfig.GPSEnabled then begin
    // nothing to connect to / bind to
    if (VOrigin in [gpsoTCPNMEA, gpsoUDPNMEA]) and
       (Trim(edServerAddress.Text) = '')
    then begin
      ShowServerAddressHint(VOrigin);
      Exit;
    end;
  end;
  // save config
  ApplyChanges;
  // change state
  FGPSConfig.GPSEnabled := not FGPSConfig.GPSEnabled;
  UpdateControlsState;
end;

procedure TfrGPSConfig.UpdateServerAddressControls;
begin
  // the same panel serves the tcp client and the udp listener: the address
  // field semantics and the probe behavior differ
  if (GetOriginByIndex(cbbGpsType.ItemIndex) = gpsoUDPNMEA) then begin
    edServerAddress.TextHint := '10110 / 224.5.1.21:1365';
    btnServerProbe.Hint := _('Bind the port and check it for NMEA data');
  end else begin
    edServerAddress.TextHint := '192.168.1.10:10110';
    btnServerProbe.Hint := _('Try to connect and check the server for NMEA or GPSD data');
  end;
end;

procedure TfrGPSConfig.OnGpsTypeChange(Sender: TObject);
var
  VOrigin: TGPSOrigin;
begin
  VOrigin := GetOriginByIndex(cbbGpsType.ItemIndex);
  SetControlEnabled(pnlComParams, VOrigin = gpsoNMEA);
  pnlTcpParams.Visible := (VOrigin in [gpsoTCPNMEA, gpsoUDPNMEA]);
  //chkUseReplayTrackFile.Enabled := (VOrigin = gpsoFlyOnTrack); // todo
  if VOrigin <> gpsoFlyOnTrack then begin
    btnGPSSwitch.Caption := _('GPS On/Off');
  end else begin
    btnGPSSwitch.Caption := _('Track Play/Stop');
  end;
  UpdateServerAddressControls;
  UpdateControlsState;
end;

procedure TfrGPSConfig.RefreshTranslation;
var
  I, J: Integer;
begin
  I := ComboBoxBoudRate.ItemIndex;
  J := cbbGpsType.ItemIndex;
  inherited RefreshTranslation;
  ComboBoxBoudRate.ItemIndex := I;
  cbbGpsType.ItemIndex := J;
  if J >= 0 then begin
    OnGpsTypeChange(nil);
  end;
end;

end.

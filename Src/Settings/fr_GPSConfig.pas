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
  ComCtrls,
  StdCtrls,
  Spin,
  ExtCtrls,
  i_Listener,
  i_ListenerTime,
  i_Notifier,
  i_NotifierTime,
  i_LanguageManager,
  i_GpsSystem,
  i_GPSConfig,
  i_GPSModuleByCOMPortSettings,
  i_MapLayerGPSTrackConfig,
  i_MapLayerGPSMarkerConfig,
  i_MainFormBehaviourByGPSConfig,
  i_SensorList,
  i_SatellitesInViewMapDraw,
  fr_GpsSatellites,
  fr_GPSDeviceCom,
  fr_GPSDeviceNetwork,
  fr_GPSDeviceReplay,
  u_GPSDeviceParamsFrame,
  u_CommonFormAndFrameParents;

type
  TfrGPSConfig = class(TFrame)
    pnlGPSLeft: TPanel;
    pnlGpsConfig: TPanel;
    btnGPSSwitch: TButton;
    lblGpsSource: TLabel;
    cbbGpsType: TComboBox;
    lblConnectionState: TLabel;
    pnlDeviceParams: TPanel;
    flwpnlGpsParams: TFlowPanel;
    SE_ConnectionTimeout: TSpinEdit;
    SpinEdit1: TSpinEdit;
    SESizeTrack: TSpinEdit;
    SE_NumTrackPoints: TSpinEdit;
    GB_GpsTrackSave: TGroupBox;
    CB_GPSlogPLT: TCheckBox;
    CB_GPSlogNmea: TCheckBox;
    CB_GPSlogGPX: TCheckBox;
    chkAutoReconnect: TCheckBox;
    CBSensorsBarAutoShow: TCheckBox;
    pnlGpsRight: TPanel;
    GroupBox3: TGroupBox;
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
    Label6: TLabel;
    Label11: TLabel;
    Label20: TLabel;
    Label5: TLabel;
    procedure btnGPSSwitchClick(Sender: TObject);
    procedure OnGpsTypeChange(Sender: TObject);
  private
    FGpsSystem: IGpsSystem;
    FTimerNotifier: INotifierTime;
    FGPSConfig: IGPSConfig;
    FGPSTrackConfig: IMapLayerGPSTrackConfig;
    FGPSMarkerConfig: IMapLayerGPSMarkerConfig;
    FGPSBehaviour: IMainFormBehaviourByGPSConfig;

    FfrDeviceCom: TfrGPSDeviceCom;
    FfrDeviceTcp: TfrGPSDeviceNetwork;
    FfrDeviceUdp: TfrGPSDeviceNetwork;
    FfrDeviceReplay: TfrGPSDeviceReplay;

    FfrGpsSatellites: TfrGpsSatellites;
    FStateListener: IListener;
    FStateTimerListener: IListenerTime;
    function GetConnectionTimeoutSec: DWORD;
    function GetActiveDeviceFrame: TGPSDeviceParamsFrame;
    procedure SetDeviceFrame(const AFrame: TGPSDeviceParamsFrame);
    procedure HideDeviceFrames;
    procedure OnDeviceFrameBusyChanged(Sender: TObject);
    procedure UpdateControlsState;
    procedure UpdateConnectionStateLabel(const AState: TGpsConnectionState);
    procedure OnGpsStateChanged;
    procedure OnGpsStateTimer;
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
  GR32,
  Math,
  gnugettext,
  vsagps_public_tracks,
  c_SensorsGUIDSimple,
  t_GPSModuleByCOMPortSettings,
  i_Sensor,
  u_ListenerByEvent,
  u_ListenerTime;

const
  // how often the gps state change is delivered to the gui
  CStateCheckIntervalMSec = 250;

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
  FTimerNotifier := AGUISyncronizedTimerNotifier;
  FGPSConfig := AGPSConfig;
  FGPSTrackConfig := AGPSTrackConfig;
  FGPSMarkerConfig := AGPSMarkerConfig;
  FGPSBehaviour := AGPSBehaviour;

  FStateListener :=
    TNotifyEventListenerSync.Create(
      AGUISyncronizedTimerNotifier,
      CStateCheckIntervalMSec,
      Self.OnGpsStateChanged
    );

  FfrDeviceCom := TfrGPSDeviceCom.Create(ALanguageManager, AGPSConfig, gpsoNMEA, Self.GetConnectionTimeoutSec);
  FfrDeviceCom.Parent := pnlDeviceParams;
  FfrDeviceCom.OnBusyChanged := Self.OnDeviceFrameBusyChanged;

  FfrDeviceTcp := TfrGPSDeviceNetwork.Create(ALanguageManager, AGPSConfig, gpsoTCPNMEA, Self.GetConnectionTimeoutSec);
  FfrDeviceTcp.Parent := pnlDeviceParams;
  FfrDeviceTcp.OnBusyChanged := Self.OnDeviceFrameBusyChanged;

  FfrDeviceUdp := TfrGPSDeviceNetwork.Create(ALanguageManager, AGPSConfig, gpsoUDPNMEA, Self.GetConnectionTimeoutSec);
  FfrDeviceUdp.Parent := pnlDeviceParams;
  FfrDeviceUdp.OnBusyChanged := Self.OnDeviceFrameBusyChanged;

  FfrDeviceReplay := TfrGPSDeviceReplay.Create(ALanguageManager, AGPSConfig, gpsoFlyOnTrack, Self.GetConnectionTimeoutSec);
  FfrDeviceReplay.Parent := pnlDeviceParams;
  FfrDeviceReplay.OnBusyChanged := Self.OnDeviceFrameBusyChanged;

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

  // refresh the reconnect countdown while the gps waits for the next attempt
  FStateTimerListener := TListenerTimeCheck.Create(Self.OnGpsStateTimer, 1000);
  AGUISyncronizedTimerNotifier.Add(FStateTimerListener);
end;

destructor TfrGPSConfig.Destroy;
begin
  if Assigned(FGpsSystem) and Assigned(FStateListener) then begin
    FGpsSystem.ConnectingNotifier.Remove(FStateListener);
    FGpsSystem.ConnectedNotifier.Remove(FStateListener);
    FGpsSystem.DisconnectingNotifier.Remove(FStateListener);
    FGpsSystem.DisconnectedNotifier.Remove(FStateListener);
  end;
  if Assigned(FTimerNotifier) and Assigned(FStateTimerListener) then begin
    FTimerNotifier.Remove(FStateTimerListener);
  end;
  FStateTimerListener := nil;
  FTimerNotifier := nil;
  FreeAndNil(FfrGpsSatellites);
  FreeAndNil(FfrDeviceCom);
  FreeAndNil(FfrDeviceTcp);
  FreeAndNil(FfrDeviceUdp);
  FreeAndNil(FfrDeviceReplay);
  FGpsSystem := nil;
  inherited;
end;

procedure TfrGPSConfig.CancelChanges;
begin
  // empty
end;

function TfrGPSConfig.CanClose: Boolean;
begin
  Result :=
    (not FfrDeviceCom.IsBusy) and
    (not FfrDeviceTcp.IsBusy) and
    (not FfrDeviceUdp.IsBusy) and
    (not FfrDeviceReplay.IsBusy);
end;

procedure TfrGPSConfig.ApplyChanges;
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

  FGPSConfig.LockWrite;
  try
    FGPSConfig.ModuleConfig.GPSOrigin := GetOriginByIndex(cbbGpsType.ItemIndex);
    FGPSConfig.ModuleConfig.ConnectionTimeout := SE_ConnectionTimeout.Value;
    FGPSConfig.ModuleConfig.LowLevelLog := CB_GPSlogNmea.Checked;
    FGPSConfig.ModuleConfig.Delay := SpinEdit1.Value;
    FGPSConfig.ModuleConfig.AutoReconnect := chkAutoReconnect.Checked;
    FGPSConfig.WriteLog[ttPLT] := CB_GPSlogPLT.Checked;
    FGPSConfig.WriteLog[ttGPX] := CB_GPSlogGPX.Checked;
  finally
    FGPSConfig.UnlockWrite;
  end;

  FfrDeviceCom.ApplyChanges;
  FfrDeviceTcp.ApplyChanges;
  FfrDeviceUdp.ApplyChanges;
  FfrDeviceReplay.ApplyChanges;
end;

procedure TfrGPSConfig.Init;
begin
  Assert(cbbGpsType.Items.Count = Length(CGpsTypeOrigins));

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
    chkAutoReconnect.Checked := FGPSConfig.ModuleConfig.AutoReconnect;
    CB_GPSlogPLT.Checked := FGPSConfig.WriteLog[ttPLT];
    CB_GPSlogGPX.Checked := FGPSConfig.WriteLog[ttGPX];
    cbbGpsType.ItemIndex := GetIndexByOrigin(FGPSConfig.ModuleConfig.GPSOrigin);
  finally
    FGPSConfig.UnlockRead;
  end;

  FfrDeviceCom.Init;
  FfrDeviceTcp.Init;
  FfrDeviceUdp.Init;
  FfrDeviceReplay.Init;

  // show the frame of the selected device
  OnGpsTypeChange(Self);
end;

function TfrGPSConfig.GetConnectionTimeoutSec: DWORD;
begin
  Result := SE_ConnectionTimeout.Value;
end;

function TfrGPSConfig.GetActiveDeviceFrame: TGPSDeviceParamsFrame;
begin
  case GetOriginByIndex(cbbGpsType.ItemIndex) of
    gpsoNMEA: begin
      Result := FfrDeviceCom;
    end;
    gpsoTCPNMEA: begin
      Result := FfrDeviceTcp;
    end;
    gpsoUDPNMEA: begin
      Result := FfrDeviceUdp;
    end;
    gpsoFlyOnTrack: begin
      Result := FfrDeviceReplay;
    end;
  else
    Result := nil;
  end;
end;

procedure TfrGPSConfig.SetDeviceFrame(const AFrame: TGPSDeviceParamsFrame);
begin
  FfrDeviceCom.Visible := (AFrame = FfrDeviceCom);
  FfrDeviceTcp.Visible := (AFrame = FfrDeviceTcp);
  FfrDeviceUdp.Visible := (AFrame = FfrDeviceUdp);
  FfrDeviceReplay.Visible := (AFrame = FfrDeviceReplay);
  pnlDeviceParams.Visible := True;
end;

procedure TfrGPSConfig.HideDeviceFrames;
begin
  FfrDeviceCom.Visible := False;
  FfrDeviceTcp.Visible := False;
  FfrDeviceUdp.Visible := False;
  FfrDeviceReplay.Visible := False;
  pnlDeviceParams.Visible := False;
end;

procedure TfrGPSConfig.OnDeviceFrameBusyChanged(Sender: TObject);
begin
  UpdateControlsState;
end;

procedure TfrGPSConfig.UpdateControlsState;
var
  VState: TGpsConnectionState;
  VConnected: Boolean;
  VGpsOn: Boolean;
  VBusy: Boolean;
  VFrame: TGPSDeviceParamsFrame;
begin
  if (FGpsSystem = nil) or (FGPSConfig = nil) then begin
    Exit;
  end;

  VState := FGpsSystem.ConnectionState;
  VConnected := IsActiveConnectionState(VState);

  // the gps module is on: a module that is reconnecting has no active
  // connection between the attempts, but it is still on
  VGpsOn := VConnected or FGPSConfig.GPSEnabled;

  VBusy :=
    FfrDeviceCom.IsBusy or
    FfrDeviceTcp.IsBusy or
    FfrDeviceUdp.IsBusy or
    FfrDeviceReplay.IsBusy;

  // the gps type and the switch button are locked while a device frame
  // runs a long operation (probe/autodetect) or the gps module is on
  cbbGpsType.Enabled := not (VBusy or VGpsOn);
  btnGPSSwitch.Enabled := not VBusy;

  CB_GPSlogPLT.Enabled := not VConnected;
  CB_GPSlogNmea.Enabled := not VConnected;
  CB_GPSlogGPX.Enabled := not VConnected;

  // the switch button reflects the actual gps state
  if GetOriginByIndex(cbbGpsType.ItemIndex) = gpsoFlyOnTrack then begin
    if VGpsOn then begin
      btnGPSSwitch.Caption := _('Track Stop');
      btnGPSSwitch.Hint := _('Stop the track playback');
    end else begin
      btnGPSSwitch.Caption := _('Track Play');
      btnGPSSwitch.Hint := _('Play the track(s) as the GPS source');
    end;
  end else begin
    if VGpsOn then begin
      btnGPSSwitch.Caption := _('GPS Off');
      btnGPSSwitch.Hint := _('Turn the GPS module off');
    end else begin
      btnGPSSwitch.Caption := _('GPS On');
      btnGPSSwitch.Hint := _('Turn the GPS module on');
    end;
  end;

  UpdateConnectionStateLabel(VState);

  // the per-device controls are updated by their own frames
  VFrame := GetActiveDeviceFrame;
  if VFrame <> nil then begin
    VFrame.UpdateControlsState(VState);
  end;
end;

procedure TfrGPSConfig.UpdateConnectionStateLabel(const AState: TGpsConnectionState);
const
  cMsgCtxt = 'gps-conn-state';
var
  VText: string;
  VDelayLeftMSec: Integer;
begin
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
      VDelayLeftMSec := FGpsSystem.ReconnectDelayLeftMSec;
      VText := Format(pgettext(cMsgCtxt, 'retrying in %d sec.'), [Ceil(VDelayLeftMSec / 1000)]);
    end;
  else
    VText := pgettext(cMsgCtxt, 'not connected');
  end;
  if lblConnectionState.Caption <> VText then begin
    lblConnectionState.Caption := VText;
  end;
end;

procedure TfrGPSConfig.OnGpsStateChanged;
begin
  UpdateControlsState;
end;

procedure TfrGPSConfig.OnGpsStateTimer;
begin
  if FGpsSystem.ConnectionState = gcsWaitingReconnect then begin
    UpdateConnectionStateLabel(gcsWaitingReconnect);
  end;
end;

procedure TfrGPSConfig.btnGPSSwitchClick(Sender: TObject);
var
  VFrame: TGPSDeviceParamsFrame;
begin
  if not FGPSConfig.GPSEnabled then begin
    // the active device frame checks its own values before the connect
    VFrame := GetActiveDeviceFrame;
    if (VFrame <> nil) and (not VFrame.ValidateBeforeConnect) then begin
      Exit;
    end;
  end;
  // save config
  ApplyChanges;
  // change state
  FGPSConfig.GPSEnabled := not FGPSConfig.GPSEnabled;
  UpdateControlsState;
end;

procedure TfrGPSConfig.OnGpsTypeChange(Sender: TObject);
var
  VOrigin: TGPSOrigin;
begin
  VOrigin := GetOriginByIndex(cbbGpsType.ItemIndex);
  case VOrigin of
    gpsoNMEA: begin
      SetDeviceFrame(FfrDeviceCom);
    end;
    gpsoTCPNMEA: begin
      SetDeviceFrame(FfrDeviceTcp);
    end;
    gpsoUDPNMEA: begin
      SetDeviceFrame(FfrDeviceUdp);
    end;
    gpsoFlyOnTrack: begin
      SetDeviceFrame(FfrDeviceReplay);
    end;
  else
    // the garmin and the location api devices have no own settings
    HideDeviceFrames;
  end;
  UpdateControlsState;
end;

procedure TfrGPSConfig.RefreshTranslation;
var
  I: Integer;
begin
  I := cbbGpsType.ItemIndex;
  inherited RefreshTranslation;
  cbbGpsType.ItemIndex := I;
  if I >= 0 then begin
    OnGpsTypeChange(nil);
  end;
end;

end.

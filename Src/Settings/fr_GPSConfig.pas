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
    rgConnectionType: TRadioGroup;
    flwpnlComPort: TFlowPanel;
    flwpnlComPortSpeed: TFlowPanel;
    pnlComParams: TPanel;
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
    procedure btnGPSSwitchClick(Sender: TObject);
    procedure rgConnectionTypeClick(Sender: TObject);
    procedure OnAutodetectItemClick(Sender: TObject);
  private
    FGpsSystem: IGpsSystem;
    FGPSConfig: IGPSConfig;
    FGPSTrackConfig: IMapLayerGPSTrackConfig;
    FGPSMarkerConfig: IMapLayerGPSMarkerConfig;
    FGPSBehaviour: IMainFormBehaviourByGPSConfig;

    FAutodetecting: Boolean;
    FCancelAutodetect: Boolean;
    FfrGpsSatellites: TfrGpsSatellites;
    FConnectListener: IListener;
    FDisconnectListener: IListener;
    procedure OnConnecting;
    procedure OnDisconnect;
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
  GR32,
  gnugettext,
  vsagps_public_base,
  vsagps_public_tracks,
{$if defined(VSAGPS_AS_DLL)}
  vsagps_public_com_checker,
{$else}
  vsagps_com_checker,
{$ifend}
  c_SensorsGUIDSimple,
  i_Sensor,
  i_GPSModuleByCOMPortSettings,
  u_Dialogs,
  u_ListenerByEvent;

const
  CAutodetectComPortAllTag = 100;

{$R *.dfm}

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
  Assert(AGPSConfig <> nil);
  inherited Create(ALanguageManager);
  FGpsSystem := AGpsSystem;
  FGPSConfig := AGPSConfig;
  FGPSTrackConfig := AGPSTrackConfig;
  FGPSMarkerConfig := AGPSMarkerConfig;
  FGPSBehaviour := AGPSBehaviour;

  ComboBoxCOM.Items.Clear;

  FAutodetecting := False;
  FConnectListener := TNotifyEventListenerSync.Create(AGUISyncronizedTimerNotifier, 1000, Self.OnConnecting);
  FDisconnectListener := TNotifyEventListenerSync.Create(AGUISyncronizedTimerNotifier, 1000, Self.OnDisconnect);

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

  FGpsSystem.ConnectingNotifier.Add(FConnectListener);
  FGpsSystem.DisconnectedNotifier.Add(FDisconnectListener);
end;

destructor TfrGPSConfig.Destroy;
begin
  if Assigned(FGpsSystem) and Assigned(FConnectListener) then begin
    FGpsSystem.ConnectingNotifier.Remove(FConnectListener);
  end;
  if Assigned(FGpsSystem) and Assigned(FDisconnectListener) then begin
    FGpsSystem.DisconnectedNotifier.Remove(FDisconnectListener);
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
  Result := not FAutodetecting;
end;

procedure TfrGPSConfig.ApplyChanges;
var
  VGPSType: TGPSOrigin;
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
    case rgConnectionType.ItemIndex of
      1: begin
        VGPSType := gpsoGarmin;
      end;
      2: begin
        VGPSType := gpsoLocationAPI;
      end;
      3: begin
        VGPSType := gpsoFlyOnTrack;
      end;
    else
      VGPSType := gpsoNMEA;
    end;
    FGPSConfig.ModuleConfig.GPSOrigin := VGPSType;
    FGPSConfig.ModuleConfig.ConnectionTimeout := SE_ConnectionTimeout.Value;
    FGPSConfig.ModuleConfig.LowLevelLog := CB_GPSlogNmea.Checked;
    FGPSConfig.ModuleConfig.Delay := SpinEdit1.Value;
    FGPSConfig.ModuleConfig.Port := GetCOMPortNumber(ComboBoxCOM.Text);
    FGPSConfig.ModuleConfig.BaudRate := StrToint(ComboBoxBoudRate.Text);
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

  FGPSMarkerConfig.MovedMarkerConfig.LockWrite;
  try
    FGPSMarkerConfig.MovedMarkerConfig.MarkerColor := SetAlpha(Color32(ColorBoxGPSstr.selected), 150);
    FGPSMarkerConfig.MovedMarkerConfig.MarkerSize := SESizeStr.Value;
    FGPSMarkerConfig.MarkerRingsConfig.Count := seGPSMarkerRingsCount.Value;
    FGPSMarkerConfig.MarkerRingsConfig.StepDistance := seGPSMarkerRingRadius.Value;
  finally
    FGPSMarkerConfig.MovedMarkerConfig.UnlockWrite;
  end;

  FfrGpsSatellites.Parent := GroupBox3;
  FGPSConfig.LockRead;
  try
    SE_ConnectionTimeout.Value := FGPSConfig.ModuleConfig.ConnectionTimeout;
    CB_GPSlogNmea.Checked := FGPSConfig.ModuleConfig.LowLevelLog;
    SpinEdit1.Value := FGPSConfig.ModuleConfig.Delay;
    ComboBoxCOM.Text := 'COM' + IntToStr(FGPSConfig.ModuleConfig.Port);
    ComboBoxBoudRate.Text := IntToStr(FGPSConfig.ModuleConfig.BaudRate);
    CB_GPSlogPLT.Checked := FGPSConfig.WriteLog[ttPLT];
    CB_GPSlogGPX.Checked := FGPSConfig.WriteLog[ttGPX];
    case FGPSConfig.ModuleConfig.GPSOrigin of
      gpsoGarmin: begin
        rgConnectionType.ItemIndex := 1;
      end;
      gpsoLocationAPI: begin
        rgConnectionType.ItemIndex := 2;
      end;
      gpsoFlyOnTrack: begin
        rgConnectionType.ItemIndex := 3;
      end;
    else
      rgConnectionType.ItemIndex := 0;
    end;
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

  rgConnectionTypeClick(Self);
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

procedure TfrGPSConfig.OnConnecting;
begin
  CB_GPSlogPLT.Enabled := False;
  CB_GPSlogNmea.Enabled := False;
  CB_GPSlogGPX.Enabled := False;
  btnGPSAutodetectCOM.Enabled := False;
end;

procedure TfrGPSConfig.OnDisconnect;
begin
  CB_GPSlogPLT.Enabled := True;
  CB_GPSlogNmea.Enabled := True;
  CB_GPSlogGPX.Enabled := True;
  btnGPSAutodetectCOM.Enabled := True;
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
  VPortName: String;
  VPortNumber: SmallInt;
  VPortIndex: Integer;
  VOldHint: string;
begin
  if FAutodetecting then begin
    FCancelAutodetect := True;
    Exit;
  end;
  FAutodetecting := True;
  FCancelAutodetect := False;
  VObj := nil;
  try
    btnGPSAutodetectCOM.Caption := 'X';
    VOldHint := btnGPSAutodetectCOM.Hint;
    btnGPSAutodetectCOM.Hint := _('Cancel device port autodetection');
    // temp. disable controls
    ComboBoxCOM.Enabled := False;
    btnGPSSwitch.Enabled := False;
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
    btnGPSAutodetectCOM.Caption := '?';
    btnGPSAutodetectCOM.Hint := VOldHint;
    ComboBoxCOM.Enabled := True;
    btnGPSSwitch.Enabled := True;
    FAutodetecting := False;
  end;
end;

procedure TfrGPSConfig.btnGPSSwitchClick(Sender: TObject);
begin
  // save config
  ApplyChanges;
  // change state
  FGPSConfig.GPSEnabled := not FGPSConfig.GPSEnabled;
end;

procedure TfrGPSConfig.rgConnectionTypeClick(Sender: TObject);
begin
  SetControlEnabled(pnlComParams, rgConnectionType.ItemIndex = 0);
  //chkUseReplayTrackFile.Enabled := rgConnectionType.ItemIndex = 3; // todo
  if rgConnectionType.ItemIndex <> 3 then begin
    btnGPSSwitch.Caption := _('GPS On/Off');
  end else begin
    btnGPSSwitch.Caption := _('Track Play/Stop');
  end;
end;

procedure TfrGPSConfig.RefreshTranslation;
var
  I: Integer;
begin
  I := ComboBoxBoudRate.ItemIndex;
  inherited RefreshTranslation;
  ComboBoxBoudRate.ItemIndex := I;
end;

end.

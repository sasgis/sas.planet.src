{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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
  StdCtrls,
  Spin,
  ExtCtrls,
  CheckLst,
  i_Listener,
  i_Notifier,
  i_NotifierTime,
  i_LanguageManager,
  i_GpsSystem,
  i_GPSConfig,
  i_MapLayerGPSTrackConfig,
  i_MainFormBehaviourByGPSConfig,
  i_SensorList,
  i_SatellitesInViewMapDraw,
  u_CommonFormAndFrameParents,
  fr_GpsSatellites;

type
  TfrGPSConfig = class(TFrame)
    pnlGPSLeft: TPanel;
    pnlGpsPort: TPanel;
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
    pnlGpsSensors: TPanel;
    CBSensorsBarAutoShow: TCheckBox;
    pnlGpsRight: TPanel;
    GroupBox3: TGroupBox;
    rgConnectionType: TRadioGroup;
    flwpnlComPort: TFlowPanel;
    flwpnlComPortSpeed: TFlowPanel;
    pnlComParams: TPanel;
    chklstAutodetect: TCheckListBox;
    chkEnableAutodetectComPort: TCheckBox;
    procedure btnGPSAutodetectCOMClick(Sender: TObject);
    procedure btnGPSSwitchClick(Sender: TObject);
    procedure rgConnectionTypeClick(Sender: TObject);
    procedure chklstAutodetectClickCheck(Sender: TObject);
    procedure chkEnableAutodetectComPortClick(Sender: TObject);
  private
    FGpsSystem: IGpsSystem;
    FGPSConfig: IGPSConfig;
    FGPSTrackConfig: IMapLayerGPSTrackConfig;
    FGPSBehaviour: IMainFormBehaviourByGPSConfig;

    FAutodetecting: Boolean;
    FfrGpsSatellites: TfrGpsSatellites;
    FConnectListener: IListener;
    FDisconnectListener: IListener;
    procedure OnConnecting;
    procedure OnDisconnect;
    function AutodetectCOMFlags: DWORD;
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
  u_ListenerByEvent;

{$R *.dfm}

constructor TfrGPSConfig.Create(
  const ALanguageManager: ILanguageManager;
  const AGpsSystem: IGpsSystem;
  const ASensorList: ISensorList;
  const AGUISyncronizedTimerNotifier: INotifierTime;
  const ASkyMapDraw: ISatellitesInViewMapDraw;
  const AGPSBehaviour: IMainFormBehaviourByGPSConfig;
  const AGPSTrackConfig: IMapLayerGPSTrackConfig;
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
  FGPSBehaviour := AGPSBehaviour;

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

procedure TfrGPSConfig.chkEnableAutodetectComPortClick(Sender: TObject);
begin
  chklstAutodetect.Enabled := chkEnableAutodetectComPort.Checked;
end;

procedure TfrGPSConfig.chklstAutodetectClickCheck(Sender: TObject);
var
  I: Integer;
begin
  if chklstAutodetect.ItemIndex = 0 then begin
    if chklstAutodetect.Checked[0] then begin
      for I := 1 to chklstAutodetect.Count - 1 do begin
        chklstAutodetect.Checked[I] := False;
      end;
    end;
  end else begin
    chklstAutodetect.Checked[0] := False;
  end;
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
    else begin
      VGPSType := gpsoNMEA;
    end;
    end;
    FGPSConfig.ModuleConfig.GPSOrigin := VGPSType;
    FGPSConfig.ModuleConfig.ConnectionTimeout := SE_ConnectionTimeout.Value;
    FGPSConfig.ModuleConfig.LowLevelLog := CB_GPSlogNmea.Checked;
    FGPSConfig.ModuleConfig.Delay := SpinEdit1.Value;
    FGPSConfig.ModuleConfig.Port := GetCOMPortNumber(ComboBoxCOM.Text);
    FGPSConfig.ModuleConfig.BaudRate := StrToint(ComboBoxBoudRate.Text);
    FGPSConfig.WriteLog[ttPLT] := CB_GPSlogPLT.Checked;
    FGPSConfig.WriteLog[ttGPX] := CB_GPSlogGPX.Checked;
    FGPSConfig.ModuleConfig.AutodetectCOMOnConnect := chkEnableAutodetectComPort.Checked;
    FGPSConfig.ModuleConfig.AutodetectCOMFlags := Self.AutodetectCOMFlags;
  finally
    FGPSConfig.UnlockWrite;
  end;
end;

procedure TfrGPSConfig.Init;
var
  I: Integer;
  VFlags: DWORD;
  VOptions: TCOMAutodetectOptions;
begin
  ComboBoxCOM.Items.Clear;
  for I := 1 to 64 do begin
    ComboBoxCOM.Items.Add('COM' + IntToStr(I));
  end;

  FGPSTrackConfig.LockRead;
  try
    SESizeTrack.Value := Trunc(FGPSTrackConfig.LineWidth);
    SE_NumTrackPoints.Value := FGPSTrackConfig.LastPointCount;
  finally
    FGPSTrackConfig.UnlockRead;
  end;
  CBSensorsBarAutoShow.Checked := FGPSBehaviour.SensorsAutoShow;

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
    chkEnableAutodetectComPort.Checked := FGPSConfig.ModuleConfig.AutodetectCOMOnConnect;
    VFlags := FGPSConfig.ModuleConfig.AutodetectCOMFlags;
  finally
    FGPSConfig.UnlockRead;
  end;
  DecodeCOMDeviceFlags(VFlags, @VOptions);

  chklstAutodetect.Checked[0] := VFlags = cCOM_src_All;
  chklstAutodetect.Checked[1] := VOptions.CheckBthModem;
  chklstAutodetect.Checked[2] := VOptions.CheckUSBSer;
  chklstAutodetect.Checked[3] := VOptions.CheckSerial;
  chklstAutodetect.Checked[4] := VOptions.CheckVirtual;
  chklstAutodetect.Checked[5] := VOptions.CheckOthers;

  rgConnectionTypeClick(Self);
  chklstAutodetectClickCheck(Self);
end;

procedure TfrGPSConfig.OnConnecting;
begin
  CB_GPSlogPLT.Enabled := False;
  CB_GPSlogNmea.Enabled := False;
  CB_GPSlogGPX.Enabled := False;
end;

procedure TfrGPSConfig.OnDisconnect;
begin
  CB_GPSlogPLT.Enabled := True;
  CB_GPSlogNmea.Enabled := True;
  CB_GPSlogGPX.Enabled := True;
end;

procedure TfrGPSConfig.AutodetectAntiFreeze(Sender, AThread: TObject);
begin
  Application.ProcessMessages;
end;

function TfrGPSConfig.AutodetectCOMFlags: DWORD;
var
  VOptions: TCOMAutodetectOptions;
begin
  if chklstAutodetect.Checked[0] then begin
    Result := cCOM_src_All;
  end else begin
    VOptions.CheckBthModem := chklstAutodetect.Checked[1];
    VOptions.CheckUSBSer := chklstAutodetect.Checked[2];
    VOptions.CheckSerial := chklstAutodetect.Checked[3];
    VOptions.CheckVirtual := chklstAutodetect.Checked[4];
    VOptions.CheckOthers := chklstAutodetect.Checked[5];
    EncodeCOMDeviceFlags(@VOptions, Result);
  end;
end;

procedure TfrGPSConfig.btnGPSAutodetectCOMClick(Sender: TObject);
var
  VObj: TCOMCheckerObject;
  VCancelled: Boolean;
  VFlags: DWORD;
  VPortName: String;
  VPortNumber: SmallInt;
  VPortIndex: Integer;
begin
  if FAutodetecting then begin
    Exit;
  end;
  FAutodetecting := True;
  VObj := nil;
  try
    // temp. disable controls
    btnGPSAutodetectCOM.Enabled := False;
    ComboBoxCOM.Enabled := False;
    btnGPSSwitch.Enabled := False;
    // make objects to enum
    VObj := TCOMCheckerObject.Create;
    // flags (what to enum)
    VFlags := AutodetectCOMFlags;
    // set timeouts as for real connection
    VObj.SetFullConnectionTimeout(SE_ConnectionTimeout.Value, True);
    // set antifreeze handlers
    VObj.OnThreadFinished := Self.AutodetectAntiFreeze;
    VObj.OnThreadPending := Self.AutodetectAntiFreeze;
    // execute
    VPortNumber := VObj.EnumExecute(nil, VCancelled, VFlags, False);
    if VPortNumber >= 0 then begin
      // port found
      // add new ports to combobox - not implemented yet
      // set first port
      VPortName := 'COM' + IntToStr(VPortNumber);
      VPortIndex := ComboBoxCOM.Items.IndexOf(VPortName);
      if (VPortIndex <> ComboBoxCOM.ItemIndex) then begin
        // select new item
        ComboBoxCOM.ItemIndex := VPortIndex;
        if Assigned(ComboBoxCOM.OnChange) then begin
          ComboBoxCOM.OnChange(ComboBoxCOM);
        end;
      end;
    end;
  finally
    VObj.Free;
    btnGPSAutodetectCOM.Enabled := True;
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
  chklstAutodetect.Enabled := chkEnableAutodetectComPort.Enabled and chkEnableAutodetectComPort.Checked;
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

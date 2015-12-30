{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
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
  i_Listener,
  i_Notifier,
  i_NotifierTime,
  i_LanguageManager,
  i_GPSModule,
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
    flwpnlGpsPort: TFlowPanel;
    Label4: TLabel;
    ComboBoxCOM: TComboBox;
    btnGPSAutodetectCOM: TButton;
    Label65: TLabel;
    ComboBoxBoudRate: TComboBox;
    lbGPSDelimiter1: TLabel;
    btnGPSSwitch: TButton;
    CB_GPSAutodetectCOMOnConnect: TCheckBox;
    CB_GPSAutodetectCOMSerial: TCheckBox;
    CB_GPSAutodetectCOMVirtual: TCheckBox;
    CB_GPSAutodetectCOMBluetooth: TCheckBox;
    CB_GPSAutodetectCOMUSBSer: TCheckBox;
    CB_GPSAutodetectCOMOthers: TCheckBox;
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
    grpAutoDetect: TGroupBox;
    flwpnlComPort: TFlowPanel;
    flwpnlComPortSpeed: TFlowPanel;
    pnlComParams: TPanel;
    procedure btnGPSAutodetectCOMClick(Sender: TObject);
    procedure btnGPSSwitchClick(Sender: TObject);
  private
    FGpsSystem: IGpsSystem;
    FGPSConfig: IGPSConfig;
    FGPSTrackConfig: IMapLayerGPSTrackConfig;
    FGPSBehaviour: IMainFormBehaviourByGPSConfig;

    FAutodetecting: Boolean;
    frGpsSatellites: TfrGpsSatellites;
    FConnectListener: IListener;
    FDisconnectListener: IListener;
    procedure OnConnecting;
    procedure OnDisconnect;
    function AutodetectCOMFlags: DWORD;
    procedure AutodetectAntiFreeze(
      Sender: TObject;
      AThread: TObject
    );
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

  FAutodetecting := FALSE;
  FConnectListener := TNotifyEventListenerSync.Create(AGUISyncronizedTimerNotifier, 1000, Self.OnConnecting);
  FDisconnectListener := TNotifyEventListenerSync.Create(AGUISyncronizedTimerNotifier, 1000, Self.OnDisconnect);

  VSensorListEntity := ASensorList.Get(CSensorGPSSatellitesGUID);
  if VSensorListEntity <> nil then begin
    VSensor := VSensorListEntity.Sensor;
    if Supports(VSensor, ISensorGPSSatellites, VSensorSatellites) then begin
      frGpsSatellites :=
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
  FreeAndNil(frGpsSatellites);
  inherited;
end;

procedure TfrGPSConfig.CancelChanges;
begin
end;

function TfrGPSConfig.CanClose: Boolean;
begin
  Result := (not FAutodetecting);
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
    FGPSConfig.ModuleConfig.AutodetectCOMOnConnect := CB_GPSAutodetectCOMOnConnect.Checked;
    FGPSConfig.ModuleConfig.AutodetectCOMFlags := Self.AutodetectCOMFlags;
  finally
    FGPSConfig.UnlockWrite;
  end;
end;

procedure TfrGPSConfig.Init;
var
  VFlags: DWORD;
  VOptions: TCOMAutodetectOptions;
  i: Integer;
begin
  ComboBoxCOM.Items.Clear;
  for i := 1 to 64 do begin
    ComboBoxCOM.Items.Add('COM' + inttostr(i));
  end;

  FGPSTrackConfig.LockRead;
  try
    SESizeTrack.Value := Trunc(FGPSTrackConfig.LineWidth);
    SE_NumTrackPoints.Value := FGPSTrackConfig.LastPointCount;
  finally
    FGPSTrackConfig.UnlockRead;
  end;
  CBSensorsBarAutoShow.Checked := FGPSBehaviour.SensorsAutoShow;

  frGpsSatellites.Parent := GroupBox3;
  FGPSConfig.LockRead;
  try
    SE_ConnectionTimeout.Value := FGPSConfig.ModuleConfig.ConnectionTimeout;
    CB_GPSlogNmea.Checked := FGPSConfig.ModuleConfig.LowLevelLog;
    SpinEdit1.Value := FGPSConfig.ModuleConfig.Delay;
    ComboBoxCOM.Text := 'COM' + IntToStr(FGPSConfig.ModuleConfig.Port);
    ComboBoxBoudRate.Text := inttostr(FGPSConfig.ModuleConfig.BaudRate);
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
    else begin
      rgConnectionType.ItemIndex := 0;
    end;
    end;
    CB_GPSAutodetectCOMOnConnect.Checked := FGPSConfig.ModuleConfig.AutodetectCOMOnConnect;
    VFlags := FGPSConfig.ModuleConfig.AutodetectCOMFlags;
  finally
    FGPSConfig.UnlockRead;
  end;
  DecodeCOMDeviceFlags(VFlags, @VOptions);
  CB_GPSAutodetectCOMSerial.Checked := VOptions.CheckSerial;
  CB_GPSAutodetectCOMVirtual.Checked := VOptions.CheckVirtual;
  CB_GPSAutodetectCOMBluetooth.Checked := VOptions.CheckBthModem;
  CB_GPSAutodetectCOMUSBSer.Checked := VOptions.CheckUSBSer;
  CB_GPSAutodetectCOMOthers.Checked := VOptions.CheckOthers;
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
  VOptions.CheckSerial := CB_GPSAutodetectCOMSerial.Checked;
  VOptions.CheckVirtual := CB_GPSAutodetectCOMVirtual.Checked;
  VOptions.CheckBthModem := CB_GPSAutodetectCOMBluetooth.Checked;
  VOptions.CheckUSBSer := CB_GPSAutodetectCOMUSBSer.Checked;
  VOptions.CheckOthers := CB_GPSAutodetectCOMOthers.Checked;
  EncodeCOMDeviceFlags(@VOptions, Result);
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
  FAutodetecting := TRUE;
  VObj := nil;
  try
    // temp. disable controls
    btnGPSAutodetectCOM.Enabled := FALSE;
    ComboBoxCOM.Enabled := FALSE;
    btnGPSSwitch.Enabled := FALSE;
    // make objects to enum
    VObj := TCOMCheckerObject.Create;
    // flags (what to enum)
    VFlags := AutodetectCOMFlags;
    // set timeouts as for real connection
    VObj.SetFullConnectionTimeout(SE_ConnectionTimeout.Value, TRUE);
    // set antifreeze handlers
    VObj.OnThreadFinished := Self.AutodetectAntiFreeze;
    VObj.OnThreadPending := Self.AutodetectAntiFreeze;
    // execute
    VPortNumber := VObj.EnumExecute(nil, VCancelled, VFlags, FALSE);
    if (VPortNumber >= 0) then begin
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
    btnGPSAutodetectCOM.Enabled := TRUE;
    ComboBoxCOM.Enabled := TRUE;
    btnGPSSwitch.Enabled := TRUE;
    FAutodetecting := FALSE;
  end;
end;

procedure TfrGPSConfig.btnGPSSwitchClick(Sender: TObject);
begin
  // save config
  ApplyChanges;
  // change state
  FGPSConfig.GPSEnabled := (not FGPSConfig.GPSEnabled);
end;

end.

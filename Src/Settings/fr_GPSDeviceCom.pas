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

unit fr_GPSDeviceCom;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  gnugettext,
  vsagps_public_base,
  vsagps_com_checker,
  t_GPSModuleByCOMPortSettings,
  i_LanguageManager,
  i_GpsSystem,
  i_GPSConfig,
  i_GPSModuleByCOMPortSettings,
  u_Dialogs,
  u_GPSDeviceParamsFrame;

type
  TfrGPSDeviceCom = class(TFrame)
    flwpnlComPort: TFlowPanel;
    lblPort: TLabel;
    cbbPort: TComboBox;
    btnAutodetectComPort: TButton;
    flwpnlComPortSpeed: TFlowPanel;
    lblSpeed: TLabel;
    cbbBoudRate: TComboBox;
    grpAutodetectComPort: TGroupBox;
    chkAutodetectAll: TCheckBox;
    chkAutodetectBluetooth: TCheckBox;
    chkAutodetectUsb: TCheckBox;
    chkAutodetectCom: TCheckBox;
    chkAutodetectComVirtual: TCheckBox;
    chkAutodetectOthers: TCheckBox;
    procedure btnAutodetectComPortClick(Sender: TObject);
    procedure OnAutodetectItemClick(Sender: TObject);
  private
    FAutodetecting: Boolean;
    FCancelAutodetect: Boolean;
    function GetAutodetectComFlags: DWORD;
    function IsAutodetectComEnabled: Boolean;
    procedure AutodetectAntiFreeze(
      Sender: TObject;
      AThread: TObject
    );
  protected
    procedure RefreshTranslation; override;
  public
    procedure Init; override;
    procedure ApplyChanges; override;
    procedure UpdateControlsState(const AState: TGpsConnectionState); override;
    function IsBusy: Boolean; override;
  end;

implementation

const
  CAutodetectComPortAllTag = 100;

{$R *.dfm}

{ TfrGPSDeviceCom }

procedure TfrGPSDeviceCom.Init;
var
  VSettings: TGPSComSettings;
  VOptions: TCOMAutodetectOptions;
begin
  chkAutodetectAll.Tag := CAutodetectComPortAllTag;

  if cbbPort.Items.Count = 0 then begin
    GetAllComPortsList(cbbPort.Items, 0);
  end;

  FGPSConfig.LockRead;
  try
    VSettings := FGPSConfig.ModuleConfig.ComSettings;
  finally
    FGPSConfig.UnlockRead;
  end;
  cbbPort.Text := 'COM' + IntToStr(VSettings.Port);
  cbbBoudRate.Text := IntToStr(VSettings.BaudRate);
  chkAutodetectAll.Checked :=
    (VSettings.AutodetectCOMFlags = cCOM_src_All) and VSettings.AutodetectCOMOnConnect;

  DecodeCOMDeviceFlags(VSettings.AutodetectCOMFlags, @VOptions);
  if not chkAutodetectAll.Checked then begin
    chkAutodetectBluetooth.Checked := VOptions.CheckBthModem;
    chkAutodetectUsb.Checked := VOptions.CheckUSBSer;
    chkAutodetectCom.Checked := VOptions.CheckSerial;
    chkAutodetectComVirtual.Checked := VOptions.CheckVirtual;
    chkAutodetectOthers.Checked := VOptions.CheckOthers;
  end;
end;

procedure TfrGPSDeviceCom.ApplyChanges;
var
  VSettings: TGPSComSettings;
begin
  VSettings := FGPSConfig.ModuleConfig.ComSettings;
  VSettings.Port := GetCOMPortNumber(cbbPort.Text);
  // the combo box is editable: keep the current value if the text is not a number
  VSettings.BaudRate := StrToIntDef(cbbBoudRate.Text, VSettings.BaudRate);
  VSettings.AutodetectCOMOnConnect := Self.IsAutodetectComEnabled;
  VSettings.AutodetectCOMFlags := Self.GetAutodetectComFlags;

  FGPSConfig.LockWrite;
  try
    FGPSConfig.ModuleConfig.ComSettings := VSettings;
  finally
    FGPSConfig.UnlockWrite;
  end;
end;

procedure TfrGPSDeviceCom.UpdateControlsState(const AState: TGpsConnectionState);
begin
  // the autodetect opens the com ports by itself, so it is allowed
  // only when the gps module is not connected
  btnAutodetectComPort.Enabled := FAutodetecting or (not IsActiveConnectionState(AState));
end;

function TfrGPSDeviceCom.IsBusy: Boolean;
begin
  Result := FAutodetecting;
end;

procedure TfrGPSDeviceCom.OnAutodetectItemClick(Sender: TObject);
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

procedure TfrGPSDeviceCom.AutodetectAntiFreeze(Sender, AThread: TObject);
begin
  Application.ProcessMessages;
  if FCancelAutodetect then begin
    FCancelAutodetect := False;
    (Sender as TCOMCheckerObject).EnumCancel(False);
  end;
end;

function TfrGPSDeviceCom.IsAutodetectComEnabled: Boolean;
begin
  Result := chkAutodetectAll.Checked or (GetAutodetectComFlags <> 0);
end;

function TfrGPSDeviceCom.GetAutodetectComFlags: DWORD;
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

procedure TfrGPSDeviceCom.btnAutodetectComPortClick(Sender: TObject);
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
  SwitchToCancelMode(btnAutodetectComPort, _('Cancel device port autodetection'), VOldCaption, VOldHint);
  if Assigned(FOnBusyChanged) then begin
    FOnBusyChanged(Self);
  end;
  try
    // temp. disable controls
    cbbPort.Enabled := False;
    // make objects to enum
    VObj := TCOMCheckerObject.Create;
    // flags (what to enum)
    VFlags := GetAutodetectComFlags;
    // set timeouts as for real connection
    VObj.SetFullConnectionTimeout(FGetConnectionTimeoutSec(), True);
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
      VPortIndex := cbbPort.Items.IndexOf(VPortName);
      if VPortIndex = -1 then begin
        VPortIndex := cbbPort.Items.Add(VPortName);
      end;
      if (VPortIndex <> cbbPort.ItemIndex) then begin
        // select new item
        cbbPort.ItemIndex := VPortIndex;
        if Assigned(cbbPort.OnChange) then begin
          cbbPort.OnChange(cbbPort);
        end;
      end;
      ShowWarningMessage(Format(_('Device found on the %s port.'), [VPortName]));
    end else begin
      ShowWarningMessage(_('No devices found!'));
    end;
  finally
    VObj.Free;
    RestoreFromCancelMode(btnAutodetectComPort, VOldCaption, VOldHint);
    cbbPort.Enabled := True;
    FAutodetecting := False;
    if Assigned(FOnBusyChanged) then begin
      FOnBusyChanged(Self);
    end;
  end;
end;

procedure TfrGPSDeviceCom.RefreshTranslation;
var
  I: Integer;
begin
  I := cbbBoudRate.ItemIndex;
  inherited RefreshTranslation;
  cbbBoudRate.ItemIndex := I;
end;

end.

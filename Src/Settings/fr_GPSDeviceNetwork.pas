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

unit fr_GPSDeviceNetwork;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Math,
  gnugettext,
  vsagps_tcp_checker,
  vsagps_udp_checker,
  t_GPSModuleByCOMPortSettings,
  i_LanguageManager,
  i_GpsSystem,
  i_GPSConfig,
  i_GPSModuleByCOMPortSettings,
  u_Dialogs,
  u_GPSDeviceParamsFrame;

type
  TfrGPSDeviceNetwork = class(TFrame)
    lblServerAddress: TLabel;
    edtServerAddress: TEdit;
    btnServerProbe: TButton;
    procedure btnServerProbeClick(Sender: TObject);
  private
    FProbing: Boolean;
    FCancelProbe: Boolean;
    function ReadSettings: TGPSNetworkSettings;
    function ProbePumpMessages: Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSConfig: IGPSConfig;
      const AOrigin: TGPSOrigin;
      const AGetConnectionTimeoutSec: TGetConnectionTimeoutSec
    ); reintroduce;
    procedure Init; override;
    procedure ApplyChanges; override;
    procedure UpdateControlsState(const AState: TGpsConnectionState); override;
    function IsBusy: Boolean; override;
    function ValidateBeforeConnect: Boolean; override;
  end;

implementation

const
  CServerProbeDataTimeoutSec = 3;
  CServerProbeMaxConnectTimeoutSec = 10;

{$R *.dfm}

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

{ TfrGPSDeviceNetwork }

constructor TfrGPSDeviceNetwork.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSConfig: IGPSConfig;
  const AOrigin: TGPSOrigin;
  const AGetConnectionTimeoutSec: TGetConnectionTimeoutSec
);
begin
  Assert(AOrigin in [gpsoTCPNMEA, gpsoUDPNMEA]);
  inherited Create(ALanguageManager, AGPSConfig, AOrigin, AGetConnectionTimeoutSec);
  if FOrigin = gpsoUDPNMEA then begin
    edtServerAddress.TextHint := '10110 / 224.5.1.21:1365';
    btnServerProbe.Hint := _('Bind the port and check it for NMEA data');
  end else begin
    edtServerAddress.TextHint := '192.168.1.10:10110';
    btnServerProbe.Hint := _('Try to connect and check the server for NMEA or GPSD data');
  end;
end;

procedure TfrGPSDeviceNetwork.Init;
var
  VSettings: TGPSNetworkSettings;
begin
  VSettings := ReadSettings;
  edtServerAddress.Text := VSettings.ServerAddress;
end;

procedure TfrGPSDeviceNetwork.ApplyChanges;
var
  VSettings: TGPSNetworkSettings;
begin
  VSettings.ServerAddress := Trim(edtServerAddress.Text);

  FGPSConfig.LockWrite;
  try
    if FOrigin = gpsoTCPNMEA then begin
      FGPSConfig.ModuleConfig.TcpSettings := VSettings;
    end else begin
      FGPSConfig.ModuleConfig.UdpSettings := VSettings;
    end;
  finally
    FGPSConfig.UnlockWrite;
  end;
end;

function TfrGPSDeviceNetwork.ReadSettings: TGPSNetworkSettings;
begin
  FGPSConfig.LockRead;
  try
    if FOrigin = gpsoTCPNMEA then begin
      Result := FGPSConfig.ModuleConfig.TcpSettings;
    end else begin
      Result := FGPSConfig.ModuleConfig.UdpSettings;
    end;
  finally
    FGPSConfig.UnlockRead;
  end;
end;

procedure TfrGPSDeviceNetwork.UpdateControlsState(const AState: TGpsConnectionState);
var
  VGpsOn: Boolean;
begin
  VGpsOn := GetIsGpsOn(AState);

  // the gps module was enabled (e.g. from the main form toolbar) while
  // the probe is running - the probe is not allowed in parallel
  if FProbing and VGpsOn then begin
    FCancelProbe := True;
  end;

  // the probe opens its own connection to the server, so it is allowed
  // only when the gps module is off
  btnServerProbe.Enabled := FProbing or (not VGpsOn);

  // the address is locked together with the probe button
  // (and while the probe itself is running)
  edtServerAddress.Enabled := not (FProbing or VGpsOn);
end;

function TfrGPSDeviceNetwork.IsBusy: Boolean;
begin
  Result := FProbing;
end;

function TfrGPSDeviceNetwork.ValidateBeforeConnect: Boolean;
begin
  Result := Trim(edtServerAddress.Text) <> '';
  if not Result then begin
    ShowServerAddressHint(FOrigin);
  end;
end;

function TfrGPSDeviceNetwork.ProbePumpMessages: Boolean;
begin
  Application.ProcessMessages;
  Result := FCancelProbe or Application.Terminated;
end;

procedure TfrGPSDeviceNetwork.btnServerProbeClick(Sender: TObject);
var
  VProbeInfo: TTcpProbeInfo;
  VUdpProbeInfo: TUdpProbeInfo;
  VAddress: string;
  VConnectTimeoutSec: DWORD;
  VOldCaption: string;
  VOldHint: string;
begin
  if FProbing then begin
    FCancelProbe := True;
    Exit;
  end;

  VAddress := Trim(edtServerAddress.Text);
  if VAddress = '' then begin
    ShowServerAddressHint(FOrigin);
    Exit;
  end;

  FProbing := True;
  FCancelProbe := False;
  SwitchToCancelMode(btnServerProbe, _('Cancel the server probe'), VOldCaption, VOldHint);
  if Assigned(FOnBusyChanged) then begin
    FOnBusyChanged(Self);
  end;
  try
    if FOrigin = gpsoUDPNMEA then begin
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
      VConnectTimeoutSec := DWORD(EnsureRange(FGetConnectionTimeoutSec(), 1, CServerProbeMaxConnectTimeoutSec));

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
    if Assigned(FOnBusyChanged) then begin
      FOnBusyChanged(Self);
    end;
  end;
end;

end.

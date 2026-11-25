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

unit u_GPSModuleByCOMPortSettings;

interface

uses
  Windows,
  t_GPSModuleByCOMPortSettings,
  i_GPSModuleByCOMPortSettings,
  u_BaseInterfacedObject;

type
  TGPSModuleByCOMPortSettings = class(TBaseInterfacedObject, IGPSModuleByCOMPortSettings)
  private
    FGPSOrigin: TGPSOrigin;
    FConnectionTimeout: DWORD;
    FDelay: DWORD;
    FAutoReconnect: Boolean;
    FLowLevelLog: Boolean;
    FLogPath: string;
    FComSettings: TGPSComSettings;
    FTcpSettings: TGPSNetworkSettings;
    FUdpSettings: TGPSNetworkSettings;
    FReplaySettings: TGPSReplaySettings;
  private
    { IGPSModuleByCOMPortSettings }
    function GetGPSOrigin: TGPSOrigin;
    function GetConnectionTimeout: DWORD;
    function GetDelay: DWORD;
    function GetAutoReconnect: Boolean;
    function GetConnectErrorDialogSuppressed: Boolean;
    function GetLowLevelLog: Boolean;
    function GetLogPath: string;
    function GetComSettings: TGPSComSettings;
    function GetTcpSettings: TGPSNetworkSettings;
    function GetUdpSettings: TGPSNetworkSettings;
    function GetReplaySettings: TGPSReplaySettings;
  public
    constructor Create(
      const AGPSOrigin: TGPSOrigin;
      const AConnectionTimeout: DWORD;
      const ADelay: DWORD;
      const AAutoReconnect: Boolean;
      const ALowLevelLog: Boolean;
      const ALogPath: string;
      const AComSettings: TGPSComSettings;
      const ATcpSettings: TGPSNetworkSettings;
      const AUdpSettings: TGPSNetworkSettings;
      const AReplaySettings: TGPSReplaySettings
    );
  end;

implementation

{ TGPSModuleByCOMPortSettings }

constructor TGPSModuleByCOMPortSettings.Create(
  const AGPSOrigin: TGPSOrigin;
  const AConnectionTimeout: DWORD;
  const ADelay: DWORD;
  const AAutoReconnect: Boolean;
  const ALowLevelLog: Boolean;
  const ALogPath: string;
  const AComSettings: TGPSComSettings;
  const ATcpSettings: TGPSNetworkSettings;
  const AUdpSettings: TGPSNetworkSettings;
  const AReplaySettings: TGPSReplaySettings
);
begin
  inherited Create;
  FGPSOrigin := AGPSOrigin;
  FConnectionTimeout := AConnectionTimeout;
  FDelay := ADelay;
  FAutoReconnect := AAutoReconnect;
  FLowLevelLog := ALowLevelLog;
  FLogPath := ALogPath;
  FComSettings := AComSettings;
  FTcpSettings := ATcpSettings;
  FUdpSettings := AUdpSettings;
  FReplaySettings := AReplaySettings;
end;

function TGPSModuleByCOMPortSettings.GetComSettings: TGPSComSettings;
begin
  Result := FComSettings;
end;

function TGPSModuleByCOMPortSettings.GetConnectionTimeout: DWORD;
begin
  Result := FConnectionTimeout;
end;

function TGPSModuleByCOMPortSettings.GetDelay: DWORD;
begin
  Result := FDelay;
end;

function TGPSModuleByCOMPortSettings.GetLogPath: string;
begin
  Result := FLogPath;
end;

function TGPSModuleByCOMPortSettings.GetLowLevelLog: Boolean;
begin
  Result := FLowLevelLog;
end;

function TGPSModuleByCOMPortSettings.GetTcpSettings: TGPSNetworkSettings;
begin
  Result := FTcpSettings;
end;

function TGPSModuleByCOMPortSettings.GetUdpSettings: TGPSNetworkSettings;
begin
  Result := FUdpSettings;
end;

function TGPSModuleByCOMPortSettings.GetReplaySettings: TGPSReplaySettings;
begin
  Result := FReplaySettings;
end;

function TGPSModuleByCOMPortSettings.GetAutoReconnect: Boolean;
begin
  Result := FAutoReconnect;
end;

function TGPSModuleByCOMPortSettings.GetConnectErrorDialogSuppressed: Boolean;
begin
  Result := FAutoReconnect and (FGPSOrigin <> gpsoFlyOnTrack);
end;

function TGPSModuleByCOMPortSettings.GetGPSOrigin: TGPSOrigin;
begin
  Result := FGPSOrigin;
end;

end.

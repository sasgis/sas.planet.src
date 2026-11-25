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

unit u_GPSModuleByCOMPortConfig;

interface

uses
  Windows,
  t_GPSModuleByCOMPortSettings,
  i_PathConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GPSModuleByCOMPortSettings,
  i_GPSModuleByCOMPortConfig,
  u_ConfigDataElementBase;

type
  TGPSModuleByCOMPortConfig = class(TConfigDataElementWithStaticBase, IGPSModuleByCOMPortConfig)
  private
    FGPSOrigin: TGPSOrigin;
    FConnectionTimeout: DWORD;
    FDelay: DWORD;
    FLowLevelLog: Boolean;
    FLogPath: IPathConfig;
    FComSettings: TGPSComSettings;
    FTcpSettings: TGPSNetworkSettings;
    FUdpSettings: TGPSNetworkSettings;
    FReplaySettings: TGPSReplaySettings;
    FAutoReconnect: Boolean;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IGPSModuleByCOMPortConfig }
    function GetGPSOrigin: TGPSOrigin;
    procedure SetGPSOrigin(const AValue: TGPSOrigin);

    function GetConnectionTimeout: DWORD;
    procedure SetConnectionTimeout(const AValue: DWORD);

    function GetDelay: DWORD;
    procedure SetDelay(const AValue: DWORD);

    function GetAutoReconnect: Boolean;
    procedure SetAutoReconnect(const AValue: Boolean);

    function GetLowLevelLog: Boolean;
    procedure SetLowLevelLog(const AValue: Boolean);

    function GetLogPath: string;

    function GetComSettings: TGPSComSettings;
    procedure SetComSettings(const AValue: TGPSComSettings);

    function GetTcpSettings: TGPSNetworkSettings;
    procedure SetTcpSettings(const AValue: TGPSNetworkSettings);

    function GetUdpSettings: TGPSNetworkSettings;
    procedure SetUdpSettings(const AValue: TGPSNetworkSettings);

    function GetReplaySettings: TGPSReplaySettings;
    procedure SetReplaySettings(const AValue: TGPSReplaySettings);

    function GetStatic: IGPSModuleByCOMPortSettings;
  public
    constructor Create(const ALogPath: IPathConfig);
  end;

implementation

uses
  u_GPSModuleByCOMPortSettings;

function ReadNetworkSettings(
  const AConfigData: IConfigDataProvider;
  const AKeyPrefix: string;
  const ADefaults: TGPSNetworkSettings
): TGPSNetworkSettings;
begin
  Result := ADefaults;
  Result.ServerAddress := AConfigData.ReadString(AKeyPrefix + 'ServerAddress', Result.ServerAddress);
end;

procedure WriteNetworkSettings(
  const AConfigData: IConfigDataWriteProvider;
  const AKeyPrefix: string;
  const ASettings: TGPSNetworkSettings
);
begin
  AConfigData.WriteString(AKeyPrefix + 'ServerAddress', ASettings.ServerAddress);
end;

{ TGPSModuleByCOMPortConfig }

constructor TGPSModuleByCOMPortConfig.Create(const ALogPath: IPathConfig);
begin
  inherited Create;

  FGPSOrigin := gpsoNMEA; // COM NMEA

  FConnectionTimeout := 300;
  FDelay := 1000;

  FLogPath := ALogPath;
  FLowLevelLog := False;

  FComSettings.Port := 1;
  FComSettings.BaudRate := 4800;
  FComSettings.AutodetectCOMOnConnect := False;
  FComSettings.AutodetectCOMFlags := 0;

  FTcpSettings.ServerAddress := '';

  FUdpSettings.ServerAddress := '';

  FReplaySettings.UseTrackList := True;
  FReplaySettings.TrackFile := '';
  FReplaySettings.PlaybackSpeedMultiplier := 10;
  FReplaySettings.LoopPlayback := False;

  FAutoReconnect := True;
end;

function TGPSModuleByCOMPortConfig.CreateStatic: IInterface;
begin
  Result :=
    TGPSModuleByCOMPortSettings.Create(
      FGPSOrigin,
      FConnectionTimeout,
      FDelay,
      FAutoReconnect,
      FLowLevelLog,
      FLogPath.FullPath,
      FComSettings,
      FTcpSettings,
      FUdpSettings,
      FReplaySettings
    ) as IGPSModuleByCOMPortSettings;
end;

procedure TGPSModuleByCOMPortConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VComSettings: TGPSComSettings;
  VNetworkSettings: TGPSNetworkSettings;
  VReplaySettings: TGPSReplaySettings;
begin
  inherited;

  if AConfigData = nil then begin
    Exit;
  end;

  Self.StopNotify;
  try
    SetGPSOrigin(TGPSOrigin(AConfigData.ReadInteger('GpsOrigin', Ord(FGPSOrigin))));

    SetConnectionTimeout(AConfigData.ReadInteger('timeout', FConnectionTimeout));
    SetDelay(AConfigData.ReadInteger('update', FDelay));
    SetAutoReconnect(AConfigData.ReadBool('AutoReconnect', FAutoReconnect));
    SetLowLevelLog(AConfigData.ReadBool('NMEAlog', FLowLevelLog));

    VComSettings := FComSettings;
    VComSettings.Port := AConfigData.ReadInteger('COM', VComSettings.Port);
    VComSettings.BaudRate := AConfigData.ReadInteger('BaudRate', VComSettings.BaudRate);
    VComSettings.AutodetectCOMOnConnect := AConfigData.ReadBool('AutodetectCOMOnConnect', VComSettings.AutodetectCOMOnConnect);
    VComSettings.AutodetectCOMFlags := AConfigData.ReadInteger('AutodetectCOMFlags', VComSettings.AutodetectCOMFlags);
    SetComSettings(VComSettings);

    VNetworkSettings := ReadNetworkSettings(AConfigData, 'Tcp', FTcpSettings);
    SetTcpSettings(VNetworkSettings);

    VNetworkSettings := ReadNetworkSettings(AConfigData, 'Udp', FUdpSettings);
    SetUdpSettings(VNetworkSettings);

    VReplaySettings := FReplaySettings;
    VReplaySettings.UseTrackList := AConfigData.ReadBool('ReplayUseTrackList', VReplaySettings.UseTrackList);
    VReplaySettings.TrackFile := AConfigData.ReadString('ReplayTrackFile', VReplaySettings.TrackFile);
    VReplaySettings.PlaybackSpeedMultiplier := AConfigData.ReadInteger('ReplayPlaybackSpeedMultiplier', VReplaySettings.PlaybackSpeedMultiplier);
    VReplaySettings.LoopPlayback := AConfigData.ReadBool('ReplayLoopPlayback', VReplaySettings.LoopPlayback);
    SetReplaySettings(VReplaySettings);

    SetChanged;
  finally
    Self.StartNotify;
  end;
end;

procedure TGPSModuleByCOMPortConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('GpsOrigin', Ord(FGPSOrigin));
  AConfigData.WriteInteger('timeout', FConnectionTimeout);
  AConfigData.WriteInteger('update', FDelay);
  AConfigData.WriteBool('AutoReconnect', FAutoReconnect);
  AConfigData.WriteBool('NMEAlog', FLowLevelLog);
  AConfigData.WriteInteger('COM', FComSettings.Port);
  AConfigData.WriteInteger('BaudRate', FComSettings.BaudRate);
  AConfigData.WriteBool('AutodetectCOMOnConnect', FComSettings.AutodetectCOMOnConnect);
  AConfigData.WriteInteger('AutodetectCOMFlags', FComSettings.AutodetectCOMFlags);
  WriteNetworkSettings(AConfigData, 'Tcp', FTcpSettings);
  WriteNetworkSettings(AConfigData, 'Udp', FUdpSettings);
  AConfigData.WriteBool('ReplayUseTrackList', FReplaySettings.UseTrackList);
  AConfigData.WriteString('ReplayTrackFile', FReplaySettings.TrackFile);
  AConfigData.WriteInteger('ReplayPlaybackSpeedMultiplier', FReplaySettings.PlaybackSpeedMultiplier);
  AConfigData.WriteBool('ReplayLoopPlayback', FReplaySettings.LoopPlayback);
end;

function TGPSModuleByCOMPortConfig.GetComSettings: TGPSComSettings;
begin
  LockRead;
  try
    Result := FComSettings;
  finally
    UnlockRead;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetComSettings(const AValue: TGPSComSettings);
var
  VValue: TGPSComSettings;
begin
  LockWrite;
  try
    VValue := AValue;
    if (VValue.Port = 0) or (VValue.Port > 255) then begin
      VValue.Port := FComSettings.Port;
    end;
    if (VValue.BaudRate = 0) or (VValue.BaudRate > 1000000) then begin
      VValue.BaudRate := FComSettings.BaudRate;
    end;
    if FComSettings <> VValue then begin
      FComSettings := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TGPSModuleByCOMPortConfig.GetConnectionTimeout: DWORD;
begin
  LockRead;
  try
    Result := FConnectionTimeout;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetDelay: DWORD;
begin
  LockRead;
  try
    Result := FDelay;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetLogPath: string;
begin
  Result := FLogPath.FullPath;
end;

function TGPSModuleByCOMPortConfig.GetLowLevelLog: Boolean;
begin
  LockRead;
  try
    Result := FLowLevelLog;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetTcpSettings: TGPSNetworkSettings;
begin
  LockRead;
  try
    Result := FTcpSettings;
  finally
    UnlockRead;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetTcpSettings(const AValue: TGPSNetworkSettings);
begin
  LockWrite;
  try
    if FTcpSettings <> AValue then begin
      FTcpSettings := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TGPSModuleByCOMPortConfig.GetUdpSettings: TGPSNetworkSettings;
begin
  LockRead;
  try
    Result := FUdpSettings;
  finally
    UnlockRead;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetUdpSettings(const AValue: TGPSNetworkSettings);
begin
  LockWrite;
  try
    if FUdpSettings <> AValue then begin
      FUdpSettings := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TGPSModuleByCOMPortConfig.GetReplaySettings: TGPSReplaySettings;
begin
  LockRead;
  try
    Result := FReplaySettings;
  finally
    UnlockRead;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetReplaySettings(const AValue: TGPSReplaySettings);
begin
  LockWrite;
  try
    if FReplaySettings <> AValue then begin
      FReplaySettings := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TGPSModuleByCOMPortConfig.GetStatic: IGPSModuleByCOMPortSettings;
begin
  Result := IGPSModuleByCOMPortSettings(GetStaticInternal);
end;

function TGPSModuleByCOMPortConfig.GetGPSOrigin: TGPSOrigin;
begin
  LockRead;
  try
    Result := FGPSOrigin;
  finally
    UnlockRead;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetConnectionTimeout(const AValue: DWORD);
begin
  if (AValue > 0) and (AValue <= 6000) then begin
    LockWrite;
    try
      if FConnectionTimeout <> AValue then begin
        FConnectionTimeout := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetDelay(const AValue: DWORD);
begin
  if (AValue > 0) and (AValue <= 300000) then begin
    LockWrite;
    try
      if FDelay <> AValue then begin
        FDelay := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetLowLevelLog(const AValue: Boolean);
begin
  LockWrite;
  try
    if FLowLevelLog <> AValue then begin
      FLowLevelLog := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetGPSOrigin(const AValue: TGPSOrigin);
begin
  LockWrite;
  try
    if FGPSOrigin <> AValue then begin
      FGPSOrigin := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TGPSModuleByCOMPortConfig.GetAutoReconnect: Boolean;
begin
  LockRead;
  try
    Result := FAutoReconnect;
  finally
    UnlockRead;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetAutoReconnect(const AValue: Boolean);
begin
  LockWrite;
  try
    if FAutoReconnect <> AValue then begin
      FAutoReconnect := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

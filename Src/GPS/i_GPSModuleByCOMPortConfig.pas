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

unit i_GPSModuleByCOMPortConfig;

interface

uses
  Windows,
  t_GPSModuleByCOMPortSettings,
  i_GPSModuleByCOMPortSettings,
  i_ConfigDataElement;

type
  IGPSModuleByCOMPortConfig = interface(IConfigDataElement)
    ['{75AC2DE2-4C88-4A0C-A1D1-D99E51995C78}']
    function GetGPSOrigin: TGPSOrigin;
    procedure SetGPSOrigin(const AValue: TGPSOrigin);
    property GPSOrigin: TGPSOrigin read GetGPSOrigin write SetGPSOrigin;

    function GetConnectionTimeout: DWORD;
    procedure SetConnectionTimeout(const AValue: DWORD);
    property ConnectionTimeout: DWORD read GetConnectionTimeout write SetConnectionTimeout;

    function GetDelay: DWORD;
    procedure SetDelay(const AValue: DWORD);
    property Delay: DWORD read GetDelay write SetDelay;

    // NMEA Log
    function GetLowLevelLog: Boolean;
    procedure SetLowLevelLog(const AValue: Boolean);
    property LowLevelLog: Boolean read GetLowLevelLog write SetLowLevelLog;

    function GetLogPath: string;
    property LogPath: string read GetLogPath;

    // COM NMEA
    function GetComSettings: TGPSComSettings;
    procedure SetComSettings(const AValue: TGPSComSettings);
    property ComSettings: TGPSComSettings read GetComSettings write SetComSettings;

    // TCP NMEA
    function GetTcpSettings: TGPSNetworkSettings;
    procedure SetTcpSettings(const AValue: TGPSNetworkSettings);
    property TcpSettings: TGPSNetworkSettings read GetTcpSettings write SetTcpSettings;

    // UDP NMEA
    function GetUdpSettings: TGPSNetworkSettings;
    procedure SetUdpSettings(const AValue: TGPSNetworkSettings);
    property UdpSettings: TGPSNetworkSettings read GetUdpSettings write SetUdpSettings;

    // Replay Track
    function GetReplaySettings: TGPSReplaySettings;
    procedure SetReplaySettings(const AValue: TGPSReplaySettings);
    property ReplaySettings: TGPSReplaySettings read GetReplaySettings write SetReplaySettings;

    function GetStatic: IGPSModuleByCOMPortSettings;
  end;

implementation

end.

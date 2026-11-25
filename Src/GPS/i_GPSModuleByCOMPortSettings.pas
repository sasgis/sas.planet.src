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

unit i_GPSModuleByCOMPortSettings;

interface

uses
  Windows,
  t_GPSModuleByCOMPortSettings;

const
  // the name of the file with the list of tracks/paths to replay
  CReplayTrackListFileName = 'ReplayTrack.txt';

type
  IGPSModuleByCOMPortSettings = interface
    ['{1E9AF59D-8988-4747-9952-5D17A0B0DB33}']
    function GetGPSOrigin: TGPSOrigin;
    property GPSOrigin: TGPSOrigin read GetGPSOrigin;

    function GetConnectionTimeout: DWORD;
    property ConnectionTimeout: DWORD read GetConnectionTimeout;

    function GetDelay: DWORD;
    property Delay: DWORD read GetDelay;

    function GetAutoReconnect: Boolean;
    property AutoReconnect: Boolean read GetAutoReconnect;

    function GetConnectErrorDialogSuppressed: Boolean;
    property ConnectErrorDialogSuppressed: Boolean read GetConnectErrorDialogSuppressed;

    // NMEA Log
    function GetLowLevelLog: Boolean;
    property LowLevelLog: Boolean read GetLowLevelLog;

    function GetLogPath: string;
    property LogPath: string read GetLogPath;

    // COM NMEA
    function GetComSettings: TGPSComSettings;
    property ComSettings: TGPSComSettings read GetComSettings;

    // TCP NMEA
    function GetTcpSettings: TGPSNetworkSettings;
    property TcpSettings: TGPSNetworkSettings read GetTcpSettings;

    // UDP NMEA
    function GetUdpSettings: TGPSNetworkSettings;
    property UdpSettings: TGPSNetworkSettings read GetUdpSettings;

    // Replay Track
    function GetReplaySettings: TGPSReplaySettings;
    property ReplaySettings: TGPSReplaySettings read GetReplaySettings;
  end;

implementation

end.

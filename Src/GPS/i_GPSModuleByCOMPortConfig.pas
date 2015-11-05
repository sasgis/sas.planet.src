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

unit i_GPSModuleByCOMPortConfig;

interface

uses
  Windows,
  i_GPSModuleByCOMPortSettings,
  i_ConfigDataElement;

type
  IGPSModuleByCOMPortConfig = interface(IConfigDataElement)
    ['{75AC2DE2-4C88-4A0C-A1D1-D99E51995C78}']
    function GetPort: DWORD;
    procedure SetPort(const AValue: DWORD);
    property Port: DWORD read GetPort write SetPort;

    function GetBaudRate: DWORD;
    procedure SetBaudRate(const AValue: DWORD);
    property BaudRate: DWORD read GetBaudRate write SetBaudRate;

    function GetConnectionTimeout: DWORD;
    procedure SetConnectionTimeout(const AValue: DWORD);
    property ConnectionTimeout: DWORD read GetConnectionTimeout write SetConnectionTimeout;

    function GetDelay: DWORD;
    procedure SetDelay(const AValue: DWORD);
    property Delay: DWORD read GetDelay write SetDelay;

    // NMEALog
    function GetLowLevelLog: Boolean;
    procedure SetLowLevelLog(const AValue: Boolean);
    property LowLevelLog: Boolean read GetLowLevelLog write SetLowLevelLog;

    function GetLogPath: string;
    property LogPath: string read GetLogPath;

    function GetStatic: IGPSModuleByCOMPortSettings;

    // USBGarmin
    function GetGPSOrigin: TGPSOrigin;
    procedure SetGPSOrigin(const AValue: TGPSOrigin);
    property GPSOrigin: TGPSOrigin read GetGPSOrigin write SetGPSOrigin;

    function GetAutodetectCOMOnConnect: Boolean;
    procedure SetAutodetectCOMOnConnect(const AValue: Boolean);
    property AutodetectCOMOnConnect: Boolean read GetAutodetectCOMOnConnect write SetAutodetectCOMOnConnect;

    function GetAutodetectCOMFlags: DWORD;
    procedure SetAutodetectCOMFlags(const AValue: DWORD);
    property AutodetectCOMFlags: DWORD read GetAutodetectCOMFlags write SetAutodetectCOMFlags;
  end;

implementation

end.

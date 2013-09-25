{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit i_GPSModuleByCOMPortSettings;

interface

uses
  Windows;

type
  TGPSOrigin = (gpsoNMEA, gpsoGarmin, gpsoFlyOnTrack, gpsoLocationAPI);

  IGPSModuleByCOMPortSettings = interface
    ['{1E9AF59D-8988-4747-9952-5D17A0B0DB33}']
    function GetPort: DWORD; safecall;
    property Port: DWORD read GetPort;

    function GetBaudRate: DWORD; safecall;
    property BaudRate: DWORD read GetBaudRate;

    function GetConnectionTimeout: DWORD; safecall;
    property ConnectionTimeout: DWORD read GetConnectionTimeout;

    function GetDelay: DWORD; safecall;
    property Delay: DWORD read GetDelay;

    // NMEALog
    function GetLowLevelLog: Boolean; safecall;
    property LowLevelLog: Boolean read GetLowLevelLog;

    function GetLogPath: WideString; safecall;
    property LogPath: WideString read GetLogPath;

    // USBGarmin
    function GetGPSOrigin: TGPSOrigin; safecall;
    property GPSOrigin: TGPSOrigin read GetGPSOrigin;

    function GetAutodetectCOMOnConnect: Boolean; safecall;
    property AutodetectCOMOnConnect: Boolean read GetAutodetectCOMOnConnect;

    function GetAutodetectCOMFlags: DWORD; safecall;
    property AutodetectCOMFlags: DWORD read GetAutodetectCOMFlags;
  end;

implementation

end.

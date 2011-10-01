{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

type
  IGPSModuleByCOMPortSettings = interface
    ['{1E9AF59D-8988-4747-9952-5D17A0B0DB33}']
    function GetPort: Integer; safecall;
    property Port: Integer read GetPort;

    function GetBaudRate: Integer; safecall;
    property BaudRate: Integer read GetBaudRate;

    function GetConnectionTimeout: Integer; safecall;
    property ConnectionTimeout: Integer read GetConnectionTimeout;

    function GetDelay: Integer; safecall;
    property Delay: Integer read GetDelay;

    function GetNMEALog: Boolean; safecall;
    property NMEALog: Boolean read GetNMEALog;

    function GetLogPath: WideString; safecall;
    property LogPath: WideString read GetLogPath;
  end;

implementation

end.

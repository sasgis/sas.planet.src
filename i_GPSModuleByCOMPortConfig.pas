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

unit i_GPSModuleByCOMPortConfig;

interface

uses
  i_GPSModuleByCOMPortSettings,
  i_ConfigDataElement;

type
  IGPSModuleByCOMPortConfig = interface(IConfigDataElement)
    ['{75AC2DE2-4C88-4A0C-A1D1-D99E51995C78}']
    function GetPort: Integer;
    procedure SetPort(const AValue: Integer);
    property Port: Integer read GetPort write SetPort;

    function GetBaudRate: Integer;
    procedure SetBaudRate(const AValue: Integer);
    property BaudRate: Integer read GetBaudRate write SetBaudRate;

    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const AValue: Integer);
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;

    function GetDelay: Integer;
    procedure SetDelay(const AValue: Integer);
    property Delay: Integer read GetDelay write SetDelay;

    function GetNMEALog: Boolean;
    procedure SetNMEALog(const AValue: Boolean);
    property NMEALog: Boolean read GetNMEALog write SetNMEALog;

    function GetLogPath: WideString;
    property LogPath: WideString read GetLogPath;

    function GetStatic: IGPSModuleByCOMPortSettings;
  end;

implementation

end.

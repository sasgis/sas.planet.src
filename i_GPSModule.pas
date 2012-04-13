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

unit i_GPSModule;

interface

uses
  i_JclNotify,
  i_GPS;

type
  IGPSModule = interface
    ['{B477FFBD-C36D-40C6-AF7F-B118E47A6815}']
    function GetPosition: IGPSPosition; safecall;
    property Position: IGPSPosition read GetPosition;

    function GetDataReciveNotifier: IJclNotifier; safecall;
    property DataReciveNotifier: IJclNotifier read GetDataReciveNotifier;

    function GetConnectingNotifier: IJclNotifier; safecall;
    property ConnectingNotifier: IJclNotifier read GetConnectingNotifier;

    function GetConnectedNotifier: IJclNotifier; safecall;
    property ConnectedNotifier: IJclNotifier read GetConnectedNotifier;

    function GetDisconnectingNotifier: IJclNotifier; safecall;
    property DisconnectingNotifier: IJclNotifier read GetDisconnectingNotifier;

    function GetDisconnectedNotifier: IJclNotifier; safecall;
    property DisconnectedNotifier: IJclNotifier read GetDisconnectedNotifier;

    function GetTimeOutNotifier: IJclNotifier; safecall;
    property TimeOutNotifier: IJclNotifier read GetTimeOutNotifier;

    function GetConnectErrorNotifier: IJclNotifier; safecall;
    property ConnectErrorNotifier: IJclNotifier read GetConnectErrorNotifier;
  end;

implementation

end.

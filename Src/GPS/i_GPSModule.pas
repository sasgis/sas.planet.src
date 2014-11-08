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

unit i_GPSModule;

interface

uses
  i_Notifier,
  i_GPS;

type
  IGPSModule = interface
    ['{B477FFBD-C36D-40C6-AF7F-B118E47A6815}']
    function GetPosition: IGPSPosition; safecall;
    property Position: IGPSPosition read GetPosition;

    function GetDataReciveNotifier: INotifier; safecall;
    property DataReciveNotifier: INotifier read GetDataReciveNotifier;

    function GetConnectingNotifier: INotifier; safecall;
    property ConnectingNotifier: INotifier read GetConnectingNotifier;

    function GetConnectedNotifier: INotifier; safecall;
    property ConnectedNotifier: INotifier read GetConnectedNotifier;

    function GetDisconnectingNotifier: INotifier; safecall;
    property DisconnectingNotifier: INotifier read GetDisconnectingNotifier;

    function GetDisconnectedNotifier: INotifier; safecall;
    property DisconnectedNotifier: INotifier read GetDisconnectedNotifier;

    function GetTimeOutNotifier: INotifier; safecall;
    property TimeOutNotifier: INotifier read GetTimeOutNotifier;

    function GetConnectErrorNotifier: INotifier; safecall;
    property ConnectErrorNotifier: INotifier read GetConnectErrorNotifier;

    function GetGPSUnitInfo: String;
    property GPSUnitInfo: String read GetGPSUnitInfo;

    procedure ApplyUTCDateTime;
    procedure ResetDGPS;
    procedure ResetUnitInfo;
  end;

implementation

end.

{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit i_TileStorageBerkeleyDBConfigStatic;

interface

type
  ITileStorageBerkeleyDBConfigStatic = interface
    ['{7891C139-B5C4-444E-8AF1-0668510000FB}']
    function GetIsReadOnly: Boolean;
    property IsReadOnly: Boolean read GetIsReadOnly;

    function GetSyncInterval: Cardinal;
    property SyncInterval: Cardinal read GetSyncInterval;

    function GetCommitsCountToSync: Cardinal;
    property CommitsCountToSync: Cardinal read GetCommitsCountToSync;

    function GetPoolSize: Cardinal;
    property PoolSize: Cardinal read GetPoolSize;

    function GetPoolObjectTTL: Cardinal;
    property PoolObjectTTL: Cardinal read GetPoolObjectTTL;

    function GetDatabasePageSize: Cardinal;
    property DatabasePageSize: Cardinal read GetDatabasePageSize;

    function GetOnDeadLockRetryCount: Integer;
    property OnDeadLockRetryCount: Integer read GetOnDeadLockRetryCount;

    function GetIsFullVerboseMode: Boolean;
    property IsFullVerboseMode: Boolean read GetIsFullVerboseMode;
  end;

implementation

end.

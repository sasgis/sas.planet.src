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

unit i_TileStorageBerkeleyDBConfigStatic;

interface

type
  ITileStorageBerkeleyDBConfigStatic = interface
    ['{7891C139-B5C4-444E-8AF1-0668510000FB}']
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

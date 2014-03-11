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

unit i_Synchronizer;

interface

uses
  i_ReadWriteSyncFactory;

type
  ISynchronizer = interface
    ['{D483F620-6E80-4BBF-885B-CFCE2F5D6ADE}']
    // very short operation - about 1 or 2 simple variables read or write
    // makes first available from { MakeSyncSRW, MakeSyncSpinLock, MakeSyncRes, MakeSyncMREW }
    function GetSyncVariable: IReadWriteSyncFactory;
    property SyncVariable: IReadWriteSyncFactory read GetSyncVariable;

    function GetSyncVariableRecursive: IReadWriteSyncFactory;
    property SyncVariableRecursive: IReadWriteSyncFactory read GetSyncVariableRecursive;

    // small symmetrical operation
    // makes first available from { MakeSyncRes, MakeSyncSRW, MakeSyncMREW }
    function GetSyncSymmetrical: IReadWriteSyncFactory;
    property SyncSymmetrical: IReadWriteSyncFactory read GetSyncSymmetrical;

    function GetSyncSymmetricalRecursive: IReadWriteSyncFactory;
    property SyncSymmetricalRecursive: IReadWriteSyncFactory read GetSyncVariableRecursive;

    // others (many readers with 1-2 writers)
    // makes first available from { MakeSyncSRW, MakeSyncRes, MakeSyncMREW }
    function GetSyncStd: IReadWriteSyncFactory;
    property SyncStd: IReadWriteSyncFactory read GetSyncStd;

    function GetSyncStdRecursive: IReadWriteSyncFactory;
    property SyncStdRecursive: IReadWriteSyncFactory read GetSyncStdRecursive;

    // many concurrent readers and writers
    // makes first available from { MakeSyncRes, MakeSyncMREW }
    function GetSyncBig: IReadWriteSyncFactory;
    property SyncBig: IReadWriteSyncFactory read GetSyncBig;

    function GetSyncBigRecursive: IReadWriteSyncFactory;
    property SyncBigRecursive: IReadWriteSyncFactory read GetSyncBigRecursive;
  end;

implementation

end.

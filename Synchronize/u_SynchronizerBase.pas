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

unit u_SynchronizerBase;

interface

uses
  i_ReadWriteSyncFactory,
  i_Synchronizer;

type
  TSynchronizerBase = class(TInterfacedObject, ISynchronizer)
  private
    FSyncVariable: IReadWriteSyncFactory;
    FSyncVariableRecursive: IReadWriteSyncFactory;
    FSyncSymmetrical: IReadWriteSyncFactory;
    FSyncSymmetricalRecursive: IReadWriteSyncFactory;
    FSyncStd: IReadWriteSyncFactory;
    FSyncStdRecursive: IReadWriteSyncFactory;
    FSyncBig: IReadWriteSyncFactory;
    FSyncBigRecursive: IReadWriteSyncFactory;
  private
    function GetSyncVariable: IReadWriteSyncFactory;
    function GetSyncVariableRecursive: IReadWriteSyncFactory;
    function GetSyncSymmetrical: IReadWriteSyncFactory;
    function GetSyncSymmetricalRecursive: IReadWriteSyncFactory;
    function GetSyncStd: IReadWriteSyncFactory;
    function GetSyncStdRecursive: IReadWriteSyncFactory;
    function GetSyncBig: IReadWriteSyncFactory;
    function GetSyncBigRecursive: IReadWriteSyncFactory;
  public
    constructor Create(
      const ASyncVariable: IReadWriteSyncFactory;
      const ASyncVariableRecursive: IReadWriteSyncFactory;
      const ASyncSymmetrical: IReadWriteSyncFactory;
      const ASyncSymmetricalRecursive: IReadWriteSyncFactory;
      const ASyncStd: IReadWriteSyncFactory;
      const ASyncStdRecursive: IReadWriteSyncFactory;
      const ASyncBig: IReadWriteSyncFactory;
      const ASyncBigRecursive: IReadWriteSyncFactory
    );
  end;

implementation

{ TSynchronizerBase }

constructor TSynchronizerBase.Create(
  const ASyncVariable: IReadWriteSyncFactory;
  const ASyncVariableRecursive: IReadWriteSyncFactory;
  const ASyncSymmetrical: IReadWriteSyncFactory;
  const ASyncSymmetricalRecursive: IReadWriteSyncFactory;
  const ASyncStd: IReadWriteSyncFactory;
  const ASyncStdRecursive: IReadWriteSyncFactory;
  const ASyncBig: IReadWriteSyncFactory;
  const ASyncBigRecursive: IReadWriteSyncFactory
);
begin
  inherited Create;
  FSyncVariable := ASyncVariable;
  FSyncVariableRecursive := ASyncVariableRecursive;
  FSyncSymmetrical := ASyncSymmetrical;
  FSyncSymmetricalRecursive := ASyncSymmetricalRecursive;
  FSyncStd := ASyncStd;
  FSyncStdRecursive := ASyncStdRecursive;
  FSyncBig := ASyncBig;
  FSyncBigRecursive := ASyncBigRecursive;
end;

function TSynchronizerBase.GetSyncBig: IReadWriteSyncFactory;
begin
  Result := FSyncBig;
end;

function TSynchronizerBase.GetSyncBigRecursive: IReadWriteSyncFactory;
begin
  Result := FSyncBigRecursive;
end;

function TSynchronizerBase.GetSyncStd: IReadWriteSyncFactory;
begin
  Result := FSyncStd;
end;

function TSynchronizerBase.GetSyncStdRecursive: IReadWriteSyncFactory;
begin
  Result := FSyncStdRecursive;
end;

function TSynchronizerBase.GetSyncSymmetrical: IReadWriteSyncFactory;
begin
  Result := FSyncSymmetrical;
end;

function TSynchronizerBase.GetSyncSymmetricalRecursive: IReadWriteSyncFactory;
begin
  Result := FSyncSymmetricalRecursive;
end;

function TSynchronizerBase.GetSyncVariable: IReadWriteSyncFactory;
begin
  Result := FSyncVariable;
end;

function TSynchronizerBase.GetSyncVariableRecursive: IReadWriteSyncFactory;
begin
  Result := FSyncVariableRecursive;
end;

end.

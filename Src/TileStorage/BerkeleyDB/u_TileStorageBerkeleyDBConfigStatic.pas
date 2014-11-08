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

unit u_TileStorageBerkeleyDBConfigStatic;

interface

uses
  i_TileStorageBerkeleyDBConfigStatic,
  u_BaseInterfacedObject;

type
  TTileStorageBerkeleyDBConfigStatic = class(TBaseInterfacedObject, ITileStorageBerkeleyDBConfigStatic)
  private
    FSyncInterval: Cardinal;
    FCommitsCountToSync: Cardinal;
    FPoolSize: Cardinal;
    FPoolObjectTTL: Cardinal;
    FDatabasePageSize: Cardinal;
    FOnDeadLockRetryCount: Integer;
    FIsFullVerboseMode: Boolean;
  private
    { ITileStorageBerkeleyDBConfigStatic }
    function GetSyncInterval: Cardinal;
    function GetCommitsCountToSync: Cardinal;
    function GetPoolSize: Cardinal;
    function GetPoolObjectTTL: Cardinal;
    function GetDatabasePageSize: Cardinal;
    function GetOnDeadLockRetryCount: Integer;
    function GetIsFullVerboseMode: Boolean;
  public
    constructor Create(
      const ASyncInterval: Cardinal;
      const ACommitsCountToSync: Cardinal;
      const APoolSize: Cardinal;
      const APoolObjectTTL: Cardinal;
      const ADatabasePageSize: Cardinal;
      const AOnDeadLockRetryCount: Integer;
      const AIsFullVerboseMode: Boolean
    );
  end;

implementation

{ TTileStorageBerkeleyDBConfigStatic }

constructor TTileStorageBerkeleyDBConfigStatic.Create(
  const ASyncInterval: Cardinal;
  const ACommitsCountToSync: Cardinal;
  const APoolSize: Cardinal;
  const APoolObjectTTL: Cardinal;
  const ADatabasePageSize: Cardinal;
  const AOnDeadLockRetryCount: Integer;
  const AIsFullVerboseMode: Boolean
);
begin
  inherited Create;
  FSyncInterval := ASyncInterval;
  FCommitsCountToSync := ACommitsCountToSync;
  FPoolSize := APoolSize;
  FPoolObjectTTL := APoolObjectTTL;
  FDatabasePageSize := ADatabasePageSize;
  FOnDeadLockRetryCount := AOnDeadLockRetryCount;
  FIsFullVerboseMode := AIsFullVerboseMode;
end;

function TTileStorageBerkeleyDBConfigStatic.GetSyncInterval: Cardinal;
begin
  Result := FSyncInterval;
end;

function TTileStorageBerkeleyDBConfigStatic.GetCommitsCountToSync: Cardinal;
begin
  Result := FCommitsCountToSync;
end;

function TTileStorageBerkeleyDBConfigStatic.GetPoolSize: Cardinal;
begin
  Result := FPoolSize;
end;

function TTileStorageBerkeleyDBConfigStatic.GetPoolObjectTTL: Cardinal;
begin
  Result := FPoolObjectTTL;
end;

function TTileStorageBerkeleyDBConfigStatic.GetDatabasePageSize: Cardinal;
begin
  Result := FDatabasePageSize;
end;

function TTileStorageBerkeleyDBConfigStatic.GetOnDeadLockRetryCount: Integer;
begin
  Result := FOnDeadLockRetryCount;
end;

function TTileStorageBerkeleyDBConfigStatic.GetIsFullVerboseMode: Boolean;
begin
  Result := FIsFullVerboseMode;
end;

end.

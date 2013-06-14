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

unit u_TileStorageBerkeleyDBConfigStatic;

interface

uses
  i_TileStorageBerkeleyDBConfigStatic,
  u_BaseInterfacedObject;

type
  TTileStorageBerkeleyDBConfigStatic = class(TBaseInterfacedObject, ITileStorageBerkeleyDBConfigStatic)
  private
    FStoragePath: string;
    FConfigFileName: string;
    FIsReadOnly: Boolean;
    FSyncInterval: Cardinal;
    FCommitsCountToSync: Cardinal;
    FPoolSize: Cardinal;
    FPoolObjectTTL: Cardinal;
    FDatabasePageSize: Cardinal;
  private
    procedure DoReadConfig;
  private
    { ITileStorageBerkeleyDBConfigStatic }
    function GetIsReadOnly: Boolean;
    function GetSyncInterval: Cardinal;
    function GetCommitsCountToSync: Cardinal;
    function GetPoolSize: Cardinal;
    function GetPoolObjectTTL: Cardinal;
    function GetDatabasePageSize: Cardinal;
  public
    constructor Create(const AStoragePath: string);
  end;

implementation

uses
  SysUtils,
  IniFiles;

const
  cBerkeleyDBStorageConfFileName = 'StorageConfig.ini';

{ TTileStorageBerkeleyDBConfigStatic }

constructor TTileStorageBerkeleyDBConfigStatic.Create(const AStoragePath: string);
begin
  inherited Create;
  FStoragePath := IncludeTrailingPathDelimiter(AStoragePath);
  FConfigFileName := FStoragePath + cBerkeleyDBStorageConfFileName;

  FIsReadOnly := False;
  FSyncInterval := 300000;
  FCommitsCountToSync := 1000;
  FPoolSize := 32;
  FPoolObjectTTL := 60000;
  FDatabasePageSize := 1024;

  DoReadConfig;
end;

function TTileStorageBerkeleyDBConfigStatic.GetIsReadOnly: Boolean;
begin
  Result := FIsReadOnly;
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

procedure TTileStorageBerkeleyDBConfigStatic.DoReadConfig;
var
  VIni: TIniFile;
begin
  if FileExists(FConfigFileName) then begin
    VIni := TIniFile.Create(FConfigFileName);
    try
      FIsReadOnly := VIni.ReadBool('BerkeleyDB', 'IsReadOnly', FIsReadOnly); 
      FSyncInterval := VIni.ReadInteger('BerkeleyDB', 'SyncInterval', FSyncInterval);
      FCommitsCountToSync := VIni.ReadInteger('BerkeleyDB', 'CommitsCountToSync', FCommitsCountToSync);
      FPoolSize := VIni.ReadInteger('BerkeleyDB', 'PoolSize', FPoolSize);
      FPoolObjectTTL := VIni.ReadInteger('BerkeleyDB', 'PoolObjectTTL', FPoolObjectTTL);
      FDatabasePageSize := VIni.ReadInteger('BerkeleyDB', 'DatabasePageSize', FDatabasePageSize);
    finally
      VIni.Free;
    end;
  end;
end;

end.

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

unit u_TileStorageTypeBerkeleyDB;

interface

uses
  i_ProjectionSet,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_NotifierTime,
  i_MapVersionFactory,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileStorageTypeConfig,
  i_ConfigDataProvider,
  i_NotifierTilePyramidUpdate,
  i_TileInfoBasicMemCache,
  i_GlobalBerkeleyDBHelper,
  i_TileStorageBerkeleyDBConfigStatic,
  u_TileStorageTypeBase;

type
  TTileStorageTypeBerkeleyDB = class(TTileStorageTypeBase)
  private
    FGCNotifier: INotifierTime;
    FIsVersioned: Boolean;
    FContentTypeManager: IContentTypeManager;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    function ReadConfig(
      const AStorageConfigData: IConfigDataProvider
    ): ITileStorageBerkeleyDBConfigStatic;
  protected
    function BuildStorageInternal(
      const AStorageConfigData: IConfigDataProvider;
      const AForceAbilities: ITileStorageAbilities;
      const AProjectionSet: IProjectionSet;
      const AMainContentType: IContentTypeInfoBasic;
      const ATileNotifier: INotifierTilePyramidUpdateInternal;
      const APath: string;
      const ACacheTileInfo: ITileInfoBasicMemCache
    ): ITileStorage; override;
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AGCNotifier: INotifierTime;
      const AIsVersioned: Boolean;
      const AContentTypeManager: IContentTypeManager;
      const AMapVersionFactory: IMapVersionFactory;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  u_TileStorageAbilities,
  u_TileStorageBerkeleyDBConfigStatic,
  u_TileStorageBerkeleyDB;

{ TTileStorageTypeBerkeleyDB }

constructor TTileStorageTypeBerkeleyDB.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AGCNotifier: INotifierTime;
  const AIsVersioned: Boolean;
  const AContentTypeManager: IContentTypeManager;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ITileStorageTypeConfig
);
var
  VAbilities: ITileStorageTypeAbilities;
  VVersionSupport: TTileStorageTypeVersionSupport;
begin
  if AIsVersioned then begin
    VVersionSupport := tstvsMultiVersions;
  end else begin
    VVersionSupport := tstvsVersionStored;
  end;
  VAbilities :=
    TTileStorageTypeAbilities.Create(
      TTileStorageAbilities.Create(False, True, True, True, True, True),
      VVersionSupport,
      True,
      tstcFolder
    );
  inherited Create(
    VAbilities,
    AMapVersionFactory,
    AConfig
  );
  FGCNotifier := AGCNotifier;
  FIsVersioned := AIsVersioned;
  FContentTypeManager := AContentTypeManager;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
end;

function TTileStorageTypeBerkeleyDB.ReadConfig(
  const AStorageConfigData: IConfigDataProvider
): ITileStorageBerkeleyDBConfigStatic;
var
  VSyncInterval: Cardinal;
  VCommitsCountToSync: Cardinal;
  VPoolSize: Cardinal;
  VPoolObjectTTL: Cardinal;
  VDatabasePageSize: Cardinal;
  VOnDeadLockRetryCount: Integer;
  VIsFullVerboseMode: Boolean;
  VBerkeleyDBConfigData: IConfigDataProvider;
begin
  VSyncInterval := 300000;
  VCommitsCountToSync := 1000;
  VPoolSize := 32;
  VPoolObjectTTL := 60000;
  VDatabasePageSize := 1024;
  VOnDeadLockRetryCount := 3;
  VIsFullVerboseMode := False;
  if Assigned(AStorageConfigData) then begin
    VBerkeleyDBConfigData := AStorageConfigData.GetSubItem('BerkeleyDB');
    if Assigned(VBerkeleyDBConfigData) then begin
      VSyncInterval := VBerkeleyDBConfigData.ReadInteger('SyncInterval', VSyncInterval);
      VCommitsCountToSync := VBerkeleyDBConfigData.ReadInteger('CommitsCountToSync', VCommitsCountToSync);
      VPoolSize := VBerkeleyDBConfigData.ReadInteger('PoolSize', VPoolSize);
      VPoolObjectTTL := VBerkeleyDBConfigData.ReadInteger('PoolObjectTTL', VPoolObjectTTL);
      VDatabasePageSize := VBerkeleyDBConfigData.ReadInteger('DatabasePageSize', VDatabasePageSize);
      VOnDeadLockRetryCount := VBerkeleyDBConfigData.ReadInteger('OnDeadLockRetryCount', VOnDeadLockRetryCount);
      VIsFullVerboseMode := VBerkeleyDBConfigData.ReadBool('IsFullVerboseMode', VIsFullVerboseMode);
    end;
  end;
  Result :=
    TTileStorageBerkeleyDBConfigStatic.Create(
      VSyncInterval,
      VCommitsCountToSync,
      VPoolSize,
      VPoolObjectTTL,
      VDatabasePageSize,
      VOnDeadLockRetryCount,
      VIsFullVerboseMode
    );
end;

function TTileStorageTypeBerkeleyDB.BuildStorageInternal(
  const AStorageConfigData: IConfigDataProvider;
  const AForceAbilities: ITileStorageAbilities;
  const AProjectionSet: IProjectionSet;
  const AMainContentType: IContentTypeInfoBasic;
  const ATileNotifier: INotifierTilePyramidUpdateInternal;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
var
  VConfig: ITileStorageBerkeleyDBConfigStatic;
begin
  VConfig := ReadConfig(AStorageConfigData);
  Result :=
    TTileStorageBerkeleyDB.Create(
      GetAbilities,
      AForceAbilities,
      FGlobalBerkeleyDBHelper,
      AProjectionSet,
      ATileNotifier,
      APath,
      VConfig,
      FIsVersioned,
      FGCNotifier,
      ACacheTileInfo,
      FContentTypeManager,
      GetMapVersionFactory,
      AMainContentType
    );
end;

end.

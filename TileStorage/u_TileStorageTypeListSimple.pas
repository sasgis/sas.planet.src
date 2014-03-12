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

unit u_TileStorageTypeListSimple;

interface

uses
  i_NotifierTime,
  i_ContentTypeManager,
  i_GlobalBerkeleyDBHelper,
  i_GlobalCacheConfig,
  i_MapVersionFactoryList,
  i_TileStorageTypeConfig,
  i_TileStorageType,
  i_TileStorageTypeListItem,
  u_TileStorageTypeList;

type
  TTileStorageTypeListSimple = class(TTileStorageTypeList)
  public
    constructor Create(
      const AMapVersionFactoryList: IMapVersionFactoryList;
      const AContentTypeManager: IContentTypeManager;
      const AGlobalCacheConfig: IGlobalCacheConfig;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AGCNotifier: INotifierTime
    );
  end;

implementation

uses
  c_CacheTypeCodes, // for default path
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_TileFileNameSAS,
  u_TileFileNameGMV,
  u_TileFileNameES,
  u_TileFileNameGM1,
  u_TileFileNameGM2,
  u_TileStorageTypeConfig,
  u_TileStorageTypeGE,
  u_TileStorageTypeGoogleEarth,
  u_TileStorageTypeBerkeleyDB,
  u_TileStorageTypeDBMS,
  u_TileStorageTypeInRAM,
  u_TileStorageTypeFileSystemSimple,
  u_TileStorageTypeListItem;

const
  CTileStorageTypeGE: TGUID = '{71C83BAA-EEA0-45E1-833E-8CCC3A8D1A1A}';
  CTileStorageTypeGETerrain: TGUID = '{C38B1837-A0E1-4139-89A3-3AB37C7ED702}';
  CTileStorageTypeGC: TGUID = '{F3163512-A190-426B-9D18-881AAD9DE61C}';
  CTileStorageTypeBerkeleyDB: TGUID = '{3DBF81CD-9356-40EB-9778-DE4D98E5BE61}';
  CTileStorageTypeBerkeleyDBVersioned: TGUID = '{CA3868AE-6762-4D17-B72F-6892E61E119B}';
  CTileStorageTypeDBMS: TGUID = '{5F9E2D54-A433-4853-B7EB-3EE218160263}';
  CTileStorageTypeFileSystemSAS: TGUID = '{BE87ACAB-7031-4F57-9C1D-FA62C709818F}';
  CTileStorageTypeFileSystemGMV: TGUID = '{CB20D66C-FC79-4D1C-93A9-1C41A8D6B002}';
  CTileStorageTypeFileSystemES: TGUID = '{F6056405-C25C-4573-AFAC-BC4F8DF52283}';
  CTileStorageTypeFileSystemGM1: TGUID = '{E6F98BC5-8684-42C9-92DE-3D994DA8C925}';
  CTileStorageTypeFileSystemGM2: TGUID = '{4EF99AD6-D05E-4175-805C-DBBE08AC43B3}';
  CTileStorageTypeInRAM: TGUID = '{717034B7-B49E-4C89-BC75-002D0523E548}';

{ TTileStorageTypeListSimple }

constructor TTileStorageTypeListSimple.Create(
  const AMapVersionFactoryList: IMapVersionFactoryList;
  const AContentTypeManager: IContentTypeManager;
  const AGlobalCacheConfig: IGlobalCacheConfig;
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AGCNotifier: INotifierTime
);
var
  VItem: ITileStorageTypeListItem;
  VStorageTypeConfig: ITileStorageTypeConfig;
  VStorageType: ITileStorageType;
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.NewCPath);
  VStorageType :=
    TTileStorageTypeFileSystemSimple.Create(
      TTileFileNameSAS.Create,
      TTileFileNameSAS.Create,
      AMapVersionFactoryList.GetSimpleVersionFactory,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeFileSystemSAS,
      c_File_Cache_Id_SAS,
      'Files SAS.Planet',
      VStorageType,
      True,
      True
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.OldCPath);
  VStorageType :=
    TTileStorageTypeFileSystemSimple.Create(
      TTileFileNameGMV.Create,
      TTileFileNameGMV.Create,
      AMapVersionFactoryList.GetSimpleVersionFactory,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeFileSystemGMV,
      c_File_Cache_Id_GMV,
      'Files GMV',
      VStorageType,
      True,
      True
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.ESCPath);
  VStorageType :=
    TTileStorageTypeFileSystemSimple.Create(
      TTileFileNameES.Create,
      TTileFileNameES.Create,
      AMapVersionFactoryList.GetSimpleVersionFactory,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeFileSystemES,
      c_File_Cache_Id_ES,
      'Files ES',
      VStorageType,
      True,
      True
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GMTilesPath);
  VStorageType :=
    TTileStorageTypeFileSystemSimple.Create(
      TTileFileNameGM1.Create,
      TTileFileNameGM1.Create,
      AMapVersionFactoryList.GetSimpleVersionFactory,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeFileSystemGM1,
      c_File_Cache_Id_GM,
      'Files GM',
      VStorageType,
      True,
      True
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GMTilesPath);
  VStorageType :=
    TTileStorageTypeFileSystemSimple.Create(
      TTileFileNameGM2.Create,
      TTileFileNameGM2.Create,
      AMapVersionFactoryList.GetSimpleVersionFactory,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeFileSystemGM2,
      c_File_Cache_Id_GM_Aux,
      'Files GM aux',
      VStorageType,
      True,
      False
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GCCachePath);
  VStorageType :=
    TTileStorageTypeGC.Create(
      AContentTypeManager,
      AMapVersionFactoryList.GetGEVersionFactory,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeGC,
      c_File_Cache_Id_GC,
      'GC cache',
      VStorageType,
      False,
      False
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.BDBCachePath);
  VStorageType :=
    TTileStorageTypeBerkeleyDB.Create(
      AGlobalBerkeleyDBHelper,
      AGCNotifier,
      False, // IsVersioned
      AContentTypeManager,
      AMapVersionFactoryList.GetSimpleVersionFactory,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeBerkeleyDB,
      c_File_Cache_Id_BDB,
      'Berkeley DB',
      VStorageType,
      True,
      True
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.BDBVerCachePath);
  VStorageType :=
    TTileStorageTypeBerkeleyDB.Create(
      AGlobalBerkeleyDBHelper,
      AGCNotifier,
      True, // IsVersioned
      AContentTypeManager,
      AMapVersionFactoryList.GetSimpleVersionFactory,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeBerkeleyDB,
      c_File_Cache_Id_BDB_Versioned,
      'Berkeley DB (Versioned)',
      VStorageType,
      True,
      True
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GECachePath);
  VStorageType :=
    TTileStorageTypeGoogleEarth.Create(
      AMapVersionFactoryList.GetSimpleVersionFactory,
      False,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeGE,
      c_File_Cache_Id_GE,
      'Google Earth Cache (Read Only)',
      VStorageType,
      False,
      False
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GECachePath);
  VStorageType :=
    TTileStorageTypeGoogleEarth.Create(
      AMapVersionFactoryList.GetSimpleVersionFactory,
      True,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeGETerrain,
      c_File_Cache_Id_GEt,
      'Google Earth Cache Terrain (Read Only)',
      VStorageType,
      False,
      False
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.DBMSCachePath);
  VStorageType :=
    TTileStorageTypeDBMS.Create(
      AGCNotifier,
      AContentTypeManager,
      AMapVersionFactoryList.GetSimpleVersionFactory,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeDBMS,
      c_File_Cache_Id_DBMS,
      'DBMS',
      VStorageType,
      True,
      False
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(nil);
  VStorageType :=
    TTileStorageTypeInRAM.Create(
      AMapVersionFactoryList.GetSimpleVersionFactory,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeInRAM,
      c_File_Cache_Id_RAM,
      'RAM',
      VStorageType,
      True,
      True
    );
  VList.Add(VItem);
  inherited Create(VList.MakeStaticAndClear);
end;

end.

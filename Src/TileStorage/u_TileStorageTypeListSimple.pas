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
  i_InterfaceListSimple,
  i_ArchiveReadWriteFactory,
  i_TileFileNameGenerator,
  i_TileFileNameParser,
  i_TileStorageTypeConfig,
  i_TileStorageType,
  i_TileStorageTypeListItem,
  u_TileStorageTypeList;

type
  TTileStorageTypeListSimple = class(TTileStorageTypeList)
  private
    procedure AddFileSystemTileStorageType(
      const AContentTypeManager: IContentTypeManager;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AStorageTypeConfig: ITileStorageTypeConfig;
      const AFileSystemGUID: TGUID;
      const AArchiveTarGUID: TGUID;
      const ANumericId: Integer;
      const ABaseName: string;
      const ANameGenerator: ITileFileNameGenerator;
      const ATileNameParser: ITileFileNameParser;
      const AMapVersionFactoryList: IMapVersionFactoryList;
      const AList: IInterfaceListSimple
    );
  public
    constructor Create(
      const AMapVersionFactoryList: IMapVersionFactoryList;
      const AContentTypeManager: IContentTypeManager;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AGlobalCacheConfig: IGlobalCacheConfig;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AGCNotifier: INotifierTime
    );
  end;

implementation

uses
  c_CacheTypeCodes,
  u_InterfaceListSimple,
  u_TileFileNameSAS,
  u_TileFileNameGMV,
  u_TileFileNameES,
  u_TileFileNameGM1,
  u_TileFileNameGM2,
  u_TileFileNameGM3,
  u_TileFileNameMOBAC,
  u_TileFileNameOsmAnd,
  u_TileFileNameTMS,
  u_TileStorageTypeArchiveTar,
  u_TileStorageTypeConfig,
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
  CTileStorageTypeFileSystemGM3: TGUID = '{A65E31AC-7561-47FA-87B6-CE7F5603D5D1}';
  CTileStorageTypeFileSystemMA: TGUID = '{033B64B5-008B-4BAF-9EA9-B8176EA35433}';
  CTileStorageTypeFileSystemOsmAnd: TGUID = '{A12465C1-2DB9-4309-8B48-A01E3BBDDE44}';
  CTileStorageTypeFileSystemTMS: TGUID = '{55510DF4-7FAD-4476-93AD-454F8689A109}';

  CTileStorageTypeArchiveTarSAS: TGUID = '{BD540FB0-6518-410E-8C39-3C24D1F69C22}';
  CTileStorageTypeArchiveTarGMV: TGUID = '{DE7D9EAC-FCE0-40A6-9CE6-CDEAB0FFAE28}';
  CTileStorageTypeArchiveTarES: TGUID = '{CDFA5927-D3CD-483A-9E4A-6BF1AA67FDD0}';
  CTileStorageTypeArchiveTarGM1: TGUID = '{6DCAE4E5-5878-4CA1-A789-5E62A499254F}';
  CTileStorageTypeArchiveTarGM2: TGUID = '{AA85021B-77FB-458B-BBE6-B11B7692C82F}';
  CTileStorageTypeArchiveTarGM3: TGUID = '{5C096A4C-FFA8-416F-AA14-DAC9C4A27D6B}';
  CTileStorageTypeArchiveTarMA: TGUID = '{11442EE8-28B7-419B-A402-3ACC56581204}';
  CTileStorageTypeArchiveTarOsmAnd: TGUID = '{5316452B-5B46-44CA-BF5F-D298240B350F}';
  CTileStorageTypeArchiveTarTMS: TGUID = '{4D8DE501-B764-4E24-BFB7-C108665849BE}';

  CTileStorageTypeInRAM: TGUID = '{717034B7-B49E-4C89-BC75-002D0523E548}';

resourcestring
  rsSASPlanetCacheName = 'SAS.Planet';
  rsGoogleMVCacheName = 'GoogleMV';
  rsEarthSlicerCacheName = 'EarthSlicer 1.95';
  rsGlobalMapperCacheName = 'GlobalMapper Tiles';
  rsGlobalMapperAuxCacheName = 'GlobalMapper Aux';
  rsGlobalMapperBingCacheName = 'GlobalMapper Bing';
  rsOsmAndCacheName = 'OsmAnd+ Tiles';
  rsTMSCacheName = 'Tile Map Service (TMS)';
  rsGoogleEarthCacheName = 'GoogleEarth';
  rsGoogleEarthTerrainCacheName = 'GoogleEarth Terrain';
  rsGeoCacherCacheName = 'GeoCacher';
  rsBerkeleyDBCacheName = 'BerkeleyDB';
  rsBerkeleyDBVersionedCacheName = 'BerkeleyDB (Versioned)';
  rsMobileAtlasCacheName = 'Mobile Atlas Creator (MOBAC)';
  rsDBMSCacheName = 'DBMS';
  rsRAMCacheName = 'RAM';

{ TTileStorageTypeListSimple }

constructor TTileStorageTypeListSimple.Create(
  const AMapVersionFactoryList: IMapVersionFactoryList;
  const AContentTypeManager: IContentTypeManager;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
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

  VStorageTypeConfig :=
    TTileStorageTypeConfig.Create(
      AGlobalCacheConfig.NewCPath
    );
  AddFileSystemTileStorageType(
    AContentTypeManager,
    AArchiveReadWriteFactory,
    VStorageTypeConfig,
    CTileStorageTypeFileSystemSAS,
    CTileStorageTypeArchiveTarSAS,
    c_File_Cache_Id_SAS,
    rsSASPlanetCacheName,
    TTileFileNameSAS.Create,
    TTileFileNameSAS.Create,
    AMapVersionFactoryList,
    VList
  );

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.OldCPath);
  AddFileSystemTileStorageType(
    AContentTypeManager,
    AArchiveReadWriteFactory,
    VStorageTypeConfig,
    CTileStorageTypeFileSystemGMV,
    CTileStorageTypeArchiveTarGMV,
    c_File_Cache_Id_GMV,
    rsGoogleMVCacheName,
    TTileFileNameGMV.Create,
    TTileFileNameGMV.Create,
    AMapVersionFactoryList,
    VList
  );

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.ESCPath);
  AddFileSystemTileStorageType(
    AContentTypeManager,
    AArchiveReadWriteFactory,
    VStorageTypeConfig,
    CTileStorageTypeFileSystemES,
    CTileStorageTypeArchiveTarES,
    c_File_Cache_Id_ES,
    rsEarthSlicerCacheName,
    TTileFileNameES.Create,
    TTileFileNameES.Create,
    AMapVersionFactoryList,
    VList
  );

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GMTilesPath);
  AddFileSystemTileStorageType(
    AContentTypeManager,
    AArchiveReadWriteFactory,
    VStorageTypeConfig,
    CTileStorageTypeFileSystemGM1,
    CTileStorageTypeArchiveTarGM1,
    c_File_Cache_Id_GM,
    rsGlobalMapperCacheName,
    TTileFileNameGM1.Create,
    TTileFileNameGM1.Create,
    AMapVersionFactoryList,
    VList
  );

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GMTilesPath);
  AddFileSystemTileStorageType(
    AContentTypeManager,
    AArchiveReadWriteFactory,
    VStorageTypeConfig,
    CTileStorageTypeFileSystemGM2,
    CTileStorageTypeArchiveTarGM2,
    c_File_Cache_Id_GM_Aux,
    rsGlobalMapperAuxCacheName,
    TTileFileNameGM2.Create,
    TTileFileNameGM2.Create,
    AMapVersionFactoryList,
    VList
  );

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GMTilesPath);
  AddFileSystemTileStorageType(
    AContentTypeManager,
    AArchiveReadWriteFactory,
    VStorageTypeConfig,
    CTileStorageTypeFileSystemGM3,
    CTileStorageTypeArchiveTarGM3,
    c_File_Cache_Id_GM_Bing,
    rsGlobalMapperBingCacheName,
    TTileFileNameGM3.Create,
    TTileFileNameGM3.Create,
    AMapVersionFactoryList,
    VList
  );

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GMTilesPath);
  AddFileSystemTileStorageType(
    AContentTypeManager,
    AArchiveReadWriteFactory,
    VStorageTypeConfig,
    CTileStorageTypeFileSystemOsmAnd,
    CTileStorageTypeArchiveTarOsmAnd,
    c_File_Cache_Id_OsmAnd,
    rsOsmAndCacheName,
    TTileFileNameOsmAnd.Create,
    TTileFileNameOsmAnd.Create,
    AMapVersionFactoryList,
    VList
  );

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.MOBACTilesPath);
  AddFileSystemTileStorageType(
    AContentTypeManager,
    AArchiveReadWriteFactory,
    VStorageTypeConfig,
    CTileStorageTypeFileSystemMA,
    CTileStorageTypeArchiveTarMA,
    c_File_Cache_Id_MOBAC,
    rsMobileAtlasCacheName,
    TTileFileNameMOBAC.Create,
    TTileFileNameMOBAC.Create,
    AMapVersionFactoryList,
    VList
  );

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.TMSTilesPath);
  AddFileSystemTileStorageType(
    AContentTypeManager,
    AArchiveReadWriteFactory,
    VStorageTypeConfig,
    CTileStorageTypeFileSystemTMS,
    CTileStorageTypeArchiveTarTMS,
    c_File_Cache_Id_TMS,
    rsTMSCacheName,
    TTileFileNameTMS.Create,
    TTileFileNameTMS.Create,
    AMapVersionFactoryList,
    VList
  );

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
      rsBerkeleyDBCacheName,
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
      rsBerkeleyDBVersionedCacheName,
      VStorageType,
      True,
      True
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GCCachePath);
  VStorageType :=
    TTileStorageTypeGoogleEarth.Create(
      AMapVersionFactoryList.GetSimpleVersionFactory,
      False, // IsTerrain
      True, // IsGeoCacher
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeGC,
      c_File_Cache_Id_GC,
      rsGeoCacherCacheName,
      VStorageType,
      False,
      False
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GECachePath);
  VStorageType :=
    TTileStorageTypeGoogleEarth.Create(
      AMapVersionFactoryList.GetSimpleVersionFactory,
      False, // IsTerrain
      False, // IsGeoCacher
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeGE,
      c_File_Cache_Id_GE,
      rsGoogleEarthCacheName,
      VStorageType,
      False,
      False
    );
  VList.Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(AGlobalCacheConfig.GECachePath);
  VStorageType :=
    TTileStorageTypeGoogleEarth.Create(
      AMapVersionFactoryList.GetSimpleVersionFactory,
      True, // IsTerrain
      False, // IsGeoCacher
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeGETerrain,
      c_File_Cache_Id_GEt,
      rsGoogleEarthTerrainCacheName,
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
      rsDBMSCacheName,
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
      rsRAMCacheName,
      VStorageType,
      True,
      True
    );
  VList.Add(VItem);
  inherited Create(VList.MakeStaticAndClear);
end;

procedure TTileStorageTypeListSimple.AddFileSystemTileStorageType(
  const AContentTypeManager: IContentTypeManager;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AStorageTypeConfig: ITileStorageTypeConfig;
  const AFileSystemGUID: TGUID;
  const AArchiveTarGUID: TGUID;
  const ANumericId: Integer;
  const ABaseName: string;
  const ANameGenerator: ITileFileNameGenerator;
  const ATileNameParser: ITileFileNameParser;
  const AMapVersionFactoryList: IMapVersionFactoryList;
  const AList: IInterfaceListSimple
);
var
  VStorageType: ITileStorageType;
  VItem: ITileStorageTypeListItem;
begin
  VStorageType :=
    TTileStorageTypeFileSystemSimple.Create(
      ANameGenerator,
      ATileNameParser,
      AMapVersionFactoryList.GetSimpleVersionFactory,
      AStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      AFileSystemGUID,
      ANumericId,
      ABaseName,
      VStorageType,
      True,
      True
    );
  AList.Add(VItem);
  VStorageType :=
    TTileStorageTypeArchiveTar.Create(
      AContentTypeManager,
      AArchiveReadWriteFactory,
      ANameGenerator,
      ATileNameParser,
      AMapVersionFactoryList.GetSimpleVersionFactory,
      AStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      AArchiveTarGUID,
      ANumericId + 100,
      'Tar ' + ABaseName,
      VStorageType,
      False,
      True
    );
  AList.Add(VItem);
end;

end.

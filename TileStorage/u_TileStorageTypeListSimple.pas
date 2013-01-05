{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_TileStorageTypeListSimple;

interface

uses
  i_PathConfig,
  i_NotifierTTLCheck,
  i_ContentTypeManager,
  i_GlobalBerkeleyDBHelper,
  i_SimpleTileStorageConfig,
  i_TileStorageTypeConfig,
  i_TileStorageType,
  i_TileStorageTypeListItem,
  u_TileStorageTypeList;

type
  TTileStorageTypeListSimple = class(TTileStorageTypeList)
  public
    constructor Create(
      const AConfig: ISimpleTileStorageConfigStatic;
      const AContentTypeManager: IContentTypeManager;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AGCNotifier: INotifierTime;
      const ABasePath: IPathConfig
    );
  end;

implementation

uses
  c_CacheTypeCodes, // for default path
  u_TileFileNameSAS,
  u_TileFileNameGMV,
  u_TileFileNameES,
  u_TileFileNameGM1,
  u_TileFileNameGM2,
  u_TileStorageTypeConfig,
  u_TileStorageTypeGE,
  u_TileStorageTypeBerkeleyDB,
  u_TileStorageTypeDBMS,
  u_TileStorageTypeInRAM,
  u_TileStorageTypeFileSystemSimple,
  u_TileStorageTypeListItem;

const
  CTileStorageTypeGE: TGUID = '{71C83BAA-EEA0-45E1-833E-8CCC3A8D1A1A}';
  CTileStorageTypeGC: TGUID = '{F3163512-A190-426B-9D18-881AAD9DE61C}';
  CTileStorageTypeBerkeleyDB: TGUID = '{3DBF81CD-9356-40EB-9778-DE4D98E5BE61}';
  CTileStorageTypeDBMS: TGUID = '{5F9E2D54-A433-4853-B7EB-3EE218160263}';
  CTileStorageTypeFileSystemSAS: TGUID = '{BE87ACAB-7031-4F57-9C1D-FA62C709818F}';
  CTileStorageTypeFileSystemGMV: TGUID = '{CB20D66C-FC79-4D1C-93A9-1C41A8D6B002}';
  CTileStorageTypeFileSystemES: TGUID = '{F6056405-C25C-4573-AFAC-BC4F8DF52283}';
  CTileStorageTypeFileSystemGM1: TGUID = '{E6F98BC5-8684-42C9-92DE-3D994DA8C925}';
  CTileStorageTypeFileSystemGM2: TGUID = '{4EF99AD6-D05E-4175-805C-DBBE08AC43B3}';
  CTileStorageTypeInRAM: TGUID = '{717034B7-B49E-4C89-BC75-002D0523E548}';

{ TTileStorageTypeListSimple }

constructor TTileStorageTypeListSimple.Create(
  const AConfig: ISimpleTileStorageConfigStatic;
  const AContentTypeManager: IContentTypeManager;
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AGCNotifier: INotifierTime;
  const ABasePath: IPathConfig
);
var
  VItem: ITileStorageTypeListItem;
  VStorageTypeConfig: ITileStorageTypeConfig;
  VStorageType: ITileStorageType;
begin
  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, c_File_Cache_Default_SAS);
  VStorageType :=
    TTileStorageTypeFileSystemSimple.Create(
      TTileFileNameSAS.Create,
      TTileFileNameSAS.Create,
      VStorageTypeConfig
    );
  VItem :=
    TTileStorageTypeListItem.Create(
      CTileStorageTypeFileSystemSAS,
      'Files SAS.Planet',
      VStorageType,
      True
    );
  inherited Create(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, c_File_Cache_Default_GMV);
  VStorageType := TTileStorageTypeFileSystemSimple.Create(
    TTileFileNameGMV.Create,
    TTileFileNameGMV.Create,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    CTileStorageTypeFileSystemGMV,
    'Files GMV',
    VStorageType,
    True
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, c_File_Cache_Default_ES);
  VStorageType := TTileStorageTypeFileSystemSimple.Create(
    TTileFileNameES.Create,
    TTileFileNameES.Create,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    CTileStorageTypeFileSystemES,
    'Files ES',
    VStorageType,
    True
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, c_File_Cache_Default_GM);
  VStorageType := TTileStorageTypeFileSystemSimple.Create(
    TTileFileNameGM1.Create,
    TTileFileNameGM1.Create,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    CTileStorageTypeFileSystemGM1,
    'Files GM',
    VStorageType,
    True
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, c_File_Cache_Default_GM);
  VStorageType := TTileStorageTypeFileSystemSimple.Create(
    TTileFileNameGM2.Create,
    TTileFileNameGM2.Create,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    CTileStorageTypeFileSystemGM2,
    'Files GM aux',
    VStorageType,
    False
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, c_File_Cache_Default_GE);
  VStorageType := TTileStorageTypeGE.Create(
    AContentTypeManager,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    CTileStorageTypeGE,
    'GE cache',
    VStorageType,
    False
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, c_File_Cache_Default_GC);
  VStorageType := TTileStorageTypeGE.Create(
    AContentTypeManager,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    CTileStorageTypeGC,
    'GC cache',
    VStorageType,
    False
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, c_File_Cache_Default_BDB);
  VStorageType := TTileStorageTypeBerkeleyDB.Create(
    AGlobalBerkeleyDBHelper,
    AGCNotifier,
    AContentTypeManager,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    CTileStorageTypeBerkeleyDB,
    'Berkeley DB',
    VStorageType,
    False
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, c_File_Cache_Default_DBMS);
  VStorageType := TTileStorageTypeDBMS.Create(
    AGCNotifier,
    AContentTypeManager,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    CTileStorageTypeDBMS,
    'DBMS',
    VStorageType,
    False
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, c_File_Cache_Default_RAM);
  VStorageType := TTileStorageTypeInRAM.Create(VStorageTypeConfig);
  VItem := TTileStorageTypeListItem.Create(
    CTileStorageTypeInRAM,
    'RAM',
    VStorageType,
    True
  );
  Add(VItem);
end;

end.

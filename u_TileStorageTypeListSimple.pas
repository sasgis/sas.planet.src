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
  i_TileStorageTypeConfig,
  i_TileStorageType,
  i_TileStorageTypeListItem,
  u_TileStorageTypeList;

type
  TTileStorageTypeListSimple = class(TTileStorageTypeList)
  public
    constructor Create(const ABasePath: IPathConfig);
  end;

implementation

uses
  u_TileFileNameSAS,
  u_TileFileNameGMV,
  u_TileFileNameES,
  u_TileFileNameGM1,
  u_TileFileNameGM2,
  u_TileStorageTypeConfig,
  u_TileStorageTypeGE,
  u_TileStorageTypeBerkeleyDB,
  u_TileStorageTypeDBMS,
  u_TileStorageTypeFileSystemSimple,
  u_TileStorageTypeListItem;

const
  CTileStorageTypeGE: TGUID = '{71C83BAA-EEA0-45E1-833E-8CCC3A8D1A1A}';
  CTileStorageTypeBerkeleyDB: TGUID = '{3DBF81CD-9356-40EB-9778-DE4D98E5BE61}';
  CTileStorageTypeFileSystemSAS: TGUID = '{BE87ACAB-7031-4F57-9C1D-FA62C709818F}';
  CTileStorageTypeFileSystemGMV: TGUID = '{CB20D66C-FC79-4D1C-93A9-1C41A8D6B002}';
  CTileStorageTypeFileSystemES: TGUID = '{F6056405-C25C-4573-AFAC-BC4F8DF52283}';
  CTileStorageTypeFileSystemGM1: TGUID = '{E6F98BC5-8684-42C9-92DE-3D994DA8C925}';
  CTileStorageTypeFileSystemGM2: TGUID = '{4EF99AD6-D05E-4175-805C-DBBE08AC43B3}';


{ TTileStorageTypeListSimple }

constructor TTileStorageTypeListSimple.Create(const ABasePath: IPathConfig);
var
  VItem: ITileStorageTypeListItem;
  VStorageTypeConfig: ITileStorageTypeConfig;
  VStorageType: ITileStorageType;
begin
  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, 'cache');
  VStorageType := TTileStorageTypeFileSystemSimple.Create(
    CTileStorageTypeFileSystemSAS,
    'Files SAS.Planet',
    TTileFileNameSAS.Create,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    VStorageType.GUID,
    VStorageType,
    True
  );
  inherited Create(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, 'cache_old');
  VStorageType := TTileStorageTypeFileSystemSimple.Create(
    CTileStorageTypeFileSystemGMV,
    'Files GMV',
    TTileFileNameGMV.Create,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    VStorageType.GUID,
    VStorageType,
    True
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, 'cache_ES');
  VStorageType := TTileStorageTypeFileSystemSimple.Create(
    CTileStorageTypeFileSystemES,
    'Files ES',
    TTileFileNameES.Create,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    VStorageType.GUID,
    VStorageType,
    True
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, 'cache_gmt');
  VStorageType := TTileStorageTypeFileSystemSimple.Create(
    CTileStorageTypeFileSystemGM1,
    'Files GM',
    TTileFileNameGM1.Create,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    VStorageType.GUID,
    VStorageType,
    True
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, 'cache_gmt');
  VStorageType := TTileStorageTypeFileSystemSimple.Create(
    CTileStorageTypeFileSystemGM2,
    'Files GM aux',
    TTileFileNameGM2.Create,
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    VStorageType.GUID,
    VStorageType,
    False
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, 'cache_GE');
  VStorageType := TTileStorageTypeGE.Create(
    CTileStorageTypeGE,
    'GE cache',
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    VStorageType.GUID,
    VStorageType,
    False
  );
  Add(VItem);

  VStorageTypeConfig := TTileStorageTypeConfig.Create(ABasePath, 'cache_db');
  VStorageType := TTileStorageTypeBerkeleyDB.Create(
    CTileStorageTypeBerkeleyDB,
    'Berkeley DB',
    VStorageTypeConfig
  );
  VItem := TTileStorageTypeListItem.Create(
    VStorageType.GUID,
    VStorageType,
    False
  );
  Add(VItem);
end;

end.

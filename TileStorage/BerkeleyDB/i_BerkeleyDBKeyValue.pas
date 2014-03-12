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

unit i_BerkeleyDBKeyValue;

interface

uses
  Types;

type
  // Бинарное представление key/value
  IBerkeleyDBKeyValueBase = interface
    ['{D3EDE8C5-CF68-428F-A63B-B815F363F021}']
    function Assign(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ): Boolean;

    function GetData: Pointer;
    property Data: Pointer read GetData;

    function GetSize: Integer;
    property Size: Integer read GetSize;
  end;

  // Неверсионный key (8 байт)
  IBerkeleyDBKey = interface(IBerkeleyDBKeyValueBase)
    ['{FD32450C-AC56-4B80-89C8-293C778A37C1}']
    function GetPoint: TPoint;
    property Point: TPoint read GetPoint;
  end;

  // Версионный key (10 байт)
  IBerkeleyDBVersionedKey = interface(IBerkeleyDBKey)
    ['{3A4C614E-F835-40F1-8471-339C3807A629}']
    function GetVersionID: Word;
    property VersionID: Word read GetVersionID;
  end;

  // Value для версионных и неверсионных ключей
  IBerkeleyDBValue = interface(IBerkeleyDBKeyValueBase)
    ['{47E2D6BD-598E-4C5A-892C-968117885975}']
    function GetTileBody: Pointer;
    property TileBody: Pointer read GetTileBody;

    function GetTileSize: Integer;
    property TileSize: Integer read GetTileSize;

    function GetTileDate: TDateTime;
    property TileDate: TDateTime read GetTileDate;

    function GetTileVersionInfo: WideString;
    property TileVersionInfo: WideString read GetTileVersionInfo;

    function GetTileContentType: WideString;
    property TileContentType: WideString read GetTileContentType;
  end;

  // Метаинформация о хранилище - одна запись на файл (*.sdb)
  IBerkeleyDBMetaKey = interface(IBerkeleyDBKey)
    ['{32DAFA1B-03DE-49AD-B66E-8FCBBBBD6B74}']
  end;

  IBerkeleyDBMetaValue = interface(IBerkeleyDBKeyValueBase)
    ['{F15DC7B4-8D53-4FA8-81A6-AC17CF0C5749}']
    function GetStorageEPSG: Integer;
    property StorageEPSG: Integer read GetStorageEPSG;
  end;

  // Метаинформация о версиях тайла - одна запись на тайл
  IBerkeleyDBVersionedMetaKey = interface(IBerkeleyDBVersionedKey)
    ['{B86D06BF-F013-4B38-8FD7-0C09005C080D}']
  end;

  IBerkeleyDBVersionedMetaValueElement = interface(IBerkeleyDBKeyValueBase)
    ['{9D8A8DBC-70E5-460F-B48F-AD8EE4D3F0DA}']
    function GetVersionID: Word;
    property VersionID: Word read GetVersionID;

    function GetTileZOrder: Word;
    property TileZOrder: Word read GetTileZOrder;

    function GetTileSize: Integer;
    property TileSize: Integer read GetTileSize;

    function GetTileDate: TDateTime;
    property TileDate: TDateTime read GetTileDate;

    function GetTileCRC: Cardinal;
    property TileCRC: Cardinal read GetTileCRC;

    function GetTileVersionInfo: WideString;
    property TileVersionInfo: WideString read GetTileVersionInfo;

    function GetTileContentType: WideString;
    property TileContentType: WideString read GetTileContentType;
  end;

  IBerkeleyDBVersionedMetaValue = interface(IBerkeleyDBKeyValueBase)
    ['{AC723A9F-2397-4D0E-B5F3-1E278F60ECC6}']
    function GetCount: Integer;
    property ItemsCount: Integer read GetCount;

    function GetItem(const AIndex: Integer): IBerkeleyDBVersionedMetaValueElement;
    property Item[const AIndex: Integer]: IBerkeleyDBVersionedMetaValueElement read GetItem;

    function Add(const AItem: IBerkeleyDBVersionedMetaValueElement): Integer;

    procedure Replace(const AIndex: Integer; const AItem: IBerkeleyDBVersionedMetaValueElement);

    procedure Del(const AIndex: Integer);
  end;

implementation

end.

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

unit i_BerkeleyDBKeyValue;

interface

uses
  Types;

type
  IBerkeleyDBKeyValueBase = interface
    ['{D3EDE8C5-CF68-428F-A63B-B815F363F021}']
    procedure Assign(const AData: Pointer; const ASize: Integer; const AOwnMem: Boolean);
    
    function GetData: Pointer;
    property Data: Pointer read GetData;

    function GetSize: Integer;
    property Size: Integer read GetSize;
  end;

  IBerkeleyDBKey = interface(IBerkeleyDBKeyValueBase)
    ['{FD32450C-AC56-4B80-89C8-293C778A37C1}']
    function GetPoint: TPoint;
    property Point: TPoint read GetPoint;
  end;

  IBerkeleyDBMetaValue = interface(IBerkeleyDBKeyValueBase)
    ['{F15DC7B4-8D53-4FA8-81A6-AC17CF0C5749}']
    function GetStorageEPSG: Integer;
    property StorageEPSG: Integer read GetStorageEPSG;
  end;

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

implementation

end.

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

unit i_SimpleTileStorageConfig;

interface

uses
  i_TileStorageAbilities,
  i_ConfigDataElement;

type
  ISimpleTileStorageConfigStatic = interface
    ['{594D2502-ACE4-4986-9459-DFD4B75C052D}']
    function GetCacheTypeCode: Integer;
    property CacheTypeCode: Integer read GetCacheTypeCode;

    function GetNameInCache: string;
    property NameInCache: string read GetNameInCache;

    function GetTileFileExt: string;
    property TileFileExt: string read GetTileFileExt;

    function GetAbilities: ITileStorageAbilities;
    property Abilities: ITileStorageAbilities read GetAbilities;

    function GetUseMemCache: Boolean;
    property UseMemCache: Boolean read GetUseMemCache;

    function GetMemCacheCapacity: Integer;
    property MemCacheCapacity: Integer read GetMemCacheCapacity;

    function GetMemCacheTTL: Cardinal;
    property MemCacheTTL: Cardinal read GetMemCacheTTL;

    function GetMemCacheClearStrategy: Integer;
    property MemCacheClearStrategy: Integer read GetMemCacheClearStrategy;
  end;

  ISimpleTileStorageConfig = interface(IConfigDataElement)
    ['{536CD168-FDBE-43F8-B090-E3B5FCE97ACE}']
    function GetCacheTypeCode: Integer;
    procedure SetCacheTypeCode(AValue: Integer);
    property CacheTypeCode: Integer read GetCacheTypeCode write SetCacheTypeCode;

    function GetNameInCache: string;
    procedure SetNameInCache(const AValue: string);
    property NameInCache: string read GetNameInCache write SetNameInCache;

    function GetTileFileExt: string;
    property TileFileExt: string read GetTileFileExt;

    function GetIsReadOnly: boolean;
    procedure SetIsReadOnly(AValue: Boolean);
    property IsReadOnly: Boolean read GetIsReadOnly write SetIsReadOnly;

    function GetAllowDelete: boolean;
    procedure SetAllowDelete(AValue: Boolean);
    property AllowDelete: Boolean read GetAllowDelete write SetAllowDelete;

    function GetAllowAdd: boolean;
    procedure SetAllowAdd(AValue: Boolean);
    property AllowAdd: Boolean read GetAllowAdd write SetAllowAdd;

    function GetAllowReplace: boolean;
    procedure SetAllowReplace(AValue: Boolean);
    property AllowReplace: Boolean read GetAllowReplace write SetAllowReplace;

    function GetUseMemCache: Boolean;
    procedure SetUseMemCache(const AValue: Boolean);
    property UseMemCache: Boolean read GetUseMemCache write SetUseMemCache;

    function GetMemCacheCapacity: Integer;
    procedure SetMemCacheCapacity(const AValue: Integer);
    property MemCacheCapacity: Integer read GetMemCacheCapacity write SetMemCacheCapacity;

    function GetMemCacheTTL: Cardinal;
    procedure SetMemCacheTTL(const AValue: Cardinal);
    property MemCacheTTL: Cardinal read GetMemCacheTTL write SetMemCacheTTL;

    function GetMemCacheClearStrategy: Integer;
    procedure SetMemCacheClearStrategy(const AValue: Integer);
    property MemCacheClearStrategy: Integer read GetMemCacheClearStrategy write SetMemCacheClearStrategy;

    function GetStatic: ISimpleTileStorageConfigStatic;
  end;

implementation

end.

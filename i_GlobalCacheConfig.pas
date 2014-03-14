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

unit i_GlobalCacheConfig;

interface

uses
  i_PathConfig,
  i_ConfigDataElement;

type
  IGlobalCacheConfig = interface(IConfigDataElement)
    ['{DAE1C885-4548-4DD5-AC19-57825DFBECF6}']
    //Способ храения кэша по-умолчанию.
    function GetDefCache: byte;
    procedure SetDefCache(const AValue: byte);
    property DefCache: byte read GetDefCache write SetDefCache;

    //Пути к кэшам разных типов
    function GetNewCPath: IPathConfig;
    property NewCPath: IPathConfig read GetNewCPath;

    function GetOldCPath: IPathConfig;
    property OldCPath: IPathConfig read GetOldCPath;

    function GetESCPath: IPathConfig;
    property ESCPath: IPathConfig read GetESCPath;

    function GetGMTilesPath: IPathConfig;
    property GMTilesPath: IPathConfig read GetGMTilesPath;

    function GetGECachePath: IPathConfig;
    property GECachePath: IPathConfig read GetGECachePath;

    function GetGCCachePath: IPathConfig;
    property GCCachePath: IPathConfig read GetGCCachePath;

    function GetBDBCachePath: IPathConfig;
    property BDBCachePath: IPathConfig read GetBDBCachePath;

    function GetBDBVerCachePath: IPathConfig;
    property BDBVerCachePath: IPathConfig read GetBDBVerCachePath;

    function GetDBMSCachePath: IPathConfig;
    property DBMSCachePath: IPathConfig read GetDBMSCachePath;
  end;

implementation

end.

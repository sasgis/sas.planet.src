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

unit i_TileObjCache;

interface

uses
  Types,
  Classes,
  i_Bitmap32Static,
  i_MapVersionInfo,
  i_VectorDataItemSimple;

type
  ITileObjCacheVector = interface
    ['{B52B38D1-C57C-424C-B85B-AC623A54E7B5}']
    procedure Clear;
    procedure DeleteTileFromCache(const AXY: TPoint; const AZoom: Byte;
                                  const AMapVersionInfo: IMapVersionInfo);
    procedure AddTileToCache(AObj: IVectorDataItemList; const AXY: TPoint; const AZoom: Byte;
                             const AMapVersionInfo: IMapVersionInfo);
    function TryLoadTileFromCache(const AXY: TPoint; const AZoom: Byte;
                                  const AMapVersionInfo: IMapVersionInfo): IVectorDataItemList;
  end;

  ITileObjCacheBitmap = interface
    ['{B52B38D1-C57C-424C-B85B-AC623A54E7B5}']
    procedure Clear;
    procedure DeleteTileFromCache(const AXY: TPoint; const AZoom: Byte;
                                  const AMapVersionInfo: IMapVersionInfo);
    procedure AddTileToCache(AObj: IBitmap32Static; const AXY: TPoint; const AZoom: Byte;
                             const AMapVersionInfo: IMapVersionInfo);
    function TryLoadTileFromCache(const AXY: TPoint; const AZoom: Byte;
                                  const AMapVersionInfo: IMapVersionInfo): IBitmap32Static;
  end;

  ITileObjCachePersistent = interface
    ['{7245DF11-BE36-4A50-B429-8AD2472B0FFB}']
    procedure Clear;
    procedure DeleteTileFromCache(const AXY: TPoint; const AZoom: Byte;
                                  const AMapVersionInfo: IMapVersionInfo);
    procedure AddTileToCache(var AObj: TPersistent; // will own this object and set NIL (if ok)
                             const AXY: TPoint; const AZoom: Byte;
                             const AMapVersionInfo: IMapVersionInfo);
    function TryLoadTileFromCache(AObj: TPersistent; // use Assign, if NIL - just check if exists in cache
                                  const AXY: TPoint; const AZoom: Byte;
                                  const AMapVersionInfo: IMapVersionInfo): Boolean;
  end;
  
implementation

end.

{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_TileCacheSimpleGlobal;

interface

uses
  GR32,
  i_VectorDataItemSimple,
  i_MemObjCache,
  i_TileObjCache;

type
  TTileCacheSimpleGlobalVector = class(TInterfacedObject, ITileObjCacheVector)
  private
    FGUID: TGUID;
    FGUIDString: String;
    FMemCache: IMemObjCacheVector;
    function GetMemCacheKey(AXY: TPoint; Azoom: byte): string;
  protected
    procedure Clear;
    procedure DeleteTileFromCache(AXY: TPoint; AZoom: Byte);
    procedure AddTileToCache(AObj: IVectorDataItemList; AXY: TPoint; AZoom: Byte);
    function TryLoadTileFromCache(var AObj: IVectorDataItemList; AXY: TPoint; AZoom: Byte): boolean;
  public
    constructor Create(AGUID: TGUID; AMemCache: IMemObjCacheVector);
  end;

  TTileCacheSimpleGlobalBitmap = class(TInterfacedObject, ITileObjCacheBitmap)
  private
    FGUID: TGUID;
    FGUIDString: String;
    FMemCache: IMemObjCacheBitmap;
    function GetMemCacheKey(AXY: TPoint; Azoom: byte): string;
    function GetMemCachePreKey(AXY: TPoint; Azoom: byte): string;
  protected
    procedure Clear;
    procedure DeleteTileFromCache(AXY: TPoint; AZoom: Byte);
    procedure AddTileToCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte);
    function TryLoadTileFromCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean;
    procedure AddTilePreToCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte);
    function TryLoadTilePreFromCache(AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean;
  public
    constructor Create(AGUID: TGUID; AMemCache: IMemObjCacheBitmap);
  end;


implementation

uses
  SysUtils,
  ComObj;

{ TTileCacheSimpleGlobal }

procedure TTileCacheSimpleGlobalVector.AddTileToCache(AObj: IVectorDataItemList;
  AXY: TPoint; AZoom: Byte);
begin
  FMemCache.AddTileToCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

procedure TTileCacheSimpleGlobalVector.Clear;
begin
  FMemCache.Clear;
end;

constructor TTileCacheSimpleGlobalVector.Create(AGUID: TGUID; AMemCache:
    IMemObjCacheVector);
begin
  FGUID := AGUID;
  FGUIDString := GUIDToString(FGUID);
  FMemCache := AMemCache;
end;

procedure TTileCacheSimpleGlobalVector.DeleteTileFromCache(AXY: TPoint;
  AZoom: Byte);
begin
  FMemCache.DeleteFileFromCache(GetMemCacheKey(AXY, AZoom));
end;

function TTileCacheSimpleGlobalVector.GetMemCacheKey(AXY: TPoint;
  Azoom: byte): string;
begin
  Result := inttostr(Azoom) + '-' + inttostr(AXY.X) + '-' + inttostr(AXY.Y) + '-' + FGUIDString;
end;

function TTileCacheSimpleGlobalVector.TryLoadTileFromCache(var AObj: IVectorDataItemList;
  AXY: TPoint; AZoom: Byte): boolean;
begin
  Result := FMemCache.TryLoadFileFromCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

{ TTileCacheSimpleGlobalBitmap }

procedure TTileCacheSimpleGlobalBitmap.AddTilePreToCache(AObj: TCustomBitmap32;
  AXY: TPoint; AZoom: Byte);
begin
  FMemCache.AddTileToCache(AObj, GetMemCachePreKey(AXY, AZoom));
end;

procedure TTileCacheSimpleGlobalBitmap.AddTileToCache(AObj: TCustomBitmap32;
  AXY: TPoint; AZoom: Byte);
begin
  FMemCache.AddTileToCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

procedure TTileCacheSimpleGlobalBitmap.Clear;
begin
  FMemCache.Clear;
end;

constructor TTileCacheSimpleGlobalBitmap.Create(AGUID: TGUID; AMemCache:
    IMemObjCacheBitmap);
begin
  FGUID := AGUID;
  FGUIDString := GUIDToString(FGUID);
  FMemCache := AMemCache;
end;

procedure TTileCacheSimpleGlobalBitmap.DeleteTileFromCache(AXY: TPoint;
  AZoom: Byte);
begin
  FMemCache.DeleteFileFromCache(GetMemCacheKey(AXY, AZoom));
end;

function TTileCacheSimpleGlobalBitmap.GetMemCacheKey(AXY: TPoint;
  Azoom: byte): string;
begin
  Result := inttostr(Azoom) + '-' + inttostr(AXY.X) + '-' + inttostr(AXY.Y) + '-' + FGUIDString;
end;

function TTileCacheSimpleGlobalBitmap.GetMemCachePreKey(AXY: TPoint;
  Azoom: byte): string;
begin
  Result := inttostr(Azoom) + '-' + inttostr(AXY.X) + '-' + inttostr(AXY.Y) + '-Pre-' + FGUIDString;
end;

function TTileCacheSimpleGlobalBitmap.TryLoadTileFromCache(
  AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean;
begin
  Result := FMemCache.TryLoadFileFromCache(AObj, GetMemCacheKey(AXY, AZoom));
end;

function TTileCacheSimpleGlobalBitmap.TryLoadTilePreFromCache(
  AObj: TCustomBitmap32; AXY: TPoint; AZoom: Byte): boolean;
begin
  Result := FMemCache.TryLoadFileFromCache(AObj, GetMemCachePreKey(AXY, AZoom));
end;

end.

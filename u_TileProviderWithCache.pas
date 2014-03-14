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

unit u_TileProviderWithCache;

interface

uses
  Types,
  i_NotifierTilePyramidUpdate,
  i_Bitmap32Static,
  i_VectorItemSubset,
  i_ProjectionInfo,
  i_TileProvider,
  i_TileObjCache,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderWithCache = class(TBaseInterfacedObject, IBitmapTileProviderWithNotifier)
  private
    FSource: IBitmapTileProviderWithNotifier;
    FCache: ITileObjCacheBitmap;
  private
    function GetProjectionInfo: IProjectionInfo;
    function GetTile(const ATile: TPoint): IBitmap32Static;
    function GetChangeNotifier: INotifierTilePyramidUpdate;
  public
    constructor Create(
      const ASource: IBitmapTileProviderWithNotifier;
      const ACache: ITileObjCacheBitmap
    );
  end;

  TVectorTileProviderWithCache = class(TBaseInterfacedObject, IVectorTileProviderWithNotifier)
  private
    FSource: IVectorTileProviderWithNotifier;
    FCache: ITileObjCacheVector;
  private
    function GetProjectionInfo: IProjectionInfo;
    function GetTile(const ATile: TPoint): IVectorItemSubset;
    function GetChangeNotifier: INotifierTilePyramidUpdate;
  public
    constructor Create(
      const ASource: IVectorTileProviderWithNotifier;
      const ACache: ITileObjCacheVector
    );
  end;

implementation

{ TBitmapTileProviderWithCache }

constructor TBitmapTileProviderWithCache.Create(
  const ASource: IBitmapTileProviderWithNotifier;
  const ACache: ITileObjCacheBitmap
);
begin
  Assert(ASource <> nil);
  Assert(ACache <> nil);
  inherited Create;
  FSource := ASource;
  FCache := ACache;
end;

function TBitmapTileProviderWithCache.GetChangeNotifier: INotifierTilePyramidUpdate;
begin
  Result := FSource.ChangeNotifier;
end;

function TBitmapTileProviderWithCache.GetProjectionInfo: IProjectionInfo;
begin
  Result := FSource.ProjectionInfo;
end;

function TBitmapTileProviderWithCache.GetTile(
  const ATile: TPoint
): IBitmap32Static;
var
  VZoom: Byte;
begin
  VZoom := FSource.ProjectionInfo.Zoom;
  Result := FCache.TryLoadTileFromCache(ATile, VZoom);
  if Result = nil then begin
    Result := FSource.GetTile(ATile);
    if Result <> nil then begin
      FCache.AddTileToCache(Result, ATile, VZoom);
    end;
  end;
end;

{ TVectorTileProviderWithCache }

constructor TVectorTileProviderWithCache.Create(
  const ASource: IVectorTileProviderWithNotifier;
  const ACache: ITileObjCacheVector
);
begin
  Assert(ASource <> nil);
  Assert(ACache <> nil);
  inherited Create;
  FSource := ASource;
  FCache := ACache;
end;

function TVectorTileProviderWithCache.GetChangeNotifier: INotifierTilePyramidUpdate;
begin
  Result := FSource.ChangeNotifier;
end;

function TVectorTileProviderWithCache.GetProjectionInfo: IProjectionInfo;
begin
  Result := FSource.ProjectionInfo;
end;

function TVectorTileProviderWithCache.GetTile(
  const ATile: TPoint
): IVectorItemSubset;
var
  VZoom: Byte;
begin
  VZoom := FSource.ProjectionInfo.Zoom;
  Result := FCache.TryLoadTileFromCache(ATile, VZoom);
  if Result = nil then begin
    Result := FSource.GetTile(ATile);
    if Result <> nil then begin
      FCache.AddTileToCache(Result, ATile, VZoom);
    end;
  end;
end;

end.

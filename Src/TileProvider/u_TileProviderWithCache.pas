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
  i_NotifierOperation,
  i_Bitmap32Static,
  i_VectorItemSubset,
  i_ProjectionInfo,
  i_BitmapTileProvider,
  i_VectorTileProvider,
  i_TileObjCache,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderWithCache = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FSource: IBitmapTileProvider;
    FCache: ITileObjCacheBitmap;
  private
    function GetProjectionInfo: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const ASource: IBitmapTileProvider;
      const ACache: ITileObjCacheBitmap
    );
  end;

  TVectorTileProviderWithCache = class(TBaseInterfacedObject, IVectorTileProvider)
  private
    FSource: IVectorTileProvider;
    FCache: ITileObjCacheVector;
  private
    function GetProjectionInfo: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IVectorItemSubset;
  public
    constructor Create(
      const ASource: IVectorTileProvider;
      const ACache: ITileObjCacheVector
    );
  end;

implementation

{ TBitmapTileProviderWithCache }

constructor TBitmapTileProviderWithCache.Create(
  const ASource: IBitmapTileProvider;
  const ACache: ITileObjCacheBitmap
);
begin
  Assert(ASource <> nil);
  Assert(ACache <> nil);
  inherited Create;
  FSource := ASource;
  FCache := ACache;
end;

function TBitmapTileProviderWithCache.GetProjectionInfo: IProjection;
begin
  Result := FSource.ProjectionInfo;
end;

function TBitmapTileProviderWithCache.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
var
  VZoom: Byte;
begin
  VZoom := FSource.ProjectionInfo.Zoom;
  Result := FCache.TryLoadTileFromCache(ATile, VZoom);
  if Result = nil then begin
    Result := FSource.GetTile(AOperationID, ACancelNotifier, ATile);
    if Result <> nil then begin
      FCache.AddTileToCache(Result, ATile, VZoom);
    end;
  end;
end;

{ TVectorTileProviderWithCache }

constructor TVectorTileProviderWithCache.Create(
  const ASource: IVectorTileProvider;
  const ACache: ITileObjCacheVector
);
begin
  Assert(ASource <> nil);
  Assert(ACache <> nil);
  inherited Create;
  FSource := ASource;
  FCache := ACache;
end;

function TVectorTileProviderWithCache.GetProjectionInfo: IProjection;
begin
  Result := FSource.ProjectionInfo;
end;

function TVectorTileProviderWithCache.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IVectorItemSubset;
var
  VZoom: Byte;
begin
  VZoom := FSource.ProjectionInfo.Zoom;
  Result := FCache.TryLoadTileFromCache(ATile, VZoom);
  if Result = nil then begin
    Result := FSource.GetTile(AOperationID, ACancelNotifier, ATile);
    if Result <> nil then begin
      FCache.AddTileToCache(Result, ATile, VZoom);
    end;
  end;
end;

end.

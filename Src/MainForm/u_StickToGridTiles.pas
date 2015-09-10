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

unit u_StickToGridTiles;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_MapLayerGridsConfig,
  i_StickToGrid,
  u_BaseInterfacedObject;

type
  TStickToGridTiles = class(TBaseInterfacedObject, IStickToGrid)
  private
    FConfig: ITileGridConfig;
    function GetActualZoom(const AProjection: IProjectionInfo): Byte;
  private
    function PointStick(
      const AProjection: IProjectionInfo;
      const ASourceLonLat: TDoublePoint
    ): TDoublePoint;
    function RectStick(
      const AProjection: IProjectionInfo;
      const ASourceRect: TDoubleRect
    ): TDoubleRect;
  public
    constructor Create(const AConfig: ITileGridConfig);
  end;

implementation

uses
  Types,
  Math,
  i_CoordConverter,
  u_GeoFunc;

{ TStickToGridTiles }

constructor TStickToGridTiles.Create(const AConfig: ITileGridConfig);
begin
  Assert(Assigned(AConfig));
  inherited Create;
  FConfig := AConfig;
end;

function TStickToGridTiles.GetActualZoom(
  const AProjection: IProjectionInfo
): Byte;
var
  VZoom: Integer;
  VRelative: Boolean;
begin
  FConfig.LockRead;
  try
    VZoom := FConfig.Zoom;
    VRelative := FConfig.UseRelativeZoom;
  finally
    FConfig.UnlockRead;
  end;
  if VRelative then begin
    VZoom := VZoom + AProjection.GetZoom;
  end;
  if VZoom < 0 then begin
    Result := 0;
  end else begin
    Result := VZoom;
    AProjection.GeoConverter.ValidateZoom(Result);
  end;
end;

function TStickToGridTiles.PointStick(
  const AProjection: IProjectionInfo;
  const ASourceLonLat: TDoublePoint
): TDoublePoint;
var
  VZoom: Byte;
  VSelectedTileFloat: TDoublePoint;
  VSelectedTile: TPoint;
  VConverter: ICoordConverter;
begin
  VConverter := AProjection.GeoConverter;
  VZoom := GetActualZoom(AProjection);
  VSelectedTileFloat := VConverter.LonLat2TilePosFloat(ASourceLonLat, VZoom);
  VSelectedTile := PointFromDoublePoint(VSelectedTileFloat, prClosest);
  Result := VConverter.TilePos2LonLat(VSelectedTile, VZoom);
end;

function TStickToGridTiles.RectStick(
  const AProjection: IProjectionInfo;
  const ASourceRect: TDoubleRect
): TDoubleRect;
var
  VZoom: Byte;
  VSelectedTilesFloat: TDoubleRect;
  VSelectedTiles: TRect;
  VConverter: ICoordConverter;
begin
  VConverter := AProjection.GeoConverter;
  VZoom := GetActualZoom(AProjection);
  VSelectedTilesFloat := VConverter.LonLatRect2TileRectFloat(ASourceRect, VZoom);
  VSelectedTiles := RectFromDoubleRect(VSelectedTilesFloat, rrOutside);
  Result := VConverter.TileRect2LonLatRect(VSelectedTiles, VZoom);
end;

end.

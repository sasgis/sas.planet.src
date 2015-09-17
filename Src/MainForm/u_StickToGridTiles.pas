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
  i_Projection,
  i_MapLayerGridsConfig,
  i_ProjectionSetChangeable,
  i_StickToGrid,
  u_BaseInterfacedObject;

type
  TStickToGridTiles = class(TBaseInterfacedObject, IStickToGrid)
  private
    FProjectionSet: IProjectionSetChangeable;
    FConfig: ITileGridConfig;
    function GetActualProjection(const AProjection: IProjection): IProjection;
  private
    function PointStick(
      const AProjection: IProjection;
      const ASourceLonLat: TDoublePoint
    ): TDoublePoint;
    function RectStick(
      const AProjection: IProjection;
      const ASourceRect: TDoubleRect
    ): TDoubleRect;
  public
    constructor Create(
      const AProjectionSet: IProjectionSetChangeable;
      const AConfig: ITileGridConfig
    );
  end;

implementation

uses
  Types,
  Math,
  i_ProjectionSet,
  u_GeoFunc;

{ TStickToGridTiles }

constructor TStickToGridTiles.Create(
  const AProjectionSet: IProjectionSetChangeable;
  const AConfig: ITileGridConfig
);
begin
  Assert(Assigned(AProjectionSet));
  Assert(Assigned(AConfig));
  inherited Create;
  FProjectionSet := AProjectionSet;
  FConfig := AConfig;
end;

function TStickToGridTiles.GetActualProjection(
  const AProjection: IProjection
): IProjection;
var
  VZoom: Integer;
  VRelative: Boolean;
  VProjectionSet: IProjectionSet;
  VProjection: IProjection;
  VResultZoom: Byte;
begin
  FConfig.LockRead;
  try
    VZoom := FConfig.Zoom;
    VRelative := FConfig.UseRelativeZoom;
  finally
    FConfig.UnlockRead;
  end;
  VProjectionSet := FProjectionSet.GetStatic;
  VProjection := VProjectionSet.GetSuitableProjection(AProjection);
  if VRelative then begin
    VZoom := VZoom + VProjection.Zoom;
  end;
  if VZoom < 0 then begin
    Result := VProjectionSet.Zooms[0];
  end else begin
    VResultZoom := VZoom;
    VProjectionSet.ValidateZoom(VResultZoom);
    Result := VProjectionSet.Zooms[VResultZoom];
  end;
end;

function TStickToGridTiles.PointStick(
  const AProjection: IProjection;
  const ASourceLonLat: TDoublePoint
): TDoublePoint;
var
  VProjection: IProjection;
  VSelectedTileFloat: TDoublePoint;
  VSelectedTile: TPoint;
begin
  VProjection := GetActualProjection(AProjection);
  VSelectedTileFloat := VProjection.LonLat2TilePosFloat(ASourceLonLat);
  VSelectedTile := PointFromDoublePoint(VSelectedTileFloat, prClosest);
  Result := VProjection.TilePos2LonLat(VSelectedTile);
end;

function TStickToGridTiles.RectStick(
  const AProjection: IProjection;
  const ASourceRect: TDoubleRect
): TDoubleRect;
var
  VProjection: IProjection;
  VSelectedTilesFloat: TDoubleRect;
  VSelectedTiles: TRect;
begin
  VProjection := GetActualProjection(AProjection);
  VSelectedTilesFloat := VProjection.LonLatRect2TileRectFloat(ASourceRect);
  VSelectedTiles := RectFromDoubleRect(VSelectedTilesFloat, rrOutside);
  Result := VProjection.TileRect2LonLatRect(VSelectedTiles);
end;

end.

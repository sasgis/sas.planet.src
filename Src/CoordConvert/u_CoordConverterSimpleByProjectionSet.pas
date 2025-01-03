{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_CoordConverterSimpleByProjectionSet;

interface

uses
  Types,
  t_GeoTypes,
  i_CoordConverterSimple,
  i_ProjectionSet,
  i_ProjectionType,
  u_BaseInterfacedObject;

type
  TCoordConverterSimpleByProjectionSet = class(TBaseInterfacedObject, ICoordConverterSimple)
  private
    FProjectionSet: IProjectionSet;
    FProjectionType: IProjectionType;
  private
    { ICoordConverterSimple }
    function Pos2LonLat(
      const XY: TPoint;
      AZoom: Byte
    ): TDoublePoint; stdcall;

    function LonLat2Pos(
      const APoint: TDoublePoint;
      AZoom: Byte
    ): TPoint; stdcall;

    function LonLat2Metr(const APoint: TDoublePoint): TDoublePoint; stdcall;
    function Metr2LonLat(const APoint: TDoublePoint): TDoublePoint; stdcall;

    function TilesAtZoom(const AZoom: Byte): Longint; stdcall;
    function PixelsAtZoom(const AZoom: Byte): Longint; stdcall;

    function TilePos2PixelPos(
      const XY: TPoint;
      const AZoom: Byte
    ): TPoint; stdcall;

    function TilePos2PixelRect(
      const XY: TPoint;
      const AZoom: Byte
    ): TRect; stdcall;
  public
    constructor Create(const AProjectionSet: IProjectionSet);
  end;

implementation

uses
  Math,
  i_Projection,
  u_GeoFunc;

{ TCoordConverterSimpleByProjectionSet }

constructor TCoordConverterSimpleByProjectionSet.Create(
  const AProjectionSet: IProjectionSet
);
begin
  Assert(Assigned(AProjectionSet));
  inherited Create;
  FProjectionSet := AProjectionSet;
  FProjectionType := FProjectionSet.Zooms[0].ProjectionType;
end;

function TCoordConverterSimpleByProjectionSet.LonLat2Metr(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result := FProjectionType.LonLat2Metr(APoint);
end;

function TCoordConverterSimpleByProjectionSet.LonLat2Pos(
  const APoint: TDoublePoint;
  AZoom: Byte
): TPoint;
var
  VProjection: IProjection;
begin
  if AZoom > 23 then begin
    VProjection := FProjectionSet.Zooms[AZoom - 8];
    Result := PointFromDoublePoint(VProjection.LonLat2PixelPosFloat(APoint), prToTopLeft);
  end else begin
    VProjection := FProjectionSet.Zooms[AZoom];
    Result := PointFromDoublePoint(VProjection.LonLat2TilePosFloat(APoint), prToTopLeft);
  end;
end;

function TCoordConverterSimpleByProjectionSet.Metr2LonLat(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result := FProjectionType.Metr2LonLat(APoint);
end;

function TCoordConverterSimpleByProjectionSet.PixelsAtZoom(
  const AZoom: Byte
): Longint;
var
  VRect: TRect;
begin
  Result := 0;
  if FProjectionSet.CheckZoom(AZoom) then begin
    VRect := FProjectionSet.Zooms[AZoom].GetPixelRect;
    Result := VRect.Right - VRect.Left;
  end;
end;

function TCoordConverterSimpleByProjectionSet.Pos2LonLat(
  const XY: TPoint;
  AZoom: Byte
): TDoublePoint;
var
  VProjection: IProjection;
begin
  if AZoom > 23 then begin
    VProjection := FProjectionSet.Zooms[AZoom - 8];
    Result := VProjection.PixelPos2LonLat(XY);
  end else begin
    VProjection := FProjectionSet.Zooms[AZoom];
    Result := VProjection.TilePos2LonLat(XY);
  end;
end;

function TCoordConverterSimpleByProjectionSet.TilePos2PixelPos(
  const XY: TPoint;
  const AZoom: Byte
): TPoint;
begin
  if FProjectionSet.CheckZoom(AZoom) then begin
    Result := FProjectionSet.Zooms[AZoom].TilePos2PixelPos(XY);
  end;
end;

function TCoordConverterSimpleByProjectionSet.TilePos2PixelRect(
  const XY: TPoint;
  const AZoom: Byte
): TRect;
begin
  if FProjectionSet.CheckZoom(AZoom) then begin
    Result := FProjectionSet.Zooms[AZoom].TilePos2PixelRect(XY);
  end;
end;

function TCoordConverterSimpleByProjectionSet.TilesAtZoom(
  const AZoom: Byte
): Longint;
var
  VRect: TRect;
begin
  Result := 0;
  if FProjectionSet.CheckZoom(AZoom) then begin
    VRect := FProjectionSet.Zooms[AZoom].GetTileRect;
    Result := VRect.Right - VRect.Left;
  end;
end;

end.

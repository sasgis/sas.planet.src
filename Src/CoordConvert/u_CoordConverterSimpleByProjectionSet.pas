{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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
    // ѕреобразует позицию тайла на заданном зуме в георафически координаты его верхнего левого угла
    function Pos2LonLat(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; stdcall;
    // ѕреобразует георафические координаты в позицию тайла на заданном зуме накрывающего данные координаты
    function LonLat2Pos(
      const Ll: TDoublePoint;
      AZoom: byte
    ): Tpoint; stdcall;

    // метрические координаты
    function LonLat2Metr(const Ll: TDoublePoint): TDoublePoint; stdcall;
    function Metr2LonLat(const Mm: TDoublePoint): TDoublePoint; stdcall;

    // ¬озвращает количество тайлов в заданном зуме
    function TilesAtZoom(const AZoom: byte): Longint; stdcall;
    // ¬озвращает общее количество пикселей на заданном зуме
    function PixelsAtZoom(const AZoom: byte): Longint; stdcall;

    // ѕреобразует позицию тайла заданного зума в координаты пиксела его левого верхнего угла
    function TilePos2PixelPos(
      const XY: TPoint;
      const AZoom: byte
    ): TPoint; stdcall;
    // ѕреобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2PixelRect(
      const XY: TPoint;
      const AZoom: byte
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
  const Ll: TDoublePoint
): TDoublePoint;
begin
  Result := FProjectionType.LonLat2Metr(Ll);
end;

function TCoordConverterSimpleByProjectionSet.LonLat2Pos(
  const Ll: TDoublePoint;
  AZoom: byte
): Tpoint;
var
  VProjection: IProjection;
begin
  if AZoom > 23 then begin
    VProjection := FProjectionSet.Zooms[AZoom - 8];
    Result := PointFromDoublePoint(VProjection.LonLat2PixelPosFloat(ll), prToTopLeft);
  end else begin
    VProjection := FProjectionSet.Zooms[AZoom];
    Result := PointFromDoublePoint(VProjection.LonLat2TilePosFloat(LL), prToTopLeft);
  end;
end;

function TCoordConverterSimpleByProjectionSet.Metr2LonLat(
  const Mm: TDoublePoint
): TDoublePoint;
begin
  Result := FProjectionType.Metr2LonLat(Mm);
end;

function TCoordConverterSimpleByProjectionSet.PixelsAtZoom(
  const AZoom: byte
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
  AZoom: byte
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
  const AZoom: byte
): TPoint;
begin
  if FProjectionSet.CheckZoom(AZoom) then begin
    Result := FProjectionSet.Zooms[AZoom].TilePos2PixelPos(XY);
  end;
end;

function TCoordConverterSimpleByProjectionSet.TilePos2PixelRect(
  const XY: TPoint;
  const AZoom: byte
): TRect;
begin
  if FProjectionSet.CheckZoom(AZoom) then begin
    Result := FProjectionSet.Zooms[AZoom].TilePos2PixelRect(XY);
  end;
end;

function TCoordConverterSimpleByProjectionSet.TilesAtZoom(
  const AZoom: byte
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

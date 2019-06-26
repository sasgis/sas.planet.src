{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_TileIteratorFactory;

interface

uses
  i_Projection,
  i_TileIterator,
  i_TileIteratorFactory,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  u_BaseInterfacedObject;

type
  TTileIteratorFactory = class(TBaseInterfacedObject, ITileIteratorFactory)
  private
    FGeometryProjectedFactory: IGeometryProjectedFactory;
  private
    { ITileIteratorFactory }
    function MakeTileIterator(
      const AProjection: IProjection;
      const ALonLatPolygon: IGeometryLonLatPolygon
    ): ITileIterator;
  public
    constructor Create(
      const AGeometryProjectedFactory: IGeometryProjectedFactory
    );
  end;

implementation

uses
  Types,
  Math,
  SysUtils,
  t_GeoTypes,
  i_TileRect,
  i_GeometryProjected,
  u_GeoFunc,
  u_TileRect,
  u_TileIteratorByRect,
  u_TileIteratorByPolygon;

{ TTileIteratorFactory }

constructor TTileIteratorFactory.Create(
  const AGeometryProjectedFactory: IGeometryProjectedFactory
);
begin
  Assert(AGeometryProjectedFactory <> nil);
  inherited Create;

  FGeometryProjectedFactory := AGeometryProjectedFactory;
end;

function TTileIteratorFactory.MakeTileIterator(
  const AProjection: IProjection;
  const ALonLatPolygon: IGeometryLonLatPolygon
): ITileIterator;
var
  I: Integer;
  VRect: ITileRect;
  VIsRect: Boolean;
  VSingle: IGeometryLonLatSinglePolygon;
  VProjected: IGeometryProjectedPolygon;
  VPoints: PDoublePointArray;
  VCornerTiles: array [0..3] of TPoint;
begin
  VRect := nil;

  if Supports(ALonLatPolygon, IGeometryLonLatSinglePolygon, VSingle) then begin
    if (VSingle.OuterBorder.Count = 4) and (VSingle.HoleCount = 0) then begin

      VPoints := VSingle.OuterBorder.Points;

      for I := 0 to 3 do begin
        VCornerTiles[I] :=
          PointFromDoublePoint(
            AProjection.LonLat2TilePosFloat(VPoints[I]),
            prToTopLeft
          );
      end;

      VIsRect :=
        (
          (VCornerTiles[0].X = VCornerTiles[3].X) and
          (VCornerTiles[2].X = VCornerTiles[1].X) and
          (VCornerTiles[0].Y = VCornerTiles[1].Y) and
          (VCornerTiles[2].Y = VCornerTiles[3].Y)
        ) or (
          (VCornerTiles[0].X = VCornerTiles[1].X) and
          (VCornerTiles[2].X = VCornerTiles[3].X) and
          (VCornerTiles[3].Y = VCornerTiles[0].Y) and
          (VCornerTiles[1].Y = VCornerTiles[2].Y)
        );

      if VIsRect then begin
        VRect :=
          TTileRect.Create(
            AProjection,
            RectFromDoubleRect(
              AProjection.LonLatRect2TileRectFloat(VSingle.Bounds.Rect),
              rrOutside
            )
          );
      end;
    end;
  end;

  if VRect <> nil then begin
    Result := TTileIteratorByRect.Create(VRect);
  end else begin
    VProjected :=
      FGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        AProjection,
        ALonLatPolygon
      );
    Result := TTileIteratorByPolygon.Create(AProjection, VProjected);
  end;
end;

end.

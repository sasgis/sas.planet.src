{******************************************************************************}
{* SAS.Planet (SAS.�������)                                                   *}
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

unit u_TileIteratorByPolygon;

interface

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
  i_TileIterator,
  i_VectorItemProjected,
  u_BaseInterfacedObject;

type
  TTileIteratorByPolygon = class(TBaseInterfacedObject, ITileIterator)
  private
    FProjected: IProjectedPolygon;
    FCurrent: TPoint;
    FTilesRect: TRect;
    FTilesTotal: Int64;
    FLine: IProjectedPolygonLine;
    FZoom: Byte;
    FGeoConverter: ICoordConverter;
  private
    function GetTilesTotal: Int64;
    function GetTilesRect: TRect;
    function Next(out ATile: TPoint): Boolean;
    procedure Reset;
  public
    constructor Create(
      const AProjected: IProjectedPolygon
    );
  end;

implementation

uses
  u_GeoFun;

{ TTileIteratorByPolygon }

constructor TTileIteratorByPolygon.Create(
  const AProjected: IProjectedPolygon
);
var
  VBounds: TDoubleRect;
  VTile: TPoint;
begin
  inherited Create;
  FProjected := AProjected;
  if FProjected.Count > 0 then begin
    FLine := FProjected.Item[0];
    VBounds := FLine.Bounds;
    FZoom := FLine.Projection.Zoom;
    FGeoConverter := FLine.Projection.GeoConverter;
    FTilesRect :=
      RectFromDoubleRect(
        FGeoConverter.PixelRectFloat2TileRectFloat(VBounds, FZoom),
        rrOutside
      );

    Reset;
    FTilesTotal := 0;
    while Next(VTile) do begin
      Inc(FTilesTotal);
    end;
    Reset;
  end;
end;

function TTileIteratorByPolygon.GetTilesRect: TRect;
begin
  Result := FTilesRect;
end;

function TTileIteratorByPolygon.GetTilesTotal: Int64;
begin
  Result := FTilesTotal;
end;

function TTileIteratorByPolygon.Next(out ATile: TPoint): Boolean;
var
  VRect: TDoubleRect;
begin
  Result := False;
  while FCurrent.X < FTilesRect.Right do begin
    while FCurrent.Y < FTilesRect.Bottom do begin
      VRect := FGeoConverter.TilePos2PixelRectFloat(FCurrent, FZoom);
      if FLine.IsRectIntersectPolygon(VRect) then begin
        ATile := FCurrent;
        Result := True;
      end;
      Inc(FCurrent.Y);
      if Result then begin
        Break;
      end;
    end;
    if Result then begin
      Break;
    end;
    if FCurrent.Y >= FTilesRect.Bottom then begin
      FCurrent.Y := FTilesRect.Top;
      Inc(FCurrent.X);
    end;
  end;
end;

procedure TTileIteratorByPolygon.Reset;
begin
  inherited;
  FCurrent := FTilesRect.TopLeft;
end;

end.

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
    // устанавливается только если один кусок (для скорости)
    FSingleLine: IProjectedPolygonLine;
    // кэш куска
    FLastUsedLine: IProjectedPolygonLine;
    FZoom: Byte;
    FGeoConverter: ICoordConverter;
  private
    function GetTilesTotal: Int64;
    function GetTilesRect: TRect;
    function Next(out ATile: TPoint): Boolean;
    procedure Reset;
  private
    function InternalIntersectPolygon(const ARect: TDoubleRect): Boolean;
  public
    constructor Create(
      const AProjected: IProjectedPolygon
    );
  end;

implementation

uses
  u_GeoFunc;

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
  FLastUsedLine := nil;
  if FProjected.Count > 0 then begin
    // в зависимости от числа сегментов...
    if (FProjected.Count=1) then begin
      // ...ходим только по одной области
      FSingleLine := FProjected.Item[0];
    end else begin
      // ...будем ходить в цикле
      FSingleLine := nil;
    end;

    // общее ограничение и прочие параметры
    with FProjected do begin
      VBounds := Bounds;
      with Projection do begin
        FZoom := Zoom;
        FGeoConverter := GeoConverter;
      end;
    end;
    
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

function TTileIteratorByPolygon.InternalIntersectPolygon(const ARect: TDoubleRect): Boolean;
var
  i: Integer;
  VLine: IProjectedPolygonLine;
begin
  if (FSingleLine<>nil) then begin
    // один сегмент - только его и проверяем
    Result := FSingleLine.IsRectIntersectPolygon(ARect);
    Exit;
  end;

  // это тут возможно будет недостаточно быстро
  // во-первых нет попадания сразу в текущий сегмент (в кэш)
  // во-вторых ненужная проверка попадания в Bounds
  // Result := FProjected.IsRectIntersectPolygon(ARect);

  // проверяем кэш
  if (FLastUsedLine<>nil) then begin
    Result := FLastUsedLine.IsRectIntersectPolygon(ARect);
    if Result then
      Exit;
  end;

  // проверяем всё в цикле
  for i := 0 to FProjected.Count-1 do begin
    VLine := FProjected.GetItem(i);
    if (Pointer(VLine)<>Pointer(FLastUsedLine)) then
    if VLine.IsRectIntersectPolygon(ARect) then begin
      // нашлось
      Result := TRUE;
      FLastUsedLine := VLine;
      Exit;
    end;
  end;

  Result := FALSE;
end;

function TTileIteratorByPolygon.Next(out ATile: TPoint): Boolean;
var
  VRect: TDoubleRect;
begin
  Result := False;
  while FCurrent.X < FTilesRect.Right do begin
    while FCurrent.Y < FTilesRect.Bottom do begin
      VRect := FGeoConverter.TilePos2PixelRectFloat(FCurrent, FZoom);
      if InternalIntersectPolygon(VRect) then begin
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
  FLastUsedLine := nil;
end;

end.

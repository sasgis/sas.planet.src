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

unit u_TileIteratorByPolygon;

interface

uses
  Types,
  t_GeoTypes,
  i_Projection,
  i_TileRect,
  i_TileIterator,
  i_GeometryProjected,
  i_TileIteratorDataProvider,
  u_BaseInterfacedObject;

type
  TTileIteratorByPolygon = class(TBaseInterfacedObject, ITileIterator)
  private
    FProjection: IProjection;
    FPolygon: IGeometryProjectedPolygon;
    FStartPoint: TPoint;
    FCurrent: TPoint;
    FTilesRect: TRect;
    FRect: ITileRect;
    FTilesTotal: Int64;
    FProcessedTilesCount: Int64;
    FStopOnProcessedTilesCount: Boolean;
    // устанавливается только если один кусок (для скорости)
    FSingleLine: IGeometryProjectedSinglePolygon;
    FMultiProjected: IGeometryProjectedMultiPolygon;
    // кэш куска
    FLastUsedLine: IGeometryProjectedSinglePolygon;
  private
    function GetTilesTotal: Int64;
    function GetTilesRect: ITileRect;
    function Next(out ATile: TPoint): Boolean;
    procedure Reset;
  private
    function InternalIntersectPolygon(const ARect: TDoubleRect): Boolean;
  public
    constructor Create(
      const AProjection: IProjection;
      const AProjected: IGeometryProjectedPolygon;
      const ATilesTotal: Int64 = -1;
      const AStartX: Integer = -1;
      const AStartY: Integer = -1
    );
  end;

implementation

uses
  SysUtils,
  Math,
  u_TileRect,
  u_GeometryFunc,
  u_GeoFunc;

const
  cFakePoint: TPoint = (X: -1; Y: -1);

{ TTileIteratorByPolygon }

constructor TTileIteratorByPolygon.Create(
  const AProjection: IProjection;
  const AProjected: IGeometryProjectedPolygon;
  const ATilesTotal: Int64;
  const AStartX: Integer;
  const AStartY: Integer
);
var
  VBounds: TDoubleRect;
begin
  inherited Create;
  FProjection := AProjection;

  if (AStartX <> -1) and (AStartY <> -1) then begin
    FStartPoint := Point(AStartX, AStartY);
  end else begin
    FStartPoint := cFakePoint;
  end;

  FProcessedTilesCount := 0;
  FStopOnProcessedTilesCount := ATilesTotal <> -1;

  if not Assigned(AProjected) then begin
    FTilesTotal := 0;
    FTilesRect := Rect(0, 0, 0, 0);
    Assert(False);
    FRect := TTileRect.Create(AProjection, FTilesRect);
  end else begin
    if Supports(AProjected, IGeometryProjectedSinglePolygon, FSingleLine) then begin
      FMultiProjected := nil;
    end else if Supports(AProjected, IGeometryProjectedMultiPolygon, FMultiProjected) then begin
      // в зависимости от числа сегментов...
      if (FMultiProjected.Count = 1) then begin
        // ...ходим только по одной области
        FSingleLine := FMultiProjected.Item[0];
      end else begin
        // ...будем ходить в цикле
        FSingleLine := nil;
      end;
    end else begin
      Assert(False);
      FSingleLine := nil;
      FMultiProjected := nil;
      FTilesTotal := 0;
      FTilesRect := Rect(0, 0, 0, 0);
      FRect := TTileRect.Create(AProjection, FTilesRect);
      Exit;
    end;
    VBounds := AProjected.Bounds;

    FTilesRect :=
      RectFromDoubleRect(
        FProjection.PixelRectFloat2TileRectFloat(VBounds),
        rrOutside
      );
    FRect := TTileRect.Create(AProjection, FTilesRect);
    FPolygon := AProjected;
    Reset;
    FTilesTotal := ATilesTotal;
  end;
end;

function TTileIteratorByPolygon.GetTilesRect: ITileRect;
begin
  Result := FRect;
end;

function TTileIteratorByPolygon.GetTilesTotal: Int64;
begin
  if FTilesTotal < 0 then begin
    FTilesTotal := CalcTileCountInProjectedPolygon(FProjection, FPolygon);
  end;
  Result := FTilesTotal;
end;

function TTileIteratorByPolygon.InternalIntersectPolygon(const ARect: TDoubleRect): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedSinglePolygon;
begin
  if (FSingleLine <> nil) then begin
    // один сегмент - только его и проверяем
    Result := FSingleLine.IsRectIntersectPolygon(ARect);
    Exit;
  end;

  // это тут возможно будет недостаточно быстро
  // во-первых нет попадания сразу в текущий сегмент (в кэш)
  // во-вторых ненужная проверка попадания в Bounds
  // Result := FProjected.IsRectIntersectPolygon(ARect);

  // проверяем кэш
  if (FLastUsedLine <> nil) then begin
    Result := FLastUsedLine.IsRectIntersectPolygon(ARect);
    if Result then begin
      Exit;
    end;
  end;

  // проверяем всё в цикле
  for i := 0 to FMultiProjected.Count - 1 do begin
    VLine := FMultiProjected.GetItem(i);
    if (Pointer(VLine) <> Pointer(FLastUsedLine)) then begin
      if VLine.IsRectIntersectPolygon(ARect) then begin
      // нашлось
        Result := TRUE;
        FLastUsedLine := VLine;
        Exit;
      end;
    end;
  end;

  Result := FALSE;
end;

function TTileIteratorByPolygon.Next(out ATile: TPoint): Boolean;
var
  VRect: TDoubleRect;
begin
  Result := False;

  if FStopOnProcessedTilesCount and (FProcessedTilesCount >= FTilesTotal) then begin
    Exit;
  end;

  while FCurrent.X < FTilesRect.Right do begin
    while FCurrent.Y < FTilesRect.Bottom do begin
      VRect := FProjection.TilePos2PixelRectFloat(FCurrent);
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

  if Result then begin
    Inc(FProcessedTilesCount);
  end;
end;

procedure TTileIteratorByPolygon.Reset;
var
  VTmp: TPoint;
begin
  if IsPointsEqual(FStartPoint, cFakePoint) then begin
    FCurrent := FTilesRect.TopLeft;
  end else begin
    FCurrent := FStartPoint;
    Next(VTmp);
  end;
  FLastUsedLine := nil;
  FProcessedTilesCount := 0;
end;

end.

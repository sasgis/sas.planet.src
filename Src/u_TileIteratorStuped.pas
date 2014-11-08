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

unit u_TileIteratorStuped;

interface

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  u_TileIteratorAbstract;

type
  TTileIteratorStuped = class(TTileIteratorByPolygonAbstract)
  private
    p_x, p_y: Integer;
    FPolyg: TArrayOfPoint;
    FPixelRect: TRect;

    FTilesTotal: Int64;
    FTilesRect: TRect;
  protected
    function GetTilesTotal: Int64; override;
    function GetTilesRect: TRect; override;
  public
    constructor Create(
      AProjected: IProjectedPolygon
    );
    destructor Destroy; override;
    function Next(out ATile: TPoint): Boolean; override;
    procedure Reset; override;
  end;

  TTileIteratorBySubRect = class(TTileIteratorStuped)
  protected
    FCurrentSubRect: TPoint;
    FSubRectWidth: integer;
    FSubRectHeight: integer;
  public
    constructor Create(
      AProjected: IProjectedPolygon;
      ASubRectSize: TPoint
    );
    function Next(out ATile: TPoint): Boolean; override;
    procedure Reset; override;
  end;

implementation

uses
  i_EnumDoublePoint,
  u_GeoFun;

{ TTileIteratorStuped }

constructor TTileIteratorStuped.Create(
  AProjected: IProjectedPolygon
);
var
  VLen: Integer;
  VEnum: IEnumProjectedPoint;
  VPoint: TDoublePoint;
  i: Integer;
  VLine: IProjectedPolygonLine;
begin
  inherited;
  if Projected.Count > 0 then begin
    VLine := Projected.Item[0];
    VLen := VLine.Count;
    SetLength(FPolyg, VLen + 1);
    i := 0;
    VEnum := VLine.GetEnum;
    while VEnum.Next(VPoint) do begin
      FPolyg[i] := Point(Trunc(VPoint.X), Trunc(VPoint.Y));
      Inc(i);
    end;
    FTilesTotal := GetDwnlNum(FPixelRect, @FPolyg[0], VLen, true);
    FTilesRect := Projected.Projection.GeoConverter.PixelRect2TileRect(FPixelRect, Projected.Projection.Zoom);
    Reset;
  end;
end;

destructor TTileIteratorStuped.Destroy;
begin
  FPolyg := nil;
  inherited;
end;

function TTileIteratorStuped.GetTilesRect: TRect;
begin
  Result := FTilesRect;
end;

function TTileIteratorStuped.GetTilesTotal: Int64;
begin
  Result := FTilesTotal;
end;

function TTileIteratorStuped.Next(out ATile: TPoint): Boolean;
begin
  Result := False;
  while p_x < FPixelRect.Right do begin
    FCurrent.X := p_x shr 8;
    while p_y < FPixelRect.Bottom do begin
      FCurrent.Y := p_y shr 8;
      if (RgnAndRgn(@FPolyg[0], Length(FPolyg), p_x, p_y, false)) then begin
        Result := True;
      end;
      inc(p_y, 256);
      if Result then begin
        Break;
      end;
    end;
    if Result then begin
      Break;
    end;
    if p_y >= FPixelRect.Bottom then begin
      p_y := FPixelRect.Top;
      inc(p_x, 256);
    end;
  end;
  ATile := FCurrent;
end;

procedure TTileIteratorStuped.Reset;
begin
  inherited;
  p_x := FPixelRect.Left;
  p_y := FPixelRect.Top;
end;

{ TTileIteratorBySubRect }

constructor TTileIteratorBySubRect.Create(
  AProjected: IProjectedPolygon;
  ASubRectSize: TPoint
);
begin
  inherited Create(AProjected);
  FSubRectWidth := ASubRectSize.x;
  FSubRectHeight := ASubRectSize.y;
  Reset;
end;

function TTileIteratorBySubRect.Next(out ATile: TPoint): Boolean;
begin
  Result := False;
  while not (result) do begin
    FCurrent.X := p_x shr 8;
    FCurrent.Y := p_y shr 8;
    ATile := FCurrent;
    if (RgnAndRgn(@FPolyg[0], Length(FPolyg), p_x, p_y, false)) then begin
      Result := True;
    end;

    inc(p_x, 256);
    FCurrent.X := p_x shr 8;
    if (((FCurrent.X + 0) mod FSubRectWidth) = 0) or (FCurrent.X >= FTilesRect.Right) then begin
      inc(p_y, 256);
      FCurrent.Y := p_y shr 8;
      if (((FCurrent.Y + 0) mod FSubRectHeight) = 0) or (FCurrent.Y >= FTilesRect.Bottom) then begin
        if (FCurrent.X >= FTilesRect.Right) then begin
          if (FCurrent.Y >= FTilesRect.Bottom) then begin
            Exit;
          end;
          FCurrentSubRect.x := 0;
          inc(FCurrentSubRect.Y);
        end else begin
          inc(FCurrentSubRect.x);
        end;
        p_y := (FTilesRect.Top - (FTilesRect.Top mod FSubRectHeight) + FSubRectHeight * FCurrentSubRect.Y) shl 8 + 128;
      end;
      p_x := (FTilesRect.Left - (FTilesRect.Left mod FSubRectHeight) + FSubRectWidth * FCurrentSubRect.x) shl 8 + 128;
    end;
  end;
end;

procedure TTileIteratorBySubRect.Reset;
begin
  inherited;
  FCurrentSubRect := Point(0, 0);
end;

end.

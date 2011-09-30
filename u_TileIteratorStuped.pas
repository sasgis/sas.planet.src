{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_TileIteratorStuped;

interface

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
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
      AZoom: byte;
      APolygLL: TArrayOfDoublePoint;
      AGeoConvert: ICoordConverter
    ); override;
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
      AZoom: byte;
      APolygLL: TArrayOfDoublePoint;
      AGeoConvert: ICoordConverter;
      ASubRectWidth, ASubRectHeight: integer
    ); reintroduce;
    function Next(out ATile: TPoint): Boolean; override;
    procedure Reset; override;
  end;

implementation

uses
  u_GeoFun;

{ TTileIteratorStuped }

constructor TTileIteratorStuped.Create(AZoom: byte;
  APolygLL: TArrayOfDoublePoint; AGeoConvert: ICoordConverter);
begin
  inherited;
  FPolyg := FGeoConvert.LonLatArray2PixelArray(FPolygLL, FZoom);
  FTilesTotal := GetDwnlNum(FPixelRect, FPolyg, true);
  FTilesRect := FGeoConvert.PixelRect2TileRect(FPixelRect, FZoom);
  Reset;
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
      if (RgnAndRgn(FPolyg, p_x, p_y, false)) then begin
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

constructor TTileIteratorBySubRect.Create(AZoom: byte; APolygLL: TArrayOfDoublePoint;
                AGeoConvert: ICoordConverter; ASubRectWidth, ASubRectHeight: integer);
begin
  inherited Create(AZoom, APolygLL, AGeoConvert);
  FSubRectWidth:=ASubRectWidth;
  FSubRectHeight:=ASubRectHeight;
  Reset;
end;

function TTileIteratorBySubRect.Next(out ATile: TPoint): Boolean;
begin
  Result := False;
  while not(result) do begin
    FCurrent.X := p_x shr 8;
    FCurrent.Y := p_y shr 8;
    ATile:= FCurrent;
    if (RgnAndRgn(FPolyg, p_x, p_y, false)) then begin
      Result := True;
    end;

    inc(p_x, 256);
    FCurrent.X := p_x shr 8;
    if (((FCurrent.X + 0) mod FSubRectWidth)=0)or(FCurrent.X>=FTilesRect.Right) then begin
      inc(p_y, 256);
      FCurrent.Y := p_y shr 8;
      if (((FCurrent.Y + 0) mod FSubRectHeight)=0)or(FCurrent.Y>=FTilesRect.Bottom) then begin
        if (FCurrent.X>=FTilesRect.Right) then begin
          if (FCurrent.Y>=FTilesRect.Bottom) then begin
            Exit;
          end;
          FCurrentSubRect.x:=0;
          inc(FCurrentSubRect.Y);
        end else begin
          inc(FCurrentSubRect.x);
        end;
        p_y := (FTilesRect.Top-(FTilesRect.Top mod FSubRectHeight)+FSubRectHeight*FCurrentSubRect.Y) shl 8 + 128;
      end;
      p_x := (FTilesRect.Left-(FTilesRect.Left mod FSubRectHeight)+FSubRectWidth*FCurrentSubRect.x) shl 8 + 128;
    end;
  end;
end;

procedure TTileIteratorBySubRect.Reset;
begin
  inherited;
  FCurrentSubRect := Point(0,0);
end;

end.

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

unit u_LocalCoordConverter;

interface

uses
  Types,
  t_GeoTypes,
  u_GeoFun,
  i_CoordConverter,
  i_LocalCoordConverter;

type
  TLocalCoordConverter = class(TInterfacedObject, ILocalCoordConverter)
  private
    FLocalRect: TRect;
    FLocalSize: TPoint;
    FLocalCenter: TDoublePoint;
    FZoom: Byte;
    FGeoConverter: ICoordConverter;
    FMapScale: TDoublePoint;
    FLocalTopLeftAtMap: TDoublePoint;
  protected
    function GetIsSameConverter(AConverter: ILocalCoordConverter): Boolean;

    function GetLocalRect: TRect;
    function GetLocalRectSize: TPoint;

    function GetZoom: Byte;
    function GetGeoConverter: ICoordConverter;

    function LocalPixel2MapPixel(const APoint: TPoint): TPoint;
    function LocalPixel2MapPixelFloat(const APoint: TPoint): TDoublePoint;
    function LocalPixelFloat2MapPixelFloat(const APoint: TDoublePoint): TDoublePoint;
    function MapPixel2LocalPixel(const APoint: TPoint): TPoint;
    function MapPixel2LocalPixelFloat(const APoint: TPoint): TDoublePoint;
    function MapPixelFloat2LocalPixelFloat(const APoint: TDoublePoint): TDoublePoint;

    function LocalRect2MapRect(const ARect: TRect): TRect;
    function LocalRect2MapRectFloat(const ARect: TRect): TDoubleRect;
    function LocalRectFloat2MapRectFloat(const ARect: TDoubleRect): TDoubleRect;
    function MapRect2LocalRect(const ARect: TRect): TRect;
    function MapRect2LocalRectFloat(const ARect: TRect): TDoubleRect;
    function MapRectFloat2LocalRectFloat(const ARect: TDoubleRect): TDoubleRect;

    function LonLat2LocalPixel(const APoint: TDoublePoint): TPoint;
    function LonLat2LocalPixelFloat(const APoint: TDoublePoint): TDoublePoint;
    function LonLatRect2LocalRectFloat(const ARect: TDoubleRect): TDoubleRect;

    function LonLatArrayToVisualFloatArray(const APolygon: TArrayOfDoublePoint): TArrayOfDoublePoint;

    function GetCenterMapPixelFloat: TDoublePoint;
    function GetCenterLonLat: TDoublePoint;
    function GetRectInMapPixel: TRect;
    function GetRectInMapPixelFloat: TDoubleRect;
  public
    constructor Create(
      ALocalRect: TRect;
      AZoom: Byte;
      AGeoConverter: ICoordConverter;
      AMapScale: TDoublePoint;
      ALocalTopLeftAtMap: TDoublePoint
    );
    destructor Destroy; override;
  end;

implementation

{ TLocalCoordConverter }

constructor TLocalCoordConverter.Create(
  ALocalRect: TRect;
  AZoom: Byte;
  AGeoConverter: ICoordConverter;
  AMapScale, ALocalTopLeftAtMap: TDoublePoint);
begin
  FLocalRect := ALocalRect;
  FLocalSize.X := FLocalRect.Right - FLocalRect.Left;
  FLocalSize.Y := FLocalRect.Bottom - FLocalRect.Top;
  FLocalCenter.X := FLocalRect.Left + FLocalSize.X / 2;
  FLocalCenter.Y := FLocalRect.Left + FLocalSize.Y / 2;
  FZoom := AZoom;
  FGeoConverter := AGeoConverter;
  FMapScale := AMapScale;
  FLocalTopLeftAtMap := ALocalTopLeftAtMap;
end;

destructor TLocalCoordConverter.Destroy;
begin
  FGeoConverter := nil;
  inherited;
end;

function TLocalCoordConverter.GetCenterLonLat: TDoublePoint;
var
  VMapPixel: TDoublePoint;
begin
  VMapPixel := LocalPixelFloat2MapPixelFloat(FLocalCenter);
  FGeoConverter.CheckPixelPosFloat(VMapPixel, FZoom, True);
  Result := FGeoConverter.PixelPosFloat2LonLat(VMapPixel, FZoom);
end;

function TLocalCoordConverter.GetCenterMapPixelFloat: TDoublePoint;
begin
  Result := LocalPixelFloat2MapPixelFloat(FLocalCenter);
end;

function TLocalCoordConverter.GetGeoConverter: ICoordConverter;
begin
  Result := FGeoConverter;
end;

function TLocalCoordConverter.GetIsSameConverter(
  AConverter: ILocalCoordConverter): Boolean;
var
  VSelf: ILocalCoordConverter;
begin
  VSelf := Self;
  if VSelf = AConverter then begin
    Result := True;
  end else begin
    Result := False;
    if FZoom = AConverter.GetZoom then begin
      if EqualRect(FLocalRect, AConverter.GetLocalRect) then begin
        if FGeoConverter.IsSameConverter(AConverter.GetGeoConverter) then begin
          if EqualRect(AConverter.GetRectInMapPixel, GetRectInMapPixel) then begin
            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

function TLocalCoordConverter.GetLocalRect: TRect;
begin
  Result := FLocalRect;
end;

function TLocalCoordConverter.GetLocalRectSize: TPoint;
begin
  Result := FLocalSize;
end;

function TLocalCoordConverter.GetRectInMapPixel: TRect;
begin
  Result := LocalRect2MapRect(GetLocalRect);
end;

function TLocalCoordConverter.GetRectInMapPixelFloat: TDoubleRect;
begin
  Result := LocalRect2MapRectFloat(GetLocalRect);
end;

function TLocalCoordConverter.GetZoom: Byte;
begin
  Result := FZoom;
end;

function TLocalCoordConverter.LocalPixel2MapPixel(const APoint: TPoint): TPoint;
var
  VResultPoint: TDoublePoint;
begin
  VResultPoint := LocalPixel2MapPixelFloat(APoint);
  Result := Point(Trunc(VResultPoint.X), Trunc(VResultPoint.Y));
end;

function TLocalCoordConverter.LocalPixel2MapPixelFloat(
  const APoint: TPoint): TDoublePoint;
var
  VSourcePoint: TDoublePoint;
begin
  VSourcePoint.X := APoint.X;
  VSourcePoint.Y := APoint.Y;
  Result := LocalPixelFloat2MapPixelFloat(VSourcePoint);
end;

function TLocalCoordConverter.LocalPixelFloat2MapPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := APoint.X  / FMapScale.X + FLocalTopLeftAtMap.X;
  Result.Y := APoint.Y  / FMapScale.Y + FLocalTopLeftAtMap.Y;
end;

function TLocalCoordConverter.LocalRect2MapRect(const ARect: TRect): TRect;
begin
  Result.TopLeft := LocalPixel2MapPixel(ARect.TopLeft);
  Result.BottomRight := LocalPixel2MapPixel(ARect.BottomRight);
end;

function TLocalCoordConverter.LocalRect2MapRectFloat(
  const ARect: TRect): TDoubleRect;
begin
  Result.TopLeft := LocalPixel2MapPixelFloat(ARect.TopLeft);
  Result.BottomRight := LocalPixel2MapPixelFloat(ARect.BottomRight);
end;

function TLocalCoordConverter.LocalRectFloat2MapRectFloat(
  const ARect: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := LocalPixelFloat2MapPixelFloat(ARect.TopLeft);
  Result.BottomRight := LocalPixelFloat2MapPixelFloat(ARect.BottomRight);
end;

function TLocalCoordConverter.LonLat2LocalPixel(
  const APoint: TDoublePoint): TPoint;
var
  VResultPoint: TDoublePoint;
begin
  VResultPoint := LonLat2LocalPixelFloat(APoint);
  Result := Point(Trunc(VResultPoint.X), Trunc(VResultPoint.Y));
end;

function TLocalCoordConverter.LonLat2LocalPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result :=
    MapPixelFloat2LocalPixelFloat(
      FGeoConverter.LonLat2PixelPosFloat(APoint, FZoom)
    );
end;

function TLocalCoordConverter.LonLatRect2LocalRectFloat(
  const ARect: TDoubleRect): TDoubleRect;
begin
  Result :=
    MapRectFloat2LocalRectFloat(
      FGeoConverter.LonLatRect2PixelRectFloat(ARect, FZoom)
    );
end;

function TLocalCoordConverter.LonLatArrayToVisualFloatArray(const APolygon: TArrayOfDoublePoint): TArrayOfDoublePoint;
var
  i: Integer;
  VPointsCount: Integer;
  VLonLat: TDoublePoint;
  VGeoConvert: ICoordConverter;
begin
  VPointsCount := Length(APolygon);
  SetLength(Result, VPointsCount);

  VGeoConvert := GetGeoConverter;
  for i := 0 to VPointsCount - 1 do begin
    VLonLat := APolygon[i];
    if PointIsEmpty(VLonLat) then begin
      Result[i] := VLonLat;
    end else begin
      VGeoConvert.CheckLonLatPos(VLonLat);
      Result[i] := LonLat2LocalPixelFloat(VLonLat);
    end;
  end;
end;

function TLocalCoordConverter.MapPixel2LocalPixel(const APoint: TPoint): TPoint;
var
  VResultPoint: TDoublePoint;
begin
  VResultPoint := MapPixel2LocalPixelFloat(APoint);
  Result := Point(Trunc(VResultPoint.X), Trunc(VResultPoint.Y));
end;

function TLocalCoordConverter.MapPixel2LocalPixelFloat(
  const APoint: TPoint): TDoublePoint;
var
  VSourcePoint: TDoublePoint;
begin
  VSourcePoint.X := APoint.X;
  VSourcePoint.Y := APoint.Y;
  Result := MapPixelFloat2LocalPixelFloat(VSourcePoint);
end;

function TLocalCoordConverter.MapPixelFloat2LocalPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := (APoint.X - FLocalTopLeftAtMap.X) * FMapScale.X;
  Result.Y := (APoint.Y - FLocalTopLeftAtMap.Y) * FMapScale.Y;
end;

function TLocalCoordConverter.MapRect2LocalRect(const ARect: TRect): TRect;
begin
  Result.TopLeft := MapPixel2LocalPixel(ARect.TopLeft);
  Result.BottomRight := MapPixel2LocalPixel(ARect.BottomRight);
end;

function TLocalCoordConverter.MapRect2LocalRectFloat(
  const ARect: TRect): TDoubleRect;
begin
  Result.TopLeft := MapPixel2LocalPixelFloat(ARect.TopLeft);
  Result.BottomRight := MapPixel2LocalPixelFloat(ARect.BottomRight);
end;

function TLocalCoordConverter.MapRectFloat2LocalRectFloat(
  const ARect: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := MapPixelFloat2LocalPixelFloat(ARect.TopLeft);
  Result.BottomRight := MapPixelFloat2LocalPixelFloat(ARect.BottomRight);
end;

end.

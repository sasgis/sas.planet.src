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

unit u_LocalCoordConverterFactorySimpe;

interface

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
  i_CoordConverterFactory,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe;

type
  TLocalCoordConverterFactorySimpe = class(TInterfacedObject, ILocalCoordConverterFactorySimpe)
  private
    FProjectionFactory: IProjectionInfoFactory;
  private
    function CreateConverter(
      ALocalRect: TRect;
      AZoom: Byte;
      AGeoConverter: ICoordConverter;
      AMapScale: TDoublePoint;
      ALocalTopLeftAtMap: TDoublePoint
    ): ILocalCoordConverter;
    function CreateConverterNoScale(
      ALocalRect: TRect;
      AZoom: Byte;
      AGeoConverter: ICoordConverter;
      ALocalTopLeftAtMap: TPoint
    ): ILocalCoordConverter;
    function CreateForTile(
      ATile: TPoint;
      AZoom: Byte;
      AGeoConverter: ICoordConverter
    ): ILocalCoordConverter;
    function CreateBySourceWithStableTileRect(
      ASource: ILocalCoordConverter
    ): ILocalCoordConverter;
    function CreateBySourceWithStableTileRectAndOtherGeo(
      ASource: ILocalCoordConverter;
      AGeoConverter: ICoordConverter
    ): ILocalCoordConverter;
  public
    constructor Create(
      AProjectionFactory: IProjectionInfoFactory
    );
  end;


implementation

uses
  u_LocalCoordConverter;

{ TLocalCoordConverterFactorySimpe }

constructor TLocalCoordConverterFactorySimpe.Create(
  AProjectionFactory: IProjectionInfoFactory);
begin
  FProjectionFactory := AProjectionFactory;
end;

function TLocalCoordConverterFactorySimpe.CreateBySourceWithStableTileRect(
  ASource: ILocalCoordConverter
): ILocalCoordConverter;
var
  VZoom: Byte;
  VSourcePixelRect: TRect;
  VConverter: ICoordConverter;
  VTileRect: TRect;
  VResultPixelRect: TRect;
  VViewSize: TPoint;
  VMovedTile: TPoint;
  VMovedPixelRect: TRect;
  VMovedTileRect: TRect;
begin
  VConverter := ASource.GetGeoConverter;
  VZoom := ASource.GetZoom;
  VSourcePixelRect := ASource.GetRectInMapPixel;
  VViewSize.X := VSourcePixelRect.Right - VSourcePixelRect.Left;
  VViewSize.Y := VSourcePixelRect.Bottom - VSourcePixelRect.Top;

  VConverter.CheckPixelRect(VSourcePixelRect, VZoom);

  VTileRect := VConverter.PixelRect2TileRect(VSourcePixelRect, VZoom);
  VMovedTile := VTileRect.TopLeft;
  Inc(VMovedTile.X);
  Inc(VMovedTile.Y);
  VConverter.CheckTilePosStrict(VMovedTile, VZoom, False);

  VMovedPixelRect.TopLeft := VConverter.TilePos2PixelPos(VMovedTile, VZoom);
  VMovedPixelRect.Right := VMovedPixelRect.Left + VViewSize.X;
  VMovedPixelRect.Bottom := VMovedPixelRect.Top + VViewSize.Y;
  VConverter.CheckPixelRect(VMovedPixelRect, VZoom);

  VMovedTileRect := VConverter.PixelRect2TileRect(VMovedPixelRect, VZoom);
  VTileRect.Right := VMovedTileRect.Right;
  VTileRect.Bottom := VMovedTileRect.Bottom;

  VResultPixelRect := VConverter.TileRect2PixelRect(VTileRect, VZoom);
  if EqualRect(VSourcePixelRect, VResultPixelRect) then begin
    Result := ASource;
  end else begin
    Result := TLocalCoordConverterNoScale.Create(
      Rect(
        0, 0,
        VResultPixelRect.Right - VResultPixelRect.Left,
        VResultPixelRect.Bottom - VResultPixelRect.Top
      ),
      ASource.ProjectionInfo,
      VResultPixelRect.TopLeft
    );
  end;
end;

function TLocalCoordConverterFactorySimpe.CreateBySourceWithStableTileRectAndOtherGeo(
  ASource: ILocalCoordConverter;
  AGeoConverter: ICoordConverter
): ILocalCoordConverter;
var
  VZoom: Byte;
  VSourcePixelRect: TRect;
  VSourceLonLatRect: TDoubleRect;
  VConverter: ICoordConverter;
  VTileRect: TRect;
  VResultPixelRect: TRect;
  VViewSize: TPoint;
  VMovedTile: TPoint;
  VMovedPixelRect: TRect;
  VMovedTileRect: TRect;
begin
  VConverter := ASource.GetGeoConverter;
  VZoom := ASource.GetZoom;
  VSourcePixelRect := ASource.GetRectInMapPixel;
  VConverter.CheckPixelRect(VSourcePixelRect, VZoom);
  VSourceLonLatRect := VConverter.PixelRect2LonLatRect(VSourcePixelRect, VZoom);
  AGeoConverter.CheckZoom(VZoom);
  AGeoConverter.CheckLonLatRect(VSourceLonLatRect);
  VSourcePixelRect := AGeoConverter.LonLatRect2PixelRect(VSourceLonLatRect, VZoom);

  VViewSize.X := VSourcePixelRect.Right - VSourcePixelRect.Left;
  VViewSize.Y := VSourcePixelRect.Bottom - VSourcePixelRect.Top;

  AGeoConverter.CheckPixelRect(VSourcePixelRect, VZoom);

  VTileRect := AGeoConverter.PixelRect2TileRect(VSourcePixelRect, VZoom);
  VMovedTile := VTileRect.TopLeft;
  Inc(VMovedTile.X);
  Inc(VMovedTile.Y);
  AGeoConverter.CheckTilePosStrict(VMovedTile, VZoom, False);

  VMovedPixelRect.TopLeft := AGeoConverter.TilePos2PixelPos(VMovedTile, VZoom);
  VMovedPixelRect.Right := VMovedPixelRect.Left + VViewSize.X;
  VMovedPixelRect.Bottom := VMovedPixelRect.Top + VViewSize.Y;
  AGeoConverter.CheckPixelRect(VMovedPixelRect, VZoom);

  VMovedTileRect := AGeoConverter.PixelRect2TileRect(VMovedPixelRect, VZoom);
  VTileRect.Right := VMovedTileRect.Right;
  VTileRect.Bottom := VMovedTileRect.Bottom;

  VResultPixelRect := AGeoConverter.TileRect2PixelRect(VTileRect, VZoom);

  Result := TLocalCoordConverterNoScale.Create(
    Rect(0, 0, VResultPixelRect.Right - VResultPixelRect.Left, VResultPixelRect.Bottom - VResultPixelRect.Top),
    FProjectionFactory.GetByConverterAndZoom(AGeoConverter, VZoom),
    VResultPixelRect.TopLeft
  );
end;

function TLocalCoordConverterFactorySimpe.CreateConverter(
  ALocalRect: TRect;
  AZoom: Byte;
  AGeoConverter: ICoordConverter;
  AMapScale,
  ALocalTopLeftAtMap: TDoublePoint
): ILocalCoordConverter;
begin
  Result :=
    TLocalCoordConverter.Create(
      ALocalRect,
      FProjectionFactory.GetByConverterAndZoom(AGeoConverter, AZoom),
      AMapScale,
      ALocalTopLeftAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.CreateConverterNoScale(
  ALocalRect: TRect;
  AZoom: Byte;
  AGeoConverter: ICoordConverter;
  ALocalTopLeftAtMap: TPoint
): ILocalCoordConverter;
begin
  Result := TLocalCoordConverterNoScale.Create(
    ALocalRect,
    FProjectionFactory.GetByConverterAndZoom(AGeoConverter, AZoom),
    ALocalTopLeftAtMap
  );
end;

function TLocalCoordConverterFactorySimpe.CreateForTile(ATile: TPoint;
  AZoom: Byte; AGeoConverter: ICoordConverter): ILocalCoordConverter;
var
  VPixelRect: TRect;
  VBitmapTileRect: TRect;
begin
  VPixelRect := AGeoConverter.TilePos2PixelRect(ATile, AZoom);
  VBitmapTileRect.Left := 0;
  VBitmapTileRect.Top := 0;
  VBitmapTileRect.Right := VPixelRect.Right - VPixelRect.Left;
  VBitmapTileRect.Bottom := VPixelRect.Bottom - VPixelRect.Top;
  Result := TLocalCoordConverterNoScale.Create(
    VBitmapTileRect,
    FProjectionFactory.GetByConverterAndZoom(AGeoConverter, AZoom),
    VPixelRect.TopLeft
  );
end;

end.

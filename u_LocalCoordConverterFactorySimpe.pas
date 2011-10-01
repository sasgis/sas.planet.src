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
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe;

type
  TLocalCoordConverterFactorySimpe = class(TInterfacedObject, ILocalCoordConverterFactorySimpe)
  protected
    function CreateConverter(
      ALocalRect: TRect;
      AZoom: Byte;
      AGeoConverter: ICoordConverter;
      AMapScale: TDoublePoint;
      ALocalTopLeftAtMap: TDoublePoint
    ): ILocalCoordConverter;
    function CreateForTile(
      ATile: TPoint;
      AZoom: Byte;
      AGeoConverter: ICoordConverter
    ): ILocalCoordConverter;
    function CreateForTileRect(
      ATileRect: TRect;
      AZoom: Byte;
      AGeoConverter: ICoordConverter
    ): ILocalCoordConverter;
  end;


implementation

uses
  u_GeoFun,
  u_LocalCoordConverter;

{ TLocalCoordConverterFactorySimpe }

function TLocalCoordConverterFactorySimpe.CreateConverter(
  ALocalRect: TRect;
  AZoom: Byte;
  AGeoConverter: ICoordConverter;
  AMapScale,
  ALocalTopLeftAtMap: TDoublePoint
): ILocalCoordConverter;
begin
  Result := TLocalCoordConverter.Create(ALocalRect, AZoom, AGeoConverter, AMapScale, ALocalTopLeftAtMap);
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
  Result := CreateConverter(VBitmapTileRect, AZoom, AGeoConverter, DoublePoint(1, 1), DoublePoint(VPixelRect.TopLeft));
end;

function TLocalCoordConverterFactorySimpe.CreateForTileRect(ATileRect: TRect;
  AZoom: Byte; AGeoConverter: ICoordConverter): ILocalCoordConverter;
var
  VPixelRect: TRect;
  VBitmapTileRect: TRect;
begin
  VPixelRect := AGeoConverter.TileRect2PixelRect(ATileRect, AZoom);
  VBitmapTileRect.Left := 0;
  VBitmapTileRect.Top := 0;
  VBitmapTileRect.Right := VPixelRect.Right - VPixelRect.Left;
  VBitmapTileRect.Bottom := VPixelRect.Bottom - VPixelRect.Top;
  Result := CreateConverter(VBitmapTileRect, AZoom, AGeoConverter, DoublePoint(1, 1), DoublePoint(VPixelRect.TopLeft));
end;

end.

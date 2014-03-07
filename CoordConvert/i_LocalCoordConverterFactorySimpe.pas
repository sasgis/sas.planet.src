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

unit i_LocalCoordConverterFactorySimpe;

interface

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
  i_LocalCoordConverter;

type
  ILocalCoordConverterFactorySimpe = interface
    ['{102D5E00-4F2C-4425-9EB9-ED4DD77141FB}']
    function CreateConverter(
      const ALocalRect: TRect;
      const AZoom: Byte;
      const AGeoConverter: ICoordConverter;
      const AMapScale: Double;
      const AMapPixelAtLocalZero: TDoublePoint
    ): ILocalCoordConverter;
    function CreateConverterNoScale(
      const ALocalRect: TRect;
      const AZoom: Byte;
      const AGeoConverter: ICoordConverter;
      const AMapPixelAtLocalZero: TPoint
    ): ILocalCoordConverter;

    function ChangeCenterLonLat(
      const ASource: ILocalCoordConverter;
      const ALonLat: TDoublePoint
    ): ILocalCoordConverter;
    function ChangeCenterLonLatAndZoom(
      const ASource: ILocalCoordConverter;
      AZoom: Byte;
      const ALonLat: TDoublePoint
    ): ILocalCoordConverter;
    function ChangeByLocalDelta(
      const ASource: ILocalCoordConverter;
      const ADelta: TDoublePoint
    ): ILocalCoordConverter;
    function ChangeCenterToLocalPoint(
      const ASource: ILocalCoordConverter;
      const AVisualPoint: TPoint
    ): ILocalCoordConverter;
    function ChangeZoomWithFreezeAtVisualPoint(
      const ASource: ILocalCoordConverter;
      const AZoom: Byte;
      const AFreezePoint: TPoint
    ): ILocalCoordConverter;
    function ChangeZoomWithFreezeAtCenter(
      const ASource: ILocalCoordConverter;
      const AZoom: Byte
    ): ILocalCoordConverter;
    function ChangeConverter(
      const ASource: ILocalCoordConverter;
      const AConverter: ICoordConverter
    ): ILocalCoordConverter;

    function CreateForTile(
      const ATile: TPoint;
      const AZoom: Byte;
      const AGeoConverter: ICoordConverter
    ): ILocalCoordConverter;
    function CreateBySourceWithTileRect(
      const ASource: ILocalCoordConverter
    ): ILocalCoordConverter;
    function CreateBySourceWithTileRectAndOtherGeo(
      const ASource: ILocalCoordConverter;
      const AGeoConverter: ICoordConverter
    ): ILocalCoordConverter;
  end;

implementation

end.

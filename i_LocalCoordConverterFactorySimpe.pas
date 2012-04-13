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
  end;

implementation

end.

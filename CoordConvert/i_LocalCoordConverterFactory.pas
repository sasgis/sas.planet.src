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

unit i_LocalCoordConverterFactory;

interface

uses
  Types,
  t_GeoTypes,
  i_ProjectionInfo,
  i_LocalCoordConverter;

type
  ILocalCoordConverterFactory = interface
    ['{7CD24832-CA4B-4C8D-AE33-C7B647695629}']
    function CreateNoScaleIntDelta(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapPixelAtLocalZero: TPoint
    ): ILocalCoordConverter;
    function CreateNoScale(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapPixelAtLocalZero: TDoublePoint
    ): ILocalCoordConverter;
    function CreateScaled(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapScale: Double;
      const AMapPixelAtLocalZero: TDoublePoint
    ): ILocalCoordConverter;
  end;

implementation

end.

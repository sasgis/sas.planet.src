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

unit i_LocalCoordConverter;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes,
  i_ProjectionInfo;

type
  ILocalCoordConverter = interface
    ['{48CD8E96-6EB3-4162-B321-B8B64D71B0AB}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetIsSameConverter(const AConverter: ILocalCoordConverter): Boolean;

    function GetProjectionInfo: IProjectionInfo;
    property ProjectionInfo: IProjectionInfo read GetProjectionInfo;

    function GetScale: Double;
    function GetLocalRect: TRect;
    function GetLocalRectSize: TPoint;
    function GetRectInMapPixel: TRect;
    function GetRectInMapPixelFloat: TDoubleRect;
    function GetCenterMapPixelFloat: TDoublePoint;
    function GetCenterLonLat: TDoublePoint;

    function LocalPixel2MapPixel(
      const APoint: TPoint;
      ARounding: TPointRounding
    ): TPoint;
    function LocalPixel2MapPixelFloat(const APoint: TPoint): TDoublePoint;
    function LocalPixelFloat2MapPixelFloat(const APoint: TDoublePoint): TDoublePoint;
    function MapPixel2LocalPixel(
      const APoint: TPoint;
      ARounding: TPointRounding
    ): TPoint;
    function MapPixel2LocalPixelFloat(const APoint: TPoint): TDoublePoint;
    function MapPixelFloat2LocalPixelFloat(const APoint: TDoublePoint): TDoublePoint;

    function LocalRect2MapRect(
      const ARect: TRect;
      ARounding: TRectRounding
    ): TRect;
    function LocalRect2MapRectFloat(const ARect: TRect): TDoubleRect;
    function LocalRectFloat2MapRectFloat(const ARect: TDoubleRect): TDoubleRect;
    function MapRect2LocalRect(
      const ARect: TRect;
      ARounding: TRectRounding
    ): TRect;
    function MapRect2LocalRectFloat(const ARect: TRect): TDoubleRect;
    function MapRectFloat2LocalRectFloat(const ARect: TDoubleRect): TDoubleRect;

    function LonLat2LocalPixel(
      const APoint: TDoublePoint;
      ARounding: TPointRounding
    ): TPoint;
    function LonLat2LocalPixelFloat(const APoint: TDoublePoint): TDoublePoint;
    function LonLatRect2LocalRectFloat(const ARect: TDoubleRect): TDoubleRect;
  end;

implementation

end.

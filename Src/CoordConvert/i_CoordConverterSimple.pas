{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit i_CoordConverterSimple;

interface

uses
  Types,
  t_GeoTypes;

type
  ICoordConverterSimple = interface
    ['{3EE2987F-7681-425A-8EFE-B676C506CDD4}']

    // Converts the position of a tile at a given zoom to
    // geographic coordinates of its upper left corner
    function Pos2LonLat(
      const XY: TPoint;
      AZoom: Byte
    ): TDoublePoint; stdcall;

    // Converts geographic coordinates to the position of a tile
    // at a given zoom level that covers the given coordinates
    function LonLat2Pos(
      const APoint: TDoublePoint;
      AZoom: Byte
    ): TPoint; stdcall;

    // Converts geographic coordinates to metric coordinates and vice versa
    function LonLat2Metr(const APoint: TDoublePoint): TDoublePoint; stdcall;
    function Metr2LonLat(const APoint: TDoublePoint): TDoublePoint; stdcall;

    // Returns the number of tiles at a given zoom level
    function TilesAtZoom(const AZoom: Byte): Longint; stdcall;

    // Returns the number of pixels at the given zoom level
    function PixelsAtZoom(const AZoom: Byte): Longint; stdcall;

    // Converts the tile position at the given zoom
    // to the pixel coordinates of its upper left corner
    function TilePos2PixelPos(
      const XY: TPoint;
      const AZoom: Byte
    ): TPoint; stdcall;

    // Converts the tile position at the given zoom
    // to the pixel coordinates of its corners
    function TilePos2PixelRect(
      const XY: TPoint;
      const AZoom: Byte
    ): TRect; stdcall;
  end;

implementation

end.

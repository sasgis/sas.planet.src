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

unit i_Projection;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes,
  i_ProjectionType;

type
  IProjection = interface
    ['{1BAC7D2B-21F1-4DA7-AE3B-F9D91548E440}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetProjectionType: IProjectionType;
    property ProjectionType: IProjectionType read GetProjectionType;

    function IsSame(const AProjection: IProjection): Boolean;

    // Returns the rectangle of tiles allowed at the given zoom level
    function GetTileRect: TRect;

    // Returns the rectangle of pixels allowed at the given zoom level
    function GetPixelRect: TRect;

    // Returns the total number of pixels at the given zoom level
    function GetPixelsFloat: Double;

    // Returns the ID of the tile division type
    // (for future reference, to implement custom tile sizes)
    function GetTileSplitCode: Integer;

    function GetTileSize(
      const APoint: TPoint
    ): TPoint;

    // Converts pixel coordinates to relative coordinates
    function PixelPos2Relative(
      const APoint: TPoint
    ): TDoublePoint;

    // Converts pixel coordinates to geographic coordinates
    function PixelPos2LonLat(
      const APoint: TPoint
    ): TDoublePoint;

    function PixelPos2TilePosFloat(
      const APoint: TPoint
    ): TDoublePoint;

    function PixelPosFloat2TilePosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    function PixelPosFloat2Relative(
      const APoint: TDoublePoint
    ): TDoublePoint;

    function PixelPosFloat2LonLat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    // Calculates the tile rectangle covering the pixel rectangle
    function PixelRect2TileRect(
      const ARect: TRect
    ): TRect;

    // Converts the coordinates of a rectangle of pixels to relative coordinates
    function PixelRect2RelativeRect(
      const ARect: TRect
    ): TDoubleRect;

    // Converts the coordinates of a rectangle of pixels to geographic coordinates
    function PixelRect2LonLatRect(
      const ARect: TRect
    ): TDoubleRect;

    function PixelRect2TileRectFloat(
      const ARect: TRect
    ): TDoubleRect;

    function PixelRectFloat2TileRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;

    function PixelRectFloat2RelativeRect(
      const ARect: TDoubleRect
    ): TDoubleRect;

    function PixelRectFloat2LonLatRect(
      const ARect: TDoubleRect
    ): TDoubleRect;

    // Converts the tile position at the given zoom to the pixel coordinates of its upper left corner
    function TilePos2PixelPos(
      const APoint: TPoint
    ): TPoint;

    // Converts the tile position at a given zoom to the pixel numbers of its corners
    function TilePos2PixelRect(
      const APoint: TPoint
    ): TRect;

    function TilePos2PixelRectFloat(
      const APoint: TPoint
    ): TDoubleRect;

    // Converts the tile position to the relative coordinates
    function TilePos2Relative(
      const APoint: TPoint
    ): TDoublePoint;

    // Converts the tile position at a given zoom to the pixel numbers of its corners
    function TilePos2RelativeRect(
      const APoint: TPoint
    ): TDoubleRect;

    // Converts tile coordinates to geographic coordinates
    function TilePos2LonLat(
      const APoint: TPoint
    ): TDoublePoint;

    // Converts the tile position at a given zoom to the geographic coordinates of its corners
    function TilePos2LonLatRect(
      const APoint: TPoint
    ): TDoubleRect;

    function TilePosFloat2PixelPosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    function TilePosFloat2Relative(
      const APoint: TDoublePoint
    ): TDoublePoint;

    function TilePosFloat2LonLat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    // Calculates the pixel coordinates of the vertices of the tile rectangle
    function TileRect2PixelRect(
      const ARect: TRect
    ): TRect;

    // Calculates the relative coordinates of the vertices of the tile rectangle
    function TileRect2RelativeRect(
      const ARect: TRect
    ): TDoubleRect;

    // Converts a tile rectangle at a given zoom to the geographic coordinates of its corners
    function TileRect2LonLatRect(
      const ARect: TRect
    ): TDoubleRect;

    function TileRectFloat2PixelRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;

    function TileRectFloat2RelativeRect(
      const ARect: TDoubleRect
    ): TDoubleRect;

    function TileRectFloat2LonLatRect(
      const ARect: TDoubleRect
    ): TDoubleRect;

    // Converts relative coordinates to pixel coordinates
    function Relative2PixelPosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    // Converts relative coordinates to tile coordinates
    function Relative2TilePosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    // Converts a rectangle with relative coordinates to a pixel rectangle
    function RelativeRect2PixelRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;

    // Converts a rectangle with relative coordinates to a tile rectangle
    function RelativeRect2TileRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;

    // Converts geographic coordinates to the coordinates of a pixel at a given zoom
    // that covers the given coordinates
    function LonLat2PixelPosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    // Converts geographic coordinates to the position of a tile at a given zoom
    // that covers the given coordinates
    function LonLat2TilePosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    function LonLatRect2PixelRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;

    function LonLatRect2TileRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;

    // Validate
    procedure ValidateTilePos(
      var APoint: TPoint;
      ACicleMap: Boolean
    );
    procedure ValidateTilePosStrict(
      var APoint: TPoint;
      ACicleMap: Boolean
    );
    procedure ValidateTileRect(
      var ARect: TRect
    );
    procedure ValidatePixelPos(
      var APoint: TPoint;
      ACicleMap: Boolean
    );
    procedure ValidatePixelPosFloat(
      var APoint: TDoublePoint;
      ACicleMap: Boolean
    );
    procedure ValidatePixelPosStrict(
      var APoint: TPoint;
      ACicleMap: Boolean
    );
    procedure ValidatePixelPosFloatStrict(
      var APoint: TDoublePoint;
      ACicleMap: Boolean
    );
    procedure ValidatePixelRect(
      var ARect: TRect
    );
    procedure ValidatePixelRectFloat(
      var ARect: TDoubleRect
    );

    // Check
    function CheckTilePos(
      const APoint: TPoint
    ): Boolean;
    function CheckTilePosStrict(
      const APoint: TPoint
    ): Boolean;
    function CheckTileRect(
      const ARect: TRect
    ): Boolean;
    function CheckPixelPos(
      const APoint: TPoint
    ): Boolean;
    function CheckPixelPosFloat(
      const APoint: TDoublePoint
    ): Boolean;
    function CheckPixelPosStrict(
      const APoint: TPoint
    ): Boolean;
    function CheckPixelPosFloatStrict(
      const APoint: TDoublePoint
    ): Boolean;
    function CheckPixelRect(
      const ARect: TRect
    ): Boolean;
    function CheckPixelRectFloat(
      const ARect: TDoubleRect
    ): Boolean;
  end;

implementation

end.

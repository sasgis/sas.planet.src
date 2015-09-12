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

unit i_ProjectionInfo;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes,
  i_ProjectionType,
  i_CoordConverter;

type
  IProjectionInfo = interface
    ['{1BAC7D2B-21F1-4DA7-AE3B-F9D91548E440}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetProjectionType: IProjectionType;
    property ProjectionType: IProjectionType read GetProjectionType;

    function GetIsSameProjectionInfo(const AProjection: IProjectionInfo): Boolean;

    // Возвращает прямоугольник тайлов допустимый в заданном зуме
    function GetTileRect: TRect;
    // Возвращает прямоугольник пикселов допустимый в заданном зуме
    function GetPixelRect: TRect;

    // Возвращает общее количество пикселей на заданном зуме
    function GetPixelsFloat: Double;

    // Возвращает код типа нарезки на тайлы (на будущее, вдруг реализую произвольный размер тайлов)
    function GetTileSplitCode: Integer;

    // Преобразует координаты пиксела в  координаты тайда cодержащего пиксель
    function PixelPos2TilePos(
      const APoint: TPoint;
      ARounding: TPointRounding
    ): TPoint;
    // Преобразует координаты пиксела в относительные координаты на карте (x/PixelsAtZoom)
    function PixelPos2Relative(
      const APoint: TPoint
    ): TDoublePoint;
    // Преобразует координаты пиксела в географические координаты
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

    // вычисляет прямоугольник тайлов покрывающий прямоугольник пикселов
    function PixelRect2TileRect(
      const ARect: TRect
    ): TRect;
    // Преобразует координаты прямоугольника пикселов в относительные координаты на карте (x/PixelsAtZoom)
    function PixelRect2RelativeRect(
      const ARect: TRect
    ): TDoubleRect;
    // Преобразует координаты прямоугольника пикселов в географические координаты на карте
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

    // Преобразует позицию тайла заданного зума в координаты пиксела его левого верхнего угла
    function TilePos2PixelPos(
      const APoint: TPoint
    ): TPoint;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2PixelRect(
      const APoint: TPoint
    ): TRect;
    function TilePos2PixelRectFloat(
      const APoint: TPoint
    ): TDoubleRect;
    // Преобразует координаты тайла в относительные координаты на карте (x/PixelsAtZoom)
    function TilePos2Relative(
      const APoint: TPoint
    ): TDoublePoint;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2RelativeRect(
      const APoint: TPoint
    ): TDoubleRect;
    // Преобразует координаты тайла в географические координаты
    function TilePos2LonLat(
      const APoint: TPoint
    ): TDoublePoint;
    // Преобразует позицию тайла заданного зума в географические координаты его углов
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

    // вычисляет координты пикселей вершин прямоугольника тайлов
    function TileRect2PixelRect(
      const ARect: TRect
    ): TRect;
    // вычисляет относительные координты вершин прямоугольника тайлов
    function TileRect2RelativeRect(
      const ARect: TRect
    ): TDoubleRect;
    // Преобразует прямоугольник тайлов заданного зума в географические координаты его углов
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

    // Перобразует относительные координаты на карте в координаты пиксела
    function Relative2PixelPosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;
    // Перобразует относительные координаты на карте в координаты тайла
    function Relative2TilePosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    // Преобразует прямоугольник с относительными координатами в прямоугольник пикселов
    function RelativeRect2PixelRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;
    // Преобразует прямоугольник с относительными координатами в прямоугольник тайлов
    function RelativeRect2TileRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;

    // Преобразует георафические координаты в координаты пиксела на заданном зуме накрывающего данные координаты
    function LonLat2PixelPosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;
    // Преобразует георафические координаты в позицию тайла на заданном зуме накрывающего данные координаты
    function LonLat2TilePosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    function LonLatRect2PixelRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;
    function LonLatRect2TileRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;

    function GetTileSize(
      const APoint: TPoint
    ): TPoint;

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

    function CheckTilePos(
      const APoint: TPoint
    ): boolean;
    function CheckTilePosStrict(
      const APoint: TPoint
    ): boolean;
    function CheckTileRect(
      const ARect: TRect
    ): boolean;

    function CheckPixelPos(
      const APoint: TPoint
    ): boolean;
    function CheckPixelPosFloat(
      const APoint: TDoublePoint
    ): boolean;
    function CheckPixelPosStrict(
      const APoint: TPoint
    ): boolean;
    function CheckPixelPosFloatStrict(
      const APoint: TDoublePoint
    ): boolean;
    function CheckPixelRect(
      const ARect: TRect
    ): boolean;
    function CheckPixelRectFloat(
      const ARect: TDoubleRect
    ): boolean;

  end;

implementation

end.

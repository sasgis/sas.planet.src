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

unit i_CoordConverter;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes,
  i_Datum;

type
  ICoordConverterSimple = interface
    ['{3EE2987F-7681-425A-8EFE-B676C506CDD4}']

    // Преобразует позицию тайла на заданном зуме в георафически координаты его верхнего левого угла
    function Pos2LonLat(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; stdcall;
    // Преобразует георафические координаты в позицию тайла на заданном зуме накрывающего данные координаты
    function LonLat2Pos(
      const Ll: TDoublePoint;
      AZoom: byte
    ): Tpoint; stdcall;

    // метрические координаты
    function LonLat2Metr(const Ll: TDoublePoint): TDoublePoint; stdcall;
    function Metr2LonLat(const Mm: TDoublePoint): TDoublePoint; stdcall;

    // Возвращает количество тайлов в заданном зуме
    function TilesAtZoom(const AZoom: byte): Longint; stdcall;
    // Возвращает общее количество пикселей на заданном зуме
    function PixelsAtZoom(const AZoom: byte): Longint; stdcall;

    // Преобразует позицию тайла заданного зума в координаты пиксела его левого верхнего угла
    function TilePos2PixelPos(
      const XY: TPoint;
      const AZoom: byte
    ): TPoint; stdcall;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2PixelRect(
      const XY: TPoint;
      const AZoom: byte
    ): TRect; stdcall;
  end;

  ICoordConverter = interface
    ['{E8884111-C538-424F-92BC-1BC9843EA6BB}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetDatum: IDatum; stdcall;
    property Datum: IDatum read GetDatum;

    // Минимальный и максимальный зумы обычно с 0 до 23
    function GetMinZoom: Byte; stdcall;
    property MinZoom: Byte read GetMinZoom;

    function GetMaxZoom: Byte; stdcall;
    property MaxZoom: Byte read GetMaxZoom;

    // Возвращает прямоугольник тайлов допустимый в заданном зуме
    function TileRectAtZoom(const AZoom: byte): TRect; stdcall;
    // Возвращает прямоугольник пикселов допустимый в заданном зуме
    function PixelRectAtZoom(const AZoom: byte): TRect; stdcall;

    // Возвращает общее количество пикселей на заданном зуме
    function PixelsAtZoomFloat(const AZoom: byte): Double; stdcall;

    // Преобразует координаты пиксела в  координаты тайда cодержащего пиксель
    function PixelPos2TilePos(
      const XY: TPoint;
      const AZoom: byte;
      ARounding: TPointRounding
    ): TPoint; stdcall;
    // Преобразует координаты пиксела в относительные координаты на карте (x/PixelsAtZoom)
    function PixelPos2Relative(
      const XY: TPoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    // Преобразует координаты пиксела в географические координаты
    function PixelPos2LonLat(
      const XY: TPoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;//TODO: Автотест
    function PixelPos2TilePosFloat(
      const XY: TPoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;//TODO: Автотест

    function PixelPosFloat2TilePosFloat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;//TODO: Автотест
    function PixelPosFloat2Relative(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;//TODO: Автотест
    function PixelPosFloat2LonLat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;//TODO: Автотест

    // вычисляет прямоугольник тайлов покрывающий прямоугольник пикселов
    function PixelRect2TileRect(
      const XY: TRect;
      const AZoom: byte
    ): TRect; stdcall;
    // Преобразует координаты прямоугольника пикселов в относительные координаты на карте (x/PixelsAtZoom)
    function PixelRect2RelativeRect(
      const XY: TRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    // Преобразует координаты прямоугольника пикселов в географические координаты на карте
    function PixelRect2LonLatRect(
      const XY: TRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function PixelRect2TileRectFloat(
      const XY: TRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест

    function PixelRectFloat2TileRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест
    function PixelRectFloat2RelativeRect(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест
    function PixelRectFloat2LonLatRect(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест

    // Преобразует позицию тайла заданного зума в координаты пиксела его левого верхнего угла
    function TilePos2PixelPos(
      const XY: TPoint;
      const AZoom: byte
    ): TPoint; stdcall;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2PixelRect(
      const XY: TPoint;
      const AZoom: byte
    ): TRect; stdcall;
    function TilePos2PixelRectFloat(
      const XY: TPoint;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    // Преобразует координаты тайла в относительные координаты на карте (x/PixelsAtZoom)
    function TilePos2Relative(
      const XY: TPoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2RelativeRect(
      const XY: TPoint;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    // Преобразует координаты тайла в географические координаты
    function TilePos2LonLat(
      const XY: TPoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;//TODO: Автотест
    // Преобразует позицию тайла заданного зума в географические координаты его углов
    function TilePos2LonLatRect(
      const XY: TPoint;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест

    function TilePosFloat2PixelPosFloat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;//TODO: Автотест
    function TilePosFloat2Relative(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;//TODO: Автотест
    function TilePosFloat2LonLat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;//TODO: Автотест

    // вычисляет координты пикселей вершин прямоугольника тайлов
    function TileRect2PixelRect(
      const XY: TRect;
      const AZoom: byte
    ): TRect; stdcall;//TODO: Автотест
    // вычисляет относительные координты вершин прямоугольника тайлов
    function TileRect2RelativeRect(
      const XY: TRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест
    // Преобразует прямоугольник тайлов заданного зума в географические координаты его углов
    function TileRect2LonLatRect(
      const XY: TRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест

    function TileRectFloat2PixelRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест
    function TileRectFloat2RelativeRect(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест
    function TileRectFloat2LonLatRect(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест

    // Перобразует относительные координаты на карте в координаты пиксела
    function Relative2PixelPosFloat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;//TODO: Автотест
    // Перобразует относительные координаты на карте в координаты тайла
    function Relative2TilePosFloat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    // Перобразует относительные координаты на карте в географические
    function Relative2LonLat(const XY: TDoublePoint): TDoublePoint; stdcall;//TODO: Автотест

    // Преобразует прямоугольник с относительными координатами в прямоугольник пикселов
    function RelativeRect2PixelRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    // Преобразует прямоугольник с относительными координатами в прямоугольник тайлов
    function RelativeRect2TileRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    // Перобразует прямоугольник с относительными координатами на карте в географические
    function RelativeRect2LonLatRect(const XY: TDoubleRect): TDoubleRect; stdcall;//TODO: Автотест

    // Преобразует георафические координаты в координаты пиксела на заданном зуме накрывающего данные координаты
    function LonLat2PixelPosFloat(
      const Ll: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    // Преобразует георафические координаты в позицию тайла на заданном зуме накрывающего данные координаты
    function LonLat2TilePosFloat(
      const Ll: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    // Преобразует географические коодинаты в относительные координаты на карте
    function LonLat2Relative(const XY: TDoublePoint): TDoublePoint; stdcall;//TODO: Автотест

    // Преобразует прямоугольник в географических коодинатах в относительные координаты на карте
    function LonLatRect2RelativeRect(const XY: TDoubleRect): TDoubleRect; stdcall;//TODO: Автотест
    function LonLatRect2PixelRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест
    function LonLatRect2TileRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;//TODO: Автотест

    function GetTileSize(
      const XY: TPoint;
      const AZoom: byte
    ): TPoint; stdcall;

    procedure ValidateZoom(var AZoom: Byte); stdcall;
    procedure ValidateTilePos(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); stdcall;
    procedure ValidateTilePosStrict(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); stdcall;
    procedure ValidateTileRect(
      var XY: TRect;
      var AZoom: byte
    ); stdcall;

    procedure ValidatePixelPos(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); stdcall;
    procedure ValidatePixelPosFloat(
      var XY: TDoublePoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); stdcall;
    procedure ValidatePixelPosStrict(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); stdcall;
    procedure ValidatePixelPosFloatStrict(
      var XY: TDoublePoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); stdcall;
    procedure ValidatePixelRect(
      var XY: TRect;
      var AZoom: byte
    ); stdcall;
    procedure ValidatePixelRectFloat(
      var XY: TDoubleRect;
      var AZoom: byte
    ); stdcall;

    procedure ValidateRelativePos(var XY: TDoublePoint); stdcall;
    procedure ValidateRelativeRect(var XY: TDoubleRect); stdcall;

    procedure ValidateLonLatPos(var XY: TDoublePoint); stdcall;
    procedure ValidateLonLatRect(var XY: TDoubleRect); stdcall;

    function CheckZoom(const AZoom: Byte): boolean; stdcall;
    function CheckTilePos(
      const XY: TPoint;
      const AZoom: byte
    ): boolean; stdcall;
    function CheckTilePosStrict(
      const XY: TPoint;
      const AZoom: byte
    ): boolean; stdcall;
    function CheckTileRect(
      const XY: TRect;
      const AZoom: byte
    ): boolean; stdcall;

    function CheckPixelPos(
      const XY: TPoint;
      const AZoom: byte
    ): boolean; stdcall;
    function CheckPixelPosFloat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): boolean; stdcall;
    function CheckPixelPosStrict(
      const XY: TPoint;
      const AZoom: byte
    ): boolean; stdcall;
    function CheckPixelPosFloatStrict(
      const XY: TDoublePoint;
      const AZoom: byte
    ): boolean; stdcall;
    function CheckPixelRect(
      const XY: TRect;
      const AZoom: byte
    ): boolean; stdcall;
    function CheckPixelRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): boolean; stdcall;

    function CheckRelativePos(const XY: TDoublePoint): boolean; stdcall;
    function CheckRelativeRect(const XY: TDoubleRect): boolean; stdcall;

    function CheckLonLatPos(const XY: TDoublePoint): boolean; stdcall;
    function CheckLonLatRect(const XY: TDoubleRect): boolean; stdcall;

    // Возвращает код EPSG для этой проекции. Для нестандартных проекций и сфероидов будет возвращать 0
    function GetProjectionEPSG: Integer; stdcall;
    property ProjectionEPSG: Integer read GetProjectionEPSG;

    // Возвращает код типа нарезки на тайлы (на будущее, вдруг реализую произвольный размер тайлов)
    function GetTileSplitCode: Integer; stdcall;
    // Возвращает является ли другой конвертер эквивалентным текущему
    function IsSameConverter(const AOtherMapCoordConv: ICoordConverter): Boolean; stdcall;

    // Преобразует георафические координаты в метрические, и обратно
    function LonLat2Metr(const Ll: TDoublePoint): TDoublePoint; stdcall;
    function Metr2LonLat(const Mm: TDoublePoint): TDoublePoint; stdcall;
  end;

implementation

end.

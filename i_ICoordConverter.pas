unit i_ICoordConverter;

interface

uses
  Types,
  t_GeoTypes;

type
  ICoordConverterSimple = interface
    ['{3EE2987F-7681-425A-8EFE-B676C506CDD4}']

    // Преобразует позицию тайла на заданном зуме в георафически координаты его верхнего левого угла
    function Pos2LonLat(const XY: TPoint; Azoom: byte): TExtendedPoint; stdcall;
    // Преобразует георафические координаты в позицию тайла на заданном зуме накрывающего данные координаты
    function LonLat2Pos(const Ll: TExtendedPoint; Azoom: byte): Tpoint; stdcall;
    // ?????????
    function LonLat2Metr(const Ll: TExtendedPoint): TExtendedPoint; stdcall;

    // Возвращает количество тайлов в заданном зуме
    function TilesAtZoom(AZoom: byte): Longint; stdcall;
    // Возвращает общее количество пикселей на заданном зуме
    function PixelsAtZoom(AZoom: byte): Longint; stdcall;

    // Преобразует позицию тайла заданного зума в координаты пиксела его левого верхнего угла
    function TilePos2PixelPos(const XY: TPoint; Azoom: byte): TPoint; stdcall;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2PixelRect(const XY: TPoint; Azoom: byte): TRect; stdcall;
  end;

  ICoordConverter = interface
    ['{E8884111-C538-424F-92BC-1BC9843EA6BB}']
    // Возвращает количество тайлов в заданном зуме
    function TilesAtZoom(AZoom: byte): Longint; stdcall;
    function TilesAtZoomFloat(AZoom: byte): Extended; stdcall;
    // Возвращает общее количество пикселей на заданном зуме
    function PixelsAtZoom(AZoom: byte): Longint; stdcall;
    function PixelsAtZoomFloat(AZoom: byte): Extended; stdcall;

    // Преобразует позицию тайла заданного зума в координаты пиксела его левого верхнего угла
    function TilePos2PixelPos(const XY: TPoint; Azoom: byte): TPoint; stdcall;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2PixelRect(const XY: TPoint; Azoom: byte): TRect; stdcall;
    // Преобразует координаты тайла в относительные координаты на карте (x/PixelsAtZoom)
    function TilePos2Relative(const XY: TPoint; Azoom: byte): TExtendedPoint; stdcall;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2RelativeRect(const XY: TPoint; Azoom: byte): TExtendedRect; stdcall;
    // Преобразует координаты тайла в географические координаты
    function TilePos2LonLat(const XY: TPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест
    // Преобразует позицию тайла заданного зума в географические координаты его углов
    function TilePos2LonLatRect(const XY: TPoint; Azoom: byte): TExtendedRect; stdcall;//TODO: Автотест

//    function TilePosFloat2TilePos(const XY: TExtendedPoint; Azoom: byte): TPoint; stdcall;//TODO: Автотест
//    function TilePosFloat2PixelPos(const XY: TExtendedPoint; Azoom: byte): TPoint; stdcall;//TODO: Автотест
//    function TilePosFloat2PixelPosFloat(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест
//    function TilePosFloat2Relative(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест
//    function TilePosFloat2LonLat(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест

    // вычисляет координты пикселей вершин прямоугольника тайлов
    function TileRect2PixelRect(const XY: TRect; AZoom: byte): TRect; stdcall;//TODO: Автотест
    // вычисляет относительные координты вершин прямоугольника тайлов
    function TileRect2RelativeRect(const XY: TRect; AZoom: byte): TExtendedRect; stdcall;//TODO: Автотест
    // Преобразует прямоугольник тайлов заданного зума в географические координаты его углов
    function TileRect2LonLatRect(const XY: TRect; Azoom: byte): TExtendedRect; stdcall;//TODO: Автотест

//    function TileRectFloat2TileRect(const XY: TExtendedRect; AZoom: byte): TRect; stdcall;//TODO: Автотест
//    function TileRectFloat2PixelRect(const XY: TExtendedRect; AZoom: byte): TRect; stdcall;//TODO: Автотест
//    function TileRectFloat2PixelRectFloat(const XY: TExtendedRect; AZoom: byte): TExtendedRect; stdcall;//TODO: Автотест
//    function TileRectFloat2RelativeRect(const XY: TExtendedRect; AZoom: byte): TExtendedRect; stdcall;//TODO: Автотест
//    function TileRectFloat2LonLatRect(const XY: TExtendedRect; Azoom: byte): TExtendedRect; stdcall;//TODO: Автотест

    // Преобразует координаты пиксела в  координаты тайда cодержащего пиксель
    function PixelPos2TilePos(const XY: TPoint; Azoom: byte): TPoint; stdcall;
    // Преобразует координаты пиксела в относительные координаты на карте (x/PixelsAtZoom)
    function PixelPos2Relative(const XY: TPoint; Azoom: byte): TExtendedPoint; stdcall;
    // Преобразует координаты пиксела в географические координаты
    function PixelPos2LonLat(const XY: TPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест
//    function PixelPos2TilePosFloat(const XY: TPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест

//    function PixelPosFloat2PixelPos(const XY: TExtendedPoint; Azoom: byte): TPoint; stdcall;//TODO: Автотест
//    function PixelPosFloat2TilePos(const XY: TExtendedPoint; Azoom: byte): TPoint; stdcall;//TODO: Автотест
//    function PixelPosFloat2TilePosFloat(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест
//    function PixelPosFloat2Relative(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест
//    function PixelPosFloat2LonLat(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест

    // вычисляет прямоугольник тайлов покрывающий прямоугольник пикселов
    function PixelRect2TileRect(const XY: TRect; AZoom: byte): TRect; stdcall;
    // Преобразует координаты прямоугольника пикселов в относительные координаты на карте (x/PixelsAtZoom)
    function PixelRect2RelativeRect(const XY: TRect; AZoom: byte): TExtendedRect; stdcall;
    // Преобразует координаты прямоугольника пикселов в географические координаты на карте
    function PixelRect2LonLatRect(const XY: TRect; AZoom: byte): TExtendedRect; stdcall;
//    function PixelRect2TileRectFloat(const XY: TRect; AZoom: byte): TExtendedRect; stdcall;//TODO: Автотест

//    function PixelRectFloat2PixelRect(const XY: TExtendedRect; AZoom: byte): TRect; stdcall;//TODO: Автотест
//    function PixelRectFloat2TileRect(const XY: TExtendedRect; AZoom: byte): TRect; stdcall;//TODO: Автотест
//    function PixelRectFloat2TileRectFloat(const XY: TExtendedRect; AZoom: byte): TExtendedRect; stdcall;//TODO: Автотест
//    function PixelRectFloat2RelativeRect(const XY: TExtendedRect; AZoom: byte): TExtendedRect; stdcall;//TODO: Автотест
//    function PixelRectFloat2LonLatRect(const XY: TExtendedRect; AZoom: byte): TExtendedRect; stdcall;//TODO: Автотест

    // Перобразует относительные координаты на карте в координаты пиксела
    function Relative2Pixel(const XY: TExtendedPoint; Azoom: byte): TPoint; stdcall;
    function Relative2PixelPosFloat(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест
    // Перобразует относительные координаты на карте в координаты тайла
    function Relative2Tile(const XY: TExtendedPoint; Azoom: byte): TPoint; stdcall;
    function Relative2TilePosFloat(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;
    // Перобразует относительные координаты на карте в географические
    function Relative2LonLat(const XY: TExtendedPoint): TExtendedPoint; stdcall;//TODO: Автотест
    // Преобразует прямоугольник с относительными координатами в прямоугольник пикселов
    function RelativeRect2PixelRect(const XY: TExtendedRect; Azoom: byte): TRect; stdcall;
    function RelativeRect2PixelRectFloat(const XY: TExtendedRect; Azoom: byte): TExtendedRect; stdcall;
    // Преобразует прямоугольник с относительными координатами в прямоугольник тайлов
    function RelativeRect2TileRect(const XY: TExtendedRect; Azoom: byte): TRect; stdcall;
    function RelativeRect2TileRectFloat(const XY: TExtendedRect; Azoom: byte): TExtendedRect; stdcall;
    // Перобразует прямоугольник с относительными координатами на карте в географические
    function RelativeRect2LonLatRect(const XY: TExtendedRect): TExtendedRect; stdcall;//TODO: Автотест

    // Преобразует георафические координаты в координаты пиксела на заданном зуме накрывающего данные координаты
    function LonLat2PixelPos(const Ll: TExtendedPoint; Azoom: byte): Tpoint; stdcall;//TODO: Автотест
    function LonLat2PixelPosFloat(const Ll: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;
    // Преобразует георафические координаты в позицию тайла на заданном зуме накрывающего данные координаты
    function LonLat2TilePos(const Ll: TExtendedPoint; Azoom: byte): Tpoint; stdcall;//TODO: Автотест
    function LonLat2TilePosFloat(const Ll: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;
    // Преобразует географические коодинаты в относительные координаты на карте
    function LonLat2Relative(const XY: TExtendedPoint): TExtendedPoint; stdcall;//TODO: Автотест
    // Преобразует прямоугольник в географических коодинатах в относительные координаты на карте
    function LonLatRect2RelativeRect(const XY: TExtendedRect): TExtendedRect; stdcall;//TODO: Автотест
    function LonLatRect2PixelRect(const XY: TExtendedRect; Azoom: byte): TRect; stdcall;//TODO: Автотест
    function LonLatRect2TileRect(const XY: TExtendedRect; Azoom: byte): TRect; stdcall;//TODO: Автотест

    function LonLatArray2PixelArray(APolyg: TExtendedPointArray; AZoom: byte): TPointArray; stdcall;
    function LonLatArray2PixelArrayFloat(APolyg: TExtendedPointArray; AZoom: byte): TExtendedPointArray; stdcall;

    function GetTileSize(const XY: TPoint; Azoom: byte): TPoint; stdcall;
    function Pos2OtherMap(XY: TPoint; Azoom: byte; AOtherMapCoordConv: ICoordConverter): TPoint;
    function CalcPoligonArea(polygon: TExtendedPointArray): Extended;
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended;

    function CheckZoom(var AZoom: Byte): boolean; stdcall;
    function CheckTilePos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; stdcall;
    function CheckTilePosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; stdcall;
    function CheckTileRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean; stdcall;

    function CheckPixelPos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; stdcall;
    function CheckPixelPosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; stdcall;
    function CheckPixelRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean; stdcall;

    function CheckRelativePos(var XY: TExtendedPoint): boolean; stdcall;
    function CheckRelativeRect(var XY: TExtendedRect): boolean; stdcall;

    function CheckLonLatPos(var XY: TExtendedPoint): boolean; stdcall;
    function CheckLonLatRect(var XY: TExtendedRect): boolean; stdcall;

    // Возвращает код EPSG для этой проекции. Для нестандартных проекций и сфероидов будет возвращать 0
    function GetProjectionEPSG: Integer; stdcall;
    // Возвращает код EPSG для этого датума. Для нестандартных проекций и сфероидов будет возвращать 0
    function GetDatumEPSG: integer; stdcall;
    // Возвращает радиус сфероида.
    function GetSpheroidRadius: Double; stdcall;
    // Возвращает единицы измерения используемые в спроецированной карте
    function GetCellSizeUnits: TCellSizeUnits; stdcall;
    // Возвращает код типа нарезки на тайлы (на будущее, вдруг реализую произвольный размер тайлов)
    function GetTileSplitCode: Integer; stdcall;


    // ?????????
    function LonLat2Metr(const Ll: TExtendedPoint): TExtendedPoint; stdcall;
  end;

implementation

end.

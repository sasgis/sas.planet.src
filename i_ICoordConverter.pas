unit i_ICoordConverter;

interface

uses
  Types,
  t_GeoTypes;

type
  ICoordConverter = interface
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
    // Преобразует координаты тайла в относительные координаты на карте (x/PixelsAtZoom)
    function TilePos2Relative(const XY: TPoint; Azoom: byte): TExtendedPoint; stdcall;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2RelativeRect(const XY: TPoint; Azoom: byte): TExtendedRect; stdcall;
    // вычисляет координты пикселей вершин прямоугольника тайлов
    function TileRect2PixelRect(const XY: TRect; AZoom: byte): TRect; stdcall;//TODO: Автотест
    // Преобразует координаты тайла в географические координаты
    function TilePos2LonLat(const XY: TPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест
    // Преобразует позицию тайла заданного зума в географические координаты его углов
    function TilePos2LonLatRect(const XY: TPoint; Azoom: byte): TExtendedRect; stdcall;//TODO: Автотест

    // Преобразует координаты пиксела в  координаты тайда cодержащего пиксель
    function PixelPos2TilePos(const XY: TPoint; Azoom: byte): TPoint; stdcall;
    // Преобразует координаты пиксела в относительные координаты на карте (x/PixelsAtZoom)
    function PixelPos2Relative(const XY: TPoint; Azoom: byte): TExtendedPoint; stdcall;
    // Преобразует координаты пиксела в географические координаты
    function PixelPos2LonLat(const XY: TPoint; Azoom: byte): TExtendedPoint; stdcall;//TODO: Автотест
    // вычисляет прямоугольник тайлов покрывающий прямоугольник пикселов
    function PixelRect2TileRect(const XY: TRect; AZoom: byte): TRect; stdcall;
    // Преобразует координаты прямоугольника пикселов в относительные координаты на карте (x/PixelsAtZoom)
    function PixelRect2RelativeRect(const XY: TRect; AZoom: byte): TExtendedRect; stdcall;
    // Преобразует координаты прямоугольника пикселов в географические координаты на карте
    function PixelRect2LonLatRect(const XY: TRect; AZoom: byte): TExtendedRect; stdcall;

    // Перобразует относительные координаты на карте в координаты пиксела
    function Relative2Pixel(const XY: TExtendedPoint; Azoom: byte): TPoint; stdcall;
    // Перобразует относительные координаты на карте в координаты тайла
    function Relative2Tile(const XY: TExtendedPoint; Azoom: byte): TPoint; stdcall;
    // Перобразует относительные координаты на карте в географические
    function Relative2LonLat(const XY: TExtendedPoint): TExtendedPoint; stdcall;//TODO: Автотест
    // Преобразует прямоугольник с относительными координатами в прямоугольник пикселов
    function RelativeRect2PixelRect(const XY: TExtendedRect; Azoom: byte): TRect; stdcall;
    // Преобразует прямоугольник с относительными координатами в прямоугольник тайлов
    function RelativeRect2TileRect(const XY: TExtendedRect; Azoom: byte): TRect; stdcall;
    // Перобразует прямоугольник с относительными координатами на карте в географические
    function RelativeRect2LonLatRect(const XY: TExtendedRect): TExtendedRect; stdcall;//TODO: Автотест

    // Преобразует георафические координаты в координаты пиксела на заданном зуме накрывающего данные координаты
    function LonLat2PixelPos(const Ll: TExtendedPoint; Azoom: byte): Tpoint; stdcall;//TODO: Автотест
    function LonLat2PixelPosf(const Ll: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;
    // Преобразует георафические координаты в позицию тайла на заданном зуме накрывающего данные координаты
    function LonLat2TilePos(const Ll: TExtendedPoint; Azoom: byte): Tpoint; stdcall;//TODO: Автотест
    function LonLat2TilePosf(const Ll: TExtendedPoint; Azoom: byte): TExtendedPoint; stdcall;
    // Преобразует географические коодинаты в относительные координаты на карте
    function LonLat2Relative(const XY: TExtendedPoint): TExtendedPoint; stdcall;//TODO: Автотест
    // Преобразует прямоугольник в географических коодинатах в относительные координаты на карте
    function LonLatRect2RelativeRect(const XY: TExtendedRect): TExtendedRect; stdcall;//TODO: Автотест

    function Pos2OtherMap(XY: TPoint; Azoom: byte; AOtherMapCoordConv: ICoordConverter):TPoint;
    function CalcPoligonArea(polygon:TExtendedPointArray): Extended;
    function PoligonProject(AZoom:byte; APolyg: TExtendedPointArray): TPointArray;
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended;

    procedure CheckZoom(var AZoom: Byte); stdcall;
    procedure CheckTilePos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean); stdcall;
    procedure CheckTilePosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean); stdcall;
    procedure CheckTileRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean); stdcall;

    procedure CheckPixelPos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean); stdcall;
    procedure CheckPixelPosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean); stdcall;
    procedure CheckPixelRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean); stdcall;

    procedure CheckRelativePos(var XY: TExtendedPoint); stdcall;
    procedure CheckRelativeRect(var XY: TExtendedRect); stdcall;

    procedure CheckLonLatPos(var XY: TExtendedPoint); stdcall;
    procedure CheckLonLatRect(var XY: TExtendedRect); stdcall;

  end;

implementation

end.
 
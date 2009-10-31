unit u_CoordConverterAbstract;

interface

uses
  Types,
  t_GeoTypes;

type
  ICoordConverter = interface
  ['{3EE2987F-7681-425A-8EFE-B676C506CDD4}']
    // Преобразует позицию тайла на заданном зуме в георафически координаты его верхнего левого угла
    function Pos2LonLat(const XY : TPoint; Azoom : byte) : TExtendedPoint; stdcall;
    // Преобразует георафические координаты в позицию тайла на заданном зуме накрывающего данные координаты
    function LonLat2Pos(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; stdcall;
    // ?????????
    function LonLat2Metr(const Ll : TExtendedPoint) : TExtendedPoint; stdcall;

    // Возвращает количество тайлов в заданном зуме
    function TilesAtZoom(AZoom: byte): Longint; stdcall;
    // Возвращает общее количество пикселей на заданном зуме
    function PixelsAtZoom(AZoom: byte): Longint; stdcall;

    // Преобразует позицию тайла заданного зума в координаты пиксела его левого верхнего угла
    function TilePos2PixelPos(const XY : TPoint; Azoom : byte): TPoint; stdcall;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2PixelRect(const XY : TPoint; Azoom : byte): TRect; stdcall;
    // Преобразует координаты тайла в относительные координаты на карте (x/PixelsAtZoom)
    function TilePos2Relative(const XY : TPoint; Azoom : byte) : TExtendedPoint; stdcall;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2RelativeRect(const XY : TPoint; Azoom : byte): TExtendedRect; stdcall;
    // вычисляет координты пикселей вершин прямоугольника тайлов
    function TileRect2PixelRect(const XY: TRect; AZoom: byte): TRect; stdcall;//TODO: Автотест
    // Преобразует координаты тайла в географические координаты
    function TilePos2LonLat(const XY : TPoint; Azoom : byte) : TExtendedPoint; stdcall;//TODO: Автотест
    // Преобразует позицию тайла заданного зума в географические координаты его углов
    function TilePos2LonLatRect(const XY : TPoint; Azoom : byte): TExtendedRect; stdcall;//TODO: Автотест

    // Преобразует координаты пиксела в  координаты тайда cодержащего пиксель
    function PixelPos2TilePos(const XY : TPoint; Azoom : byte) : TPoint; stdcall;
    // Преобразует координаты пиксела в относительные координаты на карте (x/PixelsAtZoom)
    function PixelPos2Relative(const XY : TPoint; Azoom : byte) : TExtendedPoint; stdcall;
    // вычисляет прямоугольник тайлов покрывающий прямоугольник пикселов
    function PixelRect2TileRect(const XY: TRect; AZoom: byte): TRect; stdcall;
    // Преобразует координаты прямоугольника пикселов в относительные координаты на карте (x/PixelsAtZoom)
    function PixelRect2RelativeRect(const XY: TRect; AZoom: byte): TExtendedRect; stdcall;
    // Преобразует координаты пиксела в географические координаты
    function PixelPos2LonLat(const XY : TPoint; Azoom : byte) : TExtendedPoint; stdcall;//TODO: Автотест

    // Перобразует относительные координаты на карте в координаты пиксела
    function Relative2Pixel(const XY : TExtendedPoint; Azoom : byte) : TPoint; stdcall;
    // Перобразует относительные координаты на карте в координаты тайла
    function Relative2Tile(const XY : TExtendedPoint; Azoom : byte) : TPoint; stdcall;
    // Перобразует относительные координаты на карте в географические
    function Relative2LonLat(const XY : TExtendedPoint): TExtendedPoint; stdcall;//TODO: Автотест
    // Преобразует прямоугольник с относительными координатами в прямоугольник пикселов
    function RelativeRect2PixelRect(const XY : TExtendedRect; Azoom : byte) : TRect; stdcall;
    // Преобразует прямоугольник с относительными координатами в прямоугольник тайлов
    function RelativeRect2TileRect(const XY : TExtendedRect; Azoom : byte) : TRect; stdcall;
    // Перобразует прямоугольник с относительными координатами на карте в географические
    function RelativeRect2LonLatRect(const XY : TExtendedRect): TExtendedRect; stdcall;//TODO: Автотест

    // Преобразует георафические координаты в координаты пиксела на заданном зуме накрывающего данные координаты
    function LonLat2PixelPos(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; stdcall;//TODO: Автотест
    function LonLat2PixelPosf(const Ll : TExtendedPoint; Azoom : byte) : TExtendedPoint; stdcall;
    // Преобразует георафические координаты в позицию тайла на заданном зуме накрывающего данные координаты
    function LonLat2TilePos(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; stdcall;//TODO: Автотест
    function LonLat2TilePosf(const Ll : TExtendedPoint; Azoom : byte) : TExtendedPoint; stdcall;
    // Преобразует географические коодинаты в относительные координаты на карте
    function LonLat2Relative(const XY : TExtendedPoint): TExtendedPoint; stdcall;//TODO: Автотест
    // Преобразует прямоугольник в географических коодинатах в относительные координаты на карте
    function LonLatRect2RelativeRect(const XY : TExtendedRect): TExtendedRect; stdcall;//TODO: Автотест

    function Pos2OtherMap(XY : TPoint; Azoom : byte; AOtherMapCoordConv: ICoordConverter):TPoint;
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

  TCoordConverterAbstract = class(TInterfacedObject, ICoordConverter)
  protected
    FValidLonLatRect: TExtendedRect;
    function GetValidLonLatRect: TExtendedRect; virtual;
  public
    function Pos2LonLat(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function LonLat2Pos(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; virtual; stdcall;
    function LonLat2Metr(const Ll : TExtendedPoint) : TExtendedPoint; virtual; stdcall; abstract;

    function TilesAtZoom(AZoom: byte): Longint; virtual; stdcall;
    function PixelsAtZoom(AZoom: byte): Longint; virtual; stdcall;


    function TilePos2PixelPos(const XY : TPoint; Azoom : byte): TPoint; virtual; stdcall;
    function TilePos2PixelRect(const XY : TPoint; Azoom : byte): TRect; virtual; stdcall;
    function TilePos2LonLatRect(const XY : TPoint; Azoom : byte): TExtendedRect; virtual; stdcall;
    function TilePos2LonLat(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function TileRect2PixelRect(const XY: TRect; AZoom: byte): TRect; virtual; stdcall;
    function TilePos2Relative(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function TilePos2RelativeRect(const XY : TPoint; Azoom : byte): TExtendedRect; virtual; stdcall;

    function PixelPos2LonLat(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function PixelPos2TilePos(const XY : TPoint; Azoom : byte) : TPoint; virtual; stdcall;
    function PixelPos2Relative(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function PixelRect2TileRect(const XY: TRect; AZoom: byte): TRect; virtual; stdcall;
    function PixelRect2RelativeRect(const XY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;


    function LonLat2PixelPos(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; virtual; stdcall;
    function LonLat2PixelPosf(const Ll : TExtendedPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function LonLat2TilePos(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; virtual; stdcall;
    function LonLat2TilePosf(const Ll : TExtendedPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function LonLat2Relative(const XY : TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;
    function LonLatRect2RelativeRect(const XY : TExtendedRect): TExtendedRect; virtual; stdcall;

    function Relative2Pixel(const XY : TExtendedPoint; Azoom : byte) : TPoint; virtual; stdcall;
    function Relative2Tile(const XY : TExtendedPoint; Azoom : byte) : TPoint; virtual; stdcall;
    function Relative2LonLat(const XY : TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;
    function RelativeRect2LonLatRect(const XY : TExtendedRect): TExtendedRect; virtual; stdcall;
    function RelativeRect2TileRect(const XY : TExtendedRect; Azoom : byte) : TRect; virtual; stdcall;
    function RelativeRect2PixelRect(const XY : TExtendedRect; Azoom : byte) : TRect; virtual; stdcall;

    function Pos2OtherMap(XY : TPoint; Azoom : byte; AOtherMapCoordConv: ICoordConverter):TPoint; virtual;
    function CalcPoligonArea(polygon:TExtendedPointArray): Extended; virtual;
    function PoligonProject(AZoom:byte; APolyg: TExtendedPointArray): TPointArray; virtual;
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended; virtual; abstract;

    procedure CheckZoom(var AZoom: Byte); virtual; stdcall;
    procedure CheckTilePos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean); virtual; stdcall;
    procedure CheckTilePosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean); virtual; stdcall;
    procedure CheckTileRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean); virtual; stdcall;

    procedure CheckPixelPos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean); virtual; stdcall;
    procedure CheckPixelPosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean); virtual; stdcall;
    procedure CheckPixelRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean); virtual; stdcall;

    procedure CheckRelativePos(var XY: TExtendedPoint); virtual; stdcall;
    procedure CheckRelativeRect(var XY: TExtendedRect); virtual; stdcall;

    procedure CheckLonLatPos(var XY: TExtendedPoint); virtual; stdcall;
    procedure CheckLonLatRect(var XY: TExtendedRect); virtual; stdcall;

    procedure AfterConstruction; override;
  end;

const
  CTileRelativeEpsilon = (1/(1 shl 30 + (1 shl 30 - 1)))/2;

implementation

uses
  SysUtils,
  Math;

{ TCoordConverterAbstract }

function TCoordConverterAbstract.GetValidLonLatRect: TExtendedRect;
begin
  Result := TilePos2LonLatRect(Point(0, 0), 0);
end;

procedure TCoordConverterAbstract.AfterConstruction;
begin
  inherited;
  FValidLonLatRect := GetValidLonLatRect;
end;

//------------------------------------------------------------------------------
procedure TCoordConverterAbstract.CheckZoom(var AZoom: Byte);
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
end;
procedure TCoordConverterAbstract.CheckTilePos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoom(Azoom);

  if XY.X < 0 then begin
    if ACicleMap  then begin
      XY.X := XY.X mod VTilesAtZoom + VTilesAtZoom;
    end else begin
      XY.X := 0;
    end;
  end else begin
    if XY.X > VTilesAtZoom then begin
      if ACicleMap  then begin
        XY.X := XY.X mod VTilesAtZoom;
      end else begin
        XY.X := VTilesAtZoom;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    XY.Y := 0;
  end else begin
    if XY.Y > VTilesAtZoom then begin
      XY.Y := VTilesAtZoom;
    end;
  end;
end;
procedure TCoordConverterAbstract.CheckTileRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoom(Azoom);

  if XY.Left < 0 then begin
    if ACicleMap  then begin
      XY.Left := XY.Left mod VTilesAtZoom + VTilesAtZoom;
    end else begin
      XY.Left := 0;
    end;
  end else begin
    if XY.Left >= VTilesAtZoom then begin
      if ACicleMap  then begin
        XY.Left := XY.Left mod VTilesAtZoom;
      end else begin
        XY.Left := VTilesAtZoom - 1;
      end;
    end;
  end;

  if XY.Top < 0 then begin
    XY.Top := 0;
  end else begin
    if XY.Top >= VTilesAtZoom then begin
      XY.Top := VTilesAtZoom - 1;
    end;
  end;

  if XY.Right < 0 then begin
    if ACicleMap  then begin
      XY.Right := XY.Right mod VTilesAtZoom + VTilesAtZoom;
    end else begin
      XY.Right := 0;
    end;
  end else begin
    if XY.Right >= VTilesAtZoom then begin
      if ACicleMap  then begin
        XY.Right := XY.Right mod VTilesAtZoom;
      end else begin
        XY.Right := VTilesAtZoom - 1;
      end;
    end;
  end;

  if XY.Bottom < 0 then begin
    XY.Bottom := 0;
  end else begin
    if XY.Bottom >= VTilesAtZoom then begin
      XY.Bottom := VTilesAtZoom - 1;
    end;
  end;
end;

procedure TCoordConverterAbstract.CheckTilePosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoom(Azoom);

  if XY.X < 0 then begin
    if ACicleMap  then begin
      XY.X := XY.X mod VTilesAtZoom + VTilesAtZoom;
    end else begin
      XY.X := 0;
    end;
  end else begin
    if XY.X >= VTilesAtZoom then begin
      if ACicleMap  then begin
        XY.X := XY.X mod VTilesAtZoom;
      end else begin
        XY.X := VTilesAtZoom - 1;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    XY.Y := 0;
  end else begin
    if XY.Y >= VTilesAtZoom then begin
      XY.Y := VTilesAtZoom - 1;
    end;
  end;
end;

procedure TCoordConverterAbstract.CheckPixelPos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoom(Azoom);

  if XY.X < 0 then begin
    if (Azoom < 23) then begin
      if ACicleMap  then begin
        XY.X := XY.X mod VPixelsAtZoom + VPixelsAtZoom;
      end else begin
        XY.X := 0;
      end;
    end else begin
      if (XY.X <> VPixelsAtZoom) then begin
        if ACicleMap  then begin
          XY.X := VPixelsAtZoom - XY.X;
        end else begin
          XY.X := 0;
        end;
      end;
    end;
  end else begin
    if (Azoom < 23) and (XY.X > VPixelsAtZoom) then begin
      XY.X := VPixelsAtZoom;
    end;
  end;

  if XY.Y < 0 then begin
    if (Azoom < 23) or (XY.Y <> VPixelsAtZoom) then begin
      XY.Y := 0;
    end;
  end else begin
    if (Azoom < 23) and (XY.Y > VPixelsAtZoom) then begin
      if ACicleMap  then begin
        XY.X := XY.X mod VPixelsAtZoom;
      end else begin
        XY.X := VPixelsAtZoom;
      end;
    end;
  end;
end;

procedure TCoordConverterAbstract.CheckPixelRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoom(Azoom);

  if XY.Left < 0 then begin
    if ACicleMap then begin
      XY.Left := XY.Left mod VPixelsAtZoom + VPixelsAtZoom;
    end else begin
      XY.Left := 0;
    end;
  end else begin
    if (Azoom < 23) and (XY.Left >= VPixelsAtZoom) then begin
      if ACicleMap then begin
        XY.Left := XY.Left mod VPixelsAtZoom;
      end else begin
        XY.Left := VPixelsAtZoom - 1;
      end;
    end;
  end;

  if XY.Top < 0 then begin
    XY.Top := 0;
  end else begin
    if (Azoom < 23) and (XY.Top > VPixelsAtZoom) then begin
      XY.Top := VPixelsAtZoom - 1;
    end;
  end;

  if XY.Right < 0 then begin
    if ACicleMap then begin
      XY.Right := XY.Right mod VPixelsAtZoom + VPixelsAtZoom;
    end else begin
      XY.Right := 0;
    end;
  end else begin
    if (Azoom < 23) and (XY.Right >= VPixelsAtZoom) then begin
      if ACicleMap then begin
        XY.Right := XY.Right mod VPixelsAtZoom;
      end else begin
        XY.Right := VPixelsAtZoom - 1;
      end;
    end;
  end;

  if XY.Bottom < 0 then begin
    XY.Bottom := 0;
  end else begin
    if (Azoom < 23) and (XY.Bottom > VPixelsAtZoom) then begin
      XY.Bottom := VPixelsAtZoom - 1;
    end;
  end;
end;

procedure TCoordConverterAbstract.CheckPixelPosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoom(Azoom);
  if XY.X < 0 then begin
    if ACicleMap  then begin
      XY.X := XY.X mod VPixelsAtZoom + VPixelsAtZoom;
    end else begin
      XY.X := 0;
    end;
  end else begin
    if (Azoom < 23) and (XY.X >= VPixelsAtZoom) then begin
      if ACicleMap  then begin
        XY.X := XY.X mod VPixelsAtZoom;
      end else begin
        XY.X := VPixelsAtZoom - 1;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    XY.Y := 0;
  end else begin
    if (Azoom < 23) and (XY.Y > VPixelsAtZoom) then begin
      XY.Y := VPixelsAtZoom - 1;
    end;
  end;
end;

procedure TCoordConverterAbstract.CheckRelativePos(var XY: TExtendedPoint);
begin
  if XY.X < 0 then begin
    XY.X := 0;
  end else begin
    if XY.X > 1 then begin
      XY.X := 1;
    end;
  end;

  if XY.Y < 0 then begin
    XY.Y := 0;
  end else begin
    if XY.Y > 1 then begin
      XY.Y := 1;
    end;
  end;
end;

procedure TCoordConverterAbstract.CheckRelativeRect(var XY: TExtendedRect);
begin
  if XY.Left < 0 then begin
    XY.Left := 0;
  end else begin
    if XY.Left > 1 then begin
      XY.Left := 1;
    end;
  end;

  if XY.Top < 0 then begin
    XY.Top := 0;
  end else begin
    if XY.Top > 1 then begin
      XY.Top := 1;
    end;
  end;

  if XY.Right < 0 then begin
    XY.Right := 0;
  end else begin
    if XY.Right > 1 then begin
      XY.Right := 1;
    end;
  end;

  if XY.Bottom < 0 then begin
    XY.Bottom := 0;
  end else begin
    if XY.Bottom > 1 then begin
      XY.Bottom := 1;
    end;
  end;
end;

procedure TCoordConverterAbstract.CheckLonLatPos(var XY: TExtendedPoint);
begin
  if XY.X < FValidLonLatRect.Left then begin
    XY.X := FValidLonLatRect.Left;
  end else begin
    if XY.X > FValidLonLatRect.Right then begin
      XY.X := FValidLonLatRect.Right;
    end;
  end;
  if XY.Y < FValidLonLatRect.Bottom then begin
    XY.Y := FValidLonLatRect.Bottom;
  end else begin
    if XY.Y > FValidLonLatRect.Top then begin
      XY.Y := FValidLonLatRect.Top;
    end;
  end;
end;

procedure TCoordConverterAbstract.CheckLonLatRect(var XY: TExtendedRect);
begin
  if XY.Left < FValidLonLatRect.Left then begin
    XY.Left := FValidLonLatRect.Left;
  end else begin
    if XY.Left > FValidLonLatRect.Right then begin
      XY.Left := FValidLonLatRect.Right;
    end;
  end;
  if XY.Bottom < FValidLonLatRect.Bottom then begin
    XY.Bottom := FValidLonLatRect.Bottom;
  end else begin
    if XY.Bottom > FValidLonLatRect.Top then begin
      XY.Bottom := FValidLonLatRect.Top;
    end;
  end;

  if XY.Right < FValidLonLatRect.Right then begin
    XY.Right := FValidLonLatRect.Right;
  end else begin
    if XY.Right > FValidLonLatRect.Right then begin
      XY.Right := FValidLonLatRect.Right;
    end;
  end;
  if XY.Top < FValidLonLatRect.Top then begin
    XY.Top := FValidLonLatRect.Top;
  end else begin
    if XY.Top > FValidLonLatRect.Top then begin
      XY.Top := FValidLonLatRect.Top;
    end;
  end;
end;



function TCoordConverterAbstract.CalcPoligonArea(
  polygon: TExtendedPointArray): extended;
var
  L,i:integer;
  LLPrev, LLCurr: TExtendedPoint;
begin
  result:=0;
  l:=length(polygon);
  LLPrev := LonLat2Metr(polygon[0]);
  for i:=1 to L-1 do begin
    LLCurr := LonLat2Metr(polygon[i]);
    result := result + (LLPrev.x + LLCurr.x)*(LLPrev.y - LLCurr.y);
    LLPrev := LLCurr;
  end;
  result := 0.5*abs(result)/1000000;
end;

function TCoordConverterAbstract.TilePos2PixelRect(const XY: TPoint;
  Azoom: byte): TRect;
begin
  Result.Left := XY.X shl 8;
  Result.Top := XY.Y shl 8;
  Result.Right := Result.Left + ((1 shl 8) - 1);
  Result.Bottom := Result.Top + ((1 shl 8) - 1);
end;

function TCoordConverterAbstract.PoligonProject(AZoom: byte;
  APolyg: TExtendedPointArray): TPointArray;
var
  i:integer;
  VTilesAtZoom: Integer;
begin
  VTilesAtZoom := 1 shl AZoom;
  SetLength(Result, length(APolyg));
  for i:=0 to length(APolyg)-1 do begin
    Result[i] := LonLat2Pos(Apolyg[i], AZoom);
    if Result[i].y < 0 then Result[i].y:=1;
    if Result[i].y > VTilesAtZoom then Result[i].y := VTilesAtZoom - 1;
  end;
end;

function TCoordConverterAbstract.Pos2OtherMap(XY: TPoint; Azoom: byte;
  AOtherMapCoordConv: ICoordConverter): TPoint;
begin
  if (Self = nil) or (AOtherMapCoordConv = nil) then begin
    Result := XY;
  end else begin
    Result := AOtherMapCoordConv.LonLat2Pos(Pos2LonLat(XY, Azoom), Azoom);
  end;
end;

function TCoordConverterAbstract.TilePos2LonLatRect(const XY: TPoint;
  Azoom: byte): TExtendedRect;
begin
  Result := RelativeRect2LonLatRect(TilePos2RelativeRect(XY, Azoom));
end;

function TCoordConverterAbstract.PixelsAtZoom(AZoom: byte): Longint;
begin
  Result := 1 shl (AZoom + 8);
end;

function TCoordConverterAbstract.TilesAtZoom(AZoom: byte): Longint;
begin
  Result := 1 shl AZoom;
end;

function TCoordConverterAbstract.PixelPos2Relative(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VPixelsAtZoomExt: Extended;
  VPixelsAtZoom: Integer;
begin
  VPixelsAtZoom := PixelsAtZoom(Azoom);
  VPixelsAtZoomExt := VPixelsAtZoom;
  VPixelsAtZoomExt := abs(VPixelsAtZoomExt);
  if XY.X = VPixelsAtZoom then
    Result.X := 1
  else
    Result.X := XY.X / VPixelsAtZoomExt;

  if XY.Y = VPixelsAtZoom then
    Result.Y := 1
  else
    Result.Y := XY.Y / VPixelsAtZoomExt;
end;

function TCoordConverterAbstract.Relative2Pixel(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoom(Azoom);
  VPixelsAtZoom := abs(VPixelsAtZoom);
  Result.X := Trunc(RoundTo(XY.X * VPixelsAtZoom, -2));
  Result.Y := Trunc(RoundTo(XY.Y * VPixelsAtZoom, -2));
end;

function TCoordConverterAbstract.LonLat2PixelPos(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  Result := Relative2Pixel(LonLat2Relative(LL), AZoom);
end;

function TCoordConverterAbstract.LonLat2PixelPosf(const Ll: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoom(Azoom);
  VPixelsAtZoom := abs(VPixelsAtZoom);

  Result := LonLat2Relative(LL);
  Result.X := Result.X * VPixelsAtZoom;
  Result.Y := Result.Y * VPixelsAtZoom;
end;

function TCoordConverterAbstract.LonLat2TilePos(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  Result := Relative2Tile(LonLat2Relative(LL), AZoom);
end;

function TCoordConverterAbstract.PixelPos2LonLat(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := Relative2LonLat(PixelPos2Relative(XY, Azoom));
end;

function TCoordConverterAbstract.TilePos2LonLat(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := Relative2LonLat(TilePos2Relative(XY, Azoom));
end;

function TCoordConverterAbstract.TilePos2Relative(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoom(Azoom);
  Result.X := XY.X / VTilesAtZoom;
  Result.Y := XY.Y / VTilesAtZoom;
end;

function TCoordConverterAbstract.TilePos2RelativeRect(const XY: TPoint;
  Azoom: byte): TExtendedRect;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoom(Azoom);
  Result.Left := XY.X / VTilesAtZoom;
  Result.Top := XY.Y / VTilesAtZoom;
  Result.Right := (XY.X + 1) / VTilesAtZoom;
  Result.Bottom := (XY.Y + 1) / VTilesAtZoom;
end;

function TCoordConverterAbstract.LonLatRect2RelativeRect(
  const XY: TExtendedRect): TExtendedRect;
begin
  Result.TopLeft := LonLat2Relative(XY.TopLeft);
  Result.BottomRight := LonLat2Relative(XY.BottomRight);
end;

function TCoordConverterAbstract.Relative2Tile(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoom(Azoom);
  Result.X := Trunc(RoundTo(XY.X * VTilesAtZoom, -2));
  Result.Y := Trunc(RoundTo(XY.Y * VTilesAtZoom, -2));
end;

function TCoordConverterAbstract.RelativeRect2LonLatRect(
  const XY: TExtendedRect): TExtendedRect;
begin
  Result.TopLeft := Relative2LonLat(XY.TopLeft);
  Result.BottomRight := Relative2LonLat(XY.BottomRight);
end;

function TCoordConverterAbstract.RelativeRect2PixelRect(const XY: TExtendedRect;
  Azoom: byte): TRect;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoom(Azoom);
  VPixelsAtZoom := abs(VPixelsAtZoom);

  Result.Left := Trunc((XY.Left + CTileRelativeEpsilon) * VPixelsAtZoom);
  Result.Top := Trunc((XY.Top + CTileRelativeEpsilon) * VPixelsAtZoom);

  Result.Right := Trunc((XY.Right - CTileRelativeEpsilon) * VPixelsAtZoom);
  Result.Bottom := Trunc((XY.Bottom - CTileRelativeEpsilon) * VPixelsAtZoom);
end;

function TCoordConverterAbstract.RelativeRect2TileRect(const XY: TExtendedRect;
  Azoom: byte): TRect;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoom(Azoom);

  Result.Left := Trunc((XY.Left + CTileRelativeEpsilon) * VTilesAtZoom);
  Result.Top := Trunc((XY.Top + CTileRelativeEpsilon) * VTilesAtZoom);

  Result.Right := Trunc((XY.Right - CTileRelativeEpsilon) * VTilesAtZoom);
  Result.Bottom := Trunc((XY.Bottom - CTileRelativeEpsilon) * VTilesAtZoom);
end;

function TCoordConverterAbstract.PixelPos2TilePos(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result.X := XY.X shr 8;
  Result.Y := XY.Y shr 8;
end;

function TCoordConverterAbstract.PixelRect2TileRect(const XY: TRect;
  AZoom: byte): TRect;
begin
  Result.Left := XY.Left shr 8;
  Result.Top := XY.Top shr 8;
  Result.Right := XY.Right shr 8;
  Result.Bottom := XY.Bottom shr 8;
end;

function TCoordConverterAbstract.TileRect2PixelRect(const XY: TRect;
  AZoom: byte): TRect;
begin
  Result.Left := XY.Left shl 8;
  Result.Top := XY.Top shl 8;
  Result.Right := (XY.Right + 1) shl 8 - 1;
  Result.Bottom := (XY.Bottom + 1) shl 8 - 1;
end;

function TCoordConverterAbstract.PixelRect2RelativeRect(const XY: TRect;
  AZoom: byte): TExtendedRect;
var
  VBottomRight: TPoint;
begin
  Result.TopLeft := PixelPos2Relative(XY.TopLeft, AZoom);
  VBottomRight.X := XY.Right + 1;
  VBottomRight.Y := XY.Bottom + 1;
  Result.BottomRight := PixelPos2Relative(VBottomRight, AZoom);
end;

function TCoordConverterAbstract.TilePos2PixelPos(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result.X := XY.X shl 8;
  Result.Y := XY.Y shl 8;
end;

function TCoordConverterAbstract.LonLat2TilePosf(const Ll: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoom(Azoom);
  Result := LonLat2Relative(Ll);
  Result.X := Result.X * VTilesAtZoom;
  Result.Y := Result.Y * VTilesAtZoom;
end;

function TCoordConverterAbstract.LonLat2Pos(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  if Azoom > 23 then begin
    Result := LonLat2PixelPos(Ll, Azoom - 8);
  end else begin
    Result := LonLat2TilePos(Ll, Azoom);
  end;
end;

function TCoordConverterAbstract.Pos2LonLat(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  if Azoom > 23 then begin
    Result := PixelPos2LonLat(XY, Azoom - 8);
  end else begin
    Result := TilePos2LonLat(XY, Azoom);
  end;
end;

end.

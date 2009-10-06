unit u_CoordConverterAbstract;

interface

uses
  Types,
  t_GeoTypes;

type
  ICoordConverter = interface
  ['{3EE2987F-7681-425A-8EFE-B676C506CDD4}']
    // ѕреобразует позицию тайла на заданном зуме в георафически координаты его верхнего левого угла
    function Pos2LonLat(const XY : TPoint; Azoom : byte) : TExtendedPoint; stdcall;
    // ѕреобразует георафические координаты в позицию тайла на заданном зуме накрывающего данные координаты
    function LonLat2Pos(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; stdcall;
    // ?????????
    function LonLat2Metr(const Ll : TExtendedPoint) : TExtendedPoint; stdcall;
    // ѕреобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePosToPosTRect(const XY : TPoint; Azoom : byte): TRect; stdcall;
    // ѕреобразует позицию тайла заданного зума в географические координаты его углов
    function TilePosToLonLatRect(const XY : TPoint; Azoom : byte): TExtendedRect; stdcall;
    // ¬озвращает количество тайлов в заданном зуме
    function TilesAtZoom(AZoom: byte): Longint; stdcall;
    // ¬озвращает общее количество пикселей на заданном зуме
    function PixelsAtZoom(AZoom: byte): Longint; stdcall;
    // ѕреобразует координаты пиксела в относительные координаты на карте (x/PixelsAtZoom)
    function PixelToRelative(const XY : TPoint; Azoom : byte) : TExtendedPoint; stdcall;
    // ѕреобразует географические коодинаты в относительные координаты на карте
    function LonLatToRelative(const XY : TExtendedPoint): TExtendedPoint; stdcall;
    // ѕеробразует относительные координаты на карте в координаты пиксела
    function RelativeToPixel(const XY : TExtendedPoint; Azoom : byte) : TPoint; stdcall;
    // ѕеробразует относительные координаты на карте в географические
    function RelativeToLonLat(const XY : TExtendedPoint): TExtendedPoint; stdcall;

    function Pos2OtherMap(XY : TPoint; Azoom : byte; AOtherMapCoordConv: ICoordConverter):TPoint;
    function CalcPoligonArea(polygon:TExtendedPointArray): Extended;
    function PoligonProject(AZoom:byte; APolyg: TExtendedPointArray): TPointArray;
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended;
  end;

  TCoordConverterAbstract = class(TInterfacedObject, ICoordConverter)
  public
    function Pos2LonLat(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall; abstract;
    function LonLat2Pos(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; virtual; stdcall; abstract;
    function LonLat2Metr(const Ll : TExtendedPoint) : TExtendedPoint; virtual; stdcall; abstract;
    function TilePosToPosTRect(const XY : TPoint; Azoom : byte): TRect; virtual; stdcall;
    function TilePosToLonLatRect(const XY : TPoint; Azoom : byte): TExtendedRect; virtual; stdcall;
    function TilesAtZoom(AZoom: byte): Longint; virtual; stdcall;
    function PixelsAtZoom(AZoom: byte): Longint; virtual; stdcall;
    function PixelToRelative(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function LonLatToRelative(const XY : TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;
    function RelativeToPixel(const XY : TExtendedPoint; Azoom : byte) : TPoint; virtual; stdcall;
    function RelativeToLonLat(const XY : TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;
    function Pos2OtherMap(XY : TPoint; Azoom : byte; AOtherMapCoordConv: ICoordConverter):TPoint; virtual;
    function CalcPoligonArea(polygon:TExtendedPointArray): Extended; virtual;
    function PoligonProject(AZoom:byte; APolyg: TExtendedPointArray): TPointArray; virtual;
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended; virtual; abstract;
  end;

implementation

{ TCoordConverterAbstract }

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

function TCoordConverterAbstract.TilePosToPosTRect(const XY: TPoint;
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
  if (AOtherMapCoordConv = nil) then begin
    Result := XY;
  end else begin
    Result := AOtherMapCoordConv.LonLat2Pos(Pos2LonLat(XY, Azoom), Azoom);
  end;
end;

function TCoordConverterAbstract.TilePosToLonLatRect(const XY: TPoint;
  Azoom: byte): TExtendedRect;
var
  VRect: TRect;
begin
  VRect := TilePosToPosTRect(XY, Azoom);
  Result.TopLeft := Pos2LonLat(VRect.TopLeft, Azoom + 8);
  Result.BottomRight := Pos2LonLat(VRect.BottomRight, Azoom + 8);
end;

function TCoordConverterAbstract.PixelsAtZoom(AZoom: byte): Longint;
begin
  Result := 1 shl AZoom;
end;

function TCoordConverterAbstract.TilesAtZoom(AZoom: byte): Longint;
begin
  Result := 1 shl (AZoom + 8);
end;

function TCoordConverterAbstract.PixelToRelative(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VPixelsAtZoom: Longint;
begin
  VPixelsAtZoom := PixelsAtZoom(Azoom);
  Result.X := XY.X / VPixelsAtZoom;
  Result.Y := XY.Y / VPixelsAtZoom;
end;

function TCoordConverterAbstract.RelativeToPixel(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VPixelsAtZoom: Longint;
begin
  VPixelsAtZoom := PixelsAtZoom(Azoom);
  Result.X := Trunc(XY.X * VPixelsAtZoom);
  Result.Y := Trunc(XY.Y * VPixelsAtZoom);
end;

end.

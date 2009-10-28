unit u_CoordConverterAbstract;

interface

uses
  Types,
  i_ICoordConverter,
  t_GeoTypes;

type
  TCoordConverterAbstract = class(TInterfacedObject, ICoordConverter)
  private
    FValidLonLatRect: TExtendedRect;
  protected
    function GetValidLonLatRect: TExtendedRect; virtual;

    function Pos2LonLatInternal(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function LonLat2PosInternal(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; virtual; stdcall;
    function LonLat2MetrInternal(const Ll : TExtendedPoint) : TExtendedPoint; virtual; stdcall; abstract;

    function TilesAtZoomInternal(AZoom: byte): Longint; virtual; stdcall;
    function PixelsAtZoomInternal(AZoom: byte): Longint; virtual; stdcall;


    function TilePos2PixelPosInternal(const XY : TPoint; Azoom : byte): TPoint; virtual; stdcall;
    function TilePos2PixelRectInternal(const XY : TPoint; Azoom : byte): TRect; virtual; stdcall;
    function TilePos2LonLatRectInternal(const XY : TPoint; Azoom : byte): TExtendedRect; virtual; stdcall;
    function TilePos2LonLatInternal(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function TileRect2PixelRectInternal(const XY: TRect; AZoom: byte): TRect; virtual; stdcall;
    function TilePos2RelativeInternal(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function TilePos2RelativeRectInternal(const XY : TPoint; Azoom : byte): TExtendedRect; virtual; stdcall;

    function PixelPos2LonLatInternal(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function PixelPos2TilePosInternal(const XY : TPoint; Azoom : byte) : TPoint; virtual; stdcall;
    function PixelPos2RelativeInternal(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function PixelRect2TileRectInternal(const XY: TRect; AZoom: byte): TRect; virtual; stdcall;
    function PixelRect2RelativeRectInternal(const XY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;


    function LonLat2PixelPosInternal(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; virtual; stdcall;
    function LonLat2PixelPosfInternal(const Ll : TExtendedPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function LonLat2TilePosInternal(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; virtual; stdcall;
    function LonLat2TilePosfInternal(const Ll : TExtendedPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function LonLat2RelativeInternal(const XY : TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;
    function LonLatRect2RelativeRectInternal(const XY : TExtendedRect): TExtendedRect; virtual; stdcall;

    function Relative2PixelInternal(const XY : TExtendedPoint; Azoom : byte) : TPoint; virtual; stdcall;
    function Relative2TileInternal(const XY : TExtendedPoint; Azoom : byte) : TPoint; virtual; stdcall;
    function Relative2LonLatInternal(const XY : TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;
    function RelativeRect2LonLatRectInternal(const XY : TExtendedRect): TExtendedRect; virtual; stdcall;
    function RelativeRect2TileRectInternal(const XY : TExtendedRect; Azoom : byte) : TRect; virtual; stdcall;
    function RelativeRect2PixelRectInternal(const XY : TExtendedRect; Azoom : byte) : TRect; virtual; stdcall;
  public
    constructor Create;

    function Pos2LonLat(const XY : TPoint; Azoom : byte) : TExtendedPoint; virtual; stdcall;
    function LonLat2Pos(const Ll : TExtendedPoint; Azoom : byte) : Tpoint; virtual; stdcall;
    function LonLat2Metr(const Ll : TExtendedPoint) : TExtendedPoint; virtual; stdcall;

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
    function LonLat2Relative(const XY : TExtendedPoint): TExtendedPoint; virtual; stdcall;
    function LonLatRect2RelativeRect(const XY : TExtendedRect): TExtendedRect; virtual; stdcall;

    function Relative2Pixel(const XY : TExtendedPoint; Azoom : byte) : TPoint; virtual; stdcall;
    function Relative2Tile(const XY : TExtendedPoint; Azoom : byte) : TPoint; virtual; stdcall;
    function Relative2LonLat(const XY : TExtendedPoint): TExtendedPoint; virtual; stdcall;
    function RelativeRect2LonLatRect(const XY : TExtendedRect): TExtendedRect; virtual; stdcall;
    function RelativeRect2TileRect(const XY : TExtendedRect; Azoom : byte) : TRect; virtual; stdcall;
    function RelativeRect2PixelRect(const XY : TExtendedRect; Azoom : byte) : TRect; virtual; stdcall;

    function Pos2OtherMap(XY : TPoint; Azoom : byte; AOtherMapCoordConv: ICoordConverter):TPoint; virtual;
    function CalcPoligonArea(polygon:TExtendedPointArray): Extended; virtual;
    function PoligonProject(AZoom:byte; APolyg: TExtendedPointArray): TPointArray; virtual;
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended; virtual; abstract;
  end;

const
  CTileRelativeEpsilon = (1/(1 shl 30 + (1 shl 30 - 1)))/2;

implementation

uses
  Math;

{ TCoordConverterAbstract }


constructor TCoordConverterAbstract.Create;
begin
  FValidLonLatRect := GetValidLonLatRect;
end;

function TCoordConverterAbstract.GetValidLonLatRect: TExtendedRect;
begin
  Result := TilePos2LonLatRectInternal(Point(0, 0), 0);
end;

function TCoordConverterAbstract.CalcPoligonArea(
  polygon: TExtendedPointArray): extended;
var
  L,i:integer;
  LLPrev, LLCurr: TExtendedPoint;
begin
  result:=0;
  l:=length(polygon);
  LLPrev := LonLat2MetrInternal(polygon[0]);
  for i:=1 to L-1 do begin
    LLCurr := LonLat2MetrInternal(polygon[i]);
    result := result + (LLPrev.x + LLCurr.x)*(LLPrev.y - LLCurr.y);
    LLPrev := LLCurr;
  end;
  result := 0.5*abs(result)/1000000;
end;

function TCoordConverterAbstract.PoligonProject(AZoom: byte;
  APolyg: TExtendedPointArray): TPointArray;
var
  i:integer;
  VTilesAtZoom: Integer;
begin
  VTilesAtZoom := TilesAtZoomInternal(AZoom);
  SetLength(Result, length(APolyg));
  for i:=0 to length(APolyg)-1 do begin
    Result[i] := LonLat2PosInternal(Apolyg[i], AZoom);
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
    Result := AOtherMapCoordConv.LonLat2Pos(Pos2LonLatInternal(XY, Azoom), Azoom);
  end;
end;

//------------------------------------------------------------------------------
function TCoordConverterAbstract.TilePos2PixelRectInternal(const XY: TPoint;
  Azoom: byte): TRect;
begin
  Result.Left := XY.X shl 8;
  Result.Top := XY.Y shl 8;
  Result.Right := Result.Left + ((1 shl 8) - 1);
  Result.Bottom := Result.Top + ((1 shl 8) - 1);
end;

function TCoordConverterAbstract.TilePos2LonLatRectInternal(const XY: TPoint;
  Azoom: byte): TExtendedRect;
begin
  Result := RelativeRect2LonLatRectInternal(TilePos2RelativeRectInternal(XY, Azoom));
end;

function TCoordConverterAbstract.PixelsAtZoomInternal(AZoom: byte): Longint;
begin
  Result := 1 shl (AZoom + 8);
end;

function TCoordConverterAbstract.TilesAtZoomInternal(AZoom: byte): Longint;
begin
  Result := 1 shl AZoom;
end;

function TCoordConverterAbstract.PixelPos2RelativeInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VPixelsAtZoomExt: Extended;
  VPixelsAtZoom: Integer;
begin
  VPixelsAtZoom := PixelsAtZoomInternal(Azoom);
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

function TCoordConverterAbstract.Relative2PixelInternal(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoomInternal(Azoom);
  VPixelsAtZoom := abs(VPixelsAtZoom);
  Result.X := Trunc(RoundTo(XY.X * VPixelsAtZoom, -2));
  Result.Y := Trunc(RoundTo(XY.Y * VPixelsAtZoom, -2));
end;

function TCoordConverterAbstract.LonLat2PixelPosInternal(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  Result := Relative2PixelInternal(LonLat2RelativeInternal(LL), AZoom);
end;

function TCoordConverterAbstract.LonLat2PixelPosfInternal(const Ll: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoomInternal(Azoom);
  VPixelsAtZoom := abs(VPixelsAtZoom);

  Result := LonLat2RelativeInternal(LL);
  Result.X := Result.X * VPixelsAtZoom;
  Result.Y := Result.Y * VPixelsAtZoom;
end;

function TCoordConverterAbstract.LonLat2TilePosInternal(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  Result := Relative2TileInternal(LonLat2RelativeInternal(LL), AZoom);
end;

function TCoordConverterAbstract.PixelPos2LonLatInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := Relative2LonLatInternal(PixelPos2RelativeInternal(XY, Azoom));
end;

function TCoordConverterAbstract.TilePos2LonLatInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := Relative2LonLatInternal(TilePos2RelativeInternal(XY, Azoom));
end;

function TCoordConverterAbstract.TilePos2RelativeInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomInternal(Azoom);
  Result.X := XY.X / VTilesAtZoom;
  Result.Y := XY.Y / VTilesAtZoom;
end;

function TCoordConverterAbstract.TilePos2RelativeRectInternal(const XY: TPoint;
  Azoom: byte): TExtendedRect;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomInternal(Azoom);
  Result.Left := XY.X / VTilesAtZoom;
  Result.Top := XY.Y / VTilesAtZoom;
  Result.Right := (XY.X + 1) / VTilesAtZoom;
  Result.Bottom := (XY.Y + 1) / VTilesAtZoom;
end;

function TCoordConverterAbstract.LonLatRect2RelativeRectInternal(
  const XY: TExtendedRect): TExtendedRect;
begin
  Result.TopLeft := LonLat2RelativeInternal(XY.TopLeft);
  Result.BottomRight := LonLat2RelativeInternal(XY.BottomRight);
end;

function TCoordConverterAbstract.Relative2TileInternal(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomInternal(Azoom);
  Result.X := Trunc(RoundTo(XY.X * VTilesAtZoom, -2));
  Result.Y := Trunc(RoundTo(XY.Y * VTilesAtZoom, -2));
end;

function TCoordConverterAbstract.RelativeRect2LonLatRectInternal(
  const XY: TExtendedRect): TExtendedRect;
begin
  Result.TopLeft := Relative2LonLatInternal(XY.TopLeft);
  Result.BottomRight := Relative2LonLatInternal(XY.BottomRight);
end;

function TCoordConverterAbstract.RelativeRect2PixelRectInternal(const XY: TExtendedRect;
  Azoom: byte): TRect;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoomInternal(Azoom);
  VPixelsAtZoom := abs(VPixelsAtZoom);

  Result.Left := Trunc((XY.Left + CTileRelativeEpsilon) * VPixelsAtZoom);
  Result.Top := Trunc((XY.Top + CTileRelativeEpsilon) * VPixelsAtZoom);

  Result.Right := Trunc((XY.Right - CTileRelativeEpsilon) * VPixelsAtZoom);
  Result.Bottom := Trunc((XY.Bottom - CTileRelativeEpsilon) * VPixelsAtZoom);
end;

function TCoordConverterAbstract.RelativeRect2TileRectInternal(const XY: TExtendedRect;
  Azoom: byte): TRect;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomInternal(Azoom);

  Result.Left := Trunc((XY.Left + CTileRelativeEpsilon) * VTilesAtZoom);
  Result.Top := Trunc((XY.Top + CTileRelativeEpsilon) * VTilesAtZoom);

  Result.Right := Trunc((XY.Right - CTileRelativeEpsilon) * VTilesAtZoom);
  Result.Bottom := Trunc((XY.Bottom - CTileRelativeEpsilon) * VTilesAtZoom);
end;

function TCoordConverterAbstract.PixelPos2TilePosInternal(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result.X := XY.X shr 8;
  Result.Y := XY.Y shr 8;
end;

function TCoordConverterAbstract.PixelRect2TileRectInternal(const XY: TRect;
  AZoom: byte): TRect;
begin
  Result.Left := XY.Left shr 8;
  Result.Top := XY.Top shr 8;
  Result.Right := XY.Right shr 8;
  Result.Bottom := XY.Bottom shr 8;
end;

function TCoordConverterAbstract.TileRect2PixelRectInternal(const XY: TRect;
  AZoom: byte): TRect;
begin
  Result.Left := XY.Left shl 8;
  Result.Top := XY.Top shl 8;
  Result.Right := (XY.Right + 1) shl 8 - 1;
  Result.Bottom := (XY.Bottom + 1) shl 8 - 1;
end;

function TCoordConverterAbstract.PixelRect2RelativeRectInternal(const XY: TRect;
  AZoom: byte): TExtendedRect;
var
  VBottomRight: TPoint;
begin
  Result.TopLeft := PixelPos2RelativeInternal(XY.TopLeft, AZoom);
  VBottomRight.X := XY.Right + 1;
  VBottomRight.Y := XY.Bottom + 1;
  Result.BottomRight := PixelPos2RelativeInternal(VBottomRight, AZoom);
end;

function TCoordConverterAbstract.TilePos2PixelPosInternal(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result.X := XY.X shl 8;
  Result.Y := XY.Y shl 8;
end;

function TCoordConverterAbstract.LonLat2TilePosfInternal(const Ll: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomInternal(Azoom);
  Result := LonLat2RelativeInternal(Ll);
  Result.X := Result.X * VTilesAtZoom;
  Result.Y := Result.Y * VTilesAtZoom;
end;

function TCoordConverterAbstract.LonLat2PosInternal(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  if Azoom > 23 then begin
    Result := LonLat2PixelPosInternal(Ll, Azoom - 8);
  end else begin
    Result := LonLat2TilePosInternal(Ll, Azoom);
  end;
end;

function TCoordConverterAbstract.Pos2LonLatInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  if Azoom > 23 then begin
    Result := PixelPos2LonLatInternal(XY, Azoom - 8);
  end else begin
    Result := TilePos2LonLatInternal(XY, Azoom);
  end;
end;


//------------------------------------------------------------------------------
function TCoordConverterAbstract.TilePos2PixelRect(const XY: TPoint;
  Azoom: byte): TRect;
begin
  Result := TilePos2PixelRectInternal(XY, Azoom);
end;

function TCoordConverterAbstract.TilePos2LonLatRect(const XY: TPoint;
  Azoom: byte): TExtendedRect;
begin
  Result := TilePos2LonLatRectInternal(XY, Azoom);
end;

function TCoordConverterAbstract.PixelsAtZoom(AZoom: byte): Longint;
begin
  Result := PixelsAtZoomInternal(Azoom);
end;

function TCoordConverterAbstract.TilesAtZoom(AZoom: byte): Longint;
begin
  Result := TilesAtZoomInternal(Azoom);
end;

function TCoordConverterAbstract.PixelPos2Relative(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := PixelPos2RelativeInternal(XY, Azoom);
end;

function TCoordConverterAbstract.Relative2LonLat(
  const XY: TExtendedPoint): TExtendedPoint;
begin
  Result := Relative2LonLatInternal(XY);
end;

function TCoordConverterAbstract.Relative2Pixel(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
begin
  Result := Relative2PixelInternal(XY, Azoom);
end;

function TCoordConverterAbstract.LonLat2Metr(
  const Ll: TExtendedPoint): TExtendedPoint;
begin
  Result := LonLat2MetrInternal(LL);
end;

function TCoordConverterAbstract.LonLat2PixelPos(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  Result := LonLat2PixelPosInternal(LL, Azoom);
end;

function TCoordConverterAbstract.LonLat2PixelPosf(const Ll: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := LonLat2PixelPosfInternal(LL, Azoom);
end;

function TCoordConverterAbstract.PixelPos2LonLat(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := PixelPos2LonLatInternal(XY, Azoom);
end;

function TCoordConverterAbstract.TilePos2LonLat(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := TilePos2LonLatInternal(XY, Azoom);
end;

function TCoordConverterAbstract.TilePos2Relative(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := TilePos2RelativeInternal(XY, Azoom);
end;

function TCoordConverterAbstract.TilePos2RelativeRect(const XY: TPoint;
  Azoom: byte): TExtendedRect;
begin
  Result := TilePos2RelativeRectInternal(XY, Azoom);
end;

function TCoordConverterAbstract.LonLatRect2RelativeRect(
  const XY: TExtendedRect): TExtendedRect;
begin
  Result := LonLatRect2RelativeRectInternal(XY);
end;

function TCoordConverterAbstract.Relative2Tile(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
begin
  Result := Relative2TileInternal(XY, Azoom);
end;

function TCoordConverterAbstract.RelativeRect2LonLatRect(
  const XY: TExtendedRect): TExtendedRect;
begin
  Result := RelativeRect2LonLatRectInternal(XY);
end;

function TCoordConverterAbstract.RelativeRect2PixelRect(const XY: TExtendedRect;
  Azoom: byte): TRect;
begin
  Result := RelativeRect2PixelRectInternal(XY, Azoom);
end;

function TCoordConverterAbstract.RelativeRect2TileRect(const XY: TExtendedRect;
  Azoom: byte): TRect;
begin
  Result := RelativeRect2TileRectInternal(XY, Azoom);
end;

function TCoordConverterAbstract.PixelPos2TilePos(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result := PixelPos2TilePosInternal(XY, Azoom);
end;

function TCoordConverterAbstract.PixelRect2TileRect(const XY: TRect;
  AZoom: byte): TRect;
begin
  Result := PixelRect2TileRectInternal(XY, Azoom);
end;

function TCoordConverterAbstract.TileRect2PixelRect(const XY: TRect;
  AZoom: byte): TRect;
begin
  Result := TileRect2PixelRectInternal(XY, Azoom);
end;

function TCoordConverterAbstract.PixelRect2RelativeRect(const XY: TRect;
  AZoom: byte): TExtendedRect;
begin
  Result := PixelRect2RelativeRectInternal(XY, Azoom);
end;

function TCoordConverterAbstract.TilePos2PixelPos(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result := TilePos2PixelPosInternal(XY, Azoom);
end;

function TCoordConverterAbstract.LonLat2TilePos(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  Result := LonLat2TilePosInternal(LL, Azoom);
end;

function TCoordConverterAbstract.LonLat2TilePosf(const Ll: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := LonLat2TilePosfInternal(LL, Azoom);
end;

function TCoordConverterAbstract.LonLat2Pos(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  Result := LonLat2PosInternal(LL, Azoom);
end;

function TCoordConverterAbstract.LonLat2Relative(
  const XY: TExtendedPoint): TExtendedPoint;
begin
  Result := LonLat2RelativeInternal(XY);
end;

function TCoordConverterAbstract.Pos2LonLat(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := Pos2LonLatInternal(XY, Azoom);
end;

end.

unit u_CoordConverterBasic;

interface

uses
  Types,
  i_ICoordConverter,
  t_GeoTypes,
  u_CoordConverterAbstract;

type
  TCoordConverterBasic = class(TCoordConverterAbstract, ICoordConverterSimple)
  public
    function Pos2LonLat(const AXY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function LonLat2Pos(const AXY: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall;
  end;
implementation

function TCoordConverterBasic.LonLat2Pos(const AXY: TExtendedPoint;
  Azoom: byte): Tpoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatPosInternal(VXY);
  if Azoom > 23 then begin
    VZoom := VZoom - 8;
    CheckZoomInternal(VZoom);
    Result := LonLat2PixelPosInternal(VXY, Vzoom);
  end else begin
    CheckZoomInternal(VZoom);
    Result := LonLat2TilePosInternal(VXY, Vzoom);
  end;
end;

function TCoordConverterBasic.Pos2LonLat(const AXY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  if Azoom > 23 then begin
    VZoom := VZoom - 8;
    CheckPixelPosInternal(VXY, VZoom);
    Result := PixelPos2LonLatInternal(VXY, Vzoom);
  end else begin
    CheckTilePosInternal(VXY, VZoom);
    Result := TilePos2LonLatInternal(VXY, Vzoom);
  end;
end;

end.

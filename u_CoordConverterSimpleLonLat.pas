unit u_CoordConverterSimpleLonLat;

interface
uses
  Types,
  u_CoordConverterAbstract;
type
  TCoordConverterSimpleLonLat = class(TCoordConverterAbstract)
  public
    function Pos2LonLat(XY : TPoint; Azoom : byte) : TExtendedPoint; override;
    function LonLat2Pos(Ll : TExtendedPoint; Azoom : byte) : Tpoint; override;
  end;

implementation

{ TCoordConverterSimpleLonLat }

function TCoordConverterSimpleLonLat.LonLat2Pos(Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
var
  TilesAtZoom : Integer;
begin
  TilesAtZoom := 1 shl (Azoom+8);
  Result.x := round(TilesAtZoom / 2 + Ll.x * (TilesAtZoom / 360));
  Result.y := round(TilesAtZoom / 2 - Ll.y * (TilesAtZoom / 360));
end;

function TCoordConverterSimpleLonLat.Pos2LonLat(XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  TilesAtZoom : Integer;
begin
  TilesAtZoom := 1 shl Azoom;
  //XY.x := XY.x mod TilesAtZoom;
  if XY.x < 0 then XY.x := XY.x + TilesAtZoom;
  Result.X := (XY.x - TilesAtZoom / 2) / (TilesAtZoom / 360);
  Result.y := -(XY.y - TilesAtZoom / 2) / (TilesAtZoom / 360);
end;

end.

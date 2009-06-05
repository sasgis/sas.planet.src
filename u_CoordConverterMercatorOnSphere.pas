unit u_CoordConverterMercatorOnSphere;

interface
uses
  Types,
  u_CoordConverterAbstract;
type
  TCoordConverterMercatorOnSphere = class(TCoordConverterAbstract)
  public
    function Pos2LonLat(XY : TPoint; Azoom : byte) : TExtendedPoint; override;
    function LonLat2Pos(Ll : TExtendedPoint; Azoom : byte) : Tpoint; override;
  end;

implementation

{ TCoordConverterMercatorOnSphere }

function TCoordConverterMercatorOnSphere.LonLat2Pos(Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
var
  TilesAtZoom : Integer;
  z, c : Extended;
begin
  TilesAtZoom := 1 shl Azoom;
  Result.x := round(TilesAtZoom / 2 + Ll.x * (TilesAtZoom / 360));
  z := sin(Ll.y * Pi / 180);
  c := (TilesAtZoom / (2 * Pi));
  Result.y := round(TilesAtZoom / 2 - 0.5 * ln((1 + z) / (1 - z)) * c);
end;

function TCoordConverterMercatorOnSphere.Pos2LonLat(XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  TilesAtZoom : Integer;
begin
  TilesAtZoom := 1 shl Azoom;
//  XY.x := XY.x mod TilesAtZoom;
  if XY.x < 0 then XY.x := XY.x + TilesAtZoom;
  Result.X := (XY.x - TilesAtZoom / 2) / (TilesAtZoom / 360);
  Result.Y := (XY.y - TilesAtZoom / 2) / -(TilesAtZoom / (2*PI));
  Result.Y := (2 * arctan(exp(Result.Y)) - PI / 2) * 180 / PI;
end;

end.

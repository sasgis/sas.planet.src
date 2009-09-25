unit u_CoordConverterMercatorOnSphere;

interface

uses
  Types,
  Math,
  t_GeoTypes,
  u_CoordConverterAbstract;

type
  TCoordConverterMercatorOnSphere = class(TCoordConverterAbstract)
  protected
    FRadiusa : Extended;
  public
    constructor Create(Aradiusa: Extended);
    function Pos2LonLat(XY: TPoint; Azoom : byte): TExtendedPoint; override;
    function LonLat2Pos(Ll: TExtendedPoint; Azoom: byte): Tpoint; override;
    function LonLat2Metr(Ll: TExtendedPoint): TExtendedPoint; override;
  end;

implementation

{ TCoordConverterMercatorOnSphere }

constructor TCoordConverterMercatorOnSphere.Create(Aradiusa: Extended);
begin
  inherited Create();
  Fradiusa:=Aradiusa;
end;

function TCoordConverterMercatorOnSphere.LonLat2Pos(Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
var
  TilesAtZoom : Integer;
  z, c : Extended;
begin
  TilesAtZoom := (1 shl Azoom)*256;
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
  TilesAtZoom := (1 shl Azoom)*256;
//  XY.x := XY.x mod TilesAtZoom;
  if XY.x < 0 then XY.x := XY.x + TilesAtZoom;
  Result.X := (XY.x - TilesAtZoom / 2) / (TilesAtZoom / 360);
  Result.Y := (XY.y - TilesAtZoom / 2) / -(TilesAtZoom / (2*PI));
  Result.Y := (2 * arctan(exp(Result.Y)) - PI / 2) * 180 / PI;
end;

function TCoordConverterMercatorOnSphere.LonLat2Metr(Ll : TExtendedPoint) : TExtendedPoint;
begin
  ll.x:=ll.x*(Pi/180);
  ll.y:=ll.y*(Pi/180);
  result.x:=Fradiusa*ll.x;
  result.y:=Fradiusa*Ln(Tan(PI/4+ll.y/2));
end;

end.

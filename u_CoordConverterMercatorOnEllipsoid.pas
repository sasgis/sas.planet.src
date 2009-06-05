unit u_CoordConverterMercatorOnEllipsoid;

interface
uses
  Types,
  u_CoordConverterAbstract;
type
  TCoordConverterMercatorOnEllipsoid = class(TCoordConverterAbstract)
  protected
    FExct : Extended;
//    radiusa,radiusb,exct:extended;
  public
    constructor Create(AExct : Extended);
    function Pos2LonLat(XY : TPoint; Azoom : byte) : TExtendedPoint; override;
    function LonLat2Pos(Ll : TExtendedPoint; Azoom : byte) : Tpoint; override;
  end;

implementation
uses
  Math;
const
  MerkElipsK=0.0000001;

{ TCoordConverterMercatorOnEllipsoid }

constructor TCoordConverterMercatorOnEllipsoid.Create(AExct: Extended);
begin
  inherited Create();
  FExct := AExct;
end;

function TCoordConverterMercatorOnEllipsoid.LonLat2Pos(Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
var
  TilesAtZoom : Integer;
  z, c : Extended;
begin
  TilesAtZoom := 1 shl Azoom;
  Result.x := round(TilesAtZoom / 2 + Ll.x * (TilesAtZoom / 360));
  z := sin(Ll.y * Pi / 180);
  c := (TilesAtZoom / (2 * Pi));
  Result.y := round(TilesAtZoom / 2 - c*(ArcTanh(z)-FExct*ArcTanh(FExct*z)));
end;

function TCoordConverterMercatorOnEllipsoid.Pos2LonLat(XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  TilesAtZoom : Integer;
  zu, zum1, yy : extended;
begin
  TilesAtZoom := 1 shl Azoom;
//  XY.x := XY.x mod TilesAtZoom;
  if XY.x < 0 then XY.x := XY.x + TilesAtZoom;
  Result.X := (XY.x - TilesAtZoom / 2) / (TilesAtZoom / 360);
  Result.Y := (XY.y - TilesAtZoom / 2) / -(TilesAtZoom / (2*PI));
  Result.Y := (2 * arctan(exp(Result.Y)) - PI / 2) * 180 / PI;
  Zu := result.y / (180 / Pi);
  yy := ((XY.y) - TilesAtZoom / 2);
  repeat
   Zum1 := Zu;
   Zu := arcsin(1-((1+Sin(Zum1))*power(1-FExct*sin(Zum1),FExct))/(exp((2*yy)/-(TilesAtZoom/(2*Pi)))*power(1+FExct*sin(Zum1),FExct)));
  until (abs(Zum1 - Zu) < MerkElipsK) or (isNAN(Zu));
  if not(isNAN(Zu)) then begin
    Result.Y:=zu*180/Pi;
  end;
end;

end.

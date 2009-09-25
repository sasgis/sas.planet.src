unit u_CoordConverterSimpleLonLat;

interface

uses
  Types,
  t_GeoTypes,
  u_CoordConverterAbstract;

type
  TCoordConverterSimpleLonLat = class(TCoordConverterAbstract)
  protected
    FRadiusa : Extended;
  public
    constructor Create(Aradiusa: Extended);
    function Pos2LonLat(XY : TPoint; Azoom : byte) : TExtendedPoint; override;
    function LonLat2Pos(Ll : TExtendedPoint; Azoom : byte) : Tpoint; override;
	  function LonLat2Metr(Ll : TExtendedPoint) : TExtendedPoint; override;
  end;

implementation

{ TCoordConverterSimpleLonLat }

constructor TCoordConverterSimpleLonLat.Create(Aradiusa: Extended);
begin
  FRadiusa := Aradiusa;
end;

function TCoordConverterSimpleLonLat.LonLat2Pos(Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
var
  TilesAtZoom : Integer;
begin
  TilesAtZoom := (1 shl Azoom)*256;
  Result.x := round(TilesAtZoom / 2 + Ll.x * (TilesAtZoom / 360));
  Result.y := round(TilesAtZoom / 2 - Ll.y * (TilesAtZoom / 360));
end;

function TCoordConverterSimpleLonLat.Pos2LonLat(XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  TilesAtZoom : Integer;
begin
  TilesAtZoom := (1 shl Azoom)*256;
  //XY.x := XY.x mod TilesAtZoom;
  if XY.x < 0 then XY.x := XY.x + TilesAtZoom;
  Result.X := (XY.x - TilesAtZoom / 2) / (TilesAtZoom / 360);
  Result.y := -(XY.y - TilesAtZoom / 2) / (TilesAtZoom / 360);
end;

function TCoordConverterSimpleLonLat.LonLat2Metr(Ll : TExtendedPoint) : TExtendedPoint;
begin
  result.x:=0;
  result.y:=0;
end;

end.

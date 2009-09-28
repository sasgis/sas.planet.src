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
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended; override;
  end;

implementation

uses
  Math;

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
  TilesAtZoom := (1 shl Azoom);
  Result.x := round(TilesAtZoom / 2 + Ll.x * (TilesAtZoom / 360));
  Result.y := round(TilesAtZoom / 2 - Ll.y * (TilesAtZoom / 360));
end;

function TCoordConverterSimpleLonLat.Pos2LonLat(XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  TilesAtZoom : Integer;
begin
  TilesAtZoom := (1 shl Azoom);
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

function TCoordConverterSimpleLonLat.CalcDist(AStart,
  AFinish: TExtendedPoint): Extended;
const
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы
var
  fdLambda,fdPhi,fz,a:Double;
  VStart, VFinish: TExtendedPoint; // Координаты в радианах
begin
  result := 0;
  if (AStart.X = AFinish.X) and (AStart.Y = AFinish.Y) then exit;
  a := FRadiusa;

  VStart.X := AStart.X * D2R;
  VStart.Y := AStart.Y * D2R;
  VFinish.X := AFinish.X * D2R;
  VFinish.Y := AFinish.Y * D2R;

  fdLambda := VStart.X - VFinish.X;
  fdPhi := VStart.Y - VFinish.Y;
  fz:=Sqrt(Power(Sin(fdPhi/2),2)+Cos(VFinish.Y)*Cos(VStart.Y)*Power(Sin(fdLambda/2),2));
  fz := 2*ArcSin(fz);
  result := (fz * a);
end;

end.

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
    function LonLat2Metr(const ALl: TExtendedPoint): TExtendedPoint; override;
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended; override;
    function LonLat2Relative(const XY : TExtendedPoint): TExtendedPoint; override; stdcall;
    function Relative2LonLat(const XY : TExtendedPoint): TExtendedPoint; override; stdcall;
  end;

implementation

{ TCoordConverterMercatorOnSphere }

constructor TCoordConverterMercatorOnSphere.Create(Aradiusa: Extended);
begin
  inherited Create();
  Fradiusa:=Aradiusa;
end;

function TCoordConverterMercatorOnSphere.LonLat2Metr(const ALl : TExtendedPoint) : TExtendedPoint;
var
  VLl : TExtendedPoint;
begin
  VLl := ALl;
  Vll.x:=Vll.x*(Pi/180);
  Vll.y:=Vll.y*(Pi/180);
  result.x:=Fradiusa*Vll.x;
  result.y:=Fradiusa*Ln(Tan(PI/4+Vll.y/2));
end;

function TCoordConverterMercatorOnSphere.CalcDist(AStart,
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

function TCoordConverterMercatorOnSphere.LonLat2Relative(
  const XY: TExtendedPoint): TExtendedPoint;
var
  z, c : Extended;
begin
  Result.x := 0.5 + XY.x  / 360;
  z := sin(XY.y * Pi / 180);
  c := 1 / (2 * Pi);
  Result.y := 0.5 - 0.5 * ln((1 + z) / (1 - z)) * c;
end;

function TCoordConverterMercatorOnSphere.Relative2LonLat(
  const XY: TExtendedPoint): TExtendedPoint;
begin
  Result.X := (XY.x - 0.5) * 360;
  Result.Y := -(XY.y - 0.5) *(2*PI);
  Result.Y := (2 * arctan(exp(Result.Y)) - PI / 2) * 180 / PI;
end;

end.

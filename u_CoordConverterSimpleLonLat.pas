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
	  function LonLat2MetrInternal(const ALl : TExtendedPoint) : TExtendedPoint; override;
    function LonLat2RelativeInternal(const XY : TExtendedPoint): TExtendedPoint; override; stdcall;
    function Relative2LonLatInternal(const XY : TExtendedPoint): TExtendedPoint; override; stdcall;
  public
    constructor Create(Aradiusa: Extended);
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended; override;
  end;

implementation

uses
  Math;

{ TCoordConverterSimpleLonLat }

constructor TCoordConverterSimpleLonLat.Create(Aradiusa: Extended);
begin
  inherited Create;
  FRadiusa := Aradiusa;
end;

function TCoordConverterSimpleLonLat.LonLat2MetrInternal(const ALl : TExtendedPoint) : TExtendedPoint;
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

function TCoordConverterSimpleLonLat.LonLat2RelativeInternal(
  const XY: TExtendedPoint): TExtendedPoint;
begin
  Result.x := (0.5 + XY.x / 360);
  Result.y := (0.5 - XY.y / 360);
end;

function TCoordConverterSimpleLonLat.Relative2LonLatInternal(
  const XY: TExtendedPoint): TExtendedPoint;
begin
  Result.X := (XY.x - 0.5) * 360;
  Result.y := -(XY.y - 0.5) * 360;
end;

end.

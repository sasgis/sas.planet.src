unit u_CoordConverterMercatorOnSphere;

interface

uses
  Types,
  Math,
  t_GeoTypes,
  u_CoordConverterBasic;

type
  TCoordConverterMercatorOnSphere = class(TCoordConverterBasic)
  protected
    function LonLat2MetrInternal(const ALl: TExtendedPoint): TExtendedPoint; override;
    function LonLat2MetrS(ALL:TExtendedPoint):TExtendedPoint; override;
    function LonLat2RelativeInternal(const XY: TExtendedPoint): TExtendedPoint; override; stdcall;
    function Relative2LonLatInternal(const XY: TExtendedPoint): TExtendedPoint; override; stdcall;
  public
    constructor Create(Aradiusa: Extended);
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended; override;
  end;

implementation

{ TCoordConverterMercatorOnSphere }

constructor TCoordConverterMercatorOnSphere.Create(Aradiusa: Extended);
begin
  inherited Create;
  Fradiusa := Aradiusa;
  if Abs(FRadiusa - 6378137) < 1 then begin
    FDatumEPSG := 7059;
    FProjEPSG := 3785;
    FCellSizeUnits := CELL_UNITS_METERS;
  end else if Abs(FRadiusa - 6371000) < 1 then begin
    FDatumEPSG := 53004;
    FProjEPSG := 53004;
    FCellSizeUnits := CELL_UNITS_METERS;
  end else begin
    FDatumEPSG := 0;
    FProjEPSG := 0;
    FCellSizeUnits := CELL_UNITS_UNKNOWN;
  end;

end;

function TCoordConverterMercatorOnSphere.LonLat2MetrS(ALL:TExtendedPoint):TExtendedPoint;
begin
  All.x:=All.x*(Pi/180);
  All.Y:=All.y*(Pi/180);
  result.x:=FRadiusa*All.x/2;
  result.y:=FRadiusa*Ln(Tan(PI/4+All.y/2)*
            Power((1-0*Sin(all.y))/(1+0*Sin(All.y)),0/2))/2;
end;

function TCoordConverterMercatorOnSphere.LonLat2MetrInternal(const ALl: TExtendedPoint): TExtendedPoint;
var
  VLl: TExtendedPoint;
begin
  VLl := ALl;
  Vll.x := Vll.x * (Pi / 180);
  Vll.y := Vll.y * (Pi / 180);
  result.x := Fradiusa * Vll.x;
  result.y := Fradiusa * Ln(Tan(PI / 4 + Vll.y / 2));
end;

function TCoordConverterMercatorOnSphere.CalcDist(AStart,
  AFinish: TExtendedPoint): Extended;
const
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы
var
  fdLambda, fdPhi, fz, a: Double;
  VStart, VFinish: TExtendedPoint; // Координаты в радианах
begin
  result := 0;
  if (AStart.X = AFinish.X) and (AStart.Y = AFinish.Y) then begin
    exit;
  end;
  a := FRadiusa;

  VStart.X := AStart.X * D2R;
  VStart.Y := AStart.Y * D2R;
  VFinish.X := AFinish.X * D2R;
  VFinish.Y := AFinish.Y * D2R;

  fdLambda := VStart.X - VFinish.X;
  fdPhi := VStart.Y - VFinish.Y;
  fz := Sqrt(Power(Sin(fdPhi / 2), 2) + Cos(VFinish.Y) * Cos(VStart.Y) * Power(Sin(fdLambda / 2), 2));
  fz := 2 * ArcSin(fz);
  result := (fz * a);
end;

function TCoordConverterMercatorOnSphere.LonLat2RelativeInternal(const XY: TExtendedPoint): TExtendedPoint;
var
  z, c: Extended;
begin
  Result.x := 0.5 + XY.x / 360;
  z := sin(XY.y * Pi / 180);
  c := 1 / (2 * Pi);
  Result.y := 0.5 - 0.5 * ln((1 + z) / (1 - z)) * c;
end;

function TCoordConverterMercatorOnSphere.Relative2LonLatInternal(
  const XY: TExtendedPoint): TExtendedPoint;
begin
  Result.X := (XY.x - 0.5) * 360;
  Result.Y := -(XY.y - 0.5) * (2 * PI);
  Result.Y := (2 * arctan(exp(Result.Y)) - PI / 2) * 180 / PI;
end;

end.

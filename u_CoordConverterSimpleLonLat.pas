unit u_CoordConverterSimpleLonLat;

interface

uses
  Types,
  t_GeoTypes,
  u_CoordConverterBasic;

type
  TCoordConverterSimpleLonLat = class(TCoordConverterBasic)
  protected
    FExct, FRadiusb: Double;
	   function LonLat2MetrInternal(const ALl: TDoublePoint): TDoublePoint; override;
    function LonLat2MetrS(ALL: TDoublePoint): TDoublePoint; override;
    function LonLat2RelativeInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
    function Relative2LonLatInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
  public
    constructor Create(Aradiusa, Aradiusb: Double);
    function CalcDist(AStart: TDoublePoint; AFinish: TDoublePoint): Double; override;
  end;

implementation

uses
  Math;

{ TCoordConverterSimpleLonLat }

constructor TCoordConverterSimpleLonLat.Create(Aradiusa, Aradiusb: Double);
begin
  inherited Create;
  FRadiusa := Aradiusa;
  FRadiusb := Aradiusb;
  FExct := sqrt(FRadiusa * FRadiusa - FRadiusb * FRadiusb) / FRadiusa;
  if (Abs(FRadiusa - 6378137) < 1) and (Abs(FRadiusb - 6356752) < 1) then begin
    FProjEPSG := 4326;
    FDatumEPSG := 4326;
    FCellSizeUnits := CELL_UNITS_DEGREES;
  end else begin
    FDatumEPSG := 0;
    FProjEPSG := 0;
    FCellSizeUnits := CELL_UNITS_UNKNOWN;
  end;
end;

function TCoordConverterSimpleLonLat.LonLat2MetrS(ALL: TDoublePoint): TDoublePoint;
begin
  All.x := All.x * (Pi / 180);
  All.Y := All.y * (Pi / 180);
  result.x := FRadiusa * All.x / 2;
  result.y := FRadiusa * Ln(Tan(PI / 4 + All.y / 2) *
    Power((1 - FExct * Sin(all.y)) / (1 + FExct * Sin(All.y)), FExct / 2)) / 2;
end;

function TCoordConverterSimpleLonLat.LonLat2MetrInternal(const ALl: TDoublePoint): TDoublePoint;
var
  VLL: TDoublePoint;
  b, bs: extended;
begin
  VLL := ALL;
  Vll.x := Vll.x * (Pi / 180);
  Vll.y := Vll.y * (Pi / 180);
  result.x := Fradiusa * Vll.x;

  bs := FExct * sin(VLl.y);
  b := Tan((Vll.y + PI / 2) / 2) * power((1 - bs) / (1 + bs), (FExct / 2));
  if b <= 0 then begin
    b := 0.00000000000001;
  end;
  result.y := Fradiusa * Ln(b);
end;

function TCoordConverterSimpleLonLat.CalcDist(AStart,
  AFinish: TDoublePoint): Double;
const
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы
var
  fPhimean, fdLambda, fdPhi, fAlpha, fRho, fNu, fR, fz, fTemp, a, e2: Double;
  VStart, VFinish: TDoublePoint; // Координаты в радианах
begin
  result := 0;
  if (AStart.X = AFinish.X) and (AStart.Y = AFinish.Y) then begin
    exit;
  end;
  e2 := FExct * FExct;
  a := FRadiusa;

  VStart.X := AStart.X * D2R;
  VStart.Y := AStart.Y * D2R;
  VFinish.X := AFinish.X * D2R;
  VFinish.Y := AFinish.Y * D2R;

  fdLambda := VStart.X - VFinish.X;
  fdPhi := VStart.Y - VFinish.Y;
  fPhimean := (VStart.Y + VFinish.Y) / 2.0;
  fTemp := 1 - e2 * (Power(Sin(fPhimean), 2));
  fRho := (a * (1 - e2)) / Power(fTemp, 1.5);
  fNu := a / (Sqrt(1 - e2 * (Sin(fPhimean) * Sin(fPhimean))));
  fz := Sqrt(Power(Sin(fdPhi / 2), 2) + Cos(VFinish.Y) * Cos(VStart.Y) * Power(Sin(fdLambda / 2), 2));
  fz := 2 * ArcSin(fz);
  fAlpha := Cos(VFinish.Y) * Sin(fdLambda) * 1 / Sin(fz);
  fAlpha := ArcSin(fAlpha);
  fR := (fRho * fNu) / ((fRho * Power(Sin(fAlpha), 2)) + (fNu * Power(Cos(fAlpha), 2)));
  result := (fz * fR);
end;

function TCoordConverterSimpleLonLat.LonLat2RelativeInternal(
  const XY: TDoublePoint): TDoublePoint;
begin
  Result.x := (0.5 + XY.x / 360);
  Result.y := (0.5 - XY.y / 360);
end;

function TCoordConverterSimpleLonLat.Relative2LonLatInternal(
  const XY: TDoublePoint): TDoublePoint;
begin
  Result.X := (XY.x - 0.5) * 360;
  Result.y := -(XY.y - 0.5) * 360;
end;

end.

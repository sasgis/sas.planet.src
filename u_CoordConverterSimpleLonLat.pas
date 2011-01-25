unit u_CoordConverterSimpleLonLat;

interface

uses
  Types,
  t_GeoTypes,
  u_CoordConverterBasic;

type
  TCoordConverterSimpleLonLat = class(TCoordConverterBasic)
  protected
    FExct: Double;
    function LonLat2MetrInternal(const ALl: TDoublePoint): TDoublePoint; override;
    function LonLat2RelativeInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
    function Relative2LonLatInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
  public
    constructor Create(Aradiusa, Aradiusb: Double);
  end;

implementation

uses
  Math,
  u_Datum;

{ TCoordConverterSimpleLonLat }

constructor TCoordConverterSimpleLonLat.Create(Aradiusa, Aradiusb: Double);
begin
  FExct := sqrt(ARadiusa * ARadiusa - ARadiusb * ARadiusb) / ARadiusa;
  if (Abs(ARadiusa - 6378137) < 1) and (Abs(ARadiusb - 6356752) < 1) then begin
    inherited Create(TDatum.Create(4326, Aradiusa, Aradiusb));
    FProjEPSG := 4326;
    FCellSizeUnits := CELL_UNITS_DEGREES;
  end else begin
    inherited Create(TDatum.Create(0, Aradiusa, Aradiusb));
    FProjEPSG := 0;
    FCellSizeUnits := CELL_UNITS_UNKNOWN;
  end;
end;

function TCoordConverterSimpleLonLat.LonLat2MetrInternal(const ALl: TDoublePoint): TDoublePoint;
var
  VLL: TDoublePoint;
  b, bs: extended;
begin
  VLL := ALL;
  Vll.x := Vll.x * (Pi / 180);
  Vll.y := Vll.y * (Pi / 180);
  result.x := FDatum.GetSpheroidRadiusA * Vll.x;

  bs := FExct * sin(VLl.y);
  b := Tan((Vll.y + PI / 2) / 2) * power((1 - bs) / (1 + bs), (FExct / 2));
  if b <= 0 then begin
    b := 0.00000000000001;
  end;
  result.y := FDatum.GetSpheroidRadiusA * Ln(b);
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

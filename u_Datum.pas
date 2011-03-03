unit u_Datum;

interface

uses
  t_GeoTypes,
  i_IDatum;

type
  TDatum = class(TInterfacedObject, IDatum)
  private
    FEPSG: Integer;
    FRadiusA: Double;
    FRadiusB: Double;
    FExct: Double;
    function LonLat2MetrS(ALL: TDoublePoint): TDoublePoint;
  protected
    function GetEPSG: integer; stdcall;
    function GetSpheroidRadiusA: Double; stdcall;
    function GetSpheroidRadiusB: Double; stdcall;
    function IsSameDatum(ADatum: IDatum): Boolean; stdcall;

    function CalcPoligonArea(polygon: TArrayOfDoublePoint): Double;
    function CalcDist(AStart: TDoublePoint; AFinish: TDoublePoint): Double;
  public
    constructor Create(
      AEPSG: Integer;
      ARadiusA: Double;
      ARadiusB: Double
    ); overload;
    constructor Create(
      AEPSG: Integer;
      ARadiusA: Double
    ); overload;
  end;

implementation

uses
  Math;

{ TDatum }

constructor TDatum.Create(AEPSG: Integer; ARadiusA, ARadiusB: Double);
begin
  FEPSG := AEPSG;
  FRadiusA := ARadiusA;
  FRadiusB := ARadiusB;
  FExct := sqrt(FRadiusA * FRadiusA - FRadiusB * FRadiusB) / FRadiusA;
end;

function TDatum.CalcDist(AStart, AFinish: TDoublePoint): Double;
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

function TDatum.CalcPoligonArea(polygon: TArrayOfDoublePoint): Double;
var
  L, i: integer;
  LLPrev, LLCurr: TDoublePoint;
begin
  result := 0;
  l := length(polygon);
  LLPrev := LonLat2MetrS(polygon[0]);
  for i := 1 to L - 1 do begin
    LLCurr := LonLat2MetrS(polygon[i]);
    result := result + (LLPrev.x + LLCurr.x) * (LLPrev.y - LLCurr.y);
    LLPrev := LLCurr;
  end;
  result := 0.5 * abs(result) / 1000000;
end;

constructor TDatum.Create(AEPSG: Integer; ARadiusA: Double);
begin
  Create(AEPSG, ARadiusA, ARadiusA);
end;

function TDatum.GetEPSG: integer;
begin
  Result := FEPSG;
end;

function TDatum.GetSpheroidRadiusA: Double;
begin
  Result := FRadiusA;
end;

function TDatum.GetSpheroidRadiusB: Double;
begin
  Result := FRadiusB
end;

function TDatum.IsSameDatum(ADatum: IDatum): Boolean;
begin
  Result := (ADatum.EPSG <> 0) and (FEPSG <> 0) and (FEPSG = ADatum.EPSG);
end;

function TDatum.LonLat2MetrS(ALL: TDoublePoint): TDoublePoint;
begin
  All.x := All.x * (Pi / 180);
  All.Y := All.y * (Pi / 180);
  result.x := FRadiusa * All.x / 2;
  result.y := FRadiusa * Ln(Tan(PI / 4 + All.y / 2) *
    Power((1 - FExct * Sin(all.y)) / (1 + FExct * Sin(All.y)), FExct / 2)) / 2;
end;

end.

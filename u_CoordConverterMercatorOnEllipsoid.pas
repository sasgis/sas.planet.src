unit u_CoordConverterMercatorOnEllipsoid;

interface

uses
  Types,
  t_GeoTypes,
  u_CoordConverterAbstract;

type
  TCoordConverterMercatorOnEllipsoid = class(TCoordConverterAbstract)
  protected
    FExct,FRadiusb: Extended;
    function LonLat2MetrInternal(const ALL: TExtendedPoint): TExtendedPoint; override;
    function LonLat2RelativeInternal(const XY: TExtendedPoint): TExtendedPoint; override; stdcall;
    function Relative2LonLatInternal(const XY: TExtendedPoint): TExtendedPoint; override; stdcall;
  public
    constructor Create(Aradiusa, Aradiusb: Extended);
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended; override;
  end;

implementation

uses
  Math;

const
  MerkElipsK=0.000000001;

{ TCoordConverterMercatorOnEllipsoid }

constructor TCoordConverterMercatorOnEllipsoid.Create(Aradiusa, Aradiusb: Extended);
begin
  inherited Create;
  Fradiusa:=Aradiusa;
  Fradiusb:=Aradiusb;
  FExct := sqrt(FRadiusa*FRadiusa - FRadiusb*FRadiusb)/FRadiusa;
  if (Abs(FRadiusa - 6378137) <  1) and (Abs(FRadiusb - 6356752) <  1) then begin
    FProjEPSG := 3395;
    FDatumEPSG := 3395;
    FCellSizeUnits := CELL_UNITS_METERS;
  end else begin
    FDatumEPSG := 0;
    FProjEPSG := 0;
    FCellSizeUnits := CELL_UNITS_UNKNOWN;
  end;

end;

function TCoordConverterMercatorOnEllipsoid.LonLat2MetrInternal(const ALl: TExtendedPoint): TExtendedPoint;
var
  VLL: TExtendedPoint;
  b,bs:extended;
begin
  VLL := ALL;
  Vll.x:=Vll.x*(Pi/180);
  Vll.y:=Vll.y*(Pi/180);
  result.x:=Fradiusa*Vll.x;

  bs:=FExct*sin(VLl.y);
  b:=Tan((Vll.y+PI/2)/2) * power((1-bs)/(1+bs),(FExct/2));
  result.y:=Fradiusa*Ln(b);
end;

function TCoordConverterMercatorOnEllipsoid.CalcDist(AStart,
  AFinish: TExtendedPoint): Extended;
const
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы
var
  fPhimean,fdLambda,fdPhi,fAlpha,fRho,fNu,fR,fz,fTemp,a,e2:Double;
  VStart, VFinish: TExtendedPoint; // Координаты в радианах
begin
  result := 0;
  if (AStart.X = AFinish.X) and (AStart.Y = AFinish.Y) then exit;
  e2 := FExct*FExct;
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
  fz:=Sqrt(Power(Sin(fdPhi/2),2)+Cos(VFinish.Y)*Cos(VStart.Y)*Power(Sin(fdLambda/2),2));
  fz := 2*ArcSin(fz);
  fAlpha := Cos(VFinish.Y) * Sin(fdLambda) * 1 / Sin(fz);
  fAlpha := ArcSin(fAlpha);
  fR:=(fRho*fNu)/((fRho*Power(Sin(fAlpha),2))+(fNu*Power(Cos(fAlpha),2)));
  result := (fz * fR);
end;

function TCoordConverterMercatorOnEllipsoid.LonLat2RelativeInternal(
  const XY: TExtendedPoint): TExtendedPoint;
var
  z, c: Extended;
  VLL: TExtendedPoint;
begin
  VLL := XY;
  Result.x := (0.5 + VLl.x / 360);
  z := sin(VLl.y * Pi / 180);
  c := (1 / (2 * Pi));
  Result.y := (0.5 - c*(ArcTanh(z)-FExct*ArcTanh(FExct*z)));
end;

function TCoordConverterMercatorOnEllipsoid.Relative2LonLatInternal(
  const XY: TExtendedPoint): TExtendedPoint;
var
  zu, zum1, yy: extended;
  VXY: TExtendedPoint;
  VSin: Extended;
  e_y: Extended;
begin
  VXY := XY;
  Result.X := (VXY.x - 0.5) * 360;

  if (VXY.y>0.5) then begin
    yy:=(VXY.y - 0.5);
  end else begin
    yy:=(0.5 - VXY.y);
  end;
  yy := yy * (2*PI);
  Zu := 2 * arctan(exp(yy)) - PI / 2;
  e_y := exp(2*yy);
  Result.Y := Zu * (180 / Pi);
  repeat
    Zum1 := Zu;
    VSin := Sin(Zum1);
    Zu := arcsin(1 - (1 + VSin)*power((1 - FExct*VSin)/(1 + FExct*VSin),FExct)/e_y);
  until (abs(Zum1 - Zu) < MerkElipsK) or (isNAN(Zu));
  if not(isNAN(Zu)) then begin
    if VXY.y>0.5 then begin
      result.Y:=-zu*180/Pi;
    end else begin
      result.Y:=zu*180/Pi;
    end;
  end;
end;

end.

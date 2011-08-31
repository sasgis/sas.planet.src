unit u_Datum;

interface

uses
  t_GeoTypes,
  i_Datum;

type
  TDatum = class(TInterfacedObject, IDatum)
  private
    FEPSG: Integer;
    FRadiusA: Double;
    FRadiusB: Double;
    FExct: Double;
  protected
    function GetEPSG: integer; stdcall;
    function GetSpheroidRadiusA: Double; stdcall;
    function GetSpheroidRadiusB: Double; stdcall;
    function IsSameDatum(ADatum: IDatum): Boolean; stdcall;
    function SphericalTriangleSquare(points:array of TDoublePoint):Double;
    function CalcPoligonArea(const polygon: TArrayOfDoublePoint): Double;
    function CalcDist(const AStart, AFinish: TDoublePoint): Double;
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

const
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы

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

function TDatum.CalcDist(const AStart, AFinish: TDoublePoint): Double;
var
  fSinPhimean, fdLambda, fdPhi, fAlpha, fRho, fNu, fR, fz, fTemp, a, e2: Double;
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
  fz := Sqrt(intPower(Sin(fdPhi / 2), 2) + Cos(VFinish.Y) * Cos(VStart.Y) * intPower(Sin(fdLambda / 2), 2));
  fz := 2 * ArcSin(fz);
  fSinPhimean :=Sin((VStart.Y + VFinish.Y) / 2.0);
  fTemp := 1 - e2 * (fSinPhimean*fSinPhimean);
  fRho := (a * (1 - e2)) / Power(fTemp, 1.5);
  fNu := a / (Sqrt(1 - e2 * (fSinPhimean * fSinPhimean)));
  fAlpha := Cos(VFinish.Y) * Sin(fdLambda) * 1 / Sin(fz);
  fAlpha := ArcSin(fAlpha);
  fR := (fRho * fNu) / ((fRho * intPower(Sin(fAlpha), 2)) + (fNu * intPower(Cos(fAlpha), 2)));

  if abs(fdLambda)<=Pi then begin
    result := (fz * fR);
  end else begin
    result := (Pi * fR)+((Pi * fR)-(fz * fR));
  end;
end;

function TDatum.SphericalTriangleSquare(points: array of TDoublePoint):Double;
var x,y,z:array [0..2] of double;
    a12,a23,a13,s,eps:double;
    i:integer;
begin
  for i := 0 to 2 do begin
    points[i].Y:=Pi/2-points[i].Y*D2R;
    points[i].X:=Pi+  points[i].X*D2R;
    x[i]:=Sin(points[i].Y)*Cos(points[i].X);
    y[i]:=Sin(points[i].Y)*Sin(points[i].X);
    z[i]:=Cos(points[i].Y);
  end;
  a12:=ArcCos(1-(Sqr(x[0]-x[1])+Sqr(y[0]-y[1])+Sqr(z[0]-z[1]))/2);   //Стороны сферического треугольника
  a23:=ArcCos(1-(Sqr(x[1]-x[2])+Sqr(y[1]-y[2])+Sqr(z[1]-z[2]))/2);
  a13:=ArcCos(1-(Sqr(x[0]-x[2])+Sqr(y[0]-y[2])+Sqr(z[0]-z[2]))/2);
  s:=(a12+a23+a13)/2;
  s:=Tan(s/2)*Tan((s-a12)/2)*Tan((s-a23)/2)*Tan((s-a13)/2);
  if s>=0 then begin
    eps:=4*ArcTan(Sqrt(s))  //сферический эксцесс
  end else begin
    eps:=0;
  end;
  Result:=Sqr(FRadiusA)*eps;  //Площадь
end;

function TDatum.CalcPoligonArea(const polygon: TArrayOfDoublePoint): Double;

 function sign(Avalue: double):integer;
 begin
   if AValue < 0 then begin
     Result := -1
   end else begin
     Result := 1;
   end;
 end;

 function Orientation(APoints: TArrayOfDoublePoint):extended;
 var i:integer;
     s:double;
     VPointsCount:integer;
 begin
   s:=0;
   VPointsCount:=length(APoints)-1;
   for i:=0 to VPointsCount-1 do begin
     s:=s+(APoints[(i+1) mod VPointsCount].x-APoints[i].x)*(APoints[(i+1) mod VPointsCount].y+APoints[i].y);
   end;
   result:=-s/2;
 end;

 function Det(point1,point2,point3:TDoublePoint): Extended;
 begin
   Result:=((point2.X-point1.X)*(point3.Y-point1.Y) - (point2.Y-point1.Y)*(point3.X-point1.X))/2;
 end;

 function InTriangle(point,point1,point2,point3:TDoublePoint): Boolean;
 var a,b,c : double;
 begin
   a := det(point,point1,point2);
   b := det(point,point2,point3);
   c := det(point,point3,point1);
   Result:=((a>=0)and(b>=0)and(c>=0))or((a<0)and(b<0)and(c<0));
 end;

var Node : TDoublePoint;
    p:array [0..2] of TDoublePoint;  //вершины треугольника
    pn:array [0..2] of integer;
    Noden : integer;
    PointsNum : integer;
    Orient : Integer;
    inPoint : Boolean;
    s : double;
    i:integer;
    VPointsCount:integer;
    PointsA: Array of integer;
    ErrNum:integer;
begin
   s:=0;
   VPointsCount:=Length(polygon)-1;
   if VPointsCount<3 then
   begin
    result:=s;
    exit;
   end;
   Orient := Sign(Orientation(polygon)); //ориентация многоугольника
   PointsNum:=VPointsCount;
   ErrNum:=0;
   SetLength(PointsA, VPointsCount);
   For NodeN:=0 to VPointsCount-1 do begin
     PointsA[NodeN]:=1;
   end;
   for I := 0 to 2 do begin
     pn[i]:=i;
     p[i] := polygon[i];
   end;
   while (PointsNum > 3)and(ErrNum<=VPointsCount) do begin
     if Sign(Det(p[0],p[1],p[2]))=Orient then begin//Проверка ориентации треугольника
       inPoint := false;
       NodeN:=(pn[2]+1) mod VPointsCount;
       Node := polygon[Noden];
       while NodeN <> pn[0] do begin
         if (PointsA[NodeN]=1)and(InTriangle(Node,p[0],p[1],p[2])) then begin
           inPoint := true;  // Проверка не попала ли вершина в отсекаемый треугольник
         end;
         Noden:=(NodeN+1) mod VPointsCount;
         Node := polygon[NodeN];
       end;
     end else begin
       inPoint:=true;
     end;
     if (not InPoint) then begin
       s:=s+SphericalTriangleSquare(p);
       PointsA[pn[1]]:=0;    //Удаление вершины из рассмотрения
       dec(PointsNum);
       ErrNum:=0;
     end else begin
       pn[0] := pn[1];          //  Переход к следущему треугольнику
       p[0] := polygon[pn[0]];
       inc(ErrNum);
     end;
     pn[1] := pn[2];  //  Переход к следущему треугольнику
     p[1] := polygon[pn[1]];
     pn[2]:=(pn[2]+1) mod VPointsCount;
     while (PointsA[pn[2]]=0)and(pn[2] <> pn[0]) do begin
       pn[2]:=(pn[2]+1) mod VPointsCount;
     end;
     p[2] := polygon[pn[2]];
   end;
   if ErrNum<=VPointsCount then begin
     s:=s+SphericalTriangleSquare(p);
     result:=s;
   end else begin
     result:=NAN;
   end;
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

end.

unit Ugeofun;

interface

uses
  StrUtils,
  SysUtils,
  Types,
  Math,
  UMapType,
  ECWReader,
  t_GeoTypes;

type
 TDMS = record
  D,M,S: extended;
  N:boolean;
 end;

 TRealPoint = record
   X, Y: Real;
 end;

 TResObjType = (ROTpoint,ROTline,ROTPoly);

  TResObj = record
   type_:TResObjType;
   find:boolean;
   S:Extended;
   numid:String;
   name:String;
   descr:String;
  end;

 RUnits = (SUmeter,SUdegrees);
var
  her:boolean;
  function DMS2G(D,M,S:extended;N:boolean):extended;
  function D2DMS(G:extended):TDMS;
  function ExtPoint(X, Y: extended): TExtendedPoint;
  function compare2P(p1,p2:TPoint):boolean;
  function PtInRgn(Polyg:TPointArray; P:TPoint):boolean;
  function PtInPolygon(const Pt: TPoint; const Points:TPointArray): Boolean;
  function compare2EP(p1,p2:TExtendedPoint):boolean;
  function PolygonSquare(Poly:TPointArray): Double;
  function CursorOnLinie(X, Y, x1, y1, x2, y2, d: Integer): Boolean;
  procedure CalculateMercatorCoordinates(LL1,LL2:TExtendedPoint;ImageWidth,ImageHeight:integer;TypeMap:TMapType;
            var CellIncrementX,CellIncrementY,OriginX,OriginY:extended; Units:CellSizeUnits);
  Procedure GetMinMax(var min,max:TPoint; Polyg:TPointArray;round_:boolean);
  function GetDwnlNum(var min,max:TPoint; Polyg:TPointArray; getNum:boolean):Int64;
  function RgnAndRgn(Polyg:TPointArray;x,y:integer;prefalse:boolean):boolean;

implementation

uses
  Unit1;

function RgnAndRgn(Polyg:TPointArray;x,y:integer;prefalse:boolean):boolean;
var i,xm128,ym128,xp128,yp128:integer;
begin
 xm128:=x-128;
 ym128:=y-128;
 if (prefalse=false)and(PtInPolygon(Point(xm128,ym128),polyg)) then begin result:=true; exit; end;
 xp128:=x+128;
 if (prefalse=false)and(PtInPolygon(Point(xp128,ym128),polyg)) then begin result:=true; exit; end;
 yp128:=y+128;
 if PtInPolygon(Point(xp128,yp128),polyg) then begin result:=true; exit; end;
 if PtInPolygon(Point(xm128,yp128),polyg) then begin result:=true; exit; end;
 for i:=0 to length(polyg)-2 do
  begin
   if (polyg[i].x<xp128)and(polyg[i].x>xm128)and(polyg[i].y<yp128)and(polyg[i].y>ym128)
    then begin result:=true; exit; end;
  end;
 result:=false;
end;

Procedure GetMinMax(var min,max:TPoint; Polyg:TPointArray;round_:boolean);
var i:integer;
begin
 max:=Polyg[0];
 min:=Polyg[0];
 for i:=1 to length(Polyg)-1 do
  begin
   if min.x>Polyg[i].x then min.x:=Polyg[i].x;
   if min.y>Polyg[i].y then min.y:=Polyg[i].y;
   if max.x<Polyg[i].x then max.x:=Polyg[i].x;
   if max.y<Polyg[i].y then max.y:=Polyg[i].y;
  end;
 if round_ then
  begin
   max.X:=max.X-1;
   max.Y:=max.Y-1;
   min.X:=min.X+1;
   min.Y:=min.Y+1;
   min.X:=min.x-(min.X mod 256)+128;
   max.X:=max.x-(max.X mod 256)+128;
   min.y:=min.y-(min.y mod 256)+128;
   max.y:=max.y-(max.y mod 256)+128;
  end;
end;

function GetDwnlNum(var min,max:TPoint; Polyg:TPointArray; getNum:boolean):Int64;
var i,j:integer;
    prefalse:boolean;
begin
 GetMinMax(min,max,polyg,true);
 result:=0;
 if getNum then
  if (length(Polyg)=5)and(Polyg[0].x=Polyg[3].x)and(Polyg[1].x=Polyg[2].x)
                      and(Polyg[0].y=Polyg[1].y)and(Polyg[2].y=Polyg[3].y)
    then begin
          result:=int64((max.X-min.X) div 256+1)*int64((max.Y-min.Y) div 256+1);
         end
    else begin
          i:=min.X;
          while i<=max.x do
          begin
           j:=min.y;
           prefalse:=false;
           while j<=max.y do
            begin
             prefalse:=not(RgnAndRgn(Polyg,i,j,prefalse));
             if not(prefalse) then inc(result);
             inc(j,256);
            end;
           inc(i,256);
          end;
         end;
 max.X:=max.X+1;
 max.Y:=max.Y+1;
end;

procedure CalculateMercatorCoordinates(LL1,LL2:TExtendedPoint;ImageWidth,ImageHeight:integer;TypeMap:TMapType;
            var CellIncrementX,CellIncrementY,OriginX,OriginY:extended; Units:CellSizeUnits);
var FN,FE:integer;
    k0,E1,N1,E2,N2:double;
begin
 case Units of
  ECW_CELL_UNITS_METERS:
  begin
  k0:= 1;
  FN:= 0; // False northing
  FE:= 0; // False easting
  ll1:=ExtPoint(ll1.x*D2R,ll1.y*D2R);
  ll2:=ExtPoint(ll2.x*D2R,ll2.y*D2R);
   // Calculate Earth Parameters
  E1:=FE+typemap.radiusa*k0*ll1.x;
  N1:=FN+typemap.radiusa*k0*Ln(Tan(PI/4+ll1.y/2)*
      Power((1-typemap.exct*Sin(ll1.y))/(1+typemap.exct*Sin(ll1.y)),typemap.exct/2));
  E2:=FE+typemap.radiusa*k0*ll2.x;
  N2:=FN+typemap.radiusa*k0*Ln(Tan(PI/4+ll2.y/2)*
      Power((1-typemap.exct*Sin(ll2.y))/(1+typemap.exct*Sin(ll2.y)),typemap.exct/2));
  OriginX:=E1;
  OriginY:=N1;

  CellIncrementX:=(E2-E1)/ImageWidth;
  CellIncrementY:=(N2-N1)/ImageHeight;
  end;
  ECW_CELL_UNITS_DEGREES:
  begin
  OriginX:=ll1.x;
  OriginY:=ll1.y;
  CellIncrementX:=(ll2.x-ll1.x)/ImageWidth;
  CellIncrementY:=-CellIncrementX;
  end;
 end;
end;

function CursorOnLinie(X, Y, x1, y1, x2, y2, d: Integer): Boolean;
var sine,cosinus: Double;
    dx,dy,len: Integer;
begin
  asm
   fild(y2)
   fisub(y1) // Y-Difference
   fild(x2)
   fisub(x1) // X-Difference
   fpatan    // Angle of the line in st(0)
   fsincos   // Cosinus in st(0), Sinus in st(1)
   fstp cosinus
   fstp sine
  end;
  dx:=Round(cosinus*(x-x1)+sine*(y-y1));
  dy:=Round(cosinus*(y-y1)-sine*(x-x1));
  len:=Round(cosinus*(x2-x1)+sine*(y2-y1)); // length of line
  Result:=((dy>-d)and(dy<d)and(dx>-d)and(dx<len+d));
end;

function PolygonSquare(Poly:TPointArray): Double;
var
  I, J, HP: Integer;
begin
  Result := 0;
  HP := High(Poly);
  for I := Low(Poly) to HP do
  begin
    if I = HP then
      J := 0
    else
      J := I + 1;
    Result := Result + (Poly[I].X + Poly[J].X) * (Poly[I].Y - Poly[J].Y);
  end;
  Result := Abs(Result) / 2;
end;

function PtInPolygon(const Pt: TPoint; const Points:TPointArray): Boolean;
var I:Integer;
    iPt,jPt:PPoint;
begin
  Result:=False;
  iPt:=@Points[0];
  jPt:=@Points[High(Points)-1];
  for I:=0 to High(Points)-1 do
  begin
   Result:=Result xor (((Pt.Y>=iPt.Y)xor(Pt.Y>=jPt.Y))and
           (Pt.X-iPt.X<((jPt.X-iPt.X)*(Pt.Y-iPt.Y)/(jPt.Y-iPt.Y))));
   jPt:=iPt;
   Inc(iPt);
  end;
end;

function PtInRgn(Polyg:TPointArray;P:TPoint):boolean;
var i,j:integer;
begin
  result:=false;
  j:=High(Polyg);
  if ((((Polyg[0].y<=P.y)and(P.y<Polyg[j].y))or((Polyg[j].y<=P.y)and(P.y<Polyg[0].y)))and
     (P.x>(Polyg[j].x-Polyg[0].x)*(P.y-Polyg[0].y)/(Polyg[j].y-Polyg[0].y)+Polyg[0].x))
     then result:=not(result);
  for i:=1 to High(Polyg) do
   begin
    j:=i-1;
    if ((((Polyg[i].y<=P.y)and(P.y<Polyg[j].y))or((Polyg[j].y<=P.y)and(P.y<Polyg[i].y)))and
       (P.x>(Polyg[j].x-Polyg[i].x)*(P.y-Polyg[i].y)/(Polyg[j].y-Polyg[i].y)+Polyg[i].x))
       then result:=not(result);
   end;
end;

function compare2P(p1,p2:TPoint):boolean;
begin
 if (p1.x=p2.X)and(p1.y=p2.y) then result:=true
                              else result:=false;
end;

function compare2EP(p1,p2:TExtendedPoint):boolean;
begin
 if (p1.x=p2.X)and(p1.y=p2.y) then result:=true
                              else result:=false;
end;

function ExtPoint(X, Y: extended): TExtendedPoint;
begin
  Result.X:=X;
  Result.Y:=Y;
end;

function DMS2G(D,M,S:extended;N:boolean):extended;
begin
  result:=D+M/60+S/3600;
  if N then result:=-result;
end;

function D2DMS(G:extended):TDMS;
begin
  result.N:=G<0;
  G:=abs(G);
  result.D:=int(G);
  result.M:=int(Frac(G)*60);
  result.S:=Frac(Frac(G)*60)*60;
end;

{
Пересчет широты из СК-42 в WGS-84: WGS84_SK42_Lat(Lat,Long,Height) 
Пересчет долготы из СК-42 в WGS-84: WGS84_SK42_Long(Lat,Long,Height)
Пересчет широты из WGS-84 в СК-42: SK42_WGS84_Lat(Lat,Long,Height)
Пересчет долготы из WGS-84 в СК-42: SK42_WGS84_Long(Lat,Long,Height)
Функции рабочей книги для преобразования геодезических координат из координатной системы Пулково 1942 в координатную систему WGS84 и обратно

Все угловые значения передаются и возвращаются в десятичных градусах (dd.ddddd), высоты передаются и возвращаются в метрах

Const Pi As Double = 3.14159265358979 ' Число Пи
Const ro As Double = 206264.8062 ' Число угловых секунд в радиане

' Эллипсоид Красовского
Const aP As Double = 6378245 ' Большая полуось
Const alP As Double = 1 / 298.3 ' Сжатие
Const e2P As Double = 2 * alP - alP ^ 2 ' Квадрат эксцентриситета

' Эллипсоид WGS84 (GRS80, эти два эллипсоида сходны по большинству параметров)
Const aW As Double = 6378137 ' Большая полуось
Const alW As Double = 1 / 298.257223563 ' Сжатие
Const e2W As Double = 2 * alW - alW ^ 2 ' Квадрат эксцентриситета

' Вспомогательные значения для преобразования эллипсоидов
Const a As Double = (aP + aW) / 2
Const e2 As Double = (e2P + e2W) / 2
Const da As Double = aW - aP
Const de2 As Double = e2W - e2P

' Линейные элементы трансформирования, в метрах
Const dx As Double = 23.92
Const dy As Double = -141.27
Const dz As Double = -80.9
' Угловые элементы трансформирования, в секундах
Const wx As Double = 0
Const wy As Double = 0
Const wz As Double = 0
' Дифференциальное различие масштабов
Const ms As Double = 0

Function WGS84_SK42_Lat(Bd, Ld, H) As Double
WGS84_SK42_Lat = Bd - dB(Bd, Ld, H) / 3600
End Function

Function SK42_WGS84_Lat(Bd, Ld, H) As Double
SK42_WGS84_Lat = Bd + dB(Bd, Ld, H) / 3600
End Function

Function WGS84_SK42_Long(Bd, Ld, H) As Double
WGS84_SK42_Long = Ld - dL(Bd, Ld, H) / 3600
End Function

Function SK42_WGS84_Long(Bd, Ld, H) As Double
SK42_WGS84_Long = Ld + dL(Bd, Ld, H) / 3600
End Function

Function dB(Bd, Ld, H) As Double
Dim B, L, M, N As Double
B = Bd * Pi / 180
L = Ld * Pi / 180
M = a * (1 - e2) / (1 - e2 * Sin(B) ^ 2) ^ 1.5
N = a * (1 - e2 * Sin(B) ^ 2) ^ -0.5
dB = ro / (M + H) * (N / a * e2 * Sin(B) * Cos(B) * da _ + (N ^ 2 / a ^ 2 + 1) * N * Sin(B) * Cos(B) * de2 / 2 _ - (dx * Cos(L) + dy * Sin(L)) * Sin(B) + dz * Cos(B)) _ - wx * Sin(L) * (1 + e2 * Cos(2 * B)) _ + wy * Cos(L) * (1 + e2 * Cos(2 * B)) _ - ro * ms * e2 * Sin(B) * Cos(B)
End Function

Function dL(Bd, Ld, H) As Double
Dim B, L, N As Double
B = Bd * Pi / 180
L = Ld * Pi / 180
N = a * (1 - e2 * Sin(B) ^ 2) ^ -0.5
dL = ro / ((N + H) * Cos(B)) * (-dx * Sin(L) + dy * Cos(L)) _ + Tan(B) * (1 - e2) * (wx * Cos(L) + wy * Sin(L)) - wz
End Function

Function WGS84Alt(Bd, Ld, H) As Double
Dim B, L, N, dH As Double
B = Bd * Pi / 180
L = Ld * Pi / 180
N = a * (1 - e2 * Sin(B) ^ 2) ^ -0.5
dH = -a / N * da + N * Sin(B) ^ 2 * de2 / 2 _ + (dx * Cos(L) + dy * Sin(L)) * Cos(B) + dz * Sin(B) _ - N * e2 * Sin(B) * Cos(B) * (wx / ro * Sin(L) - wy / ro * Cos(L)) _ + (a ^ 2 / N + H) * ms
WGS84Alt = H + dH
End Function


}
end.

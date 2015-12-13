// http://gis-lab.info/qa/wgs84-sk42-wgs84-formula.html

unit WGS84_SK42;

interface

function WGS84_SK42_Lat(Bd, Ld, H :Double) :Double;
function SK42_WGS84_Lat(Bd, Ld, H :Double) :Double;
function WGS84_SK42_Long(Bd, Ld, H :Double) :Double;
function SK42_WGS84_Long(Bd, Ld, H :Double) :Double;
function dB(Bd, Ld, H :Double) :Double;
function dL(Bd, Ld, H :Double) :Double;
function WGS84Alt(Bd, Ld, H :Double) :Double;

implementation

uses Math;

const
   ro   :Double = 206264.8062;          // Число угловых секунд в радиане

   // Эллипсоид Красовского
   aP   :Double = 6378245;              // Большая полуось
   alP  :Double = 1 / 298.3;            // Сжатие

   // Эллипсоид WGS84 (GRS80, эти два эллипсоида сходны по большинству параметров)
   aW   :Double = 6378137;              // Большая полуось
   alW  :Double = 1 / 298.257223563;    // Сжатие

   // Линейные элементы трансформирования, в метрах
   dx   :Double = 23.92;
   dy   :Double = -141.27;
   dz   :Double = -80.9;

   // Угловые элементы трансформирования, в секундах
   wx   :Double = 0;
   wy   :Double = 0;
   wz   :Double = 0;

   // Дифференциальное различие масштабов
   ms   :Double = 0;

var
  e2P, e2W, a, e2, da, de2: Double;

procedure InitVariables;
begin
   e2P:= 2 * alP - Sqr(alP);  // Квадрат эксцентриситета (Эллипсоид Красовского)
   e2W:= 2 * alW - Sqr(alW);  // Квадрат эксцентриситета (Эллипсоид WGS84)
   
   // Вспомогательные значения для преобразования эллипсоидов
   a:= (aP + aW) / 2;
   e2:= (e2P + e2W) / 2;
   da:= aW - aP;
   de2:= e2W - e2P;
end;

function WGS84_SK42_Lat(Bd, Ld, H :Double) :Double;
begin
   Result:= Bd - dB(Bd, Ld, H) / 3600;
end;

function SK42_WGS84_Lat(Bd, Ld, H :Double) :Double;
begin
   Result:= Bd + dB(Bd, Ld, H) / 3600;
end;

function WGS84_SK42_Long(Bd, Ld, H :Double) :Double;
begin
   Result:= Ld - dL(Bd, Ld, H) / 3600;
end;

function SK42_WGS84_Long(Bd, Ld, H :Double) :Double;
begin
   Result:= Ld + dL(Bd, Ld, H) / 3600;
end;

function dB(Bd, Ld, H :Double) :Double;
var
   B, L, M, N :Double;
begin
   B:= Bd * Pi / 180;
   L:= Ld * Pi / 180;
   M:= a * (1 - e2) / Power(1 - e2 * Sqr(Sin(B)), 1.5);
   N:= a * Power(1 - e2 * Sqr(Sin(B)), -0.5);
   Result:= ro / (M + H) * (N / a * e2 * Sin(B) * Cos(B) * da +
      (Sqr(N) / Sqr(a)+l) * N * Sin(B) * Cos(B) * de2 / 2 -
      (dx * Cos(L) + dy * Sin(L)) * Sin(B) + dz * Cos(B)) -
      wx * Sin(L) * (1 + e2 * Cos(2 * B)) + wy * Cos(L) *
      (1 + e2 * Cos(2 * B)) -  ro * ms * e2 * Sin(B) * Cos(B);
end;

function dL(Bd, Ld, H :Double) :Double;
var
   B, L, N :Double;
begin
   B:= Bd * Pi / 180;
   L:= Ld * Pi / 180;
   N:= a * Power(1 - e2 * Sqr(Sin(B)), -0.5);
   Result:= ro / ((N + H) * Cos(B)) * (-dx * Sin(L) + dy * Cos(L)) +
      Tan(B) * (1 - e2) * (wx * Cos(L) + wy * Sin(L)) - wz;
end;

function WGS84Alt(Bd, Ld, H :Double) :Double;
var
   B, L, N, dH :Double;
begin
   B:= Bd * Pi / 180;
   L:= Ld * Pi / 180;
   N:= a * Power(1 - e2 * Sqr(Sin(B)), -0.5);
   dH:= -a / N * da + N * Sqr(Sin(B)) * de2 / 2 +
      (dx * Cos(L) + dy * Sin(L)) * Cos(B) + dz * Sin(B) -
      N * e2 * Sin(B) * Cos(B) * (wx / ro * Sin(L) - wy / ro * Cos(L)) +
      (Sqr(a) / N + H) * ms;
   Result:= H + dH;
end;

initialization
  InitVariables;

end.
unit u_GeoFun;

interface

uses
  SysUtils,
  Types,
  GR32,
  t_GeoTypes,
  i_CoordConverter;

type
  TResObj = record
   find:boolean;
   S:Double;
   name:String;
   descr:String;
  end;

  function CalcAngleDelta(ADerg1, ADegr2: Double): Double;

  function PolygonFromRect(ARect: TDoubleRect): TArrayOfDoublePoint;
  function DoublePoint(APoint: TPoint): TDoublePoint; overload;
  function DoublePoint(X, Y: Double): TDoublePoint; overload;
  function DoubleRect(ARect: TRect): TDoubleRect; overload;
  function DoubleRect(ATopLeft, ABottomRight: TDoublePoint): TDoubleRect; overload;
  function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect; overload;
  function RectCenter(ARect: TRect): TDoublePoint; overload;
  function RectCenter(ARect: TDoubleRect): TDoublePoint; overload;


  function PtInRgn(Polyg:TArrayOfPoint; P:TPoint):boolean; overload;
  function PtInRgn(Polyg:TArrayOfDoublePoint; P:TDoublePoint):boolean; overload;
  function PtInPolygon(const Pt: TPoint; const Points:TArrayOfPoint): Boolean;
  function LonLatPointInRect(const APoint: TDoublePoint; const ARect: TDoubleRect): Boolean;
  function PixelPointInRect(const APoint: TDoublePoint; const ARect: TDoubleRect): Boolean;
  function IsDoubleRectEmpty(const Rect: TDoubleRect): Boolean;
  function IntersecTDoubleRect(out Rect: TDoubleRect; const R1, R2: TDoubleRect): Boolean;

  function DoublePointsEqual(p1,p2:TDoublePoint):boolean;
  function PolygonSquare(Poly:TArrayOfPoint): Double; overload;
  function PolygonSquare(Poly:TArrayOfDoublePoint): Double; overload;
  function PointOnPath(APoint:TDoublePoint; APath: TArrayOfDoublePoint; ADist: Double): Boolean;

  procedure CalculateWFileParams(LL1,LL2:TDoublePoint;ImageWidth,ImageHeight:integer;AConverter: ICoordConverter;
            var CellIncrementX,CellIncrementY,OriginX,OriginY:Double);
  Procedure GetMinMax(var min,max:TPoint; Polyg:TArrayOfPoint;round_:boolean); overload;
  Procedure GetMinMax(var ARect:TRect; Polyg:TArrayOfPoint;round_:boolean); overload;
  Procedure GetMinMax(var ARect:TDoubleRect; Polyg:TArrayOfDoublePoint); overload;
  function GetDwnlNum(var min,max:TPoint; Polyg:TArrayOfPoint; getNum:boolean):Int64; overload;
  function GetDwnlNum(var ARect: TRect; Polyg:TArrayOfPoint; getNum:boolean):Int64; overload;
  function RgnAndRect(Polyg:TArrayOfPoint; ARect: TRect):boolean;
  function RgnAndRgn(Polyg:TArrayOfPoint;x,y:integer;prefalse:boolean):boolean;
  function GetGhBordersStepByScale(AScale: Integer): TDoublePoint;
  function PointIsEmpty(APoint: TDoublePoint): Boolean;

implementation

uses
  Math;

function CalcAngleDelta(ADerg1, ADegr2: Double): Double;
begin
  Result := ADerg1 - ADegr2;
  if (Result > 360) or (Result < 360) then begin
    Result := Result - Trunc(Result / 360.0) * 360.0;
  end;
  if Result > 180.0 then begin
    Result := Result - 360.0;
  end else if Result < -180.0 then begin
    Result := Result + 360.0;
  end;
end;

function RgnAndRect(Polyg:TArrayOfPoint; ARect: TRect):boolean;
var
  i: integer;
begin
  if PtInPolygon(ARect.TopLeft,polyg) then begin
    result:=true;
  end else begin
    if PtInPolygon(Point(ARect.Right, ARect.Top),polyg) then begin
      result:=true;
    end else begin
      if PtInPolygon(ARect.BottomRight,polyg) then begin
        result:=true;
      end else begin
        if PtInPolygon(Point(ARect.Left,ARect.Bottom),polyg) then begin
          result:=true;
        end else begin
          result:=false;
          for i:=0 to length(polyg)-2 do begin
            if PtInRect(ARect, polyg[i]) then begin
              result:=true;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function RgnAndRgn(Polyg:TArrayOfPoint;x,y:integer;prefalse:boolean):boolean;
var i,xm128,ym128,xp128,yp128:integer;
begin
  xm128:=x-128;
  ym128:=y-128;
  if (not prefalse)and(PtInPolygon(Point(xm128,ym128),polyg)) then begin
    result:=true;
  end else begin
    xp128:=x+128;
    if (not prefalse)and(PtInPolygon(Point(xp128,ym128),polyg)) then begin
      result:=true;
    end else begin
      yp128:=y+128;
      if PtInPolygon(Point(xp128,yp128),polyg) then begin
        result:=true;
      end else begin
        if PtInPolygon(Point(xm128,yp128),polyg) then begin
          result:=true;
        end else begin
          result:=false;
          for i:=0 to length(polyg)-2 do begin
            if (polyg[i].x<xp128)and(polyg[i].x>xm128)and(polyg[i].y<yp128)and(polyg[i].y>ym128) then begin
              result:=true;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

Procedure GetMinMax(var min,max:TPoint; Polyg:TArrayOfPoint;round_:boolean);
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

Procedure GetMinMax(var ARect: TRect; Polyg:TArrayOfPoint;round_:boolean);
var i:integer;
begin
 ARect.TopLeft:=Polyg[0];
 ARect.BottomRight:=Polyg[0];
 for i:=1 to length(Polyg)-1 do
  begin
   if ARect.Left>Polyg[i].x then ARect.Left:=Polyg[i].x;
   if ARect.Top>Polyg[i].y then ARect.Top:=Polyg[i].y;
   if ARect.Right<Polyg[i].x then ARect.Right:=Polyg[i].x;
   if ARect.Bottom<Polyg[i].y then ARect.Bottom:=Polyg[i].y;
  end;
 if round_ then
  begin
   Dec(ARect.Right);
   Dec(ARect.Bottom);
   Inc(ARect.Left);
   Inc(ARect.Top);
   ARect.Left:=ARect.Left-(ARect.Left mod 256)+128;
   ARect.Right:=ARect.Right-(ARect.Right mod 256)+128;
   ARect.Top:=ARect.Top-(ARect.Top mod 256)+128;
   ARect.Bottom:=ARect.Bottom-(ARect.Bottom mod 256)+128;
  end;
end;

function GetDwnlNum(var min,max:TPoint; Polyg:TArrayOfPoint; getNum:boolean):Int64;
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

function GetDwnlNum(var ARect: TRect; Polyg:TArrayOfPoint; getNum:boolean):Int64;
var i,j:integer;
    prefalse:boolean;
begin
 GetMinMax(ARect,polyg,true);
 result:=0;
 if getNum then
  if (length(Polyg)=5)and(Polyg[0].x=Polyg[3].x)and(Polyg[1].x=Polyg[2].x)
                      and(Polyg[0].y=Polyg[1].y)and(Polyg[2].y=Polyg[3].y)
    then begin
          result:=int64((ARect.Right-ARect.Left) div 256+1)*int64((ARect.Bottom-ARect.Top) div 256+1);
         end
    else begin
          i:=ARect.Left;
          while i<=ARect.Right do
          begin
           j:=ARect.Top;
           prefalse:=false;
           while j<=ARect.Bottom do
            begin
             prefalse:=not(RgnAndRgn(Polyg,i,j,prefalse));
             if not(prefalse) then inc(result);
             inc(j,256);
            end;
           inc(i,256);
          end;
         end;
 Inc(ARect.Right);
 Inc(ARect.Bottom);
end;

procedure CalculateWFileParams(
  LL1, LL2: TDoublePoint;
  ImageWidth, ImageHeight: integer;
  AConverter: ICoordConverter;
  var CellIncrementX, CellIncrementY, OriginX, OriginY: Double
);
var
  VM1: TDoublePoint;
  VM2: TDoublePoint;
begin
  case AConverter.GetCellSizeUnits of
    CELL_UNITS_METERS: begin
      VM1 := AConverter.LonLat2Metr(LL1);
      VM2 := AConverter.LonLat2Metr(LL2);

      OriginX := VM1.X;
      OriginY := VM1.Y;

      CellIncrementX := (VM2.X-VM1.X)/ImageWidth;
      CellIncrementY := (VM2.Y-VM1.Y)/ImageHeight;
    end;
    CELL_UNITS_DEGREES: begin
      OriginX:=ll1.x;
      OriginY:=ll1.y;
      CellIncrementX:=(ll2.x-ll1.x)/ImageWidth;
      CellIncrementY:=-CellIncrementX;
    end;
  end;
end;

function PointOnPath(APoint:TDoublePoint; APath: TArrayOfDoublePoint; ADist: Double): Boolean;
var
  i: Integer;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VPoinsCount: Integer;
  VCurrEmpty: Boolean;
  VPrevEmpty: Boolean;
  VVectorW: TDoublePoint;
  VVectorV: TDoublePoint;
  C1: Double;
  C2: Double;
  B: Double;
  VVectorDist: TDoublePoint;
  VDistSQR: Double;
begin
  Result := False;
  VPoinsCount := Length(APath);
  if VPoinsCount > 1 then begin
    VDistSQR := ADist * ADist;
    VCurrPoint := APath[0];
    VCurrEmpty := PointIsEmpty(VPrevPoint);
    for i := 1 to VPoinsCount - 1 do begin
      VPrevPoint := VCurrPoint;
      VPrevEmpty := VCurrEmpty;
      VCurrPoint := APath[i];
      VCurrEmpty := PointIsEmpty(VCurrPoint);
      if not(VPrevEmpty or VCurrEmpty) then begin
        VVectorW.X := APoint.X - VPrevPoint.X;
        VVectorW.Y := APoint.Y - VPrevPoint.Y;
        VVectorV.X := VCurrPoint.X - VPrevPoint.X;
        VVectorV.Y := VCurrPoint.Y - VPrevPoint.Y;
        C1 := VVectorW.X * VVectorV.X + VVectorW.Y * VVectorV.Y;
        if C1 > 0 then begin
          C2 := VVectorV.X * VVectorV.X + VVectorV.Y * VVectorV.Y;
          if C2 > C1 then begin
            B := C1 / C2;
            VVectorDist.X := VVectorW.X - B * VVectorV.X;
            VVectorDist.Y := VVectorW.Y - B * VVectorV.Y;
            if (VVectorDist.X * VVectorDist.X + VVectorDist.Y * VVectorDist.Y) < VDistSQR then begin
              Result := True;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;


function PolygonSquare(Poly:TArrayOfPoint): Double;
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

function PolygonSquare(Poly:TArrayOfDoublePoint): Double; overload;
var
  I, J, HP: Integer;
begin
  Result := 0;
  HP := High(Poly);
  for I := Low(Poly) to HP do begin
    if I = HP then
      J := 0
    else
      J := I + 1;
    Result := Result + (Poly[I].X + Poly[J].X) * (Poly[I].Y - Poly[J].Y);
  end;
  Result := Abs(Result) / 2;
end;

function PtInPolygon(const Pt: TPoint; const Points:TArrayOfPoint): Boolean;
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

function PtInRgn(Polyg:TArrayOfPoint;P:TPoint):boolean;
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

function PtInRgn(Polyg:TArrayOfDoublePoint; P:TDoublePoint):boolean; overload;
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

function DoublePointsEqual(p1,p2:TDoublePoint):boolean;
begin
 if (p1.x=p2.X)and(p1.y=p2.y) then result:=true
                              else result:=false;
end;

function PolygonFromRect(ARect: TDoubleRect): TArrayOfDoublePoint;
begin
  SetLength(Result, 5);
  Result[0] := ARect.TopLeft;
  Result[1] := DoublePoint(ARect.Right, ARect.Top);
  Result[2] := ARect.BottomRight;
  Result[3] := DoublePoint(ARect.Left, ARect.Bottom);
  Result[4] := ARect.TopLeft;
end;

Procedure GetMinMax(var ARect:TDoubleRect; Polyg:TArrayOfDoublePoint); overload;
var
  i: Integer;
begin
  if Length(Polyg) > 0 then begin
    ARect.TopLeft := Polyg[0];
    ARect.BottomRight := Polyg[0];
    for i := 1 to Length(Polyg) - 1 do begin
      if ARect.Left > Polyg[i].X then begin
        ARect.Left := Polyg[i].X
      end;
      if ARect.Top < Polyg[i].Y then begin
        ARect.Top := Polyg[i].Y
      end;
      if ARect.Right < Polyg[i].X then begin
        ARect.Right := Polyg[i].X
      end;
      if ARect.Bottom > Polyg[i].Y then begin
        ARect.Bottom := Polyg[i].Y
      end;
    end;
  end else begin
    ARect.TopLeft := DoublePoint(0, 0);
    ARect.BottomRight := DoublePoint(0, 0);
  end;
end;

function GetGhBordersStepByScale(AScale: Integer): TDoublePoint;
begin
  case AScale of
    1000000: begin Result.X:=6; Result.Y:=4; end;
     500000: begin Result.X:=3; Result.Y:=2; end;
     200000: begin Result.X:=1; Result.Y:=0.66666666666666666666666666666667; end;
     100000: begin Result.X:=0.5; Result.Y:=0.33333333333333333333333333333333; end;
      50000: begin Result.X:=0.25; Result.Y:=0.1666666666666666666666666666665; end;
      25000: begin Result.X:=0.125; Result.Y:=0.08333333333333333333333333333325; end;
      10000: begin Result.X:=0.0625; Result.Y:=0.041666666666666666666666666666625; end;
    else begin Result.X:=360; Result.Y:=180; end;
  end;
end;

function LonLatPointInRect(const APoint: TDoublePoint; const ARect: TDoubleRect): Boolean;
begin
  result:=(APoint.X<=ARect.Right)and(APoint.X>=ARect.Left)and
          (APoint.Y<=ARect.Top)and(APoint.Y>=ARect.Bottom);
end;

function PixelPointInRect(const APoint: TDoublePoint; const ARect: TDoubleRect): Boolean;
begin
  result:=(APoint.X<=ARect.Right)and(APoint.X>=ARect.Left)and
          (APoint.Y>=ARect.Top)and(APoint.Y<=ARect.Bottom);
end;

function DoublePoint(APoint: TPoint): TDoublePoint; overload;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

function DoublePoint(X, Y: Double): TDoublePoint; overload;
begin
  Result.X := X;
  Result.Y := Y;
end;

function DoubleRect(ARect: TRect): TDoubleRect; overload;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

function DoubleRect(ATopLeft, ABottomRight: TDoublePoint): TDoubleRect; overload;
begin
  Result.TopLeft := ATopLeft;
  Result.BottomRight := ABottomRight;
end;

function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect; overload;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function RectCenter(ARect: TRect): TDoublePoint; overload;
begin
  Result.X := (ARect.Left + ARect.Right) / 2;
  Result.Y := (ARect.Top + ARect.Bottom) / 2;
end;

function RectCenter(ARect: TDoubleRect): TDoublePoint; overload;
begin
  Result.X := (ARect.Left + ARect.Right) / 2;
  Result.Y := (ARect.Top + ARect.Bottom) / 2;
end;

function IsDoubleRectEmpty(const Rect: TDoubleRect): Boolean;
begin
  Result := (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top);
end;

function IntersecTDoubleRect(out Rect: TDoubleRect; const R1, R2: TDoubleRect): Boolean;
begin
  Rect := R1;
  if R2.Left > R1.Left then Rect.Left := R2.Left;
  if R2.Top > R1.Top then Rect.Top := R2.Top;
  if R2.Right < R1.Right then Rect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then Rect.Bottom := R2.Bottom;
  Result := not IsDoubleRectEmpty(Rect);
  if not Result then FillChar(Rect, SizeOf(Rect), 0);
end;

function PointIsEmpty(APoint: TDoublePoint): Boolean;
begin
  Result := IsNan(APoint.X) or IsNan(APoint.Y);
end;

end.




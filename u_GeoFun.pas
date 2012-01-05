{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GeoFun;

interface

uses
  Types,
  Math,
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


  function PtInRgn(APoints: PPointArray; ACount: Integer; APoint: TPoint):boolean; overload;
  function PtInRgn(APoints: PDoublePointArray; ACount: Integer; APoint: TDoublePoint):boolean; overload;
  function PtInPolygon(const Pt: TPoint; const APoints: PPointArray; ACount: Integer): Boolean;
  function LonLatPointInRect(const APoint: TDoublePoint; const ARect: TDoubleRect): Boolean;
  function PixelPointInRect(const APoint: TDoublePoint; const ARect: TDoubleRect): Boolean;
  function IsDoubleRectEmpty(const Rect: TDoubleRect): Boolean;
  function IntersecTDoubleRect(out Rect: TDoubleRect; const R1, R2: TDoubleRect): Boolean;
  function ConveryPolyline2Polygon(APoints: PDoublePointArray; ACount: Integer; ARadius: Double; Aconverter: ICoordConverter; AZoom: byte): TArrayOfDoublePoint;

  function DoublePointsEqual(p1,p2: TDoublePoint): Boolean;
  function DoubleRectsEqual(ARect1, ARect2: TDoubleRect): Boolean;
  function PolygonSquare(Poly:TArrayOfPoint): Double; overload;
  function PolygonSquare(APoints: PDoublePointArray; ACount: Integer): Double; overload;
  function PointOnPath(APoint:TDoublePoint; APoints: PDoublePointArray; ACount: Integer; ADist: Double): Boolean;

  procedure CalculateWFileParams(LL1,LL2:TDoublePoint;ImageWidth,ImageHeight:integer;AConverter: ICoordConverter;
            var CellIncrementX,CellIncrementY,OriginX,OriginY:Double);
  Procedure GetMinMax(var min,max:TPoint; APoints: PPointArray; ACount: Integer;round_:boolean); overload;
  Procedure GetMinMax(var ARect:TRect; APoints: PPointArray; ACount: Integer;round_:boolean); overload;
  Procedure GetMinMax(var ARect:TDoubleRect; APoints: PDoublePointArray; ACount: Integer); overload;
  function GetDwnlNum(var min,max:TPoint; APoints: PPointArray; ACount: Integer; getNum:boolean):Int64; overload;
  function GetDwnlNum(var ARect: TRect; APoints: PPointArray; ACount: Integer; getNum:boolean):Int64; overload;
  function RgnAndRect(APoints: PPointArray; ACount: Integer; ARect: TRect):boolean;
  function RgnAndRgn(APoints: PPointArray; ACount: Integer; x, y: integer; prefalse: boolean):boolean; // Переделать использующий ее код в ближайшее время
  function GetGhBordersStepByScale(AScale: Integer): TDoublePoint;
  function GetDegBordersStepByScale(AScale: Integer): TDoublePoint;
  function PointIsEmpty(APoint: TDoublePoint): Boolean;

const
  CEmptyDoublePoint: TDoublePoint = (X: NAN; Y: NAN);
  
implementation

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

function RgnAndRect(APoints: PPointArray; ACount: Integer; ARect: TRect):boolean;
var
  i: integer;
begin
  if PtInPolygon(ARect.TopLeft, APoints, ACount) then begin
    result:=true;
  end else begin
    if PtInPolygon(Point(ARect.Right, ARect.Top), APoints, ACount) then begin
      result:=true;
    end else begin
      if PtInPolygon(ARect.BottomRight, APoints, ACount) then begin
        result:=true;
      end else begin
        if PtInPolygon(Point(ARect.Left,ARect.Bottom), APoints, ACount) then begin
          result:=true;
        end else begin
          result:=false;
          for i:=0 to ACount-2 do begin
            if PtInRect(ARect, APoints[i]) then begin
              result:=true;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function RgnAndRgn(APoints: PPointArray; ACount: Integer;x,y:integer;prefalse:boolean):boolean;
var i,xm128,ym128,xp128,yp128:integer;
begin
  xm128:=x-128;
  ym128:=y-128;
  if (not prefalse)and(PtInPolygon(Point(xm128,ym128), APoints, ACount)) then begin
    result:=true;
  end else begin
    xp128:=x+128;
    if (not prefalse)and(PtInPolygon(Point(xp128,ym128), APoints, ACount)) then begin
      result:=true;
    end else begin
      yp128:=y+128;
      if PtInPolygon(Point(xp128,yp128), APoints, ACount) then begin
        result:=true;
      end else begin
        if PtInPolygon(Point(xm128,yp128), APoints, ACount) then begin
          result:=true;
        end else begin
          result:=false;
          for i:=0 to ACount-2 do begin
            if (APoints[i].x<xp128)and(APoints[i].x>xm128)and(APoints[i].y<yp128)and(APoints[i].y>ym128) then begin
              result:=true;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

Procedure GetMinMax(var min,max:TPoint; APoints: PPointArray; ACount: Integer;round_:boolean);
var i:integer;
begin
 max:=APoints[0];
 min:=APoints[0];
 for i:=1 to ACount - 1 do
  begin
   if min.x>APoints[i].x then min.x:=APoints[i].x;
   if min.y>APoints[i].y then min.y:=APoints[i].y;
   if max.x<APoints[i].x then max.x:=APoints[i].x;
   if max.y<APoints[i].y then max.y:=APoints[i].y;
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

Procedure GetMinMax(var ARect: TRect; APoints: PPointArray; ACount: Integer; round_:boolean);
var i:integer;
begin
 ARect.TopLeft:=APoints[0];
 ARect.BottomRight:=APoints[0];
 for i:=1 to ACount - 1 do
  begin
   if ARect.Left>APoints[i].x then ARect.Left:=APoints[i].x;
   if ARect.Top>APoints[i].y then ARect.Top:=APoints[i].y;
   if ARect.Right<APoints[i].x then ARect.Right:=APoints[i].x;
   if ARect.Bottom<APoints[i].y then ARect.Bottom:=APoints[i].y;
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

function GetDwnlNum(var min,max:TPoint; APoints: PPointArray; ACount: Integer; getNum:boolean):Int64;
type
  P5PointArray = ^T5PointArray;
  T5PointArray = array [0..4] of TPoint;
var
  i,j:integer;
  prefalse:boolean;
begin
  GetMinMax(min,max, APoints, ACount,true);
  result:=0;
  if getNum then begin
    if (ACount = 5)and
      (P5PointArray(APoints)[0].x = P5PointArray(APoints)[3].x)and
      (P5PointArray(APoints)[1].x = P5PointArray(APoints)[2].x)and
      (P5PointArray(APoints)[0].y = P5PointArray(APoints)[1].y)and
      (P5PointArray(APoints)[2].y = P5PointArray(APoints)[3].y)
    then begin
      result:=int64((max.X-min.X) div 256+1)*int64((max.Y-min.Y) div 256+1);
    end else begin
      i:=min.X;
      while i<=max.x do
      begin
       j:=min.y;
       prefalse:=false;
       while j<=max.y do
        begin
         prefalse:=not(RgnAndRgn(APoints, ACount,i,j,prefalse));
         if not(prefalse) then inc(result);
         inc(j,256);
        end;
       inc(i,256);
      end;
    end;
  end;
  max.X:=max.X+1;
  max.Y:=max.Y+1;
end;

function GetDwnlNum(var ARect: TRect; APoints: PPointArray; ACount: Integer; getNum:boolean):Int64;
type
  P5PointArray = ^T5PointArray;
  T5PointArray = array [0..4] of TPoint;
var
  i,j:integer;
  prefalse:boolean;
begin
 GetMinMax(ARect,APoints, ACount,true);
 result:=0;
 if getNum then
    if (ACount = 5)and
      (P5PointArray(APoints)[0].x = P5PointArray(APoints)[3].x)and
      (P5PointArray(APoints)[1].x = P5PointArray(APoints)[2].x)and
      (P5PointArray(APoints)[0].y = P5PointArray(APoints)[1].y)and
      (P5PointArray(APoints)[2].y = P5PointArray(APoints)[3].y)
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
             prefalse:=not(RgnAndRgn(APoints, ACount, i,j,prefalse));
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

function PointOnPath(APoint:TDoublePoint; APoints: PDoublePointArray; ACount: Integer; ADist: Double): Boolean;
var
  i: Integer;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
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
  if ACount > 1 then begin
    VDistSQR := ADist * ADist;
    VCurrPoint := APoints[0];
    VCurrEmpty := PointIsEmpty(VPrevPoint);
    for i := 1 to ACount - 1 do begin
      VPrevPoint := VCurrPoint;
      VPrevEmpty := VCurrEmpty;
      VCurrPoint := APoints[i];
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

function ConveryPolyline2Polygon(
  APoints: PDoublePointArray;
  ACount: Integer;
  ARadius: Double;
  Aconverter: ICoordConverter;
  AZoom: byte
): TArrayOfDoublePoint;
var
  i: Integer;
  VCurrLonLat: TDoublePoint;
  VPrevLonLat: TDoublePoint;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VResPoinsCount: Integer;
  s, c: Extended;
  VRadius:double;
  VLonLatMul: Double;
  VCurrVectorAngle: Double;
  VPrevVectorAngle: Double;
  a3,Angle: Double;
  VResultPixelPos: TDoublePoint;
  VVector: TDoublePoint;
  VSinA3: Double;
begin
  if ACount > 1 then begin
    VResPoinsCount:=ACount*2+1;
    SetLength(Result,VResPoinsCount);
    VPrevVectorAngle := 0;
    VCurrVectorAngle := 0;
    VLonLatMul := 0;
    VPrevLonLat := APoints[0];
    VPrevPoint := AConverter.LonLat2PixelPosFloat(VPrevLonLat, Azoom);
    for i := 1 to ACount - 1 do begin
      VCurrLonLat := APoints[i];
      VCurrPoint := AConverter.LonLat2PixelPosFloat(VCurrLonLat, Azoom);
      VVector.X := VCurrPoint.X - VPrevPoint.X;
      VVector.Y := VCurrPoint.Y - VPrevPoint.Y;

      VLonLatMul:=ARadius/AConverter.Datum.CalcDist(VPrevLonLat, VCurrLonLat);
      VLonLatMul:=VLonLatMul*sqrt(sqr(VVector.X)+sqr(VVector.Y));

      VCurrVectorAngle := Math.Arctan2(VVector.Y, VVector.X);
      if VCurrVectorAngle < 0 then begin
        VCurrVectorAngle := 2*pi+VCurrVectorAngle;
      end;

      if i=1 then begin
        Angle:=VCurrVectorAngle;
        VRadius := VLonLatMul/sin(pi/4);
        SinCos(pi/2+pi/4+Angle, s, c);
        VResultPixelPos:=DoublePoint(VPrevPoint.x + VRadius * c, VPrevPoint.y + VRadius * s);
        AConverter.CheckPixelPosFloat(VResultPixelPos,AZoom,false);
        Result[0]:=AConverter.PixelPosFloat2LonLat(VResultPixelPos,Azoom);
        SinCos(pi/2-pi/4+Angle+pi, s, c);
        VResultPixelPos:=DoublePoint(VPrevPoint.x + VRadius * c, VPrevPoint.y + VRadius * s);
        AConverter.CheckPixelPosFloat(VResultPixelPos,AZoom,false);
        Result[VResPoinsCount-2]:=AConverter.PixelPosFloat2LonLat(VResultPixelPos,Azoom);
        Result[VResPoinsCount-1]:=Result[0];
      end else begin
        Angle:=(VCurrVectorAngle+VPrevVectorAngle)/2;
        if abs(VPrevVectorAngle-VCurrVectorAngle)>Pi then begin
          Angle:=Angle-Pi;
        end;
        a3:=abs((pi/2+Angle)-VCurrVectorAngle);
        if a3>Pi then begin
          a3:=a3-Pi;
        end;
        VSinA3 := sin(a3);
        if VSinA3 < 1.0/7 then begin
          VRadius:=VLonLatMul * 7;
        end else begin
          VRadius := VLonLatMul / VSinA3;
        end;

        SinCos(pi/2+Angle, s, c);
        VResultPixelPos:=DoublePoint(VPrevPoint.x + VRadius * c, VPrevPoint.y + VRadius * s);
        AConverter.CheckPixelPosFloat(VResultPixelPos,AZoom,false);
        Result[i-1]:=AConverter.PixelPosFloat2LonLat(VResultPixelPos,Azoom);
        SinCos(pi/2+Angle+pi, s, c);
        VResultPixelPos:=DoublePoint(VPrevPoint.x + VRadius * c, VPrevPoint.y + VRadius * s);
        AConverter.CheckPixelPosFloat(VResultPixelPos,AZoom,false);
        Result[VResPoinsCount-2-(i-1)]:=AConverter.PixelPosFloat2LonLat(VResultPixelPos,Azoom);
      end;

      VPrevLonLat := VCurrLonLat;
      VPrevPoint := VCurrPoint;
      VPrevVectorAngle := VCurrVectorAngle;
    end;

    Angle:=VCurrVectorAngle;
    VRadius := VLonLatMul/sin(pi/4);
    SinCos(pi/4+Angle, s, c);
    VResultPixelPos:=DoublePoint(VCurrPoint.x + VRadius * c, VCurrPoint.y + VRadius * s);
    AConverter.CheckPixelPosFloat(VResultPixelPos, AZoom, false);
    Result[ACount - 1] := AConverter.PixelPosFloat2LonLat(VResultPixelPos, Azoom);
    SinCos(pi/2+pi/4+Angle+pi, s, c);
    VResultPixelPos:=DoublePoint(VCurrPoint.x + VRadius * c, VCurrPoint.y + VRadius * s);
    AConverter.CheckPixelPosFloat(VResultPixelPos,AZoom,false);
    Result[ACount]:=AConverter.PixelPosFloat2LonLat(VResultPixelPos,Azoom);
  end else begin
    if ACount > 0 then begin
      SetLength(Result, ACount);
      Result[0] := APoints[0];
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

function PolygonSquare(APoints: PDoublePointArray; ACount: Integer): Double; overload;
var
  I, J, HP: Integer;
begin
  Result := 0;
  HP := ACount - 1;
  for I := 0 to HP do begin
    if I = HP then
      J := 0
    else
      J := I + 1;
    Result := Result + (APoints[I].X + APoints[J].X) * (APoints[I].Y - APoints[J].Y);
  end;
  Result := Abs(Result) / 2;
end;

function PtInPolygon(const Pt: TPoint; const APoints: PPointArray; ACount: Integer): Boolean;
var I:Integer;
    iPt,jPt:PPoint;
begin
  Result:=False;
  iPt:=@APoints[0];
  jPt:=@APoints[ACount - 1];
  for I:=0 to ACount - 1 do
  begin
   Result:=Result xor (((Pt.Y>=iPt.Y)xor(Pt.Y>=jPt.Y))and
           (Pt.X-iPt.X<((jPt.X-iPt.X)*(Pt.Y-iPt.Y)/(jPt.Y-iPt.Y))));
   jPt:=iPt;
   Inc(iPt);
  end;
end;

function PtInRgn(APoints: PPointArray; ACount: Integer; APoint: TPoint):boolean;
var i,j:integer;
begin
  result:=false;
  j:=ACount - 1;
  if ((((APoints[0].y<=APoint.y)and(APoint.y<APoints[j].y))or((APoints[j].y<=APoint.y)and(APoint.y<APoints[0].y)))and
     (APoint.x>(APoints[j].x-APoints[0].x)*(APoint.y-APoints[0].y)/(APoints[j].y-APoints[0].y)+APoints[0].x))
     then result:=not(result);
  for i:=1 to ACount - 1 do
   begin
    j:=i-1;
    if ((((APoints[i].y<=APoint.y)and(APoint.y<APoints[j].y))or((APoints[j].y<=APoint.y)and(APoint.y<APoints[i].y)))and
       (APoint.x>(APoints[j].x-APoints[i].x)*(APoint.y-APoints[i].y)/(APoints[j].y-APoints[i].y)+APoints[i].x))
       then result:=not(result);
   end;
end;

function PtInRgn(APoints: PDoublePointArray; ACount: Integer; APoint: TDoublePoint):boolean; overload;
var i,j:integer;
begin
  result:=false;
  j:=ACount - 1;
  if
    (((APoints[0].y<=APoint.y)and(APoint.y<APoints[j].y))or
    ((APoints[j].y<=APoint.y)and(APoint.y<APoints[0].y)))and
    (APoint.x>(APoints[j].x-APoints[0].x)*(APoint.y-APoints[0].y)/(APoints[j].y-APoints[0].y)+APoints[0].x)
  then begin
    result:=not(result);
  end;
  for i:=1 to ACount - 1 do begin
    j:=i-1;
    if
      (((APoints[i].y<=APoint.y)and(APoint.y<APoints[j].y))or
      ((APoints[j].y<=APoint.y)and(APoint.y<APoints[i].y)))and
      (APoint.x>(APoints[j].x-APoints[i].x)*(APoint.y-APoints[i].y)/(APoints[j].y-APoints[i].y)+APoints[i].x)
    then begin
      result:=not(result);
    end;
   end;
end;

function DoublePointsEqual(p1,p2:TDoublePoint):boolean;
var
  VP1Empty: Boolean;
  VP2Empty: Boolean;
begin
  VP1Empty := IsNan(p1.x) or IsNan(p1.Y);
  VP2Empty := IsNan(p2.x) or IsNan(p2.Y);
  if VP1Empty and VP2Empty then begin
    Result := True;
  end else begin
    if not VP1Empty and not VP2Empty then begin
      Result := (p1.x=p2.X)and(p1.y=p2.y);
    end else begin
      Result := False;
    end;
  end;
end;

function DoubleRectsEqual(ARect1, ARect2: TDoubleRect): Boolean;
begin
  Result :=
    (ARect1.Left = ARect2.Left) and
    (ARect1.Top = ARect2.Top) and
    (ARect1.Right = ARect2.Right) and
    (ARect1.Bottom = ARect2.Bottom);
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

Procedure GetMinMax(var ARect:TDoubleRect; APoints: PDoublePointArray; ACount: Integer); overload;
var
  i: Integer;
begin
  if ACount > 0 then begin
    ARect.TopLeft := APoints[0];
    ARect.BottomRight := APoints[0];
    for i := 1 to ACount - 1 do begin
      if ARect.Left > APoints[i].X then begin
        ARect.Left := APoints[i].X
      end;
      if ARect.Top < APoints[i].Y then begin
        ARect.Top := APoints[i].Y
      end;
      if ARect.Right < APoints[i].X then begin
        ARect.Right := APoints[i].X
      end;
      if ARect.Bottom > APoints[i].Y then begin
        ARect.Bottom := APoints[i].Y
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

function GetDegBordersStepByScale(AScale: Integer): TDoublePoint;
begin
  case AScale of
    1000000: begin Result.X:=10; Result.Y:=10; end;
     500000: begin Result.X:=5; Result.Y:=5; end;
     200000: begin Result.X:=2; Result.Y:=2; end;
     100000: begin Result.X:=1; Result.Y:=1; end;
      50000: begin Result.X:=0.5; Result.Y:=0.5; end;
      25000: begin Result.X:=0.25; Result.Y:=0.25; end;
      10000: begin Result.X:=0.125; Result.Y:=0.125; end;
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




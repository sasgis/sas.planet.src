{******************************************************************************}
{* SAS.Planet (SAS.ѕланета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_ProjectedCalc;

interface

uses
  t_GeoTypes,
  i_GeometryProjected,
  i_ProjectedCalc,
  u_BaseInterfacedObject;

type
  TProjectedCalc = class(TBaseInterfacedObject, IProjectedCalc)
  private
    function IsPointOnLine(
      const ALine: IGeometryProjectedLine;
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsPointOnMultiLine(
      const ALine: IGeometryProjectedMultiLine;
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectLine(
      const ALine: IGeometryProjectedLine;
      const ARect: TDoubleRect
    ): Boolean;
    function IsRectIntersectMultiLine(
      const ALine: IGeometryProjectedMultiLine;
      const ARect: TDoubleRect
    ): Boolean;
    function CalcLineLength(
      const ALine: IGeometryProjectedLine
    ): Double;
    function CalcMultiLineLength(
      const ALine: IGeometryProjectedMultiLine
    ): Double;
    function IsPointInPolygon(
      const APoly: IGeometryProjectedPolygon;
      const APoint: TDoublePoint
    ): Boolean;
    function IsPointInMultiPolygon(
      const APoly: IGeometryProjectedMultiPolygon;
      const APoint: TDoublePoint
    ): Boolean;
    function IsPointOnPolygonBorder(
      const APoly: IGeometryProjectedPolygon;
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsPointOnMultiPolygonBorder(
      const APoly: IGeometryProjectedMultiPolygon;
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsRectIntersectPolygon(
      const APoly: IGeometryProjectedPolygon;
      const ARect: TDoubleRect
    ): Boolean;
    function IsRectIntersectMultiPolygon(
      const APoly: IGeometryProjectedMultiPolygon;
      const ARect: TDoubleRect
    ): Boolean;
    function IsRectIntersectPolygonBorder(
      const APoly: IGeometryProjectedPolygon;
      const ARect: TDoubleRect
    ): Boolean;
    function IsRectIntersectMultiPolygonBorder(
      const APoly: IGeometryProjectedMultiPolygon;
      const ARect: TDoubleRect
    ): Boolean;
    function CalcPolygonArea(
      const APoly: IGeometryProjectedPolygon
    ): Double;
    function CalcMultiPolygonArea(
      const APoly: IGeometryProjectedMultiPolygon
    ): Double;
    function CalcPolygonPerimeter(
      const APoly: IGeometryProjectedPolygon
    ): Double;
    function CalcMultiPolygonPerimeter(
      const APoly: IGeometryProjectedMultiPolygon
    ): Double;
  public
    constructor Create;
  end;

implementation

uses
  i_EnumDoublePoint,
  u_GeoFunc;
  
{ TProjectedCalc }

constructor TProjectedCalc.Create;
begin
  inherited Create;
end;

function TProjectedCalc.CalcLineLength(
  const ALine: IGeometryProjectedLine
): Double;
var
  VEnum: IEnumProjectedPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  Result := 0;
  VEnum := ALine.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      Result := Result + Sqrt(Sqr(VPrevPoint.X - VCurrPoint.X) + Sqr(VPrevPoint.Y - VCurrPoint.Y));
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

function TProjectedCalc.CalcMultiLineLength(
  const ALine: IGeometryProjectedMultiLine
): Double;
var
  i: Integer;
  VLine: IGeometryProjectedLine;
begin
  Result := 0;
  for i := 0 to ALine.Count - 1 do begin
    VLine := ALine.Item[i];
    Result := Result + CalcLineLength(VLine);
  end;
end;

function TProjectedCalc.CalcMultiPolygonArea(
  const APoly: IGeometryProjectedMultiPolygon
): Double;
var
  i: Integer;
  VLine: IGeometryProjectedPolygon;
begin
  Result := 0;
  for i := 0 to APoly.Count - 1 do begin
    VLine := APoly.Item[i];
    Result := Result + CalcPolygonArea(VLine);
  end;
end;

function TProjectedCalc.CalcMultiPolygonPerimeter(
  const APoly: IGeometryProjectedMultiPolygon
): Double;
var
  i: Integer;
  VLine: IGeometryProjectedPolygon;
begin
  Result := 0;
  for i := 0 to APoly.Count - 1 do begin
    VLine := APoly.Item[i];
    Result := Result + CalcPolygonPerimeter(VLine);
  end;
end;

function TProjectedCalc.CalcPolygonArea(
  const APoly: IGeometryProjectedPolygon
): Double;
var
  VEnum: IEnumProjectedPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  Result := 0;
  VEnum := APoly.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      Result := Result + (VPrevPoint.X + VCurrPoint.X) * (VPrevPoint.Y - VCurrPoint.Y);
      VPrevPoint := VCurrPoint;
    end;
    Result := Abs(Result) / 2;
  end;
end;

function TProjectedCalc.CalcPolygonPerimeter(
  const APoly: IGeometryProjectedPolygon
): Double;
var
  VEnum: IEnumProjectedPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  Result := 0;
  VEnum := APoly.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      Result := Result + Sqrt(Sqr(VPrevPoint.X - VCurrPoint.X) + Sqr(VPrevPoint.Y - VCurrPoint.Y));
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

function TProjectedCalc.IsPointInMultiPolygon(
  const APoly: IGeometryProjectedMultiPolygon;
  const APoint: TDoublePoint
): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedPolygon;
begin
  Result := False;
  for i := 0 to APoly.Count - 1 do begin
    VLine := APoly.Item[i];
    if IsPointInPolygon(VLine, APoint) then begin
      Result := True;
      Break;
    end;
  end;
end;

function TProjectedCalc.IsPointInPolygon(
  const APoly: IGeometryProjectedPolygon;
  const APoint: TDoublePoint
): Boolean;
var
  VEnum: IEnumDoublePoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  result := false;
  VEnum := APoly.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      if (((VCurrPoint.y <= APoint.y) and (APoint.y < VPrevPoint.y)) or
        ((VPrevPoint.y <= APoint.y) and (APoint.y < VCurrPoint.y))) and
        (APoint.x > (VPrevPoint.x - VCurrPoint.x) * (APoint.y - VCurrPoint.y) / (VPrevPoint.y - VCurrPoint.y) + VCurrPoint.x) then begin
        result := not (result);
      end;
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

function TProjectedCalc.IsPointOnLine(
  const ALine: IGeometryProjectedLine;
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VVectorW: TDoublePoint;
  VVectorV: TDoublePoint;
  C1: Double;
  C2: Double;
  B: Double;
  VVectorDist: TDoublePoint;
  VDistSQR: Double;
  VEnum: IEnumProjectedPoint;
begin
  Result := False;
  VEnum := ALine.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    VDistSQR := ADist * ADist;
    while VEnum.Next(VCurrPoint) do begin
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
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

function TProjectedCalc.IsPointOnMultiLine(
  const ALine: IGeometryProjectedMultiLine;
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedLine;
begin
  Result := False;
  for i := 0 to ALine.Count - 1 do begin
    VLine := ALine.Item[i];
    if IsPointOnLine(VLine, APoint, ADist) then begin
      Result := True;
      Break;
    end;
  end;
end;

function TProjectedCalc.IsPointOnMultiPolygonBorder(
  const APoly: IGeometryProjectedMultiPolygon;
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedPolygon;
begin
  Result := False;
  for i := 0 to APoly.Count - 1 do begin
    VLine := APoly.Item[i];
    if IsPointOnPolygonBorder(VLine, APoint, ADist) then begin
      Result := True;
      Break;
    end;
  end;
end;

function TProjectedCalc.IsPointOnPolygonBorder(
  const APoly: IGeometryProjectedPolygon;
  const APoint: TDoublePoint;
  const ADist: Double
): Boolean;
var
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VVectorW: TDoublePoint;
  VVectorV: TDoublePoint;
  C1: Double;
  C2: Double;
  B: Double;
  VVectorDist: TDoublePoint;
  VDistSQR: Double;
  VEnum: IEnumProjectedPoint;
begin
  Result := False;
  VEnum := APoly.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    VDistSQR := ADist * ADist;
    while VEnum.Next(VCurrPoint) do begin
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
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

function TProjectedCalc.IsRectIntersectLine(
  const ALine: IGeometryProjectedLine;
  const ARect: TDoubleRect
): Boolean;
var
  VEnum: IEnumProjectedPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
  VIntersect: Double;
  VDelta: TDoublePoint;
begin
  Result := False;
  if IsIntersecProjectedRect(ALine.Bounds, ARect) then begin
    VEnum := ALine.GetEnum;
    // »щем есть ли пересечени€ пр€моугольника с линией
    if VEnum.Next(VPrevPoint) then begin
      while VEnum.Next(VCurrPoint) do begin
        VDelta.X := VCurrPoint.X - VPrevPoint.X;
        VDelta.Y := VCurrPoint.Y - VPrevPoint.Y;
        if (VDelta.Y < 0) then begin
          if (VCurrPoint.Y <= ARect.Top) and (ARect.Top < VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.Y <= ARect.Bottom) and (ARect.Bottom < VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
        end else if (VDelta.Y > 0) then begin
          if (VCurrPoint.Y > ARect.Top) and (ARect.Top >= VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.Y > ARect.Bottom) and (ARect.Bottom >= VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
        end;

        if (VDelta.X < 0) then begin
          if (VCurrPoint.X <= ARect.Left) and (ARect.Left < VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.X <= ARect.Right) and (ARect.Right < VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
        end else if (VDelta.X > 0) then begin
          if (VCurrPoint.X > ARect.Left) and (ARect.Left >= VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.X > ARect.Right) and (ARect.Right >= VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
        end;
        VPrevPoint := VCurrPoint;
      end;
    end;
  end;
end;

function TProjectedCalc.IsRectIntersectMultiLine(
  const ALine: IGeometryProjectedMultiLine;
  const ARect: TDoubleRect
): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedLine;
begin
  Result := False;
  for i := 0 to ALine.Count - 1 do begin
    VLine := ALine.Item[i];
    if IsRectIntersectLine(VLine, ARect) then begin
      Result := True;
      Break;
    end;
  end;
end;

function TProjectedCalc.IsRectIntersectMultiPolygon(
  const APoly: IGeometryProjectedMultiPolygon;
  const ARect: TDoubleRect
): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedPolygon;
begin
  Result := False;
  for i := 0 to APoly.Count - 1 do begin
    VLine := APoly.Item[i];
    if IsRectIntersectPolygon(VLine, ARect) then begin
      Result := True;
      Break;
    end;
  end;
end;

function TProjectedCalc.IsRectIntersectMultiPolygonBorder(
  const APoly: IGeometryProjectedMultiPolygon;
  const ARect: TDoubleRect
): Boolean;
var
  i: Integer;
  VLine: IGeometryProjectedPolygon;
begin
  Result := False;
  for i := 0 to APoly.Count - 1 do begin
    VLine := APoly.Item[i];
    if IsRectIntersectPolygonBorder(VLine, ARect) then begin
      Result := True;
      Break;
    end;
  end;
end;

function TProjectedCalc.IsRectIntersectPolygon(
  const APoly: IGeometryProjectedPolygon;
  const ARect: TDoubleRect
): Boolean;
var
  VEnum: IEnumProjectedPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
  VIntersect: Double;
  VDelta: TDoublePoint;
  VRectIn: Boolean;
begin
  if not IsIntersecProjectedRect(APoly.Bounds, ARect) then begin
    Result := False;
  end else begin
    if PixelPointInRect(APoly.Points[0], ARect) then begin
      Result := True;
    end else begin
      VRectIn := False;
      Result := False;
      VEnum := APoly.GetEnum;
      // »щем есть ли пересечени€ пр€моугольника с полигоном,
      // и заодно провер€ем попадает ли левый верхний угол в полигон
      if VEnum.Next(VPrevPoint) then begin
        while VEnum.Next(VCurrPoint) do begin
          VDelta.X := VCurrPoint.X - VPrevPoint.X;
          VDelta.Y := VCurrPoint.Y - VPrevPoint.Y;
          if (VDelta.Y < 0) then begin
            if (VCurrPoint.Y <= ARect.Top) and (ARect.Top < VPrevPoint.Y) then begin
              VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
              if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
                Result := True;
                Exit;
              end;
              if (ARect.Left > VIntersect) then begin
                VRectIn := not VRectIn;
              end;
            end;
            if (VCurrPoint.Y <= ARect.Bottom) and (ARect.Bottom < VPrevPoint.Y) then begin
              VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
              if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
                Result := True;
                Exit;
              end;
            end;
          end else if (VDelta.Y > 0) then begin
            if (VCurrPoint.Y > ARect.Top) and (ARect.Top >= VPrevPoint.Y) then begin
              VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
              if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
                Result := True;
                Exit;
              end;
              if (ARect.Left > VIntersect) then begin
                VRectIn := not VRectIn;
              end;
            end;
            if (VCurrPoint.Y > ARect.Bottom) and (ARect.Bottom >= VPrevPoint.Y) then begin
              VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
              if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
                Result := True;
                Exit;
              end;
            end;
          end;

          if (VDelta.X < 0) then begin
            if (VCurrPoint.X <= ARect.Left) and (ARect.Left < VPrevPoint.X) then begin
              VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
              if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
                Result := True;
                Exit;
              end;
            end;
            if (VCurrPoint.X <= ARect.Right) and (ARect.Right < VPrevPoint.X) then begin
              VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
              if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
                Result := True;
                Exit;
              end;
            end;
          end else if (VDelta.X > 0) then begin
            if (VCurrPoint.X > ARect.Left) and (ARect.Left >= VPrevPoint.X) then begin
              VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
              if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
                Result := True;
                Exit;
              end;
            end;
            if (VCurrPoint.X > ARect.Right) and (ARect.Right >= VPrevPoint.X) then begin
              VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
              if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
                Result := True;
                Exit;
              end;
            end;
          end;

          VPrevPoint := VCurrPoint;
        end;
        Result := VRectIn;
      end;
    end;
  end;
end;

function TProjectedCalc.IsRectIntersectPolygonBorder(
  const APoly: IGeometryProjectedPolygon;
  const ARect: TDoubleRect
): Boolean;
var
  VEnum: IEnumProjectedPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
  VIntersect: Double;
  VDelta: TDoublePoint;
begin
  if not IsIntersecProjectedRect(APoly.Bounds, ARect) then begin
    Result := False;
  end else begin
    Result := False;
    VEnum := APoly.GetEnum;
    // »щем есть ли пересечени€ пр€моугольника с полигоном
    if VEnum.Next(VPrevPoint) then begin
      while VEnum.Next(VCurrPoint) do begin
        VDelta.X := VCurrPoint.X - VPrevPoint.X;
        VDelta.Y := VCurrPoint.Y - VPrevPoint.Y;
        if (VDelta.Y < 0) then begin
          if (VCurrPoint.Y <= ARect.Top) and (ARect.Top < VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.Y <= ARect.Bottom) and (ARect.Bottom < VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
        end else if (VDelta.Y > 0) then begin
          if (VCurrPoint.Y > ARect.Top) and (ARect.Top >= VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Top - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.Y > ARect.Bottom) and (ARect.Bottom >= VPrevPoint.Y) then begin
            VIntersect := VDelta.X * (ARect.Bottom - VPrevPoint.y) / VDelta.Y + VPrevPoint.x;
            if (ARect.Left <= VIntersect) and (VIntersect < ARect.Right) then begin
              Result := True;
              Exit;
            end;
          end;
        end;

        if (VDelta.X < 0) then begin
          if (VCurrPoint.X <= ARect.Left) and (ARect.Left < VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.X <= ARect.Right) and (ARect.Right < VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
        end else if (VDelta.X > 0) then begin
          if (VCurrPoint.X > ARect.Left) and (ARect.Left >= VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Left - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
          if (VCurrPoint.X > ARect.Right) and (ARect.Right >= VPrevPoint.X) then begin
            VIntersect := VDelta.Y * (ARect.Right - VPrevPoint.X) / VDelta.X + VPrevPoint.Y;
            if (ARect.Top <= VIntersect) and (VIntersect < ARect.Bottom) then begin
              Result := True;
              Exit;
            end;
          end;
        end;

        VPrevPoint := VCurrPoint;
      end;
    end;
  end;
end;

end.


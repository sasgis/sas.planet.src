{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_MarkerDrawableSimpleCircle;

interface

uses
  GR32,
  t_GeoTypes,
  u_MarkerDrawableSimpleAbstract;

type
  TMarkerDrawableSimpleCircle = class(TMarkerDrawableSimpleAbstract)
  protected
    function GetBoundsForPosition(const APosition: TDoublePoint): TRect; override;
    function DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ): Boolean; override;
  end;

implementation

uses
  Types,
  Math,
  GR32_Math,
  GR32_Polygons,
  u_GeoFunc;

function CalculateCircleSteps(const ARadius: TFloat): Cardinal; inline;
var
  VAbsRadius: TFloat;
begin
  VAbsRadius := Abs(ARadius);
  Result := Trunc(Pi / (ArcCos(VAbsRadius / (VAbsRadius + 0.125))));
end;

function GenerateCirclePoints(
  const ACenter: TFloatPoint;
  const ARadius: TFloat
): TArrayOfFixedPoint;
var
  I: Integer;
  M: TFloat;
  C, D: TFloatPoint;
  VSteps: Integer;
begin
  VSteps := CalculateCircleSteps(ARadius);

  SetLength(Result, VSteps);
  M := 2 * System.Pi / VSteps;

  // first item
  Result[0].X := Fixed(ARadius + ACenter.X);
  Result[0].Y := Fixed(ACenter.Y);

  // calculate complex offset
  GR32_Math.SinCos(M, C.Y, C.X);
  D.X := ARadius * C.X;
  D.Y := ARadius * C.Y;

  // second item
  Result[1].X := Fixed(D.X + ACenter.X);
  Result[1].Y := Fixed(D.Y + ACenter.Y);

  // other items
  for I := 2 to VSteps - 1 do begin
    D := FloatPoint(D.X * C.X - D.Y * C.Y, D.Y * C.X + D.X * C.Y);

    Result[I].X := Fixed(D.X + ACenter.X);
    Result[I].Y := Fixed(D.Y + ACenter.Y);
  end;
end;

{ TMarkerDrawableSimpleCircle }

function TMarkerDrawableSimpleCircle.DrawToBitmap(
  ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint
): Boolean;
var
  VHalfSize: Double;
  VDoubleRect: TDoubleRect;
  VRect: TRect;
  VTargetRect: TRect;
  VCirclePoints: TArrayOfFixedPoint;
begin
  VHalfSize := Config.MarkerSize / 2;
  VDoubleRect.Left := APosition.X - VHalfSize;
  VDoubleRect.Top := APosition.Y - VHalfSize;
  VDoubleRect.Right := APosition.X + VHalfSize;
  VDoubleRect.Bottom := APosition.Y + VHalfSize;
  VRect := RectFromDoubleRect(VDoubleRect, rrClosest);

  VTargetRect := VRect;
  Types.IntersectRect(VTargetRect, ABitmap.ClipRect, VTargetRect);
  if Types.IsRectEmpty(VTargetRect) then begin
    Result := False;
    Exit;
  end;

  if not ABitmap.MeasuringMode then begin
    VCirclePoints := GenerateCirclePoints(
      FloatPoint(APosition.X, APosition.Y),
      VHalfSize
    );
    if Length(VCirclePoints) > 0 then begin
      PolygonTS(ABitmap, VCirclePoints, Config.MarkerColor);
      if Config.MarkerColor <> Config.BorderColor then begin
        PolylineXS(ABitmap, VCirclePoints, Config.BorderColor, True);
      end;
    end;
  end else begin
    ABitmap.Changed(VTargetRect);
  end;
  Result := True;
end;

function TMarkerDrawableSimpleCircle.GetBoundsForPosition(
  const APosition: TDoublePoint
): TRect;
var
  VHalfSize: Double;
  VTargetDoubleRect: TDoubleRect;
begin
  VHalfSize := Config.MarkerSize / 2;
  VTargetDoubleRect.Left := APosition.X - VHalfSize;
  VTargetDoubleRect.Top := APosition.Y - VHalfSize;
  VTargetDoubleRect.Right := APosition.X + VHalfSize;
  VTargetDoubleRect.Bottom := APosition.Y + VHalfSize;

  Result := RectFromDoubleRect(VTargetDoubleRect, rrOutside);
end;

end.

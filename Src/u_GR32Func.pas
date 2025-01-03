{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_GR32Func;

interface

uses
  GR32,
  GR32_Polygons,
  GR32_VectorUtils;

procedure UpdateRectByFloatPoint(var ARect: TFloatRect; const APoint: TFloatPoint); inline;
procedure UpdateRectByArrayOfFloatPoint(var ARect: TFloatRect; const AArray: TArrayOfFloatPoint); inline;
procedure UpdateRectByArrayOfArrayOfFloatPoint(var ARect: TFloatRect; const AArray: TArrayOfArrayOfFloatPoint); inline;

procedure DrawThickLine(ABitmap: TBitmap32; const AStart, AEnd: TFloatPoint; const AColor: TColor32; const AWidth: Integer); inline;
procedure DrawThickPolyLine(ABitmap: TBitmap32; const APoints: TArrayOfFloatPoint; const AColor: TColor32; const AWidth: Integer);

implementation

procedure UpdateRectByFloatPoint(var ARect: TFloatRect; const APoint: TFloatPoint);
begin
  if ARect.Left > APoint.X then begin
    ARect.Left := APoint.X;
  end;
  if ARect.Right < APoint.X then begin
    ARect.Right := APoint.X;
  end;
  if ARect.Top > APoint.Y then begin
    ARect.Top := APoint.Y;
  end;
  if ARect.Bottom < APoint.Y then begin
    ARect.Bottom := APoint.Y;
  end;
end;

procedure UpdateRectByArrayOfFloatPoint(var ARect: TFloatRect; const AArray: TArrayOfFloatPoint);
var
  I: Integer;
begin
  for I := 0 to Length(AArray) - 1 do begin
    UpdateRectByFloatPoint(ARect, AArray[I]);
  end;
end;

procedure UpdateRectByArrayOfArrayOfFloatPoint(var ARect: TFloatRect; const AArray: TArrayOfArrayOfFloatPoint);
var
  I: Integer;
begin
  for I := 0 to Length(AArray) - 1 do begin
    UpdateRectByArrayOfFloatPoint(ARect, AArray[I]);
  end;
end;

procedure DrawThickLine(ABitmap: TBitmap32; const AStart, AEnd: TFloatPoint; const AColor: TColor32; const AWidth: Integer);
var
  VLine: TArrayOfFloatPoint;
begin
  SetLength(VLine, 2);

  VLine[0] := AStart;
  VLine[1] := AEnd;

  DrawThickPolyLine(ABitmap, VLine, AColor, AWidth);
end;

procedure DrawThickPolyLine(ABitmap: TBitmap32; const APoints: TArrayOfFloatPoint; const AColor: TColor32; const AWidth: Integer);
var
  VPoly: TArrayOfFloatPoint;
begin
  VPoly := BuildPolyLine(APoints, AWidth);

  if ABitmap.MeasuringMode then begin
    ABitmap.Changed(MakeRect(PolygonBounds(VPoly), rrOutside));
  end else begin
    PolygonFS(ABitmap, VPoly, AColor);
  end;
end;

end.

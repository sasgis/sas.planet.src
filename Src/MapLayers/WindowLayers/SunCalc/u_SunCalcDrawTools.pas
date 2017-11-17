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

unit u_SunCalcDrawTools;

interface

uses
  GR32;

procedure ThickLine(
  ABitmap: TBitmap32;
  const AStart, AEnd: TFixedPoint;
  const AColor: TColor32;
  const AWidth: Integer
);

procedure ThickPolyLine(
  ABitmap: TBitmap32;
  const APoints: TArrayOfFixedPoint;
  const AColor: TColor32
);

implementation

uses
  Types,
  GR32_Math,
  GR32_Polygons;

var
  GUsePolygonAntiAliasing: Boolean = True;

procedure ThickLine(
  ABitmap: TBitmap32;
  const AStart, AEnd: TFixedPoint;
  const AColor: TColor32;
  const AWidth: Integer
);
var
  I: Integer;
  P1, P2: TPoint;
  VPolygon: TPolygon32;
begin
  I := AWidth div 2;

  P1 := GR32.Point(AStart);
  P2 := GR32.Point(AEnd);

  VPolygon := TPolygon32.Create;
  try
    VPolygon.Add(FixedPoint(P1.X - I, P1.Y));
    VPolygon.Add(FixedPoint(P1.X + I, P1.Y));
    VPolygon.Add(FixedPoint(P2.X + I, P2.Y));
    VPolygon.Add(FixedPoint(P2.X - I, P2.Y));

    if GUsePolygonAntiAliasing then begin
      try
        PolyPolygonXS(ABitmap, VPolygon.Points, AColor);
      except
        GUsePolygonAntiAliasing := False;
      end;
    end else begin
      PolyPolygonTS(ABitmap, VPolygon.Points, AColor);
    end;

    VPolygon.Clear;

    VPolygon.Add(FixedPoint(P1.X, P1.Y + I));
    VPolygon.Add(FixedPoint(P1.X, P1.Y - I));
    VPolygon.Add(FixedPoint(P2.X, P2.Y - I));
    VPolygon.Add(FixedPoint(P2.X, P2.Y + I));

    if GUsePolygonAntiAliasing then begin
      try
        PolyPolygonXS(ABitmap, VPolygon.Points, AColor);
      except
        GUsePolygonAntiAliasing := False;
      end;
    end else begin
      PolyPolygonTS(ABitmap, VPolygon.Points, AColor);
    end;
  finally
    VPolygon.Free;
  end;
end;

procedure ThickPolyLine(
  ABitmap: TBitmap32;
  const APoints: TArrayOfFixedPoint;
  const AColor: TColor32
);
var
  I: Integer;
  VPolygon, VTmp: TPolygon32;
begin
  VPolygon := TPolygon32.Create;
  try
    VPolygon.Closed := False;
    VPolygon.AddPoints(APoints[0], Length(APoints));
    for I := -2 to 1 do begin
      if I = 0 then begin
        PolylineXS(ABitmap, APoints, AColor, False);
      end else begin
        VTmp := VPolygon.Grow(Fixed(I));
        try
          PolyPolyLineXS(ABitmap, VTmp.Points, AColor, False);
        finally
          VTmp.Free;
        end;
      end;
    end;
  finally
    VPolygon.Free;
  end;
end;

end.

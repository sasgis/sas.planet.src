{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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
  const AStart, AEnd: TFloatPoint;
  const AColor: TColor32;
  const AWidth: Integer
);

procedure ThickPolyLine(
  ABitmap: TBitmap32;
  const APoints: TArrayOfFloatPoint;
  const AColor: TColor32
); inline;

implementation

uses
  Types,
  GR32_Math,
  GR32_Polygons;

procedure ThickLine(
  ABitmap: TBitmap32;
  const AStart, AEnd: TFloatPoint;
  const AColor: TColor32;
  const AWidth: Integer
);
var
  I: Integer;
  P1, P2: TPoint;
  VPolygon: TArrayOfFloatPoint;
begin
  I := AWidth div 2;

  P1 := GR32.Point(AStart);
  P2 := GR32.Point(AEnd);

  SetLength(VPolygon, 4);

  VPolygon[0] := FloatPoint(P1.X - I, P1.Y);
  VPolygon[1] := FloatPoint(P1.X + I, P1.Y);
  VPolygon[2] := FloatPoint(P2.X + I, P2.Y);
  VPolygon[3] := FloatPoint(P2.X - I, P2.Y);

  PolygonFS(ABitmap, VPolygon, AColor);

  VPolygon[0] := FloatPoint(P1.X, P1.Y + I);
  VPolygon[1] := FloatPoint(P1.X, P1.Y - I);
  VPolygon[2] := FloatPoint(P2.X, P2.Y - I);
  VPolygon[3] := FloatPoint(P2.X, P2.Y + I);

  PolygonFS(ABitmap, VPolygon, AColor);
end;

procedure ThickPolyLine(
  ABitmap: TBitmap32;
  const APoints: TArrayOfFloatPoint;
  const AColor: TColor32
);
begin
  PolylineFS(ABitmap, APoints, AColor, False, 4);
end;

end.

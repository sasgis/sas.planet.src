{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_SunCalcDrawTools;

interface

uses
  GR32;

procedure ThickLine(
  ABitmap: TBitmap32;
  const AStart, AEnd: TFloatPoint;
  const AColor: TColor32;
  const AWidth: Integer
); inline;

procedure ThickPolyLine(
  ABitmap: TBitmap32;
  const APoints: TArrayOfFloatPoint;
  const AColor: TColor32;
  const AWidth: Integer
);

implementation

uses
  Types,
  GR32_Polygons,
  GR32_VectorUtils;

procedure ThickLine(
  ABitmap: TBitmap32;
  const AStart, AEnd: TFloatPoint;
  const AColor: TColor32;
  const AWidth: Integer
);
var
  VLine: TArrayOfFloatPoint;
begin
  SetLength(VLine, 2);

  VLine[0] := AStart;
  VLine[1] := AEnd;

  ThickPolyLine(ABitmap, VLine, AColor, AWidth);
end;

procedure ThickPolyLine(
  ABitmap: TBitmap32;
  const APoints: TArrayOfFloatPoint;
  const AColor: TColor32;
  const AWidth: Integer
);
var
  VPoly: TArrayOfFloatPoint;
begin
  VPoly := BuildPolyLine(APoints, AWidth);

  if not ABitmap.MeasuringMode then begin
    PolygonFS(ABitmap, VPoly, AColor);
  end;

  ABitmap.Changed(MakeRect(PolygonBounds(VPoly), rrOutside));
end;

end.

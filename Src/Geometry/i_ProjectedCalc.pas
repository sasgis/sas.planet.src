{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
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

unit i_ProjectedCalc;

interface

uses
  t_GeoTypes,
  i_GeometryProjected;

type
  IProjectedCalc = interface
    ['{11E107C3-7994-4922-ABA6-9449CCD1FB39}']
    function IsPointOnLine(
      const ALine: IGeometryProjectedLine;
      const APoint: TDoublePoint;
      const ADist: Double
    ): Boolean;
    function IsPointOnSingleLine(
      const ALine: IGeometryProjectedSingleLine;
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
    function IsRectIntersectSingleLine(
      const ALine: IGeometryProjectedSingleLine;
      const ARect: TDoubleRect
    ): Boolean;
    function IsRectIntersectMultiLine(
      const ALine: IGeometryProjectedMultiLine;
      const ARect: TDoubleRect
    ): Boolean;
    function CalcLineLength(
      const ALine: IGeometryProjectedLine
    ): Double;
    function CalcSingleLineLength(
      const ALine: IGeometryProjectedSingleLine
    ): Double;
    function CalcMultiLineLength(
      const ALine: IGeometryProjectedMultiLine
    ): Double;
    function IsPointInPolygon(
      const APoly: IGeometryProjectedPolygon;
      const APoint: TDoublePoint
    ): Boolean;
    function IsPointInSinglePolygon(
      const APoly: IGeometryProjectedSinglePolygon;
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
    function IsPointOnSinglePolygonBorder(
      const APoly: IGeometryProjectedSinglePolygon;
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
    function IsRectIntersectSinglePolygon(
      const APoly: IGeometryProjectedSinglePolygon;
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
    function IsRectIntersectSinglePolygonBorder(
      const APoly: IGeometryProjectedSinglePolygon;
      const ARect: TDoubleRect
    ): Boolean;
    function IsRectIntersectMultiPolygonBorder(
      const APoly: IGeometryProjectedMultiPolygon;
      const ARect: TDoubleRect
    ): Boolean;
    function CalcPolygonArea(
      const APoly: IGeometryProjectedPolygon
    ): Double;
    function CalcSinglePolygonArea(
      const APoly: IGeometryProjectedSinglePolygon
    ): Double;
    function CalcMultiPolygonArea(
      const APoly: IGeometryProjectedMultiPolygon
    ): Double;
    function CalcPolygonPerimeter(
      const APoly: IGeometryProjectedPolygon
    ): Double;
    function CalcSinglePolygonPerimeter(
      const APoly: IGeometryProjectedSinglePolygon
    ): Double;
    function CalcMultiPolygonPerimeter(
      const APoly: IGeometryProjectedMultiPolygon
    ): Double;
  end;

implementation

end.

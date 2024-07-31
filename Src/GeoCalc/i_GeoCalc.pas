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

unit i_GeoCalc;

interface

uses
  i_NotifierOperation,
  i_GeometryLonLat;

type
  IGeoCalc = interface
    ['{C85EEB06-2EA7-437E-8A34-2C7C53A87543}']
    // distance
    function CalcSingleLineLength(const ALine: IGeometryLonLatSingleLine): Double;
    function CalcMultiLineLength(const ALine: IGeometryLonLatMultiLine): Double;
    function CalcLineLength(const ALine: IGeometryLonLatLine): Double;

    // perimeter
    function CalcContourPerimeter(const ALine: IGeometryLonLatContour): Double;
    function CalcSinglePolygonPerimeter(const ALine: IGeometryLonLatSinglePolygon): Double;
    function CalcMultiPolygonPerimeter(const ALine: IGeometryLonLatMultiPolygon): Double;
    function CalcPolygonPerimeter(const ALine: IGeometryLonLatPolygon): Double;

    // area
    function CalcContourArea(
      const ALine: IGeometryLonLatContour;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;

    function CalcSinglePolygonArea(
      const ALine: IGeometryLonLatSinglePolygon;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;

    function CalcMultiPolygonArea(
      const ALine: IGeometryLonLatMultiPolygon;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;

    function CalcPolygonArea(
      const ALine: IGeometryLonLatPolygon;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;
  end;

implementation

end.

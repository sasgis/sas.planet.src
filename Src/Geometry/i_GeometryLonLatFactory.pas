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

unit i_GeometryLonLatFactory;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_EnumDoublePoint,
  i_DoublePoints,
  i_DoublePointFilter,
  i_DoublePointsAggregator,
  i_GeometryLonLat;

type
  IGeometryLonLatLineBuilder = interface
    ['{19605EB8-E09C-4E69-A86E-B8701F1FB9C9}']
    procedure AddLine(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    ); overload;
    procedure AddLine(
      const APoints: IDoublePoints
    ); overload;

    function MakeStaticAndClear: IGeometryLonLatLine;
    function MakeStaticCopy: IGeometryLonLatLine;
  end;

  IGeometryLonLatPolygonBuilder = interface
    ['{993D049C-A360-4185-94CF-E1828503F7F4}']
    procedure AddPolygon(
      const APolygon: IGeometryLonLatSinglePolygon
    ); overload;
    procedure AddPolygon(
      const APolygon: IGeometryLonLatMultiPolygon
    ); overload;
    procedure AddPolygon(
      const APolygon: IGeometryLonLatPolygon
    ); overload;
    procedure AddOuter(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    ); overload;
    procedure AddOuter(
      const APoints: IDoublePoints
    ); overload;
    procedure AddOuter(
      const AContour: IGeometryLonLatContour
    ); overload;
    procedure AddHole(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    ); overload;
    procedure AddHole(
      const APoints: IDoublePoints
    ); overload;
    procedure AddHole(
      const AContour: IGeometryLonLatContour
    ); overload;

    function MakeStaticAndClear: IGeometryLonLatPolygon;
    function MakeStaticCopy: IGeometryLonLatPolygon;
  end;

  IGeometryLonLatFactory = interface
    ['{FD69BBD0-2065-43B0-9D7C-900E82C28069}']
    function CreateLonLatPoint(
      const APoint: TDoublePoint
    ): IGeometryLonLatPoint;
    function CreateLonLatSingleLine(
      const APoints: IDoublePoints
    ): IGeometryLonLatSingleLine;
    function CreateLonLatSinglePolygon(
      const APoints: IDoublePoints
    ): IGeometryLonLatSinglePolygon;

    function MakeLineBuilder(): IGeometryLonLatLineBuilder;
    function MakePolygonBuilder(): IGeometryLonLatPolygonBuilder;

    function CreateLonLatLine(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatLine;
    function CreateLonLatPolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatPolygon;
    function CreateLonLatLineByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatLine;
    function CreateLonLatPolygonByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatPolygon;

    function CreateLonLatPolygonByRect(
      const ARect: TDoubleRect
    ): IGeometryLonLatPolygon;
    function CreateLonLatPolygonCircleByPoint(
      const AProjection: IProjectionInfo;
      const APos: TDoublePoint;
      const ARadius: double
    ): IGeometryLonLatPolygon;
    function CreateLonLatPolygonByLonLatPathAndFilter(
      const ASource: IGeometryLonLatLine;
      const AFilter: ILonLatPointFilter
    ): IGeometryLonLatPolygon;
  end;

implementation

end.

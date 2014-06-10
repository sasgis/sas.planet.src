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
  i_DoublePointFilter,
  i_DoublePointsAggregator,
  i_GeometryLonLat;

type
  IGeometryLonLatMultiLineBuilder = interface
    ['{19605EB8-E09C-4E69-A86E-B8701F1FB9C9}']
    procedure Add(const AElement: IGeometryLonLatSingleLine);

    function MakeStaticAndClear: IGeometryLonLatMultiLine;
    function MakeStaticCopy: IGeometryLonLatMultiLine;
  end;

  IGeometryLonLatMultiPolygonBuilder = interface
    ['{993D049C-A360-4185-94CF-E1828503F7F4}']
    procedure Add(const AElement: IGeometryLonLatSinglePolygon);

    function MakeStaticAndClear: IGeometryLonLatMultiPolygon;
    function MakeStaticCopy: IGeometryLonLatMultiPolygon;
  end;

  IGeometryLonLatFactory = interface
    ['{FD69BBD0-2065-43B0-9D7C-900E82C28069}']
    function CreateLonLatPoint(
      const APoint: TDoublePoint
    ): IGeometryLonLatPoint;
    function CreateLonLatLine(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatSingleLine;
    function CreateLonLatPolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatSinglePolygon;

    function MakeGeometryLonLatMultiLineBuilder(): IGeometryLonLatMultiLineBuilder;
    function MakeGeometryLonLatMultiPolygonBuilder(): IGeometryLonLatMultiPolygonBuilder;

    function CreateLonLatMultiLine(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatMultiLine;
    function CreateLonLatMultiPolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatMultiPolygon;
    function CreateLonLatMultiLineByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatMultiLine;
    function CreateLonLatMultiPolygonByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatMultiPolygon;

    function CreateLonLatMultiPolygonByRect(
      const ARect: TDoubleRect
    ): IGeometryLonLatMultiPolygon;
    function CreateLonLatMultiPolygonCircleByPoint(
      const AProjection: IProjectionInfo;
      const APos: TDoublePoint;
      const ARadius: double
    ): IGeometryLonLatMultiPolygon;
    function CreateLonLatMultiPolygonByLonLatPathAndFilter(
      const ASource: IGeometryLonLatMultiLine;
      const AFilter: ILonLatPointFilter
    ): IGeometryLonLatMultiPolygon;
  end;

implementation

end.

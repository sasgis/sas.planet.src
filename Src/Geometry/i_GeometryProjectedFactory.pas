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

unit i_GeometryProjectedFactory;

interface

uses
  t_GeoTypes,
  i_Projection,
  i_DoublePoints,
  i_DoublePointsAggregator,
  i_GeometryLonLat,
  i_GeometryProjected;

type
  IGeometryProjectedLineBuilder = interface
    ['{95459DF2-C324-452F-A738-7C7D3D4EA533}']
    procedure AddLine(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    );

    function MakeStaticAndClear: IGeometryProjectedLine;
    function MakeStaticCopy: IGeometryProjectedLine;
  end;

  IGeometryProjectedPolygonBuilder = interface
    ['{6057514C-8A8F-40A4-A865-E92AFA4373A6}']
    procedure AddOuter(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    );
    procedure AddHole(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    );

    function MakeStaticAndClear: IGeometryProjectedPolygon;
    function MakeStaticCopy: IGeometryProjectedPolygon;
  end;

  IGeometryProjectedFactory = interface
    ['{06CC36BA-1833-4AE8-953F-D003B6D81BB7}']
    function MakeLineBuilder(): IGeometryProjectedLineBuilder;
    function MakePolygonBuilder(): IGeometryProjectedPolygonBuilder;

    function CreateProjectedLineByLonLatPath(
      const AProjection: IProjection;
      const ASource: IGeometryLonLatLine;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedLine;
    function CreateProjectedPolygonByLonLatPolygon(
      const AProjection: IProjection;
      const ASource: IGeometryLonLatPolygon;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedPolygon;
  end;

implementation

end.

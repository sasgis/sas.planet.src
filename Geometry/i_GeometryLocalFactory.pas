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

unit i_GeometryLocalFactory;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  i_GeometryLocal;

type
  IGeometryLocalFactory = interface
    ['{E44B8BA5-0443-40CC-8F48-F8B817D0328A}']
    function CreateLocalPath(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLocalMultiLine;
    function CreateLocalPolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLocalMultiPolygon;

    function CreateLocalPathByEnum(
      const AEnum: IEnumLocalPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLocalMultiLine;
    function CreateLocalPolygonByEnum(
      const AEnum: IEnumLocalPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLocalMultiPolygon;
  end;

implementation

end.

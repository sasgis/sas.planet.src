{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit i_TileIteratorDataProvider;

interface

uses
  Types,
  i_Projection,
  i_GeometryProjected;

type
  ITileIteratorDataProvider = interface
    ['{0954A639-44C5-4DED-BDD3-E65D4A9DE39F}']
    function GetProjection: IProjection;
    property Projection : IProjection read GetProjection;

    function GetPolygon: IGeometryProjectedPolygon;
    property Polygon: IGeometryProjectedPolygon read GetPolygon;

    function GetTilesTotal: Int64;
    property TilesTotal: Int64 read GetTilesTotal;

    function GetTilesToProcess(const APartIndex: Integer): Int64;
    property TilesToProcess[const APartIndex: Integer]: Int64 read GetTilesToProcess;

    function GetStartPoint(const APartIndex: Integer): TPoint;
    property StartPoint[const APartIndex: Integer]: TPoint read GetStartPoint;

    function GetPartsCount: Integer;
    property PartsCount: Integer read GetPartsCount;
  end;

implementation

end.

{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit i_TileIteratorFactory;

interface

uses
  i_Projection,
  i_TileIterator,
  i_GeometryLonLat;

type
  ITileIteratorFactory = interface
    ['{20EBCB23-848B-42BB-97DF-31C4579B3C5C}']

    function MakeTileIterator(
      const AProjection: IProjection;
      const ALonLatPolygon: IGeometryLonLatPolygon;
      const ATilesToProcess: Int64 = -1;
      const AStartPointX: Integer = -1;
      const AStartPointY: Integer = -1
    ): ITileIterator;
  end;

implementation

end.

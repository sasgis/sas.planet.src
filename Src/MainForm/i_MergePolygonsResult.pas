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

unit i_MergePolygonsResult;

interface

uses
  i_GeometryLonLat,
  i_Changeable;

type
  IMergePolygonsResult = interface(IChangeable)
    ['{4C312E45-1C11-4197-81D1-0F1E7888A5B6}']

    function GetPolygon: IGeometryLonLatPolygon;
    procedure SetPolygon(const ALonLatPolygon: IGeometryLonLatPolygon);

    property Polygon: IGeometryLonLatPolygon read GetPolygon write SetPolygon;
  end;

implementation

end.

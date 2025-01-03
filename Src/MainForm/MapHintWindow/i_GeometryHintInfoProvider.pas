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

unit i_GeometryHintInfoProvider;

interface

uses
  Types,
  t_GeoTypes,
  i_GeometryLonLat,
  i_LocalCoordConverter;

type
  TLineHintInfo = record
    LonLatPos: TDoublePoint;
    Distance: Double;
    Elevation: Double;
    TimeStamp: TDateTime;
    Speed: Double;
  end;

  TPolyHintInfo = record
    Area: Double;
    Perimeter: Double;
    PointsCount: Integer;
    ContoursCount: Integer;
    CurrentContour: Integer;
  end;

  IGeometryHintInfoProvider = interface
    ['{E3DBC106-B0C9-413A-8E6C-F788440AC92A}']
    function GetLineHintInfo(
      const ALocalConverter: ILocalCoordConverter;
      const ALine: IGeometryLonLatLine;
      const AMousePos: TPoint;
      out AInfo: TLineHintInfo
    ): Boolean;

    function GetPolyHintInfo(
      const ALocalConverter: ILocalCoordConverter;
      const APoly: IGeometryLonLatPolygon;
      const AMousePos: TPoint;
      out AInfo: TPolyHintInfo
    ): Boolean;
  end;

implementation

end.

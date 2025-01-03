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

unit i_RegionProcess;

interface

uses
  i_GeometryLonLat;

type
  IRegionProcess = interface
    ['{825D4FA8-6FDD-4BB7-8D60-F7D4EEA3EB4D}']
    procedure ProcessPolygon(
      const APolygon: IGeometryLonLatPolygon
    );
    procedure ProcessPolygonWithZoom(
      const AZoom: Byte;
      const APolygon: IGeometryLonLatPolygon
    );
  end;

  IRegionProcessFromFile = interface(IRegionProcess)
    ['{8A267DB0-B9EC-4547-8F2A-69177F9C7878}']
    procedure LoadSelFromFile(
      const AFileName: string;
      out APolygon: IGeometryLonLatPolygon
    );
    procedure StartSlsFromFile(
      const AFileName: string;
      const AStartPaused: Boolean
    );
  end;

implementation

end.

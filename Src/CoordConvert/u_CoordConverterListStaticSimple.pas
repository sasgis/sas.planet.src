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

unit u_CoordConverterListStaticSimple;

interface

uses
  i_CoordConverterFactory,
  u_CoordConverterListStatic;

type
  TCoordConverterListStaticSimple = class(TCoordConverterListStatic)
  public
    constructor Create(const AFactory: IProjectionSetFactory);
  end;

implementation

uses
  gnugettext,
  c_CoordConverter,
  i_ProjectionSet,
  i_CoordConverter;

{ TCoordConverterListStaticSimple }

constructor TCoordConverterListStaticSimple.Create;
var
  VProjectionSet: IProjectionSet;
begin
  inherited Create;
  VProjectionSet := AFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256);
  Add(VProjectionSet, gettext_NoOp('Mercator / Google Maps (Sphere Radius 6378137) / EPSG:3785'));

  VProjectionSet := AFactory.GetProjectionSetByCode(CYandexProjectionEPSG, CTileSplitQuadrate256x256);
  Add(VProjectionSet, gettext_NoOp('Mercator / WGS84 / EPSG:3395'));

  VProjectionSet := AFactory.GetProjectionSetByCode(CGELonLatProjectionEPSG, CTileSplitQuadrate256x256);
  Add(VProjectionSet, gettext_NoOp('Geographic (Latitude/Longitude) / WGS84 / EPSG:4326'));
end;

end.

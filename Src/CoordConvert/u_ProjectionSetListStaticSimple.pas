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

unit u_ProjectionSetListStaticSimple;

interface

uses
  i_ProjectionSetFactory,
  u_ProjectionSetListStatic;

type
  TProjectionSetListStaticSimple = class(TProjectionSetListStatic)
  public
    constructor Create(const AFactory: IProjectionSetFactory);
  end;

implementation

uses
  gnugettext,
  c_CoordConverter,
  i_ProjectionSet;

{ TProjectionSetListStaticSimple }

constructor TProjectionSetListStaticSimple.Create;
var
  VProjectionSet: IProjectionSet;
begin
  inherited Create;
  VProjectionSet := AFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256);
  Add(VProjectionSet, gettext_NoOp('Spherical Mercator (GoogleMaps, OSM, Bing) / EPSG:3857'));

  VProjectionSet := AFactory.GetProjectionSetByCode(CYandexProjectionEPSG, CTileSplitQuadrate256x256);
  Add(VProjectionSet, gettext_NoOp('World Mercator (Yandex) / EPSG:3395'));

  VProjectionSet := AFactory.GetProjectionSetByCode(CGELonLatProjectionEPSG, CTileSplitQuadrate256x256);
  Add(VProjectionSet, gettext_NoOp('Geographic (GoogleEarth) / EPSG:4326'));
end;

end.

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

unit u_MergePolygonsResultLayerConfig;

interface

uses
  i_MergePolygonsResultLayerConfig,
  u_PolygonLayerConfig;

type
  TMergePolygonsResultLayerConfig = class(TPolygonLayerConfig, IMergePolygonsResultLayerConfig)
  public
    constructor Create;
  end;

implementation

uses
  GR32;

{ TMergePolygonsResultLayerConfig }

constructor TMergePolygonsResultLayerConfig.Create;
begin
  inherited;
  SetFillColor(SetAlpha(clGreen32, 150));
  SetLineColor(SetAlpha(clRed32, 210));
  SetLineWidth(2);
end;

end.

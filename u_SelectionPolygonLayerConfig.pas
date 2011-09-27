{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_SelectionPolygonLayerConfig;

interface

uses
  i_SelectionPolygonLayerConfig,
  u_PolygonLayerConfig;

type
  TSelectionPolygonLayerConfig = class(TPolygonLayerConfig, ISelectionPolygonLayerConfig)
  public
    constructor Create;
  end;

implementation

uses
  GR32;

{ TSelectionPolygonLayerConfig }

constructor TSelectionPolygonLayerConfig.Create;
begin
  inherited;
  LockWrite;
  try
    SetLineColor(SetAlpha(clBlue32, 180));

    SetPointFillColor(SetAlpha(clYellow32, 150));
    SetPointRectColor(SetAlpha(ClRed32, 150));
    SetPointFirstColor(SetAlpha(ClGreen32, 255));
    SetPointActiveColor(SetAlpha(ClRed32, 255));

    SetFillColor(SetAlpha(clWhite32, 40));
  finally
    UnlockWrite;
  end;
end;

end.

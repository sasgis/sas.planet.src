{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
  i_PolyLineLayerConfig,
  i_PolygonLayerConfig,
  i_SelectionPolygonLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TSelectionPolygonLayerConfig = class(TConfigDataElementComplexBase, ISelectionPolygonLayerConfig)
  private
    FLineConfig: IPolygonLayerConfig;
    FPointsConfig: IPointsSetLayerConfig;
  private
    function GetLineConfig: IPolygonLayerConfig;
    function GetPointsConfig: IPointsSetLayerConfig;
  public
    constructor Create;
  end;

implementation

uses
  GR32,
  i_MarkerSimpleConfig,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_MarkerSimpleConfigStatic,
  u_PointsSetLayerConfig,
  u_PolygonLayerConfig;

{ TSelectionPolygonLayerConfig }

constructor TSelectionPolygonLayerConfig.Create;
var
  VFirstPointMarkerDefault: IMarkerSimpleConfigStatic;
  VActivePointMarkerDefault: IMarkerSimpleConfigStatic;
  VNormalPointMarkerDefault: IMarkerSimpleConfigStatic;
begin
  inherited Create;
  FLineConfig := TPolygonLayerConfig.Create;
  FLineConfig.LineColor := SetAlpha(clBlue32, 180);
  FLineConfig.FillColor := SetAlpha(clWhite32, 40);
  Add(FLineConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  VFirstPointMarkerDefault :=
    TMarkerSimpleConfigStatic.Create(
      8,
      SetAlpha(ClGreen32, 255),
      SetAlpha(ClRed32, 150)
    );

  VActivePointMarkerDefault :=
    TMarkerSimpleConfigStatic.Create(
      8,
      SetAlpha(ClRed32, 255),
      SetAlpha(ClRed32, 150)
    );

  VNormalPointMarkerDefault :=
    TMarkerSimpleConfigStatic.Create(
      8,
      SetAlpha(clYellow32, 150),
      SetAlpha(ClRed32, 150)
    );

  FPointsConfig :=
    TPointsSetLayerConfig.Create(
      VFirstPointMarkerDefault,
      VActivePointMarkerDefault,
      VNormalPointMarkerDefault
    );
  Add(FPointsConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TSelectionPolygonLayerConfig.GetLineConfig: IPolygonLayerConfig;
begin
  Result := FLineConfig;
end;

function TSelectionPolygonLayerConfig.GetPointsConfig: IPointsSetLayerConfig;
begin
  Result := FPointsConfig;
end;

end.

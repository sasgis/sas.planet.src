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

unit u_MarkPolygonLayerConfig;

interface

uses
  i_MarkPolygonLayerConfig,
  i_PolyLineLayerConfig,
  i_PolygonLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TMarkPolygonLayerConfig = class(TConfigDataElementComplexBase, IMarkPolygonLayerConfig)
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
  u_MarkerSimpleConfigStatic,
  u_PointsSetLayerConfig,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_PolygonLayerConfig;

{ TMarkPolygonLayerConfig }

constructor TMarkPolygonLayerConfig.Create;
var
  VFirstPointMarkerDefault: IMarkerSimpleConfigStatic;
  VActivePointMarkerDefault: IMarkerSimpleConfigStatic;
  VNormalPointMarkerDefault: IMarkerSimpleConfigStatic;
begin
  inherited Create;
  FLineConfig := TPolygonLayerConfig.Create;
  FLineConfig.LineColor := SetAlpha(ClRed32, 150);
  FLineConfig.LineWidth := 3;
  FLineConfig.FillColor := SetAlpha(ClWhite32, 50);
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

function TMarkPolygonLayerConfig.GetLineConfig: IPolygonLayerConfig;
begin
  Result := FLineConfig;
end;

function TMarkPolygonLayerConfig.GetPointsConfig: IPointsSetLayerConfig;
begin
  Result := FPointsConfig;
end;

end.

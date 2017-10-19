{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_CalcCircleLayerConfig;

interface

uses
  i_PolygonLayerConfig,
  i_PolyLineLayerConfig,
  i_PointCaptionsLayerConfig,
  i_CalcCircleLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TCalcCircleLayerConfig = class(TConfigDataElementComplexBase, ICalcCircleLayerConfig)
  private
    FPolygonConfig: IPolygonLayerConfig;
    FLineConfig: ILineLayerConfig;
    FPointsConfig: IPointsSetLayerConfig;
    FCaptionConfig: IPointCaptionsLayerConfig;
  private
    function GetPolygonConfig: IPolygonLayerConfig;
    function GetLineConfig: ILineLayerConfig;
    function GetPointsConfig: IPointsSetLayerConfig;
    function GetCaptionConfig: IPointCaptionsLayerConfig;
  public
    constructor Create;
  end;


implementation

uses
  GR32,
  i_MarkerSimpleConfig,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_PolygonLayerConfig,
  u_PointsSetLayerConfig,
  u_PolyLineLayerConfig,
  u_MarkerSimpleConfigStatic,
  u_PointCaptionsLayerConfig;

{ TCalcCircleLayerConfig }

constructor TCalcCircleLayerConfig.Create;
var
  VFirstPointMarkerDefault: IMarkerSimpleConfigStatic;
  VActivePointMarkerDefault: IMarkerSimpleConfigStatic;
  VNormalPointMarkerDefault: IMarkerSimpleConfigStatic;
begin
  inherited Create;

  FPolygonConfig := TPolygonLayerConfig.Create;
  FPolygonConfig.LineColor := SetAlpha(ClRed32, 150);
  FPolygonConfig.LineWidth := 3;
  FPolygonConfig.FillColor := SetAlpha(ClWhite32, 50);
  Add(FPolygonConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FLineConfig := TLineLayerConfig.Create;
  FLineConfig.LineColor := SetAlpha(ClRed32, 150);
  FLineConfig.LineWidth := 3;
  Add(FLineConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  VFirstPointMarkerDefault :=
    TMarkerSimpleConfigStatic.Create(
      6,
      SetAlpha(ClGreen32, 255),
      SetAlpha(ClRed32, 150)
    );

  VActivePointMarkerDefault :=
    TMarkerSimpleConfigStatic.Create(
      6,
      SetAlpha(ClRed32, 255),
      SetAlpha(ClRed32, 150)
    );

  VNormalPointMarkerDefault :=
    TMarkerSimpleConfigStatic.Create(
      6,
      SetAlpha(ClWhite32, 150),
      SetAlpha(ClRed32, 150)
    );

  FPointsConfig :=
    TPointsSetLayerConfig.Create(
      VFirstPointMarkerDefault,
      VActivePointMarkerDefault,
      VNormalPointMarkerDefault
    );
  Add(FPointsConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FCaptionConfig := TPointCaptionsLayerConfig.Create;
  Add(FCaptionConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TCalcCircleLayerConfig.GetCaptionConfig: IPointCaptionsLayerConfig;
begin
  Result := FCaptionConfig;
end;

function TCalcCircleLayerConfig.GetLineConfig: ILineLayerConfig;
begin
  Result := FLineConfig;
end;

function TCalcCircleLayerConfig.GetPointsConfig: IPointsSetLayerConfig;
begin
  Result := FPointsConfig;
end;

function TCalcCircleLayerConfig.GetPolygonConfig: IPolygonLayerConfig;
begin
  Result := FPolygonConfig;
end;

end.

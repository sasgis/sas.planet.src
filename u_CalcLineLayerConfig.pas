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

unit u_CalcLineLayerConfig;

interface

uses
  GR32,
  i_PolyLineLayerConfig,
  i_CalcLineLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TCalcLineLayerConfig = class(TConfigDataElementComplexBase, ICalcLineLayerConfig)
  private
    FLineConfig: ILineLayerConfig;
    FPointsConfig: IPointsSetLayerConfig;
    FCaptionConfig: ICalcLineLayerCaptionsConfig;
  protected
    function GetLineConfig: ILineLayerConfig;
    function GetPointsConfig: IPointsSetLayerConfig;
    function GetCaptionConfig: ICalcLineLayerCaptionsConfig;
  public
    constructor Create;
  end;


implementation

uses
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_PolyLineLayerConfig,
  u_CalcLineLayerCaptionsConfig;

{ TCalcLineLayerConfig }

constructor TCalcLineLayerConfig.Create;
begin
  inherited;

  FLineConfig := TLineLayerConfig.Create;
  FLineConfig.LineColor := SetAlpha(ClRed32, 150);
  FLineConfig.LineWidth := 3;
  Add(FLineConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FPointsConfig := TPointsSetLayerConfig.Create;
  FPointsConfig.PointFillColor := SetAlpha(ClWhite32, 150);
  FPointsConfig.PointRectColor := SetAlpha(ClRed32, 150);
  FPointsConfig.PointFirstColor := SetAlpha(ClGreen32, 255);
  FPointsConfig.PointActiveColor := SetAlpha(ClRed32, 255);
  FPointsConfig.PointSize := 6;
  Add(FPointsConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FCaptionConfig := TCalcLineLayerCaptionsConfig.Create;
  Add(FCaptionConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TCalcLineLayerConfig.GetCaptionConfig: ICalcLineLayerCaptionsConfig;
begin
  Result := FCaptionConfig;
end;

function TCalcLineLayerConfig.GetLineConfig: ILineLayerConfig;
begin
  Result := FLineConfig;
end;

function TCalcLineLayerConfig.GetPointsConfig: IPointsSetLayerConfig;
begin
  Result := FPointsConfig;
end;

end.

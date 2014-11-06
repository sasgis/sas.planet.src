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

unit u_PointsSetLayerConfig;

interface

uses
  i_MarkerSimpleConfig,
  i_PolyLineLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TPointsSetLayerConfig = class(TConfigDataElementComplexBase, IPointsSetLayerConfig)
  private
    FFirstPointMarker: IMarkerSimpleConfig;
    FActivePointMarker: IMarkerSimpleConfig;
    FNormalPointMarker: IMarkerSimpleConfig;
  private
    function GetFirstPointMarker: IMarkerSimpleConfig;
    function GetActivePointMarker: IMarkerSimpleConfig;
    function GetNormalPointMarker: IMarkerSimpleConfig;
  public
    constructor Create(
      const AFirstPointMarkerDefault: IMarkerSimpleConfigStatic;
      const AActivePointMarkerDefault: IMarkerSimpleConfigStatic;
      const ANormalPointMarkerDefault: IMarkerSimpleConfigStatic
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_MarkerSimpleConfig;

constructor TPointsSetLayerConfig.Create(
  const AFirstPointMarkerDefault: IMarkerSimpleConfigStatic;
  const AActivePointMarkerDefault: IMarkerSimpleConfigStatic;
  const ANormalPointMarkerDefault: IMarkerSimpleConfigStatic
);
begin
  inherited Create;
  FFirstPointMarker := TMarkerSimpleConfig.Create(AFirstPointMarkerDefault);
  Add(
    FFirstPointMarker,
    TConfigSaveLoadStrategyBasicProviderSubItem.Create('FirstPoint')
  );

  FActivePointMarker := TMarkerSimpleConfig.Create(AActivePointMarkerDefault);
  Add(
    FActivePointMarker,
    TConfigSaveLoadStrategyBasicProviderSubItem.Create('ActivePoint')
  );

  FNormalPointMarker := TMarkerSimpleConfig.Create(ANormalPointMarkerDefault);
  Add(
    FNormalPointMarker,
    TConfigSaveLoadStrategyBasicProviderSubItem.Create('NormalPoint')
  );
end;

function TPointsSetLayerConfig.GetActivePointMarker: IMarkerSimpleConfig;
begin
  Result := FActivePointMarker;
end;

function TPointsSetLayerConfig.GetFirstPointMarker: IMarkerSimpleConfig;
begin
  Result := FFirstPointMarker;
end;

function TPointsSetLayerConfig.GetNormalPointMarker: IMarkerSimpleConfig;
begin
  Result := FNormalPointMarker;
end;

end.

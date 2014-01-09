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

unit u_KmlLayerConfig;

interface

uses
  i_ThreadConfig,
  i_VectorItemDrawConfig,
  i_MarkerSimpleConfig,
  i_KmlLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TKmlLayerConfig = class(TConfigDataElementComplexBase, IKmlLayerConfig)
  private
    FDrawConfig: IVectorItemDrawConfig;
    FPointMarkerConfig: IMarkerSimpleConfig;
    FThreadConfig: IThreadConfig;
  private
    function GetDrawConfig: IVectorItemDrawConfig;
    function GetPointMarkerConfig: IMarkerSimpleConfig;
    function GetThreadConfig: IThreadConfig;
  public
    constructor Create;
  end;

implementation

uses
  GR32,
  Classes,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_ThreadConfig,
  u_MarkerSimpleConfig,
  u_MarkerSimpleConfigStatic,
  u_VectorItemDrawConfig;

{ TKmlLayerConfig }

constructor TKmlLayerConfig.Create;
var
  VPointMarkerDefault: IMarkerSimpleConfigStatic;
begin
  inherited Create;
  FDrawConfig := TVectorItemDrawConfig.Create;
  Add(FDrawConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  VPointMarkerDefault :=
    TMarkerSimpleConfigStatic.Create(
      6,
      SetAlpha(clWhite32, 170),
      clBlack32
    );

  FPointMarkerConfig := TMarkerSimpleConfig.Create(VPointMarkerDefault);
  Add(FPointMarkerConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FThreadConfig := TThreadConfig.Create(tpLowest);
  Add(FThreadConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TKmlLayerConfig.GetDrawConfig: IVectorItemDrawConfig;
begin
  Result := FDrawConfig;
end;

function TKmlLayerConfig.GetPointMarkerConfig: IMarkerSimpleConfig;
begin
  Result := FPointMarkerConfig;
end;

function TKmlLayerConfig.GetThreadConfig: IThreadConfig;
begin
  Result := FThreadConfig;
end;

end.

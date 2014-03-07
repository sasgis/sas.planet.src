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

unit u_MarksLayerConfig;

interface

uses
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarksLayerConfig,
  i_ThreadConfig,
  u_ConfigDataElementComplexBase;

type
  TMarksLayerConfig = class(TConfigDataElementComplexBase, IMarksLayerConfig)
  private
    FMarksShowConfig: IUsedMarksConfig;
    FMarksDrawConfig: IMarksDrawConfig;
    FThreadConfig: IThreadConfig;
  private
    function GetMarksShowConfig: IUsedMarksConfig;
    function GetMarksDrawConfig: IMarksDrawConfig;
    function GetThreadConfig: IThreadConfig;
  public
    constructor Create;
  end;

implementation

uses
  Classes,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_ThreadConfig,
  u_UsedMarksConfig,
  u_MarksDrawConfig;

{ TMainFormLayersConfig }

constructor TMarksLayerConfig.Create;
begin
  inherited Create;

  FMarksShowConfig := TUsedMarksConfig.Create;
  Add(FMarksShowConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FMarksDrawConfig := TMarksDrawConfig.Create;
  Add(FMarksDrawConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FThreadConfig := TThreadConfig.Create(tpLower);
  Add(FThreadConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TMarksLayerConfig.GetMarksDrawConfig: IMarksDrawConfig;
begin
  Result := FMarksDrawConfig;
end;

function TMarksLayerConfig.GetMarksShowConfig: IUsedMarksConfig;
begin
  Result := FMarksShowConfig;
end;

function TMarksLayerConfig.GetThreadConfig: IThreadConfig;
begin
  Result := FThreadConfig;
end;

end.

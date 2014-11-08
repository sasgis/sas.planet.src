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

unit u_MapLayerGPSMarkerConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapLayerGPSMarkerConfig,
  i_MarkerRingsConfig,
  i_MarkerSimpleConfig,
  u_ConfigDataElementComplexBase;

type
  TMapLayerGPSMarkerConfig = class(TConfigDataElementComplexBase, IMapLayerGPSMarkerConfig)
  private
    FMinMoveSpeed: Double;
    FMovedMarkerConfig: IMarkerSimpleConfig;
    FStopedMarkerConfig: IMarkerSimpleConfig;
    FMarkerRingsConfig: IMarkerRingsConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMinMoveSpeed: Double;
    procedure SetMinMoveSpeed(AValue: Double);

    function GetMovedMarkerConfig: IMarkerSimpleConfig;
    function GetStopedMarkerConfig: IMarkerSimpleConfig;
    function GetMarkerRingsConfig: IMarkerRingsConfig;
  public
    constructor Create;
  end;

implementation

uses
  GR32,
  u_MarkerSimpleConfig,
  u_MarkerSimpleConfigStatic,
  u_MarkerRingsConfig,
  u_ConfigSaveLoadStrategyBasicProviderSubItem;

{ TMapLayerGPSMarkerConfig }

constructor TMapLayerGPSMarkerConfig.Create;
var
  VMarkerProvider: IMarkerSimpleConfigStatic;
begin
  inherited Create;
  FMinMoveSpeed := 1;

  VMarkerProvider :=
    TMarkerSimpleConfigStatic.Create(
      25,
      SetAlpha(clRed32, 150),
      SetAlpha(clBlack32, 200)
    );
  FMovedMarkerConfig := TMarkerSimpleConfig.Create(VMarkerProvider);
  Add(FMovedMarkerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkerMoved'));

  VMarkerProvider :=
    TMarkerSimpleConfigStatic.Create(
      10,
      SetAlpha(clRed32, 200),
      SetAlpha(clBlack32, 200)
    );
  FStopedMarkerConfig := TMarkerSimpleConfig.Create(VMarkerProvider);
  Add(FStopedMarkerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkerStoped'));

  FMarkerRingsConfig := TMarkerRingsConfig.Create;
  Add(FMarkerRingsConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Rings'));
end;

procedure TMapLayerGPSMarkerConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FMinMoveSpeed := AConfigData.ReadFloat('MinSpeed', FMinMoveSpeed);
    SetChanged;
  end;
end;

procedure TMapLayerGPSMarkerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteFloat('MinSpeed', FMinMoveSpeed);
end;

function TMapLayerGPSMarkerConfig.GetMarkerRingsConfig: IMarkerRingsConfig;
begin
  Result := FMarkerRingsConfig;
end;

function TMapLayerGPSMarkerConfig.GetMinMoveSpeed: Double;
begin
  LockRead;
  try
    Result := FMinMoveSpeed;
  finally
    UnlockRead;
  end;
end;

function TMapLayerGPSMarkerConfig.GetMovedMarkerConfig: IMarkerSimpleConfig;
begin
  Result := FMovedMarkerConfig;
end;

function TMapLayerGPSMarkerConfig.GetStopedMarkerConfig: IMarkerSimpleConfig;
begin
  Result := FStopedMarkerConfig;
end;

procedure TMapLayerGPSMarkerConfig.SetMinMoveSpeed(AValue: Double);
begin
  LockWrite;
  try
    if FMinMoveSpeed <> AValue then begin
      FMinMoveSpeed := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

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

unit u_MapLayerGPSMarkerConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapLayerGPSMarkerConfig,
  i_MarkerSimpleConfig,
  u_ConfigDataElementComplexBase;

type
  TMapLayerGPSMarkerConfig = class(TConfigDataElementComplexBase, IMapLayerGPSMarkerConfig)
  private
    FMinMoveSpeed: Double;
    FMovedMarkerConfig: IMarkerSimpleConfig;
    FStopedMarkerConfig: IMarkerSimpleConfig;

  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMinMoveSpeed: Double;
    procedure SetMinMoveSpeed(AValue: Double);

    function GetMovedMarkerConfig: IMarkerSimpleConfig;
    function GetStopedMarkerConfig: IMarkerSimpleConfig;
  public
    constructor Create;
  end;

implementation

uses
  u_MarkerSimpleConfig,
  u_MarkerSimpleConfigStatic,
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





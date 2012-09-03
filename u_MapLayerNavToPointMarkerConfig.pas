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

unit u_MapLayerNavToPointMarkerConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapLayerNavToPointMarkerConfig,
  i_MarkerSimpleConfig,
  u_ConfigDataElementComplexBase;

type
  TMapLayerNavToPointMarkerConfig = class(TConfigDataElementComplexBase, IMapLayerNavToPointMarkerConfig)
  private
    FCrossDistInPixels: Double;
    FArrowMarkerConfig: IMarkerSimpleConfig;
    FReachedMarkerConfig: IMarkerSimpleConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetCrossDistInPixels: Double;
    procedure SetCrossDistInPixels(AValue: Double);

    function GetArrowMarkerConfig: IMarkerSimpleConfig;
    function GetReachedMarkerConfig: IMarkerSimpleConfig;
  public
    constructor Create;
  end;

implementation

uses
  u_MarkerSimpleConfig,
  u_MarkerSimpleConfigStatic,
  u_ConfigSaveLoadStrategyBasicProviderSubItem;

{ TMapLayerGPSMarkerConfig }

constructor TMapLayerNavToPointMarkerConfig.Create;
var
  VMarkerProvider: IMarkerSimpleConfigStatic;
begin
  inherited Create;
  FCrossDistInPixels := 100;

  VMarkerProvider :=
    TMarkerSimpleConfigStatic.Create(
      25,
      SetAlpha(clRed32, 150),
      SetAlpha(clBlack32, 200)
    );
  FArrowMarkerConfig := TMarkerSimpleConfig.Create(VMarkerProvider);
  Add(FArrowMarkerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('ArrowMarker'));

  VMarkerProvider :=
    TMarkerSimpleConfigStatic.Create(
      20,
      SetAlpha(clRed32, 200),
      SetAlpha(clBlack32, 200)
    );
  FReachedMarkerConfig := TMarkerSimpleConfig.Create(VMarkerProvider);
  Add(FReachedMarkerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('ReachedPointMarker'));
end;

procedure TMapLayerNavToPointMarkerConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FCrossDistInPixels := AConfigData.ReadFloat('CrossDistInPixels', FCrossDistInPixels);
    SetChanged;
  end;
end;

procedure TMapLayerNavToPointMarkerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteFloat('CrossDistInPixels', FCrossDistInPixels);
end;

function TMapLayerNavToPointMarkerConfig.GetArrowMarkerConfig: IMarkerSimpleConfig;
begin
  Result := FArrowMarkerConfig;
end;

function TMapLayerNavToPointMarkerConfig.GetReachedMarkerConfig: IMarkerSimpleConfig;
begin
  Result := FReachedMarkerConfig;
end;

function TMapLayerNavToPointMarkerConfig.GetCrossDistInPixels: Double;
begin
  LockRead;
  try
    Result := FCrossDistInPixels;
  finally
    UnlockRead;
  end;
end;

procedure TMapLayerNavToPointMarkerConfig.SetCrossDistInPixels(AValue: Double);
begin
  LockWrite;
  try
    if FCrossDistInPixels <> AValue then begin
      FCrossDistInPixels := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

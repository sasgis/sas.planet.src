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
  i_BitmapMarkerProviderSimpleConfig,
  u_ConfigDataElementComplexBase;

type
  TMapLayerGPSMarkerConfig = class(TConfigDataElementComplexBase, IMapLayerGPSMarkerConfig)
  private
    FMinMoveSpeed: Double;
    FMovedMarkerConfig: IBitmapMarkerProviderSimpleConfig;
    FStopedMarkerConfig: IBitmapMarkerProviderSimpleConfig;

  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetMinMoveSpeed: Double;
    procedure SetMinMoveSpeed(AValue: Double);

    function GetMovedMarkerConfig: IBitmapMarkerProviderSimpleConfig;
    function GetStopedMarkerConfig: IBitmapMarkerProviderSimpleConfig;
  public
    constructor Create;
  end;

implementation

uses
  u_BitmapMarkerProviderSimpleConfig,
  u_BitmapMarkerProviderSimpleConfigStatic,
  u_ConfigSaveLoadStrategyBasicProviderSubItem;

{ TMapLayerGPSMarkerConfig }

constructor TMapLayerGPSMarkerConfig.Create;
begin
  inherited;
  FMinMoveSpeed := 1;

  FMovedMarkerConfig :=
    TBitmapMarkerProviderSimpleConfig.Create(
      TBitmapMarkerProviderSimpleConfigStatic.Create(
        25,
        SetAlpha(clRed32, 150),
        SetAlpha(clBlack32, 200)
      )
    );
  Add(FMovedMarkerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkerMoved'));

  FStopedMarkerConfig :=
    TBitmapMarkerProviderSimpleConfig.Create(
      TBitmapMarkerProviderSimpleConfigStatic.Create(
        10,
        SetAlpha(clRed32, 200),
        SetAlpha(clBlack32, 200)
      )
    );
  Add(FStopedMarkerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarkerStoped'));
end;

procedure TMapLayerGPSMarkerConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FMinMoveSpeed := AConfigData.ReadFloat('MinSpeed', FMinMoveSpeed);
    SetChanged;
  end;
end;

procedure TMapLayerGPSMarkerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
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

function TMapLayerGPSMarkerConfig.GetMovedMarkerConfig: IBitmapMarkerProviderSimpleConfig;
begin
  Result := FMovedMarkerConfig;
end;

function TMapLayerGPSMarkerConfig.GetStopedMarkerConfig: IBitmapMarkerProviderSimpleConfig;
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

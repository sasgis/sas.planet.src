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

unit u_MiniMapLayerConfig;

interface

uses
  i_Bitmap32Static,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ContentTypeManager,
  i_ThreadConfig,
  i_MiniMapLayerConfig,
  i_ActiveMapsConfig,
  u_ConfigDataElementComplexBase;

type
  TMiniMapLayerConfig = class(TConfigDataElementComplexBase, IMiniMapLayerConfig)
  private
    FWidth: Integer;
    FZoomDelta: Integer;
    FMasterAlpha: Integer;
    FVisible: Boolean;
    FBottomMargin: Integer;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;

    FMapsConfig: IMiniMapMapsConfig;
    FThreadConfig: IThreadConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);

    function GetZoomDelta: Integer;
    procedure SetZoomDelta(AValue: Integer);

    function GetMasterAlpha: Integer;
    procedure SetMasterAlpha(AValue: Integer);

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetBottomMargin: Integer;
    procedure SetBottomMargin(AValue: Integer);

    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(const AValue: Boolean);

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(const AValue: Boolean);

    function GetMapsConfig: IMiniMapMapsConfig;
    function GetThreadConfig: IThreadConfig;
  public
    constructor Create(
      const AMapsConfig: IMainMapsConfig
    );
  end;

implementation

uses
  Classes,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigProviderHelpers,
  u_ThreadConfig,
  u_MiniMapMapsConfig;

{ TMiniMapLayerConfig }

constructor TMiniMapLayerConfig.Create(
  const AMapsConfig: IMainMapsConfig
);
begin
  inherited Create;
  FWidth := 100;
  FZoomDelta := 4;
  FMasterAlpha := 150;
  FVisible := True;
  FUsePrevZoomAtMap := True;
  FUsePrevZoomAtLayer := True;

  FMapsConfig := TMiniMapMapsConfig.Create(AMapsConfig);
  Add(FMapsConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Maps'));

  FThreadConfig := TThreadConfig.Create(tpLower);
  Add(FThreadConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

procedure TMiniMapLayerConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetWidth(AConfigData.ReadInteger('Width', FWidth));
    SetZoomDelta(AConfigData.ReadInteger('ZoomDelta', FZoomDelta));
    SetMasterAlpha(AConfigData.ReadInteger('Alpha', FMasterAlpha));
    SetVisible(AConfigData.ReadBool('Visible', FVisible));
    SetUsePrevZoomAtMap(AConfigData.ReadBool('UsePrevZoomAtMap', FUsePrevZoomAtMap));
    SetUsePrevZoomAtLayer(AConfigData.ReadBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer));

    SetChanged;
  end;
end;

procedure TMiniMapLayerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger('Width', FWidth);
  AConfigData.WriteInteger('ZoomDelta', FZoomDelta);
  AConfigData.WriteInteger('Alpha', FMasterAlpha);
  AConfigData.WriteBool('Visible', FVisible);
  AConfigData.WriteBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
  AConfigData.WriteBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
end;

function TMiniMapLayerConfig.GetBottomMargin: Integer;
begin
  LockRead;
  try
    Result := FBottomMargin;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetMapsConfig: IMiniMapMapsConfig;
begin
  Result := FMapsConfig;
end;

function TMiniMapLayerConfig.GetMasterAlpha: Integer;
begin
  LockRead;
  try
    Result := FMasterAlpha;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetThreadConfig: IThreadConfig;
begin
  Result := FThreadConfig;
end;

function TMiniMapLayerConfig.GetUsePrevZoomAtLayer: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtLayer;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetUsePrevZoomAtMap: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtMap;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetWidth: Integer;
begin
  LockRead;
  try
    Result := FWidth;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetZoomDelta: Integer;
begin
  LockRead;
  try
    Result := FZoomDelta;
  finally
    UnlockRead;
  end;
end;

procedure TMiniMapLayerConfig.SetBottomMargin(AValue: Integer);
begin
  LockWrite;
  try
    if FBottomMargin <> AValue then begin
      FBottomMargin := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMiniMapLayerConfig.SetMasterAlpha(AValue: Integer);
begin
  LockWrite;
  try
    if FMasterAlpha <> AValue then begin
      FMasterAlpha := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMiniMapLayerConfig.SetUsePrevZoomAtLayer(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtLayer <> AValue then begin
      FUsePrevZoomAtLayer := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMiniMapLayerConfig.SetUsePrevZoomAtMap(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtMap <> AValue then begin
      FUsePrevZoomAtMap := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMiniMapLayerConfig.SetVisible(AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMiniMapLayerConfig.SetWidth(AValue: Integer);
begin
  LockWrite;
  try
    if FWidth <> AValue then begin
      FWidth := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMiniMapLayerConfig.SetZoomDelta(AValue: Integer);
var
  VZoomDelta: Integer;
begin
  VZoomDelta := AValue;
  if VZoomDelta > 10 then begin
    VZoomDelta := 10;
  end else begin
    if VZoomDelta < -2 then begin
      VZoomDelta := -2;
    end;
  end;
  LockWrite;
  try
    if FZoomDelta <> VZoomDelta then begin
      FZoomDelta := VZoomDelta;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

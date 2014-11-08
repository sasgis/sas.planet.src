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

unit u_MiniMapLayerConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ThreadConfig,
  i_MiniMapLayerConfig,
  i_ActiveMapsConfig,
  i_MapType,
  i_MapTypeSet,
  i_MapTypeSetBuilder,
  i_UseTilePrevZoomConfig,
  u_ConfigDataElementComplexBase;

type
  TMiniMapLayerConfig = class(TConfigDataElementComplexBase, IMiniMapLayerConfig)
  private
    FMasterAlpha: Integer;

    FLocationConfig: IMiniMapLayerLocationConfig;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FMapsConfig: IActivMapWithLayers;
    FThreadConfig: IThreadConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMasterAlpha: Integer;
    procedure SetMasterAlpha(AValue: Integer);

    function GetLocationConfig: IMiniMapLayerLocationConfig;
    function GetUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    function GetMapsConfig: IActivMapWithLayers;
    function GetThreadConfig: IThreadConfig;
  public
    constructor Create(
      const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
      const AMainMap: IMapTypeChangeable;
      const AMapsSet: IMapTypeSet;
      const ALayersSet: IMapTypeSet
    );
  end;

implementation

uses
  Classes,
  u_BaseInterfacedObject,
  u_ConfigDataElementBase,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ThreadConfig,
  u_UseTilePrevZoomConfig,
  u_MiniMapMapsConfig;

type
  TMiniMapLayerLocationConfigStatic = class(TBaseInterfacedObject, IMiniMapLayerLocationConfigStatic)
  private
    FVisible: Boolean;
    FWidth: Integer;
    FBottomMargin: Integer;
    FZoomDelta: Integer;
  private
    function GetVisible: Boolean;
    function GetWidth: Integer;
    function GetBottomMargin: Integer;
    function GetZoomDelta: Integer;
  public
    constructor Create(
      const AVisible: Boolean;
      const AWidth: Integer;
      const ABottomMargin: Integer;
      const AZoomDelta: Integer
    );
  end;

{ TMiniMapLayerLocationConfigStatic }

constructor TMiniMapLayerLocationConfigStatic.Create(
  const AVisible: Boolean;
  const AWidth, ABottomMargin, AZoomDelta: Integer
);
begin
  inherited Create;
  FVisible := AVisible;
  FWidth := AWidth;
  FBottomMargin := ABottomMargin;
  FZoomDelta := AZoomDelta;
end;

function TMiniMapLayerLocationConfigStatic.GetBottomMargin: Integer;
begin
  Result := FBottomMargin;
end;

function TMiniMapLayerLocationConfigStatic.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TMiniMapLayerLocationConfigStatic.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TMiniMapLayerLocationConfigStatic.GetZoomDelta: Integer;
begin
  Result := FZoomDelta;
end;

type
  TMiniMapLayerLocationConfig = class(TConfigDataElementWithStaticBase, IMiniMapLayerLocationConfig)
  private
    FVisible: Boolean;
    FWidth: Integer;
    FBottomMargin: Integer;
    FZoomDelta: Integer;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);

    function GetBottomMargin: Integer;
    procedure SetBottomMargin(AValue: Integer);

    function GetZoomDelta: Integer;
    procedure SetZoomDelta(AValue: Integer);

    function GetStatic: IMiniMapLayerLocationConfigStatic;
  public
    constructor Create;
  end;

{ TMiniMapLayerLocationConfig }

constructor TMiniMapLayerLocationConfig.Create;
begin
  inherited Create;
  FWidth := 100;
  FZoomDelta := 4;
  FVisible := True;
end;

function TMiniMapLayerLocationConfig.CreateStatic: IInterface;
var
  VStatic: IMiniMapLayerLocationConfigStatic;
begin
  VStatic :=
    TMiniMapLayerLocationConfigStatic.Create(
      FVisible,
      FWidth,
      FBottomMargin,
      FZoomDelta
    );
  Result := VStatic;
end;

procedure TMiniMapLayerLocationConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetWidth(AConfigData.ReadInteger('Width', FWidth));
    SetZoomDelta(AConfigData.ReadInteger('ZoomDelta', FZoomDelta));
    SetVisible(AConfigData.ReadBool('Visible', FVisible));
  end;
end;

procedure TMiniMapLayerLocationConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('Width', FWidth);
  AConfigData.WriteInteger('ZoomDelta', FZoomDelta);
  AConfigData.WriteBool('Visible', FVisible);
end;

function TMiniMapLayerLocationConfig.GetStatic: IMiniMapLayerLocationConfigStatic;
begin
  Result := IMiniMapLayerLocationConfigStatic(GetStaticInternal);
end;

function TMiniMapLayerLocationConfig.GetBottomMargin: Integer;
begin
  LockRead;
  try
    Result := FBottomMargin;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerLocationConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerLocationConfig.GetWidth: Integer;
begin
  LockRead;
  try
    Result := FWidth;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerLocationConfig.GetZoomDelta: Integer;
begin
  LockRead;
  try
    Result := FZoomDelta;
  finally
    UnlockRead;
  end;
end;

procedure TMiniMapLayerLocationConfig.SetBottomMargin(AValue: Integer);
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

procedure TMiniMapLayerLocationConfig.SetVisible(AValue: Boolean);
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

procedure TMiniMapLayerLocationConfig.SetWidth(AValue: Integer);
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

procedure TMiniMapLayerLocationConfig.SetZoomDelta(AValue: Integer);
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

{ TMiniMapLayerConfig }

constructor TMiniMapLayerConfig.Create(
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const AMainMap: IMapTypeChangeable;
  const AMapsSet: IMapTypeSet;
  const ALayersSet: IMapTypeSet
);
begin
  inherited Create;
  FMasterAlpha := 150;

  FUseTilePrevZoomConfig := TUseTilePrevZoomConfig.Create;
  Add(FUseTilePrevZoomConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FLocationConfig := TMiniMapLayerLocationConfig.Create;
  Add(FLocationConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FMapsConfig :=
    TMiniMapMapsConfig.Create(
      AMapTypeSetBuilderFactory,
      AMainMap,
      AMapsSet,
      ALayersSet
    );
  Add(FMapsConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Maps'));

  FThreadConfig := TThreadConfig.Create(tpLower);
  Add(FThreadConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

procedure TMiniMapLayerConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetMasterAlpha(AConfigData.ReadInteger('Alpha', FMasterAlpha));

    SetChanged;
  end;
end;

procedure TMiniMapLayerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger('Alpha', FMasterAlpha);
end;

function TMiniMapLayerConfig.GetLocationConfig: IMiniMapLayerLocationConfig;
begin
  Result := FLocationConfig;
end;

function TMiniMapLayerConfig.GetMapsConfig: IActivMapWithLayers;
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

function TMiniMapLayerConfig.GetUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
begin
  Result := FUseTilePrevZoomConfig;
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

end.

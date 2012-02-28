{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  i_MiniMapLayerConfig,
  i_ActiveMapsConfig,
  u_ConfigDataElementComplexBase;

type
  TMiniMapLayerConfig = class(TConfigDataElementComplexBase, IMiniMapLayerConfig)
  private
    FContentTypeManager: IContentTypeManager;

    FWidth: Integer;
    FZoomDelta: Integer;
    FMasterAlpha: Integer;
    FVisible: Boolean;
    FBottomMargin: Integer;

    FPlusButtonFileName: string;
    FPlusButton: IBitmap32Static;
    FMinusButtonFileName: string;
    FMinusButton: IBitmap32Static;
    FMapsConfig: IMiniMapMapsConfig;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
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

    function GetPlusButton: IBitmap32Static;
    function GetMinusButton: IBitmap32Static;

    function GetMapsConfig: IMiniMapMapsConfig;
  public
    constructor Create(
      AContentTypeManager: IContentTypeManager;
      AMapsConfig: IMainMapsConfig
    );
  end;

implementation

uses
  SysUtils,
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigProviderHelpers,
  u_MiniMapMapsConfig;

{ TMiniMapLayerConfig }

constructor TMiniMapLayerConfig.Create(
  AContentTypeManager: IContentTypeManager;
  AMapsConfig: IMainMapsConfig
);
begin
  inherited Create;
  FContentTypeManager := AContentTypeManager;
  FWidth := 100;
  FZoomDelta := 4;
  FMasterAlpha := 150;
  FVisible := True;
  FPlusButton := nil;
  FMinusButton := nil;

  FMapsConfig := TMiniMapMapsConfig.Create(AMapsConfig);
  Add(FMapsConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Maps'));

  FPlusButtonFileName := 'sas:\Resource\ICONI.png';
  FMinusButtonFileName := 'sas:\Resource\ICONII.png';
end;

procedure TMiniMapLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetWidth(AConfigData.ReadInteger('Width', FWidth));
    SetZoomDelta(AConfigData.ReadInteger('ZoomDelta', FZoomDelta));
    SetMasterAlpha(AConfigData.ReadInteger('Alpha', FMasterAlpha));
    SetVisible(AConfigData.ReadBool('Visible', FVisible));

    FPlusButton := ReadBitmapByFileRef(AConfigData, FPlusButtonFileName, FContentTypeManager, FPlusButton);
    FMinusButton := ReadBitmapByFileRef(AConfigData, FMinusButtonFileName, FContentTypeManager, FMinusButton);
    SetChanged;
  end;
end;

procedure TMiniMapLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('Width', FWidth);
  AConfigData.WriteInteger('ZoomDelta', FZoomDelta);
  AConfigData.WriteInteger('Alpha', FMasterAlpha);
  AConfigData.WriteBool('Visible', FVisible);
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

function TMiniMapLayerConfig.GetMinusButton: IBitmap32Static;
begin
  LockRead;
  try
    Result := FMinusButton;
  finally
    UnlockRead;
  end;
end;

function TMiniMapLayerConfig.GetPlusButton: IBitmap32Static;
begin
  LockRead;
  try
    Result := FPlusButton;
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

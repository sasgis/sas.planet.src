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

unit u_MapTypeGUIConfig;

interface

uses
  Classes,
  i_Bitmap32Static,
  i_StringConfigDataElement,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LanguageManager,
  i_MapTypeGUIConfig,
  i_ZmpInfo,
  u_ConfigDataElementComplexBase;

type
  TMapTypeGUIConfig = class(TConfigDataElementComplexWithStaticBase, IMapTypeGUIConfig)
  private
    FDefConfig: IZmpInfoGUI;
    FName: IStringConfigDataElement;
    FSortIndex: Integer;
    FHotKey: TShortCut;
    FSeparator: Boolean;
    FParentSubMenu: IStringConfigDataElement;
    FEnabled: Boolean;
    FInfoUrl: IStringConfigDataElement;
  protected
    function CreateStatic: IInterface; override;
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetName: IStringConfigDataElement;

    function GetSortIndex: Integer;
    procedure SetSortIndex(const AValue: Integer);

    function GetHotKey: TShortCut;
    procedure SetHotKey(const AValue: TShortCut);

    function GetSeparator: Boolean;
    procedure SetSeparator(const AValue: Boolean);

    function GetParentSubMenu: IStringConfigDataElement;

    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);

    function GetInfoUrl: IStringConfigDataElement;

    function GetBmp18: IBitmap32Static;
    function GetBmp24: IBitmap32Static;

    function GetStatic: IMapTypeGUIConfigStatic;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ADefConfig: IZmpInfoGUI
    );
  end;

implementation

uses
  u_StringConfigDataElementWithLanguage,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_MapTypeGUIConfigStatic;

{ TMapTypeGUIConfig }

constructor TMapTypeGUIConfig.Create(
  const ALanguageManager: ILanguageManager;
  const ADefConfig: IZmpInfoGUI
);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FName :=
    TStringConfigDataElementWithLanguage.Create(
      ALanguageManager,
      True,
      'Name',
      True,
      FDefConfig.Name
    );
  Add(FName, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FParentSubMenu :=
    TStringConfigDataElementWithLanguage.Create(
      ALanguageManager,
      True,
      'ParentSubMenu',
      False,
      FDefConfig.ParentSubMenu
    );
  Add(FParentSubMenu, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FInfoUrl :=
    TStringConfigDataElementWithLanguage.Create(
      ALanguageManager,
      False,
      'InfoUrl',
      False,
      FDefConfig.InfoUrl
    );
  Add(FInfoUrl, nil);

  FSortIndex := FDefConfig.SortIndex;
  if FSortIndex < 0 then begin
    FSortIndex := 1000;
  end;
  FHotKey := FDefConfig.HotKey;
  FSeparator := FDefConfig.Separator;
  FEnabled := FDefConfig.Enabled;
end;

function TMapTypeGUIConfig.CreateStatic: IInterface;
var
  VStatic: IMapTypeGUIConfigStatic;
begin
  VStatic :=
    TMapTypeGUIConfigStatic.Create(
      FName.Value,
      FSortIndex,
      FHotKey,
      FSeparator,
      FParentSubMenu.Value,
      FEnabled,
      FInfoUrl.Value,
      FDefConfig.Bmp18,
      FDefConfig.Bmp24
    );
  Result := VStatic;
end;

procedure TMapTypeGUIConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FHotKey := AConfigData.ReadInteger('HotKey', FHotKey);
    FSeparator := AConfigData.ReadBool('separator', FSeparator);
    FEnabled := AConfigData.ReadBool('Enabled', FEnabled);
    FSortIndex := AConfigData.ReadInteger('pnum', FSortIndex);
    SetChanged;
  end;
end;

procedure TMapTypeGUIConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  if FSeparator <> FDefConfig.Separator then begin
    AConfigData.WriteBool('Separator', FSeparator);
  end else begin
    AConfigData.DeleteValue('Separator');
  end;
  if FEnabled <> FDefConfig.Enabled then begin
    AConfigData.WriteBool('Enabled', FEnabled);
  end else begin
    AConfigData.DeleteValue('Enabled');
  end;
  if FSortIndex <> FDefConfig.SortIndex then begin
    AConfigData.WriteInteger('pnum', FSortIndex);
  end else begin
    AConfigData.DeleteValue('pnum');
  end;
  if FHotKey <> FDefConfig.HotKey then begin
    AConfigData.WriteInteger('HotKey', FHotKey);
  end else begin
    AConfigData.DeleteValue('HotKey');
  end;
end;

function TMapTypeGUIConfig.GetBmp18: IBitmap32Static;
begin
  Result := FDefConfig.Bmp18;
end;

function TMapTypeGUIConfig.GetBmp24: IBitmap32Static;
begin
  Result := FDefConfig.Bmp24;
end;

function TMapTypeGUIConfig.GetEnabled: Boolean;
begin
  LockRead;
  try
    Result := FEnabled;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGUIConfig.GetHotKey: TShortCut;
begin
  LockRead;
  try
    Result := FHotKey;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGUIConfig.GetInfoUrl: IStringConfigDataElement;
begin
  Result := FInfoUrl;
end;

function TMapTypeGUIConfig.GetName: IStringConfigDataElement;
begin
  Result := FName;
end;

function TMapTypeGUIConfig.GetParentSubMenu: IStringConfigDataElement;
begin
  Result := FParentSubMenu;
end;

function TMapTypeGUIConfig.GetSeparator: Boolean;
begin
  LockRead;
  try
    Result := FSeparator;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGUIConfig.GetSortIndex: Integer;
begin
  LockRead;
  try
    Result := FSortIndex;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGUIConfig.GetStatic: IMapTypeGUIConfigStatic;
begin
  Result := IMapTypeGUIConfigStatic(GetStaticInternal);
end;

procedure TMapTypeGUIConfig.SetEnabled(const AValue: Boolean);
begin
  LockWrite;
  try
    if FEnabled <> AValue then begin
      FEnabled := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGUIConfig.SetHotKey(const AValue: TShortCut);
begin
  LockWrite;
  try
    if FHotKey <> AValue then begin
      FHotKey := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGUIConfig.SetSeparator(const AValue: Boolean);
begin
  LockWrite;
  try
    if FSeparator <> AValue then begin
      FSeparator := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGUIConfig.SetSortIndex(const AValue: Integer);
begin
  LockWrite;
  try
    if FSortIndex <> AValue then begin
      FSortIndex := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

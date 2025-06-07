{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MapTypeGuiConfigProxy;

interface

uses
  Classes,
  i_Listener,
  i_StringConfigDataElement,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LanguageManager,
  i_MapTypeGUIConfig,
  i_ZmpInfo,
  u_ConfigDataElementComplexBase;

type
  TMapTypeGuiConfigProxy = class(TConfigDataElementComplexWithStaticBase, IMapTypeGUIConfig, IMapTypeGuiConfigProxy)
  private
    FLanguageManager: ILanguageManager;
    FZmpInfoGui: IZmpInfoGuiProxy;
    FGuiConfig: IMapTypeGUIConfig;
    FName: IStringConfigDataElement;
    FParentSubMenu: IStringConfigDataElement;
    FInfoUrl: IStringConfigDataElement;
    FListener: IListener;
    procedure OnGuiConfigChange;
  protected
    function CreateStatic: IInterface; override;
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IMapTypeGUIConfig }
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

    function GetStatic: IMapTypeGUIConfigStatic;

    { IMapTypeGuiConfigProxy }
    procedure Initialize;
    procedure Reset;

    function GetIsInitialized: Boolean; inline;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AZmpInfoGui: IZmpInfoGuiProxy
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_StringConfigDataElementWithLanguage,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_MapTypeGUIConfig,
  u_MapTypeGUIConfigStatic;

{ TMapTypeGuiConfigProxy }

constructor TMapTypeGuiConfigProxy.Create(
  const ALanguageManager: ILanguageManager;
  const AZmpInfoGui: IZmpInfoGuiProxy
);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
  FZmpInfoGui := AZmpInfoGui;
  FName := TStringConfigDataElementWithLanguage.Create(ALanguageManager, False, '', False, FZmpInfoGui.Name);
  FParentSubMenu := FName;
  FInfoUrl := FName;
  FListener := TNotifyNoMmgEventListener.Create(Self.OnGuiConfigChange);
end;

destructor TMapTypeGuiConfigProxy.Destroy;
begin
  if (FGuiConfig <> nil) and (FListener <> nil) then begin
    FGuiConfig.ChangeNotifier.Remove(FListener);
  end;
  inherited Destroy;
end;

procedure TMapTypeGuiConfigProxy.Initialize;
begin
  LockWrite;
  try
    if FGuiConfig <> nil then begin
      FGuiConfig.ChangeNotifier.Remove(FListener);
    end;
    FGuiConfig := TMapTypeGuiConfig.Create(FLanguageManager, FZmpInfoGui);
    FGuiConfig.ChangeNotifier.Add(FListener);
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGuiConfigProxy.Reset;
begin
  LockWrite;
  try
    if (FGuiConfig <> nil) and (FListener <> nil) then begin
      FGuiConfig.ChangeNotifier.Remove(FListener);
    end;
    FGuiConfig := nil;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGuiConfigProxy.OnGuiConfigChange;
begin
  SetChanged;
end;

function TMapTypeGuiConfigProxy.GetIsInitialized: Boolean;
begin
  Result := FGuiConfig <> nil;
end;

function TMapTypeGuiConfigProxy.CreateStatic: IInterface;
var
  VStatic: IMapTypeGUIConfigStatic;
begin
  if GetIsInitialized then begin
    Result := FGuiConfig.GetStatic;
  end else begin
    VStatic := TMapTypeGUIConfigStatic.Create('', 0, 0, False, '', False, '');
    Result := VStatic;
  end;
end;

procedure TMapTypeGuiConfigProxy.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  // nothing to do
end;

procedure TMapTypeGuiConfigProxy.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  // nothing to do
end;

function TMapTypeGuiConfigProxy.GetEnabled: Boolean;
begin
  LockRead;
  try
    if GetIsInitialized then begin
      Result := FGuiConfig.Enabled;
    end else begin
      Result := False;
    end;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGuiConfigProxy.GetHotKey: TShortCut;
begin
  LockRead;
  try
    if GetIsInitialized then begin
      Result := FGuiConfig.HotKey;
    end else begin
      Result := 0;
    end;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGuiConfigProxy.GetInfoUrl: IStringConfigDataElement;
begin
  LockRead;
  try
    if GetIsInitialized then begin
      Result := FGuiConfig.InfoUrl;
    end else begin
      Result := FInfoUrl;
    end;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGuiConfigProxy.GetName: IStringConfigDataElement;
begin
  LockRead;
  try
    if GetIsInitialized then begin
      Result := FGuiConfig.Name;
    end else begin
      Result := FName;
    end;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGuiConfigProxy.GetParentSubMenu: IStringConfigDataElement;
begin
  LockRead;
  try
    if GetIsInitialized then begin
      Result := FGuiConfig.ParentSubMenu;
    end else begin
      Result := FParentSubMenu;
    end;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGuiConfigProxy.GetSeparator: Boolean;
begin
  LockRead;
  try
    if GetIsInitialized then begin
      Result := FGuiConfig.Separator;
    end else begin
      Result := False;
    end;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGuiConfigProxy.GetSortIndex: Integer;
begin
  LockRead;
  try
    if GetIsInitialized then begin
      Result := FGuiConfig.SortIndex;
    end else begin
      Result := 0;
    end;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGuiConfigProxy.GetStatic: IMapTypeGUIConfigStatic;
begin
  Result := IMapTypeGUIConfigStatic(GetStaticInternal);
end;

procedure TMapTypeGuiConfigProxy.SetEnabled(const AValue: Boolean);
begin
  LockWrite;
  try
    if GetIsInitialized then begin
      FGuiConfig.Enabled := AValue;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGuiConfigProxy.SetHotKey(const AValue: TShortCut);
begin
  LockWrite;
  try
    if GetIsInitialized then begin
      FGuiConfig.HotKey := AValue;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGuiConfigProxy.SetSeparator(const AValue: Boolean);
begin
  LockWrite;
  try
    if GetIsInitialized then begin
      FGuiConfig.Separator := AValue;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGuiConfigProxy.SetSortIndex(const AValue: Integer);
begin
  LockWrite;
  try
    if GetIsInitialized then begin
      FGuiConfig.SortIndex := AValue;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

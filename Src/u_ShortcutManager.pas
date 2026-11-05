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

unit u_ShortcutManager;

interface

uses
  Classes,
  TB2Item,
  i_Bitmap32BufferFactory,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ShortCutSingleConfig;

type
  TShortcutManager = class
  private
    FIgnoredItems: TList;
    FItemsList: IInterfaceList;
    FMenuItems: TList;
    procedure LoadItems(
      const ABitmapFactory: IBitmap32StaticFactory;
      Menu: TTBCustomItem;
      const ACaptionPrefix: string;
      const AScope: TShortCutScope;
      AVisitedMenus: TList
    );
    function FindItemByShortCut(
      AShortCut: TShortCut;
      const AScopes: TShortCutScopes
    ): IShortCutSingleConfig;
    procedure ApplyScope(const AScope: TShortCutScope);
    procedure AddItem(
      const AShortCutInfo: IShortCutSingleConfig;
      AMenuItem: TTBCustomItem
    );
  public
    constructor Create(
      const ABitmapFactory: IBitmap32StaticFactory;
      AMainMenu: TTBCustomItem;
      AMainPopupMenu: TTBCustomItem;
      const APopupCaptionPrefix: string;
      AIgnoredItems: TList
    );
    destructor Destroy; override;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IShortCutSingleConfig;
    function GetShortCutInfoByShortCut(AShortCut: TShortCut): IShortCutSingleConfig;
    function GetPopupItemByShortCut(AShortCut: TShortCut): IShortCutSingleConfig;
    function TryExecute(const AItem: IShortCutSingleConfig): Boolean;
    procedure Load(const AProvider: IConfigDataProvider);
    procedure Save(const AProvider: IConfigDataWriteProvider);
    procedure CancelChanges;
    procedure ApplyChanges;
  end;

implementation

uses
  SysUtils,
  u_ShortCutSingleConfig;

{ TShortcutManager }

constructor TShortcutManager.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  AMainMenu: TTBCustomItem;
  AMainPopupMenu: TTBCustomItem;
  const APopupCaptionPrefix: string;
  AIgnoredItems: TList
);
var
  VVisitedMenus: TList;
begin
  inherited Create;

  FIgnoredItems := AIgnoredItems;
  FItemsList := TInterfaceList.Create;
  FMenuItems := TList.Create;

  VVisitedMenus := TList.Create;
  try
    LoadItems(ABitmapFactory, AMainMenu, '', scMainMenu, VVisitedMenus);
    LoadItems(ABitmapFactory, AMainPopupMenu, APopupCaptionPrefix, scPopup, VVisitedMenus);
  finally
    VVisitedMenus.Free;
  end;
end;

destructor TShortcutManager.Destroy;
begin
  FItemsList := nil;
  FreeAndNil(FMenuItems);
  FreeAndNil(FIgnoredItems);
  inherited;
end;

procedure TShortcutManager.AddItem(
  const AShortCutInfo: IShortCutSingleConfig;
  AMenuItem: TTBCustomItem
);
begin
  FItemsList.Add(AShortCutInfo);
  FMenuItems.Add(AMenuItem);
end;

function TShortcutManager.TryExecute(
  const AItem: IShortCutSingleConfig
): Boolean;

  function IsMenuItemAvailable(AMenuItem: TTBCustomItem): Boolean;
  begin
    Result := False;
    while Assigned(AMenuItem) do begin
      if not AMenuItem.Visible or not AMenuItem.Enabled then begin
        Exit;
      end;
      AMenuItem := AMenuItem.Parent;
    end;
    Result := True;
  end;

var
  VIndex: Integer;
  VMenuItem: TTBCustomItem;
begin
  Result := False;
  VIndex := FItemsList.IndexOf(AItem);
  if VIndex < 0 then begin
    Exit;
  end;
  VMenuItem := TTBCustomItem(FMenuItems[VIndex]);
  if IsMenuItemAvailable(VMenuItem) then begin
    VMenuItem.Click;
    Result := True;
  end;
end;

procedure TShortcutManager.ApplyScope(const AScope: TShortCutScope);
var
  I: Integer;
  VShortCutInfo: IShortCutSingleConfig;
begin
  for I := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := IShortCutSingleConfig(FItemsList.Items[I]);
    if VShortCutInfo.Scope = AScope then begin
      VShortCutInfo.ApplyShortCut;
    end;
  end;
end;

procedure TShortcutManager.ApplyChanges;
begin
  // The order matters: changing a hotkey of an action updates all menu items
  // linked to it, so popup items (which keep own hotkeys) are applied last
  ApplyScope(scMainMenu);
  ApplyScope(scPopup);
end;

procedure TShortcutManager.CancelChanges;
var
  I: Integer;
  VShortCutInfo: IShortCutSingleConfig;
begin
  for I := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := IShortCutSingleConfig(FItemsList.Items[I]);
    VShortCutInfo.ResetShortCut;
  end;
end;

function TShortcutManager.GetCount: Integer;
begin
  Result := FItemsList.Count;
end;

function TShortcutManager.GetItem(AIndex: Integer): IShortCutSingleConfig;
begin
  Result := IShortCutSingleConfig(FItemsList.Items[AIndex]);
end;

function TShortcutManager.GetShortCutInfoByShortCut(
  AShortCut: TShortCut): IShortCutSingleConfig;
begin
  Result := FindItemByShortCut(AShortCut, [scMainMenu, scPopup]);
end;

function TShortcutManager.GetPopupItemByShortCut(
  AShortCut: TShortCut): IShortCutSingleConfig;
begin
  Result := FindItemByShortCut(AShortCut, [scPopup]);
end;

function TShortcutManager.FindItemByShortCut(
  AShortCut: TShortCut;
  const AScopes: TShortCutScopes
): IShortCutSingleConfig;
var
  I: Integer;
  VShortCutInfo: IShortCutSingleConfig;
begin
  Result := nil;
  if AShortCut = 0 then begin
    Exit;
  end;
  for I := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := IShortCutSingleConfig(FItemsList.Items[I]);
    if (VShortCutInfo.Scope in AScopes) and (VShortCutInfo.ShortCut = AShortCut) then begin
      Result := VShortCutInfo;
      Break;
    end;
  end;
end;

procedure TShortcutManager.Load(const AProvider: IConfigDataProvider);
var
  I: Integer;
  VShortCutInfo: IShortCutSingleConfig;
begin
  if AProvider <> nil then begin
    for I := 0 to FItemsList.Count - 1 do begin
      VShortCutInfo := IShortCutSingleConfig(FItemsList.Items[I]);
      VShortCutInfo.ReadConfig(AProvider);
    end;
    ApplyChanges;
  end;
end;

procedure TShortcutManager.LoadItems(
  const ABitmapFactory: IBitmap32StaticFactory;
  Menu: TTBCustomItem;
  const ACaptionPrefix: string;
  const AScope: TShortCutScope;
  AVisitedMenus: TList
);
var
  I: Integer;
  VShortCutInfo: IShortCutSingleConfig;
  VMenuItem: TTBCustomItem;
  VSubMenu: TTBCustomItem;
begin
  // protects from linked submenus loops as well
  if AVisitedMenus.IndexOf(Menu) >= 0 then begin
    Exit;
  end;
  AVisitedMenus.Add(Menu);

  for I := 0 to Menu.Count - 1 do begin
    VMenuItem := Menu.Items[I];
    if not (VMenuItem is TTBSeparatorItem) then begin
      if (FIgnoredItems = nil) or (FIgnoredItems.IndexOf(VMenuItem) < 0) then begin
        if
          Assigned(VMenuItem.Action) or
          Assigned(VMenuItem.OnClick) or
          (VMenuItem.ClassName = 'TTBXVisibilityToggleItem')
        then begin
          VShortCutInfo := TShortCutSingleConfig.Create(ABitmapFactory, VMenuItem, ACaptionPrefix, AScope);
          AddItem(VShortCutInfo, VMenuItem);
        end;

        if Assigned(VMenuItem.LinkSubitems) then begin
          VSubMenu := VMenuItem.LinkSubitems;
          if (FIgnoredItems = nil) or (FIgnoredItems.IndexOf(VSubMenu) < 0) then begin
            LoadItems(ABitmapFactory, VSubMenu, ACaptionPrefix, AScope, AVisitedMenus);
          end;
        end else if VMenuItem.Count > 0 then begin
          LoadItems(ABitmapFactory, VMenuItem, ACaptionPrefix, AScope, AVisitedMenus);
        end;
      end;
    end;
  end;
end;

procedure TShortcutManager.Save(const AProvider: IConfigDataWriteProvider);
var
  I: Integer;
  VShortCutInfo: IShortCutSingleConfig;
begin
  for I := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := IShortCutSingleConfig(FItemsList.Items[I]);
    VShortCutInfo.WriteConfig(AProvider);
  end;
end;

end.

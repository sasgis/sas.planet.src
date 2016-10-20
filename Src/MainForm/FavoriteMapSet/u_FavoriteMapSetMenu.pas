{******************************************************************************}
{* SAS.Planet (SAS.�������)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_FavoriteMapSetMenu;

interface

uses
  Dialogs,
  TB2Item,
  i_Listener,
  i_InterfaceListStatic,
  i_FavoriteMapSetConfig,
  i_FavoriteMapSetHelper,
  i_FavoriteMapSetItemStatic;

type
  TFavoriteMapSetMenu = class
  private
    FRootMenu: TTBCustomItem;
    FFavoriteMapSetConfig: IFavoriteMapSetConfig;
    FFavoriteMapSetHelper: IFavoriteMapSetHelper;
    FFavoriteMapSetChangeListener: IListener;
    procedure ClearMenu;
    procedure OnMenuItemClick(Sender: TObject);
    procedure OnFavoriteMapSetChanged;
    function CreateMenuItem(const AItem: IFavoriteMapSetItemStatic): TTBCustomItem;
  public
    constructor Create(
      const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
      const AFavoriteMapSetHelper: IFavoriteMapSetHelper;
      ARootMenu: TTBCustomItem
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Classes,
  TBX,
  c_ZeroGUID,
  i_GUIDListStatic,
  u_ListenerByEvent;

{ TFavoriteMapSetMenu }

constructor TFavoriteMapSetMenu.Create(
  const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
  const AFavoriteMapSetHelper: IFavoriteMapSetHelper;
  ARootMenu: TTBCustomItem
);
begin
  inherited Create;

  FFavoriteMapSetConfig := AFavoriteMapSetConfig;
  FFavoriteMapSetHelper := AFavoriteMapSetHelper;
  FRootMenu := ARootMenu;

  OnFavoriteMapSetChanged;

  FFavoriteMapSetChangeListener :=
    TNotifyNoMmgEventListener.Create(Self.OnFavoriteMapSetChanged);

  FFavoriteMapSetConfig.ChangeNotifier.Add(FFavoriteMapSetChangeListener);
end;

destructor TFavoriteMapSetMenu.Destroy;
begin
  FFavoriteMapSetConfig.ChangeNotifier.Remove(FFavoriteMapSetChangeListener);
  ClearMenu;
  inherited Destroy;
end;

procedure TFavoriteMapSetMenu.OnFavoriteMapSetChanged;
var
  I: Integer;
  VStatic: IInterfaceListStatic;
  VItem: IFavoriteMapSetItemStatic;
  VMenuItem: TTBCustomItem;
begin
  ClearMenu;
  VStatic := FFavoriteMapSetConfig.GetStatic;
  if Assigned(VStatic) and (VStatic.Count > 0) then begin
    for I := 0 to VStatic.Count - 1 do begin
      VItem := IFavoriteMapSetItemStatic(VStatic.Items[I]);
      VMenuItem := CreateMenuItem(VItem);
      FRootMenu.Add(VMenuItem);
    end;
  end;
end;

function TFavoriteMapSetMenu.CreateMenuItem(
  const AItem: IFavoriteMapSetItemStatic
): TTBCustomItem;
begin
  Assert(AItem <> nil);
  AItem._AddRef;
  Result := TTBXItem.Create(FRootMenu);
  Result.Tag := LongInt(AItem);
  Result.Caption := AItem.Name;
  Result.OnClick := Self.OnMenuItemClick;
  Result.ShortCut := AItem.HotKey;
end;

procedure TFavoriteMapSetMenu.OnMenuItemClick(Sender: TObject);
var
  VErrMsg: string;
  VMenuItem: TComponent;
  VItem: IFavoriteMapSetItemStatic;
begin
  VMenuItem := Sender as TComponent;
  if Assigned(VMenuItem) and (VMenuItem.Tag > 0) then begin
    VItem := IFavoriteMapSetItemStatic(VMenuItem.Tag);
    Assert(VItem <> nil);
    if not FFavoriteMapSetHelper.TrySwitchOn(VItem, VErrMsg) then begin
      MessageDlg(VErrMsg, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TFavoriteMapSetMenu.ClearMenu;
var
  I: Integer;
  VMenuItem: TTBCustomItem;
  VItem: IFavoriteMapSetItemStatic;
begin
  for I := FRootMenu.Count - 1 downto 0 do begin
    VMenuItem := FRootMenu.Items[I];
    if VMenuItem.Tag > 0 then begin
      VItem := IFavoriteMapSetItemStatic(VMenuItem.Tag);
      VItem._Release;
      VMenuItem.Tag := 0;
      FRootMenu.Remove(VMenuItem);
    end;
  end;
end;

end.

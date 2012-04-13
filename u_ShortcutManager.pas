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

unit u_ShortcutManager;

interface

uses
  Classes,
  TB2Item,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ShortCutSingleConfig;

type
  TShortcutManager = class
  private
    FIgnoredItems: TList;
    FItemsList: IInterfaceList;
    procedure LoadItems(Menu: TTBCustomItem);
  public
    constructor Create(AMainMenu: TTBCustomItem; AIgnoredItems: TList);
    destructor Destroy; override;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IShortCutSingleConfig;
    function GetShortCutInfoByShortCut(AShortCut:TShortCut): IShortCutSingleConfig;
    procedure Load(AProvider: IConfigDataProvider);
    procedure Save(AProvider: IConfigDataWriteProvider);
    procedure CancelChanges;
    procedure ApplyChanges;
  end;

implementation

uses
  SysUtils,
  u_ShortCutSingleConfig;

{ TShortcutManager }

constructor TShortcutManager.Create(AMainMenu: TTBCustomItem;
  AIgnoredItems: TList);
begin
  FIgnoredItems := AIgnoredItems;
  FItemsList := TInterfaceList.Create;
  LoadItems(AMainMenu);
end;

destructor TShortcutManager.Destroy;
begin
  FItemsList := nil;
  FreeAndNil(FIgnoredItems);
  inherited;
end;

procedure TShortcutManager.ApplyChanges;
var
  i: Integer;
  VShortCutInfo: IShortCutSingleConfig;
begin
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := IShortCutSingleConfig(FItemsList.Items[i]);
    VShortCutInfo.ApplyShortCut;
  end;
end;

procedure TShortcutManager.CancelChanges;
var
  i: Integer;
  VShortCutInfo: IShortCutSingleConfig;
begin
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := IShortCutSingleConfig(FItemsList.Items[i]);
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
var
  i: Integer;
  VShortCutInfo: IShortCutSingleConfig;
begin
  Result := nil;
  if AShortCut <> 0 then begin
    for i := 0 to FItemsList.Count - 1 do begin
      VShortCutInfo := IShortCutSingleConfig(FItemsList.Items[i]);
      if VShortCutInfo.ShortCut = AShortCut then begin
        Result := VShortCutInfo;
        Break;
      end;
    end;
  end;
end;

procedure TShortcutManager.Load(AProvider: IConfigDataProvider);
var
  i: Integer;
  VShortCutInfo: IShortCutSingleConfig;
begin
  if AProvider <> nil then begin
    for i := 0 to FItemsList.Count - 1 do begin
      VShortCutInfo := IShortCutSingleConfig(FItemsList.Items[i]);
      VShortCutInfo.ReadConfig(AProvider);
    end;
    ApplyChanges;
  end;
end;

procedure TShortcutManager.LoadItems(Menu: TTBCustomItem);
var
  i: Integer;
  VShortCutInfo: IShortCutSingleConfig;
  VMenuItem: TTBCustomItem;
begin
  for i := 0 to Menu.Count-1 do begin
    VMenuItem := Menu.Items[i];
    if not(VMenuItem is TTBSeparatorItem) then begin
      if (FIgnoredItems = nil) or (FIgnoredItems.IndexOf(VMenuItem) < 0) then begin
        if Assigned(VMenuItem.OnClick) then begin
          VShortCutInfo := TShortCutSingleConfig.Create(VMenuItem);
          FItemsList.Add(VShortCutInfo);
        end;
        if VMenuItem.Count > 0 then begin
          LoadItems(VMenuItem);
        end;
      end;
    end;
  end;
end;

procedure TShortcutManager.Save(AProvider: IConfigDataWriteProvider);
var
  i: Integer;
  VShortCutInfo: IShortCutSingleConfig;
begin
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := IShortCutSingleConfig(FItemsList.Items[i]);
    VShortCutInfo.WriteConfig(AProvider);
  end;
end;

end.

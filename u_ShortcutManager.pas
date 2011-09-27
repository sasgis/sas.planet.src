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

unit u_ShortcutManager;

interface

uses
  Types,
  Classes,
  Graphics,
  TB2Item,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

type
  TShortCutInfo = class(TObject)
  private
    FBitmap: TBitmap;
    FMenuItem: TTBCustomItem;
    FShortCut:TShortCut;
    function GetBitmap(aMenu: TTBCustomItem): TBitmap;
  public
    constructor Create(AMenuItem: TTBCustomItem);
    destructor Destroy; override;
    procedure Load(AProvider: IConfigDataProvider);
    procedure Save(AProvider: IConfigDataWriteProvider);
    function GetCaption: String;
    procedure ResetShortCut;
    procedure ApplyShortCut;
    property ShortCut:TShortCut read FShortCut write FShortCut;
    property Bitmap: TBitmap read FBitmap;
  end;

  TShortcutManager = class
  private
    FMainMenu: TTBCustomItem;
    FIgnoredItems: TList;
    FItemsList: TList;
    procedure LoadItems(Menu: TTBCustomItem);
  public
    constructor Create(AMainMenu: TTBCustomItem; AIgnoredItems: TList);
    destructor Destroy; override;
    function GetShortCutInfoByShortCut(AShortCut:TShortCut): TShortCutInfo;
    procedure Load(AProvider: IConfigDataProvider);
    procedure Save(AProvider: IConfigDataWriteProvider);
    procedure GetObjectsList(AList: TStrings);
    procedure CancelChanges;
    procedure ApplyChanges;
  end;

implementation

uses
  SysUtils;

{ TShortCutInfo }

constructor TShortCutInfo.Create(AMenuItem: TTBCustomItem);
begin
  FMenuItem := AMenuItem;
  ResetShortCut;
  FBitmap := GetBitmap(AMenuItem);
end;

destructor TShortCutInfo.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TShortCutInfo.ApplyShortCut;
begin
  FMenuItem.ShortCut := FShortCut;
end;

function TShortCutInfo.GetBitmap(AMenu: TTBCustomItem): TBitmap;
begin
  Result := nil;
  if AMenu.ImageIndex >= 0 then begin
    Result := TBitmap.Create;
    AMenu.Images.GetBitmap(AMenu.ImageIndex, Result);
  end;
end;

function TShortCutInfo.GetCaption: String;
var
  Menu: TTBCustomItem;
  AddName: String;
begin
  Result := '';
  Menu := FMenuItem;
  repeat
    AddName := Menu.Caption;
    if Pos('&', AddName) <> 0 then begin
      Delete(AddName, Pos('&', AddName), 1);
    end;
    if Result = '' then begin
      Result := AddName
    end else begin
      if AddName <> '' then begin
        Result :=AddName+' -> '+Result;
      end;
    end;

    if Assigned(Menu.Parent) then begin
      Menu := Menu.Parent
    end else begin
      Break;
    end;
  until Menu.HasParent = False;
end;

procedure TShortCutInfo.Load(AProvider: IConfigDataProvider);
begin
  FMenuItem.ShortCut := AProvider.ReadInteger(FMenuItem.name, FMenuItem.ShortCut);
  ResetShortCut;
end;

procedure TShortCutInfo.ResetShortCut;
begin
  FShortCut := FMenuItem.ShortCut;
end;

procedure TShortCutInfo.Save(AProvider: IConfigDataWriteProvider);
begin
  AProvider.WriteInteger(FMenuItem.Name, FMenuItem.ShortCut);
end;

{ TShortcutManager }

constructor TShortcutManager.Create(AMainMenu: TTBCustomItem;
  AIgnoredItems: TList);
begin
  FMainMenu := AMainMenu;
  FIgnoredItems := AIgnoredItems;
  FItemsList := TList.Create;
  LoadItems(FMainMenu);
end;

destructor TShortcutManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FItemsList.Count - 1 do begin
    TObject(FItemsList.Items[i]).Free;
  end;
  FreeAndNil(FItemsList);
  FreeAndNil(FIgnoredItems);
  inherited;
end;

procedure TShortcutManager.ApplyChanges;
var
  i: Integer;
  VShortCutInfo: TShortCutInfo;
begin
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
    VShortCutInfo.ApplyShortCut;
  end;
end;

procedure TShortcutManager.CancelChanges;
var
  i: Integer;
  VShortCutInfo: TShortCutInfo;
begin
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
    VShortCutInfo.ResetShortCut;
  end;
end;

procedure TShortcutManager.GetObjectsList(AList: TStrings);
var
  i: Integer;
  VShortCutInfo: TShortCutInfo;
begin
  AList.Clear;
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
    AList.AddObject(VShortCutInfo.GetCaption, VShortCutInfo);
  end;
end;

function TShortcutManager.GetShortCutInfoByShortCut(
  AShortCut: TShortCut): TShortCutInfo;
var
  i: Integer;
  VShortCutInfo: TShortCutInfo;
begin
  Result := nil;
  if AShortCut <> 0 then begin
    for i := 0 to FItemsList.Count - 1 do begin
      VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
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
  VShortCutInfo: TShortCutInfo;
begin
  if AProvider <> nil then begin
    for i := 0 to FItemsList.Count - 1 do begin
      VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
      VShortCutInfo.Load(AProvider);
    end;
  end;
end;

procedure TShortcutManager.LoadItems(Menu: TTBCustomItem);
var
  i: Integer;
  VShortCutInfo: TShortCutInfo;
  VMenuItem: TTBCustomItem;
begin
  for i := 0 to Menu.Count-1 do begin
    VMenuItem := Menu.Items[i];
    if not(VMenuItem is TTBSeparatorItem) then begin
      if (FIgnoredItems = nil) or (FIgnoredItems.IndexOf(VMenuItem) < 0) then begin
        if Assigned(VMenuItem.OnClick) then begin
          VShortCutInfo := TShortCutInfo.Create(VMenuItem);
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
  VShortCutInfo: TShortCutInfo;
begin
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
    VShortCutInfo.Save(AProvider);
  end;
end;

end.

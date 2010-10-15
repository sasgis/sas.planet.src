unit u_ShortcutManager;

interface

uses
  Types,
  Classes,
  IniFiles,
  Graphics,
  TB2Item,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider;

type
  TShortCutInfo = class(TObject)
  private
    MenuItem: TTBCustomItem;
  public
    ShortCut:TShortCut;
    Icon: TIcon;
    constructor Create(AMenuItem: TTBCustomItem);
    destructor Destroy; override;
  end;

  TShortcutManager = class
  private
    FMainMenu: TTBCustomItem;
    FIgnoredItems: TList;
    FItemsList: TList;
    procedure LoadItems(Menu: TTBCustomItem);
    function GetIcon(aMenu: TTBCustomItem): TIcon;
    function GetCaption(aMenu:TTBCustomItem): String;
  public
    constructor Create(AMainMenu: TTBCustomItem; AIgnoredItems: TList);
    destructor Destroy; override;
    procedure Load(AProvider: IConfigDataProvider);
    procedure Save(AProvider: IConfigDataWriteProvider);
    procedure GetObjectsList(AList: TStrings);
    procedure CancelChanges;
  end;

implementation

uses
  SysUtils,
  TBX;

{ TShortCutInfo }

constructor TShortCutInfo.Create(AMenuItem: TTBCustomItem);
begin
  MenuItem := AMenuItem;
end;

destructor TShortCutInfo.Destroy;
begin
  FreeAndNil(Icon);
  inherited;
end;

{ TShortcutManager }

procedure TShortcutManager.CancelChanges;
var
  i: Integer;
  VShortCutInfo: TShortCutInfo;
  VMenuItem: TTBCustomItem;
begin
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
    VMenuItem := VShortCutInfo.MenuItem;
    VShortCutInfo.ShortCut := VMenuItem.ShortCut;
  end;
end;

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

function TShortcutManager.GetCaption(aMenu: TTBCustomItem): String;
var
  Menu: TTBCustomItem;
  AddName: String;
begin
  Result:='';
  Menu:=aMenu;
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

function TShortcutManager.GetIcon(AMenu: TTBCustomItem): TIcon;
begin
  Result := nil;
  if AMenu.ImageIndex >= 0 then begin
    Result := TIcon.Create;
    if Assigned(fMainMenu.Images) then begin
      fMainMenu.Images.GetIcon(AMenu.ImageIndex, Result);
    end else begin
      AMenu.Images.GetIcon(AMenu.ImageIndex, Result);
    end;
  end;
end;

procedure TShortcutManager.GetObjectsList(AList: TStrings);
var
  i: Integer;
  VShortCutInfo: TShortCutInfo;
  VMenuItem: TTBCustomItem;
begin
  AList.Clear;
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
    VMenuItem := VShortCutInfo.MenuItem;
    AList.AddObject(GetCaption(VMenuItem), VShortCutInfo);
  end;
end;

procedure TShortcutManager.Load(AProvider: IConfigDataProvider);
var
  i: Integer;
  VShortCutInfo: TShortCutInfo;
  VMenuItem: TTBCustomItem;
begin
  if AProvider <> nil then begin
    for i := 0 to FItemsList.Count - 1 do begin
      VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
      VMenuItem := VShortCutInfo.MenuItem;
      VMenuItem.ShortCut := AProvider.ReadInteger(VMenuItem.name, VMenuItem.ShortCut);
      VShortCutInfo.ShortCut := VMenuItem.ShortCut;
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
        if (VMenuItem.Count = 0) and (VMenuItem is TTBXItem) then begin
          VShortCutInfo := TShortCutInfo.Create(VMenuItem);
          VShortCutInfo.MenuItem := VMenuItem;
          VShortCutInfo.ShortCut := VMenuItem.ShortCut;
          VShortCutInfo.Icon := GetIcon(VMenuItem);
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
  VMenuItem: TTBCustomItem;
begin
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
    VMenuItem := VShortCutInfo.MenuItem;
    AProvider.WriteInteger(VMenuItem.Name, VShortCutInfo.ShortCut);
    VMenuItem.ShortCut := VShortCutInfo.ShortCut;
  end;
end;

end.

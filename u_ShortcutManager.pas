unit u_ShortcutManager;

interface

uses
  Types,
  Classes,
  Controls,
  IniFiles,
  Graphics,
  StdCtrls,
  TB2Item;

type
  TShortCutInfo = class(TObject)
    ShortCut:TShortCut;
    MenuItem:TTBCustomItem;
    Icon: TIcon;
    destructor Destroy; override;
  end;

  TShortcutEditor = class(TObject)
  private
    fMainMenu: TTBCustomItem;
    fSection: String;
    FList: TListBox;
    procedure ListDblClick(Sender: TObject);
    procedure ListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ClearList;
    function GetCaption(aMenu:TTBCustomItem):String;
    function GetIcon(aMenu:TTBCustomItem):TIcon;
    procedure AddItems(Menu:TTBCustomItem);
  public
    constructor Create(AList:TListBox);
    destructor Destroy; override;
    procedure LoadShortCuts(MainMenu: TTBCustomItem; Section:String);
    procedure Save;
    procedure RefreshTranslation;
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
    procedure Load(AIni: TCustomIniFile; ASection: string);
    procedure Save(AIni: TCustomIniFile; ASection: string);
    procedure GetObjectsList(AList: TStrings);
  end;

implementation

uses
  SysUtils,
  Menus,
  Dialogs,
  TBX,
  u_GlobalState,
  UShortcutEditor;

const
  NoHotKey : array [1..12] of string = (
    'NSMB','NLayerSel','TBFillingTypeMap','NLayerParams','TBLang','N002','N003','N004',
    'N005','N006','N007','NFillMap'
  );


constructor TShortcutEditor.Create(AList:TListBox);
begin
  FList:=AList;
  FList.OnDrawItem:=ListDrawItem;
  FList.OnDblClick:=ListDblClick;
end;

destructor TShortcutEditor.Destroy;
begin
  ClearList;
  inherited;
end;

function inNotHotKey(name:string):boolean;
var i:integer;
begin
 result:=false;
 for i:=1 to length(NoHotKey) do begin
   if name=NoHotKey[i] then begin
    result:=true;
    exit;
   end;
 end;
end;

procedure TShortcutEditor.ListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ShortCut:String;
  VTempShortCut: TShortCutInfo;
begin
  with FList.Canvas do begin
    FillRect(Rect);
    VTempShortCut := TShortCutInfo(FList.Items.Objects[Index]);
    ShortCut := ShortCutToText(VTempShortCut.ShortCut);
    if VTempShortCut.Icon <> nil then begin
      Draw(2,Rect.Top+2, VTempShortCut.Icon);
    end;
    TextOut(22,Rect.Top+3, FList.Items[Index]);
    TextOut(Rect.Right-TextWidth(ShortCut)-9,Rect.Top+3, ShortCut);

    Pen.Color := clSilver;
    MoveTo(0, Rect.Bottom-1);
    LineTo(Rect.Right, Rect.Bottom-1);
  end;
end;

procedure TShortcutEditor.ListDblClick(Sender: TObject);

  function ShortCutExists(A:TShortcut):Boolean;
  var i:Integer;
  begin
    Result := False;
    for i := 0 to FList.Items.Count-1 do begin
      if TShortCutInfo(FList.Items.Objects[i]).ShortCut = A then begin
        Result := True;
        Break;
      end;
    end;
  end;

begin
  if FList.ItemIndex<>-1 then begin
    FShortcutChange.HotKey.HotKey := TShortCutInfo(FList.Items.Objects[FList.ItemIndex]).ShortCut;
    if FShortcutChange.ShowModal = mrOK then begin
      if (ShortCutExists(FShortcutChange.HotKey.HotKey))and(FShortcutChange.HotKey.HotKey<>0) then begin
        ShowMessage('Горячая клавиша уже используется, пожалуйста, выберите другую')
      end else begin
        TShortCutInfo(FList.Items.Objects[FList.ItemIndex]).ShortCut := FShortcutChange.HotKey.HotKey;
      end;
      FList.Repaint;
    end;
  end;
end;

function TShortcutEditor.GetCaption(aMenu:TTBCustomItem):String;
var Menu:TTBCustomItem;
    AddName:String;
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

function TShortcutEditor.GetIcon(aMenu: TTBCustomItem): TIcon;
begin
  Result := nil;
  if aMenu.ImageIndex<>-1 then begin
    Result := TIcon.Create;
    if Assigned(fMainMenu.Images) then begin
      fMainMenu.Images.GetIcon(aMenu.ImageIndex, Result);
    end else begin
      aMenu.Images.GetIcon(aMenu.ImageIndex, Result);
    end;
  end;
end;

procedure TShortcutEditor.AddItems(Menu:TTBCustomItem);
var
  i:Integer;
  TempShortCut:  TShortCutInfo;
begin
  for i:=0 to Menu.Count-1 do begin
    if (not(inNotHotKey(Menu.Items[i].Name)))and(Menu.Items[i].ClassType<>TTBXSeparatorItem) then begin
      if (Menu.Items[i].Count=0)and(Menu.Items[i].ClassType=TTBXItem) then begin
        TempShortCut := TShortCutInfo.Create;
        TempShortCut.MenuItem:=Menu.Items[i];
        TempShortCut.ShortCut:=Menu.Items[i].ShortCut;
        TempShortCut.Icon := GetIcon(Menu.Items[i]);
        FList.Items.AddObject(GetCaption(Menu.Items[i]), TempShortCut);
      end;
      if Menu.Items[i].Count>0 then begin
         AddItems(Menu.Items[i]);
      end;
    end;
  end;
end;

procedure TShortcutEditor.LoadShortCuts(MainMenu: TTBCustomItem; Section:String);
  procedure LoadItems(Menu:TTBCustomItem);
  var i:Integer;
  begin
    for i := 0 to Menu.Count-1 do begin
      Menu.Items[i].ShortCut := Gstate.MainIni.ReadInteger(Section, Menu.Items[i].name, Menu.Items[i].ShortCut);
      if Menu.Items[i].Count > 0 then begin
        LoadItems(Menu.Items[i]);
      end;
    end;
  end;

begin
  fSection := Section;
  fMainMenu := MainMenu;
  LoadItems(fMainMenu);
  ClearList;
  AddItems(fMainMenu);
end;

procedure TShortcutEditor.RefreshTranslation;
begin
  ClearList;
  AddItems(fMainMenu);
end;

procedure TShortcutEditor.Save;
var
  i:Integer;
  VTempShortCut: TShortCutInfo;
begin
  for i := 0 to FList.Items.Count-1 do begin
    VTempShortCut := TShortCutInfo(FList.Items.Objects[i]);
    Gstate.MainIni.WriteInteger(fSection, VTempShortCut.MenuItem.Name, VTempShortCut.ShortCut);
    VTempShortCut.MenuItem.ShortCut := VTempShortCut.ShortCut;
  end;
end;

procedure TShortcutEditor.ClearList;
var i:Integer;
begin
  for i := 0 to FList.Items.Count-1 do begin
    FList.Items.Objects[i].Free;
  end;
  FList.Clear;
end;

{ TShortCutInfo }

destructor TShortCutInfo.Destroy;
begin
  FreeAndNil(Icon);
  inherited;
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
    VShortCutInfo.ShortCut := VMenuItem.ShortCut;
    AList.AddObject(GetCaption(VMenuItem), VShortCutInfo);
  end;
end;

procedure TShortcutManager.Load(AIni: TCustomIniFile; ASection: string);
var
  i: Integer;
  VShortCutInfo: TShortCutInfo;
  VMenuItem: TTBCustomItem;
begin
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
    VMenuItem := VShortCutInfo.MenuItem;
    VMenuItem.ShortCut := AIni.ReadInteger(ASection, VMenuItem.name, VMenuItem.ShortCut);
    VShortCutInfo.ShortCut := VMenuItem.ShortCut;
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
          VShortCutInfo := TShortCutInfo.Create;
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

procedure TShortcutManager.Save(AIni: TCustomIniFile; ASection: string);
var
  i: Integer;
  VShortCutInfo: TShortCutInfo;
  VMenuItem: TTBCustomItem;
begin
  for i := 0 to FItemsList.Count - 1 do begin
    VShortCutInfo := TShortCutInfo(FItemsList.Items[i]);
    VMenuItem := VShortCutInfo.MenuItem;
    AIni.WriteInteger(ASection, VMenuItem.Name, VShortCutInfo.ShortCut);
    VMenuItem.ShortCut := AIni.ReadInteger(ASection, VMenuItem.name, VMenuItem.ShortCut);
    VMenuItem.ShortCut := VShortCutInfo.ShortCut;
  end;
end;

end.

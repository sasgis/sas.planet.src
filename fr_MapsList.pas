unit fr_MapsList;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  i_LanguageManager,
  i_MapTypeGUIConfigList,
  i_MapTypeConfigModalEdit,
  i_InternalBrowser,
  i_MapTypes,
  u_CommonFormAndFrameParents;

type
  TfrMapsList = class(TFrame)
    pnlMapsRightButtons: TPanel;
    Button15: TButton;
    Button11: TButton;
    Button12: TButton;
    btnMapInfo: TButton;
    MapList: TListView;
    procedure Button12Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure btnMapInfoClick(Sender: TObject);
    procedure MapListDblClick(Sender: TObject);
    procedure MapListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure MapListCustomDrawSubItem(Sender: TCustomListView; Item: TListItem;
      SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    FMapTypeEditor: IMapTypeConfigModalEdit;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FInternalBrowser: IInternalBrowser;
    procedure UpdateList;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AInternalBrowser: IInternalBrowser;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AMapTypeEditor: IMapTypeConfigModalEdit
    ); reintroduce;
    destructor Destroy; override;
    procedure Init;
    procedure CancelChanges;
    procedure ApplyChanges;
  end;

implementation

uses
  Menus,
  c_InternalBrowser,
  i_GUIDListStatic,
  u_MapType,
  u_ResStrings;

{$R *.dfm}

{ TfrMapsList }

procedure ExchangeItems(lv: TListView; const i, j: Integer);
var
  tempLI: TListItem;
begin
  lv.Items.BeginUpdate;
  try
    tempLI := TListItem.Create(lv.Items);
    try
      tempLI.Assign(lv.Items.Item[i]);
      lv.Items.Item[i].Assign(lv.Items.Item[j]);
      lv.Items.Item[j].Assign(tempLI);
      lv.Items.Item[j].Selected:=true;
    finally
      tempLI.Free;
    end;
  finally
    lv.Items.EndUpdate
  end;
end;

procedure TfrMapsList.ApplyChanges;
var
  i: Integer;
  VMapType: TMapType;
begin
  FGUIConfigList.LockWrite;
  try
    For i:=0 to MapList.Items.Count-1 do begin
      VMapType := TMapType(MapList.Items.Item[i].data);
      if VMapType <> nil then begin
        VMapType.GUIConfig.SortIndex := i+1;
      end;
    end;
  finally
    FGUIConfigList.UnlockWrite;
  end;
end;

procedure TfrMapsList.btnMapInfoClick(Sender: TObject);
var
  VMapType: TMapType;
  VUrl: string;
  VItem: TListItem;
begin
  VItem := MapList.Selected;
  if VItem <> nil then begin
    VMapType := TMapType(VItem.Data);
    if VMapType <> nil then begin
      VUrl := VMapType.GUIConfig.InfoUrl.Value;
      if VUrl <> '' then begin
        VUrl := CZmpInfoInternalURL + GUIDToString(VMapType.Zmp.GUID) + VUrl;
        FInternalBrowser.Navigate(VMapType.Zmp.FileName, VUrl);
      end;
    end;
  end;
end;

procedure TfrMapsList.Button11Click(Sender: TObject);
begin
  If (MapList.Selected<>nil)and(MapList.Selected.Index<MapList.Items.Count-1) then begin
    ExchangeItems(MapList, MapList.Selected.Index,MapList.Selected.Index+1)
  end;
end;

procedure TfrMapsList.Button12Click(Sender: TObject);
begin
  If (MapList.Selected<>nil)and(MapList.Selected.Index>0) then begin
    ExchangeItems(MapList, MapList.Selected.Index,MapList.Selected.Index-1);
  end;
end;

procedure TfrMapsList.Button15Click(Sender: TObject);
var
  VMapType: TMapType;
  VItem: TListItem;
begin
  VItem := MapList.Selected;
  if VItem <> nil then begin
    VMapType := TMapType(VItem.Data);
    if VMapType <> nil then begin
      if FMapTypeEditor.EditMap(VMapType) then begin
        UpdateList;
      end;
    end;
  end;
end;

procedure TfrMapsList.CancelChanges;
begin
end;

constructor TfrMapsList.Create(
  const ALanguageManager: ILanguageManager;
  const AInternalBrowser: IInternalBrowser;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AMapTypeEditor: IMapTypeConfigModalEdit
);
begin
  inherited Create(ALanguageManager);
  FInternalBrowser := AInternalBrowser;
  FFullMapsSet := AFullMapsSet;
  FMapTypeEditor := AMapTypeEditor;
  FGUIConfigList := AGUIConfigList;
  MapList.DoubleBuffered:=true;
end;

destructor TfrMapsList.Destroy;
var
  i: Integer;
begin
  if Assigned(MapList) then begin
    for i := 0 to MapList.Items.Count - 1 do begin
      MapList.Items.Item[i].data := nil;
    end;
  end;
  inherited;
end;

procedure TfrMapsList.Init;
begin
  UpdateList;
end;

procedure TfrMapsList.MapListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  VMapType: TMapType;
begin
  if Self.Visible then begin
    VMapType := TMapType(Item.Data);
    if VMapType <> nil then begin
      btnMapInfo.Enabled:=VMapType.GUIConfig.InfoUrl.Value<>'';
    end;
  end;
end;

procedure TfrMapsList.MapListCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  VMapType: TMapType;
begin
  if Item = nil then EXIT;
  VMapType := TMapType(Item.Data);
  if VMapType <> nil then begin
    if Item.Index mod 2 = 1 then begin
      Sender.canvas.brush.Color := cl3DLight;
    end else begin
      Sender.canvas.brush.Color := clwhite;
    end;
  end;
end;

procedure TfrMapsList.MapListDblClick(Sender: TObject);
begin
  Button15Click(Sender);
end;

procedure TfrMapsList.UpdateList;
procedure SetSubItem(AItem: TListItem; AIndex: Integer; const AValue: string);
var
  i: Integer;
begin
  if AIndex < AItem.SubItems.Count then begin
    AItem.SubItems.Strings[AIndex] := AValue;
  end else begin
    for i := AItem.SubItems.Count to AIndex - 1 do begin
      AItem.SubItems.Add('');
    end;
    AItem.SubItems.Add(AValue);
  end;
end;

procedure UpdateItem(AItem: TListItem; AMapType: TMapType);
var
  VValue: string;
begin
  AItem.Caption := AMapType.GUIConfig.Name.Value;
  AItem.Data := AMapType;
  VValue := AMapType.StorageConfig.NameInCache;
  SetSubItem(AItem, 0, VValue);
  if AMapType.Abilities.IsLayer then begin
    VValue := SAS_STR_Layers+'\'+AMapType.GUIConfig.ParentSubMenu.Value;
  end else begin
    VValue := SAS_STR_Maps+'\'+AMapType.GUIConfig.ParentSubMenu.Value;
  end;
  SetSubItem(AItem, 1, VValue);
  VValue := ShortCutToText(AMapType.GUIConfig.HotKey);
  SetSubItem(AItem, 2, VValue);
  VValue := AMapType.Zmp.FileName;
  SetSubItem(AItem, 3, VValue);
  if AMapType.GUIConfig.Enabled then begin
    VValue := SAS_STR_Yes;
  end else begin
    VValue := SAS_STR_No;
  end;
  SetSubItem(AItem, 4, VValue);
end;

var
  VPrevSelectedIndex: Integer;
  i: integer;
  VMapType: TMapType;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
  VItem: TListItem;
begin
  VPrevSelectedIndex := MapList.ItemIndex;
  MapList.Items.BeginUpdate;
  try
    VGUIDList := FGUIConfigList.OrderedMapGUIDList;
    for i := 0 to VGUIDList.Count - 1 do begin
      VGUID := VGUIDList.Items[i];
      VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
      if i < MapList.Items.Count then begin
        VItem := MapList.Items[i];
      end else begin
        VItem := MapList.Items.Add;
      end;
      UpdateItem(VItem, VMapType);
    end;
    for i := MapList.Items.Count - 1 downto VGUIDList.Count do begin
      MapList.Items.Delete(i);
    end;
    if MapList.Items.Count > 0 then begin
      if (VPrevSelectedIndex >= 0) and (VPrevSelectedIndex >= MapList.Items.Count) then begin
        MapList.ItemIndex := MapList.Items.Count - 1;
      end;
    end;
  finally
    MapList.Items.EndUpdate;
  end;
end;

end.

unit fr_MapsList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  i_LanguageManager,
  i_MapTypeGUIConfigList,
  i_MapTypeConfigModalEdit,
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
    procedure UpdateList;
  public
    constructor Create(
      const AMapTypeEditor: IMapTypeConfigModalEdit
    ); reintroduce;
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
  u_ResStrings,
  u_GlobalState;

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
begin
  GState.MapType.GUIConfigList.LockWrite;
  try
    For i:=0 to MapList.Items.Count-1 do begin
      TMapType(MapList.Items.Item[i].data).GUIConfig.SortIndex := i+1;
    end;
  finally
    GState.MapType.GUIConfigList.UnlockWrite;
  end;
end;

procedure TfrMapsList.btnMapInfoClick(Sender: TObject);
var
  VMap: TMapType;
  VUrl: string;
begin
  VMap := TMapType(MapList.Selected.Data);
  VUrl := VMap.GUIConfig.InfoUrl.Value;
  if VUrl <> '' then begin
    VUrl := CZmpInfoInternalURL + GUIDToString(VMap.Zmp.GUID) + VUrl;
    GState.InternalBrowser.Navigate(VMap.Zmp.FileName, VUrl);
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
begin
  VMapType := TMapType(MapList.Selected.Data);
  if FMapTypeEditor.EditMap(VMapType) then begin
    UpdateList;
  end;
end;

procedure TfrMapsList.CancelChanges;
begin
end;

constructor TfrMapsList.Create(
  const AMapTypeEditor: IMapTypeConfigModalEdit
);
begin
  inherited Create(nil);
  FMapTypeEditor := AMapTypeEditor;
  MapList.DoubleBuffered:=true;
end;

procedure TfrMapsList.Init;
begin
  UpdateList;
end;

procedure TfrMapsList.MapListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  VMap: TMapType;
begin
  if Self.Visible then begin
    if Item.Data<>nil then begin
      VMap := TMapType(Item.Data);
      btnMapInfo.Enabled:=VMap.GUIConfig.InfoUrl.Value<>'';
    end;
  end;
end;

procedure TfrMapsList.MapListCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
 if item = nil then EXIT;
 if TMapType(Item.Data).GUIConfig.Separator then
  begin
   sender.canvas.Pen.Color:=clGray;
   sender.canvas.MoveTo(2,Item.DisplayRect(drBounds).Bottom-1);
   sender.canvas.LineTo(sender.Column[0].Width,Item.DisplayRect(drBounds).Bottom-1);
  end;
 if Item.Index mod 2 = 1 then sender.canvas.brush.Color:=cl3DLight
                         else sender.canvas.brush.Color:=clwhite;
end;

procedure TfrMapsList.MapListDblClick(Sender: TObject);
begin
  Button15Click(Sender);
end;

procedure TfrMapsList.UpdateList;
var
  VPrevIndex: Integer;
  i: integer;
  VMapType: TMapType;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  VPrevIndex := MapList.ItemIndex;
  MapList.Clear;
  VGUIDList := GState.MapType.GUIConfigList.OrderedMapGUIDList;
  for i := 0 to VGUIDList.Count - 1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := GState.MapType.FullMapsSet.GetMapTypeByGUID(VGUID).MapType;

    MapList.AddItem(VMapType.GUIConfig.Name.Value, nil);
    MapList.Items.Item[i].Data:=VMapType;
    MapList.Items.Item[i].SubItems.Add(VMapType.StorageConfig.NameInCache);
    if VMapType.Abilities.IsLayer then begin
      MapList.Items.Item[i].SubItems.Add(SAS_STR_Layers+'\'+VMapType.GUIConfig.ParentSubMenu.Value);
    end else begin
      MapList.Items.Item[i].SubItems.Add(SAS_STR_Maps+'\'+VMapType.GUIConfig.ParentSubMenu.Value);
    end;
    MapList.Items.Item[i].SubItems.Add(ShortCutToText(VMapType.GUIConfig.HotKey));
    MapList.Items.Item[i].SubItems.Add(VMapType.Zmp.FileName);
    if VMapType.GUIConfig.Enabled then begin
      MapList.Items.Item[i].SubItems.Add(SAS_STR_Yes)
    end else begin
      MapList.Items.Item[i].SubItems.Add(SAS_STR_No)
    end;
  end;
  if MapList.Items.Count > 0 then begin
    if MapList.Items.Count > VPrevIndex then begin
      MapList.ItemIndex := VPrevIndex;
    end else begin
      MapList.Items.Item[0].Selected := True;
    end;
  end;
end;

end.

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

unit fr_MapsList;

interface

{$IF (CompilerVersion <= 19)}
  // Adds implementation of event TListView.OnItemChecked for Delphi 2007 and older
  {$DEFINE USE_CUSTOM_LV}
{$IFEND}

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  ComCtrls,
  {$IFDEF USE_CUSTOM_LV}
  CommCtrl,
  {$ENDIF}
  StdCtrls,
  ExtCtrls,
  i_LanguageManager,
  i_MapTypeGUIConfigList,
  i_MapTypeConfigModalEdit,
  i_InternalBrowser,
  i_MapTypeSet,
  u_CommonFormAndFrameParents;

type
  {$IFDEF USE_CUSTOM_LV}
  TLVCheckedItemEvent = procedure(Sender: TObject; Item: TListItem) of object;

  TListView = class(ComCtrls.TListView)
  private
    FOnItemChecked: TLVCheckedItemEvent;
  protected
    procedure CNNotify(var AMsg: TWMNotifyLV); message CN_NOTIFY;
  public
    property OnItemChecked: TLVCheckedItemEvent read FOnItemChecked write FOnItemChecked;
  end;
  {$ENDIF}

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
    procedure MapListChange(
      Sender: TObject;
      Item: TListItem;
      Change: TItemChange
    );
    procedure MapListCustomDrawSubItem(
      Sender: TCustomListView;
      Item: TListItem;
      SubItem: Integer;
      State: TCustomDrawState;
      var DefaultDraw: Boolean
    );
    procedure MapListCustomDrawItem(
      Sender: TCustomListView;
      Item: TListItem;
      State: TCustomDrawState;
      var DefaultDraw: Boolean
    );
    procedure MapListColumnClick(
      Sender: TObject;
      Column: TListColumn
    );
    procedure MapListItemChecked(
      Sender: TObject;
      Item: TListItem
    );
  private
    FChanged: Boolean;
    FMapTypeEditor: IMapTypeConfigModalEdit;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FInternalBrowser: IInternalBrowser;
    FPrevSortColumnIndex: Integer;
    FIsPrevSortReversed: Boolean;
    procedure UpdateList;
    procedure DoCustomSort(
      const ACol: Integer;
      const AReverse: Boolean
    );
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
  ExplorerSort,
  c_InternalBrowser,
  i_GUIDListStatic,
  i_MapType,
  u_ResStrings;

{$R *.dfm}

{$IFDEF USE_CUSTOM_LV}
procedure TListView.CNNotify(var AMsg: TWMNotifyLV);
begin
  inherited;
  if Assigned(FOnItemChecked) and (AMsg.NMHdr.code = LVN_ITEMCHANGED) then begin
    if (AMsg.NMListView.uChanged = LVIF_STATE) and
       ( ((AMsg.NMListView.uOldState and LVIS_STATEIMAGEMASK) shr 12)
         <> ((AMsg.NMListView.uNewState and LVIS_STATEIMAGEMASK) shr 12)) then
    begin
      // check box state changed
      FOnItemChecked(Self, Items[AMsg.NMListView.iItem]);
    end;
  end;
end;
{$ENDIF}

{ TfrMapsList }

procedure ExchangeItems(
  lv: TListView;
  const i, j: Integer
);
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
      lv.Items.Item[j].Selected := true;
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
  VMapType: IMapType;
begin
  if not FChanged or not ((FPrevSortColumnIndex = 0) and not FIsPrevSortReversed) then begin
    Exit;
  end;
  FGUIConfigList.LockWrite;
  try
    For i := 0 to MapList.Items.Count - 1 do begin
      VMapType := IMapType(MapList.Items.Item[i].data);
      if VMapType <> nil then begin
        VMapType.GUIConfig.SortIndex := i + 1;
      end;
    end;
  finally
    FGUIConfigList.UnlockWrite;
  end;
  UpdateList;
end;

procedure TfrMapsList.btnMapInfoClick(Sender: TObject);
var
  VMapType: IMapType;
  VUrl: string;
  VItem: TListItem;
begin
  VItem := MapList.Selected;
  if VItem <> nil then begin
    VMapType := IMapType(VItem.Data);
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
  If (MapList.Selected <> nil) and (MapList.Selected.Index < MapList.Items.Count - 1) then begin
    ExchangeItems(MapList, MapList.Selected.Index, MapList.Selected.Index + 1);
    FChanged := True;
  end;
end;

procedure TfrMapsList.Button12Click(Sender: TObject);
begin
  If (MapList.Selected <> nil) and (MapList.Selected.Index > 0) then begin
    ExchangeItems(MapList, MapList.Selected.Index, MapList.Selected.Index - 1);
    FChanged := True;
  end;
end;

procedure TfrMapsList.Button15Click(Sender: TObject);
var
  VMapType: IMapType;
  VItem: TListItem;
begin
  VItem := MapList.Selected;
  if VItem <> nil then begin
    VMapType := IMapType(VItem.Data);
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
  MapList.DoubleBuffered := True;
  FPrevSortColumnIndex := 0;
  FIsPrevSortReversed := False;

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
  MapList.OnItemChecked := nil;
  FChanged := False;
  UpdateList;
  DoCustomSort(FPrevSortColumnIndex, FIsPrevSortReversed);
  MapList.OnItemChecked := Self.MapListItemChecked;
end;

procedure TfrMapsList.MapListChange(
  Sender: TObject;
  Item: TListItem;
  Change: TItemChange
);
var
  VMapType: IMapType;
begin
  if Self.Visible then begin
    VMapType := IMapType(Item.Data);
    if VMapType <> nil then begin
      btnMapInfo.Enabled := VMapType.GUIConfig.InfoUrl.Value <> '';
    end;
  end;
end;

procedure DoItemCustomDraw(
  Sender: TCustomListView;
  Item: TListItem
);
var
  VMapType: IMapType;
begin
  if Item = nil then begin
    Exit;
  end;
  VMapType := IMapType(Item.Data);
  if VMapType <> nil then begin
    if Item.Index mod 2 = 1 then begin
      Sender.canvas.brush.Color := cl3DLight;
    end else begin
      Sender.canvas.brush.Color := clwhite;
    end;
  end;
end;

procedure TfrMapsList.MapListCustomDrawItem(
  Sender: TCustomListView;
  Item: TListItem;
  State: TCustomDrawState;
  var DefaultDraw: Boolean
);
begin
  DoItemCustomDraw(Sender, Item);
end;

procedure TfrMapsList.MapListCustomDrawSubItem(
  Sender: TCustomListView;
  Item: TListItem;
  SubItem: Integer;
  State: TCustomDrawState;
  var DefaultDraw: Boolean
);
begin
  DoItemCustomDraw(Sender, Item);
end;

procedure TfrMapsList.MapListDblClick(Sender: TObject);
begin
  Button15Click(Sender);
end;

procedure TfrMapsList.UpdateList;

  procedure SetSubItem(
    AItem: TListItem;
    AIndex: Integer;
    const AValue: string
  );
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

  procedure UpdateItem(
    AItem: TListItem;
    const AMapType: IMapType
  );
  var
    VValue: string;
  begin
    AItem.Checked := AMapType.GUIConfig.Enabled;
    AItem.Caption := IntToStr(AItem.Index + 1);
    AItem.Data := Pointer(AMapType);

    VValue := AMapType.GUIConfig.Name.Value;
    SetSubItem(AItem, 0, VValue);

    VValue := AMapType.StorageConfig.NameInCache;
    SetSubItem(AItem, 1, VValue);

    if AMapType.Zmp.IsLayer then begin
      VValue := SAS_STR_Layers + '\' + AMapType.GUIConfig.ParentSubMenu.Value;
    end else begin
      VValue := SAS_STR_Maps + '\' + AMapType.GUIConfig.ParentSubMenu.Value;
    end;
    SetSubItem(AItem, 2, VValue);

    VValue := ShortCutToText(AMapType.GUIConfig.HotKey);
    SetSubItem(AItem, 3, VValue);

    VValue := AMapType.Zmp.FileName;
    SetSubItem(AItem, 4, VValue);
  end;

var
  VPrevSelectedIndex: Integer;
  i: integer;
  VMapType: IMapType;
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
      VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID);
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

function SortByCol(AItem1, AItem2: TListItem; ACol: Integer): Integer; stdcall;
begin
  if ACol = 0 then begin
    Result := CompareStringOrdinal(AItem1.Caption, AItem2.Caption);
  end else if AItem1.SubItems.Count > ACol - 1 then begin
    if AItem2.SubItems.Count > ACol - 1 then begin
      Result := CompareStringOrdinal(AItem1.SubItems[ACol - 1], AItem2.SubItems[ACol - 1]);
    end else begin
      Result := 1;
    end;
  end else begin
    Result := -1;
  end;
end;

function SortByColReverse(AItem1, AItem2: TListItem; ACol: Integer): Integer; stdcall;
begin
  Result := SortByCol(AItem2, AItem1, ACol);
end;

procedure TfrMapsList.DoCustomSort(const ACol: Integer; const AReverse: Boolean);
begin
  if AReverse then begin
    MapList.CustomSort(@SortByColReverse, ACol);
  end else begin
    MapList.CustomSort(@SortByCol, ACol);
  end;
  FPrevSortColumnIndex := ACol;
  FIsPrevSortReversed := AReverse;
end;

procedure TfrMapsList.MapListColumnClick(Sender: TObject; Column: TListColumn);
var
  VCol: Integer;
  VReverse: Boolean;
begin
  VCol := Column.Index;
  VReverse := (VCol = FPrevSortColumnIndex) and not FIsPrevSortReversed;
  DoCustomSort(VCol, VReverse);
end;

procedure TfrMapsList.MapListItemChecked(
  Sender: TObject;
  Item: TListItem
);
var
  VMapType: IMapType;
begin
  VMapType := IMapType(Item.Data);
  if VMapType <> nil then begin
    VMapType.GUIConfig.Enabled := Item.Checked;
  end;
end;

end.

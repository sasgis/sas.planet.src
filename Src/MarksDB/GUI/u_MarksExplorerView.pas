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

unit u_MarksExplorerView;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  Controls,
  Menus,
  ExtCtrls,
  VirtualTrees,
  VirtualTrees.Types,
  VirtualTrees.BaseTree,
  i_Category,
  i_MarkId,
  i_MarkDb,
  i_MarkCategory,
  i_MarkCategoryList,
  i_MarkCategoryTree,
  i_VectorDataItemSimple,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_MarksExplorerFilter,
  u_MarksExplorerHelper,
  u_MarkDbGUIHelper;

type
  TMarksExplorerView = class
  public type
    TTreeCounts = record
      Selected: Integer;
      Visible: Integer;
      Total: Integer;
    end;
    PTreeCounts = ^TTreeCounts;

    TScrollInfo = record
      MarksOffsetY: Integer;
      CategoriesOffsetY: Integer;
    end;
    PScrollInfo = ^TScrollInfo;

    TCategoriesTreeState = record
      Expanded: TCategoryInfoArray;
      Selected: TCategoryInfo;
    end;
    PCategoriesTreeState = ^TCategoriesTreeState;
  private type
    TCategoryNodeData = record
      Category: IMarkCategoryTree;
    end;
    PCategoryNodeData = ^TCategoryNodeData;

    TMarksTreeState = record
      IsFocused: Boolean;
      FocusedIndex: Cardinal;
      SelectedIndex: array of Cardinal;
      ScrollOffsetY: TDimension;
      TreeItemsCount: Cardinal;
    end;
    PMarksTreeState = ^TMarksTreeState;
  private
    FMarkDBGUI: TMarkDbGUIHelper;
    FMarksExplorerFilter: IMarksExplorerFilter;

    FCategoryTree: TVirtualStringTree;
    FMarksTree: TVirtualStringTree;

    FMarkCategoryTree: IMarkCategoryTree;
    FMarksList: IInterfaceListStatic;
    FMarksName: TStringDynArray;

    FCategoriesCount: TTreeCounts;
    FMarksCount: TTreeCounts;

    FCascadeChange: Boolean;

    FOnMarksViewChange: TNotifyEvent;
    FOnMarksViewDblClick: TNotifyEvent;
    FOnCategoritesViewChange: TNotifyEvent;

    // Category tree event handlers
    procedure CategoryTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure CategoryTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure CategoryTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure CategoryTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure CategoryTreeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CategoryTreeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure CategoryTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure CategoryTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: TVTDragDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure CategoryTreeChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure CategoryTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);

    // Marks tree event handlers
    procedure MarksTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MarksTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure MarksTreeDblClick(Sender: TObject);
    procedure MarksTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MarksTreeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure MarksTreeChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure MarksTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);

    // Helper methods
    procedure ModifyCategoryVisible(Node: PVirtualNode);

    function GetNodeCategory(const ANode: PVirtualNode): IMarkCategory; inline;
    function GetNodeMarkId(const ANode: PVirtualNode): IMarkId; inline;
    function GetNodeMarkName(const ANode: PVirtualNode; var AName: string): Boolean; inline;

    procedure GetMarksTreeState(const AState: PMarksTreeState);
    procedure RestoreMarksTreeState(const AState: PMarksTreeState);

    function GetScrollInfo: TScrollInfo;
    procedure SetScrollInfo(const AValue: TScrollInfo);

    procedure UpdateCategoryTree(const ACateriesInfo: PCategoriesTreeState; const AScrollInfo: PScrollInfo);
  public
    procedure Reset;

    procedure UpdateFull(const ACateriesInfo: PCategoriesTreeState = nil; const AScrollInfo: PScrollInfo = nil);
    procedure UpdateMarksList(const AScrollInfo: PScrollInfo = nil);

    function GetSelectedCategory: IMarkCategory;
    function GetSelectedMarkId: IMarkId;
    function GetSelectedMarkFull: IVectorDataItem;
    function GetSelectedMarksIdList(const ASorted: Boolean = False): IInterfaceListStatic;
    function GetSelectedMarksIdArray(const ASorted: Boolean = False): TArrayOfMarkId;

    procedure SelectAllVisibleMarks;
    procedure RevertSelectedMarks;

    function GetCategoriesCount: PTreeCounts;
    function GetMarksCount: PTreeCounts;

    procedure GetCategoriesTreeState(const AState: PCategoriesTreeState);
    procedure RestoreCategoriesTreeState(const AState: PCategoriesTreeState);

    property CascadeChange: Boolean read FCascadeChange write FCascadeChange;
    property ScrollInfo: TScrollInfo read GetScrollInfo write SetScrollInfo;

    property OnMarksViewChange: TNotifyEvent read FOnMarksViewChange write FOnMarksViewChange;
    property OnMarksViewDblClick: TNotifyEvent read FOnMarksViewDblClick write FOnMarksViewDblClick;

    property OnCategoritesViewChange: TNotifyEvent read FOnCategoritesViewChange write FOnCategoritesViewChange;
  public
    constructor Create(
      const ACategoriesPanel: TPanel;
      const ACategoriesPopupMenu: TPopupMenu;
      const AMarksPanel: TPanel;
      const AMarksPopupMenu: TPopupMenu;
      const AMarkDBGUI: TMarkDbGUIHelper;
      const AMarksExplorerFilter: IMarksExplorerFilter
    );
    destructor Destroy; override;
  end;

implementation

uses
  CityHash, // inlining
  ExplorerSort,
  i_MarkCategoryDB,
  i_MarkCategoryFactory,
  u_MarkCategoryList,
  u_InterfaceListSimple,
  u_Dialogs,
  u_SortFunc;

constructor TMarksExplorerView.Create(
  const ACategoriesPanel: TPanel;
  const ACategoriesPopupMenu: TPopupMenu;
  const AMarksPanel: TPanel;
  const AMarksPopupMenu: TPopupMenu;
  const AMarkDBGUI: TMarkDbGUIHelper;
  const AMarksExplorerFilter: IMarksExplorerFilter
);
begin
  inherited Create;

  FMarkDBGUI := AMarkDBGUI;
  FMarksExplorerFilter := AMarksExplorerFilter;

  // Create category tree
  FCategoryTree := TVirtualStringTree.Create(nil);
  FCategoryTree.Parent := ACategoriesPanel;
  FCategoryTree.Align := alClient;
  FCategoryTree.NodeDataSize := SizeOf(TCategoryNodeData);
  with FCategoryTree.TreeOptions do begin
    MiscOptions := MiscOptions + [toCheckSupport];
    SelectionOptions := SelectionOptions + [toFullRowSelect];
    PaintOptions := PaintOptions + [toShowRoot, toShowTreeLines, toUseExplorerTheme];
  end;
  FCategoryTree.DragMode := dmAutomatic;
  FCategoryTree.DragType := dtVCL;
  FCategoryTree.Header.Options := FCategoryTree.Header.Options - [hoVisible];
  FCategoryTree.PopupMenu := ACategoriesPopupMenu;

  FCategoryTree.OnInitNode := Self.CategoryTreeInitNode;
  FCategoryTree.OnFreeNode := Self.CategoryTreeFreeNode;
  FCategoryTree.OnGetText := Self.CategoryTreeGetText;
  FCategoryTree.OnChange := Self.CategoryTreeChange;
  FCategoryTree.OnKeyUp := Self.CategoryTreeKeyUp;
  FCategoryTree.OnContextPopup := Self.CategoryTreeContextPopup;
  FCategoryTree.OnDragOver := Self.CategoryTreeDragOver;
  FCategoryTree.OnDragDrop := Self.CategoryTreeDragDrop;
  FCategoryTree.OnChecking := Self.CategoryTreeChecking;
  FCategoryTree.OnChecked := Self.CategoryTreeChecked;

  // Create marks tree
  FMarksTree := TVirtualStringTree.Create(nil);
  FMarksTree.Parent := AMarksPanel;
  FMarksTree.Align := alClient;
  FMarksTree.NodeDataSize := 0;
  with FMarksTree.TreeOptions do begin
    MiscOptions := MiscOptions + [toCheckSupport];
    SelectionOptions := SelectionOptions + [toFullRowSelect, toMultiSelect];
    PaintOptions := PaintOptions - [toShowRoot, toShowTreeLines];
  end;
  FMarksTree.DragMode := dmAutomatic;
  FMarksTree.DragType := dtVCL;
  FMarksTree.Header.Options := FMarksTree.Header.Options - [hoVisible];
  FMarksTree.PopupMenu := AMarksPopupMenu;

  FMarksTree.OnInitNode := Self.MarksTreeInitNode;
  FMarksTree.OnFreeNode := nil;
  FMarksTree.OnGetText := Self.MarksTreeGetText;
  FMarksTree.OnDblClick := Self.MarksTreeDblClick;
  FMarksTree.OnKeyDown := Self.MarksTreeKeyDown;
  FMarksTree.OnContextPopup := Self.MarksTreeContextPopup;
  FMarksTree.OnChecking := Self.MarksTreeChecking;
  FMarksTree.OnChecked := Self.MarksTreeChecked;
end;

destructor TMarksExplorerView.Destroy;
begin
  FreeAndNil(FMarksTree);
  FreeAndNil(FCategoryTree);

  inherited Destroy;
end;

procedure TMarksExplorerView.Reset;
begin
  FCategoryTree.OnChange := nil;
  try
    FMarksTree.Clear;
    FCategoryTree.Clear;

    FMarksList := nil;
    FMarkCategoryTree := nil;

    FMarksName := nil;
  finally
    FCategoryTree.OnChange := Self.CategoryTreeChange;
  end;
end;

function TMarksExplorerView.GetCategoriesCount: PTreeCounts;
begin
  Result := @FCategoriesCount;
  Result.Selected := FCategoryTree.SelectedCount;
end;

function TMarksExplorerView.GetMarksCount: PTreeCounts;
begin
  Result := @FMarksCount;
  Result.Selected := FMarksTree.SelectedCount;
end;

{$REGION 'Category tree event handlers'}
procedure TMarksExplorerView.CategoryTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  VNodeData: PCategoryNodeData;
  VParentData: PCategoryNodeData;
  VParentCategory: IMarkCategoryTree;
begin
  VNodeData := Sender.GetNodeData(Node);

  if ParentNode = nil then begin
    // Root node
    if Assigned(FMarkCategoryTree) and (Node.Index < Cardinal(FMarkCategoryTree.SubItemCount)) then begin
      VNodeData.Category := FMarkCategoryTree.SubItem[Node.Index];
    end;
  end else begin
    // Child node
    VParentData := Sender.GetNodeData(ParentNode);
    if Assigned(VParentData) then begin
      VParentCategory := VParentData.Category;
      if Assigned(VParentCategory) and (Node.Index < Cardinal(VParentCategory.SubItemCount)) then begin
        VNodeData.Category := VParentCategory.SubItem[Node.Index];
      end;
    end;
  end;

  // Set child count for tree expansion
  if Assigned(VNodeData.Category) then begin
    Sender.ChildCount[Node] := VNodeData.Category.SubItemCount;
  end;

  Node.CheckType := ctCheckBox;

  // Set checkbox state based on visibility
  if VNodeData.Category.MarkCategory.Visible then begin
    Node.CheckState := csCheckedNormal;
  end else begin
    Node.CheckState := csUncheckedNormal;
  end;
end;

procedure TMarksExplorerView.CategoryTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  VNodeData: PCategoryNodeData;
begin
  VNodeData := Sender.GetNodeData(Node);
  if Assigned(VNodeData) then begin
    VNodeData.Category := nil;
  end;
end;

procedure TMarksExplorerView.CategoryTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  VName: string;
  VNodeData: PCategoryNodeData;
begin
  VNodeData := Sender.GetNodeData(Node);
  if Assigned(VNodeData) and Assigned(VNodeData.Category) then begin
    VName := VNodeData.Category.Name;
    if VName = '' then begin
      VName := '(NoName)';
    end;
    CellText := VName;
  end else begin
    CellText := '';
  end;
end;

procedure TMarksExplorerView.CategoryTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  UpdateMarksList;
end;

procedure TMarksExplorerView.CategoryTreeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  VNode: PVirtualNode;
begin
  // Note: VK_F2 (Edit) and VK_DEL (Delete) keys are handled by parent

  if Key = VK_SPACE then begin
    VNode := FCategoryTree.GetFirstSelected;
    if VNode <> nil then begin
      ModifyCategoryVisible(VNode);
    end;
    Key := 0;
  end;
end;

procedure TMarksExplorerView.CategoryTreeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  VNode: PVirtualNode;
begin
  if (MousePos.X >= 0) and (MousePos.Y >= 0) then begin
    VNode := FCategoryTree.GetNodeAt(MousePos.X, MousePos.Y);
    if VNode <> nil then begin
      FCategoryTree.Selected[VNode] := True;
      FCategoryTree.FocusedNode := VNode;
    end;
  end;
end;

procedure TMarksExplorerView.CategoryTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
  State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  I: Integer;
  VNode: PVirtualNode;
  VList: IMarkCategoryList;
  VNewName: string;
  VOldCategory, VOldSubCategory, VNewCategory: IMarkCategory;
begin
  Accept := (Source = FMarksTree) or (Source = FCategoryTree);
  if not Accept then begin
    Exit;
  end;

  VNode := Sender.GetNodeAt(Pt.X, Pt.Y);
  Accept := (VNode <> nil) and (VNode <> Sender.GetFirstSelected);
  if not Accept then begin
    Exit;
  end;

  if Source = FCategoryTree then begin
    VOldCategory := GetNodeCategory(Sender.GetFirstSelected);
    VNewCategory := GetNodeCategory(VNode);
    Accept := (VOldCategory <> nil) and (VNewCategory <> nil);
    if not Accept then begin
      Exit;
    end;

    VNewName := VOldCategory.Name;
    I := LastDelimiter('\', VNewName);
    VNewName := VNewCategory.Name + '\' + Copy(VNewName, I + 1, MaxInt);
    Accept := FMarkDBGUI.MarksDb.CategoryDB.GetFirstCategoryByName(VNewName) = nil;
    if not Accept then begin
      Exit;
    end;

    // Forbid the transfer of a category to its own child category
    VList := FMarkDBGUI.MarksDb.CategoryDB.GetSubCategoryListForCategory(VOldCategory);
    if VList <> nil then begin
      for I := 0 to VList.Count - 1 do begin
        VOldSubCategory := IMarkCategory(VList[I]);
        if VNewCategory.IsSame(VOldSubCategory) then begin
          Accept := False;
          Break;
        end;
      end;
    end;
  end else
  if Source = FMarksTree then begin
    if Mode <> dmOnNode then begin
      Accept := False;
    end;
  end;
end;

procedure TMarksExplorerView.CategoryTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: TVTDragDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer;
  Mode: TDropMode);

  function MoveMarks(
    const ADestNode: PVirtualNode;
    const AMarks: IInterfaceListStatic
  ): Boolean;
  var
    I: Integer;
    VMarkIdListNew: IInterfaceListSimple;
    VCategoryNew: ICategory;
    VMark: IVectorDataItem;
  begin
    Result := False;
    if AMarks <> nil then begin
      VCategoryNew := GetNodeCategory(ADestNode);
      if VCategoryNew <> nil then begin
        VMarkIdListNew := TInterfaceListSimple.Create;
        for I := 0 to AMarks.Count - 1 do begin
          VMark := FMarkDBGUI.MarksDb.MarkDb.GetMarkByID(IMarkId(AMarks.Items[I]));
          VMarkIdListNew.Add(FMarkDBGUI.MarksDb.MarkDb.Factory.ReplaceCategory(VMark, VCategoryNew));
        end;
        FMarkDBGUI.MarksDb.MarkDb.UpdateMarkList(AMarks, VMarkIdListNew.MakeStaticAndClear);
        Result := True;
      end;
    end;
  end;

  procedure MoveCategory(const ADestNode, ASourceNode: PVirtualNode);
  var
    I: Integer;
    VNewName: string;
    VDestCategory: ICategory;
    VOldCategory, VNewCategory: IMarkCategory;
  begin
    VDestCategory := GetNodeCategory(ADestNode);
    VOldCategory := GetNodeCategory(ASourceNode);
    if (VDestCategory <> nil) and (VOldCategory <> nil) then begin
      // Get new name for category
      VNewName := VOldCategory.Name;
      I := LastDelimiter('\', VNewName);
      VNewName := VDestCategory.Name + '\' + Copy(VNewName, I + 1, MaxInt);

      // Modify category with all subcategories
      VNewCategory := FMarkDBGUI.MarksDb.CategoryDB.Factory.Modify(
        VOldCategory,
        VNewName,
        VOldCategory.Visible,
        VOldCategory.AfterScale,
        VOldCategory.BeforeScale
      );
      FMarkDBGUI.MarksDb.CategoryDB.UpdateCategory(VOldCategory, VNewCategory);
    end;
  end;

var
  VNode: PVirtualNode;
begin
  VNode := Sender.GetNodeAt(Pt.X, Pt.Y);
  if (VNode <> nil) and (VNode <> Sender.GetFirstSelected) then begin
    if Source = FMarksTree then begin
      // Replace category for all selected marks in selected category
      if MoveMarks(VNode, GetSelectedMarksIdList) then begin
        FMarksTree.ClearSelection;
      end;
    end else if Source = FCategoryTree then begin
      // Move selected category with all subcategories to new category
      MoveCategory(VNode, Sender.GetFirstSelected);
    end;
  end;
end;

procedure TMarksExplorerView.CategoryTreeChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
begin
  Sender.ClearSelection;
end;

procedure TMarksExplorerView.CategoryTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  Sender.Selected[Node] := True;
  Sender.FocusedNode := Node;

  ModifyCategoryVisible(Node);
end;

procedure TMarksExplorerView.ModifyCategoryVisible(Node: PVirtualNode);
var
  I: Integer;
  VNewVisible: Boolean;
  VCategoryOld: IMarkCategory;
  VCategoryNew: IMarkCategory;
  VOldList, VNewList: IMarkCategoryList;
  VTempOld, VTempNew: IInterfaceListSimple;
  VFactory: IMarkCategoryFactory;
  VCategoryDB: IMarkCategoryDB;
begin
  VCategoryOld := GetNodeCategory(Node);
  if VCategoryOld = nil then begin
    Exit;
  end;

  VNewVisible := Node.CheckState = csCheckedNormal;

  VTempOld := TInterfaceListSimple.Create;
  VTempNew := TInterfaceListSimple.Create;
  VCategoryDB := FMarkDBGUI.MarksDb.CategoryDB;
  VFactory := VCategoryDB.Factory;

  // Change Visible property for current category node
  if VCategoryOld.Visible <> VNewVisible then begin
    VCategoryNew := VFactory.ModifyVisible(VCategoryOld, VNewVisible);
    VTempOld.Add(VCategoryOld);
    VTempNew.Add(VCategoryNew);
  end;

  // Change Visible property for child categories if cascade mode is enabled
  if FCascadeChange then begin
    VOldList := VCategoryDB.GetSubCategoryListForCategory(VCategoryOld);
    if Assigned(VOldList) then begin
      for I := 0 to VOldList.Count - 1 do begin
        VCategoryOld := VOldList.Items[I];
        if VCategoryOld.Visible <> VNewVisible then begin
          VCategoryNew := VFactory.ModifyVisible(VCategoryOld, VNewVisible);
          VTempOld.Add(VCategoryOld);
          VTempNew.Add(VCategoryNew);
        end;
      end;
    end;
  end;

  // Apply changes
  if VTempOld.Count > 1 then begin
    VOldList := TMarkCategoryList.Build(VTempOld.MakeStaticAndClear);
    VNewList := TMarkCategoryList.Build(VTempNew.MakeStaticAndClear);
    VCategoryDB.UpdateCategoryList(VOldList, VNewList);
  end else
  if VTempOld.Count = 1 then begin
    VCategoryDB.UpdateCategory(IMarkCategory(VTempOld.Items[0]), IMarkCategory(VTempNew.Items[0]));
  end;
end;
{$ENDREGION}

{$REGION 'Marks tree event handlers'}
procedure TMarksExplorerView.MarksTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  VMarkId: IMarkId;
begin
  VMarkId := GetNodeMarkId(Node);

  if not Assigned(VMarkId) then begin
    Exit;
  end;

  Node.CheckType := ctCheckBox;

  if FMarkDBGUI.MarksDb.MarkDb.GetMarkVisibleByID(VMarkId) then begin
    Node.CheckState := csCheckedNormal;
  end else begin
    Node.CheckState := csUncheckedNormal;
  end;
end;

procedure TMarksExplorerView.MarksTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  VMarkId: IMarkId;
begin
  if GetNodeMarkName(Node, CellText) then begin
    Exit;
  end;

  VMarkId := GetNodeMarkId(Node);

  if Assigned(VMarkId) then begin
    CellText := FMarkDBGUI.MakeMarkCaption(VMarkId);
  end else begin
    CellText := '';
  end;
end;

procedure TMarksExplorerView.MarksTreeDblClick(Sender: TObject);
begin
  if Assigned(FOnMarksViewDblClick) then begin
    FOnMarksViewDblClick(nil);
  end;
end;

procedure TMarksExplorerView.MarksTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  VMarkIdList: IInterfaceListStatic;
begin
  // Note: VK_F2 (Edit) and VK_DEL (Delete) keys are handled by parent

  if Key = VK_SPACE then begin
    VMarkIdList := GetSelectedMarksIdList;
    if (VMarkIdList <> nil) and (VMarkIdList.Count > 0) then begin
      FMarkDBGUI.MarksDb.MarkDb.ToggleMarkVisibleByIDList(VMarkIdList);
    end;
    Key := 0;
  end else
  if Key = VK_RETURN then begin
    MarksTreeDblClick(Sender);
    Key := 0;
  end;
end;

procedure TMarksExplorerView.MarksTreeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  VNode: PVirtualNode;
begin
  if (MousePos.X >= 0) and (MousePos.Y >= 0) then begin
    VNode := FMarksTree.GetNodeAt(MousePos.X, MousePos.Y);
    if VNode <> nil then begin
      if not FMarksTree.Selected[VNode] then begin
        FMarksTree.ClearSelection;
        FMarksTree.Selected[VNode] := True;
        FMarksTree.FocusedNode := VNode;
      end;
    end;
  end;
end;

procedure TMarksExplorerView.MarksTreeChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
begin
  Sender.ClearSelection;
end;

procedure TMarksExplorerView.MarksTreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  VMarkId: IMarkId;
begin
  Sender.Selected[Node] := True;
  Sender.FocusedNode := Node;

  VMarkId := GetNodeMarkId(Node);
  if Assigned(VMarkId) then begin
    FMarkDBGUI.MarksDb.MarkDb.SetMarkVisibleByID(VMarkId, Node.CheckState = csCheckedNormal);
  end;
end;
{$ENDREGION}

procedure TMarksExplorerView.UpdateFull(const ACateriesInfo: PCategoriesTreeState; const AScrollInfo: PScrollInfo);
begin
  UpdateCategoryTree(ACateriesInfo, AScrollInfo);
  UpdateMarksList(AScrollInfo);
end;

procedure TMarksExplorerView.UpdateCategoryTree(const ACateriesInfo: PCategoriesTreeState; const AScrollInfo: PScrollInfo);
var
  VCategoryDB: IMarkCategoryDB;
  VCategoryList: IMarkCategoryList;
  VState: PCategoriesTreeState;
  VStateLocal: TCategoriesTreeState;
  VScrollOffsetY: TDimension;
begin
  FCategoryTree.OnChange := nil;
  try
    FCategoryTree.BeginUpdate;
    try
      if AScrollInfo <> nil then begin
        VScrollOffsetY := AScrollInfo.CategoriesOffsetY;
      end else begin
        VScrollOffsetY := FCategoryTree.OffsetY;
      end;

      if ACateriesInfo = nil then begin
        VState := @VStateLocal;
        GetCategoriesTreeState(VState);
      end else begin
        VState := ACateriesInfo;
      end;

      FCategoryTree.Clear;

      VCategoryDB := FMarkDBGUI.MarksDb.CategoryDB;
      VCategoryList := VCategoryDB.GetCategoriesList;

      FMarkCategoryTree := VCategoryDB.CategoryListToStaticTree(VCategoryList);

      if Assigned(FMarkCategoryTree) then begin
        FCategoryTree.RootNodeCount := FMarkCategoryTree.SubItemCount;
      end;

      RestoreCategoriesTreeState(VState);
      FCategoryTree.OffsetY := VScrollOffsetY;
    finally
      FCategoryTree.EndUpdate;
    end;
  finally
    FCategoryTree.OnChange := Self.CategoryTreeChange;
  end;

  if Assigned(FOnCategoritesViewChange) then begin
    FOnCategoritesViewChange(nil);
  end;
end;

procedure TMarksExplorerView.UpdateMarksList(const AScrollInfo: PScrollInfo);

  procedure PrepareMarksName(const AMarksList: IInterfaceListStatic; var AMarksName: TStringDynArray);
  var
    I: Integer;
  begin
    SetLength(AMarksName, AMarksList.Count);
    for I := 0 to AMarksList.Count - 1 do begin
      AMarksName[I] := FMarkDBGUI.MakeMarkCaption(IMarkId(AMarksList.Items[I]));
    end;
  end;

  procedure SortMarksByName(var AMarksList: IInterfaceListStatic; var AMarksName: TStringDynArray);
  var
    VList: IInterfaceListSimple;
  begin
    VList := TInterfaceListSimple.Create;
    VList.AddListStatic(AMarksList);
    SortInterfaceListByStringMeasure(VList, AMarksName); // sort marks list and names array
    AMarksList := VList.MakeStaticAndClear;
  end;

  procedure UpdateMarksCount(const AVisibleCount, ATotalCount: Integer);
  begin
    FMarksCount.Selected := FMarksTree.SelectedCount;
    FMarksCount.Visible := AVisibleCount;
    FMarksCount.Total := ATotalCount;
  end;

const
  CMaxSortCount = 10 * 1000;
var
  I: Integer;
  VMarkDb: IMarkDb;
  VCategory: IMarkCategory;
  VState: TMarksTreeState;
  VMarksCount: Integer;
  VVisibleCount: Integer;
begin
  FMarksTree.BeginUpdate;
  try
    GetMarksTreeState(@VState);

    if AScrollInfo <> nil then begin
      VState.ScrollOffsetY := AScrollInfo.MarksOffsetY;
    end;

    FMarksTree.Clear;

    FMarksList := nil;
    VCategory := GetSelectedCategory;

    VMarksCount := -1;
    VVisibleCount := 0;

    if Assigned(VCategory) then begin
      VMarkDb := FMarkDBGUI.MarksDb.MarkDb;
      FMarksList := FMarksExplorerFilter.Process(VMarkDb, VMarkDb.GetMarkIdListByCategory(VCategory));

      if Assigned(FMarksList) then begin

        VMarksCount := FMarksList.Count;

        if (VMarksCount > 1) and (VMarksCount < CMaxSortCount) then begin
          PrepareMarksName(FMarksList, FMarksName);
          SortMarksByName(FMarksList, FMarksName);
        end else begin
          FMarksName := nil;
        end;

        FMarksTree.RootNodeCount := FMarksList.Count;

        for I := 0 to VMarksCount - 1 do begin
          if VMarkDb.GetMarkVisibleByID(IMarkId(FMarksList.Items[I])) then begin
            Inc(VVisibleCount);
          end;
        end;
      end;
    end;

    RestoreMarksTreeState(@VState);
    UpdateMarksCount(VVisibleCount, VMarksCount);
  finally
    FMarksTree.EndUpdate;
  end;

  if Assigned(FOnMarksViewChange) then begin
    FOnMarksViewChange(nil);
  end;
end;

function TMarksExplorerView.GetNodeCategory(const ANode: PVirtualNode): IMarkCategory;
var
  VNodeData: PCategoryNodeData;
begin
  Result := nil;
  if Assigned(ANode) then begin
    VNodeData := FCategoryTree.GetNodeData(ANode);
    if Assigned(VNodeData) and Assigned(VNodeData.Category) then begin
      Result := VNodeData.Category.MarkCategory;
    end;
  end;
end;

function TMarksExplorerView.GetNodeMarkId(const ANode: PVirtualNode): IMarkId;
begin
  if Assigned(FMarksList) and Assigned(ANode) and (ANode.Index < Cardinal(FMarksList.Count)) then begin
    Result := IMarkId(FMarksList.Items[ANode.Index]);
  end else begin
    Result := nil;
    Assert(False);
  end;
end;

function TMarksExplorerView.GetNodeMarkName(const ANode: PVirtualNode; var AName: string): Boolean;
begin
  Result := False;
  if Assigned(FMarksList) and Assigned(ANode) and (ANode.Index < Cardinal(FMarksList.Count)) then begin
    if Length(FMarksName) = FMarksList.Count then begin
      AName := FMarksName[ANode.Index];
      Result := AName <> '';
    end;
  end else begin
    Assert(False);
  end;
end;

function TMarksExplorerView.GetSelectedCategory: IMarkCategory;
var
  VNode: PVirtualNode;
begin
  VNode := FCategoryTree.GetFirstSelected;
  if VNode <> nil then begin
    Result := GetNodeCategory(VNode);
  end else begin
    Result := nil;
  end;
end;

function TMarksExplorerView.GetSelectedMarkFull: IVectorDataItem;
var
  VMarkId: IMarkId;
begin
  VMarkId := GetSelectedMarkId;
  if VMarkId <> nil then begin
    Result := FMarkDBGUI.MarksDb.MarkDb.GetMarkByID(VMarkId);
  end else begin
    Result := nil;
  end;
end;

function TMarksExplorerView.GetSelectedMarksIdList(const ASorted: Boolean): IInterfaceListStatic;
var
  I: Integer;
  VArr: TArrayOfMarkId;
  VList: IInterfaceListSimple;
begin
  Result := nil;
  VArr := GetSelectedMarksIdArray(ASorted);
  if Length(VArr) > 0 then begin
    VList := TInterfaceListSimple.Create;
    for I := 0 to Length(VArr) - 1 do begin
      VList.Add(VArr[I]);
    end;
    Result := VList.MakeStaticAndClear;
  end;
end;

function TMarksExplorerView.GetSelectedMarksIdArray(const ASorted: Boolean): TArrayOfMarkId;
var
  I: Integer;
  VArr: TArrayOfMarkId;
  VNode: PVirtualNode;
  VName: string;
  VSortedList: TStringList;
  VCount: Integer;
begin
  VCount := FMarksTree.SelectedCount;
  SetLength(Result, VCount);

  if VCount = 0 then begin
    Exit;
  end;

  I := 0;
  VNode := FMarksTree.GetFirstSelected;
  while VNode <> nil do begin
    if I < VCount then begin
      Result[I] := GetNodeMarkId(VNode);
      if Assigned(Result[I]) then begin
        Inc(I);
      end;
      VNode := FMarksTree.GetNextSelected(VNode);
    end else begin
      // This appears to be a VirtualTreeView bug
      I := 0;
      VNode := FMarksTree.GetFirstSelected;
      while VNode <> nil do begin
        Inc(I);
        VNode := FMarksTree.GetNextSelected(VNode);
      end;
      ShowErrorMessage(Format(
        'Virtual Treeview internal error!' + #13#10 + #13#10 +
        'MarksTree.SelectedCount (%d) is less than the number of selected nodes (%d)', [VCount, I+1]
      ));
      FMarksTree.ClearSelection;
      Result := nil;
      Exit;
    end;
  end;

  SetLength(Result, I);

  if not ASorted or (Length(Result) < 2) then begin
    Exit;
  end;

  VArr := Copy(Result);

  VSortedList := TStringList.Create;
  try
    VSortedList.Duplicates := dupAccept;
    VSortedList.BeginUpdate;
    try
      for I := 0 to Length(VArr) - 1 do begin
        VName := FMarkDBGUI.MakeMarkCaption(VArr[I]);
        VSortedList.AddObject(VName, TObject(UIntPtr(I)));
      end;
      VSortedList.CustomSort(StringListCompare);
    finally
      VSortedList.EndUpdate;
    end;

    for I := 0 to VSortedList.Count - 1 do begin
      Result[I] := VArr[UIntPtr(VSortedList.Objects[I])];
    end;
  finally
    VSortedList.Free;
  end;
end;

function TMarksExplorerView.GetSelectedMarkId: IMarkId;
var
  VNode: PVirtualNode;
begin
  VNode := FMarksTree.GetFirstSelected;
  if VNode <> nil then begin
    Result := GetNodeMarkId(VNode);
  end else begin
    Result := nil;
  end;
end;

procedure TMarksExplorerView.SelectAllVisibleMarks;
var
  VNode: PVirtualNode;
begin
  FMarksTree.BeginUpdate;
  try
    VNode := FMarksTree.GetFirst;
    while VNode <> nil do begin
      FMarksTree.Selected[VNode] := (VNode.CheckState = csCheckedNormal);
      VNode := FMarksTree.GetNext(VNode);
    end;
  finally
    FMarksTree.EndUpdate;
  end;
end;

procedure TMarksExplorerView.RevertSelectedMarks;
var
  VNode: PVirtualNode;
  VWasSelected: Boolean;
begin
  FMarksTree.BeginUpdate;
  try
    VNode := FMarksTree.GetFirst;
    while VNode <> nil do begin
      VWasSelected := FMarksTree.Selected[VNode];
      FMarksTree.Selected[VNode] := not VWasSelected;
      VNode := FMarksTree.GetNext(VNode);
    end;
  finally
    FMarksTree.EndUpdate;
  end;
end;

procedure TMarksExplorerView.GetCategoriesTreeState(const AState: PCategoriesTreeState);
var
  I: Integer;
  VLen: Integer;
  VNode: PVirtualNode;
  VNodeData: PCategoryNodeData;
  VNodeIndex: Integer;
  VInfo: PCategoryInfo;
  VSelected: PCategoryInfo;
begin
  I := 0;
  VLen := 64;
  SetLength(AState.Expanded, VLen);

  VSelected := @AState.Selected;

  VSelected.UID := CEmptyUniqueID;
  VSelected.Category := nil;

  VNodeIndex := 0;
  VNode := FCategoryTree.GetFirst;

  while VNode <> nil do begin
    VNodeData := nil;

    // Expanded
    if FMarksTree.Expanded[VNode] then begin
      VNodeData := FCategoryTree.GetNodeData(VNode);
      if (VNodeData <> nil) and (VNodeData.Category <> nil) then begin
        if I >= VLen then begin
          VLen := GrowCollection(VLen, I);
          SetLength(AState.Expanded, VLen);
        end;

        VInfo := @AState.Expanded[I];

        VInfo.Index := VNodeIndex;
        VInfo.Category := VNodeData.Category.MarkCategory;
        VInfo.UID := GetCategoryUniqueID(VInfo.Category);

        if VInfo.UID <> CEmptyUniqueID then begin
          Inc(I);
        end;
      end;
    end;

    // Selected
    if (VSelected.UID = CEmptyUniqueID) and FMarksTree.Selected[VNode] then begin
      if VNodeData = nil then begin
        VNodeData := FCategoryTree.GetNodeData(VNode);
      end;
      if (VNodeData <> nil) and (VNodeData.Category <> nil) then begin
        VSelected.Index := VNodeIndex;
        VSelected.Category := VNodeData.Category.MarkCategory;
        VSelected.UID := GetCategoryUniqueID(VSelected.Category);
      end;
    end;

    VNode := FCategoryTree.GetNext(VNode);
    Inc(VNodeIndex);
  end;

  SetLength(AState.Expanded, I);
end;

procedure TMarksExplorerView.RestoreCategoriesTreeState(const AState: PCategoriesTreeState);
var
  I: Integer;
  VNode: PVirtualNode;
  VNodeData: PCategoryNodeData;
  VNodeIndex: Integer;
  VExpandedLen: Integer;
  VExpandedFoundCount: Integer;
  VExpandedMatched: array of Boolean;
  VSelectedFound: Boolean;
  VMatched: Boolean;
  VNodeUID: Cardinal;
  VInfo: PCategoryInfo;
  VSelected: PCategoryInfo;
begin
  VSelected := @AState.Selected;

  VExpandedLen := Length(AState.Expanded);
  VSelectedFound := (VSelected.UID = CEmptyUniqueID);

  if (VExpandedLen = 0) and VSelectedFound then begin
    Exit;
  end;

  SetLength(VExpandedMatched, VExpandedLen);
  VExpandedFoundCount := 0;

  VNodeIndex := 0;
  VNode := FCategoryTree.GetFirst;

  while VNode <> nil do begin
    VNodeData := FCategoryTree.GetNodeData(VNode);
    if (VNodeData <> nil) and (VNodeData.Category <> nil) then begin

      VNodeUID := CEmptyUniqueID;

      // Expanded
      if (VNode.ChildCount > 0) and (VExpandedFoundCount < VExpandedLen) then begin

        VNodeUID := GetCategoryUniqueID(VNodeData.Category.MarkCategory);

        for I := 0 to VExpandedLen - 1 do begin

          if VExpandedMatched[I] then begin
            Continue;
          end;

          VInfo := @AState.Expanded[I];

          VMatched :=
            (VInfo.Index = VNodeIndex) and
            (VInfo.UID = VNodeUID);

          if not VMatched then begin
            VMatched :=
              (VInfo.Category <> nil) and
               VInfo.Category.IsSame(VNodeData.Category.MarkCategory);
          end;

          if VMatched then begin
            FCategoryTree.Expanded[VNode] := True;
            VExpandedMatched[I] := True;
            Inc(VExpandedFoundCount);
            Break;
          end;
        end;
      end;

      // Selected
      if not VSelectedFound then begin
        if VNodeUID = CEmptyUniqueID then begin
          VNodeUID := GetCategoryUniqueID(VNodeData.Category.MarkCategory);
        end;

        VSelectedFound :=
          (VSelected.Index = VNodeIndex) and
          (VSelected.UID = VNodeUID);

        if not VSelectedFound then begin
          VSelectedFound :=
            (VSelected.Category <> nil) and
             VSelected.Category.IsSame(VNodeData.Category.MarkCategory);
        end;

        if VSelectedFound then begin
          FCategoryTree.ClearSelection;
          FCategoryTree.Selected[VNode] := True;
        end;
      end;
    end;

    if VSelectedFound and (VExpandedFoundCount >= VExpandedLen) then begin
      Break;
    end;

    VNode := FCategoryTree.GetNext(VNode);
    Inc(VNodeIndex);
  end;
end;

procedure TMarksExplorerView.GetMarksTreeState(const AState: PMarksTreeState);
var
  I: Integer;
  VLen: Integer;
  VNode: PVirtualNode;
begin
  VNode := FMarksTree.FocusedNode;

  AState.IsFocused := VNode <> nil;
  if AState.IsFocused then begin
    AState.FocusedIndex := VNode.Index;
  end;

  I := 0;
  VLen := FMarksTree.SelectedCount;
  SetLength(AState.SelectedIndex, VLen);

  for VNode in FMarksTree.SelectedNodes do begin
    if I < VLen then begin
      AState.SelectedIndex[I] := VNode.Index;
      Inc(I);
    end;
  end;

  AState.ScrollOffsetY := FMarksTree.OffsetY;
  AState.TreeItemsCount := FMarksTree.TotalCount;
end;

procedure TMarksExplorerView.RestoreMarksTreeState(const AState: PMarksTreeState);
var
  I: Integer;
  VNode: PVirtualNode;
  VSelectedLen: Integer;
  VFocusedFound: Boolean;
begin
  FMarksTree.ClearSelection;
  FMarksTree.OffsetY := AState.ScrollOffsetY;

  if AState.TreeItemsCount <> FMarksTree.TotalCount then begin
    Exit;
  end;

  VSelectedLen := Length(AState.SelectedIndex);
  VFocusedFound := not AState.IsFocused;

  if (VSelectedLen = 0) and (VFocusedFound) then begin
    Exit;
  end;

  I := 0;
  VNode := FMarksTree.GetFirst;
  while VNode <> nil do begin
    // restore selection (search algorithm requires the AState.SelectedIndex array to be pre-sorted)
    if (I < VSelectedLen) and (VNode.Index = AState.SelectedIndex[I]) then begin
      FMarksTree.Selected[VNode] := True;
      Inc(I);
    end;

    // restore focus
    if (not VFocusedFound) and (VNode.Index = AState.FocusedIndex) then begin
      FMarksTree.FocusedNode := VNode;
      VFocusedFound := True;
    end;

    if VFocusedFound and (I >= VSelectedLen) then begin
      Break;
    end;

    VNode := FMarksTree.GetNext(VNode);
  end;
end;

function TMarksExplorerView.GetScrollInfo: TScrollInfo;
begin
  Result.MarksOffsetY := FMarksTree.OffsetY;
  Result.CategoriesOffsetY := FCategoryTree.OffsetY;
end;

procedure TMarksExplorerView.SetScrollInfo(const AValue: TScrollInfo);
begin
  FMarksTree.OffsetY := AValue.MarksOffsetY;
  FCategoryTree.OffsetY := AValue.CategoriesOffsetY;
end;

end.

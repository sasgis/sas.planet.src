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

unit frm_MarksExplorer;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  ComCtrls,
  Messages,
  Menus,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst,
  ExtCtrls,
  ImgList,
  TB2Dock,
  TB2Toolbar,
  TB2Item,
  TBX,
  TBXControls,
  i_JclNotify,
  u_CommonFormAndFrameParents,
  t_GeoTypes,
  i_LanguageManager,
  i_ViewPortState,
  i_NavigationToPoint,
  i_UsedMarksConfig,
  i_MapViewGoto,
  i_WindowPositionConfig,
  i_ImportFile,
  i_MarksSimple,
  i_MarkCategory,
  i_StaticTreeItem,
  u_MarksDbGUIHelper;

type
  TfrmMarksExplorer = class(TFormWitghLanguageManager)
    grpMarks: TGroupBox;
    MarksListBox: TCheckListBox;
    grpCategory: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    OpenDialog1: TOpenDialog;
    CategoryTreeView: TTreeView;
    imlStates: TImageList;
    pnlButtons: TPanel;
    pnlMainWithButtons: TPanel;
    pnlMain: TPanel;
    splCatMarks: TSplitter;
    btnExport: TTBXButton;
    PopupExport: TPopupMenu;
    NExportAll: TMenuItem;
    NExportVisible: TMenuItem;
    btnImport: TTBXButton;
    rgMarksShowMode: TRadioGroup;
    TBXDockMark: TTBXDock;
    TBXToolbar1: TTBXToolbar;
    btnEditMark: TTBXItem;
    btnDelMark: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    btnGoToMark: TTBXItem;
    btnOpSelectMark: TTBXItem;
    btnNavOnMark: TTBXItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    btnSaveMark: TTBXItem;
    TBXDockCategory: TTBXDock;
    TBXToolbar2: TTBXToolbar;
    BtnAddCategory: TTBXItem;
    BtnDelKat: TTBXItem;
    TBXSeparatorItem3: TTBXSeparatorItem;
    BtnEditCategory: TTBXItem;
    btnExportCategory: TTBXItem;
    lblMarksCount: TStaticText;
    tbpmnCategories: TTBXPopupMenu;
    tbitmAddCategory: TTBXItem;
    tbitmEditCategory: TTBXItem;
    tbitmDeleteCategory: TTBXItem;
    tbsprtCategoriesPopUp: TTBXSeparatorItem;
    tbitmExportCategory: TTBXItem;
    tbpmnMarks: TTBXPopupMenu;
    tbitmAddMark: TTBXItem;
    tbitmEditMark: TTBXItem;
    tbitmDeleteMark: TTBXItem;
    tbsprtMarksPopUp: TTBXSeparatorItem;
    tbitmExportMark: TTBXItem;
    btnAddMark: TTBXItem;
    pnlMarksBottom: TPanel;
    pnlBottom: TPanel;
    lblReadOnly: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure MarksListBoxClickCheck(Sender: TObject);
    procedure BtnDelKatClick(Sender: TObject);
    procedure BtnEditCategoryClick(Sender: TObject);
    procedure MarksListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CategoryTreeViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CategoryTreeViewKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CategoryTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure btnExportClick(Sender: TObject);
    procedure btnExportCategoryClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnEditMarkClick(Sender: TObject);
    procedure btnDelMarkClick(Sender: TObject);
    procedure btnGoToMarkClick(Sender: TObject);
    procedure btnOpSelectMarkClick(Sender: TObject);
    procedure btnNavOnMarkClick(Sender: TObject);
    procedure btnSaveMarkClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure tbitmAddCategoryClick(Sender: TObject);
    procedure tbitmAddMarkClick(Sender: TObject);
    Procedure FormMove(Var Msg: TWMMove); Message WM_MOVE;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgMarksShowModeClick(Sender: TObject);
  private
    FMapGoto: IMapViewGoto;
    FCategoryList: IInterfaceList;
    FMarksList: IInterfaceList;
    FMarkDBGUI: TMarksDbGUIHelper;
    FImportFileByExt: IImportFile;
    FMarksShowConfig: IUsedMarksConfig;
    FWindowConfig: IWindowPositionConfig;
    FViewPortState: IViewPortState;
    FNavToPoint: INavigationToPoint;

    FCategoryDBListener: IListener;
    FMarksDBListener: IListener;
    FMarksShowConfigListener: IListener;
    FConfigListener: IListener;
    FMarksSystemStateListener: IListener;

    procedure OnCategoryDbChanged;
    procedure OnMarksDbChanged;
    procedure OnMarksShowConfigChanged;
    procedure OnConfigChange;
    procedure OnMarkSystemStateChanged;
    procedure UpdateCategoryTree;
    function GetSelectedCategory: IMarkCategory;
    procedure UpdateMarksList;
    function GetSelectedMarkId: IMarkId;
    function GetSelectedMarkFull: IMark;
    function GetSelectedMarksIdList: IInterfaceList;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AImportFileByExt: IImportFile;
      const AViewPortState: IViewPortState;
      const ANavToPoint: INavigationToPoint;
      const AWindowConfig: IWindowPositionConfig;
      const AMarksShowConfig: IUsedMarksConfig;
      AMarkDBGUI: TMarksDbGUIHelper;
      const AMapGoto: IMapViewGoto
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  t_CommonTypes,
  i_ImportConfig,
  i_MarkTemplate,
  i_MarksFactoryConfig,
  u_NotifyEventListener;

{$R *.dfm}

constructor TfrmMarksExplorer.Create(
  const ALanguageManager: ILanguageManager;
  const AImportFileByExt: IImportFile;
  const AViewPortState: IViewPortState;
  const ANavToPoint: INavigationToPoint;
  const AWindowConfig: IWindowPositionConfig;
  const AMarksShowConfig: IUsedMarksConfig;
  AMarkDBGUI: TMarksDbGUIHelper;
  const AMapGoto: IMapViewGoto
);
begin
  inherited Create(ALanguageManager);
  FMarkDBGUI := AMarkDBGUI;
  FMapGoto := AMapGoto;
  FImportFileByExt := AImportFileByExt;
  FWindowConfig := AWindowConfig;
  FMarksShowConfig := AMarksShowConfig;
  FViewPortState := AViewPortState;
  FNavToPoint := ANavToPoint;
  MarksListBox.MultiSelect:=true;
  FCategoryDBListener := TNotifyNoMmgEventListener.Create(Self.OnCategoryDbChanged);
  FMarksDBListener := TNotifyNoMmgEventListener.Create(Self.OnMarksDbChanged);
  FMarksShowConfigListener := TNotifyNoMmgEventListener.Create(Self.OnMarksShowConfigChanged);
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FMarksSystemStateListener := TNotifyNoMmgEventListener.Create(Self.OnMarkSystemStateChanged);
end;

procedure TfrmMarksExplorer.FormCreate(Sender: TObject);
begin
  if IsRectEmpty(FWindowConfig.BoundsRect) then begin
    FWindowConfig.SetWindowPosition(Self.BoundsRect);
  end;
end;

destructor TfrmMarksExplorer.Destroy;
begin
  if FWindowConfig <> nil then begin
    if FConfigListener <> nil then begin
      FWindowConfig.ChangeNotifier.Remove(FConfigListener);
      FConfigListener := nil;
    end;
  end;
  inherited;
end;

procedure TfrmMarksExplorer.UpdateCategoryTree;
  procedure AddTreeSubItems(
    const ATree: IStaticTreeItem;
    const ASelectedCategory: ICategory;
    AParentNode: TTreeNode;
    ATreeItems: TTreeNodes
  );
  var
    i: Integer;
    VTree: IStaticTreeItem;
    VNode: TTreeNode;
    VCategory: IMarkCategory;
    VName: string;
  begin
    for i := 0 to ATree.SubItemCount - 1 do begin
      VTree := ATree.SubItem[i];
      VName := VTree.Name;
      if VName = '' then begin
        VName := '(NoName)';
      end;
      VNode := ATreeItems.AddChildObject(AParentNode, VName, nil);
      VNode.StateIndex:=0;
      if Supports(VTree.Data, IMarkCategory, VCategory) then begin
        VNode.Data := Pointer(VCategory);
        if VCategory.Visible then begin
          VNode.StateIndex := 1;
        end else begin
          VNode.StateIndex := 2;
        end;
        if VCategory.IsSame(ASelectedCategory) then begin
          VNode.Selected := True;
        end;
      end;
      AddTreeSubItems(VTree, ASelectedCategory, VNode, ATreeItems);
    end;
  end;
var
  VTree: IStaticTreeItem;
  VSelectedCategory: ICategory;
begin
  VSelectedCategory := GetSelectedCategory;
  FCategoryList := FMarkDBGUI.MarksDB.CategoryDB.GetCategoriesList;
  VTree := FMarkDBGUI.MarksDB.CategoryListToStaticTree(FCategoryList);
  CategoryTreeView.OnChange:=nil;
  try
    CategoryTreeView.Items.BeginUpdate;
    try
      CategoryTreeView.SortType := stNone;
      CategoryTreeView.Items.Clear;
      AddTreeSubItems(VTree, VSelectedCategory, nil, CategoryTreeView.Items);
      CategoryTreeView.SortType:=stText;
    finally
      CategoryTreeView.Items.EndUpdate;
    end;
  finally
    CategoryTreeView.OnChange := Self.CategoryTreeViewChange;
  end;
  UpdateMarksList;
end;

procedure TfrmMarksExplorer.UpdateMarksList;
var
  VCategory: IMarkCategory;
  VMarkId: IMarkID;
  i: Integer;
  VSelectedIndex: Integer;
  VTopIndex: Integer;
begin
  VSelectedIndex := MarksListBox.ItemIndex;
  VTopIndex := MarksListBox.TopIndex;
  MarksListBox.Clear;
  FMarksList := nil;
  VCategory := GetSelectedCategory;
  if (VCategory <> nil) then begin
    FMarksList := FMarkDBGUI.MarksDb.MarksDb.GetMarskIdListByCategory(VCategory);
    MarksListBox.Items.BeginUpdate;
    try
      FMarkDBGUI.MarksListToStrings(FMarksList, MarksListBox.Items);
      for i:=0 to MarksListBox.Count-1 do begin
        VMarkId := IMarkId(Pointer(MarksListBox.Items.Objects[i]));
        MarksListBox.Checked[i] := FMarkDBGUI.MarksDB.MarksDb.GetMarkVisible(VMarkId);
      end;
      if VSelectedIndex > 0 then begin
        if VSelectedIndex < MarksListBox.Count then begin
          MarksListBox.ItemIndex := VSelectedIndex;
        end;
      end;
      if VTopIndex > 0 then begin
        if VTopIndex < MarksListBox.Count then begin
          MarksListBox.TopIndex := VTopIndex;
        end;
      end;
      lblMarksCount.Caption:='('+inttostr(MarksListBox.Count)+')';
    finally
      MarksListBox.Items.EndUpdate;
    end;
  end;
end;

function TfrmMarksExplorer.GetSelectedCategory: IMarkCategory;
begin
  Result := nil;
  if CategoryTreeView.Selected <> nil then begin
    Result := IMarkCategory(CategoryTreeView.Selected.Data);
  end;
end;

function TfrmMarksExplorer.GetSelectedMarkFull: IMark;
var
  VMarkId: IMarkId;
begin
  Result := nil;
  VMarkId := GetSelectedMarkId;
  if VMarkId <> nil then begin
    Result := FMarkDBGUI.MarksDb.MarksDb.GetMarkByID(VMarkId);
  end;
end;

function TfrmMarksExplorer.GetSelectedMarksIdList: IInterfaceList;
var
  i:integer;
begin
  Result := TInterfaceList.Create;
  for i:=0 to MarksListBox.Count-1 do begin
    if MarksListBox.selected[i] then begin
      Result.Add(IMarkId(Pointer(MarksListBox.Items.Objects[i])))
    end;
  end;
  if Result.Count=0 then begin
    Result:=nil;
  end;
end;

function TfrmMarksExplorer.GetSelectedMarkId: IMarkId;
var
  VIndex: Integer;
begin
  Result := nil;
  VIndex := MarksListBox.ItemIndex;
  if VIndex>=0 then begin
    Result := IMarkId(Pointer(MarksListBox.Items.Objects[VIndex]));
  end;
end;

procedure TfrmMarksExplorer.MarksListBoxClickCheck(Sender: TObject);
var
  VMark: IMarkId;
begin
  VMark := GetSelectedMarkId;
  if VMark <> nil then begin
    FMarkDBGUI.MarksDB.MarksDb.SetMarkVisibleByID(
      VMark,
      MarksListBox.Checked[MarksListBox.ItemIndex]
    );
  end;
end;

procedure TfrmMarksExplorer.btnImportClick(Sender: TObject);
var
  VImportConfig: IImportConfig;
  VFileName: string;
begin
  If (OpenDialog1.Execute) then begin
    VFileName := OpenDialog1.FileName;
    if (FileExists(VFileName)) then begin
      VImportConfig := FMarkDBGUI.EditModalImportConfig;
      if VImportConfig <> nil then begin
        FImportFileByExt.ProcessImport(VFileName, VImportConfig);
      end;
    end;
  end;
end;

procedure TfrmMarksExplorer.BtnDelKatClick(Sender: TObject);
var
  VCategory: IMarkCategory;
begin
  VCategory := GetSelectedCategory;
  if VCategory <> nil then begin
    FMarkDBGUI.DeleteCategoryModal(VCategory, Self.Handle);
  end;
end;

procedure TfrmMarksExplorer.btnExportClick(Sender: TObject);
var
  VCategoryList: IInterfaceList;
  VOnlyVisible: Boolean;
begin
  VOnlyVisible := (TComponent(Sender).tag = 1);
  if VOnlyVisible then begin
    VCategoryList := FMarkDBGUI.MarksDb.GetVisibleCategoriesIgnoreZoom;
  end else begin
    VCategoryList := FMarkDBGUI.MarksDb.CategoryDB.GetCategoriesList;
  end;
  FMarkDBGUI.ExportCategoryList(VCategoryList, not VOnlyVisible);
end;

procedure TfrmMarksExplorer.btnDelMarkClick(Sender: TObject);
var
  VMarkIdList: IInterfaceList;
begin
  VMarkIdList:=GetSelectedMarksIdList;
  if VMarkIdList <> nil then begin
    FMarkDBGUI.DeleteMarksModal(VMarkIdList, Self.Handle);
  end;
end;

procedure TfrmMarksExplorer.btnEditMarkClick(Sender: TObject);
var
  VMarkIdList: IInterfaceList;
  VMarksList: IInterfaceList;
  VMark: IMark;
  VImportConfig: IImportConfig;
  VMarkPoint: IMarkPoint;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
  VMarkId: IMarkId;
  i:integer;
begin
  VMarkIdList:=GetSelectedMarksIdList;
  if VMarkIdList <> nil then begin
    if VMarkIdList.Count=1 then begin
      VMark:=FMarkDBGUI.MarksDB.MarksDb.GetMarkByID(IMarkId(VMarkIdList[0]));
      VMark := FMarkDBGUI.EditMarkModal(VMark, False);
      if VMark <> nil then begin
        FMarkDBGUI.MarksDb.MarksDb.UpdateMark(VMarkIdList[0], VMark);
      end;
    end else begin
      VImportConfig := FMarkDBGUI.MarksMultiEditModal(GetSelectedCategory);
      if (VImportConfig <> nil) then begin
        VMarkIdList:=GetSelectedMarksIdList;
        if (VMarkIdList <> nil) then begin
          VMarksList:=TInterfaceList.Create;
          for i := 0 to VMarkIdList.Count - 1 do begin
            VMarkId := IMarkId(VMarkIdList[i]);
            VMark:=FMarkDBGUI.MarksDB.MarksDb.GetMarkByID(VMarkId);
            if Supports(VMark, IMarkPoint, VMarkPoint) then begin
              if VImportConfig.TemplateNewPoint<>nil then begin
                VMark:=FMarkDBGUI.MarksDB.MarksDb.Factory.ModifyPoint(
                  VMarkPoint,
                  VMarkPoint.Name,
                  FMarkDBGUI.MarksDB.MarksDb.GetMarkVisible(VMark),
                  VImportConfig.TemplateNewPoint.Pic,
                  VImportConfig.TemplateNewPoint.Category,
                  VMarkPoint.Desc,
                  VMarkPoint.Point,
                  VImportConfig.TemplateNewPoint.TextColor,
                  VImportConfig.TemplateNewPoint.TextBgColor,
                  VImportConfig.TemplateNewPoint.FontSize,
                  VImportConfig.TemplateNewPoint.MarkerSize
                );
              end;
            end else if Supports(VMark, IMarkLine, VMarkLine) then begin
              if VImportConfig.TemplateNewLine<>nil then begin
                VMark:=FMarkDBGUI.MarksDB.MarksDb.Factory.ModifyLine(
                  VMarkLine,
                  VMarkLine.Name,
                  FMarkDBGUI.MarksDB.MarksDb.GetMarkVisible(VMark),
                  VImportConfig.TemplateNewLine.Category,
                  VMarkLine.Desc,
                  VMarkLine.Line,
                  VImportConfig.TemplateNewLine.LineColor,
                  VImportConfig.TemplateNewLine.LineWidth
                );
              end;
            end else if Supports(VMark, IMarkPoly, VMarkPoly) then begin
              if VImportConfig.TemplateNewPoly<>nil then begin
                VMark:=FMarkDBGUI.MarksDB.MarksDb.Factory.ModifyPoly(
                  VMarkPoly,
                  VMarkPoly.Name,
                  FMarkDBGUI.MarksDB.MarksDb.GetMarkVisible(VMark),
                  VImportConfig.TemplateNewPoly.Category,
                  VMarkPoly.Desc,
                  VMarkPoly.Line,
                  VImportConfig.TemplateNewPoly.BorderColor,
                  VImportConfig.TemplateNewPoly.FillColor,
                  VImportConfig.TemplateNewPoly.LineWidth
                );
              end;
            end;
            if VMark <> nil then begin
              VMarksList.Add(VMark);
            end;
          end;
          if (VMarksList<>nil) then begin
            FMarkDBGUI.MarksDb.MarksDb.UpdateMarksList(VMarkIdList, VMarksList);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMarksExplorer.btnGoToMarkClick(Sender: TObject);
var
  VMark: IMark;
begin
  VMark := GetSelectedMarkFull;
  if VMark <> nil then begin
    FMapGoto.GotoPos(VMark.GetGoToLonLat, FViewPortState.GetCurrentZoom);
  end;
end;

procedure TfrmMarksExplorer.btnNavOnMarkClick(Sender: TObject);
var
  VMark: IMark;
  LL: TDoublePoint;
begin
  if (btnNavOnMark.Checked) then begin
    VMark := GetSelectedMarkFull;
    if VMark <> nil then begin
      LL := VMark.GetGoToLonLat;
      FNavToPoint.StartNavToMark(VMark as IMarkId, LL);
    end else begin
      btnNavOnMark.Checked:=not btnNavOnMark.Checked;
    end;
  end else begin
    FNavToPoint.StopNav;
  end;
end;

procedure TfrmMarksExplorer.btnOpSelectMarkClick(Sender: TObject);
var
  VMark: IMark;
begin
  VMark := GetSelectedMarkFull;
  if VMark <> nil then begin
    if FMarkDBGUI.OperationMark(VMark, FViewPortState.GetVisualCoordConverter.ProjectionInfo) then begin
      ModalResult := mrOk;
    end;
  end;
end;

procedure TfrmMarksExplorer.btnSaveMarkClick(Sender: TObject);
var
  VMark: IMark;
begin
  VMark := GetSelectedMarkFull;
  if VMark <> nil then begin
    FMarkDBGUI.ExportMark(VMark);
  end;
end;

procedure TfrmMarksExplorer.CategoryTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateMarksList;
end;

procedure TfrmMarksExplorer.CategoryTreeViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  VCategoryOld: IMarkCategory;
  VCategoryNew: IMarkCategory;
begin
  If key=VK_DELETE then begin
    VCategoryOld := GetSelectedCategory;
    if VCategoryOld <> nil then begin
      FMarkDBGUI.DeleteCategoryModal(VCategoryOld, Self.Handle);
    end;
  end;

  if Key=VK_SPACE then begin
    VCategoryOld := GetSelectedCategory;
    if VCategoryOld <> nil then begin
      if CategoryTreeView.Selected.StateIndex = 1 then begin
        VCategoryNew := FMarkDBGUI.MarksDB.CategoryDB.Factory.ModifyVisible(VCategoryOld, False);
        CategoryTreeView.Selected.StateIndex:=2;
      end else begin
        VCategoryNew := FMarkDBGUI.MarksDB.CategoryDB.Factory.ModifyVisible(VCategoryOld, True);
        CategoryTreeView.Selected.StateIndex:=1;
      end;
      if not VCategoryOld.IsEqual(VCategoryNew) then begin
        FCategoryList.Remove(VCategoryOld);
        FMarkDBGUI.MarksDb.CategoryDB.UpdateCategory(VCategoryOld, VCategoryNew);
        FCategoryList.Add(VCategoryNew);
        CategoryTreeView.Selected.Data := Pointer(VCategoryNew);
      end;
    end;
  end;
end;

procedure TfrmMarksExplorer.CategoryTreeViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  VCategoryOld: IMarkCategory;
  VCategoryNew: IMarkCategory;
  VTreeNode: TTreeNode;
begin
  if htOnStateIcon in CategoryTreeView.GetHitTestInfoAt(X,Y) then begin
    VTreeNode := CategoryTreeView.GetNodeAt(X,Y);
    VCategoryOld := IMarkCategory(VTreeNode.Data);
    if VCategoryOld <> nil then begin
      if VTreeNode.StateIndex=1 then begin
        VCategoryNew := FMarkDBGUI.MarksDB.CategoryDB.Factory.ModifyVisible(VCategoryOld, False);
        VTreeNode.StateIndex:=2;
      end else begin
        VCategoryNew := FMarkDBGUI.MarksDB.CategoryDB.Factory.ModifyVisible(VCategoryOld, True);
        VTreeNode.StateIndex:=1;
      end;
      if not VCategoryOld.IsEqual(VCategoryNew) then begin
        FCategoryList.Remove(VCategoryOld);
        FMarkDBGUI.MarksDb.CategoryDB.UpdateCategory(VCategoryOld, VCategoryNew);
        FCategoryList.Add(VCategoryNew);
        VTreeNode.Data := Pointer(VCategoryNew);
      end;
    end;
  end;
end;

procedure TfrmMarksExplorer.BtnEditCategoryClick(Sender: TObject);
var
  VCategoryOld: IMarkCategory;
  VCategoryNew: IMarkCategory;
begin
  VCategoryOld := GetSelectedCategory;
  if VCategoryOld <> nil then begin
    VCategoryNew := FMarkDBGUI.EditCategoryModal(VCategoryOld, False);
    if VCategoryNew <> nil then begin
      FMarkDBGUI.MarksDb.CategoryDB.UpdateCategory(VCategoryOld, VCategoryNew);
    end;
  end;
end;

procedure TfrmMarksExplorer.btnExportCategoryClick(Sender: TObject);
var
  VCategory: IMarkCategory;
  VOnlyVisible: Boolean;
begin
  VCategory := GetSelectedCategory;
  if VCategory<>nil then begin
    VOnlyVisible := (TComponent(Sender).tag = 1);
    FMarkDBGUI.ExportCategory(VCategory, not VOnlyVisible);
  end;
end;

procedure TfrmMarksExplorer.MarksListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  VMarkId: IMarkId;
begin
  If key=VK_DELETE then begin
    VMarkId := GetSelectedMarkId;
    if VMarkId <> nil then begin
      FMarkDBGUI.DeleteMarkModal(VMarkId, Self.Handle);
    end;
  end;
end;

procedure TfrmMarksExplorer.OnCategoryDbChanged;
begin
  UpdateCategoryTree;
end;

procedure TfrmMarksExplorer.OnConfigChange;
var
  VRect: TRect;
begin
  VRect := FWindowConfig.BoundsRect;
  if not EqualRect(BoundsRect, VRect) then begin
    BoundsRect := VRect;
  end;
end;

procedure TfrmMarksExplorer.OnMarksDbChanged;
begin
  UpdateMarksList;
end;

procedure TfrmMarksExplorer.OnMarksShowConfigChanged;
var
  VMarksConfig: IUsedMarksConfigStatic;
begin
  VMarksConfig := FMarksShowConfig.GetStatic;
  if VMarksConfig.IsUseMarks then begin
    if VMarksConfig.IgnoreCategoriesVisible and VMarksConfig.IgnoreMarksVisible then begin
      rgMarksShowMode.ItemIndex := 1;
    end else begin
      rgMarksShowMode.ItemIndex := 0;
    end;
  end else begin
    rgMarksShowMode.ItemIndex := 2;
  end;
end;

procedure TfrmMarksExplorer.OnMarkSystemStateChanged;
begin
  lblReadOnly.Visible := FMarkDBGUI.MarksDB.State.GetStatic.WriteAccess = asDisabled;
end;

procedure TfrmMarksExplorer.tbitmAddCategoryClick(Sender: TObject);
var
  VCategory: IMarkCategory;
begin
  VCategory := FMarkDBGUI.MarksDB.CategoryDB.Factory.CreateNew('');
  VCategory := FMarkDBGUI.EditCategoryModal(VCategory, True);
  if VCategory <> nil then begin
    FMarkDBGUI.MarksDb.CategoryDB.UpdateCategory(nil, VCategory);
  end;
end;

procedure TfrmMarksExplorer.tbitmAddMarkClick(Sender: TObject);
var
  VLonLat: TDoublePoint;
  VPointTemplate: IMarkTemplatePoint;
  VTemplateConfig: IMarkPointTemplateConfig;
  VMark: IMark;
  VCategory: ICategory;
begin
  VLonLat := FViewPortState.GetVisualCoordConverter.GetCenterLonLat;
  VCategory := GetSelectedCategory;
  VPointTemplate := nil;
  if VCategory <> nil then begin
    VTemplateConfig := FMarkDBGUI.MarksDB.MarksFactoryConfig.PointTemplateConfig;
    VPointTemplate := VTemplateConfig.DefaultTemplate;
    VPointTemplate :=
      VTemplateConfig.CreateTemplate(
        VPointTemplate.Pic,
        VCategory,
        VPointTemplate.TextColor,
        VPointTemplate.TextBgColor,
        VPointTemplate.FontSize,
        VPointTemplate.MarkerSize
      );
  end;

  VMark := FMarkDBGUI.MarksDB.MarksDb.Factory.CreateNewPoint(VLonLat, '', '', VPointTemplate);
  VMark := FMarkDBGUI.EditMarkModal(VMark, True);
  if VMark <> nil then begin
    FMarkDBGUI.MarksDb.MarksDb.UpdateMark(nil, VMark);
  end;
end;

procedure TfrmMarksExplorer.CheckBox2Click(Sender: TObject);
var
  VNewVisible: Boolean;
begin
  if CategoryTreeView.Items.Count>0 then begin
    VNewVisible := CheckBox2.Checked;
    FMarkDBGUI.MarksDB.CategoryDB.SetAllCategoriesVisible(VNewVisible);
  end;
end;

procedure TfrmMarksExplorer.FormActivate(Sender: TObject);
begin
  OnMarksShowConfigChanged
end;

procedure TfrmMarksExplorer.FormMove(var Msg: TWMMove);
begin
  Inherited;
  if Assigned(Self.OnResize) then begin
    Self.OnResize(Self);
  end;
end;

procedure TfrmMarksExplorer.CheckBox1Click(Sender: TObject);
var
  VNewVisible: Boolean;
  VCategory: IMarkCategory;
begin
  VCategory := GetSelectedCategory;
  if VCategory <> nil then begin
    VNewVisible := CheckBox1.Checked;
    FMarkDBGUI.MarksDB.MarksDb.SetAllMarksInCategoryVisible(VCategory, VNewVisible);
  end;
end;

procedure TfrmMarksExplorer.FormHide(Sender: TObject);
begin
  Self.OnResize := nil;
  FWindowConfig.ChangeNotifier.Remove(FConfigListener);
  FMarkDBGUI.MarksDB.CategoryDB.ChangeNotifier.Remove(FCategoryDBListener);
  FMarkDBGUI.MarksDB.MarksDb.ChangeNotifier.Remove(FMarksDBListener);
  FMarksShowConfig.ChangeNotifier.Remove(FMarksShowConfigListener);
  FMarkDBGUI.MarksDB.State.ChangeNotifier.Remove(FMarksSystemStateListener);
  CategoryTreeView.OnChange:=nil;
  CategoryTreeView.Items.Clear;
  MarksListBox.Clear;
  FCategoryList := nil;
  FMarksList := nil;
end;

procedure TfrmMarksExplorer.FormResize(Sender: TObject);
begin
  if Self.WindowState = wsNormal then begin
    FWindowConfig.SetWindowPosition(BoundsRect);
  end;
end;

procedure TfrmMarksExplorer.FormShow(Sender: TObject);
begin
  UpdateCategoryTree;
  UpdateMarksList;
  btnNavOnMark.Checked:= FNavToPoint.IsActive;
  FMarkDBGUI.MarksDB.CategoryDB.ChangeNotifier.Add(FCategoryDBListener);
  FMarkDBGUI.MarksDB.MarksDb.ChangeNotifier.Add(FMarksDBListener);
  FMarksShowConfig.ChangeNotifier.Add(FMarksShowConfigListener);
  FMarkDBGUI.MarksDB.State.ChangeNotifier.Add(FMarksSystemStateListener);
  FWindowConfig.ChangeNotifier.Add(FConfigListener);
  OnConfigChange;
  OnMarkSystemStateChanged;
  Self.OnResize := FormResize;
end;

procedure TfrmMarksExplorer.rgMarksShowModeClick(Sender: TObject);
begin
  FMarksShowConfig.LockWrite;
  try
    case rgMarksShowMode.ItemIndex of
      0: begin
        FMarksShowConfig.IsUseMarks := True;
        FMarksShowConfig.IgnoreCategoriesVisible := False;
        FMarksShowConfig.IgnoreMarksVisible := False;

      end;
      1: begin
        FMarksShowConfig.IsUseMarks := True;
        FMarksShowConfig.IgnoreCategoriesVisible := True;
        FMarksShowConfig.IgnoreMarksVisible := True;
      end;
    else
      FMarksShowConfig.IsUseMarks := False;
    end;
  finally
    FMarksShowConfig.UnlockWrite;
  end;
end;

end.


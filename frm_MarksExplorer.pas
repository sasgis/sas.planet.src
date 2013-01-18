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
  Types,
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
  i_Listener,
  u_CommonFormAndFrameParents,
  i_LanguageManager,
  i_LocalCoordConverterChangeable,
  i_NavigationToPoint,
  i_UsedMarksConfig,
  i_MapViewGoto,
  i_WindowPositionConfig,
  i_ImportFile,
  i_MarksSimple,
  i_MarkCategory,
  frm_RegionProcess,
  u_MarksDbGUIHelper;

type
  TfrmMarksExplorer = class(TFormWitghLanguageManager)
    grpMarks: TGroupBox;
    grpCategory: TGroupBox;
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
    MarksListBox: TTreeView;
    tbitmMarkInfo: TTBXItem;
    Panel1: TPanel;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure BtnAddCategoryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnDelKatClick(Sender: TObject);
    procedure BtnEditCategoryClick(Sender: TObject);
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
    procedure CategoryTreeViewContextPopup(Sender: TObject; MousePos: TPoint; var
        Handled: Boolean);
    procedure FormHide(Sender: TObject);
    procedure tbitmAddCategoryClick(Sender: TObject);
    procedure tbitmAddMarkClick(Sender: TObject);
    Procedure FormMove(Var Msg: TWMMove); Message WM_MOVE;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MarksListBoxContextPopup(Sender: TObject; MousePos: TPoint; var
        Handled: Boolean);
    procedure MarksListBoxDblClick(Sender: TObject);
    procedure MarksListBoxKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure MarksListBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure rgMarksShowModeClick(Sender: TObject);
    procedure tbpmnCategoriesPopup(Sender: TObject);
    procedure tbitmMarkInfoClick(Sender: TObject);
  private
    FUseAsIndepentWindow: Boolean;
    FMapGoto: IMapViewGoto;
    FCategoryList: IInterfaceList;
    FMarksList: IInterfaceList;
    FMarkDBGUI: TMarksDbGUIHelper;
    FImportFileByExt: IImportFile;
    FMarksShowConfig: IUsedMarksConfig;
    FWindowConfig: IWindowPositionConfig;
    FViewPortState: ILocalCoordConverterChangeable;
    FNavToPoint: INavigationToPoint;

    FCategoryDBListener: IListener;
    FMarksDBListener: IListener;
    FMarksShowConfigListener: IListener;
    FConfigListener: IListener;
    FMarksSystemStateListener: IListener;
    FFormRegionProcess: TfrmRegionProcess;


    procedure OnCategoryDbChanged;
    procedure OnMarksDbChanged;
    procedure OnMarksShowConfigChanged;
    procedure OnConfigChange;
    procedure OnMarkSystemStateChanged;
    procedure UpdateCategoryTree;
    procedure CategoryTreeViewVisible(Node: TTreeNode);
    function GetSelectedCategory: IMarkCategory;
    procedure UpdateMarksList;
    function GetSelectedMarkId: IMarkId;
    function GetSelectedMarkFull: IMark;
    function GetSelectedMarksIdList: IInterfaceList;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(
      AUseAsIndepentWindow: Boolean;
      const ALanguageManager: ILanguageManager;
      const AImportFileByExt: IImportFile;
      const AViewPortState: ILocalCoordConverterChangeable;
      const ANavToPoint: INavigationToPoint;
      const AWindowConfig: IWindowPositionConfig;
      const AMarksShowConfig: IUsedMarksConfig;
      AMarkDBGUI: TMarksDbGUIHelper;
      const AMapGoto: IMapViewGoto;
      const AFormRegionProcess: TfrmRegionProcess
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  t_CommonTypes,
  t_GeoTypes,
  i_StaticTreeItem,
  i_ImportConfig,
  i_MarkTemplate,
  i_MarksFactoryConfig,
  i_VectorItemLonLat,
  u_ListenerByEvent;

{$R *.dfm}

constructor TfrmMarksExplorer.Create(
  AUseAsIndepentWindow: Boolean;
  const ALanguageManager: ILanguageManager;
  const AImportFileByExt: IImportFile;
  const AViewPortState: ILocalCoordConverterChangeable;
  const ANavToPoint: INavigationToPoint;
  const AWindowConfig: IWindowPositionConfig;
  const AMarksShowConfig: IUsedMarksConfig;
  AMarkDBGUI: TMarksDbGUIHelper;
  const AMapGoto: IMapViewGoto;
  const AFormRegionProcess: TfrmRegionProcess
);
begin
  inherited Create(ALanguageManager);
  FUseAsIndepentWindow := AUseAsIndepentWindow;
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
  FFormRegionProcess := AFormRegionProcess;

end;

procedure TfrmMarksExplorer.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FUseAsIndepentWindow then begin
    // WS_Ex_AppWindow - отвечает за отображение кнопки на панели задач
    Params.ExStyle := Params.ExStyle or WS_Ex_AppWindow;
    // по умолчанию устанавливается Application.Handle, однако мне нужно чтобы форма была отдельно
    Params.WndParent := GetDesktopWindow; // привязываем к рабочему столу
  end;
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

procedure TfrmMarksExplorer.BtnAddCategoryClick(Sender: TObject);
var
  VCategory: IMarkCategory;
begin
  VCategory := FMarkDBGUI.MarksDb.CategoryDB.Factory.CreateNew('');
  VCategory := FMarkDBGUI.EditCategoryModal(VCategory, True);
  if VCategory <> nil then begin
    FMarkDBGUI.MarksDb.CategoryDB.UpdateCategory(nil, VCategory);
  end;
end;

procedure TfrmMarksExplorer.UpdateCategoryTree;
  procedure UpdateTreeSubItems(
    const ATree: IStaticTreeItem;
    const ASelectedCategory: ICategory;
    AParentNode: TTreeNode;
    ATreeItems: TTreeNodes
  );
  var
    i: Integer;
    VTree: IStaticTreeItem;
    VNode: TTreeNode;
    VNodeToDelete: TTreeNode;
    VCategory: IMarkCategory;
    VName: string;
  begin
    if AParentNode = nil then begin
      VNode := ATreeItems.GetFirstNode;
    end else begin
      VNode := AParentNode.getFirstChild;
    end;
    for i := 0 to ATree.SubItemCount - 1 do begin
      VTree := ATree.SubItem[i];
      VName := VTree.Name;
      if VName = '' then begin
        VName := '(NoName)';
      end;
      if VNode = nil then begin
        VNode := ATreeItems.AddChildObject(AParentNode, VName, nil);
      end else begin
        VNode.Text := VName;
      end;
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
      end else begin
        VNode.StateIndex:=0;
        VNode.Data := nil;
      end;
      UpdateTreeSubItems(VTree, ASelectedCategory, VNode, ATreeItems);
      VNode := VNode.getNextSibling;
    end;
    while VNode <> nil do begin
      VNodeToDelete := VNode;
      VNode := VNode.getNextSibling;
      VNodeToDelete.Delete;
    end;
  end;
var
  VTree: IStaticTreeItem;
  VSelectedCategory: ICategory;
begin
  VSelectedCategory := GetSelectedCategory;
  FCategoryList := FMarkDBGUI.MarksDb.CategoryDB.GetCategoriesList;
  VTree := FMarkDBGUI.MarksDb.CategoryListToStaticTree(FCategoryList);
  CategoryTreeView.OnChange:=nil;
  try
    CategoryTreeView.Items.BeginUpdate;
    try
      UpdateTreeSubItems(VTree, VSelectedCategory, nil, CategoryTreeView.Items);
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
  VMarkId: IMarkId;
  I: Integer;
  VNode: TTreeNode;
  VNodeToDelete: TTreeNode;
  VName: string;
  VSortedMarksList: TStringList;
begin
  FMarksList := nil;
  VCategory := GetSelectedCategory;
  if (VCategory <> nil) then begin
    FMarksList := FMarkDBGUI.MarksDb.MarksDb.GetMarksIdListByCategory(VCategory);
    VSortedMarksList := TStringList.Create;
    try
      VSortedMarksList.Sorted := True;
      VSortedMarksList.Duplicates := dupAccept;
      VSortedMarksList.BeginUpdate;
      try
        for I := 0 to FMarksList.Count - 1 do begin
          VMarkId := IMarkId(FMarksList.Items[I]);
          VName := FMarkDBGUI.GetMarkIdCaption(VMarkId);
          VSortedMarksList.AddObject(VName, Pointer(VMarkId));
        end;
      finally
        VSortedMarksList.EndUpdate;
      end;
      MarksListBox.Items.BeginUpdate;
      try
        VNode := MarksListBox.Items.GetFirstNode;
        for I := 0 to VSortedMarksList.Count - 1 do begin
          VMarkId := IMarkId(Pointer(VSortedMarksList.Objects[I]));
          VName := VSortedMarksList.Strings[I];
          if VNode = nil then begin
            VNode := MarksListBox.Items.AddChildObject(nil, VName, nil);
          end else begin
            VNode.Text := VName;
          end;
          VNode.Data := Pointer(VMarkId);
          if FMarkDBGUI.MarksDb.MarksDb.GetMarkVisible(VMarkId) then begin
            VNode.StateIndex := 1;
          end else begin
            VNode.StateIndex := 2;
          end;
          VNode := VNode.getNextSibling;
        end;
        while VNode <> nil do begin
          VNodeToDelete := VNode;
          VNode := VNode.getNextSibling;
          VNodeToDelete.Delete;
        end;
        lblMarksCount.Caption:='('+inttostr(MarksListBox.Items.Count)+')';
      finally
        MarksListBox.Items.EndUpdate;
      end;
    finally
      VSortedMarksList.Free;
    end;
  end else begin
    MarksListBox.Items.Clear;
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
  for i:=0 to MarksListBox.SelectionCount-1 do begin
    Result.Add(IMarkId(MarksListBox.Selections[i].Data))
  end;
  if Result.Count=0 then begin
    Result:=nil;
  end;
end;

function TfrmMarksExplorer.GetSelectedMarkId: IMarkId;
var
  VNode: TTreeNode;
begin
  Result := nil;
  VNode := MarksListBox.Selected;
  if VNode <> nil then begin
    Result := IMarkId(VNode.Data);
  end;
end;

procedure TfrmMarksExplorer.btnImportClick(Sender: TObject);
var
  VImportConfig: IImportConfig;

  procedure _RunForFile(const AFileNameToImport: String);
  begin
    if (FileExists(AFileNameToImport)) then begin
      if not Assigned(VImportConfig) then
        VImportConfig := FMarkDBGUI.EditModalImportConfig;
      if Assigned(VImportConfig) then begin
        FImportFileByExt.ProcessImport(AFileNameToImport, VImportConfig);
      end;
    end;
  end;

var
  i: Integer;
begin
  VImportConfig := nil;

  if (OpenDialog1.Execute(Self.Handle)) then begin
    if Assigned(OpenDialog1.Files) and (OpenDialog1.Files.Count>0) then begin
      // multiple files
      for i := 0 to OpenDialog1.Files.Count-1 do
        _RunForFile(OpenDialog1.Files[i]);
    end else begin
      // single file
      _RunForFile(OpenDialog1.FileName);
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
    if FMarkDBGUI.DeleteMarksModal(VMarkIdList, Self.Handle) then begin
      MarksListBox.ClearSelection(True);
    end;
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
      VMark:=FMarkDBGUI.MarksDb.MarksDb.GetMarkByID(IMarkId(VMarkIdList[0]));
      VMark := FMarkDBGUI.EditMarkModal(VMark, False);
      if VMark <> nil then begin
        FMarkDBGUI.MarksDb.MarksDb.UpdateMark(VMarkIdList[0], VMark);
      end;
    end else begin
      VImportConfig := FMarkDBGUI.MarksMultiEditModal(GetSelectedCategory);
      if (VImportConfig <> nil) then begin
        VMarksList:=TInterfaceList.Create;
        for i := 0 to VMarkIdList.Count - 1 do begin
          VMarkId := IMarkId(VMarkIdList[i]);
          VMark:=FMarkDBGUI.MarksDb.MarksDb.GetMarkByID(VMarkId);
          if Supports(VMark, IMarkPoint, VMarkPoint) then begin
            if VImportConfig.TemplateNewPoint<>nil then begin
              VMark:=FMarkDBGUI.MarksDb.MarksDb.Factory.ModifyPoint(
                VMarkPoint,
                VMarkPoint.Name,
                FMarkDBGUI.MarksDb.MarksDb.GetMarkVisible(VMark),
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
              VMark:=FMarkDBGUI.MarksDb.MarksDb.Factory.ModifyLine(
                VMarkLine,
                VMarkLine.Name,
                FMarkDBGUI.MarksDb.MarksDb.GetMarkVisible(VMark),
                VImportConfig.TemplateNewLine.Category,
                VMarkLine.Desc,
                VMarkLine.Line,
                VImportConfig.TemplateNewLine.LineColor,
                VImportConfig.TemplateNewLine.LineWidth
              );
            end;
          end else if Supports(VMark, IMarkPoly, VMarkPoly) then begin
            if VImportConfig.TemplateNewPoly<>nil then begin
              VMark:=FMarkDBGUI.MarksDb.MarksDb.Factory.ModifyPoly(
                VMarkPoly,
                VMarkPoly.Name,
                FMarkDBGUI.MarksDb.MarksDb.GetMarkVisible(VMark),
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

procedure TfrmMarksExplorer.btnGoToMarkClick(Sender: TObject);
var
  VMark: IMark;
begin
  VMark := GetSelectedMarkFull;
  if VMark <> nil then begin
    FMapGoto.GotoPos(VMark.GetGoToLonLat, FViewPortState.GetStatic.Zoom);
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
  Vpolygon: ILonLatPolygon;
begin
  VMark := GetSelectedMarkFull;
  if VMark <> nil then begin
    Vpolygon := FMarkDBGUI.PolygonForOperation(VMark, FViewPortState.GetStatic.ProjectionInfo);
    if Vpolygon <> nil then begin
      FFormRegionProcess.Show_(FViewPortState.GetStatic.ProjectionInfo.Zoom, Vpolygon);
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

procedure TfrmMarksExplorer.CategoryTreeViewVisible(Node: TTreeNode);
var
  VCategoryOld: IMarkCategory;
  VCategoryNew: IMarkCategory;
  VTreeNode: TTreeNode;
  VVisible: Boolean;
  VIndex,VLevel,VNum: Integer;
begin
  VCategoryOld := IMarkCategory(Node.Data);
  if VCategoryOld <> nil then begin
    //  VVisible, VIndex - для визуализации узлов.
    if Node.StateIndex=1 then begin
      VVisible := False;
      VIndex:=2;
    end else begin
      VVisible := True;
      VIndex:=1;
    end;
    //  Изменение значения Visible текущего узла категории меток
    VCategoryNew := FMarkDBGUI.MarksDb.CategoryDB.Factory.ModifyVisible(VCategoryOld, VVisible);
    Node.StateIndex:=VIndex;
    if not VCategoryOld.IsEqual(VCategoryNew) then begin
      FMarkDBGUI.MarksDb.CategoryDB.UpdateCategory(VCategoryOld, VCategoryNew);
    end;
    //  Изменение значения Visible дочерних узла категории меток
    //  CheckBox3 включает/выклучает функцию каскадной визуализации.
    if CheckBox3.Checked then begin
      //  VLevel - уровень глубины узла. VNum - номер узла изменения видимости.
      CategoryTreeView.Visible := False;
      VLevel:=Node.Level;
      //  Цикл продолжается пока не перейдём на новую ветвь (пока уровень глубины узлов не станет как наша)
      //  Или до конца ветвей
      for VNum:=Node.AbsoluteIndex+1 to CategoryTreeView.Items.Count - 1 do
        if CategoryTreeView.Items[VNum].Level>VLevel then begin
          VTreeNode := CategoryTreeView.Items[VNum];
          VCategoryOld := IMarkCategory(VTreeNode.Data);
          if VCategoryOld <> nil then begin
            VCategoryNew := FMarkDBGUI.MarksDb.CategoryDB.Factory.ModifyVisible(VCategoryOld, VVisible);
            VTreeNode.StateIndex:=VIndex;
            if not VCategoryOld.IsEqual(VCategoryNew) then begin
              FMarkDBGUI.MarksDb.CategoryDB.UpdateCategory(VCategoryOld, VCategoryNew);
            end;
          end;
        end else break;  //  Прерываем цикл так, как уровень глубины узлов стал как наш или выше.
      CategoryTreeView.Visible := True;
    end;
  end;
end;

procedure TfrmMarksExplorer.CategoryTreeViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  VCategoryOld: IMarkCategory;
begin
  If key=VK_DELETE then begin
    VCategoryOld := GetSelectedCategory;
    if VCategoryOld <> nil then begin
      FMarkDBGUI.DeleteCategoryModal(VCategoryOld, Self.Handle);
    end;
  end;

  if Key=VK_SPACE then
    CategoryTreeViewVisible(CategoryTreeView.Selected);
end;

procedure TfrmMarksExplorer.CategoryTreeViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if htOnStateIcon in CategoryTreeView.GetHitTestInfoAt(X,Y) then
    CategoryTreeViewVisible(CategoryTreeView.GetNodeAt(X,Y));
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

procedure TfrmMarksExplorer.CategoryTreeViewContextPopup(Sender: TObject;
    MousePos: TPoint; var Handled: Boolean);
var
  VNode: TTreeNode;
begin
  if (MousePos.X >= 0) and (MousePos.Y >= 0) then begin
    VNode := CategoryTreeView.GetNodeAt(MousePos.X, MousePos.Y);
    if VNode <> nil then begin
      CategoryTreeView.Select(VNode);
    end;
  end;
end;

procedure TfrmMarksExplorer.OnCategoryDbChanged;
begin
  UpdateCategoryTree;
end;

procedure TfrmMarksExplorer.OnConfigChange;
function UpdateRectByMonitors(ARect: TRect): TRect;
var
  i: Integer;
  VIntersectRect: TRect;
  VMonitor: TMonitor;
begin
  Result := ARect;
  for i := 0 to Screen.MonitorCount - 1 do begin
    VMonitor := Screen.Monitors[i];
    if IntersectRect(VIntersectRect, ARect, VMonitor.WorkareaRect) then begin
      Exit;
    end;
  end;
  VMonitor := Screen.MonitorFromRect(ARect, mdNearest);
  Result.TopLeft := VMonitor.WorkareaRect.TopLeft;
  Result.Right := Result.Left + (ARect.Right - ARect.Left);
  Result.Bottom := Result.Top + (ARect.Bottom - ARect.Top);
end;
var
  VRect: TRect;
begin
  VRect := FWindowConfig.BoundsRect;
  if EqualRect(BoundsRect, VRect) then begin
    Exit;
  end;
  VRect := UpdateRectByMonitors(VRect);
  if EqualRect(BoundsRect, VRect) then begin
    Exit;
  end;
  BoundsRect := VRect;
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
  lblReadOnly.Visible := FMarkDBGUI.MarksDb.State.GetStatic.WriteAccess = asDisabled;
end;

procedure TfrmMarksExplorer.tbitmAddCategoryClick(Sender: TObject);
var
  VCategory: IMarkCategory;
  VNewName: string;
begin
  VNewName := '';
  VCategory := GetSelectedCategory;
  if VCategory <> nil then begin
    VNewName := VCategory.Name + '\';
  end;

  VCategory := FMarkDBGUI.MarksDb.CategoryDB.Factory.CreateNew(VNewName);
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
  VLonLat := FViewPortState.GetStatic.GetCenterLonLat;
  VCategory := GetSelectedCategory;
  VPointTemplate := nil;
  if VCategory <> nil then begin
    VTemplateConfig := FMarkDBGUI.MarksDb.MarksFactoryConfig.PointTemplateConfig;
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

  VMark := FMarkDBGUI.MarksDb.MarksDb.Factory.CreateNewPoint(VLonLat, '', '', VPointTemplate);
  VMark := FMarkDBGUI.EditMarkModal(VMark, True);
  if VMark <> nil then begin
    FMarkDBGUI.MarksDb.MarksDb.UpdateMark(nil, VMark);
  end;
end;

procedure TfrmMarksExplorer.tbitmMarkInfoClick(Sender: TObject);
var
  VMark: IMark;
begin
  VMark := GetSelectedMarkFull;
  if VMark <> nil then begin
    FMarkDBGUI.ShowMarkInfo(VMark);
  end;
end;

procedure TfrmMarksExplorer.CheckBox2Click(Sender: TObject);
var
  VNewVisible: Boolean;
begin
  if CategoryTreeView.Items.Count>0 then begin
    VNewVisible := CheckBox2.Checked;
    FMarkDBGUI.MarksDb.CategoryDB.SetAllCategoriesVisible(VNewVisible);
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
    FMarkDBGUI.MarksDb.MarksDb.SetAllMarksInCategoryVisible(VCategory, VNewVisible);
  end;
end;

procedure TfrmMarksExplorer.FormHide(Sender: TObject);
begin
  Self.OnResize := nil;
  FWindowConfig.ChangeNotifier.Remove(FConfigListener);
  FMarkDBGUI.MarksDb.CategoryDB.ChangeNotifier.Remove(FCategoryDBListener);
  FMarkDBGUI.MarksDb.MarksDb.ChangeNotifier.Remove(FMarksDBListener);
  FMarksShowConfig.ChangeNotifier.Remove(FMarksShowConfigListener);
  FMarkDBGUI.MarksDb.State.ChangeNotifier.Remove(FMarksSystemStateListener);
  CategoryTreeView.OnChange:=nil;
  CategoryTreeView.Items.Clear;
  MarksListBox.Items.Clear;
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
  FMarkDBGUI.MarksDb.CategoryDB.ChangeNotifier.Add(FCategoryDBListener);
  FMarkDBGUI.MarksDb.MarksDb.ChangeNotifier.Add(FMarksDBListener);
  FMarksShowConfig.ChangeNotifier.Add(FMarksShowConfigListener);
  FMarkDBGUI.MarksDb.State.ChangeNotifier.Add(FMarksSystemStateListener);
  FWindowConfig.ChangeNotifier.Add(FConfigListener);
  OnConfigChange;
  OnMarkSystemStateChanged;
  Self.OnResize := FormResize;
end;

procedure TfrmMarksExplorer.MarksListBoxContextPopup(Sender: TObject; MousePos:
    TPoint; var Handled: Boolean);
var
  VNode: TTreeNode;
begin
  if (MousePos.X >= 0) and (MousePos.Y >= 0) then begin
    VNode := MarksListBox.GetNodeAt(MousePos.X, MousePos.Y);
    if VNode <> nil then begin
      MarksListBox.Select(VNode);
    end;
  end;
end;

procedure TfrmMarksExplorer.MarksListBoxDblClick(Sender: TObject);
var
  VMark: IMark;
  VMarkPoint: IMarkPoint;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
begin
  VMark := GetSelectedMarkFull;
  if VMark <> nil then begin
    if Supports(VMark, IMarkPoint, VMarkPoint) then begin
      FMapGoto.GotoPos(VMark.GetGoToLonLat, FViewPortState.GetStatic.Zoom);
    end else if Supports(VMark, IMarkLine, VMarkLine) then begin
      FMapGoto.FitRectToScreen(VMarkLine.LLRect.Rect);
    end else if Supports(VMark, IMarkPoly, VMarkPoly) then begin
      FMapGoto.FitRectToScreen(VMarkPoly.LLRect.Rect);
    end;
  end;
end;

procedure TfrmMarksExplorer.MarksListBoxKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
var
  VMarkIdList: IInterfaceList;
begin
  if Key = VK_SPACE then begin
    VMarkIdList:=GetSelectedMarksIdList;
    if (VMarkIdList <> nil) and (VMarkIdList.Count > 0) then begin
      FMarkDBGUI.MarksDb.MarksDb.ToggleMarkVisibleByIDList(VMarkIdList);
    end;
    Key := 0;
  end else if Key = VK_RETURN then begin
    MarksListBoxDblClick(Sender);
    Key := 0;
  end else if key = VK_DELETE then begin
    VMarkIdList:=GetSelectedMarksIdList;
    if VMarkIdList <> nil then begin
      if FMarkDBGUI.DeleteMarksModal(VMarkIdList, Self.Handle) then begin
        MarksListBox.ClearSelection(True);
      end;
    end;
  end;
end;

procedure TfrmMarksExplorer.MarksListBoxMouseUp(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  VTreeNode: TTreeNode;
  VMarkId: IMarkId;
begin
  if htOnStateIcon in MarksListBox.GetHitTestInfoAt(X,Y) then begin
    VTreeNode := MarksListBox.GetNodeAt(X,Y);
    VMarkId := IMarkId(VTreeNode.Data);
    if VMarkId <> nil then begin
      if VTreeNode.StateIndex=1 then begin
        FMarkDBGUI.MarksDb.MarksDb.SetMarkVisibleByID(VMarkId, False);
        VTreeNode.StateIndex:=2;
      end else begin
        FMarkDBGUI.MarksDb.MarksDb.SetMarkVisibleByID(VMarkId, True);
        VTreeNode.StateIndex:=1;
      end;
    end;
  end;
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

procedure TfrmMarksExplorer.tbpmnCategoriesPopup(Sender: TObject);
begin
  if GetSelectedCategory = nil then begin
    tbitmAddCategory.Caption := _('Add Category');
  end else begin
    tbitmAddCategory.Caption := _('Add SubCategory');
  end;
end;

end.

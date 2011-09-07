unit frm_MarksExplorer;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  ComCtrls,
  ImgList,
  Menus,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst,
  Buttons,
  ExtCtrls,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  TBX,
  TBXControls,
  u_ResStrings,
  u_CommonFormAndFrameParents,
  t_GeoTypes,
  i_MapViewGoto,
  i_MarksSimple,
  i_MarkCategory,
  u_MarksDbGUIHelper,
  frm_Main;

type
  TfrmMarksExplorer = class(TCommonFormParent)
    grpMarks: TGroupBox;
    MarksListBox: TCheckListBox;
    grpCategory: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    OpenDialog1: TOpenDialog;
    TreeView1: TTreeView;
    imlStates: TImageList;
    pnlButtons: TPanel;
    pnlMainWithButtons: TPanel;
    pnlMain: TPanel;
    splCatMarks: TSplitter;
    btnExport: TTBXButton;
    ExportDialog: TSaveDialog;
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
    btnCancel: TButton;
    btnOk: TButton;
    btnApply: TButton;
    procedure MarksListBoxClickCheck(Sender: TObject);
    procedure BtnDelKatClick(Sender: TObject);
    procedure BtnEditCategoryClick(Sender: TObject);
    procedure MarksListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure btnExportClick(Sender: TObject);
    procedure btnExportCategoryClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnEditMarkClick(Sender: TObject);
    procedure btnDelMarkClick(Sender: TObject);
    procedure btnGoToMarkClick(Sender: TObject);
    procedure btnOpSelectMarkClick(Sender: TObject);
    procedure btnNavOnMarkClick(Sender: TObject);
    procedure btnSaveMarkClick(Sender: TObject);
    procedure TBXItem4Click(Sender: TObject);
  private
    FMapGoto: IMapViewGoto;
    FCategoryList: IInterfaceList;
    FMarksList: IInterfaceList;
    FMarkDBGUI: TMarksDbGUIHelper;
    procedure UpdateCategoryTree;
    function GetSelectedCategory: IMarkCategory;
    procedure UpdateMarksList;
    function GetSelectedMarkId: IMarkId;
    function GetSelectedMarkFull: IMark;
  public
    procedure EditMarks(AMarkDBGUI: TMarksDbGUIHelper; AMapGoto: IMapViewGoto);
  end;

var
  frmMarksExplorer: TfrmMarksExplorer;

implementation

uses
  u_GlobalState,
  i_ImportConfig,
  i_UsedMarksConfig,
  u_ExportMarks2KML,
  frm_ImportConfigEdit,
  frm_MarkCategoryEdit;

{$R *.dfm}

procedure TfrmMarksExplorer.UpdateCategoryTree;
begin
  TreeView1.OnChange:=nil;
  try
    TreeView1.Items.BeginUpdate;
    try
      TreeView1.SortType := stNone;
      FCategoryList := FMarkDBGUI.MarksDB.CategoryDB.GetCategoriesList;
      FMarkDBGUI.CategoryListToTree(FCategoryList, TreeView1.Items);
      TreeView1.SortType:=stText;
    finally
      TreeView1.Items.EndUpdate;
    end;
  finally
    TreeView1.OnChange := Self.TreeView1Change;
  end;
end;

procedure TfrmMarksExplorer.UpdateMarksList;
var
  VCategory: IMarkCategory;
  i: Integer;
begin
  MarksListBox.Clear;
  FMarksList := nil;
  VCategory := GetSelectedCategory;
  if (VCategory <> nil) then begin
    FMarksList := FMarkDBGUI.MarksDb.MarksDb.GetMarskIdListByCategory(VCategory);
    MarksListBox.Items.BeginUpdate;
    try
      FMarkDBGUI.MarksListToStrings(FMarksList, MarksListBox.Items);
      for i:=0 to MarksListBox.Count-1 do begin
        MarksListBox.Checked[i] := FMarkDBGUI.MarksDB.MarksDb.GetMarkVisible(IMarkId(Pointer(MarksListBox.Items.Objects[i])));
      end;
    finally
      MarksListBox.Items.EndUpdate;
    end;
  end;
end;

function TfrmMarksExplorer.GetSelectedCategory: IMarkCategory;
begin
  Result := nil;
  if TreeView1.Selected <> nil then begin
    Result := IMarkCategory(TreeView1.Selected.Data);
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
      VImportConfig := frmImportConfigEdit.GetImportConfig(FMarkDBGUI);
      if VImportConfig <> nil then begin
        GState.ImportFileByExt.ProcessImport(VFileName, VImportConfig);
      end;
      UpdateCategoryTree;
      UpdateMarksList;
    end;
  end;
end;

procedure TfrmMarksExplorer.BtnDelKatClick(Sender: TObject);
var
  VCategory: IMarkCategory;
begin
  VCategory := GetSelectedCategory;
  if VCategory <> nil then begin
    if MessageBox(Self.handle,pchar(SAS_MSG_youasure+' "'+VCategory.name+'"'),pchar(SAS_MSG_coution),36)=IDYES then begin
      FMarkDBGUI.MarksDb.DeleteCategoryWithMarks(VCategory);
      UpdateCategoryTree;
      UpdateMarksList;
    end;
  end;
end;

procedure TfrmMarksExplorer.btnExportClick(Sender: TObject);
var KMLExport:TExportMarks2KML;
begin
  KMLExport:=TExportMarks2KML.Create(TComponent(Sender).tag=1);
  try
    if (ExportDialog.Execute)and(ExportDialog.FileName<>'') then begin
      KMLExport.ExportToKML(ExportDialog.FileName);
    end;
  finally
    KMLExport.free;
  end;
end;

procedure TfrmMarksExplorer.btnApplyClick(Sender: TObject);
begin
  GState.MainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.LockWrite;
  try
    case rgMarksShowMode.ItemIndex of
      0: begin
        GState.MainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IsUseMarks := True;
        GState.MainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IgnoreCategoriesVisible := False;
        GState.MainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IgnoreMarksVisible := False;

      end;
      1: begin
        GState.MainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IsUseMarks := True;
        GState.MainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IgnoreCategoriesVisible := True;
        GState.MainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IgnoreMarksVisible := True;
      end;
    else
      GState.MainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.IsUseMarks := False;
    end;
  finally
    GState.MainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.UnlockWrite;
  end;
  frmMain.LayerMapMarksRedraw;
end;

procedure TfrmMarksExplorer.btnDelMarkClick(Sender: TObject);
var
  VMarkId: IMarkId;
begin
  VMarkId := GetSelectedMarkId;
  if VMarkId <> nil then begin
    if FMarkDBGUI.DeleteMarkModal(VMarkId, Self.Handle) then begin
      MarksListBox.DeleteSelected;
    end;
  end;
end;

procedure TfrmMarksExplorer.btnEditMarkClick(Sender: TObject);
var
  VMark: IMark;
begin
  VMark := GetSelectedMarkFull;
  if VMark <> nil then begin
    VMark := FMarkDBGUI.EditMarkModal(VMark);
    if VMark <> nil then begin
      FMarkDBGUI.MarksDb.MarksDb.WriteMark(VMark);
      UpdateMarksList;
    end;
  end;
end;

procedure TfrmMarksExplorer.btnGoToMarkClick(Sender: TObject);
var
  VMark: IMark;
begin
  VMark := GetSelectedMarkFull;
  if VMark <> nil then begin
    FMapGoto.GotoPos(VMark.GetGoToLonLat, GState.MainFormConfig.ViewPortState.GetCurrentZoom);
  end;
end;

procedure TfrmMarksExplorer.TBXItem4Click(Sender: TObject);
var
  VCategory: IMarkCategory;
begin
  VCategory := FMarkDBGUI.MarksDB.CategoryDB.Factory.CreateNew('');
  VCategory := frmMarkCategoryEdit.EditCategory(VCategory, FMarkDBGUI);
  if VCategory <> nil then begin
    FMarkDBGUI.MarksDb.CategoryDB.WriteCategory(VCategory);
    UpdateCategoryTree;
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
      GState.MainFormConfig.NavToPoint.StartNavToMark(VMark as IMarkId, LL);
    end else begin
      btnNavOnMark.Checked:=not btnNavOnMark.Checked;
    end;
  end else begin
    GState.MainFormConfig.NavToPoint.StopNav;
  end;
end;

procedure TfrmMarksExplorer.btnOpSelectMarkClick(Sender: TObject);
var
  VMark: IMark;
begin
  VMark := GetSelectedMarkFull;
  if VMark <> nil then begin
    if FMarkDBGUI.OperationMark(VMark, GState.MainFormConfig.ViewPortState.GetCurrentZoom, GState.MainFormConfig.ViewPortState.GetCurrentCoordConverter) then begin
      ModalResult := mrOk;
    end;
  end;
end;

procedure TfrmMarksExplorer.btnSaveMarkClick(Sender: TObject);
var
  KMLExport:TExportMarks2KML;
  VMark: IMark;
begin
    VMark := GetSelectedMarkFull;
    if VMark <> nil then begin
      KMLExport:=TExportMarks2KML.Create(false);
      try
        ExportDialog.FileName:=VMark.name;
        if (ExportDialog.Execute)and(ExportDialog.FileName<>'') then begin
          KMLExport.ExportMarkToKML(VMark,ExportDialog.FileName);
        end;
      finally
        KMLExport.free;
      end;
    end;
end;

procedure TfrmMarksExplorer.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  UpdateMarksList;
end;

procedure TfrmMarksExplorer.TreeView1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  VCategory: IMarkCategory;
begin
  If key=VK_DELETE then begin
    VCategory := GetSelectedCategory;
    if VCategory <> nil then begin
      if MessageBox(Self.handle,pchar(SAS_MSG_youasure+' "'+VCategory.name+'"'),pchar(SAS_MSG_coution),36)=IDYES then begin
        FMarkDBGUI.MarksDb.DeleteCategoryWithMarks(VCategory);
        UpdateCategoryTree;
        UpdateMarksList;
      end;
    end;
  end;

  if Key=VK_SPACE then begin
    VCategory := GetSelectedCategory;
    if VCategory <> nil then begin
      FCategoryList.Remove(VCategory);
      if TreeView1.Selected.StateIndex = 1 then begin
        VCategory := FMarkDBGUI.MarksDB.CategoryDB.Factory.ModifyVisible(VCategory, False);
        TreeView1.Selected.StateIndex:=2;
      end else begin
        VCategory := FMarkDBGUI.MarksDB.CategoryDB.Factory.ModifyVisible(VCategory, True);
        TreeView1.Selected.StateIndex:=1;
      end;
      FMarkDBGUI.MarksDb.CategoryDB.WriteCategory(VCategory);
      FCategoryList.Add(VCategory);
      TreeView1.Selected.Data := Pointer(VCategory);
    end;
  end;
end;

procedure TfrmMarksExplorer.TreeView1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  VCategory: IMarkCategory;
  VTreeNode: TTreeNode;
begin
  if htOnStateIcon in TreeView1.GetHitTestInfoAt(X,Y) then begin
    VTreeNode := TreeView1.GetNodeAt(X,Y);
    VCategory := IMarkCategory(VTreeNode.Data);
    if VCategory <> nil then begin
      FCategoryList.Remove(VCategory);
      if VTreeNode.StateIndex=1 then begin
        VCategory := FMarkDBGUI.MarksDB.CategoryDB.Factory.ModifyVisible(VCategory, False);
        VTreeNode.StateIndex:=2;
      end else begin
        VCategory := FMarkDBGUI.MarksDB.CategoryDB.Factory.ModifyVisible(VCategory, True);
        VTreeNode.StateIndex:=1;
      end;
      FMarkDBGUI.MarksDb.CategoryDB.WriteCategory(VCategory);
      FCategoryList.Add(VCategory);
      VTreeNode.Data := Pointer(VCategory);
    end;
  end;
end;

procedure TfrmMarksExplorer.BtnEditCategoryClick(Sender: TObject);
var
  VCategory: IMarkCategory;
begin
  VCategory := GetSelectedCategory;
  if VCategory <> nil then begin
    VCategory := frmMarkCategoryEdit.EditCategory(VCategory, FMarkDBGUI);
    if VCategory <> nil then begin
      FMarkDBGUI.MarksDb.CategoryDB.WriteCategory(VCategory);
      UpdateCategoryTree;
    end;
  end;
end;

procedure TfrmMarksExplorer.btnExportCategoryClick(Sender: TObject);
var
  KMLExport: TExportMarks2KML;
  VCategory: IMarkCategory;
begin
  VCategory := GetSelectedCategory;
  if VCategory<>nil then begin
    KMLExport:=TExportMarks2KML.Create(TComponent(Sender).tag=1);
    try
      ExportDialog.FileName:=StringReplace(VCategory.name,'\','-',[rfReplaceAll]);
      if (ExportDialog.Execute)and(ExportDialog.FileName<>'') then begin
        KMLExport.ExportCategoryToKML(VCategory,ExportDialog.FileName);
      end;
    finally
      KMLExport.free;
    end;
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
      if FMarkDBGUI.DeleteMarkModal(VMarkId, Self.Handle) then begin
        UpdateMarksList;
      end;
    end;
  end;
end;

procedure TfrmMarksExplorer.CheckBox2Click(Sender: TObject);
var
  VNewVisible: Boolean;
begin
  if TreeView1.Items.Count>0 then begin
    VNewVisible := CheckBox2.Checked;
    FMarkDBGUI.MarksDB.CategoryDB.SetAllCategoriesVisible(VNewVisible);
    UpdateCategoryTree;
  end;
end;

procedure TfrmMarksExplorer.EditMarks(
  AMarkDBGUI: TMarksDbGUIHelper; AMapGoto: IMapViewGoto
);
var
  VModalResult: Integer;
begin
  FMarkDBGUI := AMarkDBGUI;
  FMapGoto := AMapGoto;
  UpdateCategoryTree;
  UpdateMarksList;
  btnNavOnMark.Checked:= GState.MainFormConfig.NavToPoint.IsActive;
  try
    VModalResult := ShowModal;
    if VModalResult = mrOk then begin
      btnApplyClick(nil);
    end;
  finally
    TreeView1.OnChange:=nil;
    TreeView1.Items.Clear;
    MarksListBox.Clear;
    FCategoryList := nil;
    FMarksList := nil;
  end;
end;

procedure TfrmMarksExplorer.FormActivate(Sender: TObject);
var
  VMarksConfig: IUsedMarksConfigStatic;
begin
  VMarksConfig := GState.MainFormConfig.LayersConfig.MarksLayerConfig.MarksShowConfig.GetStatic;
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

procedure TfrmMarksExplorer.CheckBox1Click(Sender: TObject);
var
  VNewVisible: Boolean;
  VCategory: IMarkCategory;
begin
  VCategory := GetSelectedCategory;
  if VCategory <> nil then begin
    VNewVisible := CheckBox1.Checked;
    FMarkDBGUI.MarksDB.MarksDb.SetAllMarksInCategoryVisible(VCategory, VNewVisible);
    UpdateMarksList;
  end;
end;

end.

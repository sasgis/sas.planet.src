unit UMarksExplorer;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst,
  Buttons,
  ExtCtrls,
  UResStrings,
  u_CommonFormAndFrameParents,
  t_GeoTypes,
  i_MarksSimple,
  u_MarksSimple,
  u_MarksDbGUIHelper,
  Unit1, ComCtrls, ImgList, TBXControls, Menus;

type
  TFMarksExplorer = class(TCommonFormParent)
    grpMarks: TGroupBox;
    BtnGotoMark: TSpeedButton;
    BtnOpMark: TSpeedButton;
    MarksListBox: TCheckListBox;
    grpCategory: TGroupBox;
    BtnDelKat: TSpeedButton;
    OpenDialog: TOpenDialog;
    BtnDelMark: TSpeedButton;
    RBall: TRadioButton;
    RBchecked: TRadioButton;
    RBnot: TRadioButton;
    SpeedButton1: TSpeedButton;
    Bevel2: TBevel;
    BtnEditCategory: TSpeedButton;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    BtnAddCategory: TSpeedButton;
    SBNavOnMark: TSpeedButton;
    OpenDialog1: TOpenDialog;
    TreeView1: TTreeView;
    imlStates: TImageList;
    pnlBottom: TPanel;
    pnlButtons: TPanel;
    pnlMainWithButtons: TPanel;
    pnlMain: TPanel;
    splCatMarks: TSplitter;
    pnlMarksTop: TPanel;
    pnlCategoriesTop: TPanel;
    btnExport: TTBXButton;
    ExportDialog: TSaveDialog;
    PopupExport: TPopupMenu;
    NExportAll: TMenuItem;
    NExportVisible: TMenuItem;
    Bevel1: TBevel;
    btnExportMark: TSpeedButton;
    Bevel3: TBevel;
    btnExportCategory: TSpeedButton;
    btnImport: TTBXButton;
    btnAccept: TTBXButton;
    btn_Close: TTBXButton;
    procedure BtnDelMarkClick(Sender: TObject);
    procedure MarksListBoxClickCheck(Sender: TObject);
    procedure BtnOpMarkClick(Sender: TObject);
    procedure BtnGotoMarkClick(Sender: TObject);
    procedure BtnDelKatClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BtnEditCategoryClick(Sender: TObject);
    procedure MarksListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure BtnAddCategoryClick(Sender: TObject);
    procedure SBNavOnMarkClick(Sender: TObject);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure btnExportClick(Sender: TObject);
    procedure btnExportMarkClick(Sender: TObject);
    procedure btnExportCategoryClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure btn_CloseClick(Sender: TObject);
  private
    FCategoryList: TList;
    FMarksList: IInterfaceList;
    FMarkDBGUI: TMarksDbGUIHelper;
    procedure UpdateCategoryTree;
    function GetSelectedCategory: TCategoryId;
    procedure UpdateMarksList;
    function GetSelectedMarkId: IMarkId;
    function GetSelectedMarkFull: IMarkFull;
  public
    procedure EditMarks(AMarkDBGUI: TMarksDbGUIHelper);
  end;

var
  FMarksExplorer: TFMarksExplorer;

implementation

uses
  u_GlobalState,
  i_IImportConfig,
  i_IUsedMarksConfig,
  UImport,
  u_ExportMarks2KML,
  UAddCategory;

{$R *.dfm}

procedure TFMarksExplorer.UpdateCategoryTree;
begin
  TreeView1.OnChange:=nil;
  try
    TreeView1.Items.BeginUpdate;
    try
      TreeView1.SortType := stNone;
      FreeAndNil(FCategoryList);
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

procedure TFMarksExplorer.UpdateMarksList;
var
  VCategory: TCategoryId;
  i: Integer;
begin
  MarksListBox.Clear;
  FMarksList := nil;
  VCategory := GetSelectedCategory;
  if (VCategory <> nil) then begin
    FMarksList := FMarkDBGUI.MarksDb.MarksDb.GetMarskIdListByCategory(VCategory.id);
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

function TFMarksExplorer.GetSelectedCategory: TCategoryId;
begin
  Result := nil;
  if TreeView1.Selected <> nil then begin
    Result := TCategoryId(TreeView1.Selected.Data);
  end;
end;

function TFMarksExplorer.GetSelectedMarkFull: IMarkFull;
var
  VMarkId: IMarkId;
begin
  Result := nil;
  VMarkId := GetSelectedMarkId;
  if VMarkId <> nil then begin
    Result := FMarkDBGUI.MarksDb.MarksDb.GetMarkByID(VMarkId.id);
  end;
end;

function TFMarksExplorer.GetSelectedMarkId: IMarkId;
var
  VIndex: Integer;
begin
  Result := nil;
  VIndex := MarksListBox.ItemIndex;
  if VIndex>=0 then begin
    Result := IMarkId(Pointer(MarksListBox.Items.Objects[VIndex]));
  end;
end;

procedure TFMarksExplorer.BtnDelMarkClick(Sender: TObject);
var
  VMarkId: IMarkId;
begin
  VMarkId := GetSelectedMarkId;
  if VMarkId <> nil then begin
    if FMarkDBGUI.DeleteMarkModal(VMarkId.id, Self.Handle) then begin
      UpdateMarksList;
    end;
  end;
end;

procedure TFMarksExplorer.MarksListBoxClickCheck(Sender: TObject);
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

procedure TFMarksExplorer.BtnOpMarkClick(Sender: TObject);
var
  VMark: IMarkID;
begin
  VMark := GetSelectedMarkId;
  if VMark <> nil then begin
    if FMarkDBGUI.OperationMark(VMark.Id, GState.MainFormConfig.ViewPortState.GetCurrentZoom) then begin
      close;
    end;
  end;
end;

procedure TFMarksExplorer.BtnGotoMarkClick(Sender: TObject);
var
  VMark: IMarkFull;
begin
  VMark := GetSelectedMarkFull;
  if VMark <> nil then begin
    Fmain.topos(VMark.GetGoToLonLat, GState.MainFormConfig.ViewPortState.GetCurrentZoom, True);
  end;
end;

procedure TFMarksExplorer.btnImportClick(Sender: TObject);
var
  VImportConfig: IImportConfig;
  VFileName: string;
begin
  If (OpenDialog1.Execute) then begin
    VFileName := OpenDialog1.FileName;
    if (FileExists(VFileName)) then begin
      VImportConfig := FImport.GetImportConfig(FMarkDBGUI);
      if VImportConfig <> nil then begin
        //Todo Доделать
      end;
      UpdateCategoryTree;
      UpdateMarksList;
    end;
  end;
end;

procedure TFMarksExplorer.BtnDelKatClick(Sender: TObject);
var
  VCategory: TCategoryId;
begin
  VCategory := GetSelectedCategory;
  if VCategory <> nil then begin
    if MessageBox(Self.handle,pchar(SAS_MSG_youasure+' "'+VCategory.name+'"'),pchar(SAS_MSG_coution),36)=IDYES then begin
      FMarkDBGUI.MarksDb.CategoryDB.DeleteCategoryWithMarks(VCategory);
      UpdateCategoryTree;
      UpdateMarksList;
    end;
  end;
end;

procedure TFMarksExplorer.SpeedButton1Click(Sender: TObject);
var
  VMark: IMarkFull;
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

procedure TFMarksExplorer.btnExportClick(Sender: TObject);
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

procedure TFMarksExplorer.btnAcceptClick(Sender: TObject);
begin
  fmain.generate_im;
end;

procedure TFMarksExplorer.btn_CloseClick(Sender: TObject);
begin
  GState.MainFormConfig.LayersConfig.MarksShowConfig.LockWrite;
  try
    if RBnot.Checked then begin
      GState.MainFormConfig.LayersConfig.MarksShowConfig.IsUseMarks := False;
    end else begin
      GState.MainFormConfig.LayersConfig.MarksShowConfig.IsUseMarks := True;
      if RBall.Checked then begin
        GState.MainFormConfig.LayersConfig.MarksShowConfig.IgnoreCategoriesVisible := True;
        GState.MainFormConfig.LayersConfig.MarksShowConfig.IgnoreMarksVisible := True;
      end else begin
        GState.MainFormConfig.LayersConfig.MarksShowConfig.IgnoreCategoriesVisible := False;
        GState.MainFormConfig.LayersConfig.MarksShowConfig.IgnoreMarksVisible := False;
      end;
    end;
  finally
    GState.MainFormConfig.LayersConfig.MarksShowConfig.UnlockWrite;
  end;
  close;
end;

procedure TFMarksExplorer.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  UpdateMarksList;
end;

procedure TFMarksExplorer.TreeView1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  VCategory: TCategoryId;
begin
  If key=VK_DELETE then begin
    VCategory := GetSelectedCategory;
    if VCategory <> nil then begin
      if MessageBox(Self.handle,pchar(SAS_MSG_youasure+' "'+VCategory.name+'"'),pchar(SAS_MSG_coution),36)=IDYES then begin
        FMarkDBGUI.MarksDb.CategoryDB.DeleteCategoryWithMarks(VCategory);
        UpdateCategoryTree;
        UpdateMarksList;
      end;
    end;
  end;

  if Key=VK_SPACE then begin
    if (TreeView1.Selected<>nil)and(TreeView1.Selected.Data<>nil) then begin
      VCategory := TCategoryId(TreeView1.Selected.Data);
      if TreeView1.Selected.StateIndex=1 then begin
        VCategory.visible := false;
        TreeView1.Selected.StateIndex:=2;
      end else begin
        VCategory.visible := true;
        TreeView1.Selected.StateIndex:=1;
      end;
      FMarkDBGUI.MarksDb.CategoryDB.WriteCategory(VCategory);
    end;
  end;
end;

procedure TFMarksExplorer.TreeView1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  VCategory: TCategoryId;
begin
  if htOnStateIcon in TreeView1.GetHitTestInfoAt(X,Y) then begin
    VCategory := TCategoryId(TreeView1.GetNodeAt(X,Y).Data);
    if VCategory <> nil then begin
      if TreeView1.GetNodeAt(X,Y).StateIndex=1 then begin
        VCategory.visible := false;
        TreeView1.GetNodeAt(X,Y).StateIndex:=2;
      end else begin
        VCategory.visible := true;
        TreeView1.GetNodeAt(X,Y).StateIndex:=1;
      end;
      FMarkDBGUI.MarksDb.CategoryDB.WriteCategory(VCategory);
    end;
  end;
end;

procedure TFMarksExplorer.BtnEditCategoryClick(Sender: TObject);
var
  VCategory: TCategoryId;
begin
  VCategory := GetSelectedCategory;
  if VCategory <> nil then begin
    if FaddCategory.EditCategory(VCategory) then begin
      FMarkDBGUI.MarksDb.CategoryDB.WriteCategory(VCategory);
      UpdateCategoryTree;
    end;
  end;
end;

procedure TFMarksExplorer.btnExportCategoryClick(Sender: TObject);
var KMLExport:TExportMarks2KML;
    VCategory:TCategoryId;
begin
  VCategory := GetSelectedCategory;
  if VCategory<>nil then begin
    KMLExport:=TExportMarks2KML.Create(TComponent(Sender).tag=1);
    try
      FMarksExplorer.ExportDialog.FileName:=StringReplace(VCategory.name,'\','-',[rfReplaceAll]);
      if (ExportDialog.Execute)and(ExportDialog.FileName<>'') then begin
        KMLExport.ExportCategoryToKML(VCategory,ExportDialog.FileName);
      end;
    finally
      KMLExport.free;
    end;
  end;
end;

procedure TFMarksExplorer.btnExportMarkClick(Sender: TObject);
var KMLExport:TExportMarks2KML;
    VMark: iMarkFull;
begin
    VMark := GetSelectedMarkFull;
    if VMark <> nil then begin
      KMLExport:=TExportMarks2KML.Create(false);
      try
        FMarksExplorer.ExportDialog.FileName:=VMark.name;
        if (ExportDialog.Execute)and(ExportDialog.FileName<>'') then begin
          KMLExport.ExportMarkToKML(VMark,ExportDialog.FileName);
        end;
      finally
        KMLExport.free;
      end;
    end;
end;

procedure TFMarksExplorer.MarksListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  VMarkId: IMarkId;
begin
  If key=VK_DELETE then begin
    VMarkId := GetSelectedMarkId;
    if VMarkId <> nil then begin
      if FMarkDBGUI.DeleteMarkModal(VMarkId.id, Self.Handle) then begin
        UpdateMarksList;
      end;
    end;
  end;
end;

procedure TFMarksExplorer.CheckBox2Click(Sender: TObject);
var
  VNewVisible: Boolean;
begin
  if TreeView1.Items.Count>0 then begin
    VNewVisible := CheckBox2.Checked;
    FMarkDBGUI.MarksDB.CategoryDB.SetAllCategoriesVisible(VNewVisible);
    UpdateCategoryTree;
  end;
end;

procedure TFMarksExplorer.EditMarks(AMarkDBGUI: TMarksDbGUIHelper);
var
  VMarksConfig: IUsedMarksConfigStatic;
begin
  FMarkDBGUI := AMarkDBGUI;
  VMarksConfig := GState.MainFormConfig.LayersConfig.MarksShowConfig.GetStatic;
  if VMarksConfig.IsUseMarks then begin
    if VMarksConfig.IgnoreCategoriesVisible and VMarksConfig.IgnoreMarksVisible then begin
      RBall.Checked := true;
    end else begin
      RBchecked.Checked := true;
    end;
  end else begin
    RBnot.Checked := true;
  end;
  UpdateCategoryTree;
  UpdateMarksList;
  SBNavOnMark.Down:= GState.MainFormConfig.NavToPoint.IsActive;
  try
    ShowModal;
  finally
    TreeView1.OnChange:=nil;
    TreeView1.Items.Clear;
    MarksListBox.Clear;
    FreeAndNil(FCategoryList);
    FMarksList := nil;
  end;
end;

procedure TFMarksExplorer.CheckBox1Click(Sender: TObject);
var
  VNewVisible: Boolean;
  VCategory: TCategoryId;
begin
  VCategory := GetSelectedCategory;
  if VCategory <> nil then begin
    VNewVisible := CheckBox1.Checked;
    FMarkDBGUI.MarksDB.MarksDb.SetAllMarksInCategoryVisible(VCategory, VNewVisible);
    UpdateMarksList;
  end;
end;

procedure TFMarksExplorer.BtnAddCategoryClick(Sender: TObject);
var
  VCategory: TCategoryId;
begin
  VCategory := TCategoryId.Create;
  VCategory.id := -1;
  if FaddCategory.EditCategory(VCategory) then begin
    FMarkDBGUI.MarksDb.CategoryDB.WriteCategory(VCategory);
    UpdateCategoryTree;
  end else begin
    VCategory.Free;
  end;
end;

procedure TFMarksExplorer.SBNavOnMarkClick(Sender: TObject);
var
  VMark: IMarkFull;
  LL: TDoublePoint;
begin
  if (SBNavOnMark.Down) then begin
    VMark := GetSelectedMarkFull;
    if VMark <> nil then begin
      LL := VMark.GetGoToLonLat;
      GState.MainFormConfig.NavToPoint.StartNavToMark(VMark.id, LL);
    end else begin
      SBNavOnMark.Down:=not SBNavOnMark.Down
    end;
  end else begin
    GState.MainFormConfig.NavToPoint.StopNav;
  end;
end;

end.

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
  u_MarksSimple,
  u_MarksDbGUIHelper,
  Unit1, ComCtrls, ImgList;

type
  TFMarksExplorer = class(TCommonFormParent)
    grpMarks: TGroupBox;
    BtnGotoMark: TSpeedButton;
    BtnOpMark: TSpeedButton;
    MarksListBox: TCheckListBox;
    grpCategory: TGroupBox;
    BtnDelKat: TSpeedButton;
    OpenDialog: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    BtnDelMark: TSpeedButton;
    RBall: TRadioButton;
    RBchecked: TRadioButton;
    RBnot: TRadioButton;
    SpeedButton1: TSpeedButton;
    Bevel2: TBevel;
    BtnEditCategory: TSpeedButton;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    Button3: TButton;
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
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BtnDelMarkClick(Sender: TObject);
    procedure MarksListBoxClickCheck(Sender: TObject);
    procedure BtnOpMarkClick(Sender: TObject);
    procedure BtnGotoMarkClick(Sender: TObject);
    procedure BtnDelKatClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnEditCategoryClick(Sender: TObject);
    procedure MarksListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BtnAddCategoryClick(Sender: TObject);
    procedure SBNavOnMarkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
  private
    FCategoryList: TList;
    FMarkDBGUI: TMarksDbGUIHelper;
    procedure UpdateCategoryTree;
    function GetSelectedCategory: TCategoryId;
  public
    procedure EditMarks(AMarkDBGUI: TMarksDbGUIHelper);
  end;

var
  FMarksExplorer: TFMarksExplorer;

implementation

uses
  t_CommonTypes,
  u_GlobalState,
  i_ICoordConverter,
  u_MarksReadWriteSimple,
  USaveas,
  UaddPoint,
  UaddPoly,
  UaddLine,
  UImport,
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
      FCategoryList := FMarkDBGUI.MarksDB.GetCategoriesList;
      FMarkDBGUI.CategoryListToTree(FCategoryList, TreeView1.Items);
      TreeView1.SortType:=stText;
    finally
      TreeView1.Items.EndUpdate;
    end;
  finally
    TreeView1.OnChange := Self.TreeView1Change;
  end;
end;

procedure TFMarksExplorer.FormShow(Sender: TObject);
var
    i:integer;
begin
 case GState.show_point of
  mshAll: RBall.Checked:=true;
  mshChecked: RBchecked.Checked:=true;
  mshNone: RBnot.Checked:=true;
 end;
 for i:=1 to MarksListBox.items.Count do MarksListBox.Items.Objects[i-1].Free;
 MarksListBox.Clear;
 UpdateCategoryTree;
 SBNavOnMark.Down:= GState.MainFormConfig.NavToPoint.IsActive;
end;

function TFMarksExplorer.GetSelectedCategory: TCategoryId;
begin
  Result := nil;
  if TreeView1.Selected <> nil then begin
    Result := TCategoryId(TreeView1.Selected.Data);
  end;
end;

procedure TFMarksExplorer.Button2Click(Sender: TObject);
begin
 if RBall.Checked then GState.show_point := mshAll;
 if RBchecked.Checked then GState.show_point := mshChecked;
 if RBnot.Checked then GState.show_point := mshNone;
 close;
end;

procedure TFMarksExplorer.BtnDelMarkClick(Sender: TObject);
var
  VIndex: Integer;
  VId: integer;
begin
  VIndex := MarksListBox.ItemIndex;
  if VIndex>=0 then begin
    VId := TMarkId(MarksListBox.Items.Objects[VIndex]).id;
    if FMarkDBGUI.DeleteMarkModal(VId, Self.Handle) then begin
      MarksListBox.Items.Objects[VIndex].Free;
      MarksListBox.DeleteSelected;
    end;
  end;
end;

procedure TFMarksExplorer.MarksListBoxClickCheck(Sender: TObject);
var
  VIndex: integer;
  VMark: TMarkId;
begin
  VIndex := MarksListBox.ItemIndex;
  if VIndex >= 0 then begin
    VMark := TMarkId(MarksListBox.Items.Objects[VIndex]);
    if VMark <> nil then begin
      VMark.visible := MarksListBox.Checked[VIndex];
      GState.MarksDb.WriteMarkId(VMark);
    end;
  end;
end;

procedure TFMarksExplorer.BtnOpMarkClick(Sender: TObject);
var
  VId: Integer;
  VMark: TMarkFull;
begin
  if MarksListBox.ItemIndex>=0 then begin
    VId := TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id;
    VMark := GState.MarksDb.GetMarkByID(VId);
    if VMark <> nil then begin
      try
        if FMarkDBGUI.OperationMark(VMark) then begin
          close;
        end;
      finally
        VMark.Free;
      end;
    end;
  end;
end;

procedure TFMarksExplorer.BtnGotoMarkClick(Sender: TObject);
var
  VId: Integer;
  VMark: TMarkFull;
begin
  if MarksListBox.ItemIndex>=0 then begin
    VId := TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id;
    VMark := GState.MarksDb.GetMarkByID(VId);
    if VMark <> nil then begin
      try
        Fmain.topos(VMark.GetGoToLonLat, GState.ViewState.GetCurrentZoom, True);
      finally
        VMark.Free
      end;
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
      GState.MarksDb.DeleteCategoryWithMarks(VCategory);
      UpdateCategoryTree;
    end;
  end;
end;

procedure TFMarksExplorer.SpeedButton1Click(Sender: TObject);
var
  VIndex: Integer;
  VMarkId: TMarkId;
  VMark: TMarkFull;
  VCategory: TCategoryId;
begin
  VIndex := MarksListBox.ItemIndex;
  if VIndex >= 0 then begin
    VMarkId := TMarkId(MarksListBox.Items.Objects[VIndex]);
    VMark := GState.MarksDb.GetMarkByID(VMarkId.id);
    if VMark <> nil then begin
      try
        if FMarkDBGUI.EditMarkModal(VMark) then begin
          GState.MarksDb.WriteMark(VMark);
          VCategory := GetSelectedCategory;
          if VMark.CategoryId<>VCategory.id then begin
            MarksListBox.Items.Objects[VIndex].Free;
            MarksListBox.DeleteSelected;
          end else begin
            VMarkId.name := VMark.name;
            VMarkId.visible := VMark.visible;
            MarksListBox.Items.Strings[VIndex]:=VMarkId.name;
            MarksListBox.Checked[VIndex]:=VMarkId.visible;
          end;
        end;
      finally
        VMark.Free;
      end;
    end;
  end;
end;

procedure TFMarksExplorer.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  i:integer;
  VCategory: TCategoryId;
begin
  if (node<>nil)and(node.Data<>nil) then begin
    VCategory := TCategoryId(node.Data);
    GState.MarksDb.Marsk2StringsWithMarkId(VCategory, MarksListBox.Items);
    for i:=0 to MarksListBox.Count-1 do begin
      MarksListBox.Checked[i]:=TMarkId(MarksListBox.Items.Objects[i]).visible;
    end;
  end else begin
    for i:=0 to MarksListBox.Count-1 do begin
      MarksListBox.Items.Objects[i].Free;
    end;
    MarksListBox.Clear;
  end;
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
        GState.MarksDb.DeleteCategoryWithMarks(VCategory);
        UpdateCategoryTree;
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
      GState.MarksDb.WriteCategory(VCategory);
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
      GState.MarksDb.WriteCategory(VCategory);
    end;
  end;
end;

procedure TFMarksExplorer.Button1Click(Sender: TObject);
begin
 If (OpenDialog1.Execute) then
  if (FileExists(OpenDialog1.FileName)) then
   begin
    FImport.ImportFile(OpenDialog1.FileName, FMarkDBGUI);
    Self.FormShow(sender);
   end;
end;

procedure TFMarksExplorer.BtnEditCategoryClick(Sender: TObject);
var
  VCategory: TCategoryId;
begin
  VCategory := GetSelectedCategory;
  if VCategory <> nil then begin
    if FaddCategory.EditCategory(VCategory) then begin
      GState.MarksDb.WriteCategory(VCategory);
      UpdateCategoryTree;
    end;
  end;
end;

procedure TFMarksExplorer.MarksListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  VIndex: Integer;
  VMarkId: TMarkId;
begin
  If key=VK_DELETE then begin
    VIndex := MarksListBox.ItemIndex;
    if VIndex >= 0 then begin
      VMarkId := TMarkId(MarksListBox.Items.Objects[VIndex]);
      if FMarkDBGUI.DeleteMarkModal(VMarkId.id, Self.Handle) then begin
        VMarkId.Free;
        MarksListBox.DeleteSelected;
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
    GState.MarksDB.SetAllCategoriesVisible(VNewVisible);
    UpdateCategoryTree;
  end;
end;

procedure TFMarksExplorer.EditMarks(AMarkDBGUI: TMarksDbGUIHelper);
begin
  FMarkDBGUI := AMarkDBGUI;
  ShowModal;
end;

procedure TFMarksExplorer.CheckBox1Click(Sender: TObject);
var
  i:integer;
  VNewVisible: Boolean;
  VCategory: TCategoryId;
begin
  VCategory := GetSelectedCategory;
  if VCategory <> nil then begin
    VNewVisible := CheckBox1.Checked;
    GState.MarksDB.SetAllMarksInCategoryVisible(VCategory, VNewVisible);

    GState.MarksDb.Marsk2StringsWithMarkId(VCategory, MarksListBox.Items);
    for i:=0 to MarksListBox.Count-1 do begin
      MarksListBox.Checked[i]:=TMarkId(MarksListBox.Items.Objects[i]).visible;
    end;
  end;
end;

procedure TFMarksExplorer.Button3Click(Sender: TObject);
begin
  fmain.generate_im;
end;

procedure TFMarksExplorer.BtnAddCategoryClick(Sender: TObject);
var
  VCategory: TCategoryId;
begin
  VCategory := TCategoryId.Create;
  VCategory.id := -1;
  if FaddCategory.EditCategory(VCategory) then begin
    GState.MarksDb.WriteCategory(VCategory);
    UpdateCategoryTree;
  end else begin
    VCategory.Free;
  end;
end;

procedure TFMarksExplorer.SBNavOnMarkClick(Sender: TObject);
var
  VIndex: Integer;
  VId:integer;
  VMark: TMarkFull;
  LL:TDoublePoint;
begin
  if (SBNavOnMark.Down) then begin
    VIndex := MarksListBox.ItemIndex;
    if (VIndex >= 0) then begin
      VId:=TMarkId(MarksListBox.Items.Objects[VIndex]).Id;
      VMark := GState.MarksDb.GetMarkByID(VId);
      if VMark <> nil then begin
        try
          LL := VMark.GetGoToLonLat;
          GState.MainFormConfig.NavToPoint.StartNavToMark(VId, LL);
        finally
          VMark.Free;
        end;
      end;
    end else begin
      SBNavOnMark.Down:=not SBNavOnMark.Down
    end;
  end else begin
    GState.MainFormConfig.NavToPoint.StopNav;
  end;
end;

procedure TFMarksExplorer.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i: integer;
begin
 for i:=0 to MarksListBox.items.Count - 1 do MarksListBox.Items.Objects[i].Free;
 MarksListBox.Clear;
 TreeView1.OnChange:=nil;
 TreeView1.Items.Clear;
  FreeAndNil(FCategoryList);
end;

end.

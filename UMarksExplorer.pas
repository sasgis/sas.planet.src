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
  Unit1, ComCtrls, ImgList;

type
  TFMarksExplorer = class(TCommonFormParent)
    GroupBox1: TGroupBox;
    BtnGotoMark: TSpeedButton;
    BtnOpMark: TSpeedButton;
    MarksListBox: TCheckListBox;
    GroupBox2: TGroupBox;
    BtnDelKat: TSpeedButton;
    OpenDialog: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    BtnDelMark: TSpeedButton;
    Bevel1: TBevel;
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
  public
  end;

var
  katitems:TStrings;
  FMarksExplorer: TFMarksExplorer;
  function DeleteMarkModal(id:integer;handle:THandle):boolean;
  function OperationMark(AMark: TMarkFull):boolean;
  function AddKategory(name:string): integer;
  function GetMarkLength(AMark: TMarkFull):extended;
  function GetMarkSq(AMark: TMarkFull):extended;
  function EditMarkModal(AMark: TMarkFull):boolean;
  function AddNewPointModal(ALonLat: TExtendedPoint): Boolean;
  function SavePolyModal(AID: Integer; ANewArrLL: TExtendedPointArray): Boolean;
  function SaveLineModal(AID: Integer; ANewArrLL: TExtendedPointArray; ADescription: string): Boolean;

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

function AddKategory(name:string): Integer;
var
  VCategory: TCategoryId;
begin
  VCategory := TCategoryId.Create;
  try
    VCategory.id := -1;
    VCategory.name := name;
    VCategory.visible := True;
    VCategory.AfterScale := 3;
    VCategory.BeforeScale := 19;
    GState.MarksDb.WriteCategory(VCategory);
    Result := VCategory.id;
  finally
    VCategory.Free;
  end;
end;

function AddNewPointModal(ALonLat: TExtendedPoint): Boolean;
var
  VMark: TMarkFull;
begin
  VMark := TMarkFull.Create;
  try
    VMark.id := -1;
    SetLength(VMark.Points, 1);
    VMark.Points[0] := ALonLat;
    Result := FaddPoint.EditMark(VMark);
    if Result then begin
      GState.MarksDb.WriteMark(VMark);
      GState.MarksDb.SaveMarks2File;
    end;
  finally
    VMark.Free;
  end;
end;

function SavePolyModal(AID: Integer; ANewArrLL: TExtendedPointArray): Boolean;
var
  VMark: TMarkFull;
begin
  Result := False;
  if AID < 0 then begin
    VMark := TMarkFull.Create;
  end else begin
    VMark := GState.MarksDb.GetMarkByID(AID)
  end;
  if VMark <> nil then begin
    try
      VMark.id := AID;
      VMark.Points := Copy(ANewArrLL);
      VMark.ClosePoly;
      Result := FaddPoly.EditMark(VMark);
      if Result then begin
        GState.MarksDb.WriteMark(VMark);
        GState.MarksDb.SaveMarks2File;
      end;
    finally
      VMark.Free;
    end;
  end;
end;

function SaveLineModal(AID: Integer; ANewArrLL: TExtendedPointArray; ADescription: string): Boolean;
var
  VMark: TMarkFull;
begin
  Result := False;
  if AID < 0 then begin
    VMark := TMarkFull.Create;
  end else begin
    VMark := GState.MarksDb.GetMarkByID(AID)
  end;
  if VMark <> nil then begin
    try
      VMark.id := AID;
      if VMark.id < 0 then begin
        VMark.Desc := ADescription;
      end;
      VMark.Points := Copy(ANewArrLL);
      Result := FaddLine.EditMark(VMark);
      if Result then begin
        GState.MarksDb.WriteMark(VMark);
        GState.MarksDb.SaveMarks2File;
      end;
    finally
      VMark.Free;
    end;
  end;
end;

function EditMarkModal(AMark: TMarkFull):boolean;
begin
  Result := false;
  if AMark.IsPoint then begin
    result:=FaddPoint.EditMark(AMark);
  end else if AMark.IsPoly then begin
    result:=FaddPoly.EditMark(AMark);
  end else if AMark.IsLine then begin
    result:=FaddLine.EditMark(AMark);
  end;
end;

procedure DrawTreeCategory(TreeView1: TTreeView; Strs: TStrings);
var
  CachedStrs: TStringList; // CachedStrs вводитс€ дл€ ускорени€ поиска
  // в уже готовом дереве.

  procedure AddItem(Lev: Integer; ParentNode: TTreeNode; S: string; Data:TObject);
    function FindNodeWithText(AParent: TTreeNode; const S: string): TTreeNode;
    var
      K: Integer;
      fStr: string;
      tmpNode: TTreeNode;
    begin
      if TCategoryId(Data).id<>123123123 then
      Result := nil;
      fStr := S + IntToStr(Integer(AParent));
      K := CachedStrs.IndexOf(fStr);
      if K > -1 then
        Result := Pointer(CachedStrs.Objects[K])
      else
      begin
        if AParent <> nil then
          tmpNode := AParent.getFirstChild
        else
          tmpNode := TreeView1.Items.GetFirstNode;
        while tmpNode <> nil do
        begin
          if tmpNode.Text = S then
          begin
            Result := tmpNode;
            CachedStrs.AddObject(fStr, Pointer(tmpNode));
            break;
          end;
          tmpNode := tmpNode.getNextSibling;
        end;
      end
    end;

  var
    prefix: string;
    ID: Integer;
    aNode: TTreeNode;
  begin
    if TCategoryId(Data).id<>123123123 then
    if S='' then begin
      Exit;
    end;
    ID:=Pos('\', S);
    prefix:='';
    if ID > 0 then begin
      prefix:=Copy(S, 1, ID - 1)
    end else begin
      prefix:=S;
      S := '';
    end;
    aNode := FindNodeWithText(ParentNode, prefix);
    if aNode = nil then
    begin
      if ID>0 then begin
        aNode := TreeView1.Items.AddChildObject(ParentNode, prefix, nil);
        aNode.StateIndex:=0;
      end else begin
        aNode := TreeView1.Items.AddChildObject(ParentNode, prefix, Data);
        aNode.StateIndex :=2;
        if TCategoryId(Data).visible then begin
          aNode.StateIndex :=1
        end;
      end;
    end else begin
      aNode.Data:=Data;
    end;
    AddItem(Lev + 1, aNode, Copy(S, ID + 1, Length(S)),Data);
  end;

var
  K: Integer;
begin
  CachedStrs := TStringList.Create;
  CachedStrs.Duplicates := dupIgnore;
  CachedStrs.Sorted := True;
  try
    TreeView1.OnChange:=nil;
    TreeView1.Items.Clear;
    TreeView1.Items.BeginUpdate;
    TreeView1.SortType := stNone;
    for K := 0 to Strs.Count - 1 do
      AddItem(0, nil, Strs[K], Strs.Objects[K]);
    TreeView1.SortType:=stText;
  finally
    TreeView1.Items.EndUpdate;
    CachedStrs.Free;
    TreeView1.OnChange:=fMarksExplorer.TreeView1Change;
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
 katitems:=TStringList.create;
 GState.MarksDb.Kategory2StringsWithObjects(katitems);
 DrawTreeCategory(TreeView1,katitems);
 SBNavOnMark.Down:= Fmain.LayerMapNavToMark.Visible;
end;

procedure TFMarksExplorer.Button2Click(Sender: TObject);
begin
 GState.MarksDb.SaveMarks2File;
 if RBall.Checked then GState.show_point := mshAll;
 if RBchecked.Checked then GState.show_point := mshChecked;
 if RBnot.Checked then GState.show_point := mshNone;
 close;
end;

function DeleteMarkModal(id:integer;handle:THandle):boolean;
var
  VMarkId: TMarkId;
begin
  result:=false;
  VMarkId := GState.MarksDb.GetMarkIdByID(id);
  if VMarkId <> nil then begin
    try
      if MessageBox(handle,pchar(SAS_MSG_youasure+' "'+VMarkId.name+'"'),pchar(SAS_MSG_coution),36)=IDNO then exit;
      result:=GState.MarksDb.DeleteMark(VMarkId);
    finally
      VMarkId.Free;
    end;
  end;
end;

function GetMarkLength(AMark: TMarkFull):extended;
var
  i:integer;
  VConverter: ICoordConverter;
  VPointCount: Integer;
begin
  Result:=0;
  VConverter := GState.ViewState.GetCurrentCoordConverter;
  VPointCount := Length(AMark.Points);
  if (VPointCount > 1) then begin
    for i:=0 to VPointCount-2 do begin
      Result:=Result+ VConverter.CalcDist(AMark.Points[i], AMark.Points[i+1]);
    end;
  end;
end;

function GetMarkSq(AMark: TMarkFull):extended;
var
  VConverter: ICoordConverter;
begin
  Result:=0;
  VConverter := GState.ViewState.GetCurrentCoordConverter;
  if (Length(AMark.Points) > 1) then begin
    result:= VConverter.CalcPoligonArea(AMark.Points);
  end;
end;

function OperationMark(AMark: TMarkFull):boolean;
begin
  Result:=false;
  if AMark.IsPoly then begin
    Fsaveas.Show_(GState.ViewState.GetCurrentZoom, AMark.Points);
    Result:=true;
  end else begin
    ShowMessage(SAS_MSG_FunExForPoly);
  end;
end;

procedure TFMarksExplorer.BtnDelMarkClick(Sender: TObject);
var
  VIndex: Integer;
  VId: integer;
begin
  VIndex := MarksListBox.ItemIndex;
  if VIndex>=0 then begin
    VId := TMarkId(MarksListBox.Items.Objects[VIndex]).id;
    if DeleteMarkModal(VId,Self.Handle) then begin
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
  VMark := TMarkId(MarksListBox.Items.Objects[VIndex]);
  VMark.visible := MarksListBox.Checked[VIndex];
  GState.MarksDb.WriteMarkId(VMark);
  GState.MarksDb.SaveMarks2File;
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
        if OperationMark(VMark) then begin
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
    try
      Fmain.topos(VMark.GetGoToLonLat, GState.ViewState.GetCurrentZoom, True);
    finally
      VMark.Free
    end;
  end;
end;

procedure TFMarksExplorer.BtnDelKatClick(Sender: TObject);
var
  VCategory: TCategoryId;
begin
  if TreeView1.Selected <> nil then begin
    if not TreeView1.Selected.HasChildren then begin
      VCategory := TCategoryId(TreeView1.Selected.Data);
      if MessageBox(Self.handle,pchar(SAS_MSG_youasure+' "'+VCategory.name+'"'),pchar(SAS_MSG_coution),36)=IDYES then begin
        GState.MarksDb.DeleteCategoryWithMarks(VCategory);
        katitems.Delete(katitems.IndexOfObject(VCategory));
        VCategory.Free;
        DrawTreeCategory(TreeView1,katitems);
      end;
    end else begin
      ShowMessage(SAS_MSG_NotDelWhereHasChildren);
    end;
  end;
end;

procedure TFMarksExplorer.SpeedButton1Click(Sender: TObject);
var
  VIndex: Integer;
  VMarkId: TMarkId;
  VMark: TMarkFull;

begin
  VIndex := MarksListBox.ItemIndex;
  if VIndex >= 0 then begin
    VMarkId := TMarkId(MarksListBox.Items.Objects[VIndex]);
    VMark := GState.MarksDb.GetMarkByID(VMarkId.id);
    if VMark <> nil then begin
      try
        if EditMarkModal(VMark) then begin
          GState.MarksDb.WriteMark(VMark);
          GState.MarksDb.SaveMarks2File;
          if VMark.CategoryId<>TCategoryId(TreeView1.Selected.Data).id then begin
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
    GState.MarksDb.Marsk2StringsWithMarkId(TCategoryId(node.Data), MarksListBox.Items);
    for i:=0 to MarksListBox.Count-1 do begin
      MarksListBox.Checked[i]:=TMarkId(MarksListBox.Items.Objects[i]).visible;
    end;
  end else begin
    MarksListBox.Clear;
  end;
end;

procedure TFMarksExplorer.TreeView1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  VCategory: TCategoryId;
begin
  If key=VK_DELETE then begin
    if TreeView1.Selected <> nil then begin
      VCategory := TCategoryId(TreeView1.Selected.Data);
      if MessageBox(Self.handle,pchar(SAS_MSG_youasure),pchar(SAS_MSG_coution),36)=IDYES then begin
        GState.MarksDb.DeleteCategoryWithMarks(VCategory);
        VCategory.Free;
        //KategoryListBox.DeleteSelected;
      end;
    end;
  end;

  if Key=VK_SPACE then begin
    if TreeView1.Selected<>nil then begin
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
  i:integer;
  VCategory: TCategoryId;
begin
  if htOnStateIcon in TreeView1.GetHitTestInfoAt(X,Y) then begin
    VCategory := TCategoryId(TreeView1.GetNodeAt(X,Y).Data);
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

procedure TFMarksExplorer.Button1Click(Sender: TObject);
begin
 If (OpenDialog1.Execute) then
  if (FileExists(OpenDialog1.FileName)) then
   begin
    FImport.ImportFile(OpenDialog1.FileName);
    Self.FormShow(sender);
   end;
end;

procedure TFMarksExplorer.BtnEditCategoryClick(Sender: TObject);
var
  VCategory: TCategoryId;
begin
  if TreeView1.Selected <> nil then begin
    VCategory := TCategoryId(TreeView1.Selected.data);
    if FaddCategory.EditCategory(VCategory) then begin
      GState.MarksDb.WriteCategory(VCategory);

      GState.MarksDb.Kategory2StringsWithObjects(katitems);
      DrawTreeCategory(TreeView1,katitems);
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
      if DeleteMarkModal(VMarkId.id, Self.Handle) then begin
        VMarkId.Free;
        MarksListBox.DeleteSelected;
      end;
    end;
  end;
end;

procedure TFMarksExplorer.CheckBox2Click(Sender: TObject);
var
  i:integer;
  VNewVisible: Boolean;
begin
  if TreeView1.Items.Count>0 then begin
    VNewVisible := CheckBox2.Checked;
    for i:=0 to TreeView1.Items.Count-1 do begin
      if VNewVisible then begin
        TreeView1.Items.Item[i].StateIndex := 1;
      end else begin
        TreeView1.Items.Item[i].StateIndex := 2;
      end;
      TCategoryId(TreeView1.Items.Item[i].Data).visible := VNewVisible;
      GState.MarksDb.WriteCategory(TreeView1.Items.Item[i].Data);
    end;
    //WriteCategoriesList(TreeView1.Items.Items);
  end;
end;

procedure TFMarksExplorer.CheckBox1Click(Sender: TObject);
var
  i:integer;
  VNewVisible: Boolean;
begin
  if MarksListBox.Count>0 then begin
    VNewVisible := CheckBox1.Checked;
    for i:=0 to MarksListBox.Count-1 do begin
      MarksListBox.Checked[i]:=VNewVisible;
      TMarkId(MarksListBox.Items.Objects[i]).visible := VNewVisible;
    end;
    GState.MarksDb.WriteMarkIdList(MarksListBox.Items);
    GState.MarksDb.SaveMarks2File;
  end;
end;

procedure TFMarksExplorer.Button3Click(Sender: TObject);
begin
  GState.MarksDb.SaveMarks2File;
  fmain.generate_im;
end;

procedure TFMarksExplorer.BtnAddCategoryClick(Sender: TObject);
var
  VCategory: TCategoryId;
  VIndex: Integer;
begin
  VCategory := TCategoryId.Create;
  VCategory.id := -1;
  if FaddCategory.EditCategory(VCategory) then begin
    GState.MarksDb.WriteCategory(VCategory);
    katitems.AddObject(VCategory.name, VCategory);
    DrawTreeCategory(TreeView1,katitems);
  end else begin
    VCategory.Free;
  end;
end;

procedure TFMarksExplorer.SBNavOnMarkClick(Sender: TObject);
var
  VIndex: Integer;
  VId:integer;
  VMark: TMarkFull;
  LL:TExtendedPoint;
begin
  if (SBNavOnMark.Down) then begin
    VIndex := MarksListBox.ItemIndex;
    if (VIndex >= 0) then begin
      VId:=TMarkId(MarksListBox.Items.Objects[VIndex]).Id;
      VMark := GState.MarksDb.GetMarkByID(VId);
      try
        LL := VMark.GetGoToLonLat;
        FMain.LayerMapNavToMark.StartNav(LL, VId);
      finally
        VMark.Free;
      end;
    end else begin
      SBNavOnMark.Down:=not SBNavOnMark.Down
    end;
  end else begin
    FMain.LayerMapNavToMark.Visible := False;
  end;
end;

procedure TFMarksExplorer.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i: integer;
begin
 for i:=1 to MarksListBox.items.Count do MarksListBox.Items.Objects[i-1].Free;
 MarksListBox.Clear;
 for i:=0 to katitems.Count-1 do katitems.Objects[i].Free;
 TreeView1.OnChange:=nil;
 TreeView1.Items.Clear;
 katitems.free;
end;

end.

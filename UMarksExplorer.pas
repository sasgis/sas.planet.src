unit UMarksExplorer;

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
  DB,
  Dialogs,
  StdCtrls,
  CheckLst,
  Buttons,
  ExtCtrls,
  DBClient,
  GR32,
  UResStrings,
  UGeoFun,
  t_GeoTypes,
  u_MarksSimple,
  Unit1;

type
  TFMarksExplorer = class(TForm)
    GroupBox1: TGroupBox;
    BtnGotoMark: TSpeedButton;
    BtnOpMark: TSpeedButton;
    MarksListBox: TCheckListBox;
    GroupBox2: TGroupBox;
    BtnDelKat: TSpeedButton;
    KategoryListBox: TCheckListBox;
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
    procedure FormShow(Sender: TObject);
    procedure KategoryListBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure BtnDelMarkClick(Sender: TObject);
    procedure MarksListBoxClickCheck(Sender: TObject);
    procedure BtnOpMarkClick(Sender: TObject);
    procedure BtnGotoMarkClick(Sender: TObject);
    procedure KategoryListBoxClickCheck(Sender: TObject);
    procedure BtnDelKatClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnEditCategoryClick(Sender: TObject);
    procedure MarksListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KategoryListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BtnAddCategoryClick(Sender: TObject);
    procedure SBNavOnMarkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMarksExplorer: TFMarksExplorer;
  function DeleteMarkModal(id:integer;handle:THandle):boolean;
  function OperationMark(AMark: TMarkFull):boolean;
  function AddKategory(name:string): integer;
  function GetMarkLength(AMark: TMarkFull):extended;
  function GetMarkSq(AMark: TMarkFull):extended;
  function EditMarkF(id:integer; var arr:TExtendedPointArray):TAOperation;
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
    WriteCategory(VCategory);
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
      WriteMark(VMark);
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
    VMark := GetMarkByID(AID)
  end;
  if VMark <> nil then begin
    try
      VMark.id := AID;
      VMark.Points := Copy(ANewArrLL);
      VMark.ClosePoly;
      Result := FaddPoly.EditMark(VMark);
      if Result then begin
        WriteMark(VMark);
        SaveMarks2File;
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
    VMark := GetMarkByID(AID)
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
        WriteMark(VMark);
        SaveMarks2File;
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

function EditMarkF(id:integer;var arr:TExtendedPointArray):TAOperation;
var
  VMark: TMarkFull;
begin
  Result := ao_movemap;
  VMark := GetMarkByID(id);
  try
    if VMark.IsPoint then begin
      result:=ao_edit_point;
      if FaddPoint.EditMark(VMark) then begin
        WriteMark(VMark);
        SaveMarks2File;
      end;
      Result := ao_movemap;
    end else if VMark.IsPoly then begin
      arr:=VMark.Points;
      result:=ao_edit_poly;
    end else if VMark.IsLine then begin
      arr:=VMark.Points;
      result:=ao_edit_line;
    end;
  finally
    VMark.Free;
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
 Kategory2StringsWithObjects(KategoryListBox.items);
  for i:=0 to KategoryListBox.items.Count - 1 do begin
    KategoryListBox.Checked[i] := TCategoryId(KategoryListBox.Items.Objects[i]).visible;
  end;

 SBNavOnMark.Down:= Fmain.LayerMapNavToMark.Visible;
end;

procedure TFMarksExplorer.KategoryListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  VIndex: integer;
  i:integer;
begin
  VIndex := KategoryListBox.ItemIndex;
  if VIndex >= 0 then begin
    Marsk2StringsWithMarkId(TCategoryId(KategoryListBox.Items.Objects[VIndex]), MarksListBox.Items);
    for i:=0 to MarksListBox.Count-1 do begin
      MarksListBox.Checked[i]:=TMarkId(MarksListBox.Items.Objects[i]).visible;
    end;
  end;
end;

procedure TFMarksExplorer.Button2Click(Sender: TObject);
begin
 SaveMarks2File;
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
  VMarkId := GetMarkIdByID(id);
  if VMarkId <> nil then begin
    try
      if MessageBox(handle,pchar(SAS_MSG_youasure+' "'+VMarkId.name+'"'),pchar(SAS_MSG_coution),36)=IDNO then exit;
      result:=DeleteMark(VMarkId);
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
    Fmain.LayerSelection.Redraw;
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
  WriteMarkId(VMark);
end;

procedure TFMarksExplorer.BtnOpMarkClick(Sender: TObject);
var
  VId: Integer;
  VMark: TMarkFull;
begin
  if MarksListBox.ItemIndex>=0 then begin
    VId := TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id;
    VMark := GetMarkByID(VId);
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
    VMark := GetMarkByID(VId);
    try
      Fmain.topos(VMark.GetGoToLonLat, GState.ViewState.GetCurrentZoom, True);
    finally
      VMark.Free
    end;
  end;
end;

procedure TFMarksExplorer.KategoryListBoxClickCheck(Sender: TObject);
var
  VIndex: integer;
  VCategory: TCategoryId;
begin
  VIndex := KategoryListBox.ItemIndex;
  VCategory := TCategoryId(KategoryListBox.Items.Objects[VIndex]);
  VCategory.visible := KategoryListBox.Checked[VIndex];
  WriteCategory(VCategory);
end;

procedure TFMarksExplorer.BtnDelKatClick(Sender: TObject);
var
  VIndex: Integer;
  VCategory: TCategoryId;
begin
  VIndex := KategoryListBox.ItemIndex;
  if VIndex >= 0 then begin
    VCategory := TCategoryId(KategoryListBox.Items.Objects[VIndex]);
    if MessageBox(Self.handle,pchar(SAS_MSG_youasure),pchar(SAS_MSG_coution),36)=IDYES then begin
      DeleteCategoryWithMarks(VCategory);
      VCategory.Free;
      KategoryListBox.DeleteSelected;
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
    VMark := GetMarkByID(VMarkId.id);
    if VMark <> nil then begin
      try
        if EditMarkModal(VMark) then begin
          WriteMark(VMark);
          SaveMarks2File;
          if VMark.CategoryId<>TCategoryId(KategoryListBox.Items.Objects[KategoryListBox.ItemIndex]).id then begin
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
  VIndex: integer;
  VCategory: TCategoryId;
begin
  VIndex := KategoryListBox.ItemIndex;
  if VIndex >=0 then begin
    VCategory := TCategoryId(KategoryListBox.Items.Objects[VIndex]);
    if FaddCategory.EditCategory(VCategory) then begin
      WriteCategory(VCategory);
      KategoryListBox.Items.Strings[VIndex] := VCategory.name;
      KategoryListBox.Checked[VIndex] := VCategory.visible;
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

procedure TFMarksExplorer.KategoryListBoxKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  VIndex: Integer;
  VCategory: TCategoryId;
begin
  If key=VK_DELETE then begin
    VIndex := KategoryListBox.ItemIndex;
    if VIndex >= 0 then begin
      VCategory := TCategoryId(KategoryListBox.Items.Objects[VIndex]);
      if MessageBox(Self.handle,pchar(SAS_MSG_youasure),pchar(SAS_MSG_coution),36)=IDYES then begin
        DeleteCategoryWithMarks(VCategory);
        VCategory.Free;
        KategoryListBox.DeleteSelected;
      end;
    end;
  end;
end;

procedure TFMarksExplorer.CheckBox2Click(Sender: TObject);
var
  i:integer;
  VNewVisible: Boolean;
begin
  if KategoryListBox.Count>0 then begin
    VNewVisible := CheckBox2.Checked;
    for i:=0 to KategoryListBox.Count-1 do begin
      KategoryListBox.Checked[i] := VNewVisible;
      TCategoryId(KategoryListBox.Items.Objects[i]).visible := VNewVisible;
    end;
    WriteCategoriesList(KategoryListBox.Items);
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
    WriteMarkIdList(MarksListBox.Items);
  end
end;

procedure TFMarksExplorer.Button3Click(Sender: TObject);
begin
 SaveMarks2File;
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
    WriteCategory(VCategory);
    VIndex := KategoryListBox.Items.AddObject(VCategory.name, VCategory);
    KategoryListBox.Checked[VIndex]:=VCategory.visible;
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
      VMark := GetMarkByID(VId);
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
 for i:=1 to KategoryListBox.items.Count do KategoryListBox.Items.Objects[i-1].Free;
 KategoryListBox.Clear;
end;

end.

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

TCategoryId = class
 id:integer;
 name:string;
 AfterScale: Integer;
 BeforeScale: Integer;
 visible:boolean;
end;

TMarkId = class
 name:string;
 id:integer;
 visible:boolean;
end;

TMarkFull = class(TMarkId)
  CategoryId: Integer;
  Desc: string;
  LLRect: TExtendedRect;
  Points: TExtendedPointArray;
  PicName: string;
  Color1: TColor32;
  Color2: TColor32;
  Scale1: Integer;
  Scale2: Integer;
end;
var
  FMarksExplorer: TFMarksExplorer;
  function GetMarkByID(id:integer): TMarkFull;
  function DeleteMark(id:integer;handle:THandle):boolean;
  function OperationMark(AMark: TMarkFull):boolean;
  function AddKategory(name:string): integer;
  procedure Kategory2StringsWithObjects(AStrings:TStrings);
  function GetGoToMarkLonLat(AMark: TMarkFull): TExtendedPoint;
  function GetMarkLength(AMark: TMarkFull):extended;
  function GetMarkSq(AMark: TMarkFull):extended;
  function Blob2ExtArr(Blobfield:Tfield):TExtendedPointArray;
  procedure BlobFromExtArr(AArr:TExtendedPointArray; Blobfield: Tfield); overload;
  procedure BlobFromExtArr(APoint:TExtendedPoint; Blobfield: Tfield); overload;
  function SaveMarks2File:boolean;
  function SaveCategory2File:boolean;
  function EditMarkF(id:integer; var arr:TExtendedPointArray):TAOperation;
  function GetMarksFileterByCategories(AZoom: Byte): string;
  function AddNewPointModal(ALonLat: TExtendedPoint): Boolean;
  function SavePolyModal(AID: Integer; ANewArrLL: TExtendedPointArray): Boolean;
  function SaveLineModal(AID: Integer; ANewArrLL: TExtendedPointArray; ADescription: string): Boolean;

implementation

uses
  t_CommonTypes,
  u_GlobalState,
  i_ICoordConverter,
  USaveas,
  UaddPoint,
  UaddPoly,
  UaddLine,
  UImport,
  UAddCategory;

{$R *.dfm}
procedure ReadCurrentCategory(ACategory: TCategoryId);
begin
  ACategory.name:=Fmain.CDSKategory.fieldbyname('name').AsString;
  ACategory.id:=Fmain.CDSKategory.fieldbyname('id').AsInteger;
  ACategory.visible:=Fmain.CDSKategory.FieldByName('visible').AsBoolean;
  ACategory.AfterScale:=Fmain.CDSKategory.fieldbyname('AfterScale').AsInteger;
  ACategory.BeforeScale:=Fmain.CDSKategory.fieldbyname('BeforeScale').AsInteger;
end;

procedure WriteCurrentCategory(ACategory: TCategoryId);
begin
  Fmain.CDSKategory.fieldbyname('name').AsString:=ACategory.name;
  Fmain.CDSKategory.FieldByName('visible').AsBoolean:=ACategory.visible;
  Fmain.CDSKategory.fieldbyname('AfterScale').AsInteger:=ACategory.AfterScale;
  Fmain.CDSKategory.fieldbyname('BeforeScale').AsInteger:=ACategory.BeforeScale;
end;

procedure WriteCategory(ACategory: TCategoryId);
begin
  if ACategory.id < 0 then begin
    Fmain.CDSKategory.Insert;
  end else begin
    Fmain.CDSKategory.Locate('id', ACategory.id,[]);
    Fmain.CDSKategory.Edit;
  end;
  WriteCurrentCategory(ACategory);
  Fmain.CDSKategory.post;
  ACategory.id:=Fmain.CDSKategory.fieldbyname('id').AsInteger;
  SaveCategory2File;
end;

procedure DeleteCategoryWithMarks(ACategory: TCategoryId);
begin
  FMain.CDSmarks.Filtered:=false;
  Fmain.CDSmarks.Filter:='categoryid = '+inttostr(ACategory.id);
  Fmain.CDSmarks.Filtered:=true;
  Fmain.CDSmarks.First;
  while not(Fmain.CDSmarks.Eof) do begin
    Fmain.CDSmarks.Delete;
  end;
  if Fmain.CDSKategory.Locate('id',ACategory.id,[]) then begin
    Fmain.CDSKategory.Delete;
  end;
  SaveCategory2File;
end;


function GetMarksFileterByCategories(AZoom: Byte): string;
begin
  Result := '';
  if GState.show_point = mshChecked then begin
    FMain.CDSKategory.Filter:='visible = 1 and ( AfterScale <= '+inttostr(AZoom + 1)+' and BeforeScale >= '+inttostr(AZoom + 1)+' )';
    FMain.CDSKategory.Filtered:=true;
    FMain.CDSKategory.First;
    if FMain.CDSKategory.Eof then begin
      FMain.CDSKategory.Filtered:=false;
      exit;
    end;
    if not(FMain.CDSKategory.Eof) then begin
      Result:=Result+'(';
      while not(FMain.CDSKategory.Eof) do begin
        Result:=Result+'categoryid='+FMain.CDSKategory.fieldbyname('id').AsString;
        FMain.CDSKategory.Next;
        if not(FMain.CDSKategory.Eof) then begin
          Result:=Result+' or ';
        end;
      end;
      Result:=Result+')';
    end;
    FMain.CDSKategory.Filtered:=false;
  end;
end;

procedure ReadCurrentMarkId(AMark: TMarkId);
begin
  AMark.id := Fmain.CDSmarks.fieldbyname('id').AsInteger;
  AMark.name := Fmain.CDSmarks.FieldByName('name').AsString;
  AMark.visible := Fmain.CDSmarks.FieldByName('Visible').AsBoolean;
end;

procedure ReadCurrentMark(AMark: TMarkFull);
begin
  ReadCurrentMarkId(AMark);
  AMark.Points := Blob2ExtArr(Fmain.CDSmarks.FieldByName('LonLatArr'));
  AMark.CategoryId := Fmain.CDSmarkscategoryid.AsInteger;
  AMark.Desc := Fmain.CDSmarks.FieldByName('descr').AsString;
  AMark.LLRect.Left := Fmain.CDSmarks.FieldByName('LonL').AsFloat;
  AMark.LLRect.Top := Fmain.CDSmarks.FieldByName('LatT').AsFloat;
  AMark.LLRect.Right := Fmain.CDSmarks.FieldByName('LonR').AsFloat;
  AMark.LLRect.Bottom := Fmain.CDSmarks.FieldByName('LatB').AsFloat;
  AMark.PicName := Fmain.CDSmarks.FieldByName('PicName').AsString;
  AMark.Color1 := TColor32(Fmain.CDSmarks.FieldByName('Color1').AsInteger);
  AMark.Color2 := TColor32(Fmain.CDSmarks.FieldByName('Color2').AsInteger);
  AMark.Scale1 := Fmain.CDSmarks.FieldByName('Scale1').AsInteger;
  AMark.Scale2 := Fmain.CDSmarks.FieldByName('Scale2').AsInteger;
end;

procedure WriteCurrentMarkId(AMark: TMarkId);
begin
  Fmain.CDSmarks.FieldByName('name').AsString := AMark.name;
  Fmain.CDSmarks.FieldByName('Visible').AsBoolean := AMark.visible;
end;

procedure WriteCurrentMark(AMark: TMarkFull);
begin
  WriteCurrentMarkId(AMark);
  BlobFromExtArr(AMark.Points, Fmain.CDSmarks.FieldByName('LonLatArr'));
  Fmain.CDSmarkscategoryid.AsInteger := AMark.CategoryId;
  Fmain.CDSmarks.FieldByName('descr').AsString := AMark.Desc;
  Fmain.CDSmarks.FieldByName('LonL').AsFloat := AMark.LLRect.Left;
  Fmain.CDSmarks.FieldByName('LatT').AsFloat := AMark.LLRect.Top;
  Fmain.CDSmarks.FieldByName('LonR').AsFloat := AMark.LLRect.Right;
  Fmain.CDSmarks.FieldByName('LatB').AsFloat := AMark.LLRect.Bottom;
  Fmain.CDSmarks.FieldByName('PicName').AsString := AMark.PicName;
  Fmain.CDSmarks.FieldByName('Color1').AsInteger := AMark.Color1;
  Fmain.CDSmarks.FieldByName('Color2').AsInteger := AMark.Color2;
  Fmain.CDSmarks.FieldByName('Scale1').AsInteger := AMark.Scale1;
  Fmain.CDSmarks.FieldByName('Scale2').AsInteger := AMark.Scale2;
end;

function GetMarkByID(id:integer): TMarkFull;
begin
  Result := nil;
  if FMain.CDSmarks.Locate('id',id,[]) then begin
    Result := TMarkFull.Create;
    ReadCurrentMark(Result);
  end;
end;

procedure WriteMark(AMark: TMarkFull);
begin
  if AMark.id >= 0 then begin
    Fmain.CDSmarks.Locate('id', AMark.id,[]);
    Fmain.CDSmarks.Edit;
  end else begin
    Fmain.CDSmarks.Insert;
  end;
  WriteCurrentMark(AMark);
  Fmain.CDSmarks.Post;
  SaveMarks2File;
end;

procedure WriteMarkId(AMark: TMarkId);
begin
  if AMark.id >= 0 then begin
    Fmain.CDSmarks.Locate('id', AMark.id,[]);
    Fmain.CDSmarks.Edit;
    WriteCurrentMarkId(AMark);
    Fmain.CDSmarks.Post;
  end;
end;

function SaveMarks2File:boolean;
var ms:TMemoryStream;
    XML:string;
begin
 result:=true;
 ms:=TMemoryStream.Create;
 try
   try
     Fmain.CDSmarks.MergeChangeLog;
     XML:=Fmain.CDSmarks.XMLData;
     ms.Write(XML[1],length(XML));
     ms.SaveToFile(GState.MarksFileName);
   except
     result:=false;
   end;
 finally
   ms.Free;
 end;
end;

function SaveCategory2File:boolean;
var ms:TMemoryStream;
    XML:string;
begin
 result:=true;
 ms:=TMemoryStream.Create;
 try
   try
     Fmain.CDSKategory.MergeChangeLog;
     XML:=Fmain.CDSKategory.XMLData;
     ms.Write(XML[1],length(XML));
     ms.SaveToFile(GState.MarksCategoryFileName);
   except
     result:=false;
   end;
 finally
   ms.Free;
 end;
end;

function Blob2ExtArr(Blobfield:Tfield):TExtendedPointArray;
var
  VSize: Integer;
  VPointsCount: Integer;
  VField: TBlobfield;
  VStream: TStream;
begin
  VField := TBlobfield(BlobField);
  VStream := VField.DataSet.CreateBlobStream(VField, bmRead);
  try
    VSize := VStream.Size;
    VPointsCount := VSize div SizeOf(TExtendedPoint);
    VSize := VPointsCount * SizeOf(TExtendedPoint);
    SetLength(result,VPointsCount);
    VStream.ReadBuffer(Result[0], VSize);
  finally
    VStream.Free;
  end;
end;
procedure BlobFromExtArr(AArr:TExtendedPointArray; Blobfield: Tfield);
var
  VField: TBlobfield;
  VStream: TStream;
  VPointsCount: Integer;
begin
  VField := TBlobfield(BlobField);
  VPointsCount := Length(AArr);
  VStream := VField.DataSet.CreateBlobStream(VField, bmWrite);
  try
    VStream.Write(AArr[0], VPointsCount * SizeOf(AArr[0]));
  finally
    VStream.Free;
  end;
end;

procedure BlobFromExtArr(APoint:TExtendedPoint; Blobfield: Tfield); overload;
var
  VField: TBlobfield;
  VStream: TStream;
begin
  VField := TBlobfield(BlobField);
  VStream := VField.DataSet.CreateBlobStream(VField, bmWrite);
  try
    VStream.Write(APoint, SizeOf(APoint));
  finally
    VStream.Free;
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
      WriteCurrentMark(VMark);
    end;
  finally
    VMark.Free;
  end;
end;

function SavePolyModal(AID: Integer; ANewArrLL: TExtendedPointArray): Boolean;
var
  VMark: TMarkFull;
begin
  VMark := TMarkFull.Create;
  try
    VMark.id := AID;
    if VMark.id >= 0 then begin
      Fmain.CDSmarks.Locate('id', VMark.id,[]);
      ReadCurrentMark(VMark);
    end;
    VMark.Points := Copy(ANewArrLL);
    Result := FaddPoly.EditMark(VMark);
    if Result then begin
      WriteMark(VMark);
    end;
  finally
    VMark.Free;
  end;
end;

function SaveLineModal(AID: Integer; ANewArrLL: TExtendedPointArray; ADescription: string): Boolean;
var
  VMark: TMarkFull;
begin
  VMark := TMarkFull.Create;
  try
    VMark.id := AID;
    if VMark.id >= 0 then begin
      Fmain.CDSmarks.Locate('id', VMark.id,[]);
      ReadCurrentMark(VMark);
    end else begin
      VMark.Desc := ADescription;
    end;
    VMark.Points := Copy(ANewArrLL);
    Result := FaddLine.EditMark(VMark);
    if Result then begin
      WriteMark(VMark);
    end;
  finally
    VMark.Free;
  end;
end;

function EditMarkModal(AMark: TMarkFull):boolean;
var
  VPointCount:integer;
begin
    VPointCount := Length(AMark.Points);
    Result := false;
    if VPointCount = 1 then begin
      result:=FaddPoint.EditMark(AMark);
    end else begin
      if (VPointCount>1) then begin
        if compare2EP(AMark.Points[0],AMark.Points[VPointCount-1]) then begin
          result:=FaddPoly.EditMark(AMark);
        end else begin
          result:=FaddLine.EditMark(AMark);
        end;
      end;
    end;
end;

function EditMarkF(id:integer;var arr:TExtendedPointArray):TAOperation;
var
  VPointCount:integer;
  VMark: TMarkFull;
begin
  VMark := TMarkFull.Create;
  try
    FMain.CDSmarks.Locate('id',id,[]);
    ReadCurrentMark(VMark);
    VPointCount := Length(VMark.Points);
    Result := ao_movemap;
    if VPointCount = 1 then begin
      result:=ao_edit_point;
      if FaddPoint.EditMark(VMark) then begin
        WriteMark(VMark);
      end;
      Result := ao_movemap;
    end else begin
      if (VPointCount>1) then begin
        if compare2EP(VMark.Points[0],VMark.Points[VPointCount-1]) then begin
          arr:=VMark.Points;
          result:=ao_edit_poly;
        end else begin
          arr:=VMark.Points;
          result:=ao_edit_line;
        end
      end;
    end;
  finally
    VMark.Free;
  end;
end;

procedure Kategory2StringsWithObjects(AStrings:TStrings);
var
  KategoryId:TCategoryId;
  i: Integer;
begin
  for i := 0 to AStrings.Count - 1 do begin
    AStrings.Objects[i].Free;
  end;
  AStrings.Clear;
  Fmain.CDSKategory.Filtered:=false;
  Fmain.CDSKategory.First;
  while not(Fmain.CDSKategory.Eof) do begin
    KategoryId:=TCategoryId.Create;
    ReadCurrentCategory(KategoryId);
    AStrings.AddObject(KategoryId.name, KategoryId);
    Fmain.CDSKategory.Next;
  end;
end;

function AddKategory(name:string): Integer;
var
  VCategory: TCategoryId;
begin
  VCategory := TCategoryId.Create;
  try
    VCategory.name := name;
    VCategory.visible := True;
    VCategory.AfterScale := 3;
    VCategory.BeforeScale := 19;

    Fmain.CDSKategory.Insert;
    WriteCurrentCategory(VCategory);
    Fmain.CDSKategory.post;
    Result := Fmain.CDSKategory.FieldByName('id').AsInteger;
    SaveCategory2File;
  finally
    VCategory.Free;
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
 SBNavOnMark.Down:= Fmain.LayerMapNavToMark.Visible;
end;

procedure TFMarksExplorer.KategoryListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var MarkId:TMarkId;
    i:integer;
    items:TStringList;
begin
 if KategoryListBox.ItemIndex<0 then exit;
 Fmain.CDSmarks.Filtered:=false;
 Fmain.CDSmarks.Filter:='categoryid = '+inttostr(TCategoryId(KategoryListBox.Items.Objects[KategoryListBox.ItemIndex]).id);
 Fmain.CDSmarks.Filtered:=true;
 Fmain.CDSmarks.First;
 for i:=1 to MarksListBox.items.Count do MarksListBox.Items.Objects[i-1].Free;
 MarksListBox.Clear;
 items:=TStringList.Create;
 while not(Fmain.CDSmarks.Eof) do begin
   MarkId:=TMarkId.Create;
   ReadCurrentMarkId(MarkId);
   items.AddObject(MarkId.name,MarkId);
   Fmain.CDSmarks.Next;
 end;
 MarksListBox.Items.Assign(items);
 FreeAndNil(items);
 for i:=0 to MarksListBox.Count-1 do
  MarksListBox.Checked[i]:=TMarkId(MarksListBox.Items.Objects[i]).visible;
end;

procedure TFMarksExplorer.Button2Click(Sender: TObject);
begin
 SaveMarks2File;
 if RBall.Checked then GState.show_point := mshAll;
 if RBchecked.Checked then GState.show_point := mshChecked;
 if RBnot.Checked then GState.show_point := mshNone;
 close;
end;

function DeleteMark(id:integer;handle:THandle):boolean;
begin
 result:=false;
 Fmain.CDSmarks.Locate('id',id,[]);
 if MessageBox(handle,pchar(SAS_MSG_youasure+' "'+Fmain.CDSmarks.fieldbyname('name').asString+'"'),pchar(SAS_MSG_coution),36)=IDNO
  then exit;
 Fmain.CDSmarks.Locate('id',id,[]);
 Fmain.CDSmarks.Delete;
 SaveMarks2File;
 result:=true;
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
var
  arLL:TExtendedPointArray;
  VPointCount: Integer;
begin
  Result:=false;
  arLL := AMark.Points;
  VPointCount := Length(arLL);
  if (VPointCount > 1)and(compare2EP(arLL[0],arLL[VPointCount-1])) then begin
    Fsaveas.Show_(GState.ViewState.GetCurrentZoom, arLL);
    Fmain.LayerSelection.Redraw;
    Result:=true;
  end else begin
    ShowMessage(SAS_MSG_FunExForPoly);
  end;
  arLL := nil;
end;

function GetGoToMarkLonLat(AMark: TMarkFull): TExtendedPoint;
var
  VPointCount:integer;
begin
  VPointCount := Length(AMark.Points);
  if VPointCount = 1 then begin
    Result := AMark.Points[0];
  end else begin
    if (AMark.Points[0].Y=AMark.Points[VPointCount-1].Y)and
    (AMark.Points[0].X=AMark.Points[VPointCount-1].X)
    then begin
      Result.X:= (AMark.LLRect.Left + AMark.LLRect.Right)/2 ;
      Result.Y:= (AMark.LLRect.Top + AMark.LLRect.Bottom)/2 ;
    end else begin
      Result := AMark.Points[0];
    end;
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
    if DeleteMark(VId,FMarksExplorer.Handle) then begin
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
      Fmain.topos(GetGoToMarkLonLat(VMark), GState.ViewState.GetCurrentZoom, True);
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
  i:integer;
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
    VMark := TMarkFull.Create;
    try
      FMain.CDSmarks.Locate('id',VMarkId.id,[]);
      ReadCurrentMark(VMark);
      if EditMarkModal(VMark) then begin
        WriteMark(VMark);
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

procedure TFMarksExplorer.Button1Click(Sender: TObject);
begin
 If (OpenDialog1.Execute) then
  if (FileExists(OpenDialog1.FileName)) then
   begin
    FImport.FileName:=OpenDialog1.FileName;
    FImport.ShowModal;
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
    FaddCategory.show_(VCategory);
    WriteCategory(VCategory);
    KategoryListBox.Items.Strings[VIndex] := VCategory.name;
    KategoryListBox.Checked[VIndex] := VCategory.visible;
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
      if DeleteMark(VMarkId.id, FMarksExplorer.Handle) then begin
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
var i:integer;
begin
 if KategoryListBox.Count>0 then begin
   for i:=0 to KategoryListBox.Count-1 do KategoryListBox.Checked[i]:=CheckBox2.Checked;
   Fmain.CDSKategory.First;
   while not(Fmain.CDSKategory.Eof) do
    begin
     Fmain.CDSKategory.Edit;
     Fmain.CDSKategory.FieldByName('visible').AsBoolean:=CheckBox2.Checked;
     Fmain.CDSKategory.Post;
     Fmain.CDSKategory.Next;
    end;
  SaveCategory2File;
 end;
end;

procedure TFMarksExplorer.CheckBox1Click(Sender: TObject);
var i:integer;
begin
 if MarksListBox.Count>0 then begin
   for i:=0 to MarksListBox.Count-1 do MarksListBox.Checked[i]:=CheckBox1.Checked;
   Fmain.CDSmarks.Filter:='categoryid = '+inttostr(TCategoryId(KategoryListBox.Items.Objects[KategoryListBox.ItemIndex]).id);
   Fmain.CDSmarks.Filtered:=true;
   Fmain.CDSmarks.First;
   while not(Fmain.CDSmarks.Eof) do begin
     Fmain.CDSmarks.Edit;
     Fmain.CDSmarks.FieldByName('visible').AsBoolean:=CheckBox1.Checked;
     Fmain.CDSmarks.Post;
     Fmain.CDSmarks.Next;
    end;
   Fmain.CDSmarks.Filtered:=False;
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

  if FaddCategory.show_(VCategory) then begin
    Fmain.CDSKategory.Insert;
    WriteCurrentCategory(VCategory);
    Fmain.CDSKategory.Post;
    SaveCategory2File;
    VCategory.id := Fmain.CDSKategory.fieldbyname('id').AsInteger;
    VIndex := KategoryListBox.Items.AddObject(VCategory.name, VCategory);
    KategoryListBox.Checked[VIndex]:=VCategory.visible;
  end else begin
    VCategory.Free;
  end;
end;

procedure TFMarksExplorer.SBNavOnMarkClick(Sender: TObject);
var
  LL:TExtendedPoint;
  arLL:TExtendedPointArray;
  VPointCount:integer;
  id:integer;
begin
 if (SBNavOnMark.Down) then
  if (MarksListBox.ItemIndex>=0) then
  begin
   id:=TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id;
   if not(Fmain.CDSmarks.Locate('id',id,[])) then exit;
   arLL := Blob2ExtArr(Fmain.CDSmarks.FieldByName('LonLatArr'));
   VPointCount := Length(arLL);
   if (arLL[0].Y=arLL[VPointCount-1].Y)and
      (arLL[0].X=arLL[VPointCount-1].X)
      then begin
            LL.X:=Fmain.CDSmarks.FieldByName('LonL').AsFloat+(Fmain.CDSmarks.FieldByName('LonR').AsFloat-Fmain.CDSmarks.FieldByName('LonL').AsFloat)/2;
            LL.Y:=Fmain.CDSmarks.FieldByName('LatB').AsFloat+(Fmain.CDSmarks.FieldByName('LatT').AsFloat-Fmain.CDSmarks.FieldByName('LatB').AsFloat)/2;
           end
      else begin
            LL:=arLL[0];
           end;
   FMain.LayerMapNavToMark.StartNav(LL, ID);
  end
  else SBNavOnMark.Down:=not SBNavOnMark.Down
 else FMain.LayerMapNavToMark.Visible := False;
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

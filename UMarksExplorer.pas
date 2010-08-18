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
 name:string;
 id:integer;
end;
TMarkId = class
 name:string;
 id:integer;
 visible:boolean;
end;

var
  FMarksExplorer: TFMarksExplorer;
  function DeleteMark(id:integer;handle:THandle):boolean;
  function OperationMark(id:integer):boolean;
  procedure AddKategory(name:string);
  procedure Kategory2Strings(strings:TStrings);
  function EditMark(id:integer):boolean;
  procedure GoToMark(id:integer;zoom:byte);
  function GetMarkLength(id:integer):extended;
  function GetMarkSq(id:integer):extended;
  function Blob2ExtArr(Blobfield:Tfield):TExtendedPointArray;
  function SaveMarks2File:boolean;
  function SaveCategory2File:boolean;
  function EditMarkF(id:integer; var arr:TExtendedPointArray):TAOperation;

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

function EditMark(id:integer):boolean;
var arrLL:PArrLL;
    arLL:array of TExtendedPoint;
    ms:TMemoryStream;
    i:integer;
begin
  FMain.CDSmarks.Locate('id',id,[]);
  ms:=TMemoryStream.Create;
  try
    TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
    GetMem(arrLL,ms.size);
    SetLength(arLL,ms.size div 24);
    ms.Position:=0;
    ms.ReadBuffer(arrLL^,ms.size);
    for i:=0 to length(arLL)-1 do arLL[i]:=arrLL^[i];
    Result := false;
    if ms.Size=24 then begin
      result:=FaddPoint.Show_(arLL[0],false);
    end else begin
      if (ms.Size>24) then begin
        if compare2EP(arLL[0],arLL[length(arLL)-1]) then begin
          result:=FaddPoly.show_(arLL,false);
        end else begin
          result:=FaddLine.show_(arLL,false, '');
        end
      end;                              
    end;

    freeMem(arrLL);
    SetLength(arLL,0);
  finally
    ms.Free;
  end;
end;

function EditMarkF(id:integer;var arr:TExtendedPointArray):TAOperation;
var arrLL:PArrLL;
    arLL:TExtendedPointArray;
    ms:TMemoryStream;
    i:integer;
begin
  FMain.CDSmarks.Locate('id',id,[]);
  ms:=TMemoryStream.Create;
  try
    TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
    GetMem(arrLL,ms.size);
    SetLength(arLL,ms.size div 24);
    ms.Position:=0;
    ms.ReadBuffer(arrLL^,ms.size);
    for i:=0 to length(arLL)-1 do arLL[i]:=arrLL^[i];
    Result := ao_movemap;
    if ms.Size=24 then begin
      result:=ao_edit_point;
      FaddPoint.Show_(arLL[0],false);
    end else begin
      if (ms.Size>24) then begin
        if compare2EP(arLL[0],arLL[length(arLL)-1]) then begin
          arr:=arLL;
          result:=ao_edit_poly;
        end else begin
          arr:=arLL;
          result:=ao_edit_line;
        end
      end;                              
    end;

    freeMem(arrLL);
    SetLength(arLL,0);
  finally
    ms.Free;
  end;
end;

procedure Kategory2Strings(strings:TStrings);
begin
 Fmain.CDSKategory.Filtered:=false;
 Fmain.CDSKategory.First;
 while not(Fmain.CDSKategory.Eof) do
  begin
   strings.Add(Fmain.CDSKategory.FieldByName('name').AsString);
   Fmain.CDSKategory.Next;
  end;
end;

procedure AddKategory(name:string);
begin
 Fmain.CDSKategory.Insert;
 Fmain.CDSKategory.FieldByName('name').AsString:=name;
 Fmain.CDSKategory.FieldByName('visible').AsBoolean:=true;
 Fmain.CDSKategory.FieldByName('AfterScale').AsInteger:=3;
 Fmain.CDSKategory.FieldByName('BeforeScale').AsInteger:=18;
 Fmain.CDSKategory.post;
 SaveCategory2File;
end;

procedure TFMarksExplorer.FormShow(Sender: TObject);
var KategoryId:TCategoryId;
    i:integer;
begin
 case GState.show_point of
  mshAll: RBall.Checked:=true;
  mshChecked: RBchecked.Checked:=true;
  mshNone: RBnot.Checked:=true;
 end;
 for i:=1 to MarksListBox.items.Count do MarksListBox.Items.Objects[i-1].Free;
 MarksListBox.Clear;
 for i:=1 to KategoryListBox.items.Count do KategoryListBox.Items.Objects[i-1].Free;
 KategoryListBox.Clear;
 Fmain.CDSKategory.Filtered:=false;
 Fmain.CDSKategory.First;
 while not(Fmain.CDSKategory.Eof) do
  begin
   KategoryId:=TCategoryId.Create;
   KategoryId.name:=Fmain.CDSKategory.fieldbyname('name').AsString;
   KategoryId.id:=Fmain.CDSKategory.fieldbyname('id').AsInteger;
   KategoryListBox.AddItem(Fmain.CDSKategory.fieldbyname('name').AsString,KategoryId);
   KategoryListBox.Checked[KategoryListBox.Items.IndexOfObject(KategoryId)]:=Fmain.CDSKategory.fieldbyname('visible').AsBoolean;
   Fmain.CDSKategory.Next;
  end;
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
 while not(Fmain.CDSmarks.Eof) do
  begin
   MarkId:=TMarkId.Create;
   MarkId.name:=Fmain.CDSmarks.fieldbyname('name').AsString;
   MarkId.id:=Fmain.CDSmarks.fieldbyname('id').AsInteger;
   MarkId.visible:=Fmain.CDSmarks.fieldbyname('visible').AsBoolean;
   items.AddObject(Fmain.CDSmarks.fieldbyname('name').AsString,MarkId);
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

function GetMarkLength(id:integer):extended;
var arrLL:PArrLL;
    arLL:array of TExtendedPoint;
    ms:TMemoryStream;
    i:integer;
    VConverter: ICoordConverter;
begin
 Result:=0;
 VConverter := GState.ViewState.GetCurrentCoordConverter;
 Fmain.CDSmarks.Locate('id',id,[]);
 ms:=TMemoryStream.Create;
 TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
 GetMem(arrLL,ms.size);
 SetLength(arLL,ms.size div 24);
 ms.Position:=0;
 ms.ReadBuffer(arrLL^,ms.size);
 for i:=0 to length(arLL)-1 do arLL[i]:=arrLL^[i];
 if (ms.Size>24)
     then begin
           for i:=0 to length(arLL)-2 do
            result:=result+ VConverter.CalcDist(arLL[i],arLL[i+1]);
          end;
 freeMem(arrLL);
 SetLength(arLL,0);
 ms.Free;
end;

function GetMarkSq(id:integer):extended;
var arrLL:PArrLL;
    arLL: TExtendedPointArray;
    ms:TMemoryStream;
    i:integer;
    VConverter: ICoordConverter;
begin
 Result:=0;
 VConverter := GState.ViewState.GetCurrentCoordConverter;
 Fmain.CDSmarks.Locate('id',id,[]);
 ms:=TMemoryStream.Create;
 TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
 GetMem(arrLL,ms.size);
 SetLength(arLL,ms.size div 24);
 ms.Position:=0;
 ms.ReadBuffer(arrLL^,ms.size);
 for i:=0 to length(arLL)-1 do arLL[i]:=arrLL^[i];
 if (ms.Size>24)
     then begin
           result:= VConverter.CalcPoligonArea(arLL);
          end;
 freeMem(arrLL);
 SetLength(arLL,0);
 ms.Free;
end;

function OperationMark(id:integer):boolean;
var arrLL:PArrLL;
    arLL: TExtendedPointArray;
    ms:TMemoryStream;
    i:integer;
begin
 Result:=false;
 Fmain.CDSmarks.Locate('id',id,[]);
 ms:=TMemoryStream.Create;
 TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
 GetMem(arrLL,ms.size);
 SetLength(arLL,ms.size div 24);
 ms.Position:=0;
 ms.ReadBuffer(arrLL^,ms.size);
 for i:=0 to length(arLL)-1 do arLL[i]:=arrLL^[i];
 if (ms.Size>24)and(compare2EP(arLL[0],arLL[length(arLL)-1]))
     then begin
           Fsaveas.Show_(GState.ViewState.GetCurrentZoom, arLL);
           Fmain.LayerSelection.Redraw;
           Result:=true;
          end
     else ShowMessage(SAS_MSG_FunExForPoly);
 freeMem(arrLL);
 SetLength(arLL,0);
 ms.Free;
end;

procedure GoToMark(id:integer;zoom:byte);
var ms:TMemoryStream;
    LL:TExtendedPoint;
    arrLL:PArrLL;
begin
   if not(Fmain.CDSmarks.Locate('id',id,[])) then exit;

   ms:=TMemoryStream.Create;
   TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
   ms.Position:=0;
   GetMem(arrLL,ms.size);
   ms.ReadBuffer(arrLL^,ms.size);
   if (arrLL^[0].Y=arrLL^[ms.size div 24-1].Y)and
      (arrLL^[0].X=arrLL^[ms.size div 24-1].X)
      then begin
            LL.X:=Fmain.CDSmarks.FieldByName('LonL').AsFloat+(Fmain.CDSmarks.FieldByName('LonR').AsFloat-Fmain.CDSmarks.FieldByName('LonL').AsFloat)/2;
            LL.Y:=Fmain.CDSmarks.FieldByName('LatB').AsFloat+(Fmain.CDSmarks.FieldByName('LatT').AsFloat-Fmain.CDSmarks.FieldByName('LatB').AsFloat)/2;
           end
      else begin
            LL:=arrLL^[0];
           end;
   ms.Free;
   FreeMem(arrLL);
   Fmain.toPos(LL,zoom - 1,true);
end;

procedure TFMarksExplorer.BtnDelMarkClick(Sender: TObject);
begin
 if MarksListBox.ItemIndex>=0 then
  begin
   if DeleteMark(TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id,FMarksExplorer.Handle)
    then begin
          MarksListBox.Items.Objects[MarksListBox.ItemIndex].Free;
          MarksListBox.DeleteSelected;
         end;
  end;
end;

procedure TFMarksExplorer.MarksListBoxClickCheck(Sender: TObject);
begin
 Fmain.CDSmarks.Locate('id',TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id,[]);
 Fmain.CDSmarks.Edit;
 Fmain.CDSmarks.FieldByName('visible').AsBoolean:=MarksListBox.Checked[MarksListBox.ItemIndex];
 Fmain.CDSmarks.Post;
end;

procedure TFMarksExplorer.BtnOpMarkClick(Sender: TObject);
begin
 if MarksListBox.ItemIndex>=0 then
  begin
   if OperationMark(TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id)
      then close;
  end;
end;

procedure TFMarksExplorer.BtnGotoMarkClick(Sender: TObject);
begin
 if MarksListBox.ItemIndex>=0 then
  begin
   GoToMark(TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id,GState.ViewState.GetCurrentZoom + 1);
  end;
end;

procedure TFMarksExplorer.KategoryListBoxClickCheck(Sender: TObject);
begin
 Fmain.CDSKategory.Locate('id',TCategoryId(KategoryListBox.Items.Objects[KategoryListBox.ItemIndex]).id,[]);
 Fmain.CDSKategory.Edit;
 Fmain.CDSKategory.FieldByName('visible').AsBoolean:=KategoryListBox.Checked[KategoryListBox.ItemIndex];
 Fmain.CDSKategory.post;
 SaveCategory2File;
end;

procedure TFMarksExplorer.BtnDelKatClick(Sender: TObject);
var i:integer;
begin
 if MessageBox(FMarksExplorer.handle,pchar(SAS_MSG_youasure),pchar(SAS_MSG_coution),36)=IDNO
  then exit;
 Fmain.CDSKategory.Locate('id',TCategoryId(KategoryListBox.Items.Objects[KategoryListBox.ItemIndex]).id,[]);
 FMain.CDSmarks.Filtered:=false;
 Fmain.CDSmarks.Filter:='categoryid = '+inttostr(TCategoryId(KategoryListBox.Items.Objects[KategoryListBox.ItemIndex]).id);
 Fmain.CDSmarks.Filtered:=true;
 Fmain.CDSmarks.First;
 while not(Fmain.CDSmarks.Eof) do
   Fmain.CDSmarks.Delete;
 Fmain.CDSKategory.Delete;
 SaveCategory2File;
 KategoryListBox.Items.Objects[KategoryListBox.ItemIndex].Free;
 KategoryListBox.DeleteSelected;
 for i:=1 to MarksListBox.items.Count do MarksListBox.Items.Objects[i-1].Free;
 MarksListBox.Clear;
end;

procedure TFMarksExplorer.SpeedButton1Click(Sender: TObject);
begin
 if MarksListBox.ItemIndex>=0 then
  begin
   if EditMark(TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id) then
    begin
     FMain.CDSmarks.Locate('id',TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id,[]);
     MarksListBox.Items.Strings[MarksListBox.ItemIndex]:=Fmain.CDSmarks.fieldbyname('name').asString;
     MarksListBox.Checked[MarksListBox.ItemIndex]:=Fmain.CDSmarks.fieldbyname('visible').AsBoolean;
     if Fmain.CDSmarks.fieldbyname('categoryid').AsInteger<>TCategoryId(KategoryListBox.Items.Objects[KategoryListBox.ItemIndex]).id then
       begin
          MarksListBox.Items.Objects[MarksListBox.ItemIndex].Free;
          MarksListBox.DeleteSelected;
       end;
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
    FMarksExplorer.FormShow(sender);
   end;
end;

procedure TFMarksExplorer.BtnEditCategoryClick(Sender: TObject);
begin
 if KategoryListBox.ItemIndex>=0 then
  begin
   Fmain.CDSKategory.Locate('id',TCategoryId(KategoryListBox.Items.Objects[KategoryListBox.ItemIndex]).id,[]);
   FaddCategory.show_(false);
   FaddCategory.ShowModal;
   KategoryListBox.Items.Strings[KategoryListBox.ItemIndex]:=Fmain.CDSKategory.fieldbyname('name').asString;
  end;
end;

procedure TFMarksExplorer.MarksListBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 If key=VK_DELETE then
   if MarksListBox.ItemIndex>=0 then
     begin
      if DeleteMark(TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id,FMarksExplorer.Handle)
        then MarksListBox.DeleteSelected;
     end;
end;

procedure TFMarksExplorer.KategoryListBoxKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
 If key=VK_DELETE then
   if MarksListBox.ItemIndex>=0 then
 begin
 if MessageBox(FMarksExplorer.handle,pchar(SAS_MSG_youasure),pchar(SAS_MSG_coution),36)=IDNO
  then exit;
 Fmain.CDSKategory.Locate('id',TCategoryId(KategoryListBox.Items.Objects[KategoryListBox.ItemIndex]).id,[]);
 FMain.CDSmarks.Filtered:=false;
 Fmain.CDSmarks.Filter:='categoryid = '+inttostr(TCategoryId(KategoryListBox.Items.Objects[KategoryListBox.ItemIndex]).id);
 Fmain.CDSmarks.Filtered:=true;
 Fmain.CDSmarks.First;
 while not(Fmain.CDSmarks.Eof) do
   Fmain.CDSmarks.Delete;
 Fmain.CDSmarks.Post;
 Fmain.CDSKategory.Delete;
 SaveCategory2File;
 KategoryListBox.DeleteSelected;
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
var KategoryId:TCategoryId;
begin
 FaddCategory.show_(true);
 FaddCategory.ShowModal;
 if FaddCategory.ModalResult=mrOk then
  begin
   KategoryId:=TCategoryId.Create;
   KategoryId.name:=Fmain.CDSKategory.fieldbyname('name').AsString;
   KategoryId.id:=Fmain.CDSKategory.fieldbyname('id').AsInteger;
   KategoryListBox.AddItem(Fmain.CDSKategory.fieldbyname('name').AsString,KategoryId);
   KategoryListBox.Checked[KategoryListBox.Items.IndexOfObject(KategoryId)]:=Fmain.CDSKategory.fieldbyname('visible').AsBoolean;
  end;
end;

procedure TFMarksExplorer.SBNavOnMarkClick(Sender: TObject);
var ms:TMemoryStream;
    LL:TExtendedPoint;
    arrLL:PArrLL;
    id:integer;
begin
 if (SBNavOnMark.Down) then
  if (MarksListBox.ItemIndex>=0) then
  begin
   id:=TMarkId(MarksListBox.Items.Objects[MarksListBox.ItemIndex]).id;
   if not(Fmain.CDSmarks.Locate('id',id,[])) then exit;
   ms:=TMemoryStream.Create;
   TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
   ms.Position:=0;
   GetMem(arrLL,ms.size);
   ms.ReadBuffer(arrLL^,ms.size);
   if (arrLL^[0].Y=arrLL^[ms.size div 24-1].Y)and
      (arrLL^[0].X=arrLL^[ms.size div 24-1].X)
      then begin
            LL.X:=Fmain.CDSmarks.FieldByName('LonL').AsFloat+(Fmain.CDSmarks.FieldByName('LonR').AsFloat-Fmain.CDSmarks.FieldByName('LonL').AsFloat)/2;
            LL.Y:=Fmain.CDSmarks.FieldByName('LatB').AsFloat+(Fmain.CDSmarks.FieldByName('LatT').AsFloat-Fmain.CDSmarks.FieldByName('LatB').AsFloat)/2;
           end
      else begin
            LL:=arrLL^[0];
           end;
   ms.Free;
   FreeMem(arrLL);
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

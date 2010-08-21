unit u_MarksReadWriteSimple;

interface

uses
  Windows,
  Classes,
  t_GeoTypes,
  u_MarksSimple;

type
  TMarksIteratorVisibleInRect = class(TMarksIteratorBase)
  private
    FFinished: Boolean;
  protected
    function GetFilterText(AZoom: Byte; ARect: TExtendedRect): string; virtual;
    procedure FinishIterate;
  public
    constructor Create(AZoom: Byte; ARect: TExtendedRect);
    destructor Destroy; override;
    function Next: Boolean; override;
  end;

  TMarksIteratorVisibleInRectIgnoreEdit = class(TMarksIteratorVisibleInRect)
  protected
    function GetFilterText(AZoom: Byte; ARect: TExtendedRect): string; override;
  end;

  function GetMarkByID(id:integer): TMarkFull;
  function GetMarkIdByID(id:integer): TMarkId;
  function DeleteMark(AMarkId: TMarkId): Boolean;
  procedure DeleteCategoryWithMarks(ACategory: TCategoryId);
  procedure WriteCategory(ACategory: TCategoryId);
  procedure WriteCategoriesList(AStrings:TStrings);
  procedure WriteMark(AMark: TMarkFull);
  procedure WriteMarkId(AMark: TMarkId);
  procedure WriteMarkIdList(AStrings:TStrings);
  procedure Marsk2StringsWithMarkId(ACategoryId: TCategoryId; AStrings:TStrings);
  procedure Kategory2StringsWithObjects(AStrings:TStrings);
  procedure AllMarsk2StringsWhitMarkId(AStrings:TStrings);
  function GetMarksFileterByCategories(AZoom: Byte): string;
  procedure LoadMarksFromFile;
  procedure LoadCategoriesFromFile;
  function SaveMarks2File:boolean;
  function SaveCategory2File:boolean;


implementation

uses
  DB,
  SysUtils,
  GR32,
  u_GlobalState,
  t_CommonTypes,
  Unit1;

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
  if Fmain.CDSKategory.Locate('id',ACategory.id,[]) then begin
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

function GetMarkIdByID(id:integer): TMarkId;
begin
  Result := nil;
  if FMain.CDSmarks.Locate('id',id,[]) then begin
    Result := TMarkId.Create;
    ReadCurrentMarkId(Result);
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

function DeleteMark(AMarkId: TMarkId): Boolean;
begin
  result:=false;
  if Fmain.CDSmarks.Locate('id',AMarkId.id,[]) then begin
    Fmain.CDSmarks.Delete;
    SaveMarks2File;
    result:=true;
  end;
end;

procedure WriteCategoriesList(AStrings:TStrings);
var
  VCategoryId: TCategoryId;
  i: Integer;
begin
  for i := 0 to AStrings.Count - 1 do begin
    VCategoryId := TCategoryId(AStrings.Objects[i]);
    WriteCategory(VCategoryId);
  end;
  SaveCategory2File;
end;

procedure Marsk2StringsWithMarkId(ACategoryId: TCategoryId; AStrings:TStrings);
var
  i: Integer;
  VMarkId: TMarkId;
begin
  for i := 0 to AStrings.Count - 1 do begin
    AStrings.Objects[i].Free;
  end;
  AStrings.Clear;
  Fmain.CDSmarks.Filtered:=false;
  Fmain.CDSmarks.Filter:='categoryid = '+inttostr(ACategoryId.id);
  Fmain.CDSmarks.Filtered:=true;
  Fmain.CDSmarks.First;
  while not(Fmain.CDSmarks.Eof) do begin
    VMarkId:=TMarkId.Create;
    ReadCurrentMarkId(VMarkId);
    AStrings.AddObject(VMarkId.name,VMarkId);
    Fmain.CDSmarks.Next;
  end;
end;

procedure AllMarsk2StringsWhitMarkId(AStrings:TStrings);
var
  i: Integer;
  VMarkId: TMarkId;
begin
  for i := 0 to AStrings.Count - 1 do begin
    AStrings.Objects[i].Free;
  end;
  AStrings.Clear;
  Fmain.CDSmarks.Filtered:=false;
  Fmain.CDSmarks.First;
  while not(Fmain.CDSmarks.Eof) do begin
    VMarkId:=TMarkId.Create;
    ReadCurrentMarkId(VMarkId);
    AStrings.AddObject(VMarkId.name,VMarkId);
    Fmain.CDSmarks.Next;
  end;
end;

procedure WriteMarkIdList(AStrings:TStrings);
var
  VMarkId: TMarkId;
  i: Integer;
begin
  for i := 0 to AStrings.Count - 1 do begin
    VMarkId := TMarkId(AStrings.Objects[i]);
    WriteMarkId(VMarkId);
  end;
  SaveCategory2File;
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

procedure LoadMarksFromFile;
begin
  if FileExists(GState.MarksFileName) then begin
    Fmain.CDSMarks.LoadFromFile(GState.MarksFileName);
    if Fmain.CDSMarks.RecordCount>0 then begin
      CopyFile(PChar(GState.MarksFileName),PChar(GState.MarksBackUpFileName),false);
    end;
  end;
end;

procedure LoadCategoriesFromFile;
begin
  if FileExists(GState.MarksCategoryFileName) then begin
    Fmain.CDSKategory.LoadFromFile(GState.MarksCategoryFileName);
    if Fmain.CDSKategory.RecordCount>0 then begin
      CopyFile(PChar(GState.MarksCategoryFileName),PChar(GState.MarksCategoryBackUpFileName),false);
    end;
  end;
end;

{ TMarksIteratorVisibleInRect }

constructor TMarksIteratorVisibleInRect.Create(AZoom: Byte;
  ARect: TExtendedRect);
begin
  inherited Create;
  FMain.CDSmarks.Filter:=GetFilterText(AZoom, ARect);
  FMain.CDSmarks.Filtered:=true;
  FMain.CDSmarks.First;
  FFinished := False;
  if FMain.CDSmarks.Eof then begin
    FinishIterate;
  end;
end;

destructor TMarksIteratorVisibleInRect.Destroy;
begin
  if not FFinished then begin
    FinishIterate;
  end;
  inherited;
end;

procedure TMarksIteratorVisibleInRect.FinishIterate;
begin
  FFinished := True;
  FMain.CDSmarks.Filtered:=false;
end;

function TMarksIteratorVisibleInRect.GetFilterText(AZoom: Byte;
  ARect: TExtendedRect): string;
var
  VCategoryFilter: string;
begin
  Result := '';
  if GState.show_point = mshChecked then begin
    Result:=Result+'visible=1';
    Result:=Result+' and ';
    VCategoryFilter := GetMarksFileterByCategories(AZoom);
    if Length(VCategoryFilter) > 0 then begin
      Result:=Result + VCategoryFilter + ' and ';
    end;
  end;
  Result:=Result+'('+
    ' LonR>'+floattostr(ARect.Left)+' and'+
    ' LonL<'+floattostr(ARect.Right)+' and'+
    ' LatB<'+floattostr(ARect.Top)+' and'+
    ' LatT>'+floattostr(ARect.Bottom)+
  ')';
end;

function TMarksIteratorVisibleInRect.Next: Boolean;
begin
  if not FFinished then begin
    ReadCurrentMark(FCurrentMark);
    FMain.CDSmarks.Next;
    if FMain.CDSmarks.Eof then begin
      FinishIterate;
    end;
    Result := True;
  end else begin
    Result := False;
  end;
end;

{ TMarksIteratorVisibleInRectIgnoreEdit }

function TMarksIteratorVisibleInRectIgnoreEdit.GetFilterText(AZoom: Byte;
  ARect: TExtendedRect): string;
begin
  if (Fmain.aoper=ao_edit_line)or(Fmain.aoper=ao_edit_poly) then begin
    Result:='id<>'+inttostr(Fmain.EditMarkId)+' and ';
  end else begin
    Result:='';
  end;
  Result := Result + inherited GetFilterText(AZoom, ARect);
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

end.

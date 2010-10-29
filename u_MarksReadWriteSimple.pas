unit u_MarksReadWriteSimple;

interface

uses
  Windows,
  Classes,
  t_GeoTypes,
  t_CommonTypes,
  u_MarksSimple;

type
  TMarksDB = class
  private
    procedure ReadCurrentMark(AMark: TMarkFull);
    procedure ReadCurrentMarkId(AMark: TMarkId);
    procedure WriteCurrentMarkId(AMark: TMarkId);
    procedure WriteCurrentMark(AMark: TMarkFull);
    procedure ReadCurrentCategory(ACategory: TCategoryId);
    procedure WriteCurrentCategory(ACategory: TCategoryId);
    function GetMarksFileterByCategories(AZoom: Byte; AShowType: TMarksShowType): string;
  public
    function GetMarkByID(id: integer): TMarkFull;
    function GetMarkIdByID(id: integer): TMarkId;
    function DeleteMark(AMarkId: TMarkId): Boolean;
    procedure DeleteCategoryWithMarks(ACategory: TCategoryId);
    procedure WriteCategory(ACategory: TCategoryId);
    procedure WriteCategoriesList(AStrings: TStrings);
    procedure WriteMark(AMark: TMarkFull);
    procedure WriteMarkId(AMark: TMarkId);
    procedure WriteMarkIdList(AStrings: TStrings);
    procedure Marsk2StringsWithMarkId(ACategoryId: TCategoryId; AStrings: TStrings);
    procedure Kategory2StringsWithObjects(AStrings: TStrings);
    procedure AllMarsk2StringsWhitMarkId(AStrings: TStrings);
    procedure LoadMarksFromFile;
    procedure LoadCategoriesFromFile;
    function SaveMarks2File: boolean;
    function SaveCategory2File: boolean;
    function GetMarksIterator(AZoom: Byte; ARect: TExtendedRect; AShowType: TMarksShowType): TMarksIteratorBase;
    function GetMarksIteratorWithIgnore(AZoom: Byte; ARect: TExtendedRect; AShowType: TMarksShowType; AIgnoredID: Integer): TMarksIteratorBase;
  end;


implementation

uses
  DB,
  SysUtils,
  GR32,
  u_GlobalState,
  dm_MarksDb;

type
  TMarksIteratorVisibleInRect = class(TMarksIteratorBase)
  private
    FMarksDb: TMarksDB;
    FFinished: Boolean;
    FShowType: TMarksShowType;
  protected
    function GetFilterText(AZoom: Byte; ARect: TExtendedRect): string; virtual;
    procedure FinishIterate;
  public
    constructor Create(AMarksDb: TMarksDB; AZoom: Byte; ARect: TExtendedRect; AShowType: TMarksShowType);
    destructor Destroy; override;
    function Next: Boolean; override;
  end;

  TMarksIteratorVisibleInRectWithIgnore = class(TMarksIteratorVisibleInRect)
  private
    FIgnoredID: Integer;
  protected
    function GetFilterText(AZoom: Byte; ARect: TExtendedRect): string; override;
  public
    constructor Create(AMarksDb: TMarksDB; AZoom: Byte; ARect: TExtendedRect; AShowType: TMarksShowType; AIgnoredID: Integer);
  end;

{ TMarksIteratorVisibleInRect }

constructor TMarksIteratorVisibleInRect.Create(AMarksDb: TMarksDB; AZoom: Byte;
  ARect: TExtendedRect; AShowType: TMarksShowType);
begin
  inherited Create;
  FMarksDb := AMarksDb;
  FShowType := AShowType;
  DMMarksDb.CDSmarks.DisableControls;
  DMMarksDb.CDSmarks.Filter := GetFilterText(AZoom, ARect);
  DMMarksDb.CDSmarks.Filtered := true;
  DMMarksDb.CDSmarks.First;
  FFinished := False;
  if DMMarksDb.CDSmarks.Eof then begin
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
  DMMarksDb.CDSmarks.Filtered := false;
  DMMarksDb.CDSmarks.EnableControls;
end;

function TMarksIteratorVisibleInRect.GetFilterText(AZoom: Byte;
  ARect: TExtendedRect): string;
var
  VCategoryFilter: string;
begin
  Result := '';
  if FShowType = mshChecked then begin
    Result := Result + 'visible=1';
    Result := Result + ' and ';
    VCategoryFilter := FMarksDb.GetMarksFileterByCategories(AZoom, FShowType);
    if Length(VCategoryFilter) > 0 then begin
      Result := Result + VCategoryFilter + ' and ';
    end;
  end;
  Result := Result + '(' +
    ' LonR>' + floattostr(ARect.Left) + ' and' +
    ' LonL<' + floattostr(ARect.Right) + ' and' +
    ' LatB<' + floattostr(ARect.Top) + ' and' +
    ' LatT>' + floattostr(ARect.Bottom) +
    ')';
end;

function TMarksIteratorVisibleInRect.Next: Boolean;
begin
  if not FFinished then begin
    FMarksDb.ReadCurrentMark(FCurrentMark);
    DMMarksDb.CDSmarks.Next;
    if DMMarksDb.CDSmarks.Eof then begin
      FinishIterate;
    end;
    Result := True;
  end else begin
    Result := False;
  end;
end;

{ TMarksIteratorVisibleInRectWithIgnore }

constructor TMarksIteratorVisibleInRectWithIgnore.Create(AMarksDb: TMarksDB; AZoom: Byte;
  ARect: TExtendedRect; AShowType: TMarksShowType; AIgnoredID: Integer);
begin
  inherited Create(AMarksDb, AZoom, ARect, AShowType);
  FIgnoredID := AIgnoredID;
end;

function TMarksIteratorVisibleInRectWithIgnore.GetFilterText(AZoom: Byte;
  ARect: TExtendedRect): string;
begin
  Result := '';
  if FIgnoredID >= 0 then begin
    Result := 'id<>' + inttostr(FIgnoredID) + ' and ';
  end;
  Result := Result + inherited GetFilterText(AZoom, ARect);
end;


procedure Blob2ExtArr(Blobfield: Tfield; var APoints: TExtendedPointArray);
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
    SetLength(APoints, VPointsCount);
    VStream.ReadBuffer(APoints[0], VSize);
  finally
    VStream.Free;
  end;
end;

procedure BlobFromExtArr(AArr: TExtendedPointArray; Blobfield: Tfield);
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

procedure TMarksDB.ReadCurrentCategory(ACategory: TCategoryId);
begin
  ACategory.name := DMMarksDb.CDSKategory.fieldbyname('name').AsString;
  ACategory.id := DMMarksDb.CDSKategory.fieldbyname('id').AsInteger;
  ACategory.visible := DMMarksDb.CDSKategory.FieldByName('visible').AsBoolean;
  ACategory.AfterScale := DMMarksDb.CDSKategory.fieldbyname('AfterScale').AsInteger;
  ACategory.BeforeScale := DMMarksDb.CDSKategory.fieldbyname('BeforeScale').AsInteger;
end;

procedure TMarksDB.WriteCurrentCategory(ACategory: TCategoryId);
begin
  DMMarksDb.CDSKategory.fieldbyname('name').AsString := ACategory.name;
  DMMarksDb.CDSKategory.FieldByName('visible').AsBoolean := ACategory.visible;
  DMMarksDb.CDSKategory.fieldbyname('AfterScale').AsInteger := ACategory.AfterScale;
  DMMarksDb.CDSKategory.fieldbyname('BeforeScale').AsInteger := ACategory.BeforeScale;
end;

procedure TMarksDB.WriteCategory(ACategory: TCategoryId);
begin
  if ACategory.id < 0 then begin
    DMMarksDb.CDSKategory.Insert;
  end else begin
    DMMarksDb.CDSKategory.Locate('id', ACategory.id, []);
    DMMarksDb.CDSKategory.Edit;
  end;
  WriteCurrentCategory(ACategory);
  DMMarksDb.CDSKategory.post;
  ACategory.id := DMMarksDb.CDSKategory.fieldbyname('id').AsInteger;
  SaveCategory2File;
end;

procedure TMarksDB.DeleteCategoryWithMarks(ACategory: TCategoryId);
begin
  if DMMarksDb.CDSKategory.Locate('id', ACategory.id, []) then begin
    DMMarksDb.CDSmarks.Filtered := false;
    DMMarksDb.CDSmarks.Filter := 'categoryid = ' + inttostr(ACategory.id);
    DMMarksDb.CDSmarks.Filtered := true;
    DMMarksDb.CDSmarks.First;
    while not (DMMarksDb.CDSmarks.Eof) do begin
      DMMarksDb.CDSmarks.Delete;
    end;
    if DMMarksDb.CDSKategory.Locate('id', ACategory.id, []) then begin
      DMMarksDb.CDSKategory.Delete;
    end;
    SaveCategory2File;
  end;
end;


function TMarksDB.GetMarksFileterByCategories(AZoom: Byte; AShowType: TMarksShowType): string;
begin
  Result := '';
  if AShowType = mshChecked then begin
    DMMarksDb.CDSKategory.DisableControls;
    try
      DMMarksDb.CDSKategory.Filter := 'visible = 1 and ( AfterScale <= ' + inttostr(AZoom + 1) + ' and BeforeScale >= ' + inttostr(AZoom + 1) + ' )';
      DMMarksDb.CDSKategory.Filtered := true;
      try
        DMMarksDb.CDSKategory.First;
        if not (DMMarksDb.CDSKategory.Eof) then begin
          Result := '(';
          while not (DMMarksDb.CDSKategory.Eof) do begin
            Result := Result + 'categoryid=' + DMMarksDb.CDSKategory.fieldbyname('id').AsString;
            DMMarksDb.CDSKategory.Next;
            if not (DMMarksDb.CDSKategory.Eof) then begin
              Result := Result + ' or ';
            end;
          end;
          Result := Result + ')';
        end else begin
          Result := '(categoryid=-1)';
        end;
      finally
        DMMarksDb.CDSKategory.Filtered := false;
      end;
    finally
      DMMarksDb.CDSKategory.EnableControls;
    end;
  end;
end;

procedure TMarksDB.ReadCurrentMarkId(AMark: TMarkId);
begin
  AMark.id := DMMarksDb.CDSmarks.fieldbyname('id').AsInteger;
  AMark.name := DMMarksDb.CDSmarks.FieldByName('name').AsString;
  AMark.visible := DMMarksDb.CDSmarks.FieldByName('Visible').AsBoolean;
end;

procedure TMarksDB.ReadCurrentMark(AMark: TMarkFull);
begin
  ReadCurrentMarkId(AMark);
  Blob2ExtArr(DMMarksDb.CDSmarks.FieldByName('LonLatArr'), AMark.Points);
  AMark.CategoryId := DMMarksDb.CDSmarkscategoryid.AsInteger;
  AMark.Desc := DMMarksDb.CDSmarks.FieldByName('descr').AsString;
  AMark.LLRect.Left := DMMarksDb.CDSmarks.FieldByName('LonL').AsFloat;
  AMark.LLRect.Top := DMMarksDb.CDSmarks.FieldByName('LatT').AsFloat;
  AMark.LLRect.Right := DMMarksDb.CDSmarks.FieldByName('LonR').AsFloat;
  AMark.LLRect.Bottom := DMMarksDb.CDSmarks.FieldByName('LatB').AsFloat;
  AMark.PicName := DMMarksDb.CDSmarks.FieldByName('PicName').AsString;
  AMark.Color1 := TColor32(DMMarksDb.CDSmarks.FieldByName('Color1').AsInteger);
  AMark.Color2 := TColor32(DMMarksDb.CDSmarks.FieldByName('Color2').AsInteger);
  AMark.Scale1 := DMMarksDb.CDSmarks.FieldByName('Scale1').AsInteger;
  AMark.Scale2 := DMMarksDb.CDSmarks.FieldByName('Scale2').AsInteger;
end;

procedure TMarksDB.WriteCurrentMarkId(AMark: TMarkId);
begin
  DMMarksDb.CDSmarks.FieldByName('name').AsString := AMark.name;
  DMMarksDb.CDSmarks.FieldByName('Visible').AsBoolean := AMark.visible;
end;

procedure TMarksDB.WriteCurrentMark(AMark: TMarkFull);
begin
  WriteCurrentMarkId(AMark);
  BlobFromExtArr(AMark.Points, DMMarksDb.CDSmarks.FieldByName('LonLatArr'));
  DMMarksDb.CDSmarkscategoryid.AsInteger := AMark.CategoryId;
  DMMarksDb.CDSmarks.FieldByName('descr').AsString := AMark.Desc;
  DMMarksDb.CDSmarks.FieldByName('LonL').AsFloat := AMark.LLRect.Left;
  DMMarksDb.CDSmarks.FieldByName('LatT').AsFloat := AMark.LLRect.Top;
  DMMarksDb.CDSmarks.FieldByName('LonR').AsFloat := AMark.LLRect.Right;
  DMMarksDb.CDSmarks.FieldByName('LatB').AsFloat := AMark.LLRect.Bottom;
  DMMarksDb.CDSmarks.FieldByName('PicName').AsString := AMark.PicName;
  DMMarksDb.CDSmarks.FieldByName('Color1').AsInteger := AMark.Color1;
  DMMarksDb.CDSmarks.FieldByName('Color2').AsInteger := AMark.Color2;
  DMMarksDb.CDSmarks.FieldByName('Scale1').AsInteger := AMark.Scale1;
  DMMarksDb.CDSmarks.FieldByName('Scale2').AsInteger := AMark.Scale2;
end;

function TMarksDB.GetMarkByID(id: integer): TMarkFull;
begin
  Result := nil;
  if DMMarksDb.CDSmarks.Locate('id', id, []) then begin
    Result := TMarkFull.Create;
    ReadCurrentMark(Result);
  end;
end;

function TMarksDB.GetMarkIdByID(id: integer): TMarkId;
begin
  Result := nil;
  if DMMarksDb.CDSmarks.Locate('id', id, []) then begin
    Result := TMarkId.Create;
    ReadCurrentMarkId(Result);
  end;
end;

function TMarksDB.GetMarksIterator(AZoom: Byte; ARect: TExtendedRect;
  AShowType: TMarksShowType): TMarksIteratorBase;
begin
  Result := TMarksIteratorVisibleInRect.Create(Self, AZoom, ARect, AShowType);
end;

function TMarksDB.GetMarksIteratorWithIgnore(AZoom: Byte; ARect: TExtendedRect;
  AShowType: TMarksShowType; AIgnoredID: Integer): TMarksIteratorBase;
begin
  Result := TMarksIteratorVisibleInRectWithIgnore.Create(Self, AZoom, ARect, AShowType, AIgnoredID);
end;

procedure TMarksDB.WriteMark(AMark: TMarkFull);
begin
  if AMark.id >= 0 then begin
    DMMarksDb.CDSmarks.Locate('id', AMark.id, []);
    DMMarksDb.CDSmarks.Edit;
  end else begin
    DMMarksDb.CDSmarks.Insert;
  end;
  WriteCurrentMark(AMark);
  DMMarksDb.CDSmarks.Post;
end;

procedure TMarksDB.WriteMarkId(AMark: TMarkId);
begin
  if AMark.id >= 0 then begin
    DMMarksDb.CDSmarks.Locate('id', AMark.id, []);
    DMMarksDb.CDSmarks.Edit;
    WriteCurrentMarkId(AMark);
    DMMarksDb.CDSmarks.Post;
  end;
end;

function TMarksDB.DeleteMark(AMarkId: TMarkId): Boolean;
begin
  result := false;
  if DMMarksDb.CDSmarks.Locate('id', AMarkId.id, []) then begin
    DMMarksDb.CDSmarks.Delete;
    SaveMarks2File;
    result := true;
  end;
end;

procedure TMarksDB.WriteCategoriesList(AStrings: TStrings);
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

procedure TMarksDB.Marsk2StringsWithMarkId(ACategoryId: TCategoryId; AStrings: TStrings);
var
  i: Integer;
  VMarkId: TMarkId;
begin
  for i := 0 to AStrings.Count - 1 do begin
    AStrings.Objects[i].Free;
  end;
  AStrings.Clear;
  DMMarksDb.CDSmarks.Filtered := false;
  DMMarksDb.CDSmarks.Filter := 'categoryid = ' + inttostr(ACategoryId.id);
  DMMarksDb.CDSmarks.Filtered := true;
  DMMarksDb.CDSmarks.First;
  while not (DMMarksDb.CDSmarks.Eof) do begin
    VMarkId := TMarkId.Create;
    ReadCurrentMarkId(VMarkId);
    AStrings.AddObject(VMarkId.name, VMarkId);
    DMMarksDb.CDSmarks.Next;
  end;
end;

procedure TMarksDB.AllMarsk2StringsWhitMarkId(AStrings: TStrings);
var
  i: Integer;
  VMarkId: TMarkId;
begin
  for i := 0 to AStrings.Count - 1 do begin
    AStrings.Objects[i].Free;
  end;
  AStrings.Clear;
  DMMarksDb.CDSmarks.Filtered := false;
  DMMarksDb.CDSmarks.First;
  while not (DMMarksDb.CDSmarks.Eof) do begin
    VMarkId := TMarkId.Create;
    ReadCurrentMarkId(VMarkId);
    AStrings.AddObject(VMarkId.name, VMarkId);
    DMMarksDb.CDSmarks.Next;
  end;
end;

procedure TMarksDB.WriteMarkIdList(AStrings: TStrings);
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

function TMarksDB.SaveMarks2File: boolean;
var
  ms: TMemoryStream;
  XML: string;
begin
  result := true;
  ms := TMemoryStream.Create;
  try
    try
      DMMarksDb.CDSmarks.MergeChangeLog;
      XML := DMMarksDb.CDSmarks.XMLData;
      ms.Write(XML[1], length(XML));
      ms.SaveToFile(GState.MarksFileName);
    except
      result := false;
    end;
  finally
    ms.Free;
  end;
end;

function TMarksDB.SaveCategory2File: boolean;
var
  ms: TMemoryStream;
  XML: string;
begin
  result := true;
  ms := TMemoryStream.Create;
  try
    try
      DMMarksDb.CDSKategory.MergeChangeLog;
      XML := DMMarksDb.CDSKategory.XMLData;
      ms.Write(XML[1], length(XML));
      ms.SaveToFile(GState.MarksCategoryFileName);
    except
      result := false;
    end;
  finally
    ms.Free;
  end;
end;

procedure TMarksDB.LoadMarksFromFile;
begin
  if FileExists(GState.MarksFileName) then begin
    DMMarksDb.CDSMarks.LoadFromFile(GState.MarksFileName);
    if DMMarksDb.CDSMarks.RecordCount > 0 then begin
      CopyFile(PChar(GState.MarksFileName), PChar(GState.MarksBackUpFileName), false);
    end;
  end;
end;

procedure TMarksDB.LoadCategoriesFromFile;
begin
  if FileExists(GState.MarksCategoryFileName) then begin
    DMMarksDb.CDSKategory.LoadFromFile(GState.MarksCategoryFileName);
    if DMMarksDb.CDSKategory.RecordCount > 0 then begin
      CopyFile(PChar(GState.MarksCategoryFileName), PChar(GState.MarksCategoryBackUpFileName), false);
    end;
  end;
end;

procedure TMarksDB.Kategory2StringsWithObjects(AStrings: TStrings);
var
  KategoryId: TCategoryId;
  i: Integer;
begin
  for i := 0 to AStrings.Count - 1 do begin
    AStrings.Objects[i].Free;
  end;
  AStrings.Clear;
  DMMarksDb.CDSKategory.Filtered := false;
  DMMarksDb.CDSKategory.First;
  while not (DMMarksDb.CDSKategory.Eof) do begin
    KategoryId := TCategoryId.Create;
    ReadCurrentCategory(KategoryId);
    AStrings.AddObject(KategoryId.name, KategoryId);
    DMMarksDb.CDSKategory.Next;
  end;
end;

end.

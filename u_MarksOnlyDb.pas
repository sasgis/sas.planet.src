unit u_MarksOnlyDb;

interface

uses
  Windows,
  DBClient,
  SysUtils,
  Classes,
  t_GeoTypes,
  i_MarksFactoryConfig,
  i_MarkCategory,
  i_MarksSimple,
  i_MarkFactory,
  i_MarkFactoryDbInternal;

type
  TMarksOnlyDb =  class
  private
    FSync: IReadWriteSync;
    FBasePath: string;
    CDSmarks: TClientDataSet;
    FFactoryDbInternal: IMarkFactoryDbInternal;
    FFactory: IMarkFactory;
    function ReadCurrentMark: IMarkFull;
    function ReadCurrentMarkId: IMarkId;
    procedure WriteCurrentMarkId(AMark: IMarkId);
    procedure WriteCurrentMark(AMark: IMarkFull);

    function GetMarksFileName: string;
    function GetMarksBackUpFileName: string;
    procedure InitEmptyDS;
  protected
    procedure LockRead; virtual;
    procedure LockWrite; virtual;
    procedure UnlockRead; virtual;
    procedure UnlockWrite; virtual;
  public
    function SaveMarks2File: boolean;
    procedure LoadMarksFromFile;
  public
    constructor Create(ABasePath: string; AFactoryConfig: IMarksFactoryConfig);
    destructor Destroy; override;
    
    function GetMarkByID(AMarkId: IMarkId): IMarkFull;
    function DeleteMark(AMarkId: IMarkId): Boolean;
    procedure DeleteMarksByCategoryID(ACategory: IMarkCategory);
    procedure WriteMark(AMark: IMarkFull);
    procedure WriteMarksList(AMarkList: IInterfaceList);
    procedure SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
    function GetMarkVisible(AMark: IMarkId): Boolean; overload;
    function GetMarkVisible(AMark: IMarkFull): Boolean; overload;
    property Factory: IMarkFactory read FFactory;
    function GetAllMarskIdList: IInterfaceList;
    function GetMarskIdListByCategory(ACategory: IMarkCategory): IInterfaceList;

    procedure SetAllMarksInCategoryVisible(ACategory: IMarkCategory; ANewVisible: Boolean);

    function GetMarksSubset(ARect: TDoubleRect; ACategoryList: IInterfaceList; AIgnoreVisible: Boolean): IMarksSubset;
  end;

implementation

uses
  DB,
  GR32,
  i_MarksDbSmlInternal,
  u_MarkFactory,
  u_MarksSubset;

type
  TExtendedPoint = record
    X, Y: Extended;
  end;


procedure Blob2ExtArr(Blobfield: Tfield; var APoints: TArrayOfDoublePoint);
var
  VSize: Integer;
  VPointsCount: Integer;
  VField: TBlobfield;
  VStream: TStream;
  i: Integer;
  VPoint: TExtendedPoint;
begin
  VField := TBlobfield(BlobField);
  VStream := VField.DataSet.CreateBlobStream(VField, bmRead);
  try
    VSize := VStream.Size;
    VPointsCount := VSize div SizeOf(TExtendedPoint);
    SetLength(APoints, VPointsCount);
    for i := 0 to VPointsCount - 1 do begin
      VStream.ReadBuffer(VPoint, SizeOf(TExtendedPoint));
      APoints[i].X := VPoint.X;
      APoints[i].Y := VPoint.Y;
    end;
  finally
    VStream.Free;
  end;
end;

procedure BlobFromExtArr(AArr: TArrayOfDoublePoint; Blobfield: Tfield);
var
  VField: TBlobfield;
  VStream: TStream;
  VPointsCount: Integer;
  i: Integer;
  VPoint: TExtendedPoint;
begin
  VField := TBlobfield(BlobField);
  VPointsCount := Length(AArr);
  VStream := VField.DataSet.CreateBlobStream(VField, bmWrite);
  try
    for i := 0 to VPointsCount - 1 do begin
      VPoint.X := AArr[i].X;
      VPoint.Y := AArr[i].Y;
      VStream.Write(VPoint, SizeOf(VPoint));
    end;
  finally
    VStream.Free;
  end;
end;

constructor TMarksOnlyDb.Create(
  ABasePath: string;
  AFactoryConfig: IMarksFactoryConfig
);
var
  VFactory: TMarkFactory;
begin
  FBasePath := ABasePath;
  FSync := TSimpleRWSync.Create;
  VFactory := TMarkFactory.Create(AFactoryConfig);
  FFactory := VFactory;
  FFactoryDbInternal := VFactory;
  CDSmarks := TClientDataSet.Create(nil);
  CDSmarks.Name := 'CDSmarks';
  InitEmptyDS;
end;

destructor TMarksOnlyDb.Destroy;
begin
  FreeAndNil(CDSmarks);
  FFactory := nil;
  FFactoryDbInternal := nil;
  FSync := nil;
  inherited;
end;

procedure TMarksOnlyDb.LockRead;
begin
  FSync.BeginRead;
  CDSmarks.DisableControls;
end;

procedure TMarksOnlyDb.LockWrite;
begin
  FSync.BeginWrite;
  CDSmarks.DisableControls;
end;

procedure TMarksOnlyDb.UnlockRead;
begin
  CDSmarks.EnableControls;
  FSync.EndRead;
end;

procedure TMarksOnlyDb.UnlockWrite;
begin
  CDSmarks.EnableControls;
  FSync.EndWrite;
end;

function TMarksOnlyDb.ReadCurrentMarkId: IMarkId;
var
  VId: Integer;
  VName: string;
  VCategoryId: Integer;
  VVisible: Boolean;
begin
  VId := CDSmarks.fieldbyname('id').AsInteger;
  VName := CDSmarks.FieldByName('name').AsString;
  VCategoryId := CDSmarks.FieldByName('categoryid').AsInteger;
  VVisible := CDSmarks.FieldByName('Visible').AsBoolean;
  Result := FFactoryDbInternal.CreateMarkId(VName, VId, VCategoryId, VVisible);
end;

function TMarksOnlyDb.ReadCurrentMark: IMarkFull;
var
  VPicName: string;
  VId: Integer;
  VName: string;
  VVisible: Boolean;
  VPoints: TArrayOfDoublePoint;
  VCategoryId: Integer;
  VDesc: string;
  VLLRect: TDoubleRect;
  VColor1: TColor32;
  VColor2: TColor32;
  VScale1: Integer;
  VScale2: Integer;
begin
  VId := CDSmarks.fieldbyname('id').AsInteger;
  VName := CDSmarks.FieldByName('name').AsString;
  VVisible := CDSmarks.FieldByName('Visible').AsBoolean;
  Blob2ExtArr(CDSmarks.FieldByName('LonLatArr'), VPoints);
  VCategoryId := CDSmarks.FieldByName('categoryid').AsInteger;
  VDesc := CDSmarks.FieldByName('descr').AsString;
  VLLRect.Left := CDSmarks.FieldByName('LonL').AsFloat;
  VLLRect.Top := CDSmarks.FieldByName('LatT').AsFloat;
  VLLRect.Right := CDSmarks.FieldByName('LonR').AsFloat;
  VLLRect.Bottom := CDSmarks.FieldByName('LatB').AsFloat;
  VPicName := CDSmarks.FieldByName('PicName').AsString;
  VColor1 := TColor32(CDSmarks.FieldByName('Color1').AsInteger);
  VColor2 := TColor32(CDSmarks.FieldByName('Color2').AsInteger);
  VScale1 := CDSmarks.FieldByName('Scale1').AsInteger;
  VScale2 := CDSmarks.FieldByName('Scale2').AsInteger;

  Result := FFactoryDbInternal.CreateMark(VId, VName, VVisible, VPicName, VCategoryId, VDesc, VLLRect, VPoints, VColor1, VColor2, VScale1, VScale2);
end;

procedure TMarksOnlyDb.WriteCurrentMarkId(AMark: IMarkId);
begin
  CDSmarks.FieldByName('name').AsString := AMark.name;
  CDSmarks.FieldByName('Visible').AsBoolean := GetMarkVisible(AMark);
end;

procedure TMarksOnlyDb.WriteCurrentMark(AMark: IMarkFull);
var
  VMarkVisible: IMarkSMLInternal;
  VCategoryId: Integer;
  VVisible: Boolean;
begin
  VVisible := True;
  VCategoryId := -1;
  if Supports(AMark, IMarkSMLInternal, VMarkVisible) then begin
    VVisible := VMarkVisible.Visible;
    VCategoryId := VMarkVisible.CategoryId;
  end;
  CDSmarks.FieldByName('name').AsString := AMark.name;
  CDSmarks.FieldByName('Visible').AsBoolean := VVisible;
  BlobFromExtArr(AMark.Points, CDSmarks.FieldByName('LonLatArr'));
  CDSmarks.FieldByName('categoryid').AsInteger := VCategoryId;
  CDSmarks.FieldByName('descr').AsString := AMark.Desc;
  CDSmarks.FieldByName('LonL').AsFloat := AMark.LLRect.Left;
  CDSmarks.FieldByName('LatT').AsFloat := AMark.LLRect.Top;
  CDSmarks.FieldByName('LonR').AsFloat := AMark.LLRect.Right;
  CDSmarks.FieldByName('LatB').AsFloat := AMark.LLRect.Bottom;
  CDSmarks.FieldByName('PicName').AsString := AMark.PicName;
  CDSmarks.FieldByName('Color1').AsInteger := AMark.Color1;
  CDSmarks.FieldByName('Color2').AsInteger := AMark.Color2;
  CDSmarks.FieldByName('Scale1').AsInteger := AMark.Scale1;
  CDSmarks.FieldByName('Scale2').AsInteger := AMark.Scale2;
end;

function TMarksOnlyDb.GetMarkByID(AMarkId: IMarkId): IMarkFull;
var
  VId: Integer;
  VMarkVisible: IMarkSMLInternal;
begin
  Result := nil;
  if AMarkId <> nil then begin
    VId := -1;
    if Supports(AMarkId, IMarkSMLInternal, VMarkVisible) then begin
      VId := VMarkVisible.Id;
    end;
    if VId >= 0 then begin
      LockRead;
      try
        CDSmarks.Filtered := false;
        if CDSmarks.Locate('id', VId, []) then begin
          Result := ReadCurrentMark;
        end;
      finally
        UnlockRead;
      end;
    end;
  end;
end;

function TMarksOnlyDb.GetMarkVisible(AMark: IMarkFull): Boolean;
var
  VMarkVisible: IMarkSMLInternal;
begin
  Result := True;
  if AMark <> nil then begin
    if Supports(AMark, IMarkSMLInternal, VMarkVisible) then begin
      Result := VMarkVisible.Visible;
    end;
  end;
end;

function TMarksOnlyDb.GetMarkVisible(AMark: IMarkId): Boolean;
var
  VMarkVisible: IMarkSMLInternal;
begin
  Result := True;
  if AMark <> nil then begin
    if Supports(AMark, IMarkSMLInternal, VMarkVisible) then begin
      Result := VMarkVisible.Visible;
    end;
  end;
end;

procedure TMarksOnlyDb.WriteMark(AMark: IMarkFull);
begin
  LockWrite;
  try
    CDSmarks.Filtered := false;
    if AMark.id >= 0 then begin
      if CDSmarks.Locate('id', AMark.id, []) then begin
        CDSmarks.Edit;
      end else begin
        CDSmarks.Insert;
      end;
    end else begin
      CDSmarks.Insert;
    end;
    WriteCurrentMark(AMark);
    CDSmarks.Post;
  finally
    UnlockWrite;
  end;
  SaveMarks2File;
end;

procedure TMarksOnlyDb.WriteMarksList(AMarkList: IInterfaceList);
var
  i: Integer;
  VMark: IMarkFull;
begin
  LockWrite;
  try
    CDSmarks.Filtered := false;
    for i := 0 to AMarkList.Count - 1 do begin
      VMark := IMarkFull(AMarkList.Items[i]);
      if VMark.id >= 0 then begin
        if CDSmarks.Locate('id', VMark.id, []) then begin
          CDSmarks.Edit;
        end else begin
          CDSmarks.Insert;
        end;
      end else begin
        CDSmarks.Insert;
      end;
      WriteCurrentMark(VMark);
      CDSmarks.Post;
    end;
  finally
    UnlockWrite;
  end;
  SaveMarks2File;
end;

function TMarksOnlyDb.DeleteMark(AMarkId: IMarkId): Boolean;
var
  VId: Integer;
  VMarkVisible: IMarkSMLInternal;
begin
  result := false;
  VId := -1;
  if Supports(AMarkId, IMarkSMLInternal, VMarkVisible) then begin
    VId := VMarkVisible.Id;
  end;
  if VId >= 0 then begin
    LockWrite;
    try
      CDSmarks.Filtered := false;
      if CDSmarks.Locate('id', VId, []) then begin
        CDSmarks.Delete;
        result := true;
      end;
    finally
      UnlockWrite;
    end;
  end;
  if Result then begin
    SaveMarks2File;
  end;
end;

procedure TMarksOnlyDb.DeleteMarksByCategoryID(ACategory: IMarkCategory);
var
  VDeleted: Boolean;
begin
  VDeleted := False;
  LockWrite;
  try
    CDSmarks.Filtered := false;
    CDSmarks.Filter := 'categoryid = ' + inttostr(ACategory.Id);
    CDSmarks.Filtered := true;
    CDSmarks.First;
    while not (CDSmarks.Eof) do begin
      CDSmarks.Delete;
      VDeleted := True;
    end;
  finally
    UnlockWrite;
  end;
  if VDeleted then begin
    SaveMarks2File;
  end;
end;

procedure TMarksOnlyDb.SetAllMarksInCategoryVisible(ACategory: IMarkCategory;
  ANewVisible: Boolean);
var
  VVisible: Boolean;
begin
  LockRead;
  try
    CDSmarks.Filtered := false;
    CDSmarks.Filter := 'categoryid = ' + inttostr(ACategory.id);
    CDSmarks.Filtered := true;
    CDSmarks.First;
    while not (CDSmarks.Eof) do begin
      VVisible := CDSmarks.FieldByName('Visible').AsBoolean;
      if VVisible <> ANewVisible then begin
        CDSmarks.Edit;
        CDSmarks.FieldByName('Visible').AsBoolean := ANewVisible;
        CDSmarks.Post;
      end;
      CDSmarks.Next;
    end;
  finally
    UnlockRead;
  end;
end;

procedure TMarksOnlyDb.SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
var
  VMarkVisible: IMarkSMLInternal;
  VId: Integer;
begin
  if AMark <> nil then begin
    VId := -1;
    if Supports(AMark, IMarkSMLInternal, VMarkVisible) then begin
      VId := VMarkVisible.Id;
      VMarkVisible.Visible := AVisible;
    end;
    if VId >= 0 then begin
      LockWrite;
      try
        CDSmarks.Filtered := false;
        if CDSmarks.Locate('id', VId, []) then begin
          CDSmarks.Edit;
          WriteCurrentMarkId(AMark);
          CDSmarks.Post;
        end;
      finally
        UnlockWrite;
      end;
    end;
  end;
end;

function TMarksOnlyDb.GetAllMarskIdList: IInterfaceList;
var
  VMarkId: IMarkId;
begin
  Result := TInterfaceList.Create;
  LockRead;
  try
    CDSmarks.Filtered := false;
    CDSmarks.First;
    while not (CDSmarks.Eof) do begin
      VMarkId := ReadCurrentMarkId;
      Result.Add(VMarkId);
      CDSmarks.Next;
    end;
  finally
    UnlockRead;
  end;
end;

function TMarksOnlyDb.GetMarskIdListByCategory(ACategory: IMarkCategory): IInterfaceList;
var
  VMarkId: IMarkId;
begin
  Result := TInterfaceList.Create;
  LockRead;
  try
    CDSmarks.Filtered := false;
    CDSmarks.Filter := 'categoryid = ' + inttostr(ACategory.Id);
    CDSmarks.Filtered := true;
    CDSmarks.First;
    while not (CDSmarks.Eof) do begin
      VMarkId := ReadCurrentMarkId;
      Result.Add(VMarkId);
      CDSmarks.Next;
    end;
  finally
    UnlockRead;
  end;
end;

procedure TMarksOnlyDb.InitEmptyDS;
begin
  CDSmarks.Close;
  CDSmarks.XMLData :=
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'+
    '<DATAPACKET Version="2.0">'+
    '	<METADATA>'+
    '		<FIELDS>'+
    '			<FIELD attrname="id" fieldtype="i4" readonly="true" SUBTYPE="Autoinc"/>'+
    '			<FIELD attrname="name" fieldtype="string" WIDTH="255"/>'+
    '			<FIELD attrname="descr" fieldtype="bin.hex" SUBTYPE="Text"/>'+
    '			<FIELD attrname="scale1" fieldtype="i4"/>'+
    ' 		<FIELD attrname="scale2" fieldtype="i4"/>'+
    '			<FIELD attrname="lonlatarr" fieldtype="bin.hex" SUBTYPE="Binary"/>'+
    '			<FIELD attrname="lonL" fieldtype="r8"/>'+
    '			<FIELD attrname="latT" fieldtype="r8"/>'+
    '			<FIELD attrname="LonR" fieldtype="r8"/>'+
    '			<FIELD attrname="LatB" fieldtype="r8"/>'+
    '			<FIELD attrname="color1" fieldtype="i4"/>'+
    '			<FIELD attrname="color2" fieldtype="i4"/>'+
    '			<FIELD attrname="visible" fieldtype="boolean"/>'+
    '			<FIELD attrname="picname" fieldtype="string" WIDTH="20"/>'+
    '			<FIELD attrname="categoryid" fieldtype="i4"/>'+
    '		</FIELDS>'+
    '		<PARAMS AUTOINCVALUE="1"/>'+
    '	</METADATA>'+
    '	<ROWDATA/>'+
    '</DATAPACKET>';
  CDSmarks.IndexFieldNames := 'categoryid;LonR;LonL;LatT;LatB;visible';
  CDSmarks.Open;
end;

function TMarksOnlyDb.GetMarksSubset(ARect: TDoubleRect;
  ACategoryList: IInterfaceList; AIgnoreVisible: Boolean): IMarksSubset;

  function GetFilterText(
    ARect: TDoubleRect;
    ACategoryList: IInterfaceList;
    AIgnoreVisible: Boolean
  ): string;
  var
    VCategoryFilter: string;
    i: Integer;
  begin
    Result := '';
    if not AIgnoreVisible then begin
      Result := Result + '(visible=1)';
      Result := Result + ' and ';
    end;
    if (ACategoryList <> nil) and (ACategoryList.Count > 0) then begin
      VCategoryFilter := IntToStr(IMarkCategory(ACategoryList[0]).Id);
      for i :=  1 to ACategoryList.Count - 1 do begin
        VCategoryFilter := VCategoryFilter + ', ' + IntToStr(IMarkCategory(ACategoryList[i]).Id);
      end;
      VCategoryFilter := '(categoryid in (' + VCategoryFilter + ')) and';
      Result := Result + VCategoryFilter;
    end;
    Result := Result + '(' +
      ' LonR>' + floattostr(ARect.Left) + ' and' +
      ' LonL<' + floattostr(ARect.Right) + ' and' +
      ' LatB<' + floattostr(ARect.Top) + ' and' +
      ' LatT>' + floattostr(ARect.Bottom) +
      ')';
  end;
var
  VMark: IMarkFull;
  VList: IInterfaceList;
begin
  VList := TInterfaceList.Create;
  Result := TMarksSubset.Create(VList);
  VList.Lock;
  try
    LockRead;
    try
      CDSmarks.Filtered := false;
      CDSmarks.Filter := GetFilterText(ARect, ACategoryList, AIgnoreVisible);
      CDSmarks.Filtered := true;
      CDSmarks.First;
      while not (CDSmarks.Eof) do begin
        VMark := ReadCurrentMark;
        if VMark <> nil then begin
          VList.Add(VMark);
        end;
        CDSmarks.Next;
      end;
    finally
      UnlockRead;
    end;
  finally
    VList.Unlock;
  end;
end;

function TMarksOnlyDb.GetMarksBackUpFileName: string;
begin
  Result := FBasePath + 'marks.~sml';
end;

function TMarksOnlyDb.GetMarksFileName: string;
begin
  Result := FBasePath + 'marks.sml';
end;

procedure TMarksOnlyDb.LoadMarksFromFile;
var
  VFileName: string;
begin
  LockWrite;
  try
    VFileName := GetMarksFileName;
    if FileExists(VFileName) then begin
      try
        CDSMarks.LoadFromFile(VFileName);
      except
        InitEmptyDS;
      end;
      if CDSMarks.RecordCount > 0 then begin
        CopyFile(PChar(VFileName), PChar(GetMarksBackUpFileName), false);
      end;
    end;
  finally
    UnlockWrite
  end;
end;

function TMarksOnlyDb.SaveMarks2File: boolean;
var
  VStream: TFileStream;
  XML: string;
begin
  result := true;
  VStream := TFileStream.Create(GetMarksFileName, fmCreate);;
  try
    try
      LockRead;
      try
        CDSmarks.MergeChangeLog;
        XML := CDSmarks.XMLData;
        VStream.WriteBuffer(XML[1], length(XML));
      finally
        UnlockRead;
      end;
    except
      result := false;
    end;
  finally
    VStream.Free;
  end;
end;

end.

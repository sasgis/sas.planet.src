unit u_MarksDb;

interface

uses
  Windows,
  DBClient,
  SysUtils,
  Classes,
  t_GeoTypes,
  i_MarksFactoryConfig,
  i_HtmlToHintTextConverter,
  i_MarkCategoryDBSmlInternal,
  i_MarkCategory,
  i_MarksSimple,
  i_MarkFactory,
  i_MarksDb,
  i_MarksDbSmlInternal,
  i_MarkFactorySmlInternal;

type
  TMarksDb =  class(TInterfacedObject, IMarksDb, IMarksDbSmlInternal)
  private
    FSync: IReadWriteSync;
    FBasePath: string;
    FCdsMarks: TClientDataSet;
    FFactoryDbInternal: IMarkFactorySmlInternal;
    FFactory: IMarkFactory;
    function ReadCurrentMark: IMark;
    function ReadCurrentMarkId: IMarkId;
    procedure WriteCurrentMarkId(AMark: IMarkId);
    procedure WriteCurrentMark(AMark: IMark);

    function GetMarksFileName: string;
    function GetMarksBackUpFileName: string;
    function GetDbCode: Integer;
    procedure InitEmptyDS;
  protected
    procedure LockRead; virtual;
    procedure LockWrite; virtual;
    procedure UnlockRead; virtual;
    procedure UnlockWrite; virtual;
  protected
    function SaveMarks2File: boolean;
    procedure LoadMarksFromFile;
  protected
    function GetMarkByID(AMarkId: IMarkId): IMark;
    function DeleteMark(AMarkId: IMarkId): Boolean;
    procedure DeleteMarksByCategoryID(ACategory: IMarkCategory);
    procedure WriteMark(AMark: IMark);
    procedure WriteMarksList(AMarkList: IInterfaceList);
    procedure SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
    function GetMarkVisible(AMark: IMarkId): Boolean; overload;
    function GetMarkVisible(AMark: IMark): Boolean; overload;
    function GetFactory: IMarkFactory;
    function GetAllMarskIdList: IInterfaceList;
    function GetMarskIdListByCategory(ACategory: IMarkCategory): IInterfaceList;

    procedure SetAllMarksInCategoryVisible(ACategory: IMarkCategory; ANewVisible: Boolean);

    function GetMarksSubset(ARect: TDoubleRect; ACategoryList: IInterfaceList; AIgnoreVisible: Boolean): IMarksSubset;
  public
    constructor Create(
      ABasePath: string;
      ACategoryDB: IMarkCategoryDBSmlInternal;
      AHintConverter: IHtmlToHintTextConverter;
      AFactoryConfig: IMarksFactoryConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  DB,
  GR32,
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

procedure BlobFromExtArr(AArr: PDoublePointArray; APointsCount: Integer; Blobfield: Tfield);
var
  VField: TBlobfield;
  VStream: TStream;
  i: Integer;
  VPoint: TExtendedPoint;
begin
  VField := TBlobfield(BlobField);
  VStream := VField.DataSet.CreateBlobStream(VField, bmWrite);
  try
    for i := 0 to APointsCount - 1 do begin
      VPoint.X := AArr[i].X;
      VPoint.Y := AArr[i].Y;
      VStream.Write(VPoint, SizeOf(VPoint));
    end;
  finally
    VStream.Free;
  end;
end;

constructor TMarksDb.Create(
  ABasePath: string;
  ACategoryDB: IMarkCategoryDBSmlInternal;
  AHintConverter: IHtmlToHintTextConverter;
  AFactoryConfig: IMarksFactoryConfig
);
var
  VFactory: TMarkFactory;
begin
  FBasePath := ABasePath;
  FSync := TSimpleRWSync.Create;
  VFactory :=
    TMarkFactory.Create(
      GetDbCode,
      AFactoryConfig,
      AHintConverter,
      ACategoryDB
    );
  FFactory := VFactory;
  FFactoryDbInternal := VFactory;
  FCdsMarks := TClientDataSet.Create(nil);
  FCdsMarks.Name := 'CDSmarks';
  InitEmptyDS;
end;

destructor TMarksDb.Destroy;
begin
  FreeAndNil(FCdsMarks);
  FFactory := nil;
  FFactoryDbInternal := nil;
  FSync := nil;
  inherited;
end;

procedure TMarksDb.LockRead;
begin
  FSync.BeginRead;
  FCdsMarks.DisableControls;
end;

procedure TMarksDb.LockWrite;
begin
  FSync.BeginWrite;
  FCdsMarks.DisableControls;
end;

procedure TMarksDb.UnlockRead;
begin
  FCdsMarks.EnableControls;
  FSync.EndRead;
end;

procedure TMarksDb.UnlockWrite;
begin
  FCdsMarks.EnableControls;
  FSync.EndWrite;
end;

function TMarksDb.ReadCurrentMarkId: IMarkId;
var
  VId: Integer;
  VName: string;
  VCategoryId: Integer;
  VVisible: Boolean;
begin
  VId := FCdsMarks.fieldbyname('id').AsInteger;
  VName := FCdsMarks.FieldByName('name').AsString;
  VCategoryId := FCdsMarks.FieldByName('categoryid').AsInteger;
  VVisible := FCdsMarks.FieldByName('Visible').AsBoolean;
  Result := FFactoryDbInternal.CreateMarkId(VName, VId, VCategoryId, VVisible);
end;

function TMarksDb.ReadCurrentMark: IMark;
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
  VId := FCdsMarks.fieldbyname('id').AsInteger;
  VName := FCdsMarks.FieldByName('name').AsString;
  VVisible := FCdsMarks.FieldByName('Visible').AsBoolean;
  Blob2ExtArr(FCdsMarks.FieldByName('LonLatArr'), VPoints);
  VCategoryId := FCdsMarks.FieldByName('categoryid').AsInteger;
  VDesc := FCdsMarks.FieldByName('descr').AsString;
  VLLRect.Left := FCdsMarks.FieldByName('LonL').AsFloat;
  VLLRect.Top := FCdsMarks.FieldByName('LatT').AsFloat;
  VLLRect.Right := FCdsMarks.FieldByName('LonR').AsFloat;
  VLLRect.Bottom := FCdsMarks.FieldByName('LatB').AsFloat;
  VPicName := FCdsMarks.FieldByName('PicName').AsString;
  VColor1 := TColor32(FCdsMarks.FieldByName('Color1').AsInteger);
  VColor2 := TColor32(FCdsMarks.FieldByName('Color2').AsInteger);
  VScale1 := FCdsMarks.FieldByName('Scale1').AsInteger;
  VScale2 := FCdsMarks.FieldByName('Scale2').AsInteger;

  Result := FFactoryDbInternal.CreateMark(VId, VName, VVisible, VPicName, VCategoryId, VDesc, VLLRect, VPoints, VColor1, VColor2, VScale1, VScale2);
end;

procedure TMarksDb.WriteCurrentMarkId(AMark: IMarkId);
begin
  FCdsMarks.FieldByName('name').AsString := AMark.name;
  FCdsMarks.FieldByName('Visible').AsBoolean := GetMarkVisible(AMark);
end;

procedure TMarksDb.WriteCurrentMark(AMark: IMark);
var
  VMarkVisible: IMarkSMLInternal;
  VMarkPointSml: IMarkPointSMLInternal;
  VPicName: string;
  VCategoryId: Integer;
  VVisible: Boolean;
  VMarkPoint: IMarkPoint;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
  VPoint: TDoublePoint;
begin
  VVisible := True;
  VCategoryId := -1;
  if Supports(AMark, IMarkSMLInternal, VMarkVisible) then begin
    VVisible := VMarkVisible.Visible;
    VCategoryId := VMarkVisible.CategoryId;
  end;

  FCdsMarks.FieldByName('Visible').AsBoolean := VVisible;
  FCdsMarks.FieldByName('name').AsString := AMark.name;
  FCdsMarks.FieldByName('categoryid').AsInteger := VCategoryId;
  FCdsMarks.FieldByName('descr').AsString := AMark.Desc;
  FCdsMarks.FieldByName('LonL').AsFloat := AMark.LLRect.Left;
  FCdsMarks.FieldByName('LatT').AsFloat := AMark.LLRect.Top;
  FCdsMarks.FieldByName('LonR').AsFloat := AMark.LLRect.Right;
  FCdsMarks.FieldByName('LatB').AsFloat := AMark.LLRect.Bottom;

  if Supports(AMark, IMarkPoint, VMarkPoint) then begin
    VPicName := '';
    if Supports(AMark, IMarkPointSMLInternal, VMarkPointSml) then begin
      VPicName := VMarkPointSml.PicName;
    end;
    FCdsMarks.FieldByName('PicName').AsString := VPicName;
    VPoint := VMarkPoint.Point;
    BlobFromExtArr(@VPoint, 1, FCdsMarks.FieldByName('LonLatArr'));
    FCdsMarks.FieldByName('Color1').AsInteger := VMarkPoint.TextColor;
    FCdsMarks.FieldByName('Color2').AsInteger := VMarkPoint.TextBgColor;
    FCdsMarks.FieldByName('Scale1').AsInteger := VMarkPoint.FontSize;
    FCdsMarks.FieldByName('Scale2').AsInteger := VMarkPoint.MarkerSize;
  end else if Supports(AMark, IMarkLine, VMarkLine) then begin
    FCdsMarks.FieldByName('PicName').AsString := '';
    BlobFromExtArr(@VMarkLine.Points[0], Length(VMarkLine.Points), FCdsMarks.FieldByName('LonLatArr'));
    FCdsMarks.FieldByName('Color1').AsInteger := VMarkLine.LineColor;
    FCdsMarks.FieldByName('Color2').AsInteger := 0;
    FCdsMarks.FieldByName('Scale1').AsInteger := VMarkLine.LineWidth;
    FCdsMarks.FieldByName('Scale2').AsInteger := 0;
  end else if Supports(AMark, IMarkPoly, VMarkPoly) then begin
    FCdsMarks.FieldByName('PicName').AsString := '';
    BlobFromExtArr(@VMarkPoly.Points[0], Length(VMarkPoly.Points), FCdsMarks.FieldByName('LonLatArr'));
    FCdsMarks.FieldByName('Color1').AsInteger := VMarkPoly.BorderColor;
    FCdsMarks.FieldByName('Color2').AsInteger := VMarkPoly.FillColor;
    FCdsMarks.FieldByName('Scale1').AsInteger := VMarkPoly.LineWidth;
    FCdsMarks.FieldByName('Scale2').AsInteger := 0;
  end;
end;

function TMarksDb.GetMarkByID(AMarkId: IMarkId): IMark;
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
        FCdsMarks.Filtered := false;
        if FCdsMarks.Locate('id', VId, []) then begin
          Result := ReadCurrentMark;
        end;
      finally
        UnlockRead;
      end;
    end;
  end;
end;

function TMarksDb.GetMarkVisible(AMark: IMark): Boolean;
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

function TMarksDb.GetMarkVisible(AMark: IMarkId): Boolean;
var
  VMarkInternal: IMarkSMLInternal;
begin
  Result := True;
  if AMark <> nil then begin
    if Supports(AMark, IMarkSMLInternal, VMarkInternal) then begin
      Result := VMarkInternal.Visible;
    end;
  end;
end;

procedure TMarksDb.WriteMark(AMark: IMark);
var
  VId: Integer;
  VMarkInternal: IMarkSMLInternal;
begin
  Assert(AMark <> nil);
  VId := -1;
  if Supports(AMark, IMarkSMLInternal, VMarkInternal) then begin
    VId := VMarkInternal.Id;
  end;
  LockWrite;
  try
    FCdsMarks.Filtered := false;
    if VId >= 0 then begin
      if FCdsMarks.Locate('id', VId, []) then begin
        FCdsMarks.Edit;
      end else begin
        FCdsMarks.Insert;
      end;
    end else begin
      FCdsMarks.Insert;
    end;
    WriteCurrentMark(AMark);
    FCdsMarks.Post;
  finally
    UnlockWrite;
  end;
  SaveMarks2File;
end;

procedure TMarksDb.WriteMarksList(AMarkList: IInterfaceList);
var
  i: Integer;
  VMark: IMark;
  VId: Integer;
  VMarkInternal: IMarkSMLInternal;
begin
  LockWrite;
  try
    FCdsMarks.Filtered := false;
    for i := 0 to AMarkList.Count - 1 do begin
      VMark := IMark(AMarkList.Items[i]);
      VId := -1;
      if Supports(VMark, IMarkSMLInternal, VMarkInternal) then begin
        VId := VMarkInternal.Id;
      end;
      if VId >= 0 then begin
        if FCdsMarks.Locate('id', VId, []) then begin
          FCdsMarks.Edit;
        end else begin
          FCdsMarks.Insert;
        end;
      end else begin
        FCdsMarks.Insert;
      end;
      WriteCurrentMark(VMark);
      FCdsMarks.Post;
    end;
  finally
    UnlockWrite;
  end;
  SaveMarks2File;
end;

function TMarksDb.DeleteMark(AMarkId: IMarkId): Boolean;
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
      FCdsMarks.Filtered := false;
      if FCdsMarks.Locate('id', VId, []) then begin
        FCdsMarks.Delete;
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

procedure TMarksDb.DeleteMarksByCategoryID(ACategory: IMarkCategory);
var
  VDeleted: Boolean;
  VCategoryID: Integer;
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  Assert(ACategory <> nil);
  VCategoryID := -1;
  if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
    VCategoryID := VCategoryInternal.Id;
  end;
  VDeleted := False;
  if VCategoryID >= 0 then begin
    LockWrite;
    try
      FCdsMarks.Filtered := false;
      FCdsMarks.Filter := 'categoryid = ' + inttostr(VCategoryID);
      FCdsMarks.Filtered := true;
      FCdsMarks.First;
      while not (FCdsMarks.Eof) do begin
        FCdsMarks.Delete;
        VDeleted := True;
      end;
    finally
      UnlockWrite;
    end;
  end;
  if VDeleted then begin
    SaveMarks2File;
  end;
end;

procedure TMarksDb.SetAllMarksInCategoryVisible(ACategory: IMarkCategory;
  ANewVisible: Boolean);
var
  VVisible: Boolean;
  VCategoryID: Integer;
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  VCategoryID := -1;
  if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
    VCategoryID := VCategoryInternal.Id;
  end;

  if VCategoryID >= 0 then begin
    LockRead;
    try
      FCdsMarks.Filtered := false;
      FCdsMarks.Filter := 'categoryid = ' + inttostr(VCategoryID);
      FCdsMarks.Filtered := true;
      FCdsMarks.First;
      while not (FCdsMarks.Eof) do begin
        VVisible := FCdsMarks.FieldByName('Visible').AsBoolean;
        if VVisible <> ANewVisible then begin
          FCdsMarks.Edit;
          FCdsMarks.FieldByName('Visible').AsBoolean := ANewVisible;
          FCdsMarks.Post;
        end;
        FCdsMarks.Next;
      end;
    finally
      UnlockRead;
    end;
  end;
end;

procedure TMarksDb.SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
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
        FCdsMarks.Filtered := false;
        if FCdsMarks.Locate('id', VId, []) then begin
          FCdsMarks.Edit;
          WriteCurrentMarkId(AMark);
          FCdsMarks.Post;
        end;
      finally
        UnlockWrite;
      end;
    end;
  end;
end;

function TMarksDb.GetAllMarskIdList: IInterfaceList;
var
  VMarkId: IMarkId;
begin
  Result := TInterfaceList.Create;
  LockRead;
  try
    FCdsMarks.Filtered := false;
    FCdsMarks.First;
    while not (FCdsMarks.Eof) do begin
      VMarkId := ReadCurrentMarkId;
      Result.Add(VMarkId);
      FCdsMarks.Next;
    end;
  finally
    UnlockRead;
  end;
end;

function TMarksDb.GetDbCode: Integer;
begin
  Result := Integer(Self);
end;

function TMarksDb.GetFactory: IMarkFactory;
begin
  Result := FFactory;
end;

function TMarksDb.GetMarskIdListByCategory(ACategory: IMarkCategory): IInterfaceList;
var
  VMarkId: IMarkId;
  VCategoryID: Integer;
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  Result := TInterfaceList.Create;
  VCategoryID := -1;
  if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
    VCategoryID := VCategoryInternal.Id;
  end;

  if VCategoryID >= 0 then begin
    LockRead;
    try
      FCdsMarks.Filtered := false;
      FCdsMarks.Filter := 'categoryid = ' + inttostr(VCategoryID);
      FCdsMarks.Filtered := true;
      FCdsMarks.First;
      while not (FCdsMarks.Eof) do begin
        VMarkId := ReadCurrentMarkId;
        Result.Add(VMarkId);
        FCdsMarks.Next;
      end;
    finally
      UnlockRead;
    end;
  end;
end;

procedure TMarksDb.InitEmptyDS;
begin
  FCdsMarks.Close;
  FCdsMarks.XMLData :=
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
  FCdsMarks.IndexFieldNames := 'categoryid;LonR;LonL;LatT;LatB;visible';
  FCdsMarks.Open;
end;

function TMarksDb.GetMarksSubset(ARect: TDoubleRect;
  ACategoryList: IInterfaceList; AIgnoreVisible: Boolean): IMarksSubset;
  function GetCategoryID(ACategory: IMarkCategory): Integer;
  var
    VCategoryInternal: IMarkCategorySMLInternal;
  begin
    Assert(ACategory <> nil);
    Result := -1;
    if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
      Result := VCategoryInternal.Id;
    end;
  end;
  function GetFilterText(
    ARect: TDoubleRect;
    ACategoryList: IInterfaceList;
    AIgnoreVisible: Boolean
  ): string;
  var
    VCategoryFilter: string;
    VCategoryID: Integer;
    i: Integer;
  begin
    Result := '';
    if not AIgnoreVisible then begin
      Result := Result + '(visible=1)';
      Result := Result + ' and ';
    end;
    if (ACategoryList <> nil) and (ACategoryList.Count > 0) then begin
      VCategoryID := GetCategoryID(IMarkCategory(ACategoryList[0]));
      VCategoryFilter := IntToStr(VCategoryID);
      for i :=  1 to ACategoryList.Count - 1 do begin
        VCategoryID := GetCategoryID(IMarkCategory(ACategoryList[i]));
        VCategoryFilter := VCategoryFilter + ', ' + IntToStr(VCategoryID);
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
  VMark: IMark;
  VList: IInterfaceList;
begin
  VList := TInterfaceList.Create;
  Result := TMarksSubset.Create(VList);
  VList.Lock;
  try
    LockRead;
    try
      FCdsMarks.Filtered := false;
      FCdsMarks.Filter := GetFilterText(ARect, ACategoryList, AIgnoreVisible);
      FCdsMarks.Filtered := true;
      FCdsMarks.First;
      while not (FCdsMarks.Eof) do begin
        VMark := ReadCurrentMark;
        if VMark <> nil then begin
          VList.Add(VMark);
        end;
        FCdsMarks.Next;
      end;
    finally
      UnlockRead;
    end;
  finally
    VList.Unlock;
  end;
end;

function TMarksDb.GetMarksBackUpFileName: string;
begin
  Result := FBasePath + 'marks.~sml';
end;

function TMarksDb.GetMarksFileName: string;
begin
  Result := FBasePath + 'marks.sml';
end;

procedure TMarksDb.LoadMarksFromFile;
var
  VFileName: string;
begin
  LockWrite;
  try
    VFileName := GetMarksFileName;
    if FileExists(VFileName) then begin
      try
        FCdsMarks.LoadFromFile(VFileName);
      except
        InitEmptyDS;
      end;
      if FCdsMarks.RecordCount > 0 then begin
        CopyFile(PChar(VFileName), PChar(GetMarksBackUpFileName), false);
      end;
    end;
  finally
    UnlockWrite
  end;
end;

function TMarksDb.SaveMarks2File: boolean;
var
  VStream: TFileStream;
  XML: string;
begin
  result := true;
  try
    VStream := TFileStream.Create(GetMarksFileName, fmCreate);;
    try
      LockRead;
      try
        FCdsMarks.MergeChangeLog;
        XML := FCdsMarks.XMLData;
        VStream.WriteBuffer(XML[1], length(XML));
      finally
        UnlockRead;
      end;
    finally
      VStream.Free;
    end;
  except
    result := false;
  end;
end;

end.

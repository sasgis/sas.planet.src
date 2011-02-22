unit u_MarksOnlyDb;

interface

uses
  Windows,
  SysUtils,
  Classes,
  t_GeoTypes,
  dm_MarksDb,
  i_IMarkPicture,
  i_IMarksFactoryConfig,
  i_MarksSimple,
  u_MarkFactory,
  u_MarksSimple;

type
  TMarksOnlyDb =  class
  private
    FSync: IReadWriteSync;
    FBasePath: string;
    FMarkPictureList: IMarkPictureList;
    FDMMarksDb: TDMMarksDb;
    FFactory: TMarkFactory;
    function ReadCurrentMark: IMarkFull;
    function ReadCurrentMarkId: IMarkId;
    procedure WriteCurrentMarkId(AMark: IMarkId);
    procedure WriteCurrentMark(AMark: IMarkFull);

    function GetMarksFileName: string;
    function GetMarksBackUpFileName: string;
  protected
    procedure LockRead; virtual;
    procedure LockWrite; virtual;
    procedure UnlockRead; virtual;
    procedure UnlockWrite; virtual;
  public
    function SaveMarks2File: boolean;
    procedure LoadMarksFromFile;
  public
    constructor Create(ABasePath: string; AMarkPictureList: IMarkPictureList; ADMMarksDb: TDMMarksDb; AFactoryConfig: IMarksFactoryConfig);
    destructor Destroy; override;
    
    function GetMarkByID(id: integer): IMarkFull;
    function GetMarkIdByID(id: integer): IMarkId;
    function DeleteMark(AMarkId: IMarkId): Boolean;
    procedure DeleteMarksByCategoryID(ACategoryID: integer);
    procedure WriteMark(AMark: IMarkFull);
    procedure SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
    function GetMarkVisible(AMark: IMarkId): Boolean; overload;
    function GetMarkVisible(AMark: IMarkFull): Boolean; overload;
    property MarkPictureList: IMarkPictureList read FMarkPictureList;
    property Factory: TMarkFactory read FFactory;
    function GetAllMarskIdList: IInterfaceList;
    function GetMarskIdListByCategory(AId: Integer): IInterfaceList;

    procedure SetAllMarksInCategoryVisible(ACategoryId: TCategoryId; ANewVisible: Boolean);

    function GetMarksSubset(ARect: TDoubleRect; ACategoryIDList: TList; AIgnoreVisible: Boolean): IMarksSubset;
  end;

implementation

uses
  DB,
  GR32,
  Ugeofun,
  u_MarksSubset;

type
  TExtendedPoint = record
    X, Y: Extended;
  end;


procedure Blob2ExtArr(Blobfield: Tfield; var APoints: TDoublePointArray);
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

procedure BlobFromExtArr(AArr: TDoublePointArray; Blobfield: Tfield);
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

constructor TMarksOnlyDb.Create(ABasePath: string;
  AMarkPictureList: IMarkPictureList; ADMMarksDb: TDMMarksDb; AFactoryConfig: IMarksFactoryConfig);
begin
  FBasePath := ABasePath;
  FMarkPictureList := AMarkPictureList;
  FDMMarksDb := ADMMarksDb;
  FSync := TSimpleRWSync.Create;
  FFactory := TMarkFactory.Create(AFactoryConfig);
end;

destructor TMarksOnlyDb.Destroy;
begin
  FreeAndNil(FFactory);
  FSync := nil;
  inherited;
end;

procedure TMarksOnlyDb.LockRead;
begin
  FSync.BeginRead;
  FDMMarksDb.CDSmarks.DisableControls;
end;

procedure TMarksOnlyDb.LockWrite;
begin
  FSync.BeginWrite;
  FDMMarksDb.CDSmarks.DisableControls;
end;

procedure TMarksOnlyDb.UnlockRead;
begin
  FDMMarksDb.CDSmarks.EnableControls;
  FSync.EndRead;
end;

procedure TMarksOnlyDb.UnlockWrite;
begin
  FDMMarksDb.CDSmarks.EnableControls;
  FSync.EndWrite;
end;

function TMarksOnlyDb.ReadCurrentMarkId: IMarkId;
var
  VId: Integer;
  VName: string;
  VVisible: Boolean;
begin
  VId := FDMMarksDb.CDSmarks.fieldbyname('id').AsInteger;
  VName := FDMMarksDb.CDSmarks.FieldByName('name').AsString;
  VVisible := FDMMarksDb.CDSmarks.FieldByName('Visible').AsBoolean;
  Result := TMarkId.Create(VName, VId, VVisible);
end;

function TMarksOnlyDb.ReadCurrentMark: IMarkFull;
var
  VPicName: string;
  VId: Integer;
  VName: string;
  VVisible: Boolean;
  VPoints: TDoublePointArray;
  VCategoryId: Integer;
  VDesc: string;
  VLLRect: TDoubleRect;
  VColor1: TColor32;
  VColor2: TColor32;
  VScale1: Integer;
  VScale2: Integer;
  VPointCount: Integer;
begin
  VId := FDMMarksDb.CDSmarks.fieldbyname('id').AsInteger;
  VName := FDMMarksDb.CDSmarks.FieldByName('name').AsString;
  VVisible := FDMMarksDb.CDSmarks.FieldByName('Visible').AsBoolean;
  Blob2ExtArr(FDMMarksDb.CDSmarks.FieldByName('LonLatArr'), VPoints);
  VCategoryId := FDMMarksDb.CDSmarkscategoryid.AsInteger;
  VDesc := FDMMarksDb.CDSmarks.FieldByName('descr').AsString;
  VLLRect.Left := FDMMarksDb.CDSmarks.FieldByName('LonL').AsFloat;
  VLLRect.Top := FDMMarksDb.CDSmarks.FieldByName('LatT').AsFloat;
  VLLRect.Right := FDMMarksDb.CDSmarks.FieldByName('LonR').AsFloat;
  VLLRect.Bottom := FDMMarksDb.CDSmarks.FieldByName('LatB').AsFloat;
  VPicName := FDMMarksDb.CDSmarks.FieldByName('PicName').AsString;
  VColor1 := TColor32(FDMMarksDb.CDSmarks.FieldByName('Color1').AsInteger);
  VColor2 := TColor32(FDMMarksDb.CDSmarks.FieldByName('Color2').AsInteger);
  VScale1 := FDMMarksDb.CDSmarks.FieldByName('Scale1').AsInteger;
  VScale2 := FDMMarksDb.CDSmarks.FieldByName('Scale2').AsInteger;

  Result := nil;
  
  VPointCount := Length(VPoints);
  if VPointCount > 0 then begin
    if VPointCount = 1 then begin
      Result := FFactory.CreatePoint(VId, VName, VVisible, VPicName, VCategoryId, VDesc, VPoints[0], VColor1, VColor2, VScale1, VScale2)
    end else begin
      if compare2EP(VPoints[0], VPoints[VPointCount - 1]) then begin
        Result := FFactory.CreatePoly(VId, VName, VVisible, VCategoryId, VDesc, VPoints, VColor1, VColor2, VScale1);
      end else begin
        Result := FFactory.CreateLine(VId, VName, VVisible, VCategoryId, VDesc, VPoints, VColor1, VScale1);
      end;
    end;
  end;
end;

procedure TMarksOnlyDb.WriteCurrentMarkId(AMark: IMarkId);
begin
  FDMMarksDb.CDSmarks.FieldByName('name').AsString := AMark.name;
  FDMMarksDb.CDSmarks.FieldByName('Visible').AsBoolean := GetMarkVisible(AMark);
end;

procedure TMarksOnlyDb.WriteCurrentMark(AMark: IMarkFull);
begin
  FDMMarksDb.CDSmarks.FieldByName('name').AsString := AMark.name;
  FDMMarksDb.CDSmarks.FieldByName('Visible').AsBoolean := GetMarkVisible(AMark);
  BlobFromExtArr(AMark.Points, FDMMarksDb.CDSmarks.FieldByName('LonLatArr'));
  FDMMarksDb.CDSmarkscategoryid.AsInteger := AMark.CategoryId;
  FDMMarksDb.CDSmarks.FieldByName('descr').AsString := AMark.Desc;
  FDMMarksDb.CDSmarks.FieldByName('LonL').AsFloat := AMark.LLRect.Left;
  FDMMarksDb.CDSmarks.FieldByName('LatT').AsFloat := AMark.LLRect.Top;
  FDMMarksDb.CDSmarks.FieldByName('LonR').AsFloat := AMark.LLRect.Right;
  FDMMarksDb.CDSmarks.FieldByName('LatB').AsFloat := AMark.LLRect.Bottom;
  FDMMarksDb.CDSmarks.FieldByName('PicName').AsString := AMark.PicName;
  FDMMarksDb.CDSmarks.FieldByName('Color1').AsInteger := AMark.Color1;
  FDMMarksDb.CDSmarks.FieldByName('Color2').AsInteger := AMark.Color2;
  FDMMarksDb.CDSmarks.FieldByName('Scale1').AsInteger := AMark.Scale1;
  FDMMarksDb.CDSmarks.FieldByName('Scale2').AsInteger := AMark.Scale2;
end;

function TMarksOnlyDb.GetMarkByID(id: integer): IMarkFull;
begin
  Result := nil;
  LockRead;
  try
    FDMMarksDb.CDSmarks.Filtered := false;
    if FDMMarksDb.CDSmarks.Locate('id', id, []) then begin
      Result := ReadCurrentMark;
    end;
  finally
    UnlockRead;
  end;
end;

function TMarksOnlyDb.GetMarkIdByID(id: integer): IMarkId;
begin
  Result := nil;
  LockRead;
  try
    FDMMarksDb.CDSmarks.Filtered := false;
    if FDMMarksDb.CDSmarks.Locate('id', id, []) then begin
      Result := ReadCurrentMarkId;
    end;
  finally
    UnlockRead;
  end;
end;

function TMarksOnlyDb.GetMarkVisible(AMark: IMarkFull): Boolean;
var
  VMarkVisible: IMarkVisible;
begin
  Result := True;
  if AMark <> nil then begin
    if Supports(AMark, IMarkVisible, VMarkVisible) then begin
      Result := VMarkVisible.Visible;
    end;
  end;
end;

function TMarksOnlyDb.GetMarkVisible(AMark: IMarkId): Boolean;
var
  VMarkVisible: IMarkVisible;
begin
  Result := True;
  if AMark <> nil then begin
    if Supports(AMark, IMarkVisible, VMarkVisible) then begin
      Result := VMarkVisible.Visible;
    end;
  end;
end;

procedure TMarksOnlyDb.WriteMark(AMark: IMarkFull);
begin
  LockWrite;
  try
    FDMMarksDb.CDSmarks.Filtered := false;
    if AMark.id >= 0 then begin
      if FDMMarksDb.CDSmarks.Locate('id', AMark.id, []) then begin
        FDMMarksDb.CDSmarks.Edit;
      end else begin
        FDMMarksDb.CDSmarks.Insert;
      end;
    end else begin
      FDMMarksDb.CDSmarks.Insert;
    end;
    WriteCurrentMark(AMark);
    FDMMarksDb.CDSmarks.Post;
  finally
    UnlockWrite;
  end;
  SaveMarks2File;
end;

function TMarksOnlyDb.DeleteMark(AMarkId: IMarkId): Boolean;
begin
  result := false;
  LockWrite;
  try
    FDMMarksDb.CDSmarks.Filtered := false;
    if FDMMarksDb.CDSmarks.Locate('id', AMarkId.id, []) then begin
      FDMMarksDb.CDSmarks.Delete;
      result := true;
    end;
  finally
    UnlockWrite;
  end;
  if Result then begin
    SaveMarks2File;
  end;
end;

procedure TMarksOnlyDb.DeleteMarksByCategoryID(ACategoryID: integer);
var
  VDeleted: Boolean;
begin
  VDeleted := False;
  LockWrite;
  try
    FDMMarksDb.CDSmarks.Filtered := false;
    FDMMarksDb.CDSmarks.Filter := 'categoryid = ' + inttostr(ACategoryID);
    FDMMarksDb.CDSmarks.Filtered := true;
    FDMMarksDb.CDSmarks.First;
    while not (FDMMarksDb.CDSmarks.Eof) do begin
      FDMMarksDb.CDSmarks.Delete;
      VDeleted := True;
    end;
  finally
    UnlockWrite;
  end;
  if VDeleted then begin
    SaveMarks2File;
  end;
end;

procedure TMarksOnlyDb.SetAllMarksInCategoryVisible(ACategoryId: TCategoryId;
  ANewVisible: Boolean);
var
  VVisible: Boolean;
begin
  LockRead;
  try
    FDMMarksDb.CDSmarks.Filtered := false;
    FDMMarksDb.CDSmarks.Filter := 'categoryid = ' + inttostr(ACategoryId.id);
    FDMMarksDb.CDSmarks.Filtered := true;
    FDMMarksDb.CDSmarks.First;
    while not (FDMMarksDb.CDSmarks.Eof) do begin
      VVisible := FDMMarksDb.CDSmarks.FieldByName('Visible').AsBoolean;
      if VVisible <> ANewVisible then begin
        FDMMarksDb.CDSmarks.Edit;
        FDMMarksDb.CDSmarks.FieldByName('Visible').AsBoolean := ANewVisible;
        FDMMarksDb.CDSmarks.Post;
      end;
      FDMMarksDb.CDSmarks.Next;
    end;
  finally
    UnlockRead;
  end;
end;

procedure TMarksOnlyDb.SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
var
  VMarkVisible: IMarkVisible;
begin
  if AMark <> nil then begin
    if Supports(AMark, IMarkVisible, VMarkVisible) then begin
      VMarkVisible.Visible := AVisible;
    end;
    if AMark.id >= 0 then begin
      LockWrite;
      try
        FDMMarksDb.CDSmarks.Filtered := false;
        if FDMMarksDb.CDSmarks.Locate('id', AMark.id, []) then begin
          FDMMarksDb.CDSmarks.Edit;
          WriteCurrentMarkId(AMark);
          FDMMarksDb.CDSmarks.Post;
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
    FDMMarksDb.CDSmarks.Filtered := false;
    FDMMarksDb.CDSmarks.First;
    while not (FDMMarksDb.CDSmarks.Eof) do begin
      VMarkId := ReadCurrentMarkId;
      Result.Add(VMarkId);
      FDMMarksDb.CDSmarks.Next;
    end;
  finally
    UnlockRead;
  end;
end;

function TMarksOnlyDb.GetMarskIdListByCategory(AId: Integer): IInterfaceList;
var
  VMarkId: IMarkId;
begin
  Result := TInterfaceList.Create;
  LockRead;
  try
    FDMMarksDb.CDSmarks.Filtered := false;
    FDMMarksDb.CDSmarks.Filter := 'categoryid = ' + inttostr(AId);
    FDMMarksDb.CDSmarks.Filtered := true;
    FDMMarksDb.CDSmarks.First;
    while not (FDMMarksDb.CDSmarks.Eof) do begin
      VMarkId := ReadCurrentMarkId;
      Result.Add(VMarkId);
      FDMMarksDb.CDSmarks.Next;
    end;
  finally
    UnlockRead;
  end;
end;

function TMarksOnlyDb.GetMarksSubset(ARect: TDoubleRect;
  ACategoryIDList: TList; AIgnoreVisible: Boolean): IMarksSubset;

  function GetFilterText(
    ARect: TDoubleRect;
    ACategoryIDList: TList;
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
    if (ACategoryIDList <> nil) and (ACategoryIDList.Count > 0) then begin
      VCategoryFilter := IntToStr(integer(ACategoryIDList[0]));
      for i :=  1 to ACategoryIDList.Count - 1 do begin
        VCategoryFilter := VCategoryFilter + ', ' + IntToStr(integer(ACategoryIDList[i]));
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
      FDMMarksDb.CDSmarks.Filtered := false;
      FDMMarksDb.CDSmarks.Filter := GetFilterText(ARect, ACategoryIDList, AIgnoreVisible);
      FDMMarksDb.CDSmarks.Filtered := true;
      FDMMarksDb.CDSmarks.First;
      while not (FDMMarksDb.CDSmarks.Eof) do begin
        VMark := ReadCurrentMark;
        if VMark <> nil then begin
          VList.Add(VMark);
        end;
        FDMMarksDb.CDSmarks.Next;
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
      FDMMarksDb.CDSMarks.LoadFromFile(VFileName);
      if FDMMarksDb.CDSMarks.RecordCount > 0 then begin
        CopyFile(PChar(VFileName), PChar(GetMarksBackUpFileName), false);
      end;
    end;
  finally
    UnlockWrite
  end;
end;

function TMarksOnlyDb.SaveMarks2File: boolean;
var
  ms: TMemoryStream;
  XML: string;
begin
  result := true;
  ms := TMemoryStream.Create;
  try
    try
      LockRead;
      try
        FDMMarksDb.CDSmarks.MergeChangeLog;
        XML := FDMMarksDb.CDSmarks.XMLData;
        ms.Write(XML[1], length(XML));
        ms.SaveToFile(GetMarksFileName);
      finally
        UnlockRead;
      end;
    except
      result := false;
    end;
  finally
    ms.Free;
  end;
end;

end.

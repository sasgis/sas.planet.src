unit u_MarksOnlyDb;

interface

uses
  Windows,
  Classes,
  t_GeoTypes,
  t_CommonTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  dm_MarksDb,
  i_IMarkPicture,
  i_MarksSimple,
  u_MarkFactory,
  u_MarksSimple;

type
  TMarksOnlyDb =  class
  private
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
  public
    function SaveMarks2File: boolean;
    procedure LoadMarksFromFile;
  public
    constructor Create(ABasePath: string; AMarkPictureList: IMarkPictureList; ADMMarksDb: TDMMarksDb);
    function GetMarkByID(id: integer): IMarkFull;
    function GetMarkIdByID(id: integer): IMarkId;
    function DeleteMark(AMarkId: IMarkId): Boolean;
    procedure WriteMark(AMark: IMarkFull);
    procedure WriteMarkId(AMark: IMarkId);
    procedure SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
    function GetMarkVisible(AMark: IMarkId): Boolean;
    property MarkPictureList: IMarkPictureList read FMarkPictureList;
    property Factory: TMarkFactory read FFactory;
    function GetAllMarskIdList: IInterfaceList;
    function GetMarskIdListByCategory(AId: Integer): IInterfaceList;

    procedure SetAllMarksInCategoryVisible(ACategoryId: TCategoryId; ANewVisible: Boolean);

    function GetMarksIteratorByCategoryIdList(ARect: TDoubleRect; ACategoryIDList: TList; AIgnoreVisible: Boolean; AOwnList: Boolean): TMarksIteratorBase;
  end;

implementation

uses
  DB,
  SysUtils,
  Contnrs,
  GR32,
  u_MarksSimpleNew;

type
  TMarksIteratorVisibleInRectBase = class(TMarksIteratorBase)
  private
    FMarksDb: TMarksOnlyDb;
    FFinished: Boolean;
    FRect: TDoubleRect;
  protected
    procedure FinishIterate;
  public
    constructor Create(AMarksDb: TMarksOnlyDb; ARect: TDoubleRect);
    destructor Destroy; override;
    function Next: Boolean; override;
  end;

  TMarksIteratorVisibleInRectByCategoryList = class(TMarksIteratorVisibleInRectBase)
  private
    FCategoryIDList: TList;
    FOwnList: Boolean;
    FIgnoreVisible: Boolean;
  protected
    function GetFilterText: string; virtual;
  public
    constructor Create(AMarksDb: TMarksOnlyDb; ARect: TDoubleRect; ACategoryIDList: TList; AIgnoreVisible: Boolean; AOwnList: Boolean);
    destructor Destroy; override;
  end;

  TMarksIteratorVisibleInRectWithIgnore = class(TMarksIteratorVisibleInRectByCategoryList)
  private
    FIgnoredID: Integer;
  protected
    function GetFilterText: string; override;
  public
    constructor Create(AMarksDb: TMarksOnlyDb; ARect: TDoubleRect; ACategoryIDList: TList; AIgnoreVisible: Boolean; AOwnList: Boolean; AIgnoredID: Integer);
  end;

{ TMarksIteratorVisibleInRectBase }

constructor TMarksIteratorVisibleInRectBase.Create(AMarksDb: TMarksOnlyDb;
  ARect: TDoubleRect);
begin
  inherited Create;
  FMarksDb := AMarksDb;
  FRect := ARect;
  FMarksDb.FDMMarksDb.CDSmarks.DisableControls;
  FFinished := False;
end;

destructor TMarksIteratorVisibleInRectBase.Destroy;
begin
  if not FFinished then begin
    FinishIterate;
  end;
  inherited;
end;

procedure TMarksIteratorVisibleInRectBase.FinishIterate;
begin
  FFinished := True;
  FMarksDb.FDMMarksDb.CDSmarks.Filtered := false;
  FMarksDb.FDMMarksDb.CDSmarks.EnableControls;
end;

function TMarksIteratorVisibleInRectBase.Next: Boolean;
begin
  if not FFinished then begin
    FCurrentMark := FMarksDb.ReadCurrentMark;
    FMarksDb.FDMMarksDb.CDSmarks.Next;
    if FMarksDb.FDMMarksDb.CDSmarks.Eof then begin
      FinishIterate;
    end;
    Result := True;
  end else begin
    Result := False;
  end;
end;

{ TMarksIteratorVisibleInRectByCategoryList }

constructor TMarksIteratorVisibleInRectByCategoryList.Create(AMarksDb: TMarksOnlyDb;
  ARect: TDoubleRect; ACategoryIDList: TList; AIgnoreVisible: Boolean; AOwnList: Boolean);
begin
  inherited Create(AMarksDb, ARect);
  FCategoryIDList := ACategoryIDList;
  FOwnList := AOwnList;
  FIgnoreVisible := AIgnoreVisible;
  FMarksDb.FDMMarksDb.CDSmarks.Filter := GetFilterText;
  FMarksDb.FDMMarksDb.CDSmarks.Filtered := true;
  FMarksDb.FDMMarksDb.CDSmarks.First;
  if FMarksDb.FDMMarksDb.CDSmarks.Eof then begin
    FinishIterate;
  end;
end;

destructor TMarksIteratorVisibleInRectByCategoryList.Destroy;
begin
  if FOwnList then begin
    FreeAndNil(FCategoryIDList);
  end;
  inherited;
end;

function TMarksIteratorVisibleInRectByCategoryList.GetFilterText: string;
var
  VCategoryFilter: string;
  i: Integer;
begin
  Result := '';
  if not FIgnoreVisible then begin
    Result := Result + '(visible=1)';
    Result := Result + ' and ';
  end;
  if (FCategoryIDList <> nil) and (FCategoryIDList.Count > 0) then begin
    VCategoryFilter := IntToStr(integer(FCategoryIDList[0]));
    for i :=  1 to FCategoryIDList.Count - 1 do begin
      VCategoryFilter := VCategoryFilter + ',' + IntToStr(integer(FCategoryIDList[i]));
    end;
    VCategoryFilter := '(categoryid in (' + VCategoryFilter + ')) and';
    Result := Result + VCategoryFilter;
  end;
  Result := Result + '(' +
    ' LonR>' + floattostr(FRect.Left) + ' and' +
    ' LonL<' + floattostr(FRect.Right) + ' and' +
    ' LatB<' + floattostr(FRect.Top) + ' and' +
    ' LatT>' + floattostr(FRect.Bottom) +
    ')';
end;

{ TMarksIteratorVisibleInRectWithIgnore }

constructor TMarksIteratorVisibleInRectWithIgnore.Create(AMarksDb: TMarksOnlyDb;
  ARect: TDoubleRect; ACategoryIDList: TList; AIgnoreVisible, AOwnList: Boolean;
  AIgnoredID: Integer);
begin
  FIgnoredID := AIgnoredID;
  inherited Create(AMarksDb, ARect, ACategoryIDList, AIgnoreVisible, AOwnList);
end;

function TMarksIteratorVisibleInRectWithIgnore.GetFilterText: string;
begin
  Result := inherited GetFilterText;
  if FIgnoredID >= 0 then begin
    Result := '(id <> '+ IntToStr(FIgnoredID) + ') and ' + Result;
  end;
end;

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
  VPicIndex: Integer;
  VPic: IMarkPicture;
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
  VPicIndex := FMarkPictureList.GetIndexByName(VPicName);
  if VPicIndex < 0 then begin
    VPic := nil;
  end else begin
    VPic := FMarkPictureList.Get(VPicIndex);
  end;
  VColor1 := TColor32(FDMMarksDb.CDSmarks.FieldByName('Color1').AsInteger);
  VColor2 := TColor32(FDMMarksDb.CDSmarks.FieldByName('Color2').AsInteger);
  VScale1 := FDMMarksDb.CDSmarks.FieldByName('Scale1').AsInteger;
  VScale2 := FDMMarksDb.CDSmarks.FieldByName('Scale2').AsInteger;
  Result := TMarkFull.Create(VName, VId, VVisible, VPicName, VPic, VCategoryId, VDesc, VLLRect, VPoints, VColor1, VColor2, VScale1, VScale2);
end;

procedure TMarksOnlyDb.WriteCurrentMarkId(AMark: IMarkId);
begin
  FDMMarksDb.CDSmarks.FieldByName('name').AsString := AMark.name;
  FDMMarksDb.CDSmarks.FieldByName('Visible').AsBoolean := False;
end;

procedure TMarksOnlyDb.WriteCurrentMark(AMark: IMarkFull);
begin
  FDMMarksDb.CDSmarks.FieldByName('name').AsString := AMark.name;
  FDMMarksDb.CDSmarks.FieldByName('Visible').AsBoolean := False;
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
  if FDMMarksDb.CDSmarks.Locate('id', id, []) then begin
    Result := ReadCurrentMark;
  end;
end;

function TMarksOnlyDb.GetMarkIdByID(id: integer): IMarkId;
begin
  Result := nil;
  if FDMMarksDb.CDSmarks.Locate('id', id, []) then begin
    Result := ReadCurrentMarkId;
  end;
end;

function TMarksOnlyDb.GetMarksIteratorByCategoryIdList(ARect: TDoubleRect;
  ACategoryIDList: TList; AIgnoreVisible: Boolean; AOwnList: Boolean): TMarksIteratorBase;
begin
  Result := TMarksIteratorVisibleInRectByCategoryList.Create(Self, ARect, ACategoryIDList, AIgnoreVisible, AOwnList);
end;

function TMarksOnlyDb.GetMarkVisible(AMark: IMarkId): Boolean;
begin
  Result := (AMark as IMarkVisible).Visible;
end;

procedure TMarksOnlyDb.WriteMark(AMark: IMarkFull);
begin
  if AMark.id >= 0 then begin
    FDMMarksDb.CDSmarks.Locate('id', AMark.id, []);
    FDMMarksDb.CDSmarks.Edit;
  end else begin
    FDMMarksDb.CDSmarks.Insert;
  end;
  WriteCurrentMark(AMark);
  FDMMarksDb.CDSmarks.Post;
  SaveMarks2File;
end;

procedure TMarksOnlyDb.WriteMarkId(AMark: IMarkId);
begin
  if AMark.id >= 0 then begin
    FDMMarksDb.CDSmarks.Locate('id', AMark.id, []);
    FDMMarksDb.CDSmarks.Edit;
    WriteCurrentMarkId(AMark);
    FDMMarksDb.CDSmarks.Post;
  end;
end;

constructor TMarksOnlyDb.Create(ABasePath: string;
  AMarkPictureList: IMarkPictureList; ADMMarksDb: TDMMarksDb);
begin
  FBasePath := ABasePath;
  FMarkPictureList := AMarkPictureList;
  FDMMarksDb := ADMMarksDb;
end;

function TMarksOnlyDb.DeleteMark(AMarkId: IMarkId): Boolean;
begin
  result := false;
  if FDMMarksDb.CDSmarks.Locate('id', AMarkId.id, []) then begin
    FDMMarksDb.CDSmarks.Delete;
    SaveMarks2File;
    result := true;
  end;
end;

procedure TMarksOnlyDb.SetAllMarksInCategoryVisible(ACategoryId: TCategoryId;
  ANewVisible: Boolean);
var
  VVisible: Boolean;
begin
  FDMMarksDb.CDSmarks.DisableControls;
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
    FDMMarksDb.CDSmarks.EnableControls;
  end;
end;

procedure TMarksOnlyDb.SetMarkVisibleByID(AMark: IMarkId; AVisible: Boolean);
begin
  (AMark as IMarkVisible).Visible := AVisible;
  WriteMarkId(AMark);
end;

function TMarksOnlyDb.GetAllMarskIdList: IInterfaceList;
var
  VMarkId: IMarkId;
begin
  Result := TInterfaceList.Create;
  FDMMarksDb.CDSmarks.DisableControls;
  try
    FDMMarksDb.CDSmarks.Filtered := false;
    FDMMarksDb.CDSmarks.First;
    while not (FDMMarksDb.CDSmarks.Eof) do begin
      VMarkId := ReadCurrentMarkId;
      Result.Add(VMarkId);
      FDMMarksDb.CDSmarks.Next;
    end;
  finally
    FDMMarksDb.CDSmarks.EnableControls;
  end;
end;

function TMarksOnlyDb.GetMarskIdListByCategory(AId: Integer): IInterfaceList;
var
  VMarkId: IMarkId;
begin
  Result := TInterfaceList.Create;
  FDMMarksDb.CDSmarks.DisableControls;
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
    FDMMarksDb.CDSmarks.EnableControls;
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
  VFileName := GetMarksFileName;
  if FileExists(VFileName) then begin
    FDMMarksDb.CDSMarks.LoadFromFile(VFileName);
    if FDMMarksDb.CDSMarks.RecordCount > 0 then begin
      CopyFile(PChar(VFileName), PChar(GetMarksBackUpFileName), false);
    end;
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
      FDMMarksDb.CDSmarks.MergeChangeLog;
      XML := FDMMarksDb.CDSmarks.XMLData;
      ms.Write(XML[1], length(XML));
      ms.SaveToFile(GetMarksFileName);
    except
      result := false;
    end;
  finally
    ms.Free;
  end;
end;

end.

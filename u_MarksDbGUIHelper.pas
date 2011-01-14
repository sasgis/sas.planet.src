unit u_MarksDbGUIHelper;

interface

uses
  Windows,
  Classes,
  ComCtrls,
  t_GeoTypes,
  i_ICoordConverter,
  u_MarksSimple,
  u_MarksReadWriteSimple;

type
  TMarksDbGUIHelper = class
  private
    FMarksDB: TMarksDB;
  public
    procedure CategoryListToStrings(AList: TList; AStrings: TStrings);
    procedure CategoryListToTree(AList: TList; ATreeItems: TTreeNodes);
    procedure MarksListToStrings(AList: TList; AStrings: TStrings);

    function DeleteMarkModal(id:integer;handle:THandle):boolean;
    function OperationMark(AMark: TMarkFull; AZoom: Byte):boolean;
    function AddKategory(name:string): integer;
    function GetMarkLength(AMark: TMarkFull; AConverter: ICoordConverter):Double;
    function GetMarkSq(AMark: TMarkFull; AConverter: ICoordConverter):Double;
    function EditMarkModal(AMark: TMarkFull):boolean;
    function AddNewPointModal(ALonLat: TDoublePoint): Boolean;
    function SavePolyModal(AID: Integer; ANewArrLL: TDoublePointArray): Boolean;
    function SaveLineModal(AID: Integer; ANewArrLL: TDoublePointArray; ADescription: string): Boolean;
    function GetVisibleCateroriesIDList(AZoom: Byte): TList;
    function GetMarksIterator(ARect: TDoubleRect; AZoom: Byte; AIgnoreMarksVisible: Boolean; AIgnoreCategoriesVisible: Boolean): TMarksIteratorBase;

    property MarksDB: TMarksDB read FMarksDB;
  public
    constructor Create(AMarksDB: TMarksDB);
  end;

implementation

uses
  SysUtils,
  Dialogs,
  UResStrings,
  USaveas,
  UaddPoint,
  UaddPoly,
  UaddLine;

{ TMarksDbGUIHelper }

function TMarksDbGUIHelper.AddKategory(name: string): integer;
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
    FMarksDb.WriteCategory(VCategory);
    Result := VCategory.id;
  finally
    VCategory.Free;
  end;
end;

function TMarksDbGUIHelper.AddNewPointModal(ALonLat: TDoublePoint): Boolean;
var
  VMark: TMarkFull;
begin
  VMark := TMarkFull.Create;
  try
    VMark.id := -1;
    SetLength(VMark.Points, 1);
    VMark.Points[0] := ALonLat;
    Result := FaddPoint.EditMark(VMark, Self);
    if Result then begin
      FMarksDb.WriteMark(VMark);
    end;
  finally
    VMark.Free;
  end;
end;

procedure TMarksDbGUIHelper.MarksListToStrings(AList: TList;
  AStrings: TStrings);
var
  i: Integer;
  VMarkId: TMarkId;
begin
  AStrings.Clear;
  for i := 0 to AList.Count - 1 do begin
    VMarkId := TMarkId(AList[i]);
    AStrings.AddObject(VMarkId.name, VMarkId);
  end;
end;

procedure TMarksDbGUIHelper.CategoryListToStrings(AList: TList; AStrings: TStrings);
var
  i: Integer;
  VCategory: TCategoryId;
begin
  AStrings.Clear;
  for i := 0 to AList.Count - 1 do begin
    VCategory := TCategoryId(AList[i]);
    AStrings.AddObject(VCategory.name, VCategory);
  end;
end;

procedure TMarksDbGUIHelper.CategoryListToTree(AList: TList; ATreeItems: TTreeNodes);
var
  VNodesCached: TStringList;

  function FindNodeWithText(AParent: TTreeNode; const VNamePart: string): TTreeNode;
  var
    i: Integer;
    VKey: string;
  begin
    VKey := VNamePart + IntToStr(Integer(AParent));
    i := VNodesCached.IndexOf(VKey);
    if i > -1 then begin
      Result := Pointer(VNodesCached.Objects[i])
    end else begin
      Result := ATreeItems.AddChildObject(AParent, VNamePart, nil);
      Result.StateIndex:=0;
      VNodesCached.AddObject(VKey, Result);
    end
  end;
  
  procedure AddItem(AParentNode: TTreeNode; const AName: string; Data: TCategoryId);
  var
    VNamePrefix: string;
    VDelimPos: Integer;
    aNode: TTreeNode;
    VNameSufix: string;
  begin
    if AName <> '' then begin
      VDelimPos:=Pos('\', AName);
      VNamePrefix:='';
      if VDelimPos > 0 then begin
        VNamePrefix := Copy(AName, 1, VDelimPos - 1);
        VNameSufix := Copy(AName, VDelimPos + 1, Length(AName));
      end else begin
        VNamePrefix:=AName;
        VNameSufix := '';
      end;
      aNode := FindNodeWithText(AParentNode, VNamePrefix);
      if VDelimPos > 0 then begin
        AddItem(aNode, VNameSufix, Data);
      end else begin
        aNode.Data := Data;
        aNode.StateIndex := 2;
        if Data.visible then begin
          aNode.StateIndex := 1
        end;
      end;
    end;
  end;

var
  i: Integer;
  VCategory: TCategoryId;
begin
  VNodesCached := TStringList.Create;
  try
    VNodesCached.Duplicates := dupIgnore;
    VNodesCached.Sorted := True;
    ATreeItems.Clear;
    ATreeItems.BeginUpdate;
    try
      for i := 0 to AList.Count - 1 do begin
        VCategory := TCategoryId(AList[i]);
        AddItem(nil, VCategory.name, VCategory);
      end;
    finally
      ATreeItems.EndUpdate;
    end;
  finally
    VNodesCached.Free;
  end;
end;

constructor TMarksDbGUIHelper.Create(AMarksDB: TMarksDB);
begin
  FMarksDB := AMarksDB;
end;

function TMarksDbGUIHelper.DeleteMarkModal(id: integer;
  handle: THandle): boolean;
var
  VMarkId: TMarkId;
begin
  result:=false;
  VMarkId := FMarksDb.GetMarkIdByID(id);
  if VMarkId <> nil then begin
    try
      if MessageBox(handle,pchar(SAS_MSG_youasure+' "'+VMarkId.name+'"'),pchar(SAS_MSG_coution),36)=IDNO then exit;
      result:=FMarksDb.DeleteMark(VMarkId);
    finally
      VMarkId.Free;
    end;
  end;
end;

function TMarksDbGUIHelper.EditMarkModal(AMark: TMarkFull): boolean;
begin
  Result := false;
  if AMark.IsPoint then begin
    result:=FaddPoint.EditMark(AMark, Self);
  end else if AMark.IsPoly then begin
    result:=FaddPoly.EditMark(AMark, Self);
  end else if AMark.IsLine then begin
    result:=FaddLine.EditMark(AMark, Self);
  end;
end;

function TMarksDbGUIHelper.GetMarkLength(AMark: TMarkFull; AConverter: ICoordConverter): Double;
var
  i:integer;
  VPointCount: Integer;
begin
  Result:=0;
  VPointCount := Length(AMark.Points);
  if (VPointCount > 1) then begin
    for i:=0 to VPointCount-2 do begin
      Result:=Result+ AConverter.CalcDist(AMark.Points[i], AMark.Points[i+1]);
    end;
  end;
end;

function TMarksDbGUIHelper.GetMarksIterator(ARect: TDoubleRect; AZoom: Byte; AIgnoreMarksVisible,
  AIgnoreCategoriesVisible: Boolean): TMarksIteratorBase;
var
  VList: TList;
begin
  VList := nil;
  if not AIgnoreCategoriesVisible then begin
    VList := GetVisibleCateroriesIDList(AZoom);
  end;
  Result := FMarksDB.GetMarksIteratorByCategoryIdList(ARect, VList, AIgnoreMarksVisible, True);
end;

function TMarksDbGUIHelper.GetMarkSq(AMark: TMarkFull; AConverter: ICoordConverter): Double;
begin
  Result:=0;
  if (Length(AMark.Points) > 1) then begin
    result:= AConverter.CalcPoligonArea(AMark.Points);
  end;
end;

function TMarksDbGUIHelper.GetVisibleCateroriesIDList(AZoom: Byte): TList;
var
  VList: TList;
  VCategory: TCategoryId;
  i: Integer;
begin
  Result := TList.Create;
  VList := FMarksDB.GetCategoriesList;
  try
    for i := 0 to VList.Count - 1 do begin
      VCategory := TCategoryId(VList[i]);
      if
        (VCategory.visible) and
        (VCategory.AfterScale <= AZoom + 1) and
        (VCategory.BeforeScale >= AZoom + 1)
      then begin
        Result.Add(Pointer(VCategory.id));
      end;
    end;
  finally
    VList.Free;
  end;
end;

function TMarksDbGUIHelper.OperationMark(AMark: TMarkFull; AZoom: Byte): boolean;
begin
  Result:=false;
  if AMark.IsPoly then begin
    Fsaveas.Show_(AZoom, AMark.Points);
    Result:=true;
  end else begin
    ShowMessage(SAS_MSG_FunExForPoly);
  end;
end;

function TMarksDbGUIHelper.SaveLineModal(AID: Integer;
  ANewArrLL: TDoublePointArray; ADescription: string): Boolean;
var
  VMark: TMarkFull;
begin
  Result := False;
  if AID < 0 then begin
    VMark := TMarkFull.Create;
  end else begin
    VMark := FMarksDb.GetMarkByID(AID)
  end;
  if VMark <> nil then begin
    try
      VMark.id := AID;
      if VMark.id < 0 then begin
        VMark.Desc := ADescription;
      end;
      VMark.Points := Copy(ANewArrLL);
      Result := FaddLine.EditMark(VMark, Self);
      if Result then begin
        FMarksDb.WriteMark(VMark);
      end;
    finally
      VMark.Free;
    end;
  end;
end;

function TMarksDbGUIHelper.SavePolyModal(AID: Integer;
  ANewArrLL: TDoublePointArray): Boolean;
var
  VMark: TMarkFull;
begin
  Result := False;
  if AID < 0 then begin
    VMark := TMarkFull.Create;
  end else begin
    VMark := FMarksDb.GetMarkByID(AID)
  end;
  if VMark <> nil then begin
    try
      VMark.id := AID;
      VMark.Points := Copy(ANewArrLL);
      VMark.ClosePoly;
      Result := FaddPoly.EditMark(VMark, Self);
      if Result then begin
        FMarksDb.WriteMark(VMark);
      end;
    finally
      VMark.Free;
    end;
  end;
end;

end.

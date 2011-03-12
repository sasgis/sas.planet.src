unit u_MarksDbGUIHelper;

interface

uses
  Windows,
  Classes,
  ComCtrls,
  t_GeoTypes,
  i_ICoordConverter,
  i_IValueToStringConverter,
  i_IMarkPicture,
  i_MarksSimple,
  u_MarksSimple,
  u_MarksReadWriteSimple;

type
  TMarksDbGUIHelper = class
  private
    FMarksDB: TMarksDB;
    FMarkPictureList: IMarkPictureList;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
  public
    procedure CategoryListToStrings(AList: TList; AStrings: TStrings);
    procedure CategoryListToTree(AList: TList; ATreeItems: TTreeNodes);
    procedure MarksListToStrings(AList: IInterfaceList; AStrings: TStrings);

    function DeleteMarkModal(id:integer;handle:THandle):boolean;
    function OperationMark(AID: Integer; AZoom: Byte):boolean;
    function AddKategory(name:string): TCategoryId;
    procedure ShowMarkLength(AID: Integer; AConverter: ICoordConverter; AHandle: THandle);
    procedure ShowMarkSq(AID: Integer; AConverter: ICoordConverter; AHandle: THandle);
    function EditMarkModal(AMark: IMarkFull): IMarkFull;
    function AddNewPointModal(ALonLat: TDoublePoint): Boolean;
    function SavePolyModal(AID: Integer; ANewArrLL: TArrayOfDoublePoint): Boolean;
    function SaveLineModal(AID: Integer; ANewArrLL: TArrayOfDoublePoint; ADescription: string): Boolean;
//    function GetMarksIterator(ARect: TDoubleRect; AZoom: Byte; AIgnoreMarksVisible: Boolean; AIgnoreCategoriesVisible: Boolean): TMarksIteratorBase;

    property MarksDB: TMarksDB read FMarksDB;
    property MarkPictureList: IMarkPictureList read FMarkPictureList;
  public
    constructor Create(AMarksDB: TMarksDB; AValueToStringConverterConfig: IValueToStringConverterConfig; AMarkPictureList: IMarkPictureList);
  end;

implementation

uses
  SysUtils,
  Dialogs,
  i_IDatum,
  UResStrings,
  USaveas,
  UaddPoint,
  UaddPoly,
  UaddLine;

{ TMarksDbGUIHelper }

function TMarksDbGUIHelper.AddKategory(name: string): TCategoryId;
var
  VCategory: TCategoryId;
  VName: string;
  VId: Integer;
begin
  VName := name;
  VName := ExcludeTrailingBackslash(VName);
  if VName = '' then begin
    VName := SAS_STR_NewCategory;
  end;
  VCategory := FMarksDb.CategoryDB.GetCategoryByName(VName);
  if VCategory = nil then begin
    VCategory := TCategoryId.Create;
    VCategory.id := -1;
    VCategory.name := VName;
    VCategory.visible := True;
    VCategory.AfterScale := 3;
    VCategory.BeforeScale := 19;
    FMarksDb.CategoryDB.WriteCategory(VCategory);
  end;
  Result := VCategory;
end;

function TMarksDbGUIHelper.AddNewPointModal(ALonLat: TDoublePoint): Boolean;
var
  VMark: IMarkFull;
begin
  Result := False;
  VMark := FMarksDB.MarksDb.Factory.CreateNewPoint(ALonLat, '', '');
  VMark := FaddPoint.EditMark(VMark, Self);
  if VMark <> nil then begin
    FMarksDb.MarksDb.WriteMark(VMark);
    Result := True;
  end;
end;

procedure TMarksDbGUIHelper.MarksListToStrings(AList: IInterfaceList;
  AStrings: TStrings);
var
  i: Integer;
  VMarkId: IMarkId;
begin
  AStrings.Clear;
  for i := 0 to AList.Count - 1 do begin
    VMarkId := IMarkId(AList[i]);
    AStrings.AddObject(VMarkId.name, Pointer(VMarkId));
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

  function GetKey(AParent: TTreeNode; const VNamePart: string): string;
  begin
    Result := VNamePart + IntToStr(Integer(AParent));
  end;

  function CreateNodeWithText(AParent: TTreeNode; const VNamePart: string; const AKey: string): TTreeNode;
  begin
    Result := ATreeItems.AddChildObject(AParent, VNamePart, nil);
    Result.StateIndex:=0;
    VNodesCached.AddObject(AKey, Result);
  end;

  function FindNodeWithText(AParent: TTreeNode; const VNamePart: string; const AKey: string): TTreeNode;
  var
    i: Integer;
  begin
    i := VNodesCached.IndexOf(AKey);
    if i > -1 then begin
      Result := Pointer(VNodesCached.Objects[i])
    end else begin
      Result := CreateNodeWithText(AParent, VNamePart, AKey);
    end
  end;
  
  procedure AddItem(AParentNode: TTreeNode; const AName: string; Data: TCategoryId);
  var
    VNamePrefix: string;
    VDelimPos: Integer;
    aNode: TTreeNode;
    VNameSufix: string;
  begin
    VDelimPos:=Pos('\', AName);
    VNamePrefix:='';
    if VDelimPos > 0 then begin
      VNamePrefix := Copy(AName, 1, VDelimPos - 1);
      if VNamePrefix = '' then begin
        VNamePrefix := '(NoName)';
      end;
      VNameSufix := Copy(AName, VDelimPos + 1, Length(AName));
      if VNameSufix = '' then begin
        VNameSufix := '(NoName)';
      end;
      aNode := FindNodeWithText(AParentNode, VNamePrefix, GetKey(AParentNode, VNamePrefix));
      AddItem(aNode, VNameSufix, Data);
    end else begin
      VNamePrefix := AName;
      if VNamePrefix = '' then begin
        VNamePrefix := '(NoName)';
      end;
      aNode := CreateNodeWithText(AParentNode, VNamePrefix, GetKey(AParentNode, VNamePrefix));
      aNode.Data := Data;
      aNode.StateIndex := 2;
      if Data.visible then begin
        aNode.StateIndex := 1
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

constructor TMarksDbGUIHelper.Create(
  AMarksDB: TMarksDB;
  AValueToStringConverterConfig: IValueToStringConverterConfig;
  AMarkPictureList: IMarkPictureList
);
begin
  FMarkPictureList := AMarkPictureList;
  FMarksDB := AMarksDB;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;

function TMarksDbGUIHelper.DeleteMarkModal(id: integer;
  handle: THandle): boolean;
var
  VMarkId: IMarkId;
begin
  Result := false;
  VMarkId := FMarksDb.MarksDb.GetMarkIdByID(id);
  if VMarkId <> nil then begin
    if MessageBox(handle,pchar(SAS_MSG_youasure+' "'+VMarkId.name+'"'),pchar(SAS_MSG_coution),36)=IDYES then begin
      result := FMarksDb.MarksDb.DeleteMark(VMarkId);
    end;
  end;
end;

function TMarksDbGUIHelper.EditMarkModal(AMark: IMarkFull): IMarkFull;
begin
  Result := nil;
  if AMark.IsPoint then begin
    result:=FaddPoint.EditMark(AMark, Self);
  end else if AMark.IsPoly then begin
    result:=FaddPoly.EditMark(AMark, Self);
  end else if AMark.IsLine then begin
    result:=FaddLine.EditMark(AMark, Self);
  end;
end;

procedure TMarksDbGUIHelper.ShowMarkLength(AID: Integer; AConverter: ICoordConverter; AHandle: THandle);
var
  i:integer;
  VPointCount: Integer;
  VMark: IMarkFull;
  VLen: Double;
  VMessage: string;
  VDatum: IDatum;
begin
  VMark := FMarksDb.MarksDb.GetMarkByID(AId);
  if VMark <> nil then begin
    VPointCount := Length(VMark.Points);
    if (VPointCount > 1) then begin
      VLen:=0;
      VDatum := AConverter.Datum;
      for i:=0 to VPointCount-2 do begin
        VLen:=VLen+ VDatum.CalcDist(VMark.Points[i], VMark.Points[i+1]);
      end;
      if VMark.IsPoly then begin
        VMessage := SAS_STR_P+' - '+
          FValueToStringConverterConfig.GetStaticConverter.DistConvert(VLen);
      end else begin
        VMessage := SAS_STR_L+' - '+
          FValueToStringConverterConfig.GetStaticConverter.DistConvert(VLen);
      end;
      MessageBox(AHandle, pchar(VMessage), pchar(VMark.name),0);
    end;
  end;
end;

//function TMarksDbGUIHelper.GetMarksIterator(ARect: TDoubleRect; AZoom: Byte; AIgnoreMarksVisible,
//  AIgnoreCategoriesVisible: Boolean): TMarksIteratorBase;
//var
//  VList: TList;
//begin
//  VList := nil;
//  if not AIgnoreCategoriesVisible then begin
//    VList := GetVisibleCateroriesIDList(AZoom);
//  end;
//  Result := FMarksDB.MarksDb.GetMarksIteratorByCategoryIdList(ARect, VList, AIgnoreMarksVisible, True);
//end;

procedure TMarksDbGUIHelper.ShowMarkSq(AID: Integer; AConverter: ICoordConverter; AHandle: THandle);
var
  VMark: IMarkFull;
  VArea: Double;
  VMessage: string;
begin
  VMark := FMarksDb.MarksDb.GetMarkByID(AId);
  if VMark <> nil then begin
    if (Length(VMark.Points) > 1) then begin
      VArea:= AConverter.Datum.CalcPoligonArea(VMark.Points);
      VMessage := SAS_STR_S+' - '+FValueToStringConverterConfig.GetStaticConverter.AreaConvert(VArea);
      MessageBox(AHandle,pchar(VMessage),pchar(VMark.name),0);
    end;
  end;
end;

function TMarksDbGUIHelper.OperationMark(AID: Integer; AZoom: Byte): boolean;
var
  VMark: IMarkFull;
begin
  Result:=false;
  VMark := FMarksDb.MarksDb.GetMarkByID(AID);
  if VMark <> nil then begin
    if VMark.IsPoly then begin
      Fsaveas.Show_(AZoom, VMark.Points);
      Result:=true;
    end else begin
      ShowMessage(SAS_MSG_FunExForPoly);
    end;
  end;
end;

function TMarksDbGUIHelper.SaveLineModal(AID: Integer;
  ANewArrLL: TArrayOfDoublePoint; ADescription: string): Boolean;
var
  VMark: IMarkFull;
begin
  Result := False;
  if AID >= 0 then begin
    VMark := FMarksDb.MarksDb.GetMarkByID(AID);
    if VMark <> nil then begin
      VMark := FMarksDB.MarksDb.Factory.CreateModifedLine(ANewArrLL, ADescription, VMark);
    end;
  end else begin
    VMark := FMarksDB.MarksDb.Factory.CreateNewLine(ANewArrLL, '', ADescription);
  end;
  if VMark <> nil then begin
    VMark := FaddLine.EditMark(VMark, Self);
    if VMark <> nil then begin
      FMarksDb.MarksDb.WriteMark(VMark);
      Result := True;
    end;
  end;
end;

function TMarksDbGUIHelper.SavePolyModal(AID: Integer;
  ANewArrLL: TArrayOfDoublePoint): Boolean;
var
  VMark: IMarkFull;
begin
  Result := False;
  if AID >= 0 then begin
    VMark := FMarksDb.MarksDb.GetMarkByID(AID);
    if VMark <> nil then begin
      VMark := FMarksDB.MarksDb.Factory.CreateModifedPoly(ANewArrLL, VMark);
    end;
  end else begin
    VMark := FMarksDB.MarksDb.Factory.CreateNewPoly(ANewArrLL, '', '');
  end;
  if VMark <> nil then begin
    VMark := FaddPoly.EditMark(VMark, Self);
    if VMark <> nil then begin
      FMarksDb.MarksDb.WriteMark(VMark);
      Result := True;
    end;
  end;
end;

end.

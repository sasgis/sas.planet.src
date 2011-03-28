unit u_MarksDbGUIHelper;

interface

uses
  Windows,
  Classes,
  ComCtrls,
  t_GeoTypes,
  i_CoordConverter,
  i_ValueToStringConverter,
  i_MarkPicture,
  i_MarksSimple,
  i_MarkCategory,
  u_MarksSimple,
  u_MarksReadWriteSimple;

type
  TMarksDbGUIHelper = class
  private
    FMarksDB: TMarksDB;
    FMarkPictureList: IMarkPictureList;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
  public
    procedure CategoryListToStrings(AList: IInterfaceList; AStrings: TStrings);
    procedure CategoryListToTree(AList: IInterfaceList; ATreeItems: TTreeNodes);
    procedure MarksListToStrings(AList: IInterfaceList; AStrings: TStrings);

    function DeleteMarkModal(AMarkID: IMarkID; handle:THandle):boolean;
    function OperationMark(AMark: IMarkFull; AZoom: Byte):boolean;
    function AddKategory(name:string): IMarkCategory;
    procedure ShowMarkLength(AMark: IMarkFull; AConverter: ICoordConverter; AHandle: THandle);
    procedure ShowMarkSq(AMark: IMarkFull; AConverter: ICoordConverter; AHandle: THandle);
    function EditMarkModal(AMark: IMarkFull): IMarkFull;
    function AddNewPointModal(ALonLat: TDoublePoint): Boolean;
    function SavePolyModal(AMark: IMarkFull; ANewArrLL: TArrayOfDoublePoint): Boolean;
    function SaveLineModal(AMark: IMarkFull; ANewArrLL: TArrayOfDoublePoint; ADescription: string): Boolean;

    property MarksDB: TMarksDB read FMarksDB;
    property MarkPictureList: IMarkPictureList read FMarkPictureList;
  public
    constructor Create(AMarksDB: TMarksDB; AValueToStringConverterConfig: IValueToStringConverterConfig; AMarkPictureList: IMarkPictureList);
  end;

implementation

uses
  SysUtils,
  Dialogs,
  i_Datum,
  UResStrings,
  USaveas,
  UaddPoint,
  UaddPoly,
  UaddLine;

{ TMarksDbGUIHelper }

function TMarksDbGUIHelper.AddKategory(name: string): IMarkCategory;
var
  VCategory: IMarkCategory;
begin
  VCategory := FMarksDB.CategoryDB.Factory.CreateNew(name);
  Result := FMarksDb.CategoryDB.GetCategoryByName(VCategory.Name);
  if Result = nil then begin
    Result := FMarksDb.CategoryDB.WriteCategory(VCategory);
  end;
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

procedure TMarksDbGUIHelper.CategoryListToStrings(AList: IInterfaceList; AStrings: TStrings);
var
  i: Integer;
  VCategory: IMarkCategory;
begin
  AStrings.Clear;
  for i := 0 to AList.Count - 1 do begin
    VCategory := IMarkCategory(AList[i]);
    AStrings.AddObject(VCategory.name, Pointer(VCategory));
  end;
end;

procedure TMarksDbGUIHelper.CategoryListToTree(AList: IInterfaceList; ATreeItems: TTreeNodes);
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
  
  procedure AddItem(AParentNode: TTreeNode; const AName: string; Data: IMarkCategory);
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
      aNode.Data := Pointer(Data);
      aNode.StateIndex := 2;
      if Data.visible then begin
        aNode.StateIndex := 1
      end;
    end;
  end;

var
  i: Integer;
  VCategory: IMarkCategory;
begin
  VNodesCached := TStringList.Create;
  try
    VNodesCached.Duplicates := dupIgnore;
    VNodesCached.Sorted := True;
    ATreeItems.Clear;
    ATreeItems.BeginUpdate;
    try
      for i := 0 to AList.Count - 1 do begin
        VCategory := IMarkCategory(AList[i]);
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

function TMarksDbGUIHelper.DeleteMarkModal(AMarkID: IMarkID;
  handle: THandle): boolean;
begin
  Result := false;
  if AMarkID <> nil then begin
    if MessageBox(handle,pchar(SAS_MSG_youasure+' "'+AMarkID.name+'"'),pchar(SAS_MSG_coution),36)=IDYES then begin
      result := FMarksDb.MarksDb.DeleteMark(AMarkID);
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
    result:=frmMarkEditPath.EditMark(AMark, Self);
  end;
end;

procedure TMarksDbGUIHelper.ShowMarkLength(AMark: IMarkFull; AConverter: ICoordConverter; AHandle: THandle);
var
  i:integer;
  VPointCount: Integer;
  VLen: Double;
  VMessage: string;
  VDatum: IDatum;
begin
  if AMark <> nil then begin
    VPointCount := Length(AMark.Points);
    if (VPointCount > 1) then begin
      VLen:=0;
      VDatum := AConverter.Datum;
      for i:=0 to VPointCount-2 do begin
        VLen:=VLen+ VDatum.CalcDist(AMark.Points[i], AMark.Points[i+1]);
      end;
      if AMark.IsPoly then begin
        VMessage := SAS_STR_P+' - '+
          FValueToStringConverterConfig.GetStaticConverter.DistConvert(VLen);
      end else begin
        VMessage := SAS_STR_L+' - '+
          FValueToStringConverterConfig.GetStaticConverter.DistConvert(VLen);
      end;
      MessageBox(AHandle, pchar(VMessage), pchar(AMark.name),0);
    end;
  end;
end;

procedure TMarksDbGUIHelper.ShowMarkSq(AMark: IMarkFull; AConverter: ICoordConverter; AHandle: THandle);
var
  VArea: Double;
  VMessage: string;
begin
  if AMark <> nil then begin
    if (Length(AMark.Points) > 1) then begin
      VArea:= AConverter.Datum.CalcPoligonArea(AMark.Points);
      VMessage := SAS_STR_S+' - '+FValueToStringConverterConfig.GetStaticConverter.AreaConvert(VArea);
      MessageBox(AHandle,pchar(VMessage),pchar(AMark.name),0);
    end;
  end;
end;

function TMarksDbGUIHelper.OperationMark(AMark: IMarkFull; AZoom: Byte): boolean;
begin
  Result:=false;
  if AMark <> nil then begin
    if AMark.IsPoly then begin
      Fsaveas.Show_(AZoom, AMark.Points);
      Result:=true;
    end else begin
      ShowMessage(SAS_MSG_FunExForPoly);
    end;
  end;
end;

function TMarksDbGUIHelper.SaveLineModal(AMark: IMarkFull;
  ANewArrLL: TArrayOfDoublePoint; ADescription: string): Boolean;
var
  VMark: IMarkFull;
begin
  Result := False;
  if AMark <> nil then begin
    VMark := FMarksDB.MarksDb.Factory.SimpleModifyLine(AMark, ANewArrLL, ADescription);
  end else begin
    VMark := FMarksDB.MarksDb.Factory.CreateNewLine(ANewArrLL, '', ADescription);
  end;
  if VMark <> nil then begin
    VMark := frmMarkEditPath.EditMark(VMark, Self);
    if VMark <> nil then begin
      FMarksDb.MarksDb.WriteMark(VMark);
      Result := True;
    end;
  end;
end;

function TMarksDbGUIHelper.SavePolyModal(AMark: IMarkFull;
  ANewArrLL: TArrayOfDoublePoint): Boolean;
var
  VMark: IMarkFull;
begin
  Result := False;
  if AMark <> nil then begin
    VMark := FMarksDB.MarksDb.Factory.SimpleModifyPoly(AMark, ANewArrLL);
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

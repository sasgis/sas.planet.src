unit u_MarksDbGUIHelper;

interface

uses
  Windows,
  Classes,
  ComCtrls,
  t_GeoTypes,
  u_MarksSimple;

type
  TMarksDbGUIHelper = class
  public
    procedure CategoryListToStrings(AList: TList; AStrings: TStrings);
    procedure CategoryListToTree(AList: TList; ATreeItems: TTreeNodes);

    function DeleteMarkModal(id:integer;handle:THandle):boolean;
    function OperationMark(AMark: TMarkFull):boolean;
    function AddKategory(name:string): integer;
    function GetMarkLength(AMark: TMarkFull):Double;
    function GetMarkSq(AMark: TMarkFull):Double;
    function EditMarkModal(AMark: TMarkFull):boolean;
    function AddNewPointModal(ALonLat: TDoublePoint): Boolean;
    function SavePolyModal(AID: Integer; ANewArrLL: TDoublePointArray): Boolean;
    function SaveLineModal(AID: Integer; ANewArrLL: TDoublePointArray; ADescription: string): Boolean;
  end;

implementation

uses
  SysUtils,
  Dialogs,
  i_ICoordConverter,
  u_GlobalState,
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
    GState.MarksDb.WriteCategory(VCategory);
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
    Result := FaddPoint.EditMark(VMark);
    if Result then begin
      GState.MarksDb.WriteMark(VMark);
    end;
  finally
    VMark.Free;
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

function TMarksDbGUIHelper.DeleteMarkModal(id: integer;
  handle: THandle): boolean;
var
  VMarkId: TMarkId;
begin
  result:=false;
  VMarkId := GState.MarksDb.GetMarkIdByID(id);
  if VMarkId <> nil then begin
    try
      if MessageBox(handle,pchar(SAS_MSG_youasure+' "'+VMarkId.name+'"'),pchar(SAS_MSG_coution),36)=IDNO then exit;
      result:=GState.MarksDb.DeleteMark(VMarkId);
    finally
      VMarkId.Free;
    end;
  end;
end;

function TMarksDbGUIHelper.EditMarkModal(AMark: TMarkFull): boolean;
begin
  Result := false;
  if AMark.IsPoint then begin
    result:=FaddPoint.EditMark(AMark);
  end else if AMark.IsPoly then begin
    result:=FaddPoly.EditMark(AMark);
  end else if AMark.IsLine then begin
    result:=FaddLine.EditMark(AMark);
  end;
end;

function TMarksDbGUIHelper.GetMarkLength(AMark: TMarkFull): Double;
var
  i:integer;
  VConverter: ICoordConverter;
  VPointCount: Integer;
begin
  Result:=0;
  VConverter := GState.ViewState.GetCurrentCoordConverter;
  VPointCount := Length(AMark.Points);
  if (VPointCount > 1) then begin
    for i:=0 to VPointCount-2 do begin
      Result:=Result+ VConverter.CalcDist(AMark.Points[i], AMark.Points[i+1]);
    end;
  end;
end;

function TMarksDbGUIHelper.GetMarkSq(AMark: TMarkFull): Double;
var
  VConverter: ICoordConverter;
begin
  Result:=0;
  VConverter := GState.ViewState.GetCurrentCoordConverter;
  if (Length(AMark.Points) > 1) then begin
    result:= VConverter.CalcPoligonArea(AMark.Points);
  end;
end;

function TMarksDbGUIHelper.OperationMark(AMark: TMarkFull): boolean;
begin
  Result:=false;
  if AMark.IsPoly then begin
    Fsaveas.Show_(GState.ViewState.GetCurrentZoom, AMark.Points);
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
    VMark := GState.MarksDb.GetMarkByID(AID)
  end;
  if VMark <> nil then begin
    try
      VMark.id := AID;
      if VMark.id < 0 then begin
        VMark.Desc := ADescription;
      end;
      VMark.Points := Copy(ANewArrLL);
      Result := FaddLine.EditMark(VMark);
      if Result then begin
        GState.MarksDb.WriteMark(VMark);
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
    VMark := GState.MarksDb.GetMarkByID(AID)
  end;
  if VMark <> nil then begin
    try
      VMark.id := AID;
      VMark.Points := Copy(ANewArrLL);
      VMark.ClosePoly;
      Result := FaddPoly.EditMark(VMark);
      if Result then begin
        GState.MarksDb.WriteMark(VMark);
      end;
    finally
      VMark.Free;
    end;
  end;
end;

end.

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
  frm_RegionProcess,
  u_MarksSystem;

type
  TMarksDbGUIHelper = class
  private
    FMarksDB: TMarksSystem;
    FMarkPictureList: IMarkPictureList;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FFormRegionProcess: TfrmRegionProcess;
  public
    procedure CategoryListToStrings(AList: IInterfaceList; AStrings: TStrings);
    procedure CategoryListToTree(AList: IInterfaceList; ATreeItems: TTreeNodes);
    procedure MarksListToStrings(AList: IInterfaceList; AStrings: TStrings);

    function DeleteMarkModal(AMarkID: IMarkID; handle:THandle):boolean;
    function OperationMark(AMark: IMark; AZoom: Byte):boolean;
    function AddKategory(name:string): IMarkCategory;
    procedure ShowMarkLength(AMark: IMarkLine; AConverter: ICoordConverter; AHandle: THandle); overload;
    procedure ShowMarkLength(AMark: IMarkPoly; AConverter: ICoordConverter; AHandle: THandle); overload;
    procedure ShowMarkSq(AMark: IMarkPoly; AConverter: ICoordConverter; AHandle: THandle);
    function EditMarkModal(AMark: IMark): IMark;
    function AddNewPointModal(ALonLat: TDoublePoint): Boolean;
    function SavePolyModal(AMark: IMarkPoly; ANewArrLL: TArrayOfDoublePoint): Boolean;
    function SaveLineModal(AMark: IMarkLine; ANewArrLL: TArrayOfDoublePoint; ADescription: string): Boolean;

    property MarksDB: TMarksSystem read FMarksDB;
    property MarkPictureList: IMarkPictureList read FMarkPictureList;
  public
    constructor Create(
      AMarksDB: TMarksSystem;
      AValueToStringConverterConfig: IValueToStringConverterConfig;
      AMarkPictureList: IMarkPictureList;
      AFormRegionProcess: TfrmRegionProcess
    );
  end;

implementation

uses
  SysUtils,
  Dialogs,
  i_Datum,
  u_ResStrings,
  frm_MarkEditPoint,
  frm_MarkEditPoly,
  frm_MarkEditPath;

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
  VMark: IMarkPoint;
begin
  Result := False;
  VMark := FMarksDB.MarksDb.Factory.CreateNewPoint(ALonLat, '', '');
  VMark := frmMarkEditPoint.EditMark(VMark, Self);
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
      aNode := FindNodeWithText(AParentNode, VNamePrefix, GetKey(AParentNode, VNamePrefix));
      if aNode.StateIndex > 0 then begin
        aNode := CreateNodeWithText(AParentNode, VNamePrefix, GetKey(AParentNode, VNamePrefix));
      end;
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
  AMarksDB: TMarksSystem;
  AValueToStringConverterConfig: IValueToStringConverterConfig;
  AMarkPictureList: IMarkPictureList;
  AFormRegionProcess: TfrmRegionProcess
);
begin
  FMarkPictureList := AMarkPictureList;
  FMarksDB := AMarksDB;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FFormRegionProcess := AFormRegionProcess;
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

function TMarksDbGUIHelper.EditMarkModal(AMark: IMark): IMark;
var
  VMarkPoint: IMarkPoint;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
begin
  Result := nil;
  if Supports(AMark, IMarkPoint, VMarkPoint) then begin
    Result := frmMarkEditPoint.EditMark(VMarkPoint, Self);
  end else if Supports(AMark, IMarkLine, VMarkLine) then begin
    Result := frmMarkEditPath.EditMark(VMarkLine, Self);
  end else if Supports(AMark, IMarkPoly, VMarkPoly) then begin
    Result := frmMarkEditPoly.EditMark(VMarkPoly, Self);
  end;
end;

procedure TMarksDbGUIHelper.ShowMarkLength(AMark: IMarkLine; AConverter: ICoordConverter; AHandle: THandle);
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
      VMessage := SAS_STR_L+' - '+
        FValueToStringConverterConfig.GetStatic.DistConvert(VLen);
      MessageBox(AHandle, pchar(VMessage), pchar(AMark.name),0);
    end;
  end;
end;

procedure TMarksDbGUIHelper.ShowMarkLength(AMark: IMarkPoly; AConverter: ICoordConverter; AHandle: THandle);
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
      VMessage := SAS_STR_P+' - '+
        FValueToStringConverterConfig.GetStatic.DistConvert(VLen);
      MessageBox(AHandle, pchar(VMessage), pchar(AMark.name),0);
    end;
  end;
end;

procedure TMarksDbGUIHelper.ShowMarkSq(AMark: IMarkPoly; AConverter: ICoordConverter; AHandle: THandle);
var
  VArea: Double;
  VMessage: string;
begin
  if AMark <> nil then begin
    if (Length(AMark.Points) > 1) then begin
      VArea:= AConverter.Datum.CalcPoligonArea(AMark.Points);
      VMessage := SAS_STR_S+' - '+FValueToStringConverterConfig.GetStatic.AreaConvert(VArea);
      MessageBox(AHandle,pchar(VMessage),pchar(AMark.name),0);
    end;
  end;
end;

function TMarksDbGUIHelper.OperationMark(AMark: IMark; AZoom: Byte): boolean;
var
  VMarkPoly: IMarkPoly;
begin
  Result:=false;
  if Supports(AMark, IMarkPoly, VMarkPoly) then begin
    FFormRegionProcess.Show_(AZoom, VMarkPoly.Points);
    Result:=true;
  end else begin
    ShowMessage(SAS_MSG_FunExForPoly);
  end;
end;

function TMarksDbGUIHelper.SaveLineModal(AMark: IMarkLine;
  ANewArrLL: TArrayOfDoublePoint; ADescription: string): Boolean;
var
  VMark: IMarkLine;
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

function TMarksDbGUIHelper.SavePolyModal(
  AMark: IMarkPoly;
  ANewArrLL: TArrayOfDoublePoint
): Boolean;
var
  VMark: IMarkPoly;
begin
  Result := False;
  if AMark <> nil then begin
    VMark := FMarksDB.MarksDb.Factory.SimpleModifyPoly(AMark, ANewArrLL);
  end else begin
    VMark := FMarksDB.MarksDb.Factory.CreateNewPoly(ANewArrLL, '', '');
  end;
  if VMark <> nil then begin
    VMark := frmMarkEditPoly.EditMark(VMark, Self);
    if VMark <> nil then begin
      FMarksDb.MarksDb.WriteMark(VMark);
      Result := True;
    end;
  end;
end;

end.

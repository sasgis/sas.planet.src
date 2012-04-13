{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MarksDbGUIHelper;

interface

uses
  Windows,
  Classes,
  t_GeoTypes,
  i_PathConfig,
  i_LanguageManager,
  i_CoordConverter,
  i_ProjectionInfo,
  i_VectorItmesFactory,
  i_ValueToStringConverter,
  i_VectorItemLonLat,
  i_ViewPortState,
  i_MarksSimple,
  i_MarkCategory,
  i_ImportConfig,
  frm_MarkCategoryEdit,
  frm_MarkEditPoint,
  frm_MarkEditPath,
  frm_MarkEditPoly,
  frm_RegionProcess,
  frm_ImportConfigEdit,
  frm_MarksMultiEdit,
  u_MarksSystem;

type
  TMarksDbGUIHelper = class
  private
    FMarksDB: TMarksSystem;
    FVectorItmesFactory: IVectorItmesFactory;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FFormRegionProcess: TfrmRegionProcess;
    FfrmMarkEditPoint: TfrmMarkEditPoint;
    FfrmMarkEditPath: TfrmMarkEditPath;
    FfrmMarkEditPoly: TfrmMarkEditPoly;
    FfrmMarkCategoryEdit: TfrmMarkCategoryEdit;
    FfrmImportConfigEdit: TfrmImportConfigEdit;
    FfrmMarksMultiEdit: TfrmMarksMultiEdit;
  public
    procedure MarksListToStrings(AList: IInterfaceList; AStrings: TStrings);

    function DeleteMarkModal(AMarkID: IMarkID; handle:THandle):boolean;
    function DeleteMarksModal(AMarkIDList: IInterfaceList; handle:THandle):boolean;
    function OperationMark(AMark: IMark; AProjection: IProjectionInfo):boolean;
    function AddKategory(name:string): IMarkCategory;
    procedure ShowMarkLength(AMark: IMarkLine; AConverter: ICoordConverter; AHandle: THandle); overload;
    procedure ShowMarkLength(AMark: IMarkPoly; AConverter: ICoordConverter; AHandle: THandle); overload;
    procedure ShowMarkSq(AMark: IMarkPoly; AConverter: ICoordConverter; AHandle: THandle);
    function EditMarkModal(AMark: IMark): IMark;
    function EditCategoryModal(ACategory: IMarkCategory): IMarkCategory;
    function AddNewPointModal(ALonLat: TDoublePoint): Boolean;
    function SavePolyModal(AMark: IMarkPoly; ALine: ILonLatPolygon): Boolean;
    function SaveLineModal(
      const AMark: IMarkLine;
      const ALine: ILonLatPath;
      const ADescription: string
    ): Boolean;
    function EditModalImportConfig: IImportConfig;
    function MarksMultiEditModal(ACategory:ICategory): IImportConfig;

    property MarksDB: TMarksSystem read FMarksDB;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AMediaPath: IPathConfig;
      AMarksDB: TMarksSystem;
      AViewPortState: IViewPortState;
      AVectorItmesFactory: IVectorItmesFactory;
      AValueToStringConverterConfig: IValueToStringConverterConfig;
      AFormRegionProcess: TfrmRegionProcess
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Dialogs,
  u_ResStrings,
  u_EnumDoublePointLine2Poly,
  u_GeoToStr;

{ TMarksDbGUIHelper }

constructor TMarksDbGUIHelper.Create(
  ALanguageManager: ILanguageManager;
  AMediaPath: IPathConfig;
  AMarksDB: TMarksSystem;
  AViewPortState: IViewPortState;
  AVectorItmesFactory: IVectorItmesFactory;
  AValueToStringConverterConfig: IValueToStringConverterConfig;
  AFormRegionProcess: TfrmRegionProcess
);
begin
  FMarksDB := AMarksDB;
  FVectorItmesFactory := AVectorItmesFactory;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FFormRegionProcess := AFormRegionProcess;
  FfrmMarkEditPoint :=
    TfrmMarkEditPoint.Create(
      ALanguageManager,
      AMediaPath,
      FMarksDB.CategoryDB,
      FMarksDB.MarksDb,
      AViewPortState,
      AValueToStringConverterConfig
    );
  FfrmMarkEditPath :=
    TfrmMarkEditPath.Create(
      ALanguageManager,
      AMediaPath,
      FMarksDB.CategoryDB,
      FMarksDB.MarksDb
    );
  FfrmMarkEditPoly :=
    TfrmMarkEditPoly.Create(
      ALanguageManager,
      AMediaPath,
      FMarksDB.CategoryDB,
      FMarksDB.MarksDb
    );
  FfrmMarkCategoryEdit :=
    TfrmMarkCategoryEdit.Create(
      ALanguageManager,
      FMarksDB.CategoryDB.Factory
    );
  FfrmImportConfigEdit :=
    TfrmImportConfigEdit.Create(
      ALanguageManager,
      FMarksDB.CategoryDB,
      FMarksDB.MarksDb
    );
  FfrmMarksMultiEdit :=
    TfrmMarksMultiEdit.Create(
      ALanguageManager,
      FMarksDB.CategoryDB,
      FMarksDB.MarksDb
    );
end;

destructor TMarksDbGUIHelper.Destroy;
begin
  FreeAndNil(FfrmMarkEditPoint);
  FreeAndNil(FfrmMarkEditPath);
  FreeAndNil(FfrmMarkEditPoly);
  FreeAndNil(FfrmMarkCategoryEdit);
  FreeAndNil(FfrmImportConfigEdit);
  FreeAndNil(FfrmMarksMultiEdit);
  inherited;
end;

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
  VMark := FfrmMarkEditPoint.EditMark(VMark);
  if VMark <> nil then begin
    FMarksDb.MarksDb.UpdateMark(nil, VMark);
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

function TMarksDbGUIHelper.DeleteMarkModal(AMarkID: IMarkID;
  handle: THandle): boolean;
begin
  Result := false;
  if AMarkID <> nil then begin
    if MessageBox(handle,pchar(SAS_MSG_youasure+' "'+AMarkID.name+'"'),pchar(SAS_MSG_coution),36)=IDYES then begin
      result := FMarksDb.MarksDb.UpdateMark(AMarkID, nil) = nil;
    end;
  end;
end;

function TMarksDbGUIHelper.DeleteMarksModal(AMarkIDList: IInterfaceList;
  handle: THandle): boolean;
var
  VMark: IMarkId;
begin
  Result := false;
  if AMarkIDList <> nil then begin
    if AMarkIDList.Count=1 then begin
      VMark:=IMarkId(AMarkIDList[0]);
      if MessageBox(handle,pchar(SAS_MSG_youasure+' "'+VMark.name+'"'),pchar(SAS_MSG_coution),36)=IDYES then begin
        result := FMarksDb.MarksDb.UpdateMark(VMark, nil) = nil;
      end;
    end else begin
      if MessageBox(handle,pchar(SAS_MSG_youasure),pchar(SAS_MSG_coution),36)=IDYES then begin
        FMarksDb.MarksDb.UpdateMarksList(AMarkIDList, nil);
        result := true;
      end;
    end;
  end;
end;

function TMarksDbGUIHelper.EditCategoryModal(
  ACategory: IMarkCategory): IMarkCategory;
begin
  Result := FfrmMarkCategoryEdit.EditCategory(ACategory);
end;

function TMarksDbGUIHelper.EditMarkModal(AMark: IMark): IMark;
var
  VMarkPoint: IMarkPoint;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
begin
  Result := nil;
  if Supports(AMark, IMarkPoint, VMarkPoint) then begin
    Result := FfrmMarkEditPoint.EditMark(VMarkPoint);
  end else if Supports(AMark, IMarkLine, VMarkLine) then begin
    Result := FfrmMarkEditPath.EditMark(VMarkLine);
  end else if Supports(AMark, IMarkPoly, VMarkPoly) then begin
    Result := FfrmMarkEditPoly.EditMark(VMarkPoly);
  end;
end;

function TMarksDbGUIHelper.EditModalImportConfig: IImportConfig;
begin
  Result := FfrmImportConfigEdit.GetImportConfig;
end;

function TMarksDbGUIHelper.MarksMultiEditModal(ACategory:ICategory): IImportConfig;
begin
  Result := FfrmMarksMultiEdit.GetImportConfig(ACategory);
end;

procedure TMarksDbGUIHelper.ShowMarkLength(AMark: IMarkLine; AConverter: ICoordConverter; AHandle: THandle);
var
  VLen: Double;
  VMessage: string;
begin
  if AMark <> nil then begin
    VLen := AMark.Line.CalcLength(AConverter.Datum);
    VMessage := SAS_STR_L+' - '+
      FValueToStringConverterConfig.GetStatic.DistConvert(VLen);
    MessageBox(AHandle, pchar(VMessage), pchar(AMark.name),0);
  end;
end;

procedure TMarksDbGUIHelper.ShowMarkLength(AMark: IMarkPoly; AConverter: ICoordConverter; AHandle: THandle);
var
  VLen: Double;
  VMessage: string;
begin
  if AMark <> nil then begin
    VLen := AMark.Line.CalcPerimeter(AConverter.Datum);
    VMessage := SAS_STR_P+' - '+
      FValueToStringConverterConfig.GetStatic.DistConvert(VLen);
    MessageBox(AHandle, pchar(VMessage), pchar(AMark.name),0);
  end;
end;

procedure TMarksDbGUIHelper.ShowMarkSq(AMark: IMarkPoly; AConverter: ICoordConverter; AHandle: THandle);
var
  VArea: Double;
  VMessage: string;
begin
  if AMark <> nil then begin
    VArea:= AMark.Line.CalcArea(AConverter.Datum);
    VMessage := SAS_STR_S+' - '+FValueToStringConverterConfig.GetStatic.AreaConvert(VArea);
    MessageBox(AHandle,pchar(VMessage),pchar(AMark.name),0);
  end;
end;

function TMarksDbGUIHelper.OperationMark(AMark: IMark; AProjection: IProjectionInfo): boolean;
var
  VMarkPoly: IMarkPoly;
  VMarkLine: IMarkLine;
  VRadius: double;
  VDefRadius: String;
  VPolygon: ILonLatPolygon;
begin
  Result:=false;
  if Supports(AMark, IMarkPoly, VMarkPoly) then begin
    FFormRegionProcess.Show_(AProjection.Zoom, VMarkPoly.Line);
    Result:=true;
  end else begin
    if Supports(AMark, IMarkLine, VMarkLine) then begin
      VDefRadius:='100';
      if InputQuery('','Radius , m', VDefRadius) then begin
        try
          VRadius:=str2r(VDefRadius);
        except
          ShowMessage(SAS_ERR_ParamsInput);
          Exit;
        end;
        VPolygon :=
          FVectorItmesFactory.CreateLonLatPolygonByLonLatPathAndFilter(
            VMarkLine.Line,
            TLonLatPointFilterLine2Poly.Create(VRadius, AProjection)
          );
        FFormRegionProcess.Show_(AProjection.Zoom, VPolygon);
        Result:=true;
      end;
    end else begin
      ShowMessage(SAS_MSG_FunExForPoly);
    end;
  end;
end;

function TMarksDbGUIHelper.SaveLineModal(
  const AMark: IMarkLine;
  const ALine: ILonLatPath;
  const ADescription: string
): Boolean;
var
  VMark: IMarkLine;
begin
  Result := False;
  if AMark <> nil then begin
    VMark := FMarksDB.MarksDb.Factory.SimpleModifyLine(AMark, ALine, ADescription);
  end else begin
    VMark := FMarksDB.MarksDb.Factory.CreateNewLine(ALine, '', ADescription);
  end;
  if VMark <> nil then begin
    VMark := FfrmMarkEditPath.EditMark(VMark);
    if VMark <> nil then begin
      FMarksDb.MarksDb.UpdateMark(AMark, VMark);
      Result := True;
    end;
  end;
end;

function TMarksDbGUIHelper.SavePolyModal(
  AMark: IMarkPoly;
  ALine: ILonLatPolygon
): Boolean;
var
  VMark: IMarkPoly;
begin
  Result := False;
  if AMark <> nil then begin
    VMark := FMarksDB.MarksDb.Factory.SimpleModifyPoly(AMark, ALine);
  end else begin
    VMark := FMarksDB.MarksDb.Factory.CreateNewPoly(ALine, '', '');
  end;
  if VMark <> nil then begin
    VMark := FfrmMarkEditPoly.EditMark(VMark);
    if VMark <> nil then begin
      FMarksDb.MarksDb.UpdateMark(AMark, VMark);
      Result := True;
    end;
  end;
end;

end.

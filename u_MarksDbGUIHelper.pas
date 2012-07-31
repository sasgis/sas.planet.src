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
  Dialogs,
  t_GeoTypes,
  i_PathConfig,
  i_LanguageManager,
  i_CoordConverter,
  i_ProjectionInfo,
  i_VectorItmesFactory,
  i_ValueToStringConverter,
  i_VectorItemLonLat,
  i_LocalCoordConverterChangeable,
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
    FExportDialog: TSaveDialog;
  public
    procedure MarksListToStrings(
      const AList: IInterfaceList;
      AStrings: TStrings
    );

    procedure DeleteMarkModal(
      const AMarkId: IMarkID;
      handle: THandle
    );
    procedure DeleteMarksModal(
      const AMarkIDList: IInterfaceList;
      handle: THandle
    );
    procedure DeleteCategoryModal(
      const ACategory: IMarkCategory;
      handle: THandle
    );
    function OperationMark(
      const AMark: IMark;
      const AProjection: IProjectionInfo
    ): boolean;
    function AddKategory(const name: string): IMarkCategory;
    procedure ShowMarkLength(
      const AMark: IMarkLine;
      const AConverter: ICoordConverter;
      AHandle: THandle
    ); overload;
    procedure ShowMarkLength(
      const AMark: IMarkPoly;
      const AConverter: ICoordConverter;
      AHandle: THandle
    ); overload;
    procedure ShowMarkSq(
      const AMark: IMarkPoly;
      const AConverter: ICoordConverter;
      AHandle: THandle
    );
    function EditMarkModal(
      const AMark: IMark;
      AIsNewMark: Boolean
    ): IMark;
    function EditCategoryModal(
      const ACategory: IMarkCategory;
      AIsNewMark: Boolean
    ): IMarkCategory;
    function AddNewPointModal(const ALonLat: TDoublePoint): Boolean;
    function SavePointModal(
      const AMark: IMarkPoint;
      const ALonLat: TDoublePoint
    ): Boolean;
    function SavePolyModal(
      const AMark: IMarkPoly;
      const ALine: ILonLatPolygon;
      AAsNewMark: Boolean = false
    ): Boolean;
    function SaveLineModal(
      const AMark: IMarkLine;
      const ALine: ILonLatPath;
      const ADescription: string;
      AAsNewMark: Boolean = false
    ): Boolean;
    function EditModalImportConfig: IImportConfig;
    function MarksMultiEditModal(const ACategory: ICategory): IImportConfig;
    procedure ExportMark(const AMark: IMark);
    procedure ExportCategory(
      const AMarkCategory: IMarkCategory;
      AIgnoreMarksVisible: Boolean
    );
    procedure ExportCategoryList(
      ACategoryList: IInterfaceList;
      AIgnoreMarksVisible: Boolean
    );

    property MarksDB: TMarksSystem read FMarksDB;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMediaPath: IPathConfig;
      AMarksDB: TMarksSystem;
      const AViewPortState: ILocalCoordConverterChangeable;
      const AVectorItmesFactory: IVectorItmesFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      AFormRegionProcess: TfrmRegionProcess
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  gnugettext,
  u_ResStrings,
  u_EnumDoublePointLine2Poly,
  u_ExportMarks2KML,
  u_GeoFun,
  u_GeoToStr;

{ TMarksDbGUIHelper }

constructor TMarksDbGUIHelper.Create(
  const ALanguageManager: ILanguageManager;
  const AMediaPath: IPathConfig;
  AMarksDB: TMarksSystem;
  const AViewPortState: ILocalCoordConverterChangeable;
  const AVectorItmesFactory: IVectorItmesFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  AFormRegionProcess: TfrmRegionProcess
);
begin
  inherited Create;
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
      FMarksDB.CategoryDB
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
  FExportDialog := TSaveDialog.Create(nil);

  //ExportDialog
  FExportDialog.Name := 'ExportDialog';
  FExportDialog.DefaultExt := '.kmz';
  FExportDialog.Filter := _('Compressed Keyhole Markup Language (kmz)|*.kmz|Keyhole Markup Language (kml)|*.kml');
  FExportDialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing];
end;

destructor TMarksDbGUIHelper.Destroy;
begin
  FreeAndNil(FfrmMarkEditPoint);
  FreeAndNil(FfrmMarkEditPath);
  FreeAndNil(FfrmMarkEditPoly);
  FreeAndNil(FfrmMarkCategoryEdit);
  FreeAndNil(FfrmImportConfigEdit);
  FreeAndNil(FfrmMarksMultiEdit);
  FreeAndNil(FExportDialog);
  inherited;
end;

function TMarksDbGUIHelper.AddKategory(const name: string): IMarkCategory;
var
  VCategory: IMarkCategory;
begin
  VCategory := FMarksDB.CategoryDB.Factory.CreateNew(name);
  Result := FMarksDb.CategoryDB.GetCategoryByName(VCategory.Name);
  if Result = nil then begin
    Result := FMarksDb.CategoryDB.UpdateCategory(nil, VCategory);
  end;
end;

function TMarksDbGUIHelper.AddNewPointModal(const ALonLat: TDoublePoint): Boolean;
var
  VMark: IMarkPoint;
begin
  Result := False;
  VMark := FMarksDB.MarksDb.Factory.CreateNewPoint(ALonLat, '', '');
  VMark := FfrmMarkEditPoint.EditMark(VMark, True);
  if VMark <> nil then begin
    FMarksDb.MarksDb.UpdateMark(nil, VMark);
    Result := True;
  end;
end;

procedure TMarksDbGUIHelper.MarksListToStrings(
  const AList: IInterfaceList;
  AStrings: TStrings
);
var
  i: Integer;
  VMarkId: IMarkId;
  VPointCaptionFormat: string;
  VPolygonCaptionFormat: string;
  VPathCaptionFormat: string;
  VFormat: string;
begin
  VPointCaptionFormat := SAS_STR_ExtendedPointCaption;
  VPolygonCaptionFormat := SAS_STR_ExtendedPolygonCaption;
  VPathCaptionFormat := SAS_STR_ExtendedPathCaption;
  AStrings.Clear;
  for i := 0 to AList.Count - 1 do begin
    VMarkId := IMarkId(AList[i]);
    if IsEqualGUID(VMarkId.MarkType, IMarkPoint) then begin
      VFormat := VPointCaptionFormat;
    end else if IsEqualGUID(VMarkId.MarkType, IMarkLine) then begin
      VFormat := VPathCaptionFormat;
    end else if IsEqualGUID(VMarkId.MarkType, IMarkPoly) then begin
      VFormat := VPolygonCaptionFormat;
    end else begin
      VFormat := '%0:s';
    end;
    AStrings.AddObject(Format(VFormat, [VMarkId.name]), Pointer(VMarkId));
  end;
end;

procedure TMarksDbGUIHelper.DeleteCategoryModal(
  const ACategory: IMarkCategory;
  handle: THandle
);
var
  VMessage: string;
begin
  if ACategory <> nil then begin
    VMessage := Format(SAS_MSG_DeleteMarkCategoryAsk, [ACategory.Name]);
    if MessageBox(handle, pchar(VMessage), pchar(SAS_MSG_coution), 36) = IDYES then begin
      FMarksDb.DeleteCategoryWithMarks(ACategory);
    end;
  end;
end;

procedure TMarksDbGUIHelper.DeleteMarkModal(
  const AMarkId: IMarkID;
  handle: THandle
);
var
  VMark: IMark;
  VMessage: string;
begin
  if AMarkId <> nil then begin
    VMark := FMarksDB.MarksDb.GetMarkByID(AMarkId);
    if VMark <> nil then begin
      if Supports(VMark, IMarkPoint) then begin
        VMessage := SAS_MSG_DeleteMarkPointAsk;
      end else if Supports(VMark, IMarkLine) then begin
        VMessage := SAS_MSG_DeleteMarkPathAsk;
      end else if Supports(VMark, IMarkPoly) then begin
        VMessage := SAS_MSG_DeleteMarkPolyAsk;
      end;
      VMessage := Format(VMessage, [AMarkId.name]);
      if MessageBox(handle, pchar(VMessage), pchar(SAS_MSG_coution), 36) = IDYES then begin
        FMarksDb.MarksDb.UpdateMark(AMarkId, nil);
      end;
    end;
  end;
end;

procedure TMarksDbGUIHelper.DeleteMarksModal(
  const AMarkIDList: IInterfaceList;
  handle: THandle
);
var
  VMark: IMarkId;
  VMessage: string;
begin
  if (AMarkIDList <> nil) and (AMarkIDList.Count > 0) then begin
    if AMarkIDList.Count = 1 then begin
      VMark := IMarkId(AMarkIDList[0]);
      DeleteMarkModal(VMark, handle);
    end else begin
      VMessage := Format(SAS_MSG_DeleteManyMarksAsk, [AMarkIDList.Count]);
      if MessageBox(handle, pchar(VMessage), pchar(SAS_MSG_coution), 36) = IDYES then begin
        FMarksDb.MarksDb.UpdateMarksList(AMarkIDList, nil);
      end;
    end;
  end;
end;

function TMarksDbGUIHelper.EditCategoryModal(
  const ACategory: IMarkCategory;
  AIsNewMark: Boolean
): IMarkCategory;
begin
  Result := FfrmMarkCategoryEdit.EditCategory(ACategory, AIsNewMark);
end;

function TMarksDbGUIHelper.EditMarkModal(
  const AMark: IMark;
  AIsNewMark: Boolean
): IMark;
var
  VMarkPoint: IMarkPoint;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
begin
  Result := nil;
  if Supports(AMark, IMarkPoint, VMarkPoint) then begin
    Result := FfrmMarkEditPoint.EditMark(VMarkPoint, AIsNewMark);
  end else if Supports(AMark, IMarkLine, VMarkLine) then begin
    Result := FfrmMarkEditPath.EditMark(VMarkLine, AIsNewMark);
  end else if Supports(AMark, IMarkPoly, VMarkPoly) then begin
    Result := FfrmMarkEditPoly.EditMark(VMarkPoly, AIsNewMark);
  end;
end;

function TMarksDbGUIHelper.EditModalImportConfig: IImportConfig;
begin
  Result := FfrmImportConfigEdit.GetImportConfig;
end;

procedure TMarksDbGUIHelper.ExportCategory(
  const AMarkCategory: IMarkCategory;
  AIgnoreMarksVisible: Boolean
);
var
  KMLExport: TExportMarks2KML;
  VMarksSubset: IMarksSubset;
  VFileName: string;
begin
  if AMarkCategory <> nil then begin
    FExportDialog.FileName := StringReplace(AMarkCategory.name, '\', '-', [rfReplaceAll]);
    if FExportDialog.Execute then begin
      VFileName := FExportDialog.FileName;
      if VFileName <> '' then begin
        KMLExport := TExportMarks2KML.Create;
        try
          VMarksSubset :=
            FMarksDb.MarksDb.GetMarksSubset(
              DoubleRect(-180, 90, 180, -90),
              AMarkCategory,
              AIgnoreMarksVisible
            );
          KMLExport.ExportCategoryToKML(AMarkCategory, VMarksSubset, VFileName);
        finally
          KMLExport.free;
        end;
      end;
    end;
  end;
end;

procedure TMarksDbGUIHelper.ExportCategoryList(
  ACategoryList: IInterfaceList;
  AIgnoreMarksVisible: Boolean
);
var
  KMLExport: TExportMarks2KML;
  VMarksSubset: IMarksSubset;
  VFileName: string;
begin
  if (ACategoryList <> nil) and (ACategoryList.Count > 0) then begin
    if FExportDialog.Execute then begin
      VFileName := FExportDialog.FileName;
      if VFileName <> '' then begin
        KMLExport := TExportMarks2KML.Create;
        try
          VMarksSubset :=
            FMarksDb.MarksDb.GetMarksSubset(
              DoubleRect(-180, 90, 180, -90),
              ACategoryList,
              AIgnoreMarksVisible
            );
          KMLExport.ExportToKML(ACategoryList, VMarksSubset, VFileName);
        finally
          KMLExport.free;
        end;
      end;
    end;
  end;
end;

procedure TMarksDbGUIHelper.ExportMark(const AMark: IMark);
var
  KMLExport: TExportMarks2KML;
  VFileName: string;
begin
  if AMark <> nil then begin
    FExportDialog.FileName := StringReplace(AMark.Name, '\', '-', [rfReplaceAll]);
    if FExportDialog.Execute then begin
      VFileName := FExportDialog.FileName;
      if VFileName <> '' then begin
        KMLExport := TExportMarks2KML.Create;
        try
          KMLExport.ExportMarkToKML(AMark, VFileName);
        finally
          KMLExport.free;
        end;
      end;
    end;
  end;
end;

function TMarksDbGUIHelper.MarksMultiEditModal(const ACategory: ICategory): IImportConfig;
begin
  Result := FfrmMarksMultiEdit.GetImportConfig(ACategory);
end;

procedure TMarksDbGUIHelper.ShowMarkLength(
  const AMark: IMarkLine;
  const AConverter: ICoordConverter;
  AHandle: THandle
);
var
  VLen: Double;
  VMessage: string;
begin
  if AMark <> nil then begin
    VLen := AMark.Line.CalcLength(AConverter.Datum);
    VMessage := SAS_STR_L + ' - ' +
      FValueToStringConverterConfig.GetStatic.DistConvert(VLen);
    MessageBox(AHandle, pchar(VMessage), pchar(AMark.name), 0);
  end;
end;

procedure TMarksDbGUIHelper.ShowMarkLength(
  const AMark: IMarkPoly;
  const AConverter: ICoordConverter;
  AHandle: THandle
);
var
  VLen: Double;
  VMessage: string;
begin
  if AMark <> nil then begin
    VLen := AMark.Line.CalcPerimeter(AConverter.Datum);
    VMessage := SAS_STR_P + ' - ' +
      FValueToStringConverterConfig.GetStatic.DistConvert(VLen);
    MessageBox(AHandle, pchar(VMessage), pchar(AMark.name), 0);
  end;
end;

procedure TMarksDbGUIHelper.ShowMarkSq(
  const AMark: IMarkPoly;
  const AConverter: ICoordConverter;
  AHandle: THandle
);
var
  VArea: Double;
  VMessage: string;
begin
  if AMark <> nil then begin
    VArea := AMark.Line.CalcArea(AConverter.Datum);
    VMessage := SAS_STR_S + ' - ' + FValueToStringConverterConfig.GetStatic.AreaConvert(VArea);
    MessageBox(AHandle, pchar(VMessage), pchar(AMark.name), 0);
  end;
end;

function TMarksDbGUIHelper.OperationMark(
  const AMark: IMark;
  const AProjection: IProjectionInfo
): boolean;
var
  VMarkPoly: IMarkPoly;
  VMarkLine: IMarkLine;
  VRadius: double;
  VDefRadius: String;
  VPolygon: ILonLatPolygon;
begin
  Result := false;
  if Supports(AMark, IMarkPoly, VMarkPoly) then begin
    FFormRegionProcess.Show_(AProjection.Zoom, VMarkPoly.Line);
    Result := true;
  end else begin
    if Supports(AMark, IMarkLine, VMarkLine) then begin
      VDefRadius := '100';
      if InputQuery('', 'Radius , m', VDefRadius) then begin
        try
          VRadius := str2r(VDefRadius);
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
        Result := true;
      end;
    end else begin
      ShowMessage(SAS_MSG_FunExForPoly);
    end;
  end;
end;

function TMarksDbGUIHelper.SaveLineModal(
  const AMark: IMarkLine;
  const ALine: ILonLatPath;
  const ADescription: string;
  AAsNewMark: Boolean
): Boolean;
var
  VMark: IMarkLine;
  VSourceMark: IMarkLine;
  VNewMark: Boolean;
begin
  Result := False;
  if AMark <> nil then begin
    VMark := FMarksDB.MarksDb.Factory.SimpleModifyLine(AMark, ALine, ADescription);
    VNewMark := AAsNewMark;
  end else begin
    VMark := FMarksDB.MarksDb.Factory.CreateNewLine(ALine, '', ADescription);
    VNewMark := True;
  end;
  if VMark <> nil then begin
    VMark := FfrmMarkEditPath.EditMark(VMark, VNewMark);
    if VMark <> nil then begin
      if AAsNewMark then begin
        VSourceMark := nil;
      end else begin
        VSourceMark := AMark;
      end;
      FMarksDb.MarksDb.UpdateMark(VSourceMark, VMark);
      Result := True;
    end;
  end;
end;

function TMarksDbGUIHelper.SavePointModal(
  const AMark: IMarkPoint;
  const ALonLat: TDoublePoint
): Boolean;
var
  VMark: IMarkPoint;
  VNewMark: Boolean;
begin
  Result := False;
  if AMark <> nil then begin
    VMark := FMarksDB.MarksDb.Factory.SimpleModifyPoint(AMark, ALonLat);
    VNewMark := False;
  end else begin
    VMark := FMarksDB.MarksDb.Factory.CreateNewPoint(ALonLat, '', '');
    VNewMark := True;
  end;
  if VMark <> nil then begin
    VMark := FfrmMarkEditPoint.EditMark(VMark, VNewMark);
    if VMark <> nil then begin
      FMarksDb.MarksDb.UpdateMark(AMark, VMark);
      Result := True;
    end;
  end;
end;

function TMarksDbGUIHelper.SavePolyModal(
  const AMark: IMarkPoly;
  const ALine: ILonLatPolygon;
  AAsNewMark: Boolean
): Boolean;
var
  VMark: IMarkPoly;
  VSourceMark: IMarkPoly;
  VNewMark: Boolean;
begin
  Result := False;
  if AMark <> nil then begin
    VMark := FMarksDB.MarksDb.Factory.SimpleModifyPoly(AMark, ALine);
    VNewMark := AAsNewMark;
  end else begin
    VMark := FMarksDB.MarksDb.Factory.CreateNewPoly(ALine, '', '');
    VNewMark := True;
  end;
  if VMark <> nil then begin
    VMark := FfrmMarkEditPoly.EditMark(VMark, VNewMark);
    if VMark <> nil then begin
      if AAsNewMark then begin
        VSourceMark := nil;
      end else begin
        VSourceMark := AMark;
      end;
      FMarksDb.MarksDb.UpdateMark(VSourceMark, VMark);
      Result := True;
    end;
  end;
end;

end.

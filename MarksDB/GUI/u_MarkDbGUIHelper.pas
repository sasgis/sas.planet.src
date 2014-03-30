{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_MarkDbGUIHelper;

interface

uses
  Windows,
  Dialogs,
  Classes,
  t_GeoTypes,
  i_PathConfig,
  i_LanguageManager,
  i_InterfaceListStatic,
  i_ProjectionInfo,
  i_GeometryLonLatFactory,
  i_ArchiveReadWriteFactory,
  i_ValueToStringConverter,
  i_GeometryLonLat,
  i_LocalCoordConverterChangeable,
  i_VectorDataItemSimple,
  i_VectorItemSubsetBuilder,
  i_MarkTemplate,
  i_MarkId,
  i_Category,
  i_MarkCategory,
  i_MarkSystem,
  i_ImportConfig,
  i_VectorItemTreeExporterList,
  i_VectorItemTreeImporterList,
  i_CoordConverterFactory,
  i_AppearanceOfMarkFactory,
  i_MarkFactory,
  i_MarkFactoryConfig,
  i_MarkPicture,
  frm_MarkCategoryEdit,
  frm_MarkEditPoint,
  frm_MarkEditPath,
  frm_MarkEditPoly,
  frm_MarkInfo,
  frm_ImportConfigEdit,
  frm_MarksMultiEdit;

type
  TMarkDbGUIHelper = class
  private
    FMarkSystem: IMarkSystem;
    FMarkFactoryConfig: IMarkFactoryConfig;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FValueToStringConverterConfig: IValueToStringConverterChangeable;
    FfrmMarkEditPoint: TfrmMarkEditPoint;
    FfrmMarkEditPath: TfrmMarkEditPath;
    FfrmMarkEditPoly: TfrmMarkEditPoly;
    FfrmMarkCategoryEdit: TfrmMarkCategoryEdit;
    FfrmImportConfigEdit: TfrmImportConfigEdit;
    FfrmMarksMultiEdit: TfrmMarksMultiEdit;
    FfrmMarkInfo: TfrmMarkInfo;
    FExportDialog: TSaveDialog;
    FImportDialog: TOpenDialog;
    FExporterList: IVectorItemTreeExporterListChangeable;
    FImporterList: IVectorItemTreeImporterListChangeable;
    procedure PrepareExportDialog(const AExporterList: IVectorItemTreeExporterListStatic);
    function GetActiveExporter(const AExporterList: IVectorItemTreeExporterListStatic): IVectorItemTreeExporterListItem;

    procedure PrepareImportDialog(const AImporterList: IVectorItemTreeImporterListStatic);
    function ImportFilesModalInternal(
      AFiles: TStrings;
      const AImporterList: IVectorItemTreeImporterListStatic
    ): IInterfaceListStatic;
  public
    function GetMarkIdCaption(const AMarkId: IMarkId): string;

    function DeleteMarkModal(
      const AMarkId: IMarkId;
      handle: THandle
    ): Boolean;
    function DeleteMarksModal(
      const AMarkIDList: IInterfaceListStatic;
      handle: THandle
    ): Boolean;
    function DeleteCategoryModal(
      const ACategory: IMarkCategory;
      handle: THandle
    ): Boolean;
    function PolygonForOperation(
      const AGeometry: IGeometryLonLat;
      const AProjection: IProjectionInfo
    ): IGeometryLonLatMultiPolygon;
    function AddKategory(const Name: string): IMarkCategory;
    procedure ShowMarkInfo(
      const AMark: IVectorDataItemSimple
    );
    function EditMarkModal(
      const AMark: IVectorDataItemSimple;
      const AIsNewMark: Boolean;
      var AVisible: Boolean
    ): IVectorDataItemSimple;
    function EditCategoryModal(
      const ACategory: IMarkCategory;
      AIsNewMark: Boolean
    ): IMarkCategory;
    function AddNewPointModal(
      const ALonLat: TDoublePoint;
      const ATemplate: IMarkTemplatePoint = nil
    ): Boolean;
    function SavePointModal(
      const AMark: IVectorDataItemPoint;
      const ALonLat: TDoublePoint
    ): Boolean;
    function SavePolyModal(
      const AMark: IVectorDataItemPoly;
      const ALine: IGeometryLonLatMultiPolygon;
      AAsNewMark: Boolean = false
    ): Boolean;
    function SaveLineModal(
      const AMark: IVectorDataItemLine;
      const ALine: IGeometryLonLatMultiLine;
      const ADescription: string;
      AAsNewMark: Boolean = false
    ): Boolean;
    function EditModalImportConfig: IImportConfig;
    function MarksMultiEditModal(const ACategory: ICategory): IImportConfig;
    procedure ExportMark(const AMark: IVectorDataItemSimple);
    procedure ExportCategory(
      const AMarkCategory: IMarkCategory;
      AIgnoreMarksVisible: Boolean
    );
    procedure ExportCategoryList(
      const ACategoryList: IInterfaceListStatic;
      AIgnoreMarksVisible: Boolean
    );
    function ImportFilesModal(
      AFiles: TStrings
    ): IInterfaceListStatic;
    function ImportModal(ParentWnd: HWND): IInterfaceListStatic;

    property MarksDb: IMarkSystem read FMarkSystem;
    property MarkFactoryConfig: IMarkFactoryConfig read FMarkFactoryConfig;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMediaPath: IPathConfig;
      const AMarkFactoryConfig: IMarkFactoryConfig;
      const AMarkPictureList: IMarkPictureList;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkSystem: IMarkSystem;
      const ADatumFactory: IDatumFactory;
      const AExporterList: IVectorItemTreeExporterListChangeable;
      const AImporterList: IVectorItemTreeImporterListChangeable;
      const AViewPortState: ILocalCoordConverterChangeable;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AValueToStringConverter: IValueToStringConverterChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  gnugettext,
  c_CoordConverter,
  i_DoublePointFilter,
  i_VectorItemTree,
  i_InterfaceListSimple,
  i_StaticTreeItem,
  i_VectorItemTreeImporter,
  u_ResStrings,
  u_EnumDoublePointLine2Poly,
  u_VectorItemTree,
  u_InterfaceListSimple,
  u_FileNameFunc,
  u_GeoToStrFunc;

{ TMarksDbGUIHelper }

constructor TMarkDbGUIHelper.Create(
  const ALanguageManager: ILanguageManager;
  const AMediaPath: IPathConfig;
  const AMarkFactoryConfig: IMarkFactoryConfig;
  const AMarkPictureList: IMarkPictureList;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkSystem: IMarkSystem;
  const ADatumFactory: IDatumFactory;
  const AExporterList: IVectorItemTreeExporterListChangeable;
  const AImporterList: IVectorItemTreeImporterListChangeable;
  const AViewPortState: ILocalCoordConverterChangeable;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AValueToStringConverter: IValueToStringConverterChangeable
);
begin
  inherited Create;
  FMarkSystem := AMarkSystem;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FMarkFactoryConfig := AMarkFactoryConfig;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FValueToStringConverterConfig := AValueToStringConverter;
  FImporterList := AImporterList;
  FExporterList := AExporterList;

  FfrmMarkEditPoint :=
    TfrmMarkEditPoint.Create(
      ALanguageManager,
      AMediaPath,
      AVectorGeometryLonLatFactory,
      AAppearanceOfMarkFactory,
      FMarkSystem.MarkDb.Factory,
      FMarkSystem.CategoryDB,
      FMarkSystem.MarkDb.Factory.MarkPictureList,
      AViewPortState,
      AValueToStringConverter
    );
  FfrmMarkEditPath :=
    TfrmMarkEditPath.Create(
      ALanguageManager,
      AMediaPath,
      AAppearanceOfMarkFactory,
      FMarkSystem.MarkDb.Factory,
      FMarkSystem.CategoryDB
    );
  FfrmMarkEditPoly :=
    TfrmMarkEditPoly.Create(
      ALanguageManager,
      AMediaPath,
      AAppearanceOfMarkFactory,
      FMarkSystem.MarkDb.Factory,
      FMarkSystem.CategoryDB
    );
  FfrmMarkCategoryEdit :=
    TfrmMarkCategoryEdit.Create(
      ALanguageManager,
      FMarkSystem.CategoryDB
    );
  FfrmImportConfigEdit :=
    TfrmImportConfigEdit.Create(
      ALanguageManager,
      AAppearanceOfMarkFactory,
      FMarkSystem.MarkDb.Factory,
      FMarkSystem.CategoryDB
    );
  FfrmMarkInfo :=
    TfrmMarkInfo.Create(
      ALanguageManager,
      AValueToStringConverter,
      ADatumFactory.GetByCode(CYandexDatumEPSG)
    );
  FfrmMarksMultiEdit :=
    TfrmMarksMultiEdit.Create(
      ALanguageManager,
      AAppearanceOfMarkFactory,
      FMarkSystem.MarkDb.Factory,
      FMarkSystem.CategoryDB
    );

  FExportDialog := TSaveDialog.Create(nil);
  FExportDialog.Name := 'ExportDialog';
  FExportDialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing];

  FImportDialog := TOpenDialog.Create(nil);
  FImportDialog.Name := 'ImportDialog';
  FImportDialog.Options := [ofAllowMultiSelect, ofEnableSizing];
end;

destructor TMarkDbGUIHelper.Destroy;
begin
  FreeAndNil(FfrmMarkEditPoint);
  FreeAndNil(FfrmMarkEditPath);
  FreeAndNil(FfrmMarkEditPoly);
  FreeAndNil(FfrmMarkCategoryEdit);
  FreeAndNil(FfrmImportConfigEdit);
  FreeAndNil(FfrmMarksMultiEdit);
  FreeAndNil(FfrmMarkInfo);
  FreeAndNil(FExportDialog);
  FreeAndNil(FImportDialog);
  inherited;
end;

function TMarkDbGUIHelper.AddKategory(const Name: string): IMarkCategory;
var
  VCategory: IMarkCategory;
begin
  VCategory := FMarkSystem.CategoryDB.Factory.CreateNew(Name);
  Result := FMarkSystem.CategoryDB.GetCategoryByName(VCategory.Name);
  if Result = nil then begin
    Result := FMarkSystem.CategoryDB.UpdateCategory(nil, VCategory);
  end;
end;

function TMarkDbGUIHelper.AddNewPointModal(
  const ALonLat: TDoublePoint;
  const ATemplate: IMarkTemplatePoint = nil
): Boolean;
var
  VMark: IVectorDataItemPoint;
  VVisible: Boolean;
  VResult: IVectorDataItemSimple;
begin
  Result := False;
  VVisible := True;
  VMark :=
    FMarkSystem.MarkDb.Factory.CreateNewPoint(
      FVectorGeometryLonLatFactory.CreateLonLatPoint(ALonLat),
      '',
      '',
      ATemplate
    );
  VMark := FfrmMarkEditPoint.EditMark(VMark, True, VVisible);
  if VMark <> nil then begin
    VResult := FMarkSystem.MarkDb.UpdateMark(nil, VMark);
    if VResult <> nil then begin
      FMarkSystem.MarkDb.SetMarkVisible(VMark, VVisible);
      Result := True;
    end;
  end;
end;

function TMarkDbGUIHelper.DeleteCategoryModal(
  const ACategory: IMarkCategory;
  handle: THandle
): Boolean;
var
  VMessage: string;
begin
  Result := False;
  if ACategory <> nil then begin
    VMessage := Format(SAS_MSG_DeleteMarkCategoryAsk, [ACategory.Name]);
    if MessageBox(handle, PChar(VMessage), PChar(SAS_MSG_coution), 36) = IDYES then begin
      FMarkSystem.DeleteCategoryWithMarks(ACategory);
      Result := True;
    end;
  end;
end;

function TMarkDbGUIHelper.DeleteMarkModal(
  const AMarkId: IMarkId;
  handle: THandle
): Boolean;
var
  VMark: IVectorDataItemSimple;
  VMessage: string;
begin
  Result := False;
  if AMarkId <> nil then begin
    VMark := FMarkSystem.MarkDb.GetMarkByID(AMarkId);
    if VMark <> nil then begin
      if Supports(VMark, IVectorDataItemPoint) then begin
        VMessage := SAS_MSG_DeleteMarkPointAsk;
      end else if Supports(VMark, IVectorDataItemLine) then begin
        VMessage := SAS_MSG_DeleteMarkPathAsk;
      end else if Supports(VMark, IVectorDataItemPoly) then begin
        VMessage := SAS_MSG_DeleteMarkPolyAsk;
      end;
      VMessage := Format(VMessage, [AMarkId.Name]);
      if MessageBox(handle, PChar(VMessage), PChar(SAS_MSG_coution), 36) = IDYES then begin
        FMarkSystem.MarkDb.UpdateMark(VMark, nil);
        Result := True;
      end;
    end;
  end;
end;

function TMarkDbGUIHelper.DeleteMarksModal(
  const AMarkIDList: IInterfaceListStatic;
  handle: THandle
): Boolean;
var
  VMark: IMarkId;
  VMessage: string;
begin
  Result := False;
  if (AMarkIDList <> nil) and (AMarkIDList.Count > 0) then begin
    if AMarkIDList.Count = 1 then begin
      VMark := IMarkId(AMarkIDList[0]);
      Result := DeleteMarkModal(VMark, handle);
    end else begin
      VMessage := Format(SAS_MSG_DeleteManyMarksAsk, [AMarkIDList.Count]);
      if MessageBox(handle, PChar(VMessage), PChar(SAS_MSG_coution), 36) = IDYES then begin
        FMarkSystem.MarkDb.UpdateMarkList(AMarkIDList, nil);
        Result := True;
      end;
    end;
  end;
end;

function TMarkDbGUIHelper.EditCategoryModal(
  const ACategory: IMarkCategory;
  AIsNewMark: Boolean
): IMarkCategory;
begin
  Result := FfrmMarkCategoryEdit.EditCategory(ACategory, AIsNewMark);
end;

function TMarkDbGUIHelper.EditMarkModal(
  const AMark: IVectorDataItemSimple;
  const AIsNewMark: Boolean;
  var AVisible: Boolean
): IVectorDataItemSimple;
var
  VMarkPoint: IVectorDataItemPoint;
  VMarkLine: IVectorDataItemLine;
  VMarkPoly: IVectorDataItemPoly;
begin
  Result := nil;
  if Supports(AMark, IVectorDataItemPoint, VMarkPoint) then begin
    Result := FfrmMarkEditPoint.EditMark(VMarkPoint, AIsNewMark, AVisible);
  end else if Supports(AMark, IVectorDataItemLine, VMarkLine) then begin
    Result := FfrmMarkEditPath.EditMark(VMarkLine, AIsNewMark, AVisible);
  end else if Supports(AMark, IVectorDataItemPoly, VMarkPoly) then begin
    Result := FfrmMarkEditPoly.EditMark(VMarkPoly, AIsNewMark, AVisible);
  end;
end;

function TMarkDbGUIHelper.EditModalImportConfig: IImportConfig;
begin
  Result := FfrmImportConfigEdit.GetImportConfig;
end;

function TMarkDbGUIHelper.GetActiveExporter(
  const AExporterList: IVectorItemTreeExporterListStatic
): IVectorItemTreeExporterListItem;
var
  VIndex: Integer;
begin
  Result := nil;
  VIndex := FExportDialog.FilterIndex - 1;
  if (VIndex >= 0) and (VIndex < AExporterList.Count) then begin
    Result := AExporterList.Items[VIndex];
  end;
end;

procedure TMarkDbGUIHelper.PrepareExportDialog(
  const AExporterList: IVectorItemTreeExporterListStatic);
var
  VSelectedFilter: Integer;
  VFilterStr: string;
  i: Integer;
  VItem: IVectorItemTreeExporterListItem;
begin
  VSelectedFilter := FExportDialog.FilterIndex;
  VFilterStr := '';
  for i := 0 to AExporterList.Count - 1 do begin
    VItem := AExporterList.Items[i];
    if i = 0 then begin
      FExportDialog.DefaultExt := VItem.DefaultExt;
    end else begin
      VFilterStr := VFilterStr + '|';
    end;
    VFilterStr := VFilterStr + VItem.Name + ' (*.'+ VItem.DefaultExt + ')|*.' + VItem.DefaultExt;
  end;
  FExportDialog.Filter := VFilterStr;
  FExportDialog.FilterIndex := VSelectedFilter;
end;

procedure TMarkDbGUIHelper.ExportCategory(
  const AMarkCategory: IMarkCategory;
  AIgnoreMarksVisible: Boolean
);
var
  VFileName: string;
  VSubCategoryList: IInterfaceListStatic;
  VCategoryTree: IStaticTreeItem;
  VMarkTree: IVectorItemTree;
  VList: IInterfaceListSimple;
  VExporterList: IVectorItemTreeExporterListStatic;
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  if AMarkCategory <> nil then begin
    VFileName := PrepareFileName(AMarkCategory.Name);
    VExporterList := FExporterList.GetStatic;
    PrepareExportDialog(VExporterList);
    FExportDialog.FileName := VFileName;
    if FExportDialog.Execute then begin
      VFileName := FExportDialog.FileName;
      if VFileName <> '' then begin
        VExporterItem := GetActiveExporter(VExporterList);
        if Assigned(VExporterItem) then begin
          VSubCategoryList := FMarkSystem.CategoryDB.GetSubCategoryListForCategory(AMarkCategory);
          if not AIgnoreMarksVisible then begin
            VSubCategoryList := FMarkSystem.CategoryDB.FilterVisibleCategories(VSubCategoryList);
          end;
          VList := TInterfaceListSimple.Create;
          VList.Add(AMarkCategory);
          VList.AddListStatic(VSubCategoryList);
          VCategoryTree := FMarkSystem.CategoryDB.CategoryListToStaticTree(VList.MakeStaticAndClear);
          VMarkTree := FMarkSystem.CategoryTreeToMarkTree(VCategoryTree, AIgnoreMarksVisible);
          VExporterItem.Exporter.ProcessExport(VFileName, VMarkTree);
        end;
      end;
    end;
  end;
end;

procedure TMarkDbGUIHelper.ExportCategoryList(
  const ACategoryList: IInterfaceListStatic;
  AIgnoreMarksVisible: Boolean
);
var
  VFileName: string;
  VCategoryTree: IStaticTreeItem;
  VMarkTree: IVectorItemTree;
  VExporterList: IVectorItemTreeExporterListStatic;
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  if (ACategoryList <> nil) and (ACategoryList.Count > 0) then begin
    VExporterList := FExporterList.GetStatic;
    PrepareExportDialog(VExporterList);
    if FExportDialog.Execute then begin
      VFileName := FExportDialog.FileName;
      if VFileName <> '' then begin
        VExporterItem := GetActiveExporter(VExporterList);
        if Assigned(VExporterItem) then begin
          VCategoryTree := FMarkSystem.CategoryDB.CategoryListToStaticTree(ACategoryList);
          VMarkTree := FMarkSystem.CategoryTreeToMarkTree(VCategoryTree, AIgnoreMarksVisible);
          VExporterItem.Exporter.ProcessExport(VFileName, VMarkTree);
        end;
      end;
    end;
  end;
end;

procedure TMarkDbGUIHelper.ExportMark(const AMark: IVectorDataItemSimple);
var
  VFileName: string;
  VMarkTree: IVectorItemTree;
  VSubsetBuilder: IVectorItemSubsetBuilder;
  VExporterList: IVectorItemTreeExporterListStatic;
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  if AMark <> nil then begin
    VFileName := PrepareFileName(AMark.Name);
    VExporterList := FExporterList.GetStatic;
    PrepareExportDialog(VExporterList);
    FExportDialog.FileName := VFileName;
    if FExportDialog.Execute then begin
      VFileName := FExportDialog.FileName;
      if VFileName <> '' then begin
        VExporterItem := GetActiveExporter(VExporterList);
        if Assigned(VExporterItem) then begin
          VSubsetBuilder := FVectorItemSubsetBuilderFactory.Build;
          VSubsetBuilder.Add(AMark);
          VMarkTree := TVectorItemTree.Create('', VSubsetBuilder.MakeStaticAndClear, nil);
          VExporterItem.Exporter.ProcessExport(VFileName, VMarkTree);
        end;
      end;
    end;
  end;
end;

function TMarkDbGUIHelper.GetMarkIdCaption(const AMarkId: IMarkId): string;
var
  VPointCaptionFormat: string;
  VPolygonCaptionFormat: string;
  VPathCaptionFormat: string;
  VFormat: string;
  VName: string;
begin
  VPointCaptionFormat := SAS_STR_ExtendedPointCaption;
  VPolygonCaptionFormat := SAS_STR_ExtendedPolygonCaption;
  VPathCaptionFormat := SAS_STR_ExtendedPathCaption;
  VName := AMarkId.Name;
  if VName = '' then begin
    VName := '(NoName)';
  end;
  if IsEqualGUID(AMarkId.MarkType, IVectorDataItemPoint) then begin
    VFormat := VPointCaptionFormat;
  end else if IsEqualGUID(AMarkId.MarkType, IVectorDataItemLine) then begin
    VFormat := VPathCaptionFormat;
  end else if IsEqualGUID(AMarkId.MarkType, IVectorDataItemPoly) then begin
    VFormat := VPolygonCaptionFormat;
  end else begin
    VFormat := '%0:s';
  end;
  Result := Format(VFormat, [AMarkId.Name]);
end;

function TMarkDbGUIHelper.ImportFilesModalInternal(
  AFiles: TStrings;
  const AImporterList: IVectorItemTreeImporterListStatic
): IInterfaceListStatic;
var
  VImportConfig: IImportConfig;
  i: Integer;
  VFileName: string;
  VExt: string;
  VImporter: IVectorItemTreeImporter;
  VTree: IVectorItemTree;
begin
  Result := nil;
  if Assigned(AFiles) and (AFiles.Count>0) then begin
    for i := 0 to AFiles.Count - 1 do begin
      VFileName := AFiles[i];
      if (FileExists(VFileName)) then begin
        VExt := ExtractFileExt(VFileName);
        VImporter := AImporterList.GetImporterByExt(VExt);
        if Assigned(VImporter) then begin
          VTree := VImporter.ProcessImport(VFileName);
          if Assigned(VTree) then begin
            if not Assigned(VImportConfig) then
              VImportConfig := EditModalImportConfig;
            if Assigned(VImportConfig) then begin
              Result := FMarkSystem.ImportItemsTree(VTree, VImportConfig);
            end else begin
              Break;
            end;
          end;
        end;
      end else begin
        ShowMessageFmt(_('Can''t open file: %s'), [VFileName]);
      end;
    end;
  end;
end;

procedure TMarkDbGUIHelper.PrepareImportDialog(const AImporterList: IVectorItemTreeImporterListStatic);
var
  VSelectedFilter: Integer;
  VFilterStr: string;
  i: Integer;
  VItem: IVectorItemTreeImporterListItem;
  VAllMasks: string;
begin
  VSelectedFilter := FImportDialog.FilterIndex;
  VFilterStr := '';
  VAllMasks := '';
  for i := 0 to AImporterList.Count - 1 do begin
    VItem := AImporterList.Items[i];
    VFilterStr := VFilterStr + '|' + VItem.Name + ' (*.'+ VItem.DefaultExt + ')|*.' + VItem.DefaultExt;
    if i > 0 then begin
      VAllMasks := VAllMasks + ';'
    end;
    VAllMasks := VAllMasks + '*.' + VItem.DefaultExt;
  end;
  VFilterStr :=  _('All compatible formats') + '(' + VAllMasks + ')|' + VAllMasks + VFilterStr;
  FImportDialog.Filter := VFilterStr;
  FImportDialog.FilterIndex := VSelectedFilter;
end;

function TMarkDbGUIHelper.ImportModal(ParentWnd: HWND): IInterfaceListStatic;
var
  VList: IVectorItemTreeImporterListStatic;
begin
  Result := nil;
  VList := FImporterList.GetStatic;
  PrepareImportDialog(VList);
  if (FImportDialog.Execute(ParentWnd)) then begin
    Result := ImportFilesModalInternal(FImportDialog.Files, VList);
  end;
end;

function TMarkDbGUIHelper.ImportFilesModal(
  AFiles: TStrings
): IInterfaceListStatic;
var
  VList: IVectorItemTreeImporterListStatic;
begin
  VList := FImporterList.GetStatic;
  Result := ImportFilesModalInternal(AFiles, VList);
end;

function TMarkDbGUIHelper.MarksMultiEditModal(const ACategory: ICategory): IImportConfig;
begin
  Result := FfrmMarksMultiEdit.GetImportConfig(ACategory);
end;

procedure TMarkDbGUIHelper.ShowMarkInfo(
  const AMark: IVectorDataItemSimple
);
begin
  if AMark <> nil then begin
    FfrmMarkInfo.ShowInfoModal(AMark);
  end;
end;


function TMarkDbGUIHelper.PolygonForOperation(
  const AGeometry: IGeometryLonLat;
  const AProjection: IProjectionInfo
  ): IGeometryLonLatMultiPolygon;
var
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatMultiLine;
  VPoly: IGeometryLonLatMultiPolygon;
  VDefRadius: String;
  VRadius: double;
  VFilter: ILonLatPointFilter;
begin
  Result := nil;
  if Supports(AGeometry, IGeometryLonLatMultiPolygon, VPoly) then begin
    Result := VPoly;
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VLine) then begin
    VDefRadius := '100';
    if InputQuery('', 'Radius , m', VDefRadius) then begin
      try
        VRadius := str2r(VDefRadius);
      except
        ShowMessage(SAS_ERR_ParamsInput);
        Exit;
      end;
      VFilter := TLonLatPointFilterLine2Poly.Create(VRadius, AProjection);
      Result :=
        FVectorGeometryLonLatFactory.CreateLonLatMultiPolygonByLonLatPathAndFilter(
          VLine,
          VFilter
          );
    end;
  end else begin
    if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
      VDefRadius := '100';
      if InputQuery('', 'Radius , m', VDefRadius) then begin
        try
          VRadius := str2r(VDefRadius);
        except
          ShowMessage(SAS_ERR_ParamsInput);
          Exit;
        end;
        Result :=
          FVectorGeometryLonLatFactory.CreateLonLatMultiPolygonCircleByPoint(
            AProjection,
            VPoint.Point,
            VRadius
          );
      end;
    end;
  end;
end;

function TMarkDbGUIHelper.SaveLineModal(
  const AMark: IVectorDataItemLine;
  const ALine: IGeometryLonLatMultiLine;
  const ADescription: string;
  AAsNewMark: Boolean
): Boolean;
var
  VMark: IVectorDataItemLine;
  VSourceMark: IVectorDataItemLine;
  VVisible: Boolean;
  VResult: IVectorDataItemSimple;
begin
  Result := False;
  VSourceMark := nil;
  if AMark <> nil then begin
    VVisible := FMarkSystem.MarkDb.GetMarkVisible(AMark);
    if AAsNewMark then begin
      VSourceMark := nil;
    end else begin
      VSourceMark := AMark;
    end;
    VMark := FMarkSystem.MarkDb.Factory.SimpleModifyLine(AMark, ALine, ADescription);
  end else begin
    VVisible := True;
    VMark := FMarkSystem.MarkDb.Factory.CreateNewLine(ALine, '', ADescription);
  end;
  if VMark <> nil then begin
    VMark := FfrmMarkEditPath.EditMark(VMark, VSourceMark = nil, VVisible);
    if VMark <> nil then begin
      VResult := FMarkSystem.MarkDb.UpdateMark(VSourceMark, VMark);
      if VResult <> nil then begin
        FMarkSystem.MarkDb.SetMarkVisible(VResult, VVisible);
        Result := True;
      end;
    end;
  end;
end;

function TMarkDbGUIHelper.SavePointModal(
  const AMark: IVectorDataItemPoint;
  const ALonLat: TDoublePoint
): Boolean;
var
  VMark: IVectorDataItemPoint;
  VVisible: Boolean;
  VResult: IVectorDataItemSimple;
  VSourceMark: IVectorDataItemPoint;
begin
  Result := False;
  if AMark <> nil then begin
    VVisible := FMarkSystem.MarkDb.GetMarkVisible(AMark);
    VSourceMark := AMark;
    VMark :=
      FMarkSystem.MarkDb.Factory.SimpleModifyPoint(
        AMark,
        FVectorGeometryLonLatFactory.CreateLonLatPoint(ALonLat)
      );
  end else begin
    VVisible := True;
    VSourceMark := nil;
    VMark :=
      FMarkSystem.MarkDb.Factory.CreateNewPoint(
        FVectorGeometryLonLatFactory.CreateLonLatPoint(ALonLat),
        '',
        ''
      );
  end;
  if VMark <> nil then begin
    VMark := FfrmMarkEditPoint.EditMark(VMark, VSourceMark = nil, VVisible);
    if VMark <> nil then begin
      VResult := FMarkSystem.MarkDb.UpdateMark(VSourceMark, VMark);
      if VResult <> nil then begin
        FMarkSystem.MarkDb.SetMarkVisible(VResult, VVisible);
        Result := True;
      end;
    end;
  end;
end;

function TMarkDbGUIHelper.SavePolyModal(
  const AMark: IVectorDataItemPoly;
  const ALine: IGeometryLonLatMultiPolygon;
  AAsNewMark: Boolean
): Boolean;
var
  VMark: IVectorDataItemPoly;
  VSourceMark: IVectorDataItemPoly;
  VVisible: Boolean;
  VResult: IVectorDataItemSimple;
begin
  Result := False;
  VSourceMark := nil;
  if AMark <> nil then begin
    VVisible := FMarkSystem.MarkDb.GetMarkVisible(AMark);
    if AAsNewMark then begin
      VSourceMark := nil;
    end else begin
      VSourceMark := AMark;
    end;
    VMark := FMarkSystem.MarkDb.Factory.SimpleModifyPoly(AMark, ALine);
  end else begin
    VVisible := True;
    VMark := FMarkSystem.MarkDb.Factory.CreateNewPoly(ALine, '', '');
  end;
  if VMark <> nil then begin
    VMark := FfrmMarkEditPoly.EditMark(VMark, VSourceMark = nil, VVisible);
    if VMark <> nil then begin
      VResult := FMarkSystem.MarkDb.UpdateMark(VSourceMark, VMark);
      if VResult <> nil then begin
        FMarkSystem.MarkDb.SetMarkVisible(VResult, VVisible);
        Result := True;
      end;
    end;
  end;
end;

end.



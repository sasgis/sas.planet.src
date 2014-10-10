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
  i_PathConfig,
  i_LanguageManager,
  i_InterfaceListStatic,
  i_ProjectionInfo,
  i_GeometryLonLatFactory,
  i_ArchiveReadWriteFactory,
  i_ValueToStringConverter,
  i_GeoCalc,
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
  frm_JpegImportConfigEdit,
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
    FfrmJpegImportConfigEdit: TfrmJpegImportConfigEdit;
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
    ): IGeometryLonLatPolygon;
    function AddKategory(const Name: string): IMarkCategory;
    procedure ShowMarkInfo(
      const AMark: IVectorDataItem
    );
    function EditMarkModal(
      const AMark: IVectorDataItem;
      const AIsNewMark: Boolean;
      var AVisible: Boolean
    ): IVectorDataItem;
    function EditCategoryModal(
      const ACategory: IMarkCategory;
      AIsNewMark: Boolean
    ): IMarkCategory;
    function SaveMarkModal(
      const AMark: IVectorDataItem;
      const AGeometry: IGeometryLonLat;
      AAsNewMark: Boolean = False;
      const ADescription: string = '';
      const ATemplate: IMarkTemplate = nil
    ): Boolean;
    function EditModalImportConfig: IImportConfig;
    function EditModalJpegImportConfig: IInterface;
    function MarksMultiEditModal(const ACategory: ICategory): IImportConfig;
    procedure ExportMark(const AMark: IVectorDataItem);
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
      const AGeoCalc: IGeoCalc;
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
  i_DoublePointFilter,
  i_VectorItemTree,
  i_InterfaceListSimple,
  i_StaticTreeItem,
  i_NotifierOperation,
  i_VectorItemTreeImporter,
  u_ResStrings,
  u_EnumDoublePointLine2Poly,
  u_VectorItemTree,
  u_InterfaceListSimple,
  u_NotifierOperation,
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
  const AGeoCalc: IGeoCalc;
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
  FfrmJpegImportConfigEdit :=
    TfrmJpegImportConfigEdit.Create(
      ALanguageManager,
      AAppearanceOfMarkFactory,
      FMarkSystem.MarkDb.Factory,
      FMarkSystem.CategoryDB
    );
  FfrmMarkInfo :=
    TfrmMarkInfo.Create(
      ALanguageManager,
      AValueToStringConverter,
      AGeoCalc
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
  FreeAndNil(FfrmJpegImportConfigedit);
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

function TMarkDbGUIHelper.DeleteCategoryModal(
  const ACategory: IMarkCategory;
  handle: THandle
): Boolean;
var
  I: Integer;
  VMessage: string;
  VList: IInterfaceListStatic;
begin
  Result := False;
  if ACategory <> nil then begin
    VList := FMarkSystem.CategoryDB.GetSubCategoryListForCategory(ACategory);
    if Assigned(VList) and (VList.Count > 0) then begin
      VMessage := Format(SAS_MSG_DeleteSubCategoryAsk, [ACategory.Name, VList.Count]);
    end else begin
      VMessage := Format(SAS_MSG_DeleteMarkCategoryAsk, [ACategory.Name]);
    end;
    if MessageBox(handle, PChar(VMessage), PChar(SAS_MSG_coution), 36) = IDYES then begin
      if Assigned(VList) then begin
        for I := 0 to VList.Count - 1 do begin
          FMarkSystem.DeleteCategoryWithMarks(IMarkCategory(VList[I]));
        end;
      end;
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
  VMessage: string;
  VMark: IVectorDataItem;
begin
  Result := False;
  if AMarkId <> nil then begin
    case AMarkId.MarkType of
      midPoint: VMessage := SAS_MSG_DeleteMarkPointAsk;
      midLine: VMessage := SAS_MSG_DeleteMarkPathAsk;
      midPoly: VMessage := SAS_MSG_DeleteMarkPolyAsk;
    else
      VMessage := _('Are you sure you want to delete placemark of unknown type with name "%0:s"?');
    end;
    VMessage := Format(VMessage, [AMarkId.Name]);
    if MessageBox(handle, PChar(VMessage), PChar(SAS_MSG_coution), 36) = IDYES then begin
      VMark := FMarkSystem.MarkDb.GetMarkByID(AMarkId);
      if Assigned(VMark) then begin
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
  const AMark: IVectorDataItem;
  const AIsNewMark: Boolean;
  var AVisible: Boolean
): IVectorDataItem;
begin
  Result := nil;
  if Supports(AMark.Geometry, IGeometryLonLatPoint) then begin
    Result := FfrmMarkEditPoint.EditMark(AMark, AIsNewMark, AVisible);
  end else if Supports(AMark.Geometry, IGeometryLonLatLine) then begin
    Result := FfrmMarkEditPath.EditMark(AMark, AIsNewMark, AVisible);
  end else if Supports(AMark.Geometry, IGeometryLonLatPolygon) then begin
    Result := FfrmMarkEditPoly.EditMark(AMark, AIsNewMark, AVisible);
  end;
end;

function TMarkDbGUIHelper.EditModalImportConfig: IImportConfig;
begin
  Result := FfrmImportConfigEdit.GetImportConfig;
end;

function TMarkDbGUIHelper.EditModalJpegImportConfig: IInterface;
begin
  Result := FfrmJpegImportConfigEdit.GetConfig;
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
  const AExporterList: IVectorItemTreeExporterListStatic
);
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
  VNotifier: INotifierOperation;
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
          VNotifier := TNotifierOperationFake.Create;
          VExporterItem.Exporter.ProcessExport(VNotifier.CurrentOperation, VNotifier, VFileName, VMarkTree);
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
  VNotifier: INotifierOperation;
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
          VNotifier := TNotifierOperationFake.Create;
          VExporterItem.Exporter.ProcessExport(VNotifier.CurrentOperation, VNotifier, VFileName, VMarkTree);
        end;
      end;
    end;
  end;
end;

procedure TMarkDbGUIHelper.ExportMark(const AMark: IVectorDataItem);
var
  VFileName: string;
  VMarkTree: IVectorItemTree;
  VSubsetBuilder: IVectorItemSubsetBuilder;
  VExporterList: IVectorItemTreeExporterListStatic;
  VExporterItem: IVectorItemTreeExporterListItem;
  VNotifier: INotifierOperation;
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
          VNotifier := TNotifierOperationFake.Create;
          VExporterItem.Exporter.ProcessExport(VNotifier.CurrentOperation, VNotifier, VFileName, VMarkTree);
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
  case AMarkId.MarkType of
    midPoint: VFormat := VPointCaptionFormat;
    midLine: VFormat := VPathCaptionFormat;
    midPoly: VFormat := VPolygonCaptionFormat;
  else
    VFormat := '%0:s';
  end;
  Result := Format(VFormat, [AMarkId.Name]);
end;

function TMarkDbGUIHelper.ImportFilesModalInternal(
  AFiles: TStrings;
  const AImporterList: IVectorItemTreeImporterListStatic
): IInterfaceListStatic;

type
  TImportRec = record
    FExt: string;
    FConfig: IInterface;
    FImporter: IVectorItemTreeImporter;
  end;
  TImportRecArr = array of TImportRec;

  function _RecIndexByFileExt(AExt: string; var ARecArr: TImportRecArr): Integer;
  var
    I: Integer;
    VConfig: IInterface;
    VImporter: IVectorItemTreeImporter;
  begin
    Result := -1;
    if SameText(AExt, '.jpeg') then begin
      AExt := '.jpg';
    end;
    for I := 0 to Length(ARecArr) - 1 do begin
      if SameText(AExt, ARecArr[I].FExt) then begin
        Result := I;
        Exit;
      end;
    end;
    VImporter := AImporterList.GetImporterByExt(AExt);
    if Assigned(VImporter) then begin
      if SameText(AExt, '.jpg') then begin
        VConfig := EditModalJpegImportConfig;
      end else begin
        VConfig := EditModalImportConfig;
      end;
      if Assigned(VConfig) then begin
        I := Length(ARecArr);
        SetLength(ARecArr, I + 1);
        ARecArr[I].FExt := AExt;
        ARecArr[I].FImporter := VImporter;
        ARecArr[I].FConfig := VConfig;
        Result := I;
      end;
    end;
  end;

var
  I, J: Integer;
  VCount: Integer;
  VFileName: string;
  VTree: IVectorItemTree;
  VImportConfig: IImportConfig;
  VRecArr: TImportRecArr;
  VNotifier: INotifierOperation;
begin
  Result := nil;

  VCount := 0;
  SetLength(VRecArr, 0);

  if Assigned(AFiles) and (AFiles.Count > 0) then begin
    VNotifier := TNotifierOperationFake.Create;
    for I := 0 to AFiles.Count - 1 do begin
      VFileName := AFiles[I];
      if FileExists(VFileName) then begin
        J := _RecIndexByFileExt(ExtractFileExt(VFileName), VRecArr);
        if J < 0 then begin
          Break;
        end;
        VTree := VRecArr[J].FImporter.ProcessImport(VNotifier.CurrentOperation, VNotifier, VFileName, VRecArr[J].FConfig);
        if Assigned(VTree) then begin
          if Supports(VRecArr[J].FConfig, IImportConfig, VImportConfig) then begin
            Result := FMarkSystem.ImportItemsTree(VTree, VImportConfig);
            Inc(VCount);
          end else begin
            Break;
          end;
        end;
      end else begin
        ShowMessageFmt(_('Can''t open file: %s'), [VFileName]);
      end;
    end;
  end;

  if VCount > 0 then begin
    ShowMessageFmt(_('Import finished. Total processed: %d files. Successfull imported: %d files.'), [AFiles.Count, VCount]);
  end else begin
    ShowMessage(_('Nothing to import!'));
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
  const AMark: IVectorDataItem
);
begin
  if AMark <> nil then begin
    FfrmMarkInfo.ShowInfoModal(AMark);
  end;
end;


function TMarkDbGUIHelper.PolygonForOperation(
  const AGeometry: IGeometryLonLat;
  const AProjection: IProjectionInfo
  ): IGeometryLonLatPolygon;
var
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatLine;
  VPoly: IGeometryLonLatPolygon;
  VDefRadius: String;
  VRadius: double;
  VFilter: ILonLatPointFilter;
begin
  Result := nil;
  if Supports(AGeometry, IGeometryLonLatPolygon, VPoly) then begin
    Result := VPoly;
  end else if Supports(AGeometry, IGeometryLonLatLine, VLine) then begin
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

function TMarkDbGUIHelper.SaveMarkModal(
  const AMark: IVectorDataItem;
  const AGeometry: IGeometryLonLat;
  AAsNewMark: Boolean;
  const ADescription: string;
  const ATemplate: IMarkTemplate
): Boolean;
var
  VMark: IVectorDataItem;
  VSourceMark: IVectorDataItem;
  VVisible: Boolean;
  VResult: IVectorDataItem;
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
    VMark := FMarkSystem.MarkDb.Factory.ModifyGeometry(AMark, AGeometry, ADescription);
  end else begin
    VVisible := True;
    VMark := FMarkSystem.MarkDb.Factory.CreateNewMark(AGeometry, '', ADescription, ATemplate);
  end;
  if VMark <> nil then begin
    VMark := EditMarkModal(VMark, VSourceMark = nil, VVisible);
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



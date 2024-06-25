{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit frm_MarksExport;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  UITypes,
  TBXDkPanels,
  i_LanguageManager,
  i_CommonDialogConfig,
  i_ExportConfig,
  i_MarkSystem,
  i_MarkCategory,
  i_MarkCategoryList,
  i_VectorItemTree,
  i_VectorDataItemSimple,
  i_VectorItemSubsetBuilder,
  i_VectorItemTreeExporterList,
  frm_MarksExportConfig,
  u_CommonFormAndFrameParents;

type
  TfrmMarksExport = class(TFormWitghLanguageManager)
    lblFormat: TLabel;
    cbbFormat: TComboBox;
    lblDest: TLabel;
    edtDest: TEdit;
    btnDest: TButton;
    chkFilePerMark: TCheckBox;
    btnCancel: TButton;
    btnRun: TButton;
    btnConfig: TTBXButton;
    dlgSave: TSaveDialog;
    procedure cbbFormatChange(Sender: TObject);
    procedure btnConfigClick(Sender: TObject);
    procedure btnDestClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMarkSystem: IMarkSystem;
    FExportDialogConfig: ICommonDialogConfig;
    FExporterList: IVectorItemTreeExporterListStatic;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;

    FMarkTree: IVectorItemTree;
    FfrmMarksExportConfig: TfrmMarksExportConfig;

    function GetActiveExporter: IVectorItemTreeExporterListItem;
    class function CalcItemsCount(const ATree: IVectorItemTree): Int64;
  public
    procedure ExportMark(
      const AMark: IVectorDataItem
    );
    procedure ExportCategory(
      const AMarkCategory: IMarkCategory;
      const AIgnoreMarksVisible: Boolean
    );
    procedure ExportCategoryList(
      const ACategoryList: IMarkCategoryList;
      const AIgnoreMarksVisible: Boolean
    );
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMarkSystem: IMarkSystem;
      const AExportDialogConfig: ICommonDialogConfig;
      const AExporterList: IVectorItemTreeExporterListChangeable;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  i_MarkCategoryTree,
  i_VectorItemSubset,
  i_NotifierOperation,
  u_VectorItemTree,
  u_FileSystemFunc,
  u_NotifierOperation;

{$R *.dfm}

{ TfrmMarksExport }

constructor TfrmMarksExport.Create(
  const ALanguageManager: ILanguageManager;
  const AMarkSystem: IMarkSystem;
  const AExportDialogConfig: ICommonDialogConfig;
  const AExporterList: IVectorItemTreeExporterListChangeable;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory
);
var
  I: Integer;
begin
  inherited Create(ALanguageManager);

  FMarkSystem := AMarkSystem;
  FExportDialogConfig := AExportDialogConfig;
  FExporterList := AExporterList.GetStatic;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;

  cbbFormat.Items.Clear;
  for I := 0 to FExporterList.Count - 1 do begin
    cbbFormat.Items.Add(FExporterList.Items[I].Name);
  end;
  cbbFormat.DropDownCount := FExporterList.Count;

  I := FExportDialogConfig.FilterIndex;
  if (I >= 0) and (I < FExporterList.Count) then begin
    cbbFormat.ItemIndex := I;
  end else begin
    cbbFormat.ItemIndex := 0;
  end;

  chkFilePerMark.Checked := False;
end;

destructor TfrmMarksExport.Destroy;
begin
  FreeAndNil(FfrmMarksExportConfig);
  inherited Destroy;
end;

class function TfrmMarksExport.CalcItemsCount(const ATree: IVectorItemTree): Int64;
var
  I: Integer;
  VItems: IVectorItemSubset;
  VSubTree: IVectorItemTree;
begin
  Result := 0;

  if ATree = nil then begin
    Exit;
  end;

  VItems := ATree.Items;
  if VItems <> nil then begin
    Result := VItems.Count;
  end;

  for I := 0 to ATree.SubTreeItemCount - 1 do begin
    VSubTree := ATree.GetSubTreeItem(I);
    Inc(Result, CalcItemsCount(VSubTree)); // recursion
  end;
end;

procedure TfrmMarksExport.FormShow(Sender: TObject);
var
  VCount: Integer;
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  if FMarkTree <> nil then begin
    VCount := CalcItemsCount(FMarkTree);
  end else begin
    VCount := 0;
    Assert(False);
  end;

  Self.Caption := _('Export placemarks') + Format(' (%d)', [VCount]);

  chkFilePerMark.Enabled := False; // ToDo: VCount > 1;

  VExporterItem := GetActiveExporter;
  btnConfig.Enabled := (VExporterItem <> nil) and (VExporterItem.Config <> nil);
end;

function TfrmMarksExport.GetActiveExporter: IVectorItemTreeExporterListItem;
var
  I: Integer;
begin
  I := cbbFormat.ItemIndex;
  if (I >= 0) and (I < FExporterList.Count) then begin
    Result := FExporterList.Items[I];
  end else begin
    Result := nil;
  end;
end;

procedure TfrmMarksExport.btnDestClick(Sender: TObject);
var
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  VExporterItem := GetActiveExporter;
  if VExporterItem = nil then begin
    Exit;
  end;

  dlgSave.Filter := VExporterItem.Name + ' (*.' + VExporterItem.DefaultExt + ')|*.' + VExporterItem.DefaultExt;
  dlgSave.DefaultExt := VExporterItem.DefaultExt;
  dlgSave.InitialDir := FExportDialogConfig.InitialDir;

  if dlgSave.Execute then begin
    edtDest.Text := dlgSave.FileName;
    FExportDialogConfig.InitialDir := ExtractFileDir(edtDest.Text);
  end;
end;

procedure TfrmMarksExport.cbbFormatChange(Sender: TObject);
var
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  VExporterItem := GetActiveExporter;
  btnConfig.Enabled := (VExporterItem <> nil) and (VExporterItem.Config <> nil);
  FExportDialogConfig.FilterIndex := cbbFormat.ItemIndex;
  edtDest.Text := '';
end;

procedure TfrmMarksExport.btnConfigClick(Sender: TObject);
var
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  VExporterItem := GetActiveExporter;
  if (VExporterItem <> nil) and (VExporterItem.Config <> nil) then begin
    if FfrmMarksExportConfig = nil then begin
      FfrmMarksExportConfig := TfrmMarksExportConfig.Create(Self);
    end;
    FfrmMarksExportConfig.DoShowModal(VExporterItem.Config);
  end;
end;

procedure TfrmMarksExport.btnRunClick(Sender: TObject);
var
  VFileName: string;
  VNotifier: INotifierOperation;
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  VExporterItem := GetActiveExporter;
  if (VExporterItem <> nil) and (FMarkTree <> nil) then begin
    VFileName := Trim(edtDest.Text);
    if VFileName = '' then begin
      MessageDlg(_('Set destination first!'), mtError, [mbOk], 0);
      Exit;
    end;

    VNotifier := TNotifierOperationFake.Create;

    if chkFilePerMark.Checked then begin
      // ToDo
    end else begin
      VExporterItem.Exporter.ProcessExport(VNotifier.CurrentOperation, VNotifier, VFileName, FMarkTree);
    end;
  end;

  Close;
end;

procedure TfrmMarksExport.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMarksExport.ExportMark(const AMark: IVectorDataItem);
var
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  if AMark = nil then begin
    Exit;
  end;

  try
    edtDest.Text := '';
    dlgSave.FileName := ReplaceIllegalFileNameChars(AMark.Name);

    VSubsetBuilder := FVectorItemSubsetBuilderFactory.Build;
    VSubsetBuilder.Add(AMark);
    FMarkTree := TVectorItemTree.Create('Export', VSubsetBuilder.MakeStaticAndClear, nil);

    Self.PopupParent := Application.MainForm;
    Self.ShowModal;
  finally
    FMarkTree := nil;
  end;
end;

procedure TfrmMarksExport.ExportCategory(
  const AMarkCategory: IMarkCategory;
  const AIgnoreMarksVisible: Boolean
);
var
  VCategoryTree: IMarkCategoryTree;
  VSubCategoryList: IMarkCategoryList;
begin
  if AMarkCategory = nil then begin
    Exit;
  end;

  try
    edtDest.Text := '';
    dlgSave.FileName := ReplaceIllegalFileNameChars(AMarkCategory.Name);

    VSubCategoryList := FMarkSystem.CategoryDB.GetCategoryWithSubCategories(AMarkCategory);
    if not AIgnoreMarksVisible then begin
      VSubCategoryList := FMarkSystem.CategoryDB.FilterVisibleCategories(VSubCategoryList);
    end;
    VCategoryTree := FMarkSystem.CategoryDB.CategoryListToStaticTree(VSubCategoryList);
    FMarkTree := FMarkSystem.CategoryTreeToMarkTree(VCategoryTree, AIgnoreMarksVisible);

    Self.PopupParent := Application.MainForm;
    Self.ShowModal;
  finally
    FMarkTree := nil;
  end;
end;

procedure TfrmMarksExport.ExportCategoryList(
  const ACategoryList: IMarkCategoryList;
  const AIgnoreMarksVisible: Boolean
);
var
  VCategoryTree: IMarkCategoryTree;
begin
  if (ACategoryList = nil) or (ACategoryList.Count = 0) then begin
    Exit;
  end;

  try
    edtDest.Text := '';
    dlgSave.FileName := '';

    VCategoryTree := FMarkSystem.CategoryDB.CategoryListToStaticTree(ACategoryList);
    FMarkTree := FMarkSystem.CategoryTreeToMarkTree(VCategoryTree, AIgnoreMarksVisible);

    Self.PopupParent := Application.MainForm;
    Self.ShowModal;
  finally
    FMarkTree := nil;
  end;
end;

end.

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
  SysUtils,
  Classes,
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
  i_NotifierOperation,
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
    procedure chkFilePerMarkClick(Sender: TObject);
    procedure edtDestChange(Sender: TObject);
  private
    FMarkSystem: IMarkSystem;
    FExportDialogConfig: ICommonDialogConfig;
    FExporterList: IVectorItemTreeExporterListStatic;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;

    FMarkTree: IVectorItemTree;
    FItemsCount: Int64;
    FfrmMarksExportConfig: TfrmMarksExportConfig;

    function GetActiveExporter: IVectorItemTreeExporterListItem;
    procedure UpdateUI;
    function IsExportToSeparateFiles: Boolean; inline;
    procedure DoExportToSeparateFiles(
      const ATree: IVectorItemTree;
      const AExporterItem: IVectorItemTreeExporterListItem;
      const ANotifier: INotifierOperation;
      const ADestPath: string
    );
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
  FileCtrl,
  gnugettext,
  i_MarkCategoryTree,
  i_VectorItemSubset,
  u_VectorItemTree,
  u_FileSystemFunc,
  u_NotifierOperation;

const
  CMaxCountToWarning = 1000;

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

procedure TfrmMarksExport.UpdateUI;
var
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  VExporterItem := GetActiveExporter;

  btnConfig.Visible := (VExporterItem <> nil) and (VExporterItem.Config <> nil);

  chkFilePerMark.Enabled :=
    (FItemsCount > 1) and
    (VExporterItem <> nil) and
    (elioAllowSeparateFiles in VExporterItem.Options);

  if not chkFilePerMark.Enabled then begin
    chkFilePerMark.Checked := False;
  end;

  if
    (VExporterItem <> nil) and
    (dlgSave.FileName <> '') and
    (FExportDialogConfig.InitialDir <> '')
  then begin
    edtDest.Text :=
      IncludeTrailingPathDelimiter(FExportDialogConfig.InitialDir) +
      ChangeFileExt(ExtractFileName(dlgSave.FileName), '');
    if not IsExportToSeparateFiles and (elioSaveToFile in VExporterItem.Options) then begin
      edtDest.Text := edtDest.Text + '.' + VExporterItem.DefaultExt;
    end;
  end;
end;

procedure TfrmMarksExport.FormShow(Sender: TObject);
begin
  if FMarkTree <> nil then begin
    FItemsCount := CalcItemsCount(FMarkTree);
  end else begin
    FItemsCount := 0;
    Assert(False);
  end;

  Self.Caption := Format(_('Export placemarks (%d)'), [FItemsCount]);

  UpdateUI;
  btnRun.SetFocus;
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

function TfrmMarksExport.IsExportToSeparateFiles: Boolean;
begin
  Result := chkFilePerMark.Enabled and chkFilePerMark.Checked;
end;

procedure TfrmMarksExport.btnDestClick(Sender: TObject);
var
  VPath: string;
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  VExporterItem := GetActiveExporter;
  if VExporterItem = nil then begin
    Exit;
  end;

  if IsExportToSeparateFiles or (elioSaveToDir in VExporterItem.Options) then begin
    VPath := FExportDialogConfig.InitialDir;
    if SelectDirectory('', '', VPath, [sdNewFolder, sdNewUI]) then begin
      edtDest.Text := VPath;
      FExportDialogConfig.InitialDir := edtDest.Text;
    end;
  end else begin
    dlgSave.Filter := VExporterItem.Name + ' (*.' + VExporterItem.DefaultExt + ')' +
      '|*.' + VExporterItem.DefaultExt;
    dlgSave.DefaultExt := VExporterItem.DefaultExt;
    dlgSave.InitialDir := FExportDialogConfig.InitialDir;
    if dlgSave.Execute then begin
      edtDest.Text := dlgSave.FileName;
      FExportDialogConfig.InitialDir := ExtractFileDir(edtDest.Text);
    end;
  end;
end;

procedure TfrmMarksExport.cbbFormatChange(Sender: TObject);
begin
  FExportDialogConfig.FilterIndex := cbbFormat.ItemIndex;
  UpdateUI;
end;

procedure TfrmMarksExport.chkFilePerMarkClick(Sender: TObject);
begin
  UpdateUI;
end;

procedure TfrmMarksExport.btnConfigClick(Sender: TObject);
var
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  VExporterItem := GetActiveExporter;
  if (VExporterItem <> nil) and (VExporterItem.Config <> nil) then begin
    if FfrmMarksExportConfig = nil then begin
      FfrmMarksExportConfig := TfrmMarksExportConfig.Create(Self.LanguageManager);
    end;
    FfrmMarksExportConfig.DoShowModal(
      UpperCase(VExporterItem.DefaultExt),
      VExporterItem.Config
    );
  end;
end;

procedure TfrmMarksExport.DoExportToSeparateFiles(
  const ATree: IVectorItemTree;
  const AExporterItem: IVectorItemTreeExporterListItem;
  const ANotifier: INotifierOperation;
  const ADestPath: string
);
var
  VNoNameCounter: Integer;

  function _GetFileNameUnique(const AMarkName: string): string;
  var
    VName: string;
    VCounter: Integer;
  begin
    if AMarkName = '' then begin
      VName := '(NoName)';
      VCounter := VNoNameCounter;
    end else begin
      VName := AMarkName;
      VCounter := 0;
    end;

    Result := ADestPath + VName + '.' + AExporterItem.DefaultExt;

    while FileExists(Result) do begin
      Inc(VCounter);
      Result := ADestPath + VName + ' (' + VCounter.ToString + ').' + AExporterItem.DefaultExt;
    end;

    if AMarkName = '' then begin
      VNoNameCounter := VCounter;
    end;
  end;

var
  I: Integer;
  VSubDir: string;
  VFileName: string;
  VMark: IVectorDataItem;
  VItems: IVectorItemSubset;
  VSubTree: IVectorItemTree;
  VMarkTree: IVectorItemTree;
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  if ATree = nil then begin
    Exit;
  end;

  VItems := ATree.Items;
  if VItems <> nil then begin
    if not SysUtils.ForceDirectories(ADestPath) then begin
      RaiseLastOSError;
    end;

    VNoNameCounter := 0;

    for I := 0 to VItems.Count - 1 do begin
      VMark := VItems[I];

      VFileName := _GetFileNameUnique(ReplaceIllegalFileNameChars(VMark.Name));

      VSubsetBuilder := FVectorItemSubsetBuilderFactory.Build;
      VSubsetBuilder.Add(VMark);
      VMarkTree := TVectorItemTree.Create('Export', VSubsetBuilder.MakeStaticAndClear, nil);

      // do export
      AExporterItem.Exporter.ProcessExport(ANotifier.CurrentOperation, ANotifier, VFileName, VMarkTree);
    end;
  end;

  for I := 0 to ATree.SubTreeItemCount - 1 do begin
    VSubTree := ATree.GetSubTreeItem(I);

    if VSubTree.Name <> '' then begin
      VSubDir := ADestPath + VSubTree.Name + PathDelim;
    end else begin
      VSubDir := ADestPath + '(NoName)' + PathDelim;
    end;

    DoExportToSeparateFiles(VSubTree, AExporterItem, ANotifier, VSubDir); // recursion
  end;
end;

procedure TfrmMarksExport.edtDestChange(Sender: TObject);
begin
  edtDest.Hint := edtDest.Text;
end;

procedure TfrmMarksExport.btnRunClick(Sender: TObject);
var
  VMsg: string;
  VFileDir: string;
  VFileName: string;
  VNotifier: INotifierOperation;
  VExporterItem: IVectorItemTreeExporterListItem;
begin
  VExporterItem := GetActiveExporter;
  if (VExporterItem <> nil) and (FMarkTree <> nil) then begin
    VFileName := Trim(edtDest.Text);
    if VFileName = '' then begin
      MessageDlg(_('Please specify where to save!'), mtError, [mbOK], 0);
      Exit;
    end;

    VNotifier := TNotifierOperationFake.Create;

    if IsExportToSeparateFiles then begin
      if FItemsCount > CMaxCountToWarning then begin
        VMsg := Format(
          _('You are about to export placemarks into %d separate files. Are you sure?'), [FItemsCount]
        );
        if MessageDlg(VMsg, mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbCancel], 0) <> mrYes then begin
          Exit;
        end;
      end;

      VFileDir := IncludeTrailingPathDelimiter(VFileName);
      DoExportToSeparateFiles(FMarkTree, VExporterItem, VNotifier, VFileDir);
    end else begin
      if elioSaveToFile in VExporterItem.Options then begin
        VFileDir := ExtractFileDir(VFileName);
      end else
      if elioSaveToDir in VExporterItem.Options then begin
        VFileDir := IncludeTrailingPathDelimiter(VFileName);
        VFileName := VFileDir;
      end else begin
        raise Exception.Create('Unexpected SaveTo target!');
      end;

      if not SysUtils.ForceDirectories(VFileDir) then begin
        RaiseLastOSError;
      end;

      // do export
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

    if (FMarkTree.Items = nil) and (FMarkTree.SubTreeItemCount = 1) then begin
      FMarkTree := FMarkTree.GetSubTreeItem(0);
    end;

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

    Self.ShowModal;
  finally
    FMarkTree := nil;
  end;
end;

end.

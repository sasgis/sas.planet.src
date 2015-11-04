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

unit frm_MapTypeEdit;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Spin,
  SynEdit,
  i_MapType,
  i_LanguageManager,
  i_TileStorageTypeList,
  u_CommonFormAndFrameParents,
  fr_CacheTypeList;

type
  TfrmMapTypeEdit = class(TFormWitghLanguageManager)
    EditNameinCache: TEdit;
    EditParSubMenu: TEdit;
    lblUrl: TLabel;
    lblFolder: TLabel;
    lblSubMenu: TLabel;
    chkBoxSeparator: TCheckBox;
    EditHotKey: THotKey;
    btnOk: TButton;
    btnCancel: TButton;
    btnByDefault: TButton;
    btnResetUrl: TButton;
    btnResetFolder: TButton;
    btnResetSubMenu: TButton;
    btnResetHotKey: TButton;
    EditURL: TMemo;
    SESleep: TSpinEdit;
    lblPause: TLabel;
    btnResetPause: TButton;
    lblCacheType: TLabel;
    btnResetCacheType: TButton;
    pnlBottomButtons: TPanel;
    pnlCacheType: TPanel;
    grdpnlHotKey: TGridPanel;
    pnlParentItem: TPanel;
    pnlCacheName: TPanel;
    pnlUrl: TPanel;
    pnlUrlRight: TPanel;
    lblHotKey: TLabel;
    CheckEnabled: TCheckBox;
    pnlBottom: TPanel;
    lblZmpName: TLabel;
    edtZmp: TEdit;
    pnlVersion: TPanel;
    btnResetVersion: TButton;
    edtVersion: TEdit;
    lblVersion: TLabel;
    pnlHeader: TPanel;
    lblHeader: TLabel;
    pnlHeaderReset: TPanel;
    btnResetHeader: TButton;
    mmoHeader: TMemo;
    pnlDownloaderState: TPanel;
    lblDownloaderState: TLabel;
    mmoDownloadState: TMemo;
    chkDownloadEnabled: TCheckBox;
    PageControl1: TPageControl;
    tsInternet: TTabSheet;
    tsParams: TTabSheet;
    tsGetURLScript: TTabSheet;
    tsInfo: TTabSheet;
    BtnSelectPath: TButton;
    tvMenu: TTreeView;
    tsOthers: TTabSheet;
    spl1: TSplitter;
    pnlSleep: TPanel;
    pnlCacheTypesList: TPanel;
    chkCacheReadOnly: TCheckBox;
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(
      Sender: TObject;
      var Action: TCloseAction
    );
    procedure btnByDefaultClick(Sender: TObject);
    procedure btnResetUrlClick(Sender: TObject);
    procedure btnResetFolderClick(Sender: TObject);
    procedure btnResetSubMenuClick(Sender: TObject);
    procedure btnResetHotKeyClick(Sender: TObject);
    procedure btnResetPauseClick(Sender: TObject);
    procedure btnResetCacheTypeClick(Sender: TObject);
    procedure btnResetVersionClick(Sender: TObject);
    procedure btnResetHeaderClick(Sender: TObject);
    procedure BtnSelectPathClick(Sender: TObject);
    procedure tvMenuClick(Sender: TObject);
    procedure tvMenuCollapsing(
      Sender: TObject;
      Node: TTreeNode;
      var AllowCollapse: Boolean
    );
    procedure FormShow(Sender: TObject);
  private
    synedtParams: TSynEdit;
    synedtScript: TSynEdit;
    synedtInfo: TSynEdit;
    FTileStorageTypeList: ITileStorageTypeListStatic;
    FMapType: IMapType;
    FfrCacheTypesList: TfrCacheTypeList;
    procedure BuildTreeViewMenu;
    procedure CreateSynEditTextHighlighters;
  public
    function EditMapModadl(const AMapType: IMapType): Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ATileStorageTypeList: ITileStorageTypeListStatic
    );
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  SysUtils,
  t_CommonTypes,
  c_CacheTypeCodes,
  i_TileDownloaderState,
  u_SynEditExt,
  u_SafeStrUtil,
  u_ResStrings;

{$R *.dfm}

constructor TfrmMapTypeEdit.Create(
  const ALanguageManager: ILanguageManager;
  const ATileStorageTypeList: ITileStorageTypeListStatic
);
begin
  inherited Create(ALanguageManager);
  FfrCacheTypesList :=
    TfrCacheTypeList.Create(
      ALanguageManager,
      ATileStorageTypeList,
      True,
      CTileStorageTypeClassAll,
      [tsacRead]
    );
  FTileStorageTypeList := ATileStorageTypeList;
  BuildTreeViewMenu;
  CreateSynEditTextHighlighters;
end;

destructor TfrmMapTypeEdit.Destroy;
begin
  FreeAndNil(FfrCacheTypesList);
  inherited;
end;

procedure TfrmMapTypeEdit.FormShow(Sender: TObject);
begin
  FfrCacheTypesList.Show(pnlCacheTypesList);
end;

procedure TfrmMapTypeEdit.btnResetHeaderClick(Sender: TObject);
begin
  mmoHeader.Text := string(FMapType.Zmp.TileDownloadRequestBuilderConfig.RequestHeader);
end;

procedure TfrmMapTypeEdit.btnOkClick(Sender: TObject);
begin
  FMapType.TileDownloadRequestBuilderConfig.LockWrite;
  try
    FMapType.TileDownloadRequestBuilderConfig.UrlBase := SafeStringToAnsi(EditURL.Text);
    FMapType.TileDownloadRequestBuilderConfig.RequestHeader := SafeStringToAnsi(mmoHeader.Text);
  finally
    FMapType.TileDownloadRequestBuilderConfig.UnlockWrite;
  end;

  FMapType.GUIConfig.LockWrite;
  try
    FMapType.GUIConfig.ParentSubMenu.Value := EditParSubMenu.Text;
    FMapType.GUIConfig.HotKey := EditHotKey.HotKey;
    FMapType.GUIConfig.Enabled := CheckEnabled.Checked;
    FMapType.GUIConfig.Separator := chkBoxSeparator.Checked;
  finally
    FMapType.GUIConfig.UnlockWrite;
  end;

  FMapType.TileDownloaderConfig.WaitInterval := SESleep.Value;
  FMapType.StorageConfig.LockWrite;
  try
    FMapType.StorageConfig.NameInCache := EditNameinCache.Text;
    // do not change cache types for GE and GC
    if not (FMapType.StorageConfig.CacheTypeCode in [c_File_Cache_Id_GE, c_File_Cache_Id_GC]) then begin
      FMapType.StorageConfig.CacheTypeCode := FfrCacheTypesList.IntCode;
      FMapType.StorageConfig.IsReadOnly := chkCacheReadOnly.Checked;
    end;
  finally
    FMapType.StorageConfig.UnlockWrite;
  end;
  FMapType.VersionRequestConfig.Version := edtVersion.Text;
  FMapType.Abilities.UseDownload := chkDownloadEnabled.Checked;

  ModalResult := mrOk;
end;

procedure TfrmMapTypeEdit.btnResetVersionClick(Sender: TObject);
begin
  edtVersion.Text := FMapType.Zmp.Version;
end;

procedure TfrmMapTypeEdit.BtnSelectPathClick(Sender: TObject);
var
  TempPath: string;
begin
  TempPath := EditNameinCache.text;
  if SelectDirectory(_('Cache folder'), '', TempPath) then begin
    EditNameinCache.Text := IncludeTrailingPathDelimiter(TempPath);
  end;
end;

procedure TfrmMapTypeEdit.FormClose(
  Sender: TObject;
  var Action: TCloseAction
);
begin
  FMapType := nil;
end;

procedure TfrmMapTypeEdit.btnByDefaultClick(Sender: TObject);
begin
  EditURL.Text := string(FMapType.Zmp.TileDownloadRequestBuilderConfig.UrlBase);
  mmoHeader.Text := string(FMapType.Zmp.TileDownloadRequestBuilderConfig.RequestHeader);

  EditNameinCache.Text := FMapType.Zmp.StorageConfig.NameInCache;
  SESleep.Value := FMapType.Zmp.TileDownloaderConfig.WaitInterval;
  EditHotKey.HotKey := FMapType.Zmp.GUI.HotKey;

  if not (FMapType.StorageConfig.CacheTypeCode in [c_File_Cache_Id_GE, c_File_Cache_Id_GC]) then begin
    FfrCacheTypesList.IntCode := FMapType.Zmp.StorageConfig.CacheTypeCode;
  end;
  chkCacheReadOnly.Checked := FMapType.Zmp.StorageConfig.Abilities.IsReadOnly;
  EditParSubMenu.Text := FMapType.GUIConfig.ParentSubMenu.GetDefaultValue;
  chkBoxSeparator.Checked := FMapType.Zmp.GUI.Separator;
  CheckEnabled.Checked := FMapType.Zmp.GUI.Enabled;
  edtVersion.Text := FMapType.Zmp.Version;
end;

procedure TfrmMapTypeEdit.btnResetUrlClick(Sender: TObject);
begin
  EditURL.Text := string(FMapType.Zmp.TileDownloadRequestBuilderConfig.UrlBase);
end;

procedure TfrmMapTypeEdit.btnResetFolderClick(Sender: TObject);
begin
  EditNameinCache.Text := FMapType.Zmp.StorageConfig.NameInCache;
end;

procedure TfrmMapTypeEdit.btnResetSubMenuClick(Sender: TObject);
begin
  EditParSubMenu.Text := FMapType.GUIConfig.ParentSubMenu.GetDefaultValue;
end;

procedure TfrmMapTypeEdit.btnResetHotKeyClick(Sender: TObject);
begin
  EditHotKey.HotKey := FMapType.Zmp.GUI.HotKey;
end;

procedure TfrmMapTypeEdit.btnResetPauseClick(Sender: TObject);
begin
  SESleep.Value := FMapType.TileDownloaderConfig.WaitInterval;
end;

procedure TfrmMapTypeEdit.btnResetCacheTypeClick(Sender: TObject);
begin
  if not (FMapType.StorageConfig.CacheTypeCode in [c_File_Cache_Id_GE, c_File_Cache_Id_GC]) then begin
    FfrCacheTypesList.IntCode := FMapType.Zmp.StorageConfig.CacheTypeCode;
  end;
end;

function TfrmMapTypeEdit.EditMapModadl(const AMapType: IMapType): Boolean;
var
  VDownloadState: ITileDownloaderStateStatic;
begin
  FMapType := AMapType;

  Caption := SAS_STR_EditMap + ' ' + FMapType.GUIConfig.Name.Value;
  edtZmp.Text := '%Maps%' + PathDelim + AMapType.Zmp.FileName;

  FMapType.TileDownloadRequestBuilderConfig.LockRead;
  try
    EditURL.Text := string(FMapType.TileDownloadRequestBuilderConfig.UrlBase);
    mmoHeader.Text := string(FMapType.TileDownloadRequestBuilderConfig.RequestHeader);
  finally
    FMapType.TileDownloadRequestBuilderConfig.UnlockRead;
  end;

  synedtParams.Text := FMapType.Zmp.DataProvider.ReadString('params.txt', '');
  synedtInfo.Text := FMapType.Zmp.DataProvider.ReadString('info.txt', '');
  synedtScript.Text := string(FMapType.Zmp.DataProvider.ReadAnsiString('GetUrlScript.txt', ''));

  SESleep.Value := FMapType.TileDownloaderConfig.WaitInterval;
  EditParSubMenu.Text := FMapType.GUIConfig.ParentSubMenu.Value;
  EditHotKey.HotKey := FMapType.GUIConfig.HotKey;

  FMapType.StorageConfig.LockRead;
  try
    EditNameinCache.Text := FMapType.StorageConfig.NameInCache;
    chkCacheReadOnly.Checked := FMapType.StorageConfig.IsReadOnly;
    chkCacheReadOnly.Enabled := not FMapType.Zmp.StorageConfig.Abilities.IsReadOnly;
    if not (FMapType.StorageConfig.CacheTypeCode in [c_File_Cache_Id_GE, c_File_Cache_Id_GC]) then begin
      pnlCacheType.Visible := True;
      pnlCacheType.Enabled := True;
      FfrCacheTypesList.IntCode := FMapType.StorageConfig.CacheTypeCode;
    end else begin
      // GE or GC
      pnlCacheType.Visible := False;
      pnlCacheType.Enabled := False;
    end;
  finally
    FMapType.StorageConfig.UnlockRead;
  end;
  chkBoxSeparator.Checked := FMapType.GUIConfig.Separator;
  CheckEnabled.Checked := FMapType.GUIConfig.Enabled;
  edtVersion.Text := FMapType.VersionRequestConfig.Version;
  VDownloadState := FMapType.TileDownloadSubsystem.State.GetStatic;

  // download availability
  if VDownloadState.Enabled then begin
    mmoDownloadState.Text := _('Download Enabled');
  end else begin
    mmoDownloadState.Text := VDownloadState.DisableReason;
  end;
  chkDownloadEnabled.Checked := FMapType.Abilities.UseDownload;

  // check storage write access
  if (FMapType.TileStorage.State.GetStatic.WriteAccess = asDisabled) then begin
    mmoDownloadState.Lines.Add(_('No write access to tile storage'));
  end;

  Result := ShowModal = mrOk;
end;

procedure TfrmMapTypeEdit.BuildTreeViewMenu;
var
  I: Integer;
  VRoot, VDefNode: TTreeNode;
begin
  // hide all tabs
  for I := 0 to PageControl1.PageCount - 1 do begin
    PageControl1.Pages[I].TabVisible := False;
  end;

  // build menu
  VRoot := tvMenu.Items.AddChildObjectFirst(nil, _('Settings'), nil);
  VDefNode := tvMenu.Items.AddChildObject(VRoot, _('Internet'), tsInternet);
  tvMenu.Items.AddChildObject(VRoot, _('Cache and Other'), tsOthers);

  VRoot := tvMenu.Items.AddChildObject(nil, _('Listings'), nil);
  tvMenu.Items.AddChildObject(VRoot, 'Params.txt', tsParams);
  tvMenu.Items.AddChildObject(VRoot, 'GetUrlScript.txt', tsGetURLScript);
  tvMenu.Items.AddChildObject(VRoot, 'Info.txt', tsInfo);

  tvMenu.FullExpand;

  // show default tab
  VDefNode.Selected := True;
  tvMenuClick(Self);
end;

procedure TfrmMapTypeEdit.tvMenuClick(Sender: TObject);
var
  VTreeIndex: Integer;
  VNewPageIndex, VActivePageIndex: Integer;
begin
  if not Assigned(tvMenu.Selected.Data) then begin
    VTreeIndex := tvMenu.Selected.AbsoluteIndex;
    Inc(VTreeIndex);
    tvMenu.Items[VTreeIndex].Selected := True;
    Assert(Assigned(tvMenu.Selected.Data));
  end;
  VNewPageIndex := TTabSheet(tvMenu.Selected.Data).PageIndex;
  VActivePageIndex := PageControl1.ActivePageIndex;
  if VNewPageIndex <> VActivePageIndex then begin
    PageControl1.ActivePageIndex := VNewPageIndex;
  end;
end;

procedure TfrmMapTypeEdit.tvMenuCollapsing(
  Sender: TObject;
  Node: TTreeNode;
  var AllowCollapse: Boolean
);
begin
  AllowCollapse := False;
end;

procedure TfrmMapTypeEdit.CreateSynEditTextHighlighters;

  procedure SetProps(
    ASynEdit: TSynEdit;
    AParent: TWinControl
  );
  begin
    with ASynEdit do begin
      Parent := AParent;
      Align := alClient;
      Gutter.Visible := False;
      ReadOnly := True;
      ScrollBars := ssVertical;
      FontSmoothing := fsmNone;
      WordWrap := True;
      DoubleBuffered := True;
    end;
  end;

begin
  synedtParams := TSynEditBuilder.SynEditWithIniHighlighter(Self);
  SetProps(synedtParams, tsParams);

  synedtScript := TSynEditBuilder.SynEditWithPasHighlighter(Self);
  SetProps(synedtScript, tsGetURLScript);

  synedtInfo := TSynEditBuilder.SynEditWithHtmlHighlighter(Self);
  SetProps(synedtInfo, tsInfo);
end;

end.

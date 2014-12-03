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
  i_MapType,
  i_LanguageManager,
  i_TileStorageTypeList,
  u_CommonFormAndFrameParents;

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
    CBCacheType: TComboBox;
    lblCacheType: TLabel;
    btnResetCacheType: TButton;
    pnlBottomButtons: TPanel;
    pnlSeparator: TPanel;
    pnlCacheType: TPanel;
    grdpnlHotKey: TGridPanel;
    grdpnlSleep: TGridPanel;
    grdpnlSleepAndKey: TGridPanel;
    pnlParentItem: TPanel;
    pnlCacheName: TPanel;
    pnlUrl: TPanel;
    pnlUrlRight: TPanel;
    lblHotKey: TLabel;
    CheckEnabled: TCheckBox;
    pnlTop: TPanel;
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
    tsMaps: TTabSheet;
    tsParams: TTabSheet;
    tsGetURLScript: TTabSheet;
    tsInfo: TTabSheet;
    mmoParams: TMemo;
    mmoScript: TMemo;
    mmoInfo: TMemo;
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
  private
    FTileStorageTypeList: ITileStorageTypeListStatic;
    FMapType: IMapType;
  public
    function EditMapModadl(const AMapType: IMapType): Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ATileStorageTypeList: ITileStorageTypeListStatic
    );
  end;

implementation

uses
  t_CommonTypes,
  c_CacheTypeCodes,
  i_TileDownloaderState,
  u_SafeStrUtil,
  u_GlobalState,
  u_ResStrings;

{$R *.dfm}

constructor TfrmMapTypeEdit.Create(
  const ALanguageManager: ILanguageManager;
  const ATileStorageTypeList: ITileStorageTypeListStatic
);
begin
  inherited Create(ALanguageManager);
  FTileStorageTypeList := ATileStorageTypeList;
end;

function GetCacheIdFromIndex(const AIndex: Integer): Byte;
begin
  case AIndex of
    0: begin
      Result := c_File_Cache_Id_DEFAULT;
    end;
    1: begin
      Result := c_File_Cache_Id_GMV;
    end;
    2: begin
      Result := c_File_Cache_Id_SAS;
    end;
    3: begin
      Result := c_File_Cache_Id_ES;
    end;
    4: begin
      Result := c_File_Cache_Id_GM;
    end;
    5: begin
      Result := c_File_Cache_Id_BDB;
    end;
    6: begin
      Result := c_File_Cache_Id_BDB_Versioned;
    end;
    7: begin
      Result := c_File_Cache_Id_DBMS;
    end;
    8: begin
      Result := c_File_Cache_Id_RAM;
    end;
    9: begin
      Result := c_File_Cache_Id_Mobile_Atlas;
    end;
  else begin
    Result := c_File_Cache_Id_DEFAULT;
  end;
  end;
end;

function GetIndexFromCacheId(const ACacheId: Byte): Integer;
begin
  case ACacheId of
    c_File_Cache_Id_DEFAULT: begin
      Result := 0;
    end;
    c_File_Cache_Id_GMV: begin
      Result := 1;
    end;
    c_File_Cache_Id_SAS: begin
      Result := 2;
    end;
    c_File_Cache_Id_ES: begin
      Result := 3;
    end;
    c_File_Cache_Id_GM: begin
      Result := 4;
    end;
    c_File_Cache_Id_BDB: begin
      Result := 5;
    end;
    c_File_Cache_Id_BDB_Versioned: begin
      Result := 6;
    end;
    c_File_Cache_Id_DBMS: begin
      Result := 7;
    end;
    c_File_Cache_Id_RAM: begin
      Result := 8;
    end;
    c_File_Cache_Id_Mobile_Atlas: begin
      Result := 9;
    end
  else begin
    Result := 0;
  end;
  end;
end;

procedure TfrmMapTypeEdit.btnResetHeaderClick(Sender: TObject);
begin
  mmoHeader.Text := FMapType.Zmp.TileDownloadRequestBuilderConfig.RequestHeader;
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
      FMapType.StorageConfig.CacheTypeCode := GetCacheIdFromIndex(CBCacheType.ItemIndex);
    end;
  finally
    FMapType.StorageConfig.UnlockWrite;
  end;
  FMapType.VersionRequestConfig.Version := FMapType.VersionRequestConfig.VersionFactory.GetStatic.CreateByStoreString(edtVersion.Text);
  FMapType.Abilities.UseDownload := chkDownloadEnabled.Checked;

  ModalResult := mrOk;
end;

procedure TfrmMapTypeEdit.btnResetVersionClick(Sender: TObject);
begin
  edtVersion.Text := FMapType.Zmp.VersionConfig.StoreString;
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
  EditURL.Text := FMapType.Zmp.TileDownloadRequestBuilderConfig.UrlBase;
  mmoHeader.Text := FMapType.Zmp.TileDownloadRequestBuilderConfig.RequestHeader;

  EditNameinCache.Text := FMapType.Zmp.StorageConfig.NameInCache;
  SESleep.Value := FMapType.Zmp.TileDownloaderConfig.WaitInterval;
  EditHotKey.HotKey := FMapType.Zmp.GUI.HotKey;

  if not (FMapType.StorageConfig.CacheTypeCode in [c_File_Cache_Id_GE, c_File_Cache_Id_GC]) then begin
    CBCacheType.ItemIndex := GetIndexFromCacheId(FMapType.Zmp.StorageConfig.CacheTypeCode);
  end;

  EditParSubMenu.Text := FMapType.GUIConfig.ParentSubMenu.GetDefaultValue;
  chkBoxSeparator.Checked := FMapType.Zmp.GUI.Separator;
  CheckEnabled.Checked := FMapType.Zmp.GUI.Enabled;
  edtVersion.Text := FMapType.Zmp.VersionConfig.StoreString;
end;

procedure TfrmMapTypeEdit.btnResetUrlClick(Sender: TObject);
begin
  EditURL.Text := FMapType.Zmp.TileDownloadRequestBuilderConfig.UrlBase;
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
    CBCacheType.ItemIndex := GetIndexFromCacheId(FMapType.Zmp.StorageConfig.CacheTypeCode);
  end;
end;

function TfrmMapTypeEdit.EditMapModadl(const AMapType: IMapType): Boolean;
var
  VDownloadState: ITileDownloaderStateStatic;
begin
  FMapType := AMapType;

  Caption := SAS_STR_EditMap + ' ' + FMapType.GUIConfig.Name.Value;
  edtZmp.Text := AMapType.Zmp.FileName;

  FMapType.TileDownloadRequestBuilderConfig.LockRead;
  try
    EditURL.Text := FMapType.TileDownloadRequestBuilderConfig.UrlBase;
    mmoHeader.Text := FMapType.TileDownloadRequestBuilderConfig.RequestHeader;
  finally
    FMapType.TileDownloadRequestBuilderConfig.UnlockRead;
  end;

  mmoParams.Text := FMapType.Zmp.DataProvider.ReadString('params.txt', '');
  mmoInfo.Text := FMapType.Zmp.DataProvider.ReadString('info.txt', '');
  mmoScript.Text := FMapType.Zmp.DataProvider.ReadString('GetUrlScript.txt', '');


  SESleep.Value := FMapType.TileDownloaderConfig.WaitInterval;
  EditParSubMenu.Text := FMapType.GUIConfig.ParentSubMenu.Value;
  EditHotKey.HotKey := FMapType.GUIConfig.HotKey;

  FMapType.StorageConfig.LockRead;
  try
    EditNameinCache.Text := FMapType.StorageConfig.NameInCache;

    if not (FMapType.StorageConfig.CacheTypeCode in [c_File_Cache_Id_GE, c_File_Cache_Id_GC]) then begin
      pnlCacheType.Visible := True;
      pnlCacheType.Enabled := True;
      CBCacheType.ItemIndex := GetIndexFromCacheId(FMapType.StorageConfig.CacheTypeCode);
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
  edtVersion.Text := FMapType.VersionRequestConfig.Version.StoreString;
  pnlHeader.Visible := GState.Config.InternalDebugConfig.IsShowDebugInfo;
  VDownloadState := FMapType.TileDownloadSubsystem.State.GetStatic;

  // download availability
  if VDownloadState.Enabled then begin
    mmoDownloadState.Text := SAS_STR_Yes;
  end else begin
    mmoDownloadState.Text := VDownloadState.DisableReason;
  end;
  chkDownloadEnabled.Checked := FMapType.Abilities.UseDownload;

  // check storage write access
  if (FMapType.TileStorage.State.GetStatic.WriteAccess = asDisabled) then begin
    mmoDownloadState.Lines.Add('No write access to tile storage');
  end;

  Result := ShowModal = mrOk;
end;

end.

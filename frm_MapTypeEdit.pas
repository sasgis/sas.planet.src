{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit frm_MapTypeEdit;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Spin,
  u_CommonFormAndFrameParents,
  u_MapType,
  u_ResStrings;

type
  TfrmMapTypeEdit = class(TCommonFormParent)
    EditNameinCache: TEdit;
    EditParSubMenu: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    EditHotKey: THotKey;
    btnOk: TButton;
    btnCancel: TButton;
    btnByDefault: TButton;
    Button6: TButton;
    Button4: TButton;
    Button5: TButton;
    Button7: TButton;
    EditURL: TMemo;
    SESleep: TSpinEdit;
    Label6: TLabel;
    Button8: TButton;
    CBCacheType: TComboBox;
    Label5: TLabel;
    Button9: TButton;
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
    Label4: TLabel;
    CheckEnabled: TCheckBox;
    pnlTop: TPanel;
    lblZmpName: TLabel;
    edtZmp: TEdit;
    pnlVersion: TPanel;
    btnVersionReset: TButton;
    edtVersion: TEdit;
    lblVersion: TLabel;
    pnlHeader: TPanel;
    lblHeader: TLabel;
    pnlHeaderReset: TPanel;
    btnHeaderReset: TButton;
    mmoHeader: TMemo;
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnByDefaultClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure btnVersionResetClick(Sender: TObject);
    procedure btnHeaderResetClick(Sender: TObject);
  private
    FMapType: TMapType;
  public
    function EditMapModadl(AMapType: TMapType): Boolean;
  end;

implementation

uses
  u_GlobalState;

{$R *.dfm}

procedure TfrmMapTypeEdit.btnHeaderResetClick(Sender: TObject);
begin
  mmoHeader.Text := FMapType.Zmp.TileRequestBuilderConfig.RequestHeader;
end;

procedure TfrmMapTypeEdit.btnOkClick(Sender: TObject);
begin
  FmapType.TileRequestBuilderConfig.UrlBase := EditURL.Text;

  FMapType.GUIConfig.LockWrite;
  try
    FmapType.GUIConfig.ParentSubMenu.Value:=EditParSubMenu.Text;
    FmapType.GUIConfig.HotKey:=EditHotKey.HotKey;
    FMapType.GUIConfig.Enabled:=CheckEnabled.Checked;
    FmapType.GUIConfig.separator:=CheckBox1.Checked;
  finally
    FMapType.GUIConfig.UnlockWrite;
  end;

  FmapType.TileDownloaderConfig.WaitInterval:=SESleep.Value;
  FmapType.StorageConfig.LockWrite;
  try
    FmapType.StorageConfig.NameInCache := EditNameinCache.Text;
    if FMapType.StorageConfig.CacheTypeCode <> 5 then begin
      if CBCacheType.ItemIndex > 0 then begin
        FMapType.StorageConfig.CacheTypeCode := CBCacheType.ItemIndex;
      end else begin
        FMapType.StorageConfig.CacheTypeCode := 0;
      end;
    end;
  finally
    FmapType.StorageConfig.UnlockWrite;
  end;
  FMapType.VersionConfig.Version := edtVersion.Text;
  FMapType.TileRequestBuilderConfig.RequestHeader := mmoHeader.Text;

  ModalResult := mrOk;
end;

procedure TfrmMapTypeEdit.btnVersionResetClick(Sender: TObject);
begin
  edtVersion.Text := FMapType.Zmp.VersionConfig.Version;
end;

procedure TfrmMapTypeEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FmapType:=nil;
end;

procedure TfrmMapTypeEdit.btnByDefaultClick(Sender: TObject);
begin
  EditURL.Text := FmapType.Zmp.TileRequestBuilderConfig.UrlBase;
  EditNameinCache.Text := FMapType.Zmp.StorageConfig.NameInCache;
  SESleep.Value:=FmapType.Zmp.TileDownloaderConfig.WaitInterval;
  EditHotKey.HotKey:=FmapType.Zmp.GUI.HotKey;
  if FMapType.StorageConfig.CacheTypeCode <> 5 then begin
    CBCacheType.ItemIndex := FmapType.Zmp.StorageConfig.CacheTypeCode;
  end;

  EditParSubMenu.Text:=FmapType.GUIConfig.ParentSubMenu.GetDefaultValue;
  CheckBox1.Checked:=FmapType.Zmp.GUI.Separator;
  CheckEnabled.Checked:=FMapType.Zmp.GUI.Enabled;
  edtVersion.Text := FMapType.Zmp.VersionConfig.Version;
  mmoHeader.Text := FMapType.Zmp.TileRequestBuilderConfig.RequestHeader;
end;

procedure TfrmMapTypeEdit.Button6Click(Sender: TObject);
begin
 EditURL.Text := FMapType.Zmp.TileRequestBuilderConfig.UrlBase;
end;

procedure TfrmMapTypeEdit.Button4Click(Sender: TObject);
begin
  EditNameinCache.Text := FMapType.Zmp.StorageConfig.NameInCache;
end;

procedure TfrmMapTypeEdit.Button5Click(Sender: TObject);
begin
  EditParSubMenu.Text := FmapType.GUIConfig.ParentSubMenu.GetDefaultValue;
end;

procedure TfrmMapTypeEdit.Button7Click(Sender: TObject);
begin
 EditHotKey.HotKey := FMapType.Zmp.GUI.HotKey;
end;

procedure TfrmMapTypeEdit.Button8Click(Sender: TObject);
begin
  SESleep.Value := FMapType.TileDownloaderConfig.WaitInterval;
end;

procedure TfrmMapTypeEdit.Button9Click(Sender: TObject);
begin
  if FMapType.StorageConfig.CacheTypeCode <> 5 then begin
    CBCacheType.ItemIndex := FMapType.Zmp.StorageConfig.CacheTypeCode;
  end;
end;

function TfrmMapTypeEdit.EditMapModadl(AMapType: TMapType): Boolean;
begin
  FMapType := AMapType;

  Caption:=SAS_STR_EditMap+' '+FmapType.GUIConfig.Name.Value;
  edtZmp.Text := AMapType.Zmp.FileName;
  EditURL.Text:=FMapType.TileRequestBuilderConfig.UrlBase;

  SESleep.Value:=FMapType.TileDownloaderConfig.WaitInterval;
  EditParSubMenu.Text := FMapType.GUIConfig.ParentSubMenu.Value;
  EditHotKey.HotKey:=FMapType.GUIConfig.HotKey;

  FMapType.StorageConfig.LockRead;
  try
    EditNameinCache.Text := FMapType.StorageConfig.NameInCache;
    if FMapType.StorageConfig.CacheTypeCode <> 5 then begin
      pnlCacheType.Visible := True;
      pnlCacheType.Enabled := True;
      CBCacheType.ItemIndex := FMapType.StorageConfig.CacheTypeCode;
    end else begin
      pnlCacheType.Visible := False;
      pnlCacheType.Enabled := False;
    end;
  finally
    FMapType.StorageConfig.UnlockRead;
  end;
  CheckBox1.Checked:=FMapType.GUIConfig.separator;
  CheckEnabled.Checked:=FMapType.GUIConfig.Enabled;
  edtVersion.Text := FMapType.VersionConfig.Version;
  mmoHeader.Text := FMapType.TileRequestBuilderConfig.RequestHeader;
  pnlHeader.Visible := GState.GlobalAppConfig.IsShowDebugInfo;

  Result := ShowModal = mrOk;
end;

end.

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

var
  frmMapTypeEdit: TfrmMapTypeEdit;

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
 FmapType.TileStorage.CacheConfig.NameInCache:=EditNameinCache.Text;
 FmapType.ParentSubMenu:=EditParSubMenu.Text;
 FmapType.TileDownloaderConfig.WaitInterval:=SESleep.Value;
 FmapType.HotKey:=EditHotKey.HotKey;
 FMapType.Enabled:=CheckEnabled.Checked;
 if CBCacheType.ItemIndex > 0 then begin
   FmapType.TileStorage.CacheConfig.cachetype:=CBCacheType.ItemIndex;
 end else begin
   FmapType.TileStorage.CacheConfig.cachetype:=0;
 end;
 FmapType.separator:=CheckBox1.Checked;
 FMapType.VersionConfig.Version := edtVersion.Text;
 FMapType.TileRequestBuilderConfig.RequestHeader := mmoHeader.Text;
 pnlHeader.Visible := GState.GlobalAppConfig.IsShowDebugInfo;

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
 EditNameinCache.Text:=FmapType.TileStorage.CacheConfig.DefNameInCache;
 EditParSubMenu.Text:=FmapType.Zmp.ParentSubMenu;
 SESleep.Value:=FmapType.Zmp.TileDownloaderConfig.WaitInterval;
 EditHotKey.HotKey:=FmapType.Zmp.HotKey;
 CBCacheType.ItemIndex:=FmapType.TileStorage.CacheConfig.CacheType;
 CheckBox1.Checked:=FmapType.Zmp.Separator;
 CheckEnabled.Checked:=FMapType.Zmp.Enabled;
 edtVersion.Text := FMapType.Zmp.VersionConfig.Version;
 mmoHeader.Text := FMapType.Zmp.TileRequestBuilderConfig.RequestHeader;
end;

procedure TfrmMapTypeEdit.Button6Click(Sender: TObject);
begin
 EditURL.Text := FMapType.Zmp.TileRequestBuilderConfig.UrlBase;
end;

procedure TfrmMapTypeEdit.Button4Click(Sender: TObject);
begin
 EditNameinCache.Text := FMapType.TileStorage.CacheConfig.DefNameInCache;
end;

procedure TfrmMapTypeEdit.Button5Click(Sender: TObject);
begin
 EditParSubMenu.Text := FMapType.Zmp.ParentSubMenu;
end;

procedure TfrmMapTypeEdit.Button7Click(Sender: TObject);
begin
 EditHotKey.HotKey := FMapType.Zmp.HotKey;
end;

procedure TfrmMapTypeEdit.Button8Click(Sender: TObject);
begin
  SESleep.Value := FMapType.TileDownloaderConfig.WaitInterval;
end;

procedure TfrmMapTypeEdit.Button9Click(Sender: TObject);
begin
  CBCacheType.ItemIndex := FMapType.TileStorage.CacheConfig.defcachetype;
end;

function TfrmMapTypeEdit.EditMapModadl(AMapType: TMapType): Boolean;
begin
  FMapType := AMapType;

  Caption:=SAS_STR_EditMap+' '+FmapType.name;
  edtZmp.Text := AMapType.Zmp.FileName;
  EditURL.Text:=FMapType.TileRequestBuilderConfig.UrlBase;
  EditNameinCache.Text:=FMapType.TileStorage.CacheConfig.NameInCache;
  SESleep.Value:=FMapType.TileDownloaderConfig.WaitInterval;
  EditParSubMenu.Text:=FMapType.ParentSubMenu;
  EditHotKey.HotKey:=FMapType.HotKey;
  CBCacheType.ItemIndex:=FMapType.TileStorage.CacheConfig.cachetype;
  CheckBox1.Checked:=FMapType.separator;
  CheckEnabled.Checked:=FMapType.Enabled;
  edtVersion.Text := FMapType.VersionConfig.Version;
  mmoHeader.Text := FMapType.TileRequestBuilderConfig.RequestHeader;

  Result := ShowModal = mrOk;
end;

end.

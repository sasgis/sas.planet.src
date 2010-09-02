unit UEditMap;

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
  UMapType,
  UResStrings;

type
  TFEditMap = class(TForm)
    EditNameinCache: TEdit;
    EditParSubMenu: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    EditHotKey: THotKey;
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
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
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    FMapType: TMapType;
  public
    function EditMapModadl(AMapType: TMapType): Boolean;
  end;

var
  FEditMap: TFEditMap;

implementation

uses
  u_MapTypeCacheConfig;

{$R *.dfm}

procedure TFEditMap.Button1Click(Sender: TObject);
begin
 FmapType.UrlGenerator.URLBase:=EditURL.Text;
 FmapType.CacheConfig.NameInCache:=EditNameinCache.Text;
 FmapType.ParentSubMenu:=EditParSubMenu.Text;
 FmapType.DownloaderFactory.WaitInterval:=SESleep.Value;
 FmapType.HotKey:=EditHotKey.HotKey;
 if CBCacheType.ItemIndex > 0 then begin
   FmapType.CacheConfig.cachetype:=CBCacheType.ItemIndex;
 end else begin
   FmapType.CacheConfig.cachetype:=0;
 end;
 FmapType.separator:=CheckBox1.Checked;
 ModalResult := mrOk;
end;

procedure TFEditMap.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FmapType:=nil;
end;

procedure TFEditMap.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFEditMap.Button3Click(Sender: TObject);
begin
 EditURL.Text:=FmapType.UrlGenerator.DefURLBase;
 EditNameinCache.Text:=FmapType.CacheConfig.DefNameInCache;
 EditParSubMenu.Text:=FmapType.DefParentSubMenu;
 SESleep.Value:=FmapType.DownloaderFactory.WaitInterval;
 EditHotKey.HotKey:=FmapType.DefHotKey;
 CBCacheType.ItemIndex:=FmapType.CacheConfig.CacheType;
 CheckBox1.Checked:=FmapType.Defseparator;
end;

procedure TFEditMap.Button6Click(Sender: TObject);
begin
 EditURL.Text := FMapType.UrlGenerator.DefURLBase;
end;

procedure TFEditMap.Button4Click(Sender: TObject);
begin
 EditNameinCache.Text := FMapType.CacheConfig.DefNameInCache;
end;

procedure TFEditMap.Button5Click(Sender: TObject);
begin
 EditParSubMenu.Text := FMapType.DefParentSubMenu;
end;

procedure TFEditMap.Button7Click(Sender: TObject);
begin
 EditHotKey.HotKey := FMapType.DefHotKey;
end;

procedure TFEditMap.Button8Click(Sender: TObject);
begin
 SESleep.Value := FMapType.DefSleep;
end;

procedure TFEditMap.Button9Click(Sender: TObject);
begin
  CBCacheType.ItemIndex := FMapType.CacheConfig.defcachetype;
end;

function TFEditMap.EditMapModadl(AMapType: TMapType): Boolean;
begin
  FMapType := AMapType;

  FEditMap.Caption:=SAS_STR_EditMap+' '+FmapType.name;
  EditURL.Text:=FMapType.UrlGenerator.URLBase;
  EditNameinCache.Text:=FMapType.CacheConfig.NameInCache;
  SESleep.Value:=FMapType.DownloaderFactory.WaitInterval;
  EditParSubMenu.Text:=FMapType.ParentSubMenu;
  EditHotKey.HotKey:=FMapType.HotKey;
  CBCacheType.ItemIndex:=FMapType.CacheConfig.cachetype;
  CheckBox1.Checked:=FMapType.separator;

  Result := ShowModal = mrOk;
end;

end.

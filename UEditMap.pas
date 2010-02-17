unit UEditMap;

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
    RBCacheType: TRadioGroup;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
  public
    FMapType: TMapType;
  end;

var
  FEditMap: TFEditMap;

implementation

uses
  unit1;

{$R *.dfm}

procedure TFEditMap.FormShow(Sender: TObject);
begin
 FEditMap.Caption:=SAS_STR_EditMap+' '+FmapType.name;
 EditURL.Text:=FMapType.URLBase;
 EditNameinCache.Text:=FMapType.NameInCache;
 SESleep.Value:=FMapType.Sleep;
 EditParSubMenu.Text:=FMapType.ParentSubMenu;
 EditHotKey.HotKey:=FMapType.HotKey;
 RBCacheType.ItemIndex:=FMapType.cachetype;
 CheckBox1.Checked:=FMapType.separator;
end;

procedure TFEditMap.Button1Click(Sender: TObject);
begin
 MapsEdit:=true;
 FmapType.URLBase:=EditURL.Text;
 FmapType.NameInCache:=EditNameinCache.Text;
 FmapType.ParentSubMenu:=EditParSubMenu.Text;
 FmapType.Sleep:=SESleep.Value;
 FmapType.HotKey:=EditHotKey.HotKey;
 FmapType.cachetype:=RBCacheType.ItemIndex;
 FmapType.separator:=CheckBox1.Checked;
 CreateMapUI;
 Fmain.generate_im;
 close;
end;

procedure TFEditMap.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FmapType:=nil;
end;

procedure TFEditMap.Button2Click(Sender: TObject);
begin
 close;
end;

procedure TFEditMap.Button3Click(Sender: TObject);
begin
 EditURL.Text:=FmapType.DefURLBase;
 EditNameinCache.Text:=FmapType.DefNameInCache;
 EditParSubMenu.Text:=FmapType.DefParentSubMenu;
 SESleep.Value:=FmapType.Sleep;
 EditHotKey.HotKey:=FmapType.DefHotKey;
 RBCacheType.ItemIndex:=FmapType.cachetype;
 CheckBox1.Checked:=FmapType.Defseparator;
end;

procedure TFEditMap.Button6Click(Sender: TObject);
begin
 EditURL.Text := FMapType.DefURLBase;
end;

procedure TFEditMap.Button4Click(Sender: TObject);
begin
 EditNameinCache.Text := FMapType.DefNameInCache;
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

end.

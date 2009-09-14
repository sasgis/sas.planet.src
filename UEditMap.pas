unit UEditMap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UMapType, Spin, ComCtrls, UResStrings;

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
    AmapType:PMapType;
  end;

var
  FEditMap: TFEditMap;

implementation
uses unit1;
{$R *.dfm}

procedure TFEditMap.FormShow(Sender: TObject);
begin
 FEditMap.Caption:=SAS_STR_EditMap+' '+AmapType.name;
 EditURL.Text:=AmapType.URLBase;
 EditNameinCache.Text:=AmapType.NameInCache;
 SESleep.Value:=AmapType.Sleep;
 EditParSubMenu.Text:=AmapType.ParentSubMenu;
 EditHotKey.HotKey:=AmapType.HotKey;
 RBCacheType.ItemIndex:=AmapType.cachetype;
 CheckBox1.Checked:=AmapType.separator;
end;

procedure TFEditMap.Button1Click(Sender: TObject);
begin
 MapsEdit:=true;
 AmapType.URLBase:=EditURL.Text;
 AmapType.NameInCache:=EditNameinCache.Text;
 AmapType.ParentSubMenu:=EditParSubMenu.Text;
 AmapType.Sleep:=SESleep.Value;
 AmapType.HotKey:=EditHotKey.HotKey;
 AmapType.cachetype:=RBCacheType.ItemIndex;
 AmapType.separator:=CheckBox1.Checked;
 CreateMapUI;
 Fmain.generate_im(nilLastLoad,'');
 close;
end;

procedure TFEditMap.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 AmapType:=nil;
end;

procedure TFEditMap.Button2Click(Sender: TObject);
begin
 close;
end;

procedure TFEditMap.Button3Click(Sender: TObject);
begin
 EditURL.Text:=AmapType.DefURLBase;
 EditNameinCache.Text:=AmapType.DefNameInCache;
 EditParSubMenu.Text:=AmapType.DefParentSubMenu;
 SESleep.Value:=AmapType.Sleep;
 EditHotKey.HotKey:=AmapType.DefHotKey;
 RBCacheType.ItemIndex:=AmapType.cachetype;
 CheckBox1.Checked:=AmapType.Defseparator;
end;

procedure TFEditMap.Button6Click(Sender: TObject);
begin
 EditURL.Text:=AmapType.DefURLBase;
end;

procedure TFEditMap.Button4Click(Sender: TObject);
begin
 EditNameinCache.Text:=AmapType.DefNameInCache;
end;

procedure TFEditMap.Button5Click(Sender: TObject);
begin
 EditParSubMenu.Text:=AmapType.DefParentSubMenu;
end;

procedure TFEditMap.Button7Click(Sender: TObject);
begin
 EditHotKey.HotKey:=AmapType.DefHotKey;
end;

procedure TFEditMap.Button8Click(Sender: TObject);
begin
 SESleep.Value:=AMapType.DefSleep;
end;

end.

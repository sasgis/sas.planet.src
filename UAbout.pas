unit UAbout;

interface

uses
  Windows,
  Forms,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls;

type
  TFabout = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelName: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Label7: TLabel;
    LabelVer: TLabel;
    Label2: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label10Click(Sender: TObject);
    procedure Label8Click(Sender: TObject);
  private
  public
  end;

var
  Fabout: TFabout;

implementation

uses
  u_GlobalState,
  Unit1;

{$R *.dfm}

procedure TFabout.Button1Click(Sender: TObject);
begin
 close;
end;

procedure TFabout.FormCreate(Sender: TObject);
begin
 LabelVer.Caption:=SASVersion;
end;

procedure TFabout.Label10Click(Sender: TObject);
begin
 Fmain.OpenUrlInBrowser('http://sasgis.ru');
end;

procedure TFabout.Label8Click(Sender: TObject);
begin
 Fmain.OpenUrlInBrowser('mailto:'+Label8.Caption);
end;

end.

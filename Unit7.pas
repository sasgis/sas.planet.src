unit Unit7;
interface

uses                    
  Windows, Forms, Classes, Controls, StdCtrls, ExtCtrls;

type
  TFabout = class(TForm)
    Panel1: TPanel;
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
    procedure Panel1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  Fabout: TFabout;

implementation 

uses UProgress, Unit1;

{$R *.dfm}

procedure TFabout.Panel1Click(Sender: TObject);
begin
 Fmain.ShowCaptcha('http://sasgis.ru');
end;

procedure TFabout.Button1Click(Sender: TObject);
begin
 close;
end;

procedure TFabout.FormCreate(Sender: TObject);
begin
 LabelVer.Caption:=SASVersion;
end;

end.

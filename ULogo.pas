unit ULogo;

interface

uses
  Forms,
  Classes,
  GR32_Image,
  Controls,
  ExtCtrls,
  StdCtrls;

type
  TFLogo = class(TForm)
    Timer1: TTimer;
    Image321: TImage32;
    Label1: TLabel;
    Label2: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image321Click(Sender: TObject);
  private
  public
  end;

var
  FLogo: TFLogo;

implementation
{$R *.dfm}
uses
  u_GlobalState;

procedure TFLogo.Timer1Timer(Sender: TObject);
begin
 timer1.Enabled:=false;
 FLogo.Close;
end;

procedure TFLogo.FormShow(Sender: TObject);
begin
  GState.LoadBitmapFromJpegRes('LOGOI', Image321.Bitmap);
end;

procedure TFLogo.Image321Click(Sender: TObject);
begin
   FLogo.Close;
   timer1.Enabled:=false;
end;

end.

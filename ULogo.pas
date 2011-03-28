unit ULogo;

interface

uses
  Forms,
  Classes,
  GR32_Image,
  Controls,
  ExtCtrls,
  StdCtrls,
  u_CommonFormAndFrameParents;

type
  TFLogo = class(TCommonFormParent)
    tmrLogo: TTimer;
    imgLogo: TImage32;
    lblVersion: TLabel;
    lblWebSite: TLabel;
    procedure tmrLogoTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure imgLogoClick(Sender: TObject);
  private
    FReadyToHide: Boolean;
  public
    class procedure ShowLogo;
    class procedure ReadyToHideLogo;
  end;


implementation

uses
  c_SasVersion,
  u_GlobalState;

var
  FLogo: TFLogo;

{$R *.dfm}

procedure TFLogo.tmrLogoTimer(Sender: TObject);
begin
  tmrLogo.Enabled:=false;
  Self.Close;
end;

procedure TFLogo.FormShow(Sender: TObject);
begin
  GState.LoadBitmapFromJpegRes('LOGOI', imgLogo.Bitmap);
  lblVersion.Caption:='v '+SASVersion;
  FReadyToHide := False;
end;

procedure TFLogo.imgLogoClick(Sender: TObject);
begin
  if FReadyToHide then begin
    tmrLogo.Enabled := false;
    Self.Close;
  end;
end;

class procedure TFLogo.ReadyToHideLogo;
begin
  if FLogo <> nil then begin
    FLogo.FReadyToHide := True;
    FLogo.tmrLogo.Enabled := True;
  end;
end;

class procedure TFLogo.ShowLogo;
begin
  FLogo := TFLogo.Create(Application);
  FLogo.Show;
  Application.ProcessMessages;
end;

end.

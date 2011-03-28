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
  TfrmStartLogo = class(TCommonFormParent)
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
  frmStartLogo: TfrmStartLogo;

{$R *.dfm}

procedure TfrmStartLogo.tmrLogoTimer(Sender: TObject);
begin
  tmrLogo.Enabled:=false;
  Self.Close;
end;

procedure TfrmStartLogo.FormShow(Sender: TObject);
begin
  GState.LoadBitmapFromJpegRes('LOGOI', imgLogo.Bitmap);
  lblVersion.Caption:='v '+SASVersion;
  FReadyToHide := False;
end;

procedure TfrmStartLogo.imgLogoClick(Sender: TObject);
begin
  if FReadyToHide then begin
    tmrLogo.Enabled := false;
    Self.Close;
  end;
end;

class procedure TfrmStartLogo.ReadyToHideLogo;
begin
  if frmStartLogo <> nil then begin
    frmStartLogo.FReadyToHide := True;
    frmStartLogo.tmrLogo.Enabled := True;
  end;
end;

class procedure TfrmStartLogo.ShowLogo;
begin
  frmStartLogo := TfrmStartLogo.Create(Application);
  frmStartLogo.Show;
  Application.ProcessMessages;
end;

end.

unit frm_StartLogo;

interface

uses
  Forms,
  Classes,
  GR32_Image,
  Controls,
  ExtCtrls,
  StdCtrls,
  i_StartUpLogoConfig,
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
    FConfig: IStartUpLogoConfig;
  public
    constructor Create(AOwner: TComponent; AConfig: IStartUpLogoConfig);
    class procedure ShowLogo(AConfig: IStartUpLogoConfig);
    class procedure ReadyToHideLogo;
  end;


implementation

uses
  Types,
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

constructor TfrmStartLogo.Create(AOwner: TComponent;
  AConfig: IStartUpLogoConfig);
begin
  inherited Create(AOwner);
  FConfig := AConfig;
end;

procedure TfrmStartLogo.FormShow(Sender: TObject);
var
  VBitmapSize: TPoint;
begin
  imgLogo.Bitmap.Assign(FConfig.Logo);
  VBitmapSize.X := imgLogo.Bitmap.Width;
  VBitmapSize.Y := imgLogo.Bitmap.Height;

  if VBitmapSize.X < 100 then begin
    VBitmapSize.X := 480;
  end;
  if VBitmapSize.Y < 100 then begin
    VBitmapSize.Y := 276;
  end;
  imgLogo.Bitmap.SetSize(VBitmapSize.X, VBitmapSize.Y);
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

class procedure TfrmStartLogo.ShowLogo(AConfig: IStartUpLogoConfig);
begin
  if AConfig.IsShowLogo then begin
    frmStartLogo := TfrmStartLogo.Create(Application, AConfig);
    frmStartLogo.Show;
    Application.ProcessMessages;
  end;
end;

end.

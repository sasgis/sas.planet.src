unit UAbout;

interface

uses
  Windows,
  Forms,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  u_CommonFormAndFrameParents;

type
  TfrmAbout = class(TCommonFormParent)
    Bevel1: TBevel;
    btnClose: TButton;
    lblVersionCatpion: TLabel;
    lblAuthorCaption: TLabel;
    lblWebSiteCaption: TLabel;
    lblEMailCaption: TLabel;
    lblDonateCaption: TLabel;
    lblProgramName: TLabel;
    edtWME: TEdit;
    edtWMZ: TEdit;
    edtYandexMoney: TEdit;
    lblWMCaption: TLabel;
    lblVersion: TLabel;
    lblAuthor: TLabel;
    lblEMail: TLabel;
    lblYandexMoneyCaption: TLabel;
    lblWebSite: TLabel;
    edtWMR: TEdit;
    pnlBottom: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblWebSiteClick(Sender: TObject);
    procedure lblEMailClick(Sender: TObject);
  private
  public
  end;

var
  frmAbout: TfrmAbout;

implementation

uses
  c_SasVersion,
  frm_InvisibleBrowser;

{$R *.dfm}

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
 lblVersion.Caption:=SASVersion;
end;

procedure TfrmAbout.lblWebSiteClick(Sender: TObject);
begin
  OpenUrlInBrowser('http://sasgis.ru');
end;

procedure TfrmAbout.lblEMailClick(Sender: TObject);
begin
  OpenUrlInBrowser('mailto:'+lblEMail.Caption);
end;

end.

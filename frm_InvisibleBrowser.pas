unit frm_InvisibleBrowser;

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
  OleCtrls,
  SHDocVw_EWB,
  EwbCore,
  EmbeddedWB;

type
  TfrmInvisibleBrowser = class(TForm)
    WebBrowser1: TEmbeddedWB;
    procedure WebBrowser1Authenticate(Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName, szPassWord: WideString; var Rezult: HRESULT);
  private
    { Private declarations }
  public
    procedure NavigateAndWait(AUrl: WideString);
  end;

var
  frmInvisibleBrowser: TfrmInvisibleBrowser;

implementation

{$R *.dfm}

uses
  u_GlobalState;

{ TfrmInvisibleBrowser }

procedure TfrmInvisibleBrowser.NavigateAndWait(AUrl: WideString);
begin
  WebBrowser1.NavigateWait(AUrl, 10000);
end;

procedure TfrmInvisibleBrowser.WebBrowser1Authenticate(
  Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName,
  szPassWord: WideString; var Rezult: HRESULT);
begin
 if GState.InetConnect.uselogin then
  begin
   szUserName:=GState.InetConnect.loginstr;
   szPassWord:=GState.InetConnect.passstr;
  end;
end;

end.

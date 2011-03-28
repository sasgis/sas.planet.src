unit frm_IntrnalBrowser;

interface

uses
  Windows,
  Forms,
  Classes,
  Controls,
  OleCtrls,
  EwbCore,
  EmbeddedWB,
  SHDocVw_EWB,
  u_CommonFormAndFrameParents;

type
  TfrmIntrnalBrowser = class(TCommonFormParent)
    EmbeddedWB1: TEmbeddedWB;
    procedure EmbeddedWB1Authenticate(Sender: TCustomEmbeddedWB;
      var hwnd: HWND; var szUserName, szPassWord: WideString;
      var Rezult: HRESULT);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EmbeddedWB1KeyDown(Sender: TObject; var Key: Word;
      ScanCode: Word; Shift: TShiftState);
  private
  protected
  public
    procedure showmessage(ACaption,AText: string);
    procedure TextToWebBrowser(Text: string);
    procedure Navigate(AUrl: string);
  end;

var
  frmIntrnalBrowser: TfrmIntrnalBrowser;

implementation

uses
  i_ProxySettings,
  u_GlobalState;

{$R *.dfm}

procedure TfrmIntrnalBrowser.EmbeddedWB1Authenticate(Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName, szPassWord: WideString; var Rezult: HRESULT);
var
  VProxyConfig: IProxyConfig;
  VUseLogin: Boolean;
begin
  VProxyConfig := GState.InetConfig.ProxyConfig;
  VProxyConfig.LockRead;
  try
    VUselogin := (not VProxyConfig.GetUseIESettings) and VProxyConfig.GetUseProxy and VProxyConfig.GetUseLogin;
    if VUselogin then begin
      szUserName := VProxyConfig.GetLogin;
      szPassWord := VProxyConfig.GetPassword;
    end;
  finally
    VProxyConfig.UnlockRead;
  end;
end;

procedure TfrmIntrnalBrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 EmbeddedWB1.Stop;
end;

procedure TfrmIntrnalBrowser.Navigate(AUrl: string);
begin
  EmbeddedWB1.Navigate(AUrl);
end;

procedure TfrmIntrnalBrowser.TextToWebBrowser(Text: string);
begin
  EmbeddedWB1.HTMLCode.Text:=Text;
end;

procedure TfrmIntrnalBrowser.showmessage(ACaption,AText: string);
begin
  TextToWebBrowser(AText);
  Caption:=ACaption;
  show;
end;

procedure TfrmIntrnalBrowser.EmbeddedWB1KeyDown(Sender: TObject; var Key: Word;
  ScanCode: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    close;
  end;
end;

end.

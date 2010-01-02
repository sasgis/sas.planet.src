unit Ubrowser;

interface

uses
  Windows,
  Forms,
  Classes,
  Controls,
  OleCtrls,
  EwbCore,
  EmbeddedWB,
  SHDocVw_EWB;

type
  TFbrowser = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    procedure EmbeddedWB1Authenticate(Sender: TCustomEmbeddedWB;
      var hwnd: HWND; var szUserName, szPassWord: WideString;
      var Rezult: HRESULT);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  protected
  public
    { Public declarations }
  end;

var
  Fbrowser: TFbrowser;

implementation

uses
  SysUtils,
  u_GlobalState;

{$R *.dfm}

procedure TFbrowser.EmbeddedWB1Authenticate(Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName, szPassWord: WideString; var Rezult: HRESULT);
begin
 if GState.InetConnect.uselogin then
  begin
   szUserName:=GState.InetConnect.loginstr;
   szPassWord:=GState.InetConnect.passstr;
  end;
end;

procedure TFbrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 EmbeddedWB1.Stop;
end;

end.

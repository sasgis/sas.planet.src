unit Ubrowser;

interface

uses
  Forms, EwbCore, EmbeddedWB, SHDocVw_EWB, Classes, Controls, Windows,
  OleCtrls, StdCtrls;

type
  TFbrowser = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    procedure EmbeddedWB1Authenticate(Sender: TCustomEmbeddedWB;
      var hwnd: HWND; var szUserName, szPassWord: WideString;
      var Rezult: HRESULT);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  protected
//    procedure Loaded; override;
  public
    { Public declarations }
  end;

var
  Fbrowser: TFbrowser;

implementation

uses SysUtils, Unit1;

{$R *.dfm}


{procedure TFbrowser.Loaded;
var r:TPoint;
begin
  inherited;
  BorderStyle:=bsSizeToolWin;
  Parent:=Fmain.map;
  r:=Fmain.map.ScreenToClient(Mouse.CursorPos);
  Left:=r.x - (Width div 2);
  top:=r.y - (Height div 2);
  Position:=poDesigned;
end;     }

procedure TFbrowser.EmbeddedWB1Authenticate(Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName, szPassWord: WideString; var Rezult: HRESULT);
begin
 if InetConnect.uselogin then
  begin
   szUserName:=InetConnect.loginstr;
   szPassWord:=InetConnect.passstr;
  end;
end;

procedure TFbrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 EmbeddedWB1.Stop;
end;

end.

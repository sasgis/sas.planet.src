unit Ubrowser;

interface

uses
  Forms, EwbCore, EmbeddedWB, SHDocVw_EWB, OleCtrls, Controls, Classes;

type
  TFbrowser = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
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

end.

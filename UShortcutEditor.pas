unit UShortcutEditor;

interface

uses
  Windows,
  Forms,
  Classes,
  Controls,
  ComCtrls,
  StdCtrls,
  Buttons;

type
  TFShortcutChange = class(TForm)
    GroupBox1: TGroupBox;
    HotKey: THotKey;
    Button1: TButton;
    Button2: TButton;
    SpeedButton1: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
  end;

var
  FShortcutChange: TFShortcutChange;

implementation



{$R *.dfm}

procedure TFShortcutChange.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TFShortcutChange.Button3Click(Sender: TObject);
begin
  HotKey.HotKey := 0;
end;

procedure TFShortcutChange.FormShow(Sender: TObject);
begin
  HotKey.SetFocus;
end;

end.

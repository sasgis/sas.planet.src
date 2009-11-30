unit UProgress;

interface

uses
  Forms,
  windows,
  messages,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Controls,
  Classes,
  RarProgress,
  UResStrings;

type
  TFProgress = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    RProgr: TRarProgress;
    LabelValue0: TLabel;
    LabelValue1: TLabel;
    LabelValue2: TLabel;
    LabelValue3: TLabel;
    LabelName0: TLabel;
    LabelName1: TLabel;
    LabelName2: TLabel;
    LabelName3: TLabel;
    LabelName4: TLabel;
    LabelValue4: TLabel;
    SaveSessionDialog: TSaveDialog;
    ButtonSave: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    stop:boolean;
  end;

var
  FProgress: TFProgress;
implementation

{$R *.dfm}


procedure TFProgress.Button2Click(Sender: TObject);
begin
 close;
end;

procedure TFProgress.Button3Click(Sender: TObject);
begin
 Perform(wm_SysCommand, SC_MINIMIZE, 0)
end;

procedure TFProgress.Button1Click(Sender: TObject);
begin
 if stop then begin
               stop:=false;
               button1.Caption:=SAS_STR_Stop;
              end
         else begin
               stop:=true;
               button1.Caption:=SAS_STR_Continue;
              end
end;

procedure TFProgress.FormCreate(Sender: TObject);
begin
 stop:=false;
end;
end.

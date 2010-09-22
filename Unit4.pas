unit Unit4;

interface

uses
   Windows,
   Classes,
   Forms,
   Controls,
   StdCtrls,
   Dialogs,
   RarProgress,
  u_CommonFormAndFrameParents;

type
  TFprogress2 = class(TCommonFormParent)
    ProgressBar1: TRarProgress;
    MemoInfo: TMemo;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoInfoChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  protected
    procedure Loaded; override;
  public
  end;

var Fprogress2: TFprogress2;

implementation

uses
  Unit1;

{$R *.dfm}
procedure TFProgress2.Loaded;
begin
  inherited;
  Enabled:=true;
  BorderStyle:=bsToolWindow;
end;

procedure TFprogress2.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key=VK_ESCAPE then close;
end;

procedure TFprogress2.MemoInfoChange(Sender: TObject);
begin
 HideCaret(MemoInfo.Handle);
end;

procedure TFprogress2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 fmain.Enabled:=true;
end;

end.

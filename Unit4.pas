unit Unit4;

interface

uses
  Windows,
  Classes,
  Forms,
  Controls,
  StdCtrls,
  RarProgress,
  u_CommonFormAndFrameParents;

type
  TFprogress2 = class(TCommonFormParent)
    ProgressBar1: TRarProgress;
    MemoInfo: TMemo;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoInfoChange(Sender: TObject);
  private
  protected
  public
  end;

var Fprogress2: TFprogress2;

implementation



{$R *.dfm}

procedure TFprogress2.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then close;
end;

procedure TFprogress2.MemoInfoChange(Sender: TObject);
begin
  HideCaret(MemoInfo.Handle);
end;

end.

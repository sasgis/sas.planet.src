unit frm_ProgressSimple;

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
  TfrmProgressSimple = class(TCommonFormParent)
    ProgressBar1: TRarProgress;
    MemoInfo: TMemo;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoInfoChange(Sender: TObject);
  private
  protected
  public
  end;

implementation

{$R *.dfm}

procedure TfrmProgressSimple.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then close;
end;

procedure TfrmProgressSimple.MemoInfoChange(Sender: TObject);
begin
  HideCaret(MemoInfo.Handle);
end;

end.

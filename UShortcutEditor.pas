unit UShortcutEditor;

interface

uses
  Windows,
  Forms,
  Classes,
  Controls,
  ComCtrls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  u_CommonFormAndFrameParents;

type
  TfrmShortCutEdit = class(TCommonFormParent)
    GroupBox1: TGroupBox;
    HotKey: THotKey;
    btnOk: TButton;
    btnCancel: TButton;
    btnClear: TSpeedButton;
    pnlBottom: TPanel;
    procedure FormShow(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
  public
    
  end;

var
  frmShortCutEdit: TfrmShortCutEdit;

implementation



{$R *.dfm}

procedure TfrmShortCutEdit.btnClearClick(Sender: TObject);
begin
  HotKey.HotKey := 0;
end;

procedure TfrmShortCutEdit.FormShow(Sender: TObject);
begin
  HotKey.SetFocus;
end;

end.

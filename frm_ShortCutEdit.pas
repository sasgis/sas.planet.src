unit frm_ShortCutEdit;

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
  u_ShortcutManager,
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
    function EditHotKeyModal(AShortCutInfo: TShortCutInfo): Boolean;
  end;

var
  frmShortCutEdit: TfrmShortCutEdit;

implementation



{$R *.dfm}

procedure TfrmShortCutEdit.btnClearClick(Sender: TObject);
begin
  HotKey.HotKey := 0;
end;

function TfrmShortCutEdit.EditHotKeyModal(AShortCutInfo: TShortCutInfo): Boolean;
begin
  HotKey.HotKey := AShortCutInfo.ShortCut;
  if ShowModal = mrOK then begin
    AShortCutInfo.ShortCut := HotKey.HotKey;
  end else begin
    Result := False;
  end;
end;

procedure TfrmShortCutEdit.FormShow(Sender: TObject);
begin
  HotKey.SetFocus;
end;

end.

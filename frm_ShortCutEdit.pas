{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

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

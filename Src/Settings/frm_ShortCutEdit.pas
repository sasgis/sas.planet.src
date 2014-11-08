{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit frm_ShortCutEdit;

interface

uses
  Forms,
  Classes,
  Controls,
  ComCtrls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  i_ShortCutSingleConfig,
  u_CommonFormAndFrameParents;

type
  TfrmShortCutEdit = class(TFormWitghLanguageManager)
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
    function EditHotKeyModal(const AShortCutInfo: IShortCutSingleConfig): Boolean;
  end;

implementation

{$R *.dfm}

procedure TfrmShortCutEdit.btnClearClick(Sender: TObject);
begin
  HotKey.HotKey := 0;
end;

function TfrmShortCutEdit.EditHotKeyModal(
  const AShortCutInfo: IShortCutSingleConfig
): Boolean;
begin
  HotKey.HotKey := AShortCutInfo.ShortCut;
  if ShowModal = mrOK then begin
    AShortCutInfo.ShortCut := HotKey.HotKey;
    Result := True;
  end else begin
    Result := False;
  end;
end;

procedure TfrmShortCutEdit.FormShow(Sender: TObject);
begin
  HotKey.SetFocus;
end;

end.

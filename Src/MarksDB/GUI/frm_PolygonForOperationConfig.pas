{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit frm_PolygonForOperationConfig;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  UITypes,
  i_LanguageManager,
  u_CommonFormAndFrameParents;

type
  TShapeType = (stCircle, stSquare, stSquareOnSurface);

  TfrmPolygonForOperationConfig = class(TFormWitghLanguageManager)
    pnlBottom: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    cbbShapeType: TComboBox;
    lblRadius: TLabel;
    lblShape: TLabel;
    edtRadius: TEdit;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    procedure SetupItems;
    procedure ShowError(const AMsg: string); inline;
  protected
    procedure RefreshTranslation; override;
  public
    constructor Create(const ALanguageManager: ILanguageManager);
    function GetOptions(out ARadius: Double; out AShape: TShapeType): Boolean;
  end;

implementation

uses
  gnugettext,
  u_GeoToStrFunc;

{$R *.dfm}

{ TfrmPolygonForOperation }

constructor TfrmPolygonForOperationConfig.Create(
  const ALanguageManager: ILanguageManager
);
begin
  inherited Create(ALanguageManager);
  SetupItems;
end;

function TfrmPolygonForOperationConfig.GetOptions(
  out ARadius: Double;
  out AShape: TShapeType
): Boolean;
begin
  try
    Result := Self.ShowModal = mrOk;
    if Result then begin
      AShape := TShapeType(cbbShapeType.ItemIndex);
      ARadius := str2r(Trim(edtRadius.Text));
      if ARadius = 0 then begin
        ShowError(_('Radius must be greater than zero!'));
        Result := False;
      end;
    end;
  except
    on E: Exception do begin
      ShowError(E.ClassName + ': ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TfrmPolygonForOperationConfig.RefreshTranslation;
var
  I: Integer;
begin
  inherited RefreshTranslation;
  I := cbbShapeType.ItemIndex;
  SetupItems;
  cbbShapeType.ItemIndex := I;
end;

procedure TfrmPolygonForOperationConfig.SetupItems;
begin
  cbbShapeType.Items.Clear;
  cbbShapeType.Items.Add( _('Circle') );
  cbbShapeType.Items.Add( _('Square') );
  cbbShapeType.Items.Add( _('Square (on the surface)') );
  cbbShapeType.ItemIndex := 0;
end;

procedure TfrmPolygonForOperationConfig.ShowError(const AMsg: string);
begin
  MessageDlg(AMsg, mtError, [mbOK], 0);
end;

procedure TfrmPolygonForOperationConfig.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmPolygonForOperationConfig.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

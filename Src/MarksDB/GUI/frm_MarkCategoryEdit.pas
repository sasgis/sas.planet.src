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

unit frm_MarkCategoryEdit;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Spin,
  i_LanguageManager,
  i_MarkCategory,
  i_MarkCategoryDB,
  u_CommonFormAndFrameParents;

type
  TfrmMarkCategoryEdit = class(TFormWitghLanguageManager)
    Label1: TLabel;
    EditName: TEdit;
    CBShow: TCheckBox;
    EditS1: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    EditS2: TSpinEdit;
    Label4: TLabel;
    Bevel5: TBevel;
    btnOk: TButton;
    btnCancel: TButton;
    pnlBottomButtons: TPanel;
    flwpnlZooms: TFlowPanel;
    pnlName: TPanel;
    btnSetAsTemplate: TButton;
    lblReadOnly: TLabel;
    procedure btnSetAsTemplateClick(Sender: TObject);
  private
    FCategoryDB: IMarkCategoryDB;
  public
    function EditCategory(
      const ACategory: IMarkCategory;
      const AIsNewCategory: Boolean;
      const AMarksDBWriteAccess: Boolean
    ): IMarkCategory;
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ACategoryDB: IMarkCategoryDB
    ); reintroduce;
  end;

implementation

uses
  gnugettext,
  i_MarkCategoryFactory,
  i_MarkCategoryFactoryConfig,
  u_ResStrings;

{$R *.dfm}

{ TfrmMarkCategoryEdit }

constructor TfrmMarkCategoryEdit.Create(
  const ALanguageManager: ILanguageManager;
  const ACategoryDB: IMarkCategoryDB
);
begin
  inherited Create(ALanguageManager);
  FCategoryDB := ACategoryDB;
end;

procedure TfrmMarkCategoryEdit.btnSetAsTemplateClick(Sender: TObject);
var
  VMsg: string;
  VResult: Integer;
  VConfig: IMarkCategoryFactoryConfig;
begin
  VMsg := _('Set as default for new marks?');
  VResult := MessageBox(Self.Handle, PChar(VMsg), PChar(SAS_MSG_coution), MB_YESNO or MB_ICONQUESTION);
  if VResult = IDYES then begin
    VConfig := FCategoryDB.Factory.Config;
    VConfig.LockWrite;
    try
      VConfig.AfterScale := EditS1.Value;
      VConfig.BeforeScale := EditS2.Value;
    finally
      VConfig.UnlockWrite;
    end;
  end;
end;

function TfrmMarkCategoryEdit.EditCategory(
  const ACategory: IMarkCategory;
  const AIsNewCategory: Boolean;
  const AMarksDBWriteAccess: Boolean
): IMarkCategory;

  function _CheckNewName(const ANewName: string): Boolean;
  var
    VErr: string;
  begin
    if ANewName = '' then begin
      VErr := _('Category name can''t be empty!');
    end else
    if FCategoryDB.GetFirstCategoryByName(ANewName) <> nil then begin
      if AIsNewCategory or not SameText(ANewName, ACategory.Name) then begin
        VErr := Format(_('Category with name: "%s" already exists!'), [ANewName]);
      end;
    end else begin
      VErr := '';
    end;
    Result := VErr = '';
    if not Result then begin
      MessageBox(Self.Handle, PChar(VErr), PChar(SAS_MSG_error), MB_OK or MB_ICONERROR);
    end;
  end;

begin
  Result := nil;

  lblReadOnly.Visible := not AMarksDBWriteAccess;

  if AIsNewCategory then begin
    Self.Caption := SAS_STR_AddNewCategory;
  end else begin
    Self.Caption := SAS_STR_EditCategory;
  end;

  EditName.Text := ACategory.Name;
  EditS1.Value := ACategory.AfterScale;
  EditS2.Value := ACategory.BeforeScale;
  CBShow.Checked := ACategory.Visible;

  if (ShowModal = mrOk) and _CheckNewName(EditName.Text) then begin
    Result :=
      FCategoryDB.Factory.Modify(
        ACategory,
        EditName.Text,
        CBShow.Checked,
        EditS1.Value,
        EditS2.Value
      );
  end;
end;

end.

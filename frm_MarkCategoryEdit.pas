{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit frm_MarkCategoryEdit;

interface

uses
  Windows,
  Classes,
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
    procedure btnSetAsTemplateClick(Sender: TObject);
  private
    FCategoryDB: IMarkCategoryDB;
  public
    function EditCategory(const ACategory: IMarkCategory; AIsNewMark: Boolean): IMarkCategory;
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ACategoryDB: IMarkCategoryDB
    ); reintroduce;
  end;

implementation

uses
  i_MarkCategoryFactory,
  i_MarkCategoryFactoryConfig,
  u_ResStrings;

{$R *.dfm}

procedure TfrmMarkCategoryEdit.btnSetAsTemplateClick(Sender: TObject);
var
  VConfig: IMarkCategoryFactoryConfig;
begin
  if MessageBox(handle, pchar('Set as default for new marks?'), pchar(SAS_MSG_coution), 36) = IDYES then begin
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

constructor TfrmMarkCategoryEdit.Create(
  const ALanguageManager: ILanguageManager;
  const ACategoryDB: IMarkCategoryDB
);
begin
  inherited Create(ALanguageManager);
  FCategoryDB := ACategoryDB;
end;

function TfrmMarkCategoryEdit.EditCategory(const ACategory: IMarkCategory; AIsNewMark: Boolean): IMarkCategory;
begin
  EditName.Text:=SAS_STR_NewPoly;

  if AIsNewMark then begin
    Self.Caption:=SAS_STR_AddNewCategory;
  end else begin
    Self.Caption:=SAS_STR_EditCategory;
  end;
  EditName.Text:=ACategory.Name;
  EditS1.Value:=ACategory.AfterScale;
  EditS2.Value:=ACategory.BeforeScale;
  CBShow.Checked:=ACategory.Visible;
  if ShowModal = mrOk then begin
    Result := FCategoryDB.Factory.Modify(
        ACategory,
        EditName.Text,
        CBShow.Checked,
        EditS1.Value,
        EditS2.Value
      );
  end else begin
    Result := nil;
  end;
end;

end.

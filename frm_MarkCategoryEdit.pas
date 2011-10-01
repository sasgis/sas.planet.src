{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Spin,
  i_MarkCategory,
  u_CommonFormAndFrameParents,
  u_MarksDbGUIHelper,
  u_ResStrings;

type
  TfrmMarkCategoryEdit = class(TCommonFormParent)
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
  private
    FMarkDBGUI: TMarksDbGUIHelper;
  public
   function EditCategory(ACategory: IMarkCategory; AMarkDBGUI: TMarksDbGUIHelper): IMarkCategory;
  end;

var
  frmMarkCategoryEdit: TfrmMarkCategoryEdit;

implementation

{$R *.dfm}

function TfrmMarkCategoryEdit.EditCategory(ACategory: IMarkCategory; AMarkDBGUI: TMarksDbGUIHelper): IMarkCategory;
begin
  FMarkDBGUI := AMarkDBGUI;
  EditName.Text:=SAS_STR_NewPoly;
  if ACategory.IsNew then begin
    Self.Caption:=SAS_STR_AddNewCategory;
  end else begin
    Self.Caption:=SAS_STR_EditCategory;
  end;
  EditName.Text:=ACategory.name;
  EditS1.Value:=ACategory.AfterScale;
  EditS2.Value:=ACategory.BeforeScale;
  CBShow.Checked:=ACategory.visible;
  if ShowModal = mrOk then begin
    Result := FMarkDBGUI.MarksDB.CategoryDB.Factory.Modify(
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

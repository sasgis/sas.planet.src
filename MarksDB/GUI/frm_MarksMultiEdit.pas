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

unit frm_MarksMultiEdit;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  i_Category,
  i_LanguageManager,
  i_ImportConfig,
  i_MarkFactory,
  i_MarksDb,
  i_MarkCategoryDB,
  u_CommonFormAndFrameParents,
  fr_MarksGeneralOptions;

type
  TfrmMarksMultiEdit = class(TFormWitghLanguageManager)
    btnOk: TButton;
    btnCancel: TButton;
    pnlMarksGeneralOptions: TPanel;
    pnlBottom: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    frMarksGeneralOptions: TfrMarksGeneralOptions;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMarkFactory: IMarkFactory;
      const ACategoryDB: IMarkCategoryDB
    ); reintroduce;
    destructor Destroy; override;
    function GetImportConfig(const ACategory: ICategory): IImportConfig;
  end;

implementation

{$R *.dfm}

constructor TfrmMarksMultiEdit.Create(
  const ALanguageManager: ILanguageManager;
  const AMarkFactory: IMarkFactory;
  const ACategoryDB: IMarkCategoryDB
);
begin
  inherited Create(ALanguageManager);

  frMarksGeneralOptions:=
    TfrMarksGeneralOptions.Create(
      ALanguageManager,
      AMarkFactory,
      ACategoryDB,
      True,
      True
    );
end;

destructor TfrmMarksMultiEdit.Destroy;
begin
  FreeAndNil(frMarksGeneralOptions);
  inherited;
end;

function TfrmMarksMultiEdit.GetImportConfig(const ACategory:ICategory): IImportConfig;
begin
  frMarksGeneralOptions.Init(ACategory);
  try
    Self.PopupParent := Application.MainForm;
    if ShowModal = mrOk then begin
      Result := frMarksGeneralOptions.GetImportConfig;
    end else begin
      Result := nil;
    end;
  finally
    frMarksGeneralOptions.Clear;
  end;
end;

procedure TfrmMarksMultiEdit.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmMarksMultiEdit.FormShow(Sender: TObject);
begin
  frMarksGeneralOptions.Parent := pnlMarksGeneralOptions;
end;

end.

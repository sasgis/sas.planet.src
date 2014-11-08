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

unit frm_JpegImportConfigEdit;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  i_LanguageManager,
  i_AppearanceOfMarkFactory,
  i_MarkFactory,
  i_MarkCategoryDB,
  u_CommonFormAndFrameParents,
  fr_JpegImportOptions;

type
  TfrmJpegImportConfigEdit = class(TFormWitghLanguageManager)
    btnOk: TButton;
    btnCancel: TButton;
    pnlOptions: TPanel;
    pnlBottom: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    frOptions: TfrJpegImportOptions;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkFactory: IMarkFactory;
      const ACategoryDB: IMarkCategoryDB
    ); reintroduce;
    destructor Destroy; override;
    function GetConfig: IInterface;
  end;

implementation

{$R *.dfm}

constructor TfrmJpegImportConfigEdit.Create(
  const ALanguageManager: ILanguageManager;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const ACategoryDB: IMarkCategoryDB
);
begin
  inherited Create(ALanguageManager);

  frOptions:=
    TfrJpegImportOptions.Create(
      ALanguageManager,
      AAppearanceOfMarkFactory,
      AMarkFactory,
      ACategoryDB
    );
end;

destructor TfrmJpegImportConfigEdit.Destroy;
begin
  FreeAndNil(frOptions);
  inherited;
end;

function TfrmJpegImportConfigEdit.GetConfig: IInterface;
begin
  frOptions.Init(nil);
  try
    Self.PopupParent := Application.MainForm;
    if ShowModal = mrOk then begin
      Result := frOptions.GetConfig;
    end else begin
      Result := nil;
    end;
  finally
    frOptions.Clear;
  end;
end;

procedure TfrmJpegImportConfigEdit.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmJpegImportConfigEdit.FormShow(Sender: TObject);
begin
  frOptions.Parent := pnlOptions;
end;

end.

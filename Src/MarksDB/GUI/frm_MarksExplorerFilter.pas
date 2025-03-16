{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit frm_MarksExplorerFilter;

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
  i_LanguageManager,
  i_MarksExplorerFilter,
  u_CommonFormAndFrameParents;

type
  TfrmMarksExplorerFilter = class(TFormWitghLanguageManager)
    pnlBottomButtons: TPanel;
    btnCancel: TButton;
    btnOk: TButton;
    btnApply: TButton;
    btnReset: TButton;
    grpFilterByType: TGroupBox;
    chkAllowPoints: TCheckBox;
    grdpnlFilterByType: TGridPanel;
    chkAllowPaths: TCheckBox;
    chkAllowPolygons: TCheckBox;
    grpFilterByText: TGroupBox;
    edtSearchText: TEdit;
    cbbSearchMethod: TComboBox;
    chkIgnoreCase: TCheckBox;
    chkSearchInName: TCheckBox;
    chkSearchInDesc: TCheckBox;
    procedure btnApplyClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFilter: IMarksExplorerFilter;
    FConfig: TMarksExplorerFilterConfig;
    procedure UpdateUIByConfig;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AFilter: IMarksExplorerFilter
    ); reintroduce;
  end;

implementation

{$R *.dfm}

{ TfrmMarksExplorerFilter }

constructor TfrmMarksExplorerFilter.Create(
  const ALanguageManager: ILanguageManager;
  const AFilter: IMarksExplorerFilter
);
begin
  inherited Create(ALanguageManager);
  FFilter := AFilter;
end;

procedure TfrmMarksExplorerFilter.FormShow(Sender: TObject);
begin
  FConfig := FFilter.Config;
  UpdateUIByConfig;
end;

procedure TfrmMarksExplorerFilter.UpdateUIByConfig;
begin
  chkAllowPoints.Checked := FConfig.AllowPoints;
  chkAllowPaths.Checked := FConfig.AllowPaths;
  chkAllowPolygons.Checked := FConfig.AllowPolygons;

  edtSearchText.Text := FConfig.SearchText;
  cbbSearchMethod.ItemIndex := Integer(FConfig.SearchMethod);
  chkIgnoreCase.Checked := FConfig.IgnoreCase;
  chkSearchInName.Checked := FConfig.SearchInName;
  chkSearchInDesc.Checked := FConfig.SearchInDesc;
end;

procedure TfrmMarksExplorerFilter.btnApplyClick(Sender: TObject);
begin
  FConfig.Reset;

  FConfig.AllowPoints := chkAllowPoints.Checked;
  FConfig.AllowPaths := chkAllowPaths.Checked;
  FConfig.AllowPolygons := chkAllowPolygons.Checked;

  FConfig.SearchText := edtSearchText.Text;
  FConfig.SearchMethod := TSearchMethod(cbbSearchMethod.ItemIndex);
  FConfig.IgnoreCase := chkIgnoreCase.Checked;
  FConfig.SearchInName := chkSearchInName.Checked;
  FConfig.SearchInDesc := chkSearchInDesc.Checked;

  FFilter.Config := FConfig;
  FFilter.Enabled := FConfig.IsEnabled;
end;

procedure TfrmMarksExplorerFilter.btnOkClick(Sender: TObject);
begin
  btnApplyClick(Sender);
  Close;
end;

procedure TfrmMarksExplorerFilter.btnResetClick(Sender: TObject);
begin
  FFilter.Enabled := False;
  FConfig.Reset;
  FFilter.Config := FConfig;
  UpdateUIByConfig;
end;

procedure TfrmMarksExplorerFilter.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.

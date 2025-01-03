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

unit frm_MarksExportConfig;

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
  ExtCtrls,
  ComCtrls,
  StdCtrls,
  Spin,
  i_ExportConfig,
  i_ExportMarks2KMLConfig,
  u_CommonFormAndFrameParents;

type
  TfrmMarksExportConfig = class(TFormWitghLanguageManager)
    pnlBottom: TPanel;
    btnApply: TButton;
    btnCancel: TButton;
    pgcMain: TPageControl;
    tsExportToKml: TTabSheet;
    chkFixedCoordPrecision: TCheckBox;
    seCoordDigits: TSpinEdit;
    rgSorting: TRadioGroup;
    chkAbsPathToIcon: TCheckBox;
    edtAbsPathToIcon: TEdit;
    grpCoordinates: TGroupBox;
    GridPanel1: TGridPanel;
    rgIconScale: TRadioGroup;
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure chkFixedCoordPrecisionClick(Sender: TObject);
    procedure chkAbsPathToIconClick(Sender: TObject);
  private
    FExportMarks2KMLConfig: IExportMarks2KMLConfig;
  public
    function DoShowModal(
      const AFormatExt: string;
      const AConfig: IExportConfig
    ): Integer;
  end;

implementation

uses
  gnugettext;

{$R *.dfm}

{ TfrmMarksExportConfig }

function TfrmMarksExportConfig.DoShowModal(
  const AFormatExt: string;
  const AConfig: IExportConfig
): Integer;
var
  I: Integer;
begin
  for I := 0 to pgcMain.PageCount - 1 do begin
    pgcMain.Pages[I].TabVisible := False;
  end;

  FExportMarks2KMLConfig := nil;

  if Supports(AConfig, IExportMarks2KMLConfig, FExportMarks2KMLConfig) then begin
    pgcMain.ActivePageIndex := 0;
  end else begin
    Assert(False);
  end;

  Self.Caption := Format(_('%s format settings'), [AFormatExt]);

  Result := ShowModal;
end;

procedure TfrmMarksExportConfig.FormShow(Sender: TObject);
var
  VKmlConfig: IExportMarks2KMLConfigStatic;
begin
  btnCancel.SetFocus;

  if FExportMarks2KMLConfig <> nil then begin
    VKmlConfig := FExportMarks2KMLConfig.GetStatic;

    chkFixedCoordPrecision.Checked := VKmlConfig.UseCoordFormatting;
    seCoordDigits.Value := VKmlConfig.CoordPrecision;
    seCoordDigits.Enabled := chkFixedCoordPrecision.Checked;

    rgSorting.ItemIndex := Integer(VKmlConfig.SortingType);

    chkAbsPathToIcon.Checked := VKmlConfig.UseAbsPathToIcon;
    edtAbsPathToIcon.Text := VKmlConfig.AbsPathToIcon;
    edtAbsPathToIcon.Enabled := chkAbsPathToIcon.Checked;

    rgIconScale.ItemIndex := Integer(VKmlConfig.IconScaleType);
  end;
end;

procedure TfrmMarksExportConfig.btnApplyClick(Sender: TObject);
begin
  if FExportMarks2KMLConfig <> nil then begin
    FExportMarks2KMLConfig.StopNotify;
    try
      FExportMarks2KMLConfig.UseCoordFormatting := chkFixedCoordPrecision.Checked;
      FExportMarks2KMLConfig.CoordPrecision := seCoordDigits.Value;

      FExportMarks2KMLConfig.SortingType := TKmlSortingType(rgSorting.ItemIndex);

      FExportMarks2KMLConfig.UseAbsPathToIcon := chkAbsPathToIcon.Checked;
      FExportMarks2KMLConfig.AbsPathToIcon := edtAbsPathToIcon.Text;

      FExportMarks2KMLConfig.IconScaleType := TKmlIconScaleType(rgIconScale.ItemIndex);
    finally
      FExportMarks2KMLConfig.StartNotify;
    end;
  end;

  Close;
end;

procedure TfrmMarksExportConfig.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMarksExportConfig.chkAbsPathToIconClick(Sender: TObject);
begin
  edtAbsPathToIcon.Enabled := chkAbsPathToIcon.Checked;
end;

procedure TfrmMarksExportConfig.chkFixedCoordPrecisionClick(Sender: TObject);
begin
  seCoordDigits.Enabled := chkFixedCoordPrecision.Checked;
end;

end.

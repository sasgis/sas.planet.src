{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2018, SAS.Planet development team.                      *}
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
  i_ExportMarks2KMLConfig,
  u_CommonFormAndFrameParents;

type
  TfrmMarksExportConfig = class(TCommonFormParent)
    tvMenu: TTreeView;
    pnlBottom: TPanel;
    btnApply: TButton;
    btnClose: TButton;
    pgcMain: TPageControl;
    tsExportToKml: TTabSheet;
    spl1: TSplitter;
    chkFixedCoordPrecision: TCheckBox;
    seCoordDigits: TSpinEdit;
    rgSorting: TRadioGroup;
    chkAbsPathToIcon: TCheckBox;
    edtAbsPathToIcon: TEdit;
    grpCoordinates: TGroupBox;
    GridPanel1: TGridPanel;
    procedure tvMenuClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkFixedCoordPrecisionClick(Sender: TObject);
    procedure chkAbsPathToIconClick(Sender: TObject);
  private
    FExportMarks2KMLConfig: IExportMarks2KMLConfig;
    procedure BuildTreeViewMenu;
  public
    constructor Create(
      const AOwner: TComponent;
      const AExportMarks2KMLConfig: IExportMarks2KMLConfig
    ); reintroduce;
  end;

implementation

{$R *.dfm}

{ TfrmMarksExportConfig }

constructor TfrmMarksExportConfig.Create(
  const AOwner: TComponent;
  const AExportMarks2KMLConfig: IExportMarks2KMLConfig
);
begin
  inherited Create(AOwner);
  FExportMarks2KMLConfig := AExportMarks2KMLConfig;
  BuildTreeViewMenu;
end;

procedure TfrmMarksExportConfig.tvMenuClick(Sender: TObject);
var
  VNewPageIndex, VActivePageIndex: Integer;
begin
  VNewPageIndex := TTabSheet(tvMenu.Selected.Data).PageIndex;
  VActivePageIndex := pgcMain.ActivePageIndex;
  if VNewPageIndex <> VActivePageIndex then begin
    pgcMain.ActivePageIndex := VNewPageIndex;
  end;
end;

procedure TfrmMarksExportConfig.BuildTreeViewMenu;
var
  I: Integer;
  VNode: TTreeNode;
begin
  // hide all tabs
  for I := 0 to pgcMain.PageCount - 1 do begin
    pgcMain.Pages[I].TabVisible := False;
  end;

  // build menu
  VNode := tvMenu.Items.AddChildObjectFirst(nil, '*.kml; *.kmz', tsExportToKml);

  tvMenu.FullExpand;

  // show default tab
  VNode.Selected := True;

  tvMenuClick(Self);
end;

procedure TfrmMarksExportConfig.chkAbsPathToIconClick(Sender: TObject);
begin
  edtAbsPathToIcon.Enabled := chkAbsPathToIcon.Checked;
end;

procedure TfrmMarksExportConfig.chkFixedCoordPrecisionClick(Sender: TObject);
begin
  seCoordDigits.Enabled := chkFixedCoordPrecision.Checked;
end;

procedure TfrmMarksExportConfig.FormShow(Sender: TObject);
var
  VConfig: IExportMarks2KMLConfigStatic;
begin
  btnClose.SetFocus;

  VConfig := FExportMarks2KMLConfig.GetStatic;

  chkFixedCoordPrecision.Checked := VConfig.UseCoordFormatting;
  seCoordDigits.Value := VConfig.CoordPrecision;
  seCoordDigits.Enabled := chkFixedCoordPrecision.Checked;

  rgSorting.ItemIndex := Integer(VConfig.SortingType);

  chkAbsPathToIcon.Checked := VConfig.UseAbsPathToIcon;
  edtAbsPathToIcon.Text := VConfig.AbsPathToIcon;
  edtAbsPathToIcon.Enabled := chkAbsPathToIcon.Checked;
end;

procedure TfrmMarksExportConfig.btnApplyClick(Sender: TObject);
begin
  FExportMarks2KMLConfig.StopNotify;
  try
    FExportMarks2KMLConfig.UseCoordFormatting := chkFixedCoordPrecision.Checked;
    FExportMarks2KMLConfig.CoordPrecision := seCoordDigits.Value;

    FExportMarks2KMLConfig.SortingType := TKmlSortingType(rgSorting.ItemIndex);

    FExportMarks2KMLConfig.UseAbsPathToIcon := chkAbsPathToIcon.Checked;
    FExportMarks2KMLConfig.AbsPathToIcon := edtAbsPathToIcon.Text;
  finally
    FExportMarks2KMLConfig.StartNotify;
  end;

  Close;
end;

procedure TfrmMarksExportConfig.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

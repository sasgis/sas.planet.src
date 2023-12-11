{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit frm_GeoTiffOptions;

interface

uses
  Types,
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
  Spin,
  CheckLst,
  UITypes,
  t_GeoTIFF,
  i_LanguageManager,
  u_CommonFormAndFrameParents;

type
  TfrmGeoTiffOptions = class(TCommonFormParent)
    lblCompression: TLabel;
    cbbCompression: TComboBox;
    lblFormat: TLabel;
    cbbFormat: TComboBox;
    btnApply: TButton;
    btnCancel: TButton;
    pnlFormat: TPanel;
    pnlCompression: TPanel;
    pnlCompressionLevel: TPanel;
    lblCompressionLevel: TLabel;
    seCompressionLevel: TSpinEdit;
    pnlOverview: TPanel;
    grpOverview: TGroupBox;
    chklstOverview: TCheckListBox;
    lblCustomOverview: TLabel;
    edtOverview: TEdit;
    chkCopyRawJpeg: TCheckBox;
    pnlColorspace: TPanel;
    lblColorspace: TLabel;
    cbbColorspace: TComboBox;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cbbCompressionChange(Sender: TObject);
    procedure edtOverviewChange(Sender: TObject);
  private
    FIsTiled: Boolean;
    FOptions: PGeoTiffOptions;
    function GetOverview(out AOverview: TIntegerDynArray): Boolean;
  public
    procedure ShowOptionsModal(
      var AOptions: TGeoTiffOptions
    );
    constructor Create(AOwner: TComponent); reintroduce;
  end;

const
  CDefaultGeoTiffOptions: TGeoTiffOptions = (
    FileFormatType: gtfAuto;
    StorageType: gtstTiled;
    CompressionType: gtcLzw;
    CompressionLevelZip: 6;
    CompressionLevelJpeg: 75;
    Colorspace: gtcsYCbCr;
    CopyRawJpegTiles: True;
    Overview: nil
  );

implementation

uses
  gnugettext,
  StrUtils,
  u_GeoTiffFunc;

{$R *.dfm}

procedure SetControlEnabled(const AControl: TControl; const AEnabled: Boolean);
var
  I: Integer;
begin
  if AControl = nil then begin
    Exit;
  end;
  if AControl is TWinControl then begin
    for I := 0 to TWinControl(AControl).ControlCount - 1 do begin
      SetControlEnabled(TWinControl(AControl).Controls[I], AEnabled);
    end;
  end;
  AControl.Enabled := AEnabled;
end;

{ TfrmGeoTiffOptions }

constructor TfrmGeoTiffOptions.Create(AOwner: TComponent);
begin
  TP_Ignore(Self, 'cbbColorspace.Items');

  inherited Create(AOwner);
end;

procedure TfrmGeoTiffOptions.ShowOptionsModal(
  var AOptions: TGeoTiffOptions
);
begin
  FOptions := @AOptions;
  FIsTiled := FOptions.StorageType = gtstTiled;

  cbbFormat.ItemIndex := Integer(FOptions.FileFormatType);
  cbbCompression.ItemIndex := Integer(FOptions.CompressionType);

  pnlColorspace.Visible := True;
  cbbColorspace.ItemIndex := Integer(FOptions.Colorspace);

  chkCopyRawJpeg.Visible := FIsTiled;
  chkCopyRawJpeg.Checked := FOptions.CopyRawJpegTiles;

  cbbCompressionChange(Self);

  ShowModal;
end;

function TfrmGeoTiffOptions.GetOverview(out AOverview: TIntegerDynArray): Boolean;
var
  I, J: Integer;
  VStrArr: TStringDynArray;
begin
  Result := False;

  if chklstOverview.Enabled then begin
    J := 0;
    SetLength(VStrArr, chklstOverview.Count);
    for I := 0 to chklstOverview.Count - 1 do begin
      if chklstOverview.Checked[I] then begin
        VStrArr[J] := chklstOverview.Items[I];
        Inc(J);
      end;
    end;
    SetLength(VStrArr, J);
  end else begin
    VStrArr := SplitString(Trim(edtOverview.Text), ' ');
  end;

  SetLength(AOverview, Length(VStrArr));
  for I := 0 to Length(VStrArr) - 1 do begin
    if not TryStrToInt(VStrArr[I], AOverview[I]) then begin
      MessageDlg(Format(_('Invalid Overview level value: "%s"'), [VStrArr[I]]), mtError, [mbOK], 0);
      Exit;
    end;
    if not TGeoTiffFunc.IsValidOverviewValue(AOverview[I]) then begin
      MessageDlg(Format(_('Given Overview level "%s" is not a power of 2!'), [VStrArr[I]]), mtError, [mbOK], 0);
      Exit;
    end;
  end;

  Result := True;
end;

procedure TfrmGeoTiffOptions.btnApplyClick(Sender: TObject);
var
  VOverview: TIntegerDynArray;
begin
  if not GetOverview(VOverview) then begin
    Exit; // exit without closing window
  end;

  with FOptions^ do begin
    FileFormatType := TGeoTiffFileFormat(cbbFormat.ItemIndex);
    CompressionType := TGeoTiffCompression(cbbCompression.ItemIndex);

    case CompressionType of
      gtcZip: FOptions.CompressionLevelZip := seCompressionLevel.Value;
      gtcJpeg: FOptions.CompressionLevelJpeg := seCompressionLevel.Value;
    end;

    Overview := VOverview;
    Colorspace := TGeoTiffColorspace(cbbColorspace.ItemIndex);

    if FIsTiled then begin
      CopyRawJpegTiles := chkCopyRawJpeg.Checked;
    end;
  end;

  Close;
end;

procedure TfrmGeoTiffOptions.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmGeoTiffOptions.cbbCompressionChange(Sender: TObject);
var
  VIsColorspaceEnabled: Boolean;
  VIsCopyRawJpegEnabled: Boolean;
  VIsCompressionLevelEnabled: Boolean;
begin
  VIsColorspaceEnabled := False;
  VIsCopyRawJpegEnabled := False;
  VIsCompressionLevelEnabled := False;

  case TGeoTiffCompression(cbbCompression.ItemIndex) of
    gtcZip: begin
      seCompressionLevel.MinValue := 0;
      seCompressionLevel.MaxValue := 9;
      seCompressionLevel.Value := FOptions.CompressionLevelZip;

      VIsCompressionLevelEnabled := True;
    end;
    gtcJpeg: begin
      seCompressionLevel.MinValue := 0;
      seCompressionLevel.MaxValue := 100;
      seCompressionLevel.Value := FOptions.CompressionLevelJpeg;
      VIsCompressionLevelEnabled := True;

      cbbColorspace.ItemIndex := Integer(FOptions.Colorspace);

      VIsColorspaceEnabled := pnlColorspace.Visible;
      VIsCopyRawJpegEnabled := chkCopyRawJpeg.Visible;
    end;
  end;

  SetControlEnabled(pnlCompressionLevel, VIsCompressionLevelEnabled);
  if not seCompressionLevel.Enabled then begin
    seCompressionLevel.Value := 0;
  end;

  SetControlEnabled(pnlColorspace, VIsColorspaceEnabled);
  if not cbbColorspace.Enabled then begin
    cbbColorspace.ItemIndex := 0;
  end;

  chkCopyRawJpeg.Enabled := VIsCopyRawJpegEnabled;
end;

procedure TfrmGeoTiffOptions.edtOverviewChange(Sender: TObject);
begin
  chklstOverview.Enabled := Trim(edtOverview.Text) = '';

  if not chklstOverview.Enabled then begin
    chklstOverview.ItemIndex := -1; // reset focus
  end;
end;

end.

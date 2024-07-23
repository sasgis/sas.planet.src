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
  StdCtrls,
  ExtCtrls,
  Spin,
  CheckLst,
  t_GeoTIFF,
  i_LanguageManager,
  u_CheckListBoxExt,
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FIsTiled: Boolean;
    FOptions: TGeoTiffOptions;
    procedure OptionsToGui;
    procedure GuiToOptions;
    function GetOptions: TGeoTiffOptions;
    function GetOverview(out AOverview: TIntegerDynArray; const ASilent: Boolean): Boolean;
    procedure SetOverview(const AOverview: TIntegerDynArray);
  public
    function ShowModal: Integer; override;
    property Options: TGeoTiffOptions read GetOptions;
  public
    constructor Create(
      AOwner: TComponent;
      const AGeoTiffStorageType: TGeoTiffStorageType
    ); reintroduce;
  end;

  TfrmGeoTiffOptionsStripped = class(TfrmGeoTiffOptions)
  public
    constructor Create(AOwner: TComponent); reintroduce;
  end;

  TfrmGeoTiffOptionsTiled = class(TfrmGeoTiffOptions)
  public
    constructor Create(AOwner: TComponent); reintroduce;
  end;

implementation

uses
  Math,
  StrUtils,
  gnugettext,
  u_Dialogs,
  u_GeoTiffFunc;

{$R *.dfm}

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

{ TfrmGeoTiffOptions }

constructor TfrmGeoTiffOptions.Create(
  AOwner: TComponent;
  const AGeoTiffStorageType: TGeoTiffStorageType
);
begin
  TP_Ignore(Self, 'cbbColorspace.Items');
  TP_Ignore(Self, 'cbbColorspace.Text');

  inherited Create(AOwner);

  FOptions := CDefaultGeoTiffOptions;
  FOptions.StorageType := AGeoTiffStorageType;

  FIsTiled := FOptions.StorageType = gtstTiled;

  if FIsTiled then begin
    Self.Name := Self.Name + 'Tiled';
  end else begin
    Self.Name := Self.Name + 'Stripped';
  end;

  pnlColorspace.Visible := True;
  chkCopyRawJpeg.Visible := FIsTiled;

  OptionsToGui;
end;

function TfrmGeoTiffOptions.ShowModal: Integer;
begin
  cbbCompressionChange(Self);
  GuiToOptions;
  Result := inherited ShowModal;
end;

function TfrmGeoTiffOptions.GetOptions: TGeoTiffOptions;
begin
  GuiToOptions;
  Result := FOptions;
end;

procedure TfrmGeoTiffOptions.SetOverview(const AOverview: TIntegerDynArray);
const
  CFixedOverviewItems = [2, 4, 8, 16, 32, 64];
var
  I: Integer;
  VIndex: Integer;
  VList: TStringList;
  VIsCustomOverview: Boolean;
begin
  chklstOverview.CheckAll(cbUnchecked);
  edtOverview.Text := '';

  VIsCustomOverview := Length(AOverview) > chklstOverview.Count;
  if not VIsCustomOverview then begin
    for I := 0 to Length(AOverview) - 1 do begin
      if not (AOverview[I] in CFixedOverviewItems) then begin
        VIsCustomOverview := True;
        Break;
      end;
    end;
  end;

  if not VIsCustomOverview then begin
    for I := 0 to Length(AOverview) - 1 do begin
      VIndex := Trunc(Log2(AOverview[I])) - 1;
      Assert((VIndex >= 0) and (VIndex < chklstOverview.Count));
      chklstOverview.Checked[VIndex] := True;
    end;
  end else begin
    VList := TStringList.Create;
    try
      VList.Duplicates := dupIgnore;
      for I := 0 to Length(AOverview) - 1 do begin
        VList.Add(IntToStr(AOverview[I]));
      end;
      VList.Delimiter := ' ';
      edtOverview.Text := VList.DelimitedText;
    finally
      VList.Free;
    end;
  end;
end;

function TfrmGeoTiffOptions.GetOverview(out AOverview: TIntegerDynArray; const ASilent: Boolean): Boolean;
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
      if not ASilent then begin
        ShowErrorMessage(Format(_('Invalid Overview level value: "%s"'), [VStrArr[I]]));
      end;
      Exit;
    end;
    if not TGeoTiffFunc.IsValidOverviewValue(AOverview[I]) then begin
      if not ASilent then begin
        ShowErrorMessage(Format(_('Given Overview level "%s" is not a power of 2!'), [VStrArr[I]]));
      end;
      Exit;
    end;
  end;

  Result := True;
end;

procedure TfrmGeoTiffOptions.GuiToOptions;
begin
  if not GetOverview(FOptions.Overview, True) then begin
    FOptions.Overview := nil; // ignore errors here
  end;

  with FOptions do begin
    FileFormatType := TGeoTiffFileFormat(cbbFormat.ItemIndex);
    CompressionType := TGeoTiffCompression(cbbCompression.ItemIndex);

    case CompressionType of
      gtcZip: FOptions.CompressionLevelZip := seCompressionLevel.Value;
      gtcJpeg: FOptions.CompressionLevelJpeg := seCompressionLevel.Value;
    end;

    Colorspace := TGeoTiffColorspace(cbbColorspace.ItemIndex);

    if FIsTiled then begin
      CopyRawJpegTiles := chkCopyRawJpeg.Checked;
    end;
  end;
end;

procedure TfrmGeoTiffOptions.OptionsToGui;
begin
  cbbFormat.ItemIndex := Integer(FOptions.FileFormatType);
  cbbCompression.ItemIndex := Integer(FOptions.CompressionType);
  cbbColorspace.ItemIndex := Integer(FOptions.Colorspace);
  chkCopyRawJpeg.Checked := FOptions.CopyRawJpegTiles;

  SetOverview(FOptions.Overview);

  cbbCompressionChange(Self);
end;

procedure TfrmGeoTiffOptions.btnApplyClick(Sender: TObject);
var
  VOverview: TIntegerDynArray;
begin
  if not GetOverview(VOverview, False) then begin
    Exit; // show warning and exit without closing window
  end;

  GuiToOptions;
  Close;
end;

procedure TfrmGeoTiffOptions.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmGeoTiffOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OptionsToGui;
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

{ TfrmGeoTiffOptionsStripped }

constructor TfrmGeoTiffOptionsStripped.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, gtstStripped);
end;

{ TfrmGeoTiffOptionsTiled }

constructor TfrmGeoTiffOptionsTiled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, gtstTiled);
end;

end.

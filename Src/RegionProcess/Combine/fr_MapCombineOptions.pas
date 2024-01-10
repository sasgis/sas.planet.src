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

unit fr_MapCombineOptions;

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
  Spin,
  Math,
  TBXDkPanels,
  gnugettext,
  t_GeoTIFF,
  t_MapCombineOptions,
  i_LanguageManager,
  i_RegionProcessParamsFrame,
  frm_GeoTiffOptions,
  u_CommonFormAndFrameParents;

type
  TfrMapCombineCustomOptions = class(TFrame, IMapCombineCustomOptions)
    flwpnlJpegQuality: TFlowPanel;
    lblJpgQulity: TLabel;
    seJpgQuality: TSpinEdit;
    chkPngWithAlpha: TCheckBox;
    chkSaveGeoRefInfoToJpegExif: TCheckBox;
    flwpnlThreadCount: TFlowPanel;
    lblThreadCount: TLabel;
    seThreadCount: TSpinEdit;
    flwpnlFormatOptions: TFlowPanel;
    btnFormatOptions: TTBXButton;
    flwpnlKmzTileSize: TFlowPanel;
    lblKmzTileSize: TLabel;
    cbbKmzTileSize: TComboBox;
    flwpnlCompressionLevel: TFlowPanel;
    lblCompressionLevel: TLabel;
    seCompressionLevel: TSpinEdit;
  private
    FOptionsSet: TMapCombineOptionsSet;
    FRoundToTileRect: Boolean;
    FGeoTiffOptions: TGeoTiffOptions;
    FfrmGeoTiffOptions: TfrmGeoTiffOptions;
    procedure UpdateFormatOptionsButton(const ACaption: string = '');
    procedure OnGetGeoTiffOptionsClick(Sender: TObject);
  private
    { IMapCombineCustomOptions }
    function GetQuality: Integer;
    function GetIsSaveGeoRefInfoToExif: Boolean;
    function GetThreadCount: Integer;
    function GetIsSaveAlfa: Boolean;
    function GetKmzTileSize: Integer;
    function GetGeoTiffOptions: TGeoTiffOptions;
    function GetRoundToTileRect: Boolean;
  protected
    procedure RefreshTranslation; override;
  public
    procedure Show(AParent: TWinControl);
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AOptionsSet: TMapCombineOptionsSet
    ); reintroduce;
  end;

implementation

{$R *.dfm}

function GetTextWidth(const AText: string; const AFont: TFont): Integer;
var
  VBitmap: TBitmap;
begin
  VBitmap := TBitmap.Create;
  try
    VBitmap.Canvas.Font := AFont;
    Result := VBitmap.Canvas.TextWidth(AText);
  finally
    VBitmap.Free;
  end;
end;

{ TfrMapCombineCustomOptions }

constructor TfrMapCombineCustomOptions.Create(
  const ALanguageManager: ILanguageManager;
  const AOptionsSet: TMapCombineOptionsSet
);
begin
  TP_Ignore(Self, 'cbbKmzTileSize.Items');

  inherited Create(ALanguageManager);

  SetControlVisible(Self, False);

  FOptionsSet := AOptionsSet;

  if (mcAlphaCheck in AOptionsSet) or (mcAlphaUncheck in AOptionsSet) then begin
    chkPngWithAlpha.Checked := (mcAlphaCheck in AOptionsSet);
    chkPngWithAlpha.Visible := True;
    Self.Visible := True;
  end;

  if mcExif in AOptionsSet then begin
    chkSaveGeoRefInfoToJpegExif.Checked := False;
    chkSaveGeoRefInfoToJpegExif.Visible := True;
    Self.Visible := True;
  end;

  if mcQuality in AOptionsSet then begin
    SetControlVisible(flwpnlJpegQuality, True);
    Self.Visible := True;
  end;

  if mcCompressionLevel in AOptionsSet then begin
    SetControlVisible(flwpnlCompressionLevel, True);
    Self.Visible := True;
  end;

  if mcThreadCount in AOptionsSet then begin
    SetControlVisible(flwpnlThreadCount, True);
    Self.Visible := True;
  end;

  if mcKmzTileSize in AOptionsSet then begin
    SetControlVisible(flwpnlKmzTileSize, True);
    cbbKmzTileSize.ItemIndex := 2; // 1024x1024
  end;

  if (mcGeoTiff in AOptionsSet) or (mcGeoTiffTiled in AOptionsSet) then begin
    SetControlVisible(flwpnlFormatOptions, True);
    FGeoTiffOptions := CDefaultGeoTiffOptions;
    if mcGeoTiff in AOptionsSet then begin
      FGeoTiffOptions.StorageType := gtstStripped;
    end else begin
      FGeoTiffOptions.StorageType := gtstTiled;
    end;
    FfrmGeoTiffOptions := TfrmGeoTiffOptions.Create(Self);
    btnFormatOptions.OnClick := Self.OnGetGeoTiffOptionsClick;
    UpdateFormatOptionsButton(FfrmGeoTiffOptions.Caption);
    Self.Visible := True;
  end;

  FRoundToTileRect := (mcGeoTiffTiled in AOptionsSet);

  FPropertyState := CreateComponentPropertyState(
    Self, [btnFormatOptions, FfrmGeoTiffOptions], [], True, False, True, False
  );
end;

procedure TfrMapCombineCustomOptions.UpdateFormatOptionsButton(const ACaption: string);
begin
  if ACaption <> '' then begin
    btnFormatOptions.Caption := ACaption;
  end;
  if btnFormatOptions.Visible then begin
    btnFormatOptions.Width := GetTextWidth(btnFormatOptions.Caption, btnFormatOptions.Font) + 40;
  end;
end;

procedure TfrMapCombineCustomOptions.Show(AParent: TWinControl);
begin
  Parent := AParent;
end;

function TfrMapCombineCustomOptions.GetGeoTiffOptions: TGeoTiffOptions;
begin
  Result := FGeoTiffOptions;
end;

function TfrMapCombineCustomOptions.GetIsSaveAlfa: Boolean;
begin
  Result := chkPngWithAlpha.Visible and chkPngWithAlpha.Checked;
end;

function TfrMapCombineCustomOptions.GetIsSaveGeoRefInfoToExif: Boolean;
begin
  Result := chkSaveGeoRefInfoToJpegExif.Checked;
end;

function TfrMapCombineCustomOptions.GetKmzTileSize: Integer;
begin
  Result := (1 shl cbbKmzTileSize.ItemIndex) * 256;
end;

function TfrMapCombineCustomOptions.GetQuality: Integer;
begin
  if mcQuality in FOptionsSet then begin
    Result := seJpgQuality.Value;
  end else
  if mcCompressionLevel in FOptionsSet then begin
    Result := seCompressionLevel.Value;
  end else begin
    Assert(False);
    Result := 0;
  end;
end;

function TfrMapCombineCustomOptions.GetThreadCount: Integer;
begin
  Result := seThreadCount.Value;
end;

procedure TfrMapCombineCustomOptions.OnGetGeoTiffOptionsClick(Sender: TObject);
begin
  FfrmGeoTiffOptions.ShowOptionsModal(FGeoTiffOptions);
end;

function TfrMapCombineCustomOptions.GetRoundToTileRect: Boolean;
begin
  Result := FRoundToTileRect;
end;

procedure TfrMapCombineCustomOptions.RefreshTranslation;
begin
  inherited RefreshTranslation;

  UpdateFormatOptionsButton;
end;

end.

{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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
  i_LanguageManager,
  i_RegionProcessParamsFrame,
  t_GeoTIFF,
  t_MapCombineOptions,
  u_CommonFormAndFrameParents;

type
  TfrMapCombineCustomOptions = class(TFrame, IMapCombineCustomOptions)
    flwpnlCompression: TFlowPanel;
    lblCompression: TLabel;
    cbbCompression: TComboBox;
    flwpnlFormat: TFlowPanel;
    lblFormat: TLabel;
    cbbFormat: TComboBox;
    flwpnlJpegQuality: TFlowPanel;
    lblJpgQulity: TLabel;
    seJpgQuality: TSpinEdit;
    chkPngWithAlpha: TCheckBox;
    chkSaveGeoRefInfoToJpegExif: TCheckBox;
    flwpnlThreadCount: TFlowPanel;
    lblThreadCount: TLabel;
    seThreadCount: TSpinEdit;
  private
    { IMapCombineCustomOptions }
    function GetQuality: Integer;
    function GetIsSaveGeoRefInfoToExif: Boolean;
    function GetThreadCount: Integer;
    function GetIsSaveAlfa: Boolean;
    function GetGeoTiffCompression: TGeoTiffCompression;
    function GetGeoTiffFormat: TGeoTiffFileFormat;
  public
    procedure Show(AParent: TWinControl);
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AOptionsSet: TMapCombineOptionsSet
    ); reintroduce;
  end;

implementation

{$R *.dfm}

{ TfrMapCombineCustomOptions }

procedure SetControlVisible(const AControl: TControl; const AVisible: Boolean);
var
  I: Integer;
begin
  if AControl = nil then begin
    Exit;
  end;
  if AControl is TWinControl then begin
    for I := 0 to TWinControl(AControl).ControlCount - 1 do begin
      SetControlVisible(TWinControl(AControl).Controls[I], AVisible);
    end;
  end;
  AControl.Visible := AVisible;
end;

constructor TfrMapCombineCustomOptions.Create(
  const ALanguageManager: ILanguageManager;
  const AOptionsSet: TMapCombineOptionsSet
);
begin
  inherited Create(ALanguageManager);

  SetControlVisible(Self, False);

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

  if mcThreadCount in AOptionsSet then begin
    SetControlVisible(flwpnlThreadCount, True);
    Self.Visible := True;
  end;

  if mcGeoTiff in AOptionsSet then begin
    SetControlVisible(flwpnlFormat, True);
    SetControlVisible(flwpnlCompression, True);
    cbbFormat.ItemIndex := 0; // Auto
    cbbCompression.ItemIndex := 2; // LZW
    Self.Visible := True;
  end;
end;

procedure TfrMapCombineCustomOptions.Show(AParent: TWinControl);
begin
  Parent := AParent;
end;

function TfrMapCombineCustomOptions.GetGeoTiffCompression: TGeoTiffCompression;
begin
  Result := TGeoTiffCompression(cbbCompression.ItemIndex);
end;

function TfrMapCombineCustomOptions.GetGeoTiffFormat: TGeoTiffFileFormat;
begin
  Result := TGeoTiffFileFormat(cbbFormat.ItemIndex);
end;

function TfrMapCombineCustomOptions.GetIsSaveAlfa: Boolean;
begin
  if chkPngWithAlpha.Visible then begin
    Result := chkPngWithAlpha.Checked;
  end else begin
    Result := False;
  end;
end;

function TfrMapCombineCustomOptions.GetIsSaveGeoRefInfoToExif: Boolean;
begin
  Result := chkSaveGeoRefInfoToJpegExif.Checked;
end;

function TfrMapCombineCustomOptions.GetQuality: Integer;
begin
  Result := seJpgQuality.Value;
end;

function TfrMapCombineCustomOptions.GetThreadCount: Integer;
begin
  Result := seThreadCount.Value;
end;

end.

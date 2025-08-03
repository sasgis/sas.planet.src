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

unit fr_PathSelect;

interface

uses
  Classes,
  Controls,
  StdCtrls,
  SysUtils,
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  ExtCtrls,
  i_LanguageManager,
  i_PathConfig,
  u_CommonFormAndFrameParents;

type
  TfrPathSelectOptions = (
    psoNotAutoIncludeDelimeters,
    psoNotShowResetToDefaultBtn,
    psoNotShowSelectPathBtn
  );

  TfrPathSelectOptionsSet = set of TfrPathSelectOptions;

  TfrPathSelect = class(TFrame)
    btnDef: TButton;
    btnSelectPath: TButton;
    lblCaption: TLabel;
    edtPath: TEdit;
    pnlPath: TPanel;
    pnlMain: TPanel;
    pnlCaption: TPanel;
    pnlButtnos: TPanel;
    procedure btnSelectPathClick(Sender: TObject);
    procedure btnDefClick(Sender: TObject);
  private
    FPathConfig: IPathConfig;
    FCaption: string;
    FOptions: TfrPathSelectOptionsSet;
    function IncludePathDelim(const APath: string): string;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ACaption: string;
      const APathConfig: IPathConfig;
      const AFrameOptions: TfrPathSelectOptionsSet = []
    ); reintroduce;
    procedure RefreshTranslation; override;
    procedure CancelChanges;
    procedure ApplyChanges;
    procedure Show(AParent: TWinControl);
  end;

implementation

uses
  gnugettext;

{$R *.dfm}

{ TfrPathSelect }

constructor TfrPathSelect.Create(
  const ALanguageManager: ILanguageManager;
  const ACaption: string;
  const APathConfig: IPathConfig;
  const AFrameOptions: TfrPathSelectOptionsSet
);
begin
  inherited Create(ALanguageManager);
  FCaption := ACaption;
  FPathConfig := APathConfig;
  FOptions := AFrameOptions;

  btnDef.Visible := not (psoNotShowResetToDefaultBtn in FOptions);
  btnSelectPath.Visible := not (psoNotShowSelectPathBtn in FOptions);

  lblCaption.Caption := _(FCaption);
end;

function TfrPathSelect.IncludePathDelim(const APath: string): string;
begin
  if psoNotAutoIncludeDelimeters in FOptions then begin
    Result := APath;
  end else begin
    Result := IncludeTrailingPathDelimiter(APath);
  end;
end;

procedure TfrPathSelect.RefreshTranslation;
begin
  inherited;
  lblCaption.Caption := _(FCaption);
end;

procedure TfrPathSelect.ApplyChanges;
begin
  FPathConfig.Path := IncludePathDelim(edtPath.Text);
end;

procedure TfrPathSelect.btnDefClick(Sender: TObject);
begin
  edtPath.Text := IncludePathDelim(FPathConfig.DefaultPath);
end;

procedure TfrPathSelect.btnSelectPathClick(Sender: TObject);
var
  VTempPath: string;
begin
  VTempPath := FPathConfig.FullPath;
  if SelectDirectory(FCaption, '', VTempPath) then begin
    edtPath.Text := StringReplace(IncludePathDelim(VTempPath), FPathConfig.BasePathConfig.Path, '.\', [rfIgnoreCase]);
  end;
end;

procedure TfrPathSelect.Show(AParent: TWinControl);
begin
  edtPath.Text := IncludePathDelim(FPathConfig.Path);
  Parent := AParent;
end;

procedure TfrPathSelect.CancelChanges;
begin
  //
end;

end.

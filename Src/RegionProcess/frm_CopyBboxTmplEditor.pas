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

unit frm_CopyBboxTmplEditor;

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
  i_RegionProcessConfig,
  u_CommonFormAndFrameParents;

type
  TfrmCopyBboxTmplEditor = class(TCommonFormParent)
    pnlBottomButtons: TPanel;
    btnApply: TButton;
    btnCancel: TButton;
    mmoTmpl: TMemo;
    procedure btnApplyClick(Sender: TObject);
  private
    FRegionProcessConfig: IRegionProcessConfig;
  public
    function ShowModal: Integer; override;
  public
    constructor Create(
      AOwner: TComponent;
      const ARegionProcessConfig: IRegionProcessConfig
    ); reintroduce;
  end;

implementation

{$R *.dfm}

uses
  i_StringListStatic,
  u_StringListStatic;

{ TfrmCopyBboxTmplEditor }

constructor TfrmCopyBboxTmplEditor.Create(
  AOwner: TComponent;
  const ARegionProcessConfig: IRegionProcessConfig
);
begin
  inherited Create(AOwner);
  FRegionProcessConfig := ARegionProcessConfig;
end;

function TfrmCopyBboxTmplEditor.ShowModal: Integer;
var
  I: Integer;
  VTmpl: IStringListStatic;
begin
  VTmpl := FRegionProcessConfig.CopyBboxTemplates;

  mmoTmpl.Lines.BeginUpdate;
  try
    mmoTmpl.Lines.Clear;
    for I := 0 to VTmpl.Count - 1 do begin
      mmoTmpl.Lines.Add(VTmpl.Items[I]);
    end;
  finally
    mmoTmpl.Lines.EndUpdate;
  end;

  Result := inherited ShowModal;
end;

procedure TfrmCopyBboxTmplEditor.btnApplyClick(Sender: TObject);
var
  I: Integer;
  VTmpl: IStringListStatic;
begin
  for I := mmoTmpl.Lines.Count - 1 downto 0 do begin
    if Trim(mmoTmpl.Lines[I]) = '' then begin
      mmoTmpl.Lines.Delete(I);
    end;
  end;

  if mmoTmpl.Lines.Count > 0 then begin
    VTmpl := TStringListStatic.CreateByStrings(mmoTmpl.Lines);
  end else begin
    VTmpl := TStringListStatic.CreateByStringDynArray(['{bbox}']);
  end;

  FRegionProcessConfig.CopyBboxTemplates := VTmpl;
  FRegionProcessConfig.CopyBboxTemplateActiveIndex := 0;
end;

end.

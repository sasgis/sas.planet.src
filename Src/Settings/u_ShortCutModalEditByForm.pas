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

unit u_ShortCutModalEditByForm;

interface

uses
  Windows,
  i_LanguageManager,
  i_ShortCutSingleConfig,
  i_ShortCutModalEdit,
  u_BaseInterfacedObject,
  frm_ShortCutEdit;

type
  TShortCutModalEditByForm = class(TBaseInterfacedObject, IShortCutModalEdit)
  private
    FLanguageManager: ILanguageManager;
    FEditCounter: Longint;
    FfrmShortCutEdit: TfrmShortCutEdit;
  private
    function EditShortCut(const AShortCutInfo: IShortCutSingleConfig): Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TShortCutModalEditByForm }

constructor TShortCutModalEditByForm.Create(
  const ALanguageManager: ILanguageManager
);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
  FEditCounter := 0;
end;

destructor TShortCutModalEditByForm.Destroy;
begin
  Assert(FEditCounter = 0);
  if FfrmShortCutEdit <> nil then begin
    FreeAndNil(FfrmShortCutEdit);
  end;

  inherited;
end;

function TShortCutModalEditByForm.EditShortCut(
  const AShortCutInfo: IShortCutSingleConfig
): Boolean;
var
  VCounter: Longint;
begin
  VCounter := InterlockedIncrement(FEditCounter);
  try
    if VCounter = 1 then begin
      if FfrmShortCutEdit = nil then begin
        FfrmShortCutEdit := TfrmShortCutEdit.Create(FLanguageManager);
      end;
      Result := FfrmShortCutEdit.EditHotKeyModal(AShortCutInfo);
    end else begin
      Result := False;
    end;
  finally
    InterlockedDecrement(FEditCounter);
  end;
end;

end.

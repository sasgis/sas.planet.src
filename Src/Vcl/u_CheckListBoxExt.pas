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

unit u_CheckListBoxExt;

interface

uses
  CheckLst;

type
  // TCheckListBox that can save/restore its items checked state (handle first 32 items only)

  TCheckListBoxExt = class(TCheckListBox)
  private
    function GetCheckedBitMask: Integer;
    procedure SetCheckedBitMask(const AValue: Integer);
  public
    property CheckedBitMask: Integer read GetCheckedBitMask write SetCheckedBitMask;
  end;

  TCheckListBox = class(TCheckListBoxExt);

implementation

{ TCheckListBoxExt }

function TCheckListBoxExt.GetCheckedBitMask: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Self.Items.Count - 1 do begin
    if Self.Checked[I] then begin
      Result := Result or (1 shl I);
    end;
    if I = 31 then begin
      Break;
    end;
  end;
end;

procedure TCheckListBoxExt.SetCheckedBitMask(const AValue: Integer);
var
  I: Integer;
begin
  for I := 0 to Self.Items.Count - 1 do begin
    Self.Checked[I] := (AValue and (1 shl I)) > 0;
    if I = 31 then begin
      Break;
    end;
  end;
end;

end.

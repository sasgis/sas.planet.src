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

unit u_TBXExtItems;

interface

uses
  Classes,
  Messages,
  TB2ExtItems,
  TBXExtItems;

type
  TOnWmPaste = procedure(var AText: string) of object;

  TTBXComboBoxItem = class (TBXExtItems.TTBXComboBoxItem)
  private
    FOldWndProc: TWndMethod;
    FOnWmPaste: TOnWmPaste;
    procedure EditWndProc(var AMsg: TMessage);
  protected
    procedure DoBeginEdit(Viewer: TTBEditItemViewer); override;
  public
    property OnWmPaste: TOnWmPaste read FOnWmPaste write FOnWmPaste;
  end;

implementation

uses
  Clipbrd;

{ TTBXComboBoxItem }

procedure TTBXComboBoxItem.DoBeginEdit(Viewer: TTBEditItemViewer);
begin
  inherited DoBeginEdit(Viewer);

  if Assigned(FOnWmPaste) then begin
    FOldWndProc := Viewer.EditControl.WindowProc;
    Viewer.EditControl.WindowProc := Self.EditWndProc;

    Assert( Assigned(FOldWndProc) );
  end;
end;

procedure TTBXComboBoxItem.EditWndProc(var AMsg: TMessage);
var
  VText, VNewText: string;
begin
  if AMsg.Msg = WM_PASTE then begin
    VText := Clipboard.AsText;
    VNewText := VText;

    FOnWmPaste(VNewText);

    if VNewText <> VText then begin
      Clipboard.AsText := VNewText;
    end;
  end;

  FOldWndProc(AMsg);
end;

end.

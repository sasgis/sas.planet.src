{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_TBXSubmenuItemWithIndicator;

interface

uses
  Graphics,
  Classes,
  TB2Item,
  TBX;

type
  TTBXSubmenuItemWithIndicator = class(TTBXSubmenuItem)
  private
    procedure AdjustFont(
      Item: TTBCustomItem;
      Viewer: TTBItemViewer;
      Font: TFont;
      StateFlags: Integer
    );
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TTBXSubmenuItemWithIndicator }

constructor TTBXSubmenuItemWithIndicator.Create(AOwner: TComponent);
begin
  inherited;
  OnAdjustFont := Self.AdjustFont;
end;

function Items_checked(const Aitem : TTBCustomItem): Boolean ;
  var i:integer;
begin
  Result := false;
  for i := 0 to Aitem.Count - 1 do begin
    if Aitem.Items[i].count >0  then begin
      Result := Items_checked(Aitem.Items[i]);
      if Result then Break;
    end;
    if Aitem.Items[i].Checked then begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TTBXSubmenuItemWithIndicator.AdjustFont(
  Item: TTBCustomItem;
  Viewer: TTBItemViewer;
  Font: TFont;
  StateFlags: Integer
);
begin
  if Items_checked(Self) then begin
    Self.FontSettings.Bold := tsTrue;
  end else begin
    Self.FontSettings.Bold := tsDefault;
  end;
end;

end.

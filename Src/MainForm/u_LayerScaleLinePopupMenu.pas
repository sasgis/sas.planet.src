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

unit u_LayerScaleLinePopupMenu;

interface

uses
  Menus,
  GR32_Image,
  i_PopUp,
  u_BaseInterfacedObject;

type
  TLayerScaleLinePopupMenu = class(TBaseInterfacedObject, IPopUp)
  private
    FParentMap: TImage32;
    FPopup: TPopupMenu;
  private
    procedure PopUp;
  public
    constructor Create(
      const AParentMap: TImage32;
      const APopup: TPopupMenu
    );
  end;

implementation

{ TLayerScaleLinePopupMenu }

constructor TLayerScaleLinePopupMenu.Create(
  const AParentMap: TImage32;
  const APopup: TPopupMenu
);
begin
  Assert(Assigned(AParentMap));
  Assert(Assigned(APopup));
  inherited Create;
  FParentMap := AParentMap;
  FPopup := APopup;
end;

procedure TLayerScaleLinePopupMenu.PopUp;
begin
  FParentMap.PopupMenu := FPopup;
end;

end.

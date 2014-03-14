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

unit u_PathDetalizeProviderListEntity;

interface

uses
  i_PathDetalizeProvider,
  i_StringConfigDataElement,
  i_PathDetalizeProviderList,
  u_UserInterfaceItemBase;

type
  TPathDetalizeProviderListEntity = class(TUserInterfaceItemBase, IPathDetalizeProviderListEntity)
  private
    FProvider: IPathDetalizeProvider;
  private
    function GetProvider: IPathDetalizeProvider;
  public
    constructor Create(
      const AGUID: TGUID;
      const ACaption: IStringConfigDataElement;
      const ADescription: IStringConfigDataElement;
      const AMenuItemName: IStringConfigDataElement;
      const AProvider: IPathDetalizeProvider
    );
  end;


implementation

{ TPathDetalizeProviderListEntity }

constructor TPathDetalizeProviderListEntity.Create(
  const AGUID: TGUID;
  const ACaption: IStringConfigDataElement;
  const ADescription: IStringConfigDataElement;
  const AMenuItemName: IStringConfigDataElement;
  const AProvider: IPathDetalizeProvider
);
begin
  inherited Create(AGUID, ACaption, ADescription, AMenuItemName);
  FProvider := AProvider;
end;

function TPathDetalizeProviderListEntity.GetProvider: IPathDetalizeProvider;
begin
  Result := FProvider;
end;

end.

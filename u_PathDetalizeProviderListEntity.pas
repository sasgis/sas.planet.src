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

unit u_PathDetalizeProviderListEntity;

interface

uses
  i_PathDetalizeProvider,
  i_VectorItemLonLat,
  u_UserInterfaceItemBase,
  i_PathDetalizeProviderList;

type
  TPathDetalizeProviderListEntity = class(TUserInterfaceItemBase, IPathDetalizeProviderListEntity, IPathDetalizeProvider)
  protected { IPathDetalizeProviderListEntity }
    function GetProvider: IPathDetalizeProvider;
  protected { IPathDetalizeProvider }
    function GetPath(const ASource: ILonLatPath; var AComment: string): ILonLatPath; virtual; abstract;
  end;


implementation

{ TPathDetalizeProviderListEntity }

function TPathDetalizeProviderListEntity.GetProvider: IPathDetalizeProvider;
begin
  Result := Self;
end;

end.

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

unit i_PathDetalizeProviderList;

interface

uses
  ActiveX,
  i_Changeable,
  i_PathDetalizeProvider;

type
  IPathDetalizeProviderListEntity = interface(IChangeable)
    ['{343F27D6-4DDE-46D8-8F58-4BA220C1733E}']
    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    function GetDescription: string;
    property Description: string read GetDescription;

    function GetMenuItemName: string;
    property MenuItemName: string read GetMenuItemName;

    function GetProvider: IPathDetalizeProvider;
  end;

  IPathDetalizeProviderList = interface(IChangeable)
    ['{73A94DEE-3216-402E-9A22-90E84A215CEC}']
    function GetGUIDEnum: IEnumGUID;
    function Get(const AGUID: TGUID): IPathDetalizeProviderListEntity;
  end;


implementation

end.

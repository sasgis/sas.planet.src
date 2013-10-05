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

unit i_MapTypes;

interface

uses
  i_Changeable,
  u_MapType;

type
  IMapType = interface
    ['{85957D2C-19D7-4F44-A183-F3679B2A5973}']
    function GetMapType: TMapType;
    property MapType: TMapType read GetMapType;

    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;
  end;

  IMapTypeChangeable = interface(IChangeable)
    ['{8B43402D-0D20-4A6B-8198-71DDAAADD2A9}']
    function GetStatic: IMapType;
  end;

implementation

end.

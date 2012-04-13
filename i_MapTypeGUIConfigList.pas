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

unit i_MapTypeGUIConfigList;

interface

uses
  i_GUIDListStatic,
  i_MapTypeHotKeyListStatic,
  i_ConfigDataElement;

type
  IMapTypeGUIConfigList = interface(IConfigDataElement)
    ['{6EAFA879-3A76-40CA-89A7-598D45E2C92E}']
    function GetOrderedMapGUIDList: IGUIDListStatic;
    property OrderedMapGUIDList: IGUIDListStatic read GetOrderedMapGUIDList;

    function GetHotKeyList: IMapTypeHotKeyListStatic;
    property HotKeyList: IMapTypeHotKeyListStatic read GetHotKeyList;
  end;

implementation

end.

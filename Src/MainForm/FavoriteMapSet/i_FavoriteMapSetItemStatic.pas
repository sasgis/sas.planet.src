{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit i_FavoriteMapSetItemStatic;

interface

uses
  Classes,
  i_GUIDListStatic;

type
  IFavoriteMapSetItemStatic = interface
    ['{35E1D826-5C9A-473D-B4EC-13261D7A037F}']
    function GetID: TGUID;
    property ID: TGUID read GetID;

    function GetBaseMap: TGUID;
    property BaseMap: TGUID read GetBaseMap;

    function GetLayers: IGUIDSetStatic;
    property Layers: IGUIDSetStatic read GetLayers;

    function GetZoom: Integer;
    property Zoom: Integer read GetZoom;

    function GetName: string;
    property Name: string read GetName;

    function GetHotKey: TShortCut;
    property HotKey: TShortCut read GetHotKey;

    function GetSortIndex: Integer;
    property SortIndex: Integer read GetSortIndex;
  end;

implementation

end.

{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit i_TileStorageTypeListItem;

interface

uses
  i_TileStorageType;

type
  ITileStorageTypeListItem = interface
    ['{103AA969-E2D0-4E66-8B8D-F78E6D442E7D}']
    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    function GetIntCode: Integer;
    property IntCode: Integer read GetIntCode;

    function GetCaption: string;
    property Caption: string read GetCaption;

    function GetStorageType: ITileStorageType;
    property StorageType: ITileStorageType read GetStorageType;

    function GetCanUseAsDefault: Boolean;
    property CanUseAsDefault: Boolean read GetCanUseAsDefault;

    function GetIsChangeable: Boolean;
    property IsChangeable: Boolean read GetIsChangeable;
  end;


implementation

end.

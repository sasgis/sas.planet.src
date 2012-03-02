{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit i_TileStorageType;

interface

uses
  i_StorageTypeAbilities,
  i_MapVersionConfig,
  i_TileStorageTypeConfig,
  i_TileStorage;

type
  ITileStorageType = interface
    ['{EBB122FB-5382-49CA-A265-3BEA89694B0E}']
    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    function GetInfo: IStorageTypeAbilities;
    property Info: IStorageTypeAbilities read GetInfo;

    function GetConfig: ITileStorageTypeConfig;
    property Config: ITileStorageTypeConfig read GetConfig;

    function GetMapVersionFactory: IMapVersionFactory;
    property MapVersionFactory: IMapVersionFactory read GetMapVersionFactory;

    function GetCaption: string;
    property Caption: string read GetCaption;

    function BuildStorage(APath: string): ITileStorage;
  end;

implementation

end.

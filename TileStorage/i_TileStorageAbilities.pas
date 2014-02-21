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

unit i_TileStorageAbilities;

interface

type
  ITileStorageAbilities = interface
    ['{EBB122FB-5382-49CA-A265-3BEA89694B0E}']
    function GetAllowRead: Boolean;
    property AllowRead: Boolean read GetAllowRead;

    function GetIsReadOnly: boolean;
    property IsReadOnly: boolean read GetIsReadOnly;

    function GetAllowAdd: Boolean;
    property AllowAdd: Boolean read GetAllowAdd;

    function GetAllowDelete: boolean;
    property AllowDelete: boolean read GetAllowDelete;

    function GetAllowReplace: boolean;
    property AllowReplace: boolean read GetAllowReplace;
  end;

  ITileStorageTypeAbilities = interface
    ['{EEB09E02-E81A-4566-866F-356008CC808D}']
    function GetBaseStorageAbilities: ITileStorageAbilities;
    property BaseStorageAbilities: ITileStorageAbilities read GetBaseStorageAbilities;

    function GetIsVersioned: Boolean;
    property IsVersioned: Boolean read GetIsVersioned;

    function GetIsFileCache: Boolean;
    property IsFileCache: Boolean read GetIsFileCache;
  end;

implementation

end.

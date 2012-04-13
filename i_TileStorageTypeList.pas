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

unit i_TileStorageTypeList;

interface

uses
  ActiveX,
  i_ConfigDataElement,
  i_TileStorageType;

type
  ITileStorageTypeList = interface(IConfigDataElement)
    ['{42BD0720-3B8A-4F19-8208-C6E4105377DE}']
    function GetDefault: ITileStorageType;
    procedure SetDefaultByGUID(const AGUID: TGUID);

    function Get(const AGUID: TGUID): ITileStorageType;
    function GetCanUseAsDefault(const AGUID: TGUID): Boolean;
    function GetEnum: IEnumGUID;
  end;

implementation

end.

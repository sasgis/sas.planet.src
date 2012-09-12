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

unit i_MapAbilitiesConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapAbilitiesConfigStatic = interface
    ['{89BC8688-41A7-4ADE-A911-E90BAC6B5689}']
    function GetIsLayer: Boolean;
    property IsLayer: Boolean read GetIsLayer;

    function GetIsShowOnSmMap: Boolean;
    property IsShowOnSmMap: Boolean read GetIsShowOnSmMap;

    function GetUseDownload: Boolean;
    property UseDownload: Boolean read GetUseDownload;
  end;

  IMapAbilitiesConfig = interface(IConfigDataElement)
    ['{6CF60AD7-0284-4252-AC55-2A2C1ABAF4FC}']
    function GetIsLayer: Boolean;
    property IsLayer: Boolean read GetIsLayer;

    function GetIsShowOnSmMap: Boolean;
    procedure SetIsShowOnSmMap(AValue: Boolean);
    property IsShowOnSmMap: Boolean read GetIsShowOnSmMap write SetIsShowOnSmMap;

    function GetUseDownload: Boolean;
    procedure SetUseDownload(AValue: Boolean);
    property UseDownload: Boolean read GetUseDownload write SetUseDownload;

    function GetStatic: IMapAbilitiesConfigStatic;
  end;

implementation

end.

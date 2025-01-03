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

unit i_MapAbilitiesConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapAbilitiesConfigStatic = interface
    ['{89BC8688-41A7-4ADE-A911-E90BAC6B5689}']
    function GetIsShowOnSmMap: Boolean;
    property IsShowOnSmMap: Boolean read GetIsShowOnSmMap;

    function GetUseDownload: Boolean;
    property UseDownload: Boolean read GetUseDownload;
  end;

  IMapAbilitiesConfig = interface(IConfigDataElement)
    ['{6CF60AD7-0284-4252-AC55-2A2C1ABAF4FC}']
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

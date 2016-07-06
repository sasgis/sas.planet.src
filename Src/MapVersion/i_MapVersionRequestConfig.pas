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

unit i_MapVersionRequestConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapVersionRequestConfigStatic = interface
    ['{296D487B-53A6-4715-B024-88BFECC14C76}']
    function GetVersion: string;
    property Version: string read GetVersion;

    function GetShowOtherVersions: Boolean;
    property ShowOtherVersions: Boolean read GetShowOtherVersions;
  end;

  IMapVersionRequestConfig = interface(IConfigDataElement)
    ['{0D710534-C49F-43BC-8092-A0F5ABB5E107}']
    function GetVersion: string;
    procedure SetVersion(const AValue: string);
    property Version: string read GetVersion write SetVersion;

    function GetShowOtherVersions: Boolean;
    procedure SetShowOtherVersions(const AValue: Boolean);
    property ShowOtherVersions: Boolean read GetShowOtherVersions write SetShowOtherVersions;

    function GetStatic: IMapVersionRequestConfigStatic;
  end;

implementation

end.

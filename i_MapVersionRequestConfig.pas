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

unit i_MapVersionRequestConfig;

interface

uses
  i_MapVersionInfo,
  i_MapVersionRequest,
  i_MapVersionFactory,
  i_ConfigDataElement;

type
  IMapVersionRequestConfig = interface(IConfigDataElement)
    ['{0D710534-C49F-43BC-8092-A0F5ABB5E107}']
    function GetVersionFactory: IMapVersionFactoryChangeable;
    property VersionFactory: IMapVersionFactoryChangeable read GetVersionFactory;

    function GetVersion: IMapVersionInfo;
    procedure SetVersion(const AValue: IMapVersionInfo);
    property Version: IMapVersionInfo read GetVersion write SetVersion;

    function GetShowPrevVersion: Boolean;
    procedure SetShowPrevVersion(const AValue: Boolean);
    property ShowPrevVersion: Boolean read GetShowPrevVersion write SetShowPrevVersion;

    function GetStatic: IMapVersionRequest;
  end;

implementation

end.

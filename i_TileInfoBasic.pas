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

unit i_TileInfoBasic;

interface

uses
  i_MapVersionInfo,
  i_ContentTypeInfo;

type
  ITileInfoBasic = interface
    ['{7916FA97-49F1-451E-B2C1-0669B9336291}']
    function GetIsExists: Boolean;
    property IsExists: Boolean read GetIsExists;

    function GetIsExistsTNE: Boolean;
    property IsExistsTNE: Boolean read GetIsExistsTNE;

    function GetLoadDate: TDateTime;
    property LoadDate: TDateTime read GetLoadDate;

    function GetTile: Pointer;
    property Tile: Pointer read GetTile;

    function GetSize: Cardinal;
    property Size: Cardinal read GetSize;

    function GetVersionInfo: IMapVersionInfo;
    property VersionInfo: IMapVersionInfo read GetVersionInfo;

    function GetContentType: IContentTypeInfoBasic;
    property ContentType: IContentTypeInfoBasic read GetContentType;
  end;


implementation

end.

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

unit i_TileRequest;

interface

uses
  Types,
  i_MapVersionInfo;

type
  ITileRequest = interface
    ['{2E7FE7D3-1343-4823-876A-BBAD4D483728}']
    function GetTile: TPoint;
    property Tile: TPoint read GetTile;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetVersionInfo: IMapVersionInfo;
    property VersionInfo: IMapVersionInfo read GetVersionInfo;
  end;

  ITileRequestWithSizeCheck = interface(ITileRequest)
    ['{7F77BF27-E741-40BD-9F8B-D25CDC09C57B}']
  end;

implementation

end.

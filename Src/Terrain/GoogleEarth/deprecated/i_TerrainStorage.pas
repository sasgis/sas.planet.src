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

unit i_TerrainStorage deprecated;

interface

uses
  Types,
  i_Notifier,
  i_TileInfoBasic;

type
  ITerrainStorage = interface
    ['{ADBF684D-72B7-493B-B79D-5D88F6A17194}']

    function GetAvailable: Boolean;
    property Available: Boolean read GetAvailable;

    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: Byte
    ): ITileInfoBasic;

    function SetPath(const APath: string): Boolean;

    function GetNotifier: INotifier;
    property Notifier: INotifier read GetNotifier;
  end;

implementation

end.

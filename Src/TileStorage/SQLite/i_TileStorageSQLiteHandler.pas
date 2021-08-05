{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit i_TileStorageSQLiteHandler;

interface

uses
  Types,
  t_TileStorageSQLite,
  t_NotifierOperationRec,
  i_MapVersionInfo,
  i_MapVersionListStatic;

type
  ITileStorageSQLiteHandler = interface
    ['{D35C0131-C9AD-47E2-BCBF-7589AF2C5CDE}']
    // check database available
    function Opened: Boolean;

    // select tile
    function GetTileInfo(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoModeSQLite;
      const AShowOtherVersions: Boolean;
      const AResult: PGetTileResult
    ): Boolean;

    // delete tile
    function DeleteTile(const ADeleteTileAllData: PDeleteTileAllData): Boolean;

    // save tile
    function SaveTile(const ASaveTileAllData: PSaveTileAllData): Boolean;

    // get list of versions
    function GetListOfTileVersions(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): IMapVersionListStatic;

    // get information by tile rect
    function GetTileRectInfo(
      const AOper: PNotifierOperationRec;
      const AShowOtherVersions: Boolean;
      const AEnumData: TTileInfoShortEnumData
    ): Boolean;
  end;

implementation

end.

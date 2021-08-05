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

unit i_TileStorageSQLiteHelper;

interface

uses
  Types,
  t_TileStorageSQLite,
  t_NotifierOperationRec,
  i_MapVersionRequest,
  i_MapVersionInfo,
  i_MapVersionListStatic,
  i_TileStorage;

type
  ITileStorageSQLiteHelper = interface
    ['{581A3C7C-86C2-4B7C-9B42-FC79CEB333C8}']
    // sync
    procedure Sync;

    // load tile info
    function GetTileInfo(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode;
      const AShowOtherVersions: Boolean;
      const AResult: PGetTileResult
    ): Boolean;

    // load tile rect info
    function GetTileRectInfo(
      const AOper: PNotifierOperationRec;
      const AShowOtherVersions: Boolean;
      const AEnumData: TTileInfoShortEnumData
    ): Boolean;

    // delete tile
    function DeleteTile(
      const AOper: PNotifierOperationRec;
      const ADeleteTileAllData: PDeleteTileAllData
    ): Boolean;

    // save tile or tne to storage
    function SaveTile(
      const AOper: PNotifierOperationRec;
      const ASaveTileAllData: PSaveTileAllData
    ): Boolean;

    // get list of versions
    function GetListOfTileVersions(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersion: IMapVersionRequest
    ): IMapVersionListStatic;
  end;

implementation

end.

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

unit i_TileStorage;

interface

uses
  Types,
  i_BinaryData,
  i_CoordConverter,
  i_NotifierTilePyramidUpdate,
  i_ContentTypeInfo,
  i_MapVersionInfo,
  i_MapVersionListStatic,
  i_StorageState,
  i_TileInfoBasic;

type
  TGetTileInfoMode = (gtimWithData = 1, gtimWithoutData = -1, gtimAsIs = 0);

  ITileStorage = interface
    ['{80A0246E-68E0-4EA0-9B0F-3338472FDB3C}']
    function GetTileNotifier: INotifierTilePyramidUpdate;
    property TileNotifier: INotifierTilePyramidUpdate read GetTileNotifier;

    function GetState: IStorageStateChangeble;
    property State: IStorageStateChangeble read GetState;

    function GetCoordConverter: ICoordConverter;
    property CoordConverter: ICoordConverter read GetCoordConverter;

    function GetIsFileCache: Boolean;
    property IsFileCache: Boolean read GetIsFileCache;

    function GetIsCanSaveMultiVersionTiles: Boolean;
    function AllowListOfTileVersions: Boolean;
    function AllowShowPrevVersion: Boolean;

    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo
    ): string;

    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo
    ): Boolean;

    // For save tne AContentType = nil or AData = nil
    // returns True if ok, False if exists and AIsOverwrite = false
    function SaveTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData;
      const AIsOverwrite: Boolean
    ): Boolean;

    function GetListOfTileVersions(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): IMapVersionListStatic;

    function GetTileRectInfo(
      const ARect: TRect;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileRectInfo;

    function ScanTiles(
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    ): IEnumTileInfo;
  end;

implementation

end.

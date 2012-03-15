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

unit i_TileStorage;

interface

uses
  Classes,
  GR32,
  i_BinaryData,
  i_OperationNotifier,
  i_TileRectUpdateNotifier,
  i_MapVersionInfo,
  i_StorageState,
  i_TileInfoBasic,
  i_FillingMapColorer,
  i_TileStorageInfo;

type
  ITileStorage = interface
    ['{80A0246E-68E0-4EA0-9B0F-3338472FDB3C}']
    function GetInfo: ITileStorageInfo;
    property Info: ITileStorageInfo read GetInfo;

    function GetNotifierByZoom(AZoom: Byte): ITileRectUpdateNotifier;
    property NotifierByZoom[AZoom: Byte]: ITileRectUpdateNotifier read GetNotifierByZoom;

    function GetState: IStorageStateChangeble;
    property State: IStorageStateChangeble read GetState;

    function GetTileFileName(
      AXY: TPoint;
      Azoom: byte;
      AVersion: IMapVersionInfo
    ): string;
    function GetTileInfo(
      AXY: TPoint;
      Azoom: byte;
      AVersion: IMapVersionInfo
    ): ITileInfoBasic;

    function LoadTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo;
      out ATileInfo: ITileInfoBasic
    ): IBinaryData;
    function DeleteTile(
      AXY: TPoint;
      Azoom: byte;
      AVersion: IMapVersionInfo
    ): Boolean;
    function DeleteTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersion: IMapVersionInfo
    ): Boolean;
    procedure SaveTile(
      AXY: TPoint;
      Azoom: byte;
      AVersion: IMapVersionInfo;
      AData: IBinaryData
    );
    procedure SaveTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersion: IMapVersionInfo
    );

    function LoadFillingMap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      btm: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      ASourceZoom: byte;
      AVersion: IMapVersionInfo;
      AColorer: IFillingMapColorer
    ): boolean;
  end;


implementation

end.

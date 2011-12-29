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
  i_TileInfoBasic,
  i_TileStorageInfo;

type
  ITileStorage = interface
    ['{80A0246E-68E0-4EA0-9B0F-3338472FDB3C}']
    function GetInfo: ITileStorageInfo;

    function GetTileFileName(AXY: TPoint; Azoom: byte; AVersion: Variant): string;
    function GetTileInfo(AXY: TPoint; Azoom: byte; AVersion: Variant): ITileInfoBasic;

    function LoadTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream; out ATileInfo: ITileInfoBasic): Boolean;
    function DeleteTile(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
    function DeleteTNE(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
    procedure SaveTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream);
    procedure SaveTNE(AXY: TPoint; Azoom: byte; AVersion: Variant);

    function LoadFillingMap(
      btm: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      ASourceZoom: byte;
      AVersion: Variant;
      IsStop: PBoolean
    ): boolean;
  end;


implementation

end.

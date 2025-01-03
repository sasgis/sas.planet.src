{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit i_TileInfoBasicMemCache;

interface

uses
  Types,
  i_Notifier,
  i_MapVersionInfo,
  i_TileStorage,
  i_TileInfoBasic;

type
  TOnTileInfoUpdateNotify = procedure(
    const ATile: TPoint;
    const AZoom: Byte;
    const AVersion: IMapVersionInfo
  ) of object;

  ITileInfoBasicMemCache = interface
    ['{9D01F466-1089-46D7-B13B-1ED1116A8376}']
    procedure Add(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ATileInfoBasic: ITileInfoBasic
    );

    function Remove(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean;

    function Get(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode;
      const AUpdateTTL: Boolean
    ): ITileInfoBasic;

    procedure Clear;

    function GetClearByTTLNotifier: INotifier;
    property ClearByTTLNotifier: INotifier read GetClearByTTLNotifier;

    function GetOnTileInfoUpdate: TOnTileInfoUpdateNotify;
    procedure SetOnTileInfoUpdate(const AValue: TOnTileInfoUpdateNotify);
    property OnTileInfoUpdate: TOnTileInfoUpdateNotify read GetOnTileInfoUpdate write SetOnTileInfoUpdate;

    function GetEnum(
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    ): IEnumTileInfo;
  end;

implementation

end.

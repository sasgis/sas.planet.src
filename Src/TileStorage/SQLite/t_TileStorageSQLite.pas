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

unit t_TileStorageSQLite;

interface

uses
  Types,
  i_BinaryData,
  i_TileInfoBasic,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  u_TileRectInfoShort; // for TArrayOfTileInfoShortInternal

type
  TGetTileInfoItem = (
    gtiiLoadDate,
    gtiiSize,
    gtiiBody,
    gtiiContentType
  );
  TGetTileInfoModeSQLite = set of TGetTileInfoItem;

  TDeleteTileAllData = record
    DXY: TPoint;
    DZoom: Byte;
    DVersionInfo: IMapVersionInfo;
  end;
  PDeleteTileAllData = ^TDeleteTileAllData;

  TSaveTileFlag = (
    stfKeepExisting,
    stfSkipIfSameAsPrev
  );
  TSaveTileFlags = set of TSaveTileFlag;

  TSaveTileAllData = record
    SXY: TPoint;
    SZoom: Byte;
    SVersionInfo: IMapVersionInfo;
    SLoadDate: TDateTime;
    SContentType: IContentTypeInfoBasic;
    SData: IBinaryData;
    SSaveTileFlags: TSaveTileFlags;
  end;
  PSaveTileAllData = ^TSaveTileAllData;

  TGetTileInfo = record
    GTilePos: TPoint;
    GZoom: Byte;
    GVersion: IMapVersionInfo;
    GShowOtherVersions: Boolean;
    GMode: TGetTileInfoModeSQLite;
  end;
  PGetTileInfo = ^TGetTileInfo;

  TTileInfoShortEnumData = record
    DestRect: TRect;
    DestZoom: Byte;
    RectVersionInfo: IMapVersionInfo;
    RectCount: TPoint;
    RectItems: TArrayOfTileInfoShortInternal;
  end;
  PTileInfoShortEnumData = ^TTileInfoShortEnumData;

  TGetTileResult = record
    // результат
    GTileInfo: ITileInfoBasic;
    // дополнительные параметры, которые прочитаны, но которые возможно не запрашивались
    GExtraMode: TGetTileInfoModeSQLite;
  end;
  PGetTileResult = ^TGetTileResult;

implementation

end.

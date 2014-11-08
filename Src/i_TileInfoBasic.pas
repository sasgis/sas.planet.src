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

unit i_TileInfoBasic;

interface

uses
  Types,
  i_BinaryData,
  i_TileIterator,
  i_MapVersionInfo,
  i_ContentTypeInfo;

type
  TTileInfoType = (titUnknown = 0, titNotExists = 1, titExists = 2, titTneExists = 3);

  TTileInfo = record
    FTile: TPoint;
    FLoadDate: TDateTime;
    FVersionInfo: IMapVersionInfo;
    FContentType: IContentTypeInfoBasic;
    FData: IBinaryData;
    FSize: Cardinal;
    FZoom: Byte;
    FInfoType: TTileInfoType;
  end;

  IEnumTileInfo = interface
    ['{E04CF298-53DE-4ABC-A083-03C917599DE5}']
    function Next(var ATileInfo: TTileInfo): Boolean;
  end;

  ITileRectInfo = interface
    ['{43C243C5-203A-41D3-9454-35A1CB8250D5}']
    function GetTileRect: TRect;
    property TileRect: TRect read GetTileRect;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetEnum(const ATileIterator: ITileIterator): IEnumTileInfo;
  end;

  ITileInfoBasic = interface
    ['{7916FA97-49F1-451E-B2C1-0669B9336291}']
    function GetIsExists: Boolean;
    property IsExists: Boolean read GetIsExists;

    function GetIsExistsTNE: Boolean;
    property IsExistsTNE: Boolean read GetIsExistsTNE;

    function GetLoadDate: TDateTime;
    property LoadDate: TDateTime read GetLoadDate;

    function GetSize: Cardinal;
    property Size: Cardinal read GetSize;

    function GetVersionInfo: IMapVersionInfo;
    property VersionInfo: IMapVersionInfo read GetVersionInfo;

    function GetContentType: IContentTypeInfoBasic;
    property ContentType: IContentTypeInfoBasic read GetContentType;
  end;

  ITileInfoWithData = interface(ITileInfoBasic)
    ['{A268209A-DE1A-442E-AE25-6B6284080092}']
    function GetTileData: IBinaryData;
    property TileData: IBinaryData read GetTileData;
  end;

implementation

end.

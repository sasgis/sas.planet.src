{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit i_TileStorageBerkeleyDBHelper;

interface

uses
  Types,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_BinaryData;

type
  TPointArray = array of TPoint;

  ITileStorageBerkeleyDBHelper = interface
    ['{FF965C3E-DE85-4ADE-B275-32F889AE22E9}']
    function SaveTile(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const ATileDate: TDateTime;
      const AVersionInfo: IMapVersionInfo;
      const ATileContetType: IContentTypeInfoBasic;
      const AData: IBinaryData;
      const AKeepExisting: Boolean
    ): Boolean;

    function DeleteTile(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean;

    function LoadTile(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileBinaryData: IBinaryData;
      out ATileVersion: WideString;
      out ATileContentType: WideString;
      out ATileDate: TDateTime
    ): Boolean;

    function LoadTileInfo(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ASingleTileInfo: Boolean;
      out ATileVersionListStatic: IMapVersionListStatic;
      out ATileVersion: WideString;
      out ATileContentType: WideString;
      out ATileSize: Integer;
      out ATileDate: TDateTime
    ): Boolean;

    function TileExists(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean;

    function IsTNEFound(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileDate: TDateTime
    ): Boolean;

    function GetTileExistsArray(
      const ADatabaseFileName: string;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileExistsArray: TPointArray
    ): Boolean;

    procedure Sync(out AHotDatabaseCount: Integer);

    function GetRefCount: Integer;
    property RefCount: Integer read GetRefCount;
  end;

implementation

end.

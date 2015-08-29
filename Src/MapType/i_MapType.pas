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

unit i_MapType;

interface

uses
  Types,
  i_Changeable,
  i_MapVersionInfo,
  i_MapVersionRequest,
  i_TileObjCache,
  i_Bitmap32Static,
  i_VectorItemSubset,
  i_ProjectionInfo,
  i_CoordConverter,
  i_ZmpInfo,
  i_MapVersionRequestConfig,
  i_ContentTypeInfo,
  i_MapAbilitiesConfig,
  i_SimpleTileStorageConfig,
  i_TileDownloadSubsystem,
  i_TileStorage,
  i_MapTypeGUIConfig,
  i_LayerDrawConfig,
  i_TileDownloaderConfig,
  i_TileDownloadRequestBuilderConfig,
  i_ConfigDataWriteProvider;

type
  IMapType = interface
    ['{85957D2C-19D7-4F44-A183-F3679B2A5973}']
    procedure SaveConfig(const ALocalConfig: IConfigDataWriteProvider);

    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    procedure ClearMemCache;
    function GetTileShowName(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo
    ): string;
    function LoadTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionRequest;
      IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function LoadTileVector(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionRequest;
      IgnoreError: Boolean;
      const ACache: ITileObjCacheVector = nil
    ): IVectorItemSubset;
    function LoadTileUni(
      const AXY: TPoint;
      const AProjection: IProjectionInfo;
      const AVersion: IMapVersionRequest;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function LoadBitmap(
      const APixelRectTarget: TRect;
      const AZoom: byte;
      const AVersion: IMapVersionRequest;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;
    function LoadBitmapUni(
      const APixelRectTarget: TRect;
      const AProjection: IProjectionInfo;
      const AVersion: IMapVersionRequest;
      AUsePre, AAllowPartial, IgnoreError: Boolean;
      const ACache: ITileObjCacheBitmap = nil
    ): IBitmap32Static;

    function GetShortFolderName: string;

    function GetZmp: IZmpInfo;
    property Zmp: IZmpInfo read GetZmp;

    function GetGeoConvert: ICoordConverter;
    property GeoConvert: ICoordConverter read GetGeoConvert;
    function GetViewGeoConvert: ICoordConverter;
    property ViewGeoConvert: ICoordConverter read GetViewGeoConvert;
    function GetVersionRequestConfig: IMapVersionRequestConfig;
    property VersionRequestConfig: IMapVersionRequestConfig read GetVersionRequestConfig;
    function GetContentType: IContentTypeInfoBasic;
    property ContentType: IContentTypeInfoBasic read GetContentType;

    function GetAbilities: IMapAbilitiesConfig;
    property Abilities: IMapAbilitiesConfig read GetAbilities;
    function GetStorageConfig: ISimpleTileStorageConfig;
    property StorageConfig: ISimpleTileStorageConfig read GetStorageConfig;
    function GetIsBitmapTiles: Boolean;
    property IsBitmapTiles: Boolean read GetIsBitmapTiles;
    function GetIsKmlTiles: Boolean;
    property IsKmlTiles: Boolean read GetIsKmlTiles;

    function GetTileDownloadSubsystem: ITileDownloadSubsystem;
    property TileDownloadSubsystem: ITileDownloadSubsystem read GetTileDownloadSubsystem;
    function GetTileStorage: ITileStorage;
    property TileStorage: ITileStorage read GetTileStorage;
    function GetGUIConfig: IMapTypeGUIConfig;
    property GUIConfig: IMapTypeGUIConfig read GetGUIConfig;
    function GetLayerDrawConfig: ILayerDrawConfig;
    property LayerDrawConfig: ILayerDrawConfig read GetLayerDrawConfig;
    function GetTileDownloaderConfig: ITileDownloaderConfig;
    property TileDownloaderConfig: ITileDownloaderConfig read GetTileDownloaderConfig;
    function GetTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
    property TileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig read GetTileDownloadRequestBuilderConfig;
    function GetCacheBitmap: ITileObjCacheBitmap;
    property CacheBitmap: ITileObjCacheBitmap read GetCacheBitmap;
    function GetCacheVector: ITileObjCacheVector;
    property CacheVector: ITileObjCacheVector read GetCacheVector;
  end;

  IMapTypeChangeable = interface(IChangeable)
    ['{8B43402D-0D20-4A6B-8198-71DDAAADD2A9}']
    function GetStatic: IMapType;
  end;

implementation

end.

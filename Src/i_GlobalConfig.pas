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

unit i_GlobalConfig;

interface

uses
  i_PathConfig,
  i_GlobalAppConfig,
  i_LanguageManager,
  i_InetConfig,
  i_BitmapPostProcessingConfig,
  i_CoordRepresentationConfig,
  i_ValueToStringConverterConfig,
  i_ImageResamplerConfig,
  i_MainMemCacheConfig,
  i_GPSConfig,
  i_InternalDebugConfig,
  i_GlobalViewMainConfig,
  i_MarkFactoryConfig,
  i_MarkCategoryFactoryConfig,
  i_MarksGUIConfig,
  i_GlobalDownloadConfig,
  i_ThreadConfig,
  i_StartUpLogoConfig,
  i_WindowPositionConfig,
  i_TerrainConfig,
  i_ZmpConfig,
  i_MapSvcScanConfig,
  i_ConfigDataElement;

type
  IGlobalConfig = interface(IConfigDataElement)
    ['{D683FBDE-9549-4581-8516-9ADDCEA64C23}']
    function GetBaseCahcePath: IPathConfig;
    property BaseCahcePath: IPathConfig read GetBaseCahcePath;

    function GetMapsPath: IPathConfig;
    property MapsPath: IPathConfig read GetMapsPath;

    function GetTrackPath: IPathConfig;
    property TrackPath: IPathConfig read GetTrackPath;

    function GetMarksDbPath: IPathConfig;
    property MarksDbPath: IPathConfig read GetMarksDbPath;

    function GetMarksIconsPath: IPathConfig;
    property MarksIconsPath: IPathConfig read GetMarksIconsPath;

    function GetMediaDataPath: IPathConfig;
    property MediaDataPath: IPathConfig read GetMediaDataPath;

    function GetTerrainDataPath: IPathConfig;
    property TerrainDataPath: IPathConfig read GetTerrainDataPath;

    function GetUserDataPath: IPathConfig;
    property UserDataPath: IPathConfig read GetUserDataPath;

    function GetUpdatesPath: IPathConfig;
    property UpdatesPath: IPathConfig read GetUpdatesPath;

    function GetLastSelectionFileName: IPathConfig;
    property LastSelectionFileName: IPathConfig read GetLastSelectionFileName;

    function GetGpsRecorderFileName: IPathConfig;
    property GpsRecorderFileName: IPathConfig read GetGpsRecorderFileName;

    function GetGpsTrackRecorderFileName: IPathConfig;
    property GpsTrackRecorderFileName: IPathConfig read GetGpsTrackRecorderFileName;

    function GetInternalDebugConfig: IInternalDebugConfig;
    property InternalDebugConfig: IInternalDebugConfig read GetInternalDebugConfig;

    function GetGlobalAppConfig: IGlobalAppConfig;
    property GlobalAppConfig: IGlobalAppConfig read GetGlobalAppConfig;

    function GetLanguageManager: ILanguageManager;
    property LanguageManager: ILanguageManager read GetLanguageManager;

    function GetInetConfig: IInetConfig;
    property InetConfig: IInetConfig read GetInetConfig;

    function GetInternalBrowserConfig: IWindowPositionConfig;
    property InternalBrowserConfig: IWindowPositionConfig read GetInternalBrowserConfig;

    function GetMainThreadConfig: IThreadConfig;
    property MainThreadConfig: IThreadConfig read GetMainThreadConfig;

    function GetTileLoadResamplerConfig: IImageResamplerConfig;
    property TileLoadResamplerConfig: IImageResamplerConfig read GetTileLoadResamplerConfig;

    function GetTileGetPrevResamplerConfig: IImageResamplerConfig;
    property TileGetPrevResamplerConfig: IImageResamplerConfig read GetTileGetPrevResamplerConfig;

    function GetTileReprojectResamplerConfig: IImageResamplerConfig;
    property TileReprojectResamplerConfig: IImageResamplerConfig read GetTileReprojectResamplerConfig;

    function GetTileDownloadResamplerConfig: IImageResamplerConfig;
    property TileDownloadResamplerConfig: IImageResamplerConfig read GetTileDownloadResamplerConfig;

    function GetBitmapPostProcessingConfig: IBitmapPostProcessingConfig;
    property BitmapPostProcessingConfig: IBitmapPostProcessingConfig read GetBitmapPostProcessingConfig;

    function GetCoordRepresentationConfig: ICoordRepresentationConfig;
    property CoordRepresentationConfig: ICoordRepresentationConfig read GetCoordRepresentationConfig;

    function GetValueToStringConverterConfig: IValueToStringConverterConfig;
    property ValueToStringConverterConfig: IValueToStringConverterConfig read GetValueToStringConverterConfig;

    function GetImageResamplerConfig: IImageResamplerConfig;
    property ImageResamplerConfig: IImageResamplerConfig read GetImageResamplerConfig;

    function GetTileMatrixDraftResamplerConfig: IImageResamplerConfig;
    property TileMatrixDraftResamplerConfig: IImageResamplerConfig read GetTileMatrixDraftResamplerConfig;

    function GetMainMemCacheConfig: IMainMemCacheConfig;
    property MainMemCacheConfig: IMainMemCacheConfig read GetMainMemCacheConfig;

    function GetGPSConfig: IGpsConfig;
    property GPSConfig: IGPSConfig read GetGpsConfig;

    function GetMarksFactoryConfig: IMarkFactoryConfig;
    property MarksFactoryConfig: IMarkFactoryConfig read GetMarksFactoryConfig;

    function GetMarksGUIConfig: IMarksGUIConfig;
    property MarksGUIConfig: IMarksGUIConfig read GetMarksGUIConfig;

    function GetMarksCategoryFactoryConfig: IMarkCategoryFactoryConfig;
    property MarksCategoryFactoryConfig: IMarkCategoryFactoryConfig read GetMarksCategoryFactoryConfig;

    function GetViewConfig: IGlobalViewMainConfig;
    property ViewConfig: IGlobalViewMainConfig read GetViewConfig;

    function GetDownloadConfig: IGlobalDownloadConfig;
    property DownloadConfig: IGlobalDownloadConfig read GetDownloadConfig;

    function GetDownloaderThreadConfig: IThreadConfig;
    property DownloaderThreadConfig: IThreadConfig read GetDownloaderThreadConfig;

    function GetStartUpLogoConfig: IStartUpLogoConfig;
    property StartUpLogoConfig: IStartUpLogoConfig read GetStartUpLogoConfig;

    function GetTerrainConfig: ITerrainConfig;
    property TerrainConfig: ITerrainConfig read GetTerrainConfig;

    function GetZmpConfig: IZmpConfig;
    property ZmpConfig: IZmpConfig read GetZmpConfig;

    function GetMapSvcScanConfig: IMapSvcScanConfig;
    property MapSvcScanConfig: IMapSvcScanConfig read GetMapSvcScanConfig;
  end;

implementation

end.

unit i_GlobalConfig;

interface

uses
  i_PathConfig,
  i_GlobalAppConfig,
  i_LastSelectionInfo,
  i_LanguageManager,
  i_GSMGeoCodeConfig,
  i_InetConfig,
  i_BitmapPostProcessingConfig,
  i_ValueToStringConverter,
  i_ImageResamplerConfig,
  i_MainMemCacheConfig,
  i_GPSConfig,
  i_GlobalViewMainConfig,
  i_MarksFactoryConfig,
  i_MarkCategoryFactoryConfig,
  i_GlobalDownloadConfig,
  i_ThreadConfig,
  i_StartUpLogoConfig,
  i_WindowPositionConfig,
  i_TerrainConfig,
  i_ZmpConfig,
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

    function GetLastSelectionFileName: IPathConfig;
    property LastSelectionFileName: IPathConfig read GetLastSelectionFileName;

    function GetGpsRecorderFileName: IPathConfig;
    property GpsRecorderFileName: IPathConfig read GetGpsRecorderFileName;

    function GetGpsTrackRecorderFileName: IPathConfig;
    property GpsTrackRecorderFileName: IPathConfig read GetGpsTrackRecorderFileName;

    function GetGlobalAppConfig: IGlobalAppConfig;
    property GlobalAppConfig: IGlobalAppConfig read GetGlobalAppConfig;

    function GetLastSelectionInfo: ILastSelectionInfo;
    property LastSelectionInfo: ILastSelectionInfo read GetLastSelectionInfo;

    function GetLanguageManager: ILanguageManager;
    property LanguageManager: ILanguageManager read GetLanguageManager;

(*
    function GetGsmConfig: IGSMGeoCodeConfig;
    property GsmConfig: IGSMGeoCodeConfig read GetGsmConfig;

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

    function GetMarksFactoryConfig: IMarksFactoryConfig;
    property MarksFactoryConfig: IMarksFactoryConfig read GetMarksFactoryConfig;

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
*)
  end;

implementation

end.

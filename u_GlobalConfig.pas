unit u_GlobalConfig;

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
  i_MarksFactoryConfig,
  i_MarkCategoryFactoryConfig,
  i_GPSConfig,
  i_GlobalViewMainConfig,
  i_GlobalDownloadConfig,
  i_ThreadConfig,
  i_StartUpLogoConfig,
  i_TerrainConfig,
  i_ZmpConfig,
  i_WindowPositionConfig,
  i_GlobalConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementComplexBase;

type
  TGlobalConfig = class(TConfigDataElementComplexBase, IGlobalConfig)
  private
    FBaseCahcePath: IPathConfig;
    FMapsPath: IPathConfig;
    FTrackPath: IPathConfig;
    FMarksDbPath: IPathConfig;
    FMarksIconsPath: IPathConfig;
    FMediaDataPath: IPathConfig;
    FTerrainDataPath: IPathConfig;
    FLastSelectionFileName: IPathConfig;
    FGpsRecorderFileName: IPathConfig;
    FGpsTrackRecorderFileName: IPathConfig;

    FGlobalAppConfig: IGlobalAppConfig;

    FLastSelectionInfo: ILastSelectionInfo;
    FLanguageManager: ILanguageManager;
    FGsmConfig: IGSMGeoCodeConfig;
    FInetConfig: IInetConfig;
    FInternalBrowserConfig: IWindowPositionConfig;
    FMainThreadConfig: IThreadConfig;
    FTileLoadResamplerConfig: IImageResamplerConfig;
    FTileGetPrevResamplerConfig: IImageResamplerConfig;
    FTileReprojectResamplerConfig: IImageResamplerConfig;
    FTileDownloadResamplerConfig: IImageResamplerConfig;
    FBitmapPostProcessingConfig: IBitmapPostProcessingConfig;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FImageResamplerConfig: IImageResamplerConfig;
    FTileMatrixDraftResamplerConfig: IImageResamplerConfig;
    FMainMemCacheConfig: IMainMemCacheConfig;
    FGPSConfig: IGpsConfig;
    FMarksFactoryConfig: IMarksFactoryConfig;
    FMarksCategoryFactoryConfig: IMarkCategoryFactoryConfig;
    FViewConfig: IGlobalViewMainConfig;
    FDownloadConfig: IGlobalDownloadConfig;
    FDownloaderThreadConfig: IThreadConfig;
    FStartUpLogoConfig: IStartUpLogoConfig;
    FTerrainConfig: ITerrainConfig;
    FZmpConfig: IZmpConfig;
  private
    function GetBaseCahcePath: IPathConfig;
    function GetMapsPath: IPathConfig;
    function GetTrackPath: IPathConfig;
    function GetMarksDbPath: IPathConfig;
    function GetMarksIconsPath: IPathConfig;
    function GetMediaDataPath: IPathConfig;
    function GetTerrainDataPath: IPathConfig;
    function GetLastSelectionFileName: IPathConfig;
    function GetGpsRecorderFileName: IPathConfig;
    function GetGpsTrackRecorderFileName: IPathConfig;
    function GetGlobalAppConfig: IGlobalAppConfig;
    function GetLastSelectionInfo: ILastSelectionInfo;
    function GetLanguageManager: ILanguageManager;
    function GetGsmConfig: IGSMGeoCodeConfig;
    function GetInetConfig: IInetConfig;
    function GetInternalBrowserConfig: IWindowPositionConfig;
    function GetMainThreadConfig: IThreadConfig;
    function GetTileLoadResamplerConfig: IImageResamplerConfig;
    function GetTileGetPrevResamplerConfig: IImageResamplerConfig;
    function GetTileReprojectResamplerConfig: IImageResamplerConfig;
    function GetTileDownloadResamplerConfig: IImageResamplerConfig;
    function GetBitmapPostProcessingConfig: IBitmapPostProcessingConfig;
    function GetValueToStringConverterConfig: IValueToStringConverterConfig;
    function GetImageResamplerConfig: IImageResamplerConfig;
    function GetTileMatrixDraftResamplerConfig: IImageResamplerConfig;
    function GetMainMemCacheConfig: IMainMemCacheConfig;
    function GetGPSConfig: IGpsConfig;
    function GetMarksFactoryConfig: IMarksFactoryConfig;
    function GetMarksCategoryFactoryConfig: IMarkCategoryFactoryConfig;
    function GetViewConfig: IGlobalViewMainConfig;
    function GetDownloadConfig: IGlobalDownloadConfig;
    function GetDownloaderThreadConfig: IThreadConfig;
    function GetStartUpLogoConfig: IStartUpLogoConfig;
    function GetTerrainConfig: ITerrainConfig;
    function GetZmpConfig: IZmpConfig;
  public
    constructor Create(
      const ABaseCacheDataPath: IPathConfig;
      const ABaseConfigPath: IPathConfig;
      const ABaseDataPath: IPathConfig;
      const ABaseApplicationPath: IPathConfig
    );
  end;

implementation

uses
  SysUtils,
  Classes,
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_GlobalAppConfig,
  u_LastSelectionInfo,
  u_LanguageManager,
  u_GSMGeoCodeConfig,
  u_InetConfig,
  u_WindowPositionConfig,
  u_ThreadConfig,
  u_PathConfig;

{ TGlobalConfig }

constructor TGlobalConfig.Create(
  const ABaseCacheDataPath: IPathConfig;
  const ABaseConfigPath: IPathConfig;
  const ABaseDataPath: IPathConfig;
  const ABaseApplicationPath: IPathConfig
);
begin
  inherited Create;
  FBaseCahcePath := TPathConfig.Create('PrimaryPath', '.', ABaseCacheDataPath);
  Add(FBaseCahcePath, TConfigSaveLoadStrategyBasicProviderSubItem.Create('PATHtoCACHE'), False, False, False, False);

  FMapsPath := TPathConfig.Create('PrimaryPath', '.\Maps', ABaseConfigPath);
  Add(FMapsPath, TConfigSaveLoadStrategyBasicProviderSubItem.Create('PATHtoMAPS'), False, False, False, False);

  FTrackPath := TPathConfig.Create('PrimaryPath', '.\TrackLog', ABaseDataPath);
  Add(FTrackPath, TConfigSaveLoadStrategyBasicProviderSubItem.Create('PATHtoTRACKS'), False, False, False, False);

  FMarksDbPath := TPathConfig.Create('PrimaryPath', '.', ABaseDataPath);
  Add(FMarksDbPath, TConfigSaveLoadStrategyBasicProviderSubItem.Create('PATHtoMARKS'), False, False, False, False);

  FMarksIconsPath := TPathConfig.Create('PrimaryPath', '.\MarksIcons', ABaseApplicationPath);
  Add(FMarksIconsPath, TConfigSaveLoadStrategyBasicProviderSubItem.Create('PathToMarksIcons'), False, False, False, False);

  FMediaDataPath := TPathConfig.Create('PrimaryPath', '.\MediaData', ABaseDataPath);
  Add(FMediaDataPath, TConfigSaveLoadStrategyBasicProviderSubItem.Create('PATHtoMediaData'), False, False, False, False);

  FTerrainDataPath := TPathConfig.Create('PrimaryPath', '.\TerrainData', ABaseDataPath);
  Add(FTerrainDataPath, TConfigSaveLoadStrategyBasicProviderSubItem.Create('PATHtoTerrainData'), False, False, False, False);

  FLastSelectionFileName := TPathConfig.Create('FileName', '.\LastSelection.hlg', ABaseDataPath);
  Add(FLastSelectionFileName, TConfigSaveLoadStrategyBasicProviderSubItem.Create('LastSelection'), False, False, False, False);

  FGpsRecorderFileName := TPathConfig.Create('InfoFileName', '.\GpsInfo.ini', ABaseDataPath);
  Add(FGpsRecorderFileName, TConfigSaveLoadStrategyBasicProviderSubItem.Create('GpsData'), False, False, False, False);

  FGpsTrackRecorderFileName := TPathConfig.Create('TrackFileName', '.\LastPoints.dat', ABaseDataPath);
  Add(FGpsTrackRecorderFileName, TConfigSaveLoadStrategyBasicProviderSubItem.Create('GpsData'), False, False, False, False);

  FGlobalAppConfig := TGlobalAppConfig.Create;
  Add(FGlobalAppConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('VIEW'), False, False, False, False);

  FLastSelectionInfo := TLastSelectionInfo.Create;

  FLanguageManager := TLanguageManager.Create(IncludeTrailingPathDelimiter(ABaseApplicationPath.FullPath) + 'lang');
  Add(FLanguageManager, TConfigSaveLoadStrategyBasicProviderSubItem.Create('View'), False, False, False, False);

  FGsmConfig := TGSMGeoCodeConfig.Create;
  Add(FGsmConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('GSM'), False, False, False, False);

  FInetConfig := TInetConfig.Create;
  Add(FInetConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Internet'), False, False, False, False);

  FInternalBrowserConfig := TWindowPositionConfig.Create;
  Add(FInternalBrowserConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('InternalBrowser'), False, False, False, False);

  FMainThreadConfig := TThreadConfig.Create(tpHigher);
  Add(FMainThreadConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('View'), False, False, False, False);
end;

function TGlobalConfig.GetBaseCahcePath: IPathConfig;
begin
  Result := FBaseCahcePath;
end;

function TGlobalConfig.GetBitmapPostProcessingConfig: IBitmapPostProcessingConfig;
begin
  Result := FBitmapPostProcessingConfig;
end;

function TGlobalConfig.GetDownloadConfig: IGlobalDownloadConfig;
begin
  Result := FDownloadConfig;
end;

function TGlobalConfig.GetDownloaderThreadConfig: IThreadConfig;
begin
  Result := FDownloaderThreadConfig;
end;

function TGlobalConfig.GetGlobalAppConfig: IGlobalAppConfig;
begin
  Result := FGlobalAppConfig;
end;

function TGlobalConfig.GetGPSConfig: IGpsConfig;
begin
  Result := FGPSConfig;
end;

function TGlobalConfig.GetGpsRecorderFileName: IPathConfig;
begin
  Result := FGpsRecorderFileName;
end;

function TGlobalConfig.GetGpsTrackRecorderFileName: IPathConfig;
begin
  Result := FGpsTrackRecorderFileName;
end;

function TGlobalConfig.GetGsmConfig: IGSMGeoCodeConfig;
begin
  Result := FGsmConfig;
end;

function TGlobalConfig.GetImageResamplerConfig: IImageResamplerConfig;
begin
  Result := FImageResamplerConfig;
end;

function TGlobalConfig.GetInetConfig: IInetConfig;
begin
  Result := FInetConfig;
end;

function TGlobalConfig.GetInternalBrowserConfig: IWindowPositionConfig;
begin
  Result := FInternalBrowserConfig;
end;

function TGlobalConfig.GetLanguageManager: ILanguageManager;
begin
  Result := FLanguageManager;
end;

function TGlobalConfig.GetLastSelectionFileName: IPathConfig;
begin
  Result := FLastSelectionFileName;
end;

function TGlobalConfig.GetLastSelectionInfo: ILastSelectionInfo;
begin
  Result := FLastSelectionInfo;
end;

function TGlobalConfig.GetMainMemCacheConfig: IMainMemCacheConfig;
begin
  Result := FMainMemCacheConfig;
end;

function TGlobalConfig.GetMainThreadConfig: IThreadConfig;
begin
  Result := FMainThreadConfig;
end;

function TGlobalConfig.GetMapsPath: IPathConfig;
begin
  Result := FMapsPath;
end;

function TGlobalConfig.GetMarksCategoryFactoryConfig: IMarkCategoryFactoryConfig;
begin
  Result := FMarksCategoryFactoryConfig;
end;

function TGlobalConfig.GetMarksDbPath: IPathConfig;
begin
  Result := FMarksDbPath;
end;

function TGlobalConfig.GetMarksFactoryConfig: IMarksFactoryConfig;
begin
  Result := FMarksFactoryConfig;
end;

function TGlobalConfig.GetMarksIconsPath: IPathConfig;
begin
  Result := FMarksIconsPath;
end;

function TGlobalConfig.GetMediaDataPath: IPathConfig;
begin
  Result := FMediaDataPath;
end;

function TGlobalConfig.GetStartUpLogoConfig: IStartUpLogoConfig;
begin
  Result := FStartUpLogoConfig;
end;

function TGlobalConfig.GetTerrainConfig: ITerrainConfig;
begin
  Result := FTerrainConfig;
end;

function TGlobalConfig.GetTerrainDataPath: IPathConfig;
begin
  Result := FTerrainDataPath;
end;

function TGlobalConfig.GetTileDownloadResamplerConfig: IImageResamplerConfig;
begin
  Result := FTileDownloadResamplerConfig;
end;

function TGlobalConfig.GetTileGetPrevResamplerConfig: IImageResamplerConfig;
begin
  Result := FTileGetPrevResamplerConfig;
end;

function TGlobalConfig.GetTileLoadResamplerConfig: IImageResamplerConfig;
begin
  Result := FTileLoadResamplerConfig;
end;

function TGlobalConfig.GetTileMatrixDraftResamplerConfig: IImageResamplerConfig;
begin
  Result := FTileMatrixDraftResamplerConfig;
end;

function TGlobalConfig.GetTileReprojectResamplerConfig: IImageResamplerConfig;
begin
  Result := FTileReprojectResamplerConfig;
end;

function TGlobalConfig.GetTrackPath: IPathConfig;
begin
  Result := FTrackPath;
end;

function TGlobalConfig.GetValueToStringConverterConfig: IValueToStringConverterConfig;
begin
  Result := FValueToStringConverterConfig;
end;

function TGlobalConfig.GetViewConfig: IGlobalViewMainConfig;
begin
  Result := FViewConfig;
end;

function TGlobalConfig.GetZmpConfig: IZmpConfig;
begin
  Result := FZmpConfig;
end;

end.

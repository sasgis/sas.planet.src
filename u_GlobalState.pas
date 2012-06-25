{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_GlobalState;

interface

uses
  {$IFDEF SasDebugWithJcl}
  Windows,
  JclDebug,
  {$ENDIF SasDebugWithJcl}
  ExtCtrls,
  Classes,
  IniFiles,
  SysUtils,
  i_JclNotify,
  i_GPSPositionFactory,
  i_LanguageManager,
  i_InetConfig,
  i_ConfigDataWriteProvider,
  i_ConfigDataProvider,
  i_TileFileNameGeneratorsList,
  i_TileFileNameParsersList,
  i_ContentTypeManager,
  i_VectorDataLoader,
  i_CoordConverterFactory,
  i_BatteryStatus,
  i_LocalCoordConverterFactorySimpe,
  i_ProxySettings,
  i_GSMGeoCodeConfig,
  i_MainFormConfig,
  i_WindowPositionConfig,
  i_GlobalAppConfig,
  i_BitmapPostProcessingConfig,
  i_ValueToStringConverter,
  u_GarbageCollectorThread,
  i_LastSelectionInfo,
  i_DownloadInfoSimple,
  i_ImageResamplerConfig,
  i_GeoCoderList,
  i_MainMemCacheConfig,
  i_MarkPicture,
  i_InternalPerformanceCounter,
  i_LayerBitmapClearStrategy,
  u_LastSelectionInfo,
  u_MarksSystem,
  u_MapTypesMainList,
  i_ThreadConfig,
  i_ZmpConfig,
  i_ZmpInfoSet,
  i_GPSConfig,
  i_PathConfig,
  i_MapCalibration,
  i_MarkCategoryFactoryConfig,
  i_GlobalViewMainConfig,
  i_GlobalDownloadConfig,
  i_StartUpLogoConfig,
  i_DownloadResultTextProvider,
  i_ImportFile,
  i_PathDetalizeProviderList,
  i_GPSRecorder,
  i_SatellitesInViewMapDraw,
  i_SensorList,
  i_TimeZoneDiffByLonLat,
  i_VectorItmesFactory,
  i_InvisibleBrowser,
  i_InternalBrowser,
  i_DebugInfoWindow,
  i_GlobalInternetState,
  u_IeEmbeddedProtocolRegistration,
  u_GPSState,
  u_GlobalCahceConfig;

type
  TGlobalState = class
  private
    FBaseConfigPath: IPathConfig;
    FBaseDataPath: IPathConfig;
    FBaseCahcePath: IPathConfig;
    FBaseApplicationPath: IPathConfig;
    FMapsPath: IPathConfig;
    FTrackPath: IPathConfig;
    FMarksDbPath: IPathConfig;
    FMarksIconsPath: IPathConfig;
    FMediaDataPath: IPathConfig;

    FMainConfigProvider: IConfigDataWriteProvider;
    FZmpConfig: IZmpConfig;
    FZmpInfoSet: IZmpInfoSet;
    FResourceProvider: IConfigDataProvider;
    FGlobalAppConfig: IGlobalAppConfig;
    FStartUpLogoConfig: IStartUpLogoConfig;
    FTileNameGenerator: ITileFileNameGeneratorsList;
    FTileNameParser: ITileFileNameParsersList;
    FGCThread: TGarbageCollectorThread;
    FContentTypeManager: IContentTypeManager;
    FMapCalibrationList: IMapCalibrationList;
    FCacheConfig: TGlobalCahceConfig;
    FLanguageManager: ILanguageManager;
    FLastSelectionInfo: ILastSelectionInfo;
    FMarksDB: TMarksSystem;
    FCoordConverterFactory: ICoordConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FMainMapsList: TMapTypesMainList;
    FInetConfig: IInetConfig;
    FGPSConfig: IGPSConfig;
    FGSMpar: IGSMGeoCodeConfig;
    FGPSPositionFactory: IGPSPositionFactory;
    FMainFormConfig: IMainFormConfig;
    FBitmapPostProcessingConfig: IBitmapPostProcessingConfig;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FDownloadInfo: IDownloadInfoSimple;
    FDownloadConfig: IGlobalDownloadConfig;
    FDownloaderThreadConfig: IThreadConfig;
    FGlobalInternetState: IGlobalInternetState;
    FImageResamplerConfig: IImageResamplerConfig;
    FGeoCoderList: IGeoCoderList;
    FMainMemCacheConfig: IMainMemCacheConfig;
    FMarkPictureList: IMarkPictureList;
    FMarksCategoryFactoryConfig: IMarkCategoryFactoryConfig;
    FGPSpar: TGPSpar;
    FImportFileByExt: IImportFile;
    FViewConfig: IGlobalViewMainConfig;
    FGPSRecorder: IGPSRecorder;
    FSkyMapDraw: ISatellitesInViewMapDraw;
    FGUISyncronizedTimer: TTimer;
    FGUISyncronizedTimerNotifier: IJclNotifier;
    FSensorList: ISensorList;
    FPerfCounterList: IInternalPerformanceCounterList;
    FDownloadResultTextProvider: IDownloadResultTextProvider;
    FProtocol: TIeEmbeddedProtocolRegistration;
    FPathDetalizeList: IPathDetalizeProviderList;
    FClearStrategyFactory: ILayerBitmapClearStrategyFactory;
    FInvisibleBrowser: IInvisibleBrowser;
    FInternalBrowserConfig: IWindowPositionConfig;
    FInternalBrowser: IInternalBrowser;
    FDebugInfoWindow: IDebugInfoWindow;
    FAppClosingNotifier: IJclNotifier;
    FTimeZoneDiffByLonLat: ITimeZoneDiffByLonLat;
    FVectorItmesFactory: IVectorItmesFactory;
    FBatteryStatus: IBatteryStatus;

    procedure OnGUISyncronizedTimer(Sender: TObject);
    {$IFDEF SasDebugWithJcl}
    procedure DoException(
      Sender: TObject;
      E: Exception
    );
    {$ENDIF SasDebugWithJcl}
  public
    property MapType: TMapTypesMainList read FMainMapsList;
    property CacheConfig: TGlobalCahceConfig read FCacheConfig;
    property GCThread: TGarbageCollectorThread read FGCThread;
    property MarksDB: TMarksSystem read FMarksDB;
    property GPSpar: TGPSpar read FGPSpar;

    // Список генераторов имен файлов с тайлами
    property TileNameGenerator: ITileFileNameGeneratorsList read FTileNameGenerator;
    property TileNameParser: ITileFileNameParsersList read FTileNameParser;
    property ContentTypeManager: IContentTypeManager read FContentTypeManager;
    property CoordConverterFactory: ICoordConverterFactory read FCoordConverterFactory;
    property ProjectionFactory: IProjectionInfoFactory read FProjectionFactory;
    property LocalConverterFactory: ILocalCoordConverterFactorySimpe read FLocalConverterFactory;
    property MapCalibrationList: IMapCalibrationList read FMapCalibrationList;
    property AppClosingNotifier: IJclNotifier read FAppClosingNotifier;
    property MediaDataPath: IPathConfig read FMediaDataPath;

    property MainConfigProvider: IConfigDataWriteProvider read FMainConfigProvider;
    property ResourceProvider: IConfigDataProvider read FResourceProvider;
    property DownloadInfo: IDownloadInfoSimple read FDownloadInfo;
    property GlobalInternetState: IGlobalInternetState read FGlobalInternetState;
    property ImportFileByExt: IImportFile read FImportFileByExt;
    property DownloadResultTextProvider: IDownloadResultTextProvider read FDownloadResultTextProvider;
    property SkyMapDraw: ISatellitesInViewMapDraw read FSkyMapDraw;
    property GUISyncronizedTimerNotifier: IJclNotifier read FGUISyncronizedTimerNotifier;
    property PerfCounterList: IInternalPerformanceCounterList read FPerfCounterList;

    property GlobalAppConfig: IGlobalAppConfig read FGlobalAppConfig;
    property LastSelectionInfo: ILastSelectionInfo read FLastSelectionInfo;
    property LanguageManager: ILanguageManager read FLanguageManager;
    property GSMpar: IGSMGeoCodeConfig read FGSMpar;
    property InetConfig: IInetConfig read FInetConfig;
    property MainFormConfig: IMainFormConfig read FMainFormConfig;
    property BitmapPostProcessingConfig: IBitmapPostProcessingConfig read FBitmapPostProcessingConfig;
    property ValueToStringConverterConfig: IValueToStringConverterConfig read FValueToStringConverterConfig;
    property ImageResamplerConfig: IImageResamplerConfig read FImageResamplerConfig;
    property MainMemCacheConfig: IMainMemCacheConfig read FMainMemCacheConfig;
    property GPSConfig: IGPSConfig read FGPSConfig;
    property MarksCategoryFactoryConfig: IMarkCategoryFactoryConfig read FMarksCategoryFactoryConfig;
    property ViewConfig: IGlobalViewMainConfig read FViewConfig;
    property GPSRecorder: IGPSRecorder read FGPSRecorder;
    property PathDetalizeList: IPathDetalizeProviderList read FPathDetalizeList;
    property SensorList: ISensorList read FSensorList;
    property DownloadConfig: IGlobalDownloadConfig read FDownloadConfig;
    property DownloaderThreadConfig: IThreadConfig read FDownloaderThreadConfig;
    property StartUpLogoConfig: IStartUpLogoConfig read FStartUpLogoConfig;
    property ClearStrategyFactory: ILayerBitmapClearStrategyFactory read FClearStrategyFactory;
    property InternalBrowser: IInternalBrowser read FInternalBrowser;
    property DebugInfoWindow: IDebugInfoWindow read FDebugInfoWindow;
    property TimeZoneDiffByLonLat: ITimeZoneDiffByLonLat read FTimeZoneDiffByLonLat;
    property VectorItmesFactory: IVectorItmesFactory read FVectorItmesFactory;

    constructor Create;
    destructor Destroy; override;
    procedure LoadConfig;
    procedure SaveMainParams;
    procedure StartThreads;
    procedure SendTerminateToThreads;

    procedure StartExceptionTracking;
    procedure StopExceptionTracking;
  end;

var
  GState: TGlobalState;

implementation

uses
  {$IFDEF SasDebugWithJcl}
  Forms,
  {$ENDIF}
  u_JclNotify,
  c_InternalBrowser,
  u_SASMainConfigProvider,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile,
  u_ConfigDataProviderByPathConfig,
  i_InternalDomainInfoProvider,
  i_ProjConverter,
  i_TTLCheckNotifier,
  u_TTLCheckNotifier,
  i_FileNameIterator,
  u_ContentTypeManagerSimple,
  u_MapCalibrationListBasic,
  u_XmlInfoSimpleParser,
  u_KmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_CoordConverterFactorySimple,
  u_LanguageManager,
  u_DownloadInfoSimple,
  u_StartUpLogoConfig,
  u_InetConfig,
  u_Datum,
  u_PLTSimpleParser,
  u_GSMGeoCodeConfig,
  u_GPSConfig,
  u_MarkCategoryFactoryConfig,
  u_GeoCoderListSimple,
  u_BitmapPostProcessingConfig,
  u_ValueToStringConverterConfig,
  u_GlobalAppConfig,
  u_MainMemCacheConfig,
  u_MarkPictureListSimple,
  u_ImageResamplerConfig,
  u_ImageResamplerFactoryListStaticSimple,
  u_ImportByFileExt,
  u_GlobalViewMainConfig,
  u_GlobalDownloadConfig,
  u_GPSRecorder,
  u_SatellitesInViewMapDrawSimple,
  u_GPSModuleFactoryByVSAGPS,
  u_GPSPositionFactory,
  u_LocalCoordConverterFactorySimpe,
  u_LayerBitmapClearStrategyFactory,
  u_DownloadResultTextProvider,
  u_TimeZoneDiffByLonLatStuped,
  u_MainFormConfig,
  u_ProjConverterFactory,
  u_PathConfig,
  u_ThreadConfig,
  u_BatteryStatus,
  u_ZmpConfig,
  u_ZmpInfoSet,
  u_ZmpFileNamesIteratorFactory,
  u_SensorListStuped,
  u_WindowPositionConfig,
  u_HtmlToHintTextConverterStuped,
  u_InvisibleBrowserByFormSynchronize,
  u_InternalBrowserByForm,
  u_DebugInfoWindow,
  u_InternalPerformanceCounterList,
  u_IeEmbeddedProtocolFactory,
  u_VectorItmesFactorySimple,
  u_VectorDataFactorySimple,
  u_DownloadResultFactory,
  u_PathDetalizeProviderListSimple,
  u_InternalDomainInfoProviderList,
  u_InternalDomainInfoProviderByMapTypeList,
  u_InternalDomainInfoProviderByDataProvider,
  u_GlobalInternetState,
  u_TileFileNameParsersSimpleList,
  u_TileFileNameGeneratorsSimpleList;

{ TGlobalState }

constructor TGlobalState.Create;
var
  VList: ITTLCheckNotifier;
  VViewCnonfig: IConfigDataProvider;
  VInternalDomainInfoProviderList: TInternalDomainInfoProviderList;
  VMarksKmlLoadCounterList: IInternalPerformanceCounterList;
  VXmlLoader: IVectorDataLoader;
  VKmlLoader: IVectorDataLoader;
  VKmzLoader: IVectorDataLoader;
  VFilesIteratorFactory: IFileNameIteratorFactory;
  VFilesIterator: IFileNameIterator;
  VCoordConverterFactorySimple: TCoordConverterFactorySimple;
  VProgramPath: string;
  VSleepByClass: IConfigDataProvider;
  VInternalDomainInfoProvider: IInternalDomainInfoProvider;
begin
  inherited Create;
  if ModuleIsLib then begin
    // run as DLL or PACKAGE
    VProgramPath := GetModuleName(HInstance);
    VProgramPath := ExtractFilePath(VProgramPath);
  end else begin
    // run as EXE
    VProgramPath := ExtractFilePath(ParamStr(0));
  end;
  FBaseApplicationPath := TPathConfig.Create('', VProgramPath, nil);
  FBaseConfigPath := TPathConfig.Create('', VProgramPath, nil);
  FBaseDataPath := TPathConfig.Create('', VProgramPath, nil);
  FBaseCahcePath := TPathConfig.Create('PrimaryPath', VProgramPath, nil);
  FMapsPath := TPathConfig.Create('PrimaryPath', '.\Maps', FBaseConfigPath);
  FTrackPath := TPathConfig.Create('PrimaryPath', '.\TrackLog', FBaseDataPath);
  FMarksDbPath := TPathConfig.Create('PrimaryPath', '.', FBaseDataPath);
  FMarksIconsPath := TPathConfig.Create('', '.\MarksIcons', FBaseApplicationPath);
  FMediaDataPath := TPathConfig.Create('PrimaryPath', '.\MediaData', FBaseDataPath);

  FAppClosingNotifier := TJclBaseNotifier.Create;
  FMainConfigProvider :=
    TSASMainConfigProvider.Create(
      FBaseConfigPath.FullPath,
      ExtractFileName(ParamStr(0)),
      HInstance
    );

  // read directories
  FMapsPath.ReadConfig(FMainConfigProvider.GetSubItem('PATHtoMAPS'));
  FTrackPath.ReadConfig(FMainConfigProvider.GetSubItem('PATHtoTRACKS'));
  FMarksDbPath.ReadConfig(FMainConfigProvider.GetSubItem('PATHtoMARKS'));
  FMediaDataPath.ReadConfig(FMainConfigProvider.GetSubItem('PATHtoMediaData'));

  VSleepByClass := FMainConfigProvider.GetSubItem('SleepByClass');

  FResourceProvider := FMainConfigProvider.GetSubItem('sas:\Resource');
  FVectorItmesFactory := TVectorItmesFactorySimple.Create;
  FGUISyncronizedTimer := TTimer.Create(nil);
  FGUISyncronizedTimer.Enabled := False;
  FGUISyncronizedTimer.Interval := VSleepByClass.ReadInteger('GUISyncronizedTimer', 500);
  FGUISyncronizedTimer.OnTimer := Self.OnGUISyncronizedTimer;

  FGUISyncronizedTimerNotifier := TJclBaseNotifier.Create;

  FGlobalAppConfig := TGlobalAppConfig.Create;
  FGlobalInternetState := TGlobalInternetState.Create;

  VCoordConverterFactorySimple := TCoordConverterFactorySimple.Create;
  FCoordConverterFactory := VCoordConverterFactorySimple;
  FProjectionFactory := VCoordConverterFactorySimple;
  FLocalConverterFactory := TLocalCoordConverterFactorySimpe.Create(FProjectionFactory);

  FTimeZoneDiffByLonLat := TTimeZoneDiffByLonLatStuped.Create(FVectorItmesFactory);

  FCacheConfig := TGlobalCahceConfig.Create(FBaseCahcePath);
  FDownloadInfo := TDownloadInfoSimple.Create(nil);
  VViewCnonfig := FMainConfigProvider.GetSubItem('VIEW');
  FLanguageManager := TLanguageManager.Create;
  FLanguageManager.ReadConfig(VViewCnonfig);
  if VViewCnonfig <> nil then begin
    FGlobalAppConfig.ReadConfig(VViewCnonfig);
  end;

  if FGlobalAppConfig.IsShowDebugInfo then begin
    FPerfCounterList := TInternalPerformanceCounterList.Create('Main');
  end else begin
    FPerfCounterList := TInternalPerformanceCounterFake.Create;
  end;

  FDownloadConfig := TGlobalDownloadConfig.Create;
  FDownloaderThreadConfig := TThreadConfig.Create(tpLower);
  FImageResamplerConfig :=
    TImageResamplerConfig.Create(
      TImageResamplerFactoryListStaticSimple.Create
    );

  FClearStrategyFactory := TLayerBitmapClearStrategyFactory.Create(FImageResamplerConfig, FPerfCounterList.CreateAndAddNewSubList('ClearStrategy'));

  FInetConfig := TInetConfig.Create;
  FGPSConfig := TGPSConfig.Create(FTrackPath);
  FGPSPositionFactory := TGPSPositionFactory.Create;
  FGPSRecorder :=
    TGPSRecorder.Create(
      FVectorItmesFactory,
      TDatum.Create(3395, 6378137, 6356752),
      FGPSPositionFactory
    );
  FGSMpar := TGSMGeoCodeConfig.Create;
  FMainMemCacheConfig := TMainMemCacheConfig.Create;
  FViewConfig := TGlobalViewMainConfig.Create;

  FTileNameGenerator := TTileFileNameGeneratorsSimpleList.Create(FCacheConfig);
  FTileNameParser := TTileFileNameParsersSimpleList.Create(FCacheConfig);

  FContentTypeManager :=
    TContentTypeManagerSimple.Create(
      FVectorItmesFactory,
      FPerfCounterList
    );

  if (not ModuleIsLib) then begin
    FStartUpLogoConfig := TStartUpLogoConfig.Create(FContentTypeManager);
    FStartUpLogoConfig.ReadConfig(FMainConfigProvider.GetSubItem('StartUpLogo'));
  end;

  FInternalBrowserConfig := TWindowPositionConfig.Create;
  
  FMapCalibrationList := TMapCalibrationListBasic.Create;
  VMarksKmlLoadCounterList := FPerfCounterList.CreateAndAddNewSubList('Import');

  // xml loaders
  VXmlLoader :=
    TXmlInfoSimpleParser.Create(
      FVectorItmesFactory,
      VMarksKmlLoadCounterList
    );
  VKmlLoader :=
    TKmlInfoSimpleParser.Create(
      FVectorItmesFactory,
      VMarksKmlLoadCounterList
    );
  VKmzLoader :=
    TKmzInfoSimpleParser.Create(
      FVectorItmesFactory,
      VMarksKmlLoadCounterList
    );

  FImportFileByExt := TImportByFileExt.Create(
    TVectorDataFactorySimple.Create(THtmlToHintTextConverterStuped.Create),
    FVectorItmesFactory,
    VXmlLoader,
    TPLTSimpleParser.Create(
      FVectorItmesFactory,
      VMarksKmlLoadCounterList
    ),
    VKmlLoader,
    VKmzLoader
  );
  VList := TTTLCheckNotifier.Create;
  FGCThread := TGarbageCollectorThread.Create(VList, VSleepByClass.ReadInteger(TGarbageCollectorThread.ClassName, 1000));
  FBitmapPostProcessingConfig := TBitmapPostProcessingConfig.Create;
  FValueToStringConverterConfig := TValueToStringConverterConfig.Create(FLanguageManager);
  FGPSpar :=
    TGPSpar.Create(
      TGPSModuleFactoryByVSAGPS.Create(FGPSPositionFactory),
      FGPSConfig,
      FGPSRecorder,
      GUISyncronizedTimerNotifier,
      FPerfCounterList
    );
  FLastSelectionInfo := TLastSelectionInfo.Create(FVectorItmesFactory);
  FDownloadResultTextProvider := TDownloadResultTextProvider.Create(FLanguageManager);
  FGeoCoderList :=
    TGeoCoderListSimple.Create(
      FInetConfig,
      FGCThread.List,
      TDownloadResultFactory.Create(FDownloadResultTextProvider),
      FValueToStringConverterConfig
    );
  FMarkPictureList := TMarkPictureListSimple.Create(FMarksIconsPath, FContentTypeManager);
  FMarksCategoryFactoryConfig := TMarkCategoryFactoryConfig.Create(FLanguageManager);
  FMarksDB :=
    TMarksSystem.Create(
      FLanguageManager,
      FMarksDbPath,
      FMarkPictureList,
      FVectorItmesFactory,
      THtmlToHintTextConverterStuped.Create,
      FMarksCategoryFactoryConfig
    );
  VFilesIteratorFactory := TZmpFileNamesIteratorFactory.Create;
  VFilesIterator := VFilesIteratorFactory.CreateIterator(FMapsPath.FullPath, '');
  FZmpConfig := TZmpConfig.Create;
  FZmpConfig.ReadConfig(FMainConfigProvider.GetSubItem('ZmpDefaultParams'));
  FZmpInfoSet :=
    TZmpInfoSet.Create(
      FZmpConfig,
      FCoordConverterFactory,
      FContentTypeManager,
      FLanguageManager,
      VFilesIterator
    );
  FMainMapsList := TMapTypesMainList.Create(FZmpInfoSet, FPerfCounterList.CreateAndAddNewSubList('MapType'));
  FSkyMapDraw := TSatellitesInViewMapDrawSimple.Create;
  FPathDetalizeList :=
    TPathDetalizeProviderListSimple.Create(
      FLanguageManager,
      FInetConfig,
      FGCThread.List,
      TDownloadResultFactory.Create(FDownloadResultTextProvider),
      TVectorDataFactorySimple.Create(THtmlToHintTextConverterStuped.Create),
      FVectorItmesFactory,
      VKmlLoader
    );
  VInternalDomainInfoProviderList := TInternalDomainInfoProviderList.Create;
  VInternalDomainInfoProviderList.Add(
    CZmpInfoInternalDomain,
    TInternalDomainInfoProviderByMapTypeList.Create(FZmpInfoSet, FContentTypeManager)
  );

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByDataProvider.Create(
      TConfigDataProviderByPathConfig.Create(FMediaDataPath),
      FContentTypeManager
    );
  VInternalDomainInfoProviderList.Add(
    CMediaDataInternalDomain,
    VInternalDomainInfoProvider
  );


  FProtocol := TIeEmbeddedProtocolRegistration.Create(CSASProtocolName, TIeEmbeddedProtocolFactory.Create(VInternalDomainInfoProviderList));
  FInvisibleBrowser :=
    TInvisibleBrowserByFormSynchronize.Create(
      FLanguageManager,
      FInetConfig.ProxyConfig
    );
  FInternalBrowser :=
    TInternalBrowserByForm.Create(
      FLanguageManager,
      FInternalBrowserConfig,
      FInetConfig.ProxyConfig,
      FContentTypeManager
    );
  FDebugInfoWindow :=
    TDebugInfoWindow.Create(
      FGlobalAppConfig,
      FPerfCounterList
    );
  FBatteryStatus := TBatteryStatus.Create;
end;

destructor TGlobalState.Destroy;
begin
  FGCThread.Terminate;
  FGCThread.WaitFor;
  FreeAndNil(FGCThread);
  FLanguageManager := nil;
  FTileNameGenerator := nil;
  FContentTypeManager := nil;
  FMapCalibrationList := nil;
  FreeAndNil(FMarksDB);
  FLastSelectionInfo := nil;
  FGPSConfig := nil;
  FGPSRecorder := nil;
  FreeAndNil(FGPSpar);
  FreeAndNil(FMainMapsList);
  FCoordConverterFactory := nil;
  FGSMpar := nil;
  FInetConfig := nil;
  FViewConfig := nil;
  FImageResamplerConfig := nil;
  FMainFormConfig := nil;
  FBitmapPostProcessingConfig := nil;
  FValueToStringConverterConfig := nil;
  FMainMemCacheConfig := nil;
  FMarksCategoryFactoryConfig := nil;
  FMarkPictureList := nil;
  FreeAndNil(FCacheConfig);
  FSkyMapDraw := nil;
  FreeAndNil(FProtocol);
  FreeAndNil(FGUISyncronizedTimer);
  FGUISyncronizedTimerNotifier := nil;
  FMainConfigProvider := nil;
  FGlobalInternetState := nil;
  inherited;
end;

{$IFDEF SasDebugWithJcl}
procedure TGlobalState.DoException(
  Sender: TObject;
  E: Exception
);
var
  VStr: TStringList;
begin
  VStr := TStringList.Create;
  try
    JclLastExceptStackListToStrings(VStr, True, True, True, True);
    VStr.Insert(0, E.Message);
    VStr.Insert(1, '');
    Application.MessageBox(PChar(VStr.Text), 'Ошибка', MB_OK or MB_ICONSTOP);
  finally
    FreeAndNil(VStr);
  end;
end;

{$ENDIF SasDebugWithJcl}

procedure TGlobalState.StartExceptionTracking;
begin
  {$IFDEF SasDebugWithJcl}
  JclStackTrackingOptions := JclStackTrackingOptions + [stRAWMode];
  JclStartExceptionTracking;
  Application.OnException := DoException;
  {$ENDIF SasDebugWithJcl}
end;

procedure TGlobalState.StartThreads;
begin
  if FGlobalAppConfig.IsSendStatistic then begin
    FInvisibleBrowser.NavigateAndWait('http://sasgis.ru/stat/index.html');
  end;
  GPSpar.StartThreads;
  FGUISyncronizedTimer.Enabled := True;
end;

procedure TGlobalState.StopExceptionTracking;
begin
  {$IFDEF SasDebugWithJcl}
  Application.OnException := nil;
  JclStopExceptionTracking;
  {$ENDIF SasDebugWithJcl}
end;

procedure TGlobalState.LoadConfig;
var
  VLocalMapsConfig: IConfigDataProvider;
  Ini: TMeminifile;
  VMapsPath: String;
  VProjFactory: IProjConverterFactory;
begin
  VMapsPath := IncludeTrailingPathDelimiter(FMapsPath.FullPath);
  ForceDirectories(VMapsPath);
  Ini := TMeminiFile.Create(VMapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataProviderByIniFile.Create(Ini);

  FCacheConfig.LoadConfig(FMainConfigProvider);

  VProjFactory := TProjConverterFactory.Create;

  FMainMapsList.LoadMaps(
    FLanguageManager,
    FMainMemCacheConfig,
    FCacheConfig,
    FTileNameGenerator,
    FTileNameParser,
    FGCThread.List,
    FAppClosingNotifier,
    FInetConfig,
    FImageResamplerConfig,
    FDownloadConfig,
    FDownloaderThreadConfig,
    FContentTypeManager,
    FDownloadResultTextProvider,
    FCoordConverterFactory,
    FInvisibleBrowser,
    VProjFactory,
    VLocalMapsConfig
  );
  FMainFormConfig :=
    TMainFormConfig.Create(
      FLocalConverterFactory,
      FContentTypeManager,
      FGeoCoderList,
      FMainMapsList.MapsSet,
      FMainMapsList.LayersSet,
      FMainMapsList.FirstMainMapGUID,
      FPerfCounterList.CreateAndAddNewSubList('ViewState')
    );

  FSensorList :=
    TSensorListStuped.Create(
      FLanguageManager,
      FMainFormConfig.ViewPortState,
      FMainFormConfig.NavToPoint,
      FGPSRecorder,
      FBatteryStatus,
      FValueToStringConverterConfig
    );
  FInternalBrowserConfig.ReadConfig(MainConfigProvider.GetSubItem('InternalBrowser'));
  FViewConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
  FGPSRecorder.ReadConfig(MainConfigProvider.GetSubItem('GPS'));
  FGPSConfig.ReadConfig(MainConfigProvider.GetSubItem('GPS'));
  FInetConfig.ReadConfig(MainConfigProvider.GetSubItem('Internet'));
  FDownloadConfig.ReadConfig(MainConfigProvider.GetSubItem('Internet'));
  FDownloaderThreadConfig.ReadConfig(MainConfigProvider.GetSubItem('Internet'));
  FGSMpar.ReadConfig(MainConfigProvider.GetSubItem('GSM'));
  FBitmapPostProcessingConfig.ReadConfig(MainConfigProvider.GetSubItem('COLOR_LEVELS'));
  FValueToStringConverterConfig.ReadConfig(MainConfigProvider.GetSubItem('ValueFormats'));

  if (not ModuleIsLib) then begin
    FMainFormConfig.ReadConfig(MainConfigProvider);
    FLastSelectionInfo.ReadConfig(MainConfigProvider.GetSubItem('LastSelection'));
    FImageResamplerConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
    FMainMemCacheConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
    FMarkPictureList.ReadConfig(MainConfigProvider);
    FMarksCategoryFactoryConfig.ReadConfig(MainConfigProvider.GetSubItem('MarkNewCategory'));
    FMarksDb.ReadConfig(MainConfigProvider);
  end;
end;

procedure TGlobalState.OnGUISyncronizedTimer(Sender: TObject);
begin
  FGUISyncronizedTimerNotifier.Notify(nil);
end;

procedure TGlobalState.SaveMainParams;
var
  Ini: TMeminifile;
  VLocalMapsConfig: IConfigDataWriteProvider;
  VMapsPath: String;
begin
  if ModuleIsLib then begin
    Exit;
  end;
  VMapsPath := IncludeTrailingPathDelimiter(FMapsPath.FullPath);
  Ini := TMeminiFile.Create(VMapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataWriteProviderByIniFile.Create(Ini);
  FMainMapsList.SaveMaps(VLocalMapsConfig);

  FGPSRecorder.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GPS'));
  FGPSConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GPS'));
  FInetConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Internet'));
  FDownloadConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Internet'));
  FDownloaderThreadConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Internet'));
  FZmpConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('ZmpDefaultParams'));
  FGSMpar.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GSM'));
  FViewConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FInternalBrowserConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('InternalBrowser'));
  FLastSelectionInfo.WriteConfig(MainConfigProvider.GetOrCreateSubItem('LastSelection'));
  FLanguageManager.WriteConfig(FMainConfigProvider.GetOrCreateSubItem('VIEW'));
  FGlobalAppConfig.WriteConfig(FMainConfigProvider.GetOrCreateSubItem('VIEW'));
  FStartUpLogoConfig.WriteConfig(FMainConfigProvider.GetOrCreateSubItem('StartUpLogo'));
  FBitmapPostProcessingConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('COLOR_LEVELS'));
  FValueToStringConverterConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('ValueFormats'));
  FMainFormConfig.WriteConfig(MainConfigProvider);
  FCacheConfig.SaveConfig(FMainConfigProvider);
  FImageResamplerConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FMainMemCacheConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FMarkPictureList.WriteConfig(MainConfigProvider);
  FMarksCategoryFactoryConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('MarkNewCategory'));
  FMarksDb.WriteConfig(MainConfigProvider);

  FMapsPath.WriteConfig(FMainConfigProvider.GetOrCreateSubItem('PATHtoMAPS'));
  FTrackPath.WriteConfig(FMainConfigProvider.GetOrCreateSubItem('PATHtoTRACKS'));
  FMarksDbPath.WriteConfig(FMainConfigProvider.GetOrCreateSubItem('PATHtoMARKS'));
  FMediaDataPath.WriteConfig(FMainConfigProvider.GetOrCreateSubItem('PATHtoMediaData'));
end;

procedure TGlobalState.SendTerminateToThreads;
begin
  FGUISyncronizedTimer.Enabled := False;
  FAppClosingNotifier.Notify(nil);
  GPSpar.SendTerminateToThreads;
  FGCThread.Terminate;
end;

end.

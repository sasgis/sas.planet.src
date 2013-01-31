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
  Windows,
  {$IFDEF SasDebugWithJcl}
  JclDebug,
  {$ENDIF SasDebugWithJcl}
  ExtCtrls,
  Classes,
  IniFiles,
  SysUtils,
  i_NotifierOperation,
  i_GPSPositionFactory,
  i_LanguageManager,
  i_InetConfig,
  i_Listener,
  i_BackgroundTask,
  i_ConfigDataWriteProvider,
  i_ConfigDataProvider,
  i_LastSearchResultConfig,
  i_TileFileNameGeneratorsList,
  i_TileFileNameParsersList,
  i_ContentTypeManager,
  i_VectorDataLoader,
  i_CoordConverterFactory,
  i_CoordConverterList,
  i_ProjConverter,
  i_BatteryStatus,
  i_LocalCoordConverterFactorySimpe,
  i_ProxySettings,
  i_GPSModule,
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
  u_LastSelectionInfo,
  i_MarksSystem,
  u_MapTypesMainList,
  i_ThreadConfig,
  i_ZmpConfig,
  i_ZmpInfoSet,
  i_GPSConfig,
  i_PathConfig,
  i_NotifierTime,
  i_Bitmap32StaticFactory,
  i_MapCalibration,
  i_MarksFactoryConfig,
  i_MarkCategoryFactoryConfig,
  i_GlobalViewMainConfig,
  i_GlobalDownloadConfig,
  i_StartUpLogoConfig,
  i_ImportFile,
  i_PathDetalizeProviderList,
  i_GPSRecorder,
  i_SatellitesInViewMapDraw,
  i_SensorList,
  i_TerrainProviderList,
  i_TerrainConfig,
  i_VectorItemsFactory,
  i_InvisibleBrowser,
  i_InternalBrowser,
  i_DebugInfoWindow,
  i_GlobalInternetState,
  i_GlobalBerkeleyDBHelper,
  i_BitmapTileSaveLoadFactory,
  i_ArchiveReadWriteFactory,
  i_GlobalConfig,
  u_IeEmbeddedProtocolRegistration,
  u_GlobalCacheConfig;

{$I vsagps_defines.inc}

type
  TGlobalState = class
  private
    FGlobalConfig: IGlobalConfig;
    FBaseConfigPath: IPathConfig;
    FBaseDataPath: IPathConfig;
    FBaseCahcePath: IPathConfig;
    FBaseApplicationPath: IPathConfig;

    FMainConfigProvider: IConfigDataWriteProvider;
    FZmpConfig: IZmpConfig;
    FZmpInfoSet: IZmpInfoSet;
    FResourceProvider: IConfigDataProvider;
    FStartUpLogoConfig: IStartUpLogoConfig;
    FTileNameGenerator: ITileFileNameGeneratorsList;
    FTileNameParser: ITileFileNameParsersList;
    FGCThread: TGarbageCollectorThread;
    FContentTypeManager: IContentTypeManager;
    FMapCalibrationList: IMapCalibrationList;
    FCacheConfig: TGlobalCacheConfig;
    FMarksDb: IMarksSystem;
    FCoordConverterFactory: ICoordConverterFactory;
    FCoordConverterList: ICoordConverterList;
    FProjConverterFactory: IProjConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FMainMapsList: TMapTypesMainList;
    FGPSConfig: IGPSConfig;
    FGPSPositionFactory: IGPSPositionFactory;
    FMainFormConfig: IMainFormConfig;
    FBitmapPostProcessingConfig: IBitmapPostProcessingConfig;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FDownloadInfo: IDownloadInfoSimple;
    FDownloadConfig: IGlobalDownloadConfig;
    FDownloaderThreadConfig: IThreadConfig;
    FMainThreadConfig: IThreadConfig;
    FGlobalInternetState: IGlobalInternetState;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FImageResamplerConfig: IImageResamplerConfig;
    FTileMatrixDraftResamplerConfig: IImageResamplerConfig;
    FTileLoadResamplerConfig: IImageResamplerConfig;
    FTileGetPrevResamplerConfig: IImageResamplerConfig;
    FTileReprojectResamplerConfig: IImageResamplerConfig;
    FTileDownloadResamplerConfig: IImageResamplerConfig;
    FGeoCoderList: IGeoCoderList;
    FMainMemCacheConfig: IMainMemCacheConfig;
    FMarkPictureList: IMarkPictureList;
    FMarksFactoryConfig: IMarksFactoryConfig;
    FMarksCategoryFactoryConfig: IMarkCategoryFactoryConfig;
    FGpsSystem: IGPSModule;
    FImportFileByExt: IImportFile;
    FViewConfig: IGlobalViewMainConfig;
    FGPSRecorder: IGPSRecorder;
    FGPSRecorderInternal: IGPSRecorderInternal;
    FGpsTrackRecorder: IGpsTrackRecorder;
    FGpsTrackRecorderInternal: IGpsTrackRecorderInternal;
    FSkyMapDraw: ISatellitesInViewMapDraw;
    FBGTimerNotifier: INotifierTime;
    FBGTimerNotifierInternal: INotifierTimeInternal;
    FGUISyncronizedTimer: TTimer;
    FGUISyncronizedTimerNotifierInternal: INotifierTimeInternal;
    FGUISyncronizedTimerNotifier: INotifierTime;
    FGUISyncronizedTimerCounter: IInternalPerformanceCounter;
    FSensorList: ISensorList;
    FPerfCounterList: IInternalPerformanceCounterList;
    FProtocol: TIeEmbeddedProtocolRegistration;
    FPathDetalizeList: IPathDetalizeProviderList;
    FInvisibleBrowser: IInvisibleBrowser;
    FInternalBrowser: IInternalBrowser;
    FDebugInfoWindow: IDebugInfoWindow;
    FAppStartedNotifier: INotifierOneOperation;
    FAppStartedNotifierInternal: INotifierOneOperationInternal;
    FAppClosingNotifier: INotifierOneOperation;
    FAppClosingNotifierInternal: INotifierOneOperationInternal;
    FVectorItemsFactory: IVectorItemsFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FBatteryStatus: IBatteryStatus;
    FTerrainProviderList: ITerrainProviderList;
    FTerrainConfig: ITerrainConfig;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FLastSelectionSaver: IBackgroundTask;
    FLastSearchResultConfig: ILastSearchResultConfig;
    FMainThreadConfigListener: IListener;
    procedure OnMainThreadConfigChange;
    procedure InitProtocol;

    procedure OnGUISyncronizedTimer(Sender: TObject);
    {$IFDEF SasDebugWithJcl}
    procedure DoException(
      Sender: TObject;
      E: Exception
    );
    {$ENDIF SasDebugWithJcl}
  public
    property Config: IGlobalConfig read FGlobalConfig;

    property MapType: TMapTypesMainList read FMainMapsList;
    property CacheConfig: TGlobalCacheConfig read FCacheConfig;
    property MarksDb: IMarksSystem read FMarksDb;
    property GpsSystem: IGPSModule read FGpsSystem;

    // Список генераторов имен файлов с тайлами
    property TileNameGenerator: ITileFileNameGeneratorsList read FTileNameGenerator;
    property TileNameParser: ITileFileNameParsersList read FTileNameParser;
    property ContentTypeManager: IContentTypeManager read FContentTypeManager;
    property CoordConverterFactory: ICoordConverterFactory read FCoordConverterFactory;
    property CoordConverterList: ICoordConverterList read FCoordConverterList;
    property ProjectionFactory: IProjectionInfoFactory read FProjectionFactory;
    property LocalConverterFactory: ILocalCoordConverterFactorySimpe read FLocalConverterFactory;
    property MapCalibrationList: IMapCalibrationList read FMapCalibrationList;
    property AppStartedNotifier: INotifierOneOperation read FAppStartedNotifier;
    property AppClosingNotifier: INotifierOneOperation read FAppClosingNotifier;

    property MainConfigProvider: IConfigDataWriteProvider read FMainConfigProvider;
    property ResourceProvider: IConfigDataProvider read FResourceProvider;
    property DownloadInfo: IDownloadInfoSimple read FDownloadInfo;
    property GlobalInternetState: IGlobalInternetState read FGlobalInternetState;
    property ImportFileByExt: IImportFile read FImportFileByExt;
    property SkyMapDraw: ISatellitesInViewMapDraw read FSkyMapDraw;
    property GUISyncronizedTimerNotifier: INotifierTime read FGUISyncronizedTimerNotifier;
    property BGTimerNotifier: INotifierTime read FBGTimerNotifier;
    property PerfCounterList: IInternalPerformanceCounterList read FPerfCounterList;

    property MainFormConfig: IMainFormConfig read FMainFormConfig;
    property BitmapPostProcessingConfig: IBitmapPostProcessingConfig read FBitmapPostProcessingConfig;
    property ValueToStringConverterConfig: IValueToStringConverterConfig read FValueToStringConverterConfig;
    property ImageResamplerConfig: IImageResamplerConfig read FImageResamplerConfig;
    property TileMatrixDraftResamplerConfig: IImageResamplerConfig read FTileMatrixDraftResamplerConfig;
    property MainMemCacheConfig: IMainMemCacheConfig read FMainMemCacheConfig;
    property GPSConfig: IGPSConfig read FGPSConfig;
    property MarksCategoryFactoryConfig: IMarkCategoryFactoryConfig read FMarksCategoryFactoryConfig;
    property ViewConfig: IGlobalViewMainConfig read FViewConfig;
    property GPSRecorder: IGPSRecorder read FGPSRecorder;
    property GpsTrackRecorder: IGpsTrackRecorder read FGpsTrackRecorder;
    property PathDetalizeList: IPathDetalizeProviderList read FPathDetalizeList;
    property SensorList: ISensorList read FSensorList;
    property DownloadConfig: IGlobalDownloadConfig read FDownloadConfig;
    property DownloaderThreadConfig: IThreadConfig read FDownloaderThreadConfig;
    property StartUpLogoConfig: IStartUpLogoConfig read FStartUpLogoConfig;
    property InternalBrowser: IInternalBrowser read FInternalBrowser;
    property DebugInfoWindow: IDebugInfoWindow read FDebugInfoWindow;
    property VectorItemsFactory: IVectorItemsFactory read FVectorItemsFactory;
    property BitmapFactory: IBitmap32StaticFactory read FBitmapFactory;
    property BitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory read FBitmapTileSaveLoadFactory;
    property ArchiveReadWriteFactory: IArchiveReadWriteFactory read FArchiveReadWriteFactory;
    property TerrainProviderList: ITerrainProviderList read FTerrainProviderList;
    property TerrainConfig: ITerrainConfig read FTerrainConfig;
    property GlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper read FGlobalBerkeleyDBHelper;

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
  u_Notifier,
  u_NotifierOperation,
  c_InternalBrowser,
  u_SASMainConfigProvider,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile,
  u_ConfigDataProviderByPathConfig,
  i_InternalDomainInfoProvider,
  i_ImageResamplerFactory,
  i_TextByVectorItem,
  u_TextByVectorItemHTMLByDescription,
  u_TextByVectorItemMarkInfo,
  u_NotifierTime,
  i_FileNameIterator,
  u_ContentTypeManagerSimple,
  u_MarksSystem,
  u_MapCalibrationListBasic,
  u_XmlInfoSimpleParser,
  u_KmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_CoordConverterFactorySimple,
  u_CoordConverterListStaticSimple,
  u_DownloadInfoSimple,
  u_StartUpLogoConfig,
  u_Datum,
  u_PLTSimpleParser,
  u_GPSConfig,
  u_MarkCategoryFactoryConfig,
  u_GeoCoderListSimple,
  u_BitmapPostProcessingConfig,
  u_ValueToStringConverterConfig,
  u_GlobalAppConfig,
  u_MainMemCacheConfig,
  u_MarkPictureListSimple,
  u_MarksFactoryConfig,
  u_ImageResamplerConfig,
  u_ImageResamplerFactoryListStaticSimple,
  u_ImportByFileExt,
  u_GlobalViewMainConfig,
  u_GlobalDownloadConfig,
  u_GlobalBerkeleyDBHelper,
  u_GPSRecorder,
  u_GpsTrackRecorder,
  u_SatellitesInViewMapDrawSimple,
  u_GPSModuleFactoryByVSAGPS,
  u_GPSPositionFactory,
  u_ProjectionInfoFactory,
  u_LocalCoordConverterFactorySimpe,
  u_TerrainProviderList,
  u_TerrainConfig,
  u_MainFormConfig,
  u_LastSearchResultConfig,
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
  u_BaseInterfacedObject,
  u_BaseInterfacedObjectDebug,
  u_InternalPerformanceCounter,
  u_InternalPerformanceCounterList,
  u_InternalPerformanceCounterFake,
  u_IeEmbeddedProtocolFactory,
  u_VectorItemsFactorySimple,
  u_VectorDataFactorySimple,
  u_DownloadResultFactory,
  u_PathDetalizeProviderListSimple,
  u_InternalDomainInfoProviderList,
  u_InternalDomainInfoProviderByMapTypeList,
  u_InternalDomainInfoProviderByDataProvider,
  u_InternalDomainInfoProviderByMarksSystem,
  u_InternalDomainInfoProviderByMapData,
  u_InternalDomainInfoProviderByLastSearchResults,
  u_InternalDomainInfoProviderByTileStorageOptions,
  u_Bitmap32StaticFactory,
  u_GpsSystem,
  u_LastSelectionInfoSaver,
  u_ListenerByEvent,
  u_Synchronizer,
  u_GlobalConfig,
  u_GlobalInternetState,
  u_BitmapTileSaveLoadFactory,
  u_ArchiveReadWriteFactory,
  u_TileFileNameParsersSimpleList,
  u_TileFileNameGeneratorsSimpleList;

{ TGlobalState }

constructor TGlobalState.Create;
var
  VViewCnonfig: IConfigDataProvider;
  VMarksKmlLoadCounterList: IInternalPerformanceCounterList;
  VXmlLoader: IVectorDataLoader;
  VKmlLoader: IVectorDataLoader;
  VKmzLoader: IVectorDataLoader;
  VFilesIteratorFactory: IFileNameIteratorFactory;
  VFilesIterator: IFileNameIterator;
  VProgramPath: string;
  VSleepByClass: IConfigDataProvider;
  VResamplerFactoryList: IImageResamplerFactoryList;
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
  FBaseCahcePath := TPathConfig.Create('', VProgramPath, nil);

  FGlobalConfig :=
    TGlobalConfig.Create(
      FBaseCahcePath,
      FBaseConfigPath,
      FBaseDataPath,
      FBaseApplicationPath
    );
  FBGTimerNotifierInternal := TNotifierTime.Create;
  FBGTimerNotifier := FBGTimerNotifierInternal;
  FBitmapFactory :=
    TBitmap32StaticFactory.Create(
      FBGTimerNotifier,
      MakeSyncRW_Var(Self, false)
    );

  FBitmapTileSaveLoadFactory := TBitmapTileSaveLoadFactory.Create(FBitmapFactory);
  FArchiveReadWriteFactory := TArchiveReadWriteFactory.Create;

  FAppStartedNotifierInternal := TNotifierOneOperation.Create(TNotifierBase.Create);
  FAppStartedNotifier := FAppStartedNotifierInternal;
  FAppClosingNotifierInternal := TNotifierOneOperation.Create(TNotifierBase.Create);
  FAppClosingNotifier := FAppClosingNotifierInternal;
  FMainConfigProvider :=
    TSASMainConfigProvider.Create(
      FBaseConfigPath.FullPath,
      ExtractFileName(ParamStr(0)),
      HInstance
    );

  FGlobalConfig.ReadConfig(FMainConfigProvider);

  VSleepByClass := FMainConfigProvider.GetSubItem('SleepByClass');

  FResourceProvider := FMainConfigProvider.GetSubItem('sas:\Resource');
  FVectorItemsFactory := TVectorItemsFactorySimple.Create;

  FGlobalInternetState := TGlobalInternetState.Create;

  FProjConverterFactory := TProjConverterFactory.Create;

  FCoordConverterFactory := TCoordConverterFactorySimple.Create;
  FProjectionFactory := TProjectionInfoFactory.Create(MakeSyncRW_Var(Self, False));
  FCoordConverterList := TCoordConverterListStaticSimple.Create(FCoordConverterFactory);
  FLocalConverterFactory := TLocalCoordConverterFactorySimpe.Create(FProjectionFactory);

  FCacheConfig := TGlobalCacheConfig.Create(FBaseCahcePath);
  FDownloadInfo := TDownloadInfoSimple.Create(nil);
  VViewCnonfig := FMainConfigProvider.GetSubItem('VIEW');

  if FGlobalConfig.GlobalAppConfig.IsShowDebugInfo then begin
    FPerfCounterList := TInternalPerformanceCounterList.Create('Main', TInternalPerformanceCounterFactory.Create);
    if TBaseInterfacedObject = TBaseInterfacedObjectDebug then begin
      FPerfCounterList.AddSubList(TBaseInterfacedObjectDebug.GetCounters);
    end;
  end else begin
    FPerfCounterList := TInternalPerformanceCounterFake.Create;
  end;

  FGUISyncronizedTimer := TTimer.Create(nil);
  FGUISyncronizedTimer.Enabled := False;
  FGUISyncronizedTimer.Interval := VSleepByClass.ReadInteger('GUISyncronizedTimer', 200);
  FGUISyncronizedTimer.OnTimer := Self.OnGUISyncronizedTimer;

  FGUISyncronizedTimerNotifierInternal := TNotifierTime.Create;
  FGUISyncronizedTimerNotifier := FGUISyncronizedTimerNotifierInternal;
  FGUISyncronizedTimerCounter := FPerfCounterList.CreateAndAddNewCounter('GUITimer');

  FGlobalBerkeleyDBHelper := TGlobalBerkeleyDBHelper.Create(FCacheConfig.BDBCachePath);

  FTerrainProviderList := TTerrainProviderListSimple.Create(FProjConverterFactory, FGlobalConfig.TerrainDataPath, FCacheConfig);
  FTerrainConfig := TTerrainConfig.Create;

  FDownloadConfig := TGlobalDownloadConfig.Create;
  FDownloaderThreadConfig := TThreadConfig.Create(tpLower);
  FMainThreadConfig := TThreadConfig.Create(tpHigher);
  FMainThreadConfigListener := TNotifyEventListenerSync.Create(FGUISyncronizedTimerNotifier, 1000, Self.OnMainThreadConfigChange);
  FMainThreadConfig.ChangeNotifier.Add(FMainThreadConfigListener);
  OnMainThreadConfigChange;

  VResamplerFactoryList := TImageResamplerFactoryListStaticSimple.Create;
  FImageResamplerConfig := TImageResamplerConfig.Create(VResamplerFactoryList);
  FTileMatrixDraftResamplerConfig := TImageResamplerConfig.Create(VResamplerFactoryList);

  FGPSConfig := TGPSConfig.Create(FGlobalConfig.TrackPath);
  FGPSPositionFactory := TGPSPositionFactory.Create;
  FGPSRecorderInternal :=
    TGPSRecorder.Create(
      TDatum.Create(3395, 6378137, 6356752),
      FGlobalConfig.GpsRecorderFileName,
      FGPSPositionFactory.BuildPositionEmpty
    );
  FGPSRecorder := FGPSRecorderInternal;

  FGpsTrackRecorderInternal :=
    TGpsTrackRecorder.Create(
      FVectorItemsFactory,
      FGlobalConfig.GpsTrackRecorderFileName
    );
  FGpsTrackRecorder := FGpsTrackRecorderInternal;
  FMainMemCacheConfig := TMainMemCacheConfig.Create;
  FViewConfig := TGlobalViewMainConfig.Create;

  FTileNameGenerator := TTileFileNameGeneratorsSimpleList.Create;
  FTileNameParser := TTileFileNameParsersSimpleList.Create;

  FContentTypeManager :=
    TContentTypeManagerSimple.Create(
      FVectorItemsFactory,
      FBitmapTileSaveLoadFactory,
      FArchiveReadWriteFactory,
      FPerfCounterList
    );

  if (not ModuleIsLib) then begin
    FStartUpLogoConfig := TStartUpLogoConfig.Create(FContentTypeManager);
    FStartUpLogoConfig.ReadConfig(FMainConfigProvider.GetSubItem('StartUpLogo'));
  end;

  FMapCalibrationList := TMapCalibrationListBasic.Create;
  VMarksKmlLoadCounterList := FPerfCounterList.CreateAndAddNewSubList('Import');

  // xml loaders
  VXmlLoader :=
    TXmlInfoSimpleParser.Create(
      FVectorItemsFactory,
      False,
      nil,
      VMarksKmlLoadCounterList
    );
{$if defined(VSAGPS_ALLOW_IMPORT_KML)}
  VKmlLoader := VXmlLoader;
  VKmzLoader :=
    TXmlInfoSimpleParser.Create(
      FVectorItemsFactory,
      False,
      FArchiveReadWriteFactory,
      VMarksKmlLoadCounterList
    );
{$else}
  VKmlLoader :=
    TKmlInfoSimpleParser.Create(
      FVectorItemsFactory,
      VMarksKmlLoadCounterList
    );
  VKmzLoader :=
    TKmzInfoSimpleParser.Create(
      FVectorItemsFactory,
      FArchiveReadWriteFactory,
      VMarksKmlLoadCounterList
    );
{$ifend}

  FImportFileByExt := TImportByFileExt.Create(
    TVectorDataFactorySimple.Create(THtmlToHintTextConverterStuped.Create),
    FVectorItemsFactory,
    VXmlLoader,
    TPLTSimpleParser.Create(
      FVectorItemsFactory,
      VMarksKmlLoadCounterList
    ),
    VKmlLoader,
    VKmzLoader
  );
  FGCThread :=
    TGarbageCollectorThread.Create(
      FAppClosingNotifier,
      FPerfCounterList.CreateAndAddNewCounter('GCTimer'),
      FBGTimerNotifierInternal,
      VSleepByClass.ReadInteger(TGarbageCollectorThread.ClassName, 1000)
    );
  FBitmapPostProcessingConfig := TBitmapPostProcessingConfig.Create(FBitmapFactory);
  FValueToStringConverterConfig := TValueToStringConverterConfig.Create(FGlobalConfig.LanguageManager);
  FGpsSystem :=
    TGpsSystem.Create(
      FAppStartedNotifier,
      FAppClosingNotifier,
      TGPSModuleFactoryByVSAGPS.Create(FGPSPositionFactory),
      FGPSConfig,
      FGPSRecorderInternal,
      FGpsTrackRecorderInternal,
      GUISyncronizedTimerNotifier,
      FPerfCounterList
    );
  FGeoCoderList :=
    TGeoCoderListSimple.Create(
      FGlobalConfig.InetConfig,
      BGTimerNotifier,
      TDownloadResultFactory.Create,
      FValueToStringConverterConfig
    );
  FMarkPictureList := TMarkPictureListSimple.Create(FGlobalConfig.MarksIconsPath, FContentTypeManager);
  FMarksFactoryConfig :=
    TMarksFactoryConfig.Create(
      FGlobalConfig.LanguageManager,
      FMarkPictureList
    );
  FMarksCategoryFactoryConfig := TMarkCategoryFactoryConfig.Create(FGlobalConfig.LanguageManager);

  FMarksDb :=
    TMarksSystem.Create(
      FGlobalConfig.LanguageManager,
      FGlobalConfig.MarksDbPath,
      FMarkPictureList,
      FVectorItemsFactory,
      FPerfCounterList.CreateAndAddNewSubList('MarksSystem'),
      THtmlToHintTextConverterStuped.Create,
      FMarksFactoryConfig,
      FMarksCategoryFactoryConfig
    );
  VFilesIteratorFactory := TZmpFileNamesIteratorFactory.Create;
  VFilesIterator := VFilesIteratorFactory.CreateIterator(FGlobalConfig.MapsPath.FullPath, '');
  FZmpConfig := TZmpConfig.Create;
  FZmpConfig.ReadConfig(FMainConfigProvider.GetSubItem('ZmpDefaultParams'));
  FZmpInfoSet :=
    TZmpInfoSet.Create(
      FZmpConfig,
      FCoordConverterFactory,
      FArchiveReadWriteFactory,
      FContentTypeManager,
      FBitmapFactory,
      FGlobalConfig.LanguageManager,
      VFilesIterator
    );

  FTileLoadResamplerConfig := TImageResamplerConfig.Create(VResamplerFactoryList);
  FTileGetPrevResamplerConfig := TImageResamplerConfig.Create(VResamplerFactoryList);
  FTileReprojectResamplerConfig := TImageResamplerConfig.Create(VResamplerFactoryList);
  FTileDownloadResamplerConfig := TImageResamplerConfig.Create(VResamplerFactoryList);

  FMainMapsList :=
    TMapTypesMainList.Create(
      FZmpInfoSet,
      FTileLoadResamplerConfig,
      FTileGetPrevResamplerConfig,
      FTileReprojectResamplerConfig,
      FTileDownloadResamplerConfig,
      FPerfCounterList.CreateAndAddNewSubList('MapType')
    );
  FSkyMapDraw := TSatellitesInViewMapDrawSimple.Create;
  FPathDetalizeList :=
    TPathDetalizeProviderListSimple.Create(
      FGlobalConfig.LanguageManager,
      FGlobalConfig.InetConfig,
      FBGTimerNotifier,
      TDownloadResultFactory.Create,
      TVectorDataFactorySimple.Create(THtmlToHintTextConverterStuped.Create),
      FVectorItemsFactory,
      VKmlLoader
    );
  FLastSearchResultConfig := TLastSearchResultConfig.Create;

  InitProtocol;

  FInvisibleBrowser :=
    TInvisibleBrowserByFormSynchronize.Create(
      FGlobalConfig.LanguageManager,
      FGlobalConfig.InetConfig.ProxyConfig
    );
  FInternalBrowser :=
    TInternalBrowserByForm.Create(
      FGlobalConfig.LanguageManager,
      FGlobalConfig.InternalBrowserConfig,
      FGlobalConfig.InetConfig.ProxyConfig,
      FContentTypeManager
    );
  FDebugInfoWindow :=
    TDebugInfoWindow.Create(
      FGlobalConfig.GlobalAppConfig,
      FPerfCounterList
    );
  FBatteryStatus := TBatteryStatus.Create;
  FLastSelectionSaver :=
    TLastSelectionInfoSaver.Create(
      FAppClosingNotifier,
      FVectorItemsFactory,
      FGlobalConfig.LastSelectionInfo,
      FGlobalConfig.LastSelectionFileName
    );
end;

destructor TGlobalState.Destroy;
begin
  FGCThread.Terminate;
  FGCThread.WaitFor;
  FreeAndNil(FGCThread);
  FTileNameGenerator := nil;
  FContentTypeManager := nil;
  FMapCalibrationList := nil;
  FMarksDb := nil;
  FGPSConfig := nil;
  FGPSRecorder := nil;
  FreeAndNil(FMainMapsList);
  FCoordConverterFactory := nil;
  FViewConfig := nil;
  FMainFormConfig := nil;
  FLastSearchResultConfig := nil;
  FImageResamplerConfig := nil;
  FTileMatrixDraftResamplerConfig := nil;
  FBitmapPostProcessingConfig := nil;
  FValueToStringConverterConfig := nil;
  FMainMemCacheConfig := nil;
  FMarksCategoryFactoryConfig := nil;
  FMarkPictureList := nil;
  FSkyMapDraw := nil;
  FreeAndNil(FProtocol);
  FreeAndNil(FGUISyncronizedTimer);
  FGUISyncronizedTimerNotifier := nil;
  FMainConfigProvider := nil;
  FGlobalInternetState := nil;
  FArchiveReadWriteFactory := nil;
  FBitmapTileSaveLoadFactory := nil;
  FTerrainProviderList := nil;
  FTerrainConfig := nil;
  FProjConverterFactory := nil;
  FGlobalBerkeleyDBHelper := nil;
  FreeAndNil(FCacheConfig);
  inherited;
end;

procedure TGlobalState.InitProtocol;
var
  VInternalDomainInfoProviderList: TInternalDomainInfoProviderList;
  VInternalDomainInfoProvider: IInternalDomainInfoProvider;
  VTextProivder: ITextByVectorItem;
  VTextProviderList: TStringList;
begin
  VInternalDomainInfoProviderList := TInternalDomainInfoProviderList.Create;

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByMapTypeList.Create(
      FZmpInfoSet,
      FContentTypeManager
    );

  VInternalDomainInfoProviderList.Add(
    CZmpInfoInternalDomain,
    VInternalDomainInfoProvider
  );

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByDataProvider.Create(
      TConfigDataProviderByPathConfig.Create(FGlobalConfig.MediaDataPath),
      FContentTypeManager
    );
  VInternalDomainInfoProviderList.Add(
    CMediaDataInternalDomain,
    VInternalDomainInfoProvider
  );
  VTextProviderList := TStringList.Create;
  VTextProviderList.Sorted := True;
  VTextProviderList.Duplicates := dupError;
  VTextProivder :=
    TTextByVectorItemMarkInfo.Create(
      FValueToStringConverterConfig,
      TDatum.Create(3395, 6378137, 6356752)
    );

  VTextProviderList.AddObject(CVectorItemInfoSuffix, Pointer(VTextProivder));
  VTextProivder._AddRef;

  VTextProivder := TTextByVectorItemHTMLByDescription.Create;
  VTextProviderList.AddObject(CVectorItemDescriptionSuffix, Pointer(VTextProivder));
  VTextProivder._AddRef;

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByMarksSystem.Create(
      FMarksDb,
      VTextProivder,
      VTextProviderList
    );
  VInternalDomainInfoProviderList.Add(
    CMarksSystemInternalDomain,
    VInternalDomainInfoProvider
  );

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByLastSearchResults.Create(
      FLastSearchResultConfig,
      VTextProivder,
      nil
    );
  VInternalDomainInfoProviderList.Add(
    CLastSearchResultsInternalDomain,
    VInternalDomainInfoProvider
  );

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByMapData.Create(
      FMainMapsList.FullMapsSetChangeable,
      VTextProivder,
      CVectorItemDescriptionSuffix
    );

  VInternalDomainInfoProviderList.Add(
    CMapDataInternalDomain,
    VInternalDomainInfoProvider
  );

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByTileStorageOptions.Create(
      FMainMapsList.FullMapsSetChangeable
    );

  VInternalDomainInfoProviderList.Add(
    CTileStorageOptionsInternalDomain,
    VInternalDomainInfoProvider
  );

  FProtocol :=
    TIeEmbeddedProtocolRegistration.Create(
      CSASProtocolName,
      TIeEmbeddedProtocolFactory.Create(VInternalDomainInfoProviderList)
    );
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
  FAppStartedNotifierInternal.ExecuteOperation;
  if FGlobalConfig.GlobalAppConfig.IsSendStatistic then begin
    FInvisibleBrowser.NavigateAndWait('http://sasgis.ru/stat/index.html');
  end;
  FLastSelectionSaver.Start;
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
begin
  VMapsPath := IncludeTrailingPathDelimiter(FGlobalConfig.MapsPath.FullPath);
  ForceDirectories(VMapsPath);
  Ini := TMeminiFile.Create(VMapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataProviderByIniFile.Create(Ini);

  FCacheConfig.LoadConfig(FMainConfigProvider);

  FTerrainConfig.ReadConfig(MainConfigProvider.GetSubItem('Terrain'));

  FMainMapsList.LoadMaps(
    FGlobalConfig.LanguageManager,
    FMainMemCacheConfig,
    FCacheConfig,
    FGlobalBerkeleyDBHelper,
    FTileNameGenerator,
    FTileNameParser,
    FBGTimerNotifier,
    FAppClosingNotifier,
    FGlobalConfig.InetConfig,
    FDownloadConfig,
    FDownloaderThreadConfig,
    FBitmapFactory,
    FContentTypeManager,
    FCoordConverterFactory,
    FInvisibleBrowser,
    FProjConverterFactory,
    VLocalMapsConfig
  );
  FMainFormConfig :=
    TMainFormConfig.Create(
      FLocalConverterFactory,
      FContentTypeManager,
      FGeoCoderList,
      FLastSearchResultConfig,
      FMainMapsList.MapsSet,
      FMainMapsList.LayersSet,
      FMainMapsList.FirstMainMapGUID,
      FPerfCounterList.CreateAndAddNewSubList('ViewState')
    );

  FSensorList :=
    TSensorListStuped.Create(
      FGlobalConfig.LanguageManager,
      FMainFormConfig.ViewPortState.View,
      FMainFormConfig.NavToPoint,
      FGPSRecorder,
      FGpsSystem,
      FBatteryStatus,
      FValueToStringConverterConfig
    );
  FViewConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
  FGPSRecorderInternal.Load;
  FGpsTrackRecorderInternal.Load;
  FGPSConfig.ReadConfig(MainConfigProvider.GetSubItem('GPS'));
  FDownloadConfig.ReadConfig(MainConfigProvider.GetSubItem('Internet'));
  FDownloaderThreadConfig.ReadConfig(MainConfigProvider.GetSubItem('Internet'));
  FMainThreadConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
  FBitmapPostProcessingConfig.ReadConfig(MainConfigProvider.GetSubItem('COLOR_LEVELS'));
  FValueToStringConverterConfig.ReadConfig(MainConfigProvider.GetSubItem('ValueFormats'));

  if (not ModuleIsLib) then begin
    FMainFormConfig.ReadConfig(MainConfigProvider);
    FImageResamplerConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
    FTileMatrixDraftResamplerConfig.ReadConfig(MainConfigProvider.GetSubItem('View_TilesDrafts'));
    FTileLoadResamplerConfig.ReadConfig(MainConfigProvider.GetSubItem('Maps_Load'));
    FTileGetPrevResamplerConfig.ReadConfig(MainConfigProvider.GetSubItem('Maps_GetPrev'));
    FTileReprojectResamplerConfig.ReadConfig(MainConfigProvider.GetSubItem('Maps_Reproject'));
    FTileDownloadResamplerConfig.ReadConfig(MainConfigProvider.GetSubItem('Maps_Download'));
    FMainMemCacheConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
    FMarkPictureList.ReadConfig(MainConfigProvider);
    FMarksFactoryConfig.ReadConfig(MainConfigProvider);
    FMarksCategoryFactoryConfig.ReadConfig(MainConfigProvider.GetSubItem('MarkNewCategory'));
    FMarksDb.ReadConfig(MainConfigProvider);
  end;
end;

procedure TGlobalState.OnGUISyncronizedTimer(Sender: TObject);
var
  VContext: TInternalPerformanceCounterContext;
  VNow: Cardinal;
begin
  VContext := FGUISyncronizedTimerCounter.StartOperation;
  try
    VNow := GetTickCount;
    FGUISyncronizedTimerNotifierInternal.Notify(VNow);
  finally
    FGUISyncronizedTimerCounter.FinishOperation(VContext);
  end;
end;

procedure TGlobalState.OnMainThreadConfigChange;
const
  Priorities: array [TThreadPriority] of Integer =
   (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);
begin
  SetThreadPriority(GetCurrentThread(), Priorities[FMainThreadConfig.Priority]);
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
  VMapsPath := IncludeTrailingPathDelimiter(FGlobalConfig.MapsPath.FullPath);
  Ini := TMeminiFile.Create(VMapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataWriteProviderByIniFile.Create(Ini);
  FMainMapsList.SaveMaps(VLocalMapsConfig);

  FGPSRecorderInternal.Save;
  FGpsTrackRecorderInternal.Save;
  FGPSConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GPS'));
  FDownloadConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Internet'));
  FDownloaderThreadConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Internet'));
  FMainThreadConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FZmpConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('ZmpDefaultParams'));
  FViewConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FStartUpLogoConfig.WriteConfig(FMainConfigProvider.GetOrCreateSubItem('StartUpLogo'));
  FBitmapPostProcessingConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('COLOR_LEVELS'));
  FValueToStringConverterConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('ValueFormats'));
  FMainFormConfig.WriteConfig(MainConfigProvider);
  FCacheConfig.SaveConfig(FMainConfigProvider);
  FImageResamplerConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FTileMatrixDraftResamplerConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View_TilesDrafts'));
  FTileLoadResamplerConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Maps_Load'));
  FTileGetPrevResamplerConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Maps_GetPrev'));
  FTileReprojectResamplerConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Maps_Reproject'));
  FTileDownloadResamplerConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Maps_Download'));
  FMainMemCacheConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FMarkPictureList.WriteConfig(MainConfigProvider);
  FMarksFactoryConfig.WriteConfig(MainConfigProvider);
  FMarksCategoryFactoryConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('MarkNewCategory'));
  FMarksDb.WriteConfig(MainConfigProvider);
  FTerrainConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Terrain'));
  FGlobalConfig.WriteConfig(MainConfigProvider);
end;

procedure TGlobalState.SendTerminateToThreads;
begin
  if FMainThreadConfig <> nil then begin
    FMainThreadConfig.ChangeNotifier.Remove(FMainThreadConfigListener);
  end;

  FGUISyncronizedTimer.Enabled := False;
  FAppClosingNotifierInternal.ExecuteOperation;
  FGCThread.Terminate;
end;

end.


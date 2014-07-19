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
  i_MapVersionFactoryList,
  i_NotifierOperation,
  i_GPSPositionFactory,
  i_HashFunction,
  i_Listener,
  i_AppearanceOfMarkFactory,
  i_BackgroundTask,
  i_ConfigDataWriteProvider,
  i_ConfigDataProvider,
  i_TileFileNameGeneratorsList,
  i_TileFileNameParsersList,
  i_ContentTypeManager,
  i_VectorDataLoader,
  i_CoordConverterFactory,
  i_CoordConverterList,
  i_ProjConverter,
  i_BatteryStatus,
  i_InternalBrowserLastContent,
  i_LocalCoordConverterFactorySimpe,
  i_GPSModule,
  i_GeometryProjectedProvider,
  i_DownloadInfoSimple,
  i_ImageResamplerConfig,
  i_GeoCoderList,
  i_MarkPicture,
  i_LastSelectionInfo,
  i_InternalPerformanceCounter,
  i_DebugInfoSubSystem,
  i_MarkSystem,
  i_ZmpInfoSet,
  i_Datum,
  i_GeoCalc,
  i_PathConfig,
  i_NotifierTime,
  i_Bitmap32BufferFactory,
  i_VectorDataFactory,
  i_GeometryProjectedFactory,
  i_VectorItemSubsetBuilder,
  i_GeoCoder,
  i_MapTypeSetBuilder,
  i_MapTypeListBuilder,
  i_MapCalibration,
  i_PathDetalizeProviderList,
  i_GPSRecorder,
  i_SatellitesInViewMapDraw,
  i_TerrainProviderList,
  i_GeometryLonLatFactory,
  i_InvisibleBrowser,
  i_InternalBrowser,
  i_DebugInfoWindow,
  i_GlobalInternetState,
  i_GlobalBerkeleyDBHelper,
  i_BitmapPostProcessing,
  i_BitmapTileSaveLoadFactory,
  i_ArchiveReadWriteFactory,
  i_SystemTimeProvider,
  i_MarkCategoryFactory,
  i_MarkFactory,
  i_ValueToStringConverter,
  i_VectorItemTreeImporterList,
  i_VectorItemTreeExporterList,
  i_TileStorageTypeList,
  i_LastSearchResult,
  i_ImageResamplerFactory,
  i_BuildInfo,
  i_GlobalConfig,
  i_GlobalCacheConfig,
  u_GarbageCollectorThread,
  u_MapTypesMainList,
  u_IeEmbeddedProtocolRegistration;

type
  TGlobalState = class
  private
    FGlobalConfig: IGlobalConfig;
    FBaseConfigPath: IPathConfig;
    FBaseDataPath: IPathConfig;
    FBaseCahcePath: IPathConfig;
    FBaseApplicationPath: IPathConfig;

    FMainConfigProvider: IConfigDataWriteProvider;
    FZmpInfoSet: IZmpInfoSet;
    FHashFunction: IHashFunction;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FGeoCodePlacemarkFactory: IGeoCodePlacemarkFactory;
    FMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
    FMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
    FResourceProvider: IConfigDataProvider;
    FTileNameGenerator: ITileFileNameGeneratorsList;
    FTileNameParser: ITileFileNameParsersList;
    FGCThread: TGarbageCollectorThread;
    FContentTypeManager: IContentTypeManager;
    FMapCalibrationList: IMapCalibrationList;
    FCacheConfig: IGlobalCacheConfig;
    FMarkSystem: IMarkSystem;
    FDatumFactory: IDatumFactory;
    FCoordConverterFactory: ICoordConverterFactory;
    FCoordConverterList: ICoordConverterList;
    FProjConverterFactory: IProjConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FMainMapsList: TMapTypesMainList;
    FGPSPositionFactory: IGPSPositionFactory;
    FBitmapPostProcessing: IBitmapPostProcessingChangeable;
    FDownloadInfo: IDownloadInfoSimple;
    FGlobalInternetState: IGlobalInternetState;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FGeoCoderList: IGeoCoderListStatic;
    FMarkPictureList: IMarkPictureList;
    FGpsSystem: IGPSModule;
    FImporterList: IVectorItemTreeImporterListChangeable;
    FExporterList: IVectorItemTreeExporterListChangeable;
    FGPSDatum: IDatum;
    FGeoCalc: IGeoCalc;
    FGPSRecorder: IGPSRecorder;
    FGPSRecorderInternal: IGPSRecorderInternal;
    FGpsTrackRecorder: IGpsTrackRecorder;
    FGpsTrackRecorderInternal: IGpsTrackRecorderInternal;
    FSkyMapDraw: ISatellitesInViewMapDraw;
    FSystemTime: ISystemTimeProvider;
    FSystemTimeInternal: ISystemTimeProviderInternal;
    FBGTimerNotifier: INotifierTime;
    FBGTimerNotifierInternal: INotifierTimeInternal;
    FGUISyncronizedTimer: TTimer;
    FGUISyncronizedTimerNotifierInternal: INotifierTimeInternal;
    FGUISyncronizedTimerNotifier: INotifierTime;
    FGUISyncronizedTimerCounter: IInternalPerformanceCounter;
    FDebugInfoSubSystem: IDebugInfoSubSystem;
    FProtocol: TIeEmbeddedProtocolRegistration;
    FMapVersionFactoryList: IMapVersionFactoryList;
    FPathDetalizeList: IPathDetalizeProviderList;
    FInvisibleBrowser: IInvisibleBrowser;
    FInternalBrowser: IInternalBrowser;
    FDebugInfoWindow: IDebugInfoWindow;
    FAppStartedNotifier: INotifierOneOperation;
    FAppStartedNotifierInternal: INotifierOneOperationInternal;
    FAppClosingNotifier: INotifierOneOperation;
    FAppClosingNotifierInternal: INotifierOneOperationInternal;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FBitmapFactory: IBitmap32BufferFactory;
    FBatteryStatus: IBatteryStatus;
    FTerrainProviderList: ITerrainProviderList;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FLastSelectionSaver: IBackgroundTask;
    FMainThreadConfigListener: IListener;
    FVectorDataFactory: IVectorDataFactory;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
    FProjectedGeometryProvider: IGeometryProjectedProvider;
    FMarkFactory: IMarkFactory;
    FMarkCategoryFactory: IMarkCategoryFactory;
    FBuildInfo: IBuildInfo;
    FInternalBrowserContent: IInternalBrowserLastContent;
    FTileStorageTypeList: ITileStorageTypeListStatic;
    FLastSelectionInfo: ILastSelectionInfo;
    FImageResamplerFactoryList: IImageResamplerFactoryList;
    FLastSearchResult: ILastSearchResult;
    FValueToStringConverter: IValueToStringConverterChangeable;

    procedure OnMainThreadConfigChange;
    procedure InitProtocol;

    procedure OnGUISyncronizedTimer(Sender: TObject);
    function GetPerfCounterList: IInternalPerformanceCounterList;
    {$IFDEF SasDebugWithJcl}
    procedure DoException(
      Sender: TObject;
      E: Exception
    );
    {$ENDIF SasDebugWithJcl}
  public
    property Config: IGlobalConfig read FGlobalConfig;

    property MapType: TMapTypesMainList read FMainMapsList;
    property CacheConfig: IGlobalCacheConfig read FCacheConfig;
    property MarksDb: IMarkSystem read FMarkSystem;
    property GpsSystem: IGPSModule read FGpsSystem;
    property GPSDatum: IDatum read FGPSDatum;
    property GeoCalc: IGeoCalc read FGeoCalc;

    // Список генераторов имен файлов с тайлами
    property TileNameGenerator: ITileFileNameGeneratorsList read FTileNameGenerator;
    property TileNameParser: ITileFileNameParsersList read FTileNameParser;
    property ContentTypeManager: IContentTypeManager read FContentTypeManager;
    property DatumFactory: IDatumFactory read FDatumFactory;
    property CoordConverterFactory: ICoordConverterFactory read FCoordConverterFactory;
    property CoordConverterList: ICoordConverterList read FCoordConverterList;
    property ProjectionFactory: IProjectionInfoFactory read FProjectionFactory;
    property LocalConverterFactory: ILocalCoordConverterFactorySimpe read FLocalConverterFactory;
    property MapTypeSetBuilderFactory: IMapTypeSetBuilderFactory read FMapTypeSetBuilderFactory;
    property MapTypeListBuilderFactory: IMapTypeListBuilderFactory read FMapTypeListBuilderFactory;
    property MapCalibrationList: IMapCalibrationList read FMapCalibrationList;
    property AppStartedNotifier: INotifierOneOperation read FAppStartedNotifier;
    property AppClosingNotifier: INotifierOneOperation read FAppClosingNotifier;

    property HashFunction: IHashFunction read FHashFunction;
    property AppearanceOfMarkFactory: IAppearanceOfMarkFactory read FAppearanceOfMarkFactory;
    property MainConfigProvider: IConfigDataWriteProvider read FMainConfigProvider;
    property ResourceProvider: IConfigDataProvider read FResourceProvider;
    property DownloadInfo: IDownloadInfoSimple read FDownloadInfo;
    property GlobalInternetState: IGlobalInternetState read FGlobalInternetState;
    property ImporterList: IVectorItemTreeImporterListChangeable read FImporterList;
    property ExporterList: IVectorItemTreeExporterListChangeable read FExporterList;
    property SkyMapDraw: ISatellitesInViewMapDraw read FSkyMapDraw;
    property GUISyncronizedTimerNotifier: INotifierTime read FGUISyncronizedTimerNotifier;
    property BGTimerNotifier: INotifierTime read FBGTimerNotifier;
    property PerfCounterList: IInternalPerformanceCounterList read GetPerfCounterList;
    property SystemTime: ISystemTimeProvider read FSystemTime;

    property LastSelectionInfo: ILastSelectionInfo read FLastSelectionInfo;
    property BitmapPostProcessing: IBitmapPostProcessingChangeable read FBitmapPostProcessing;
    property GPSRecorder: IGPSRecorder read FGPSRecorder;
    property GpsTrackRecorder: IGpsTrackRecorder read FGpsTrackRecorder;
    property PathDetalizeList: IPathDetalizeProviderList read FPathDetalizeList;
    property InternalBrowser: IInternalBrowser read FInternalBrowser;
    property DebugInfoWindow: IDebugInfoWindow read FDebugInfoWindow;
    property VectorGeometryLonLatFactory: IGeometryLonLatFactory read FVectorGeometryLonLatFactory;
    property VectorGeometryProjectedFactory: IGeometryProjectedFactory read FVectorGeometryProjectedFactory;
    property BitmapFactory: IBitmap32BufferFactory read FBitmapFactory;
    property VectorDataFactory: IVectorDataFactory read FVectorDataFactory;
    property VectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory read FVectorDataItemMainInfoFactory;
    property ProjectedGeometryProvider: IGeometryProjectedProvider read FProjectedGeometryProvider;
    property VectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory read FVectorItemSubsetBuilderFactory;
    property BitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory read FBitmapTileSaveLoadFactory;
    property GeoCodePlacemarkFactory: IGeoCodePlacemarkFactory read FGeoCodePlacemarkFactory;
    property ArchiveReadWriteFactory: IArchiveReadWriteFactory read FArchiveReadWriteFactory;
    property TerrainProviderList: ITerrainProviderList read FTerrainProviderList;
    property GlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper read FGlobalBerkeleyDBHelper;
    property MarkPictureList: IMarkPictureList read FMarkPictureList;
    property MapVersionFactoryList: IMapVersionFactoryList read FMapVersionFactoryList;
    property BuildInfo: IBuildInfo read FBuildInfo;
    property TileStorageTypeList: ITileStorageTypeListStatic read FTileStorageTypeList;
    property ImageResamplerFactoryList: IImageResamplerFactoryList read FImageResamplerFactoryList;
    property LastSearchResult: ILastSearchResult read FLastSearchResult;
    property ValueToStringConverter: IValueToStringConverterChangeable read FValueToStringConverter;
    property GeoCoderList: IGeoCoderListStatic read FGeoCoderList;
    property DebugInfoSubSystem: IDebugInfoSubSystem read FDebugInfoSubSystem;
    property BatteryStatus: IBatteryStatus read FBatteryStatus;

    constructor Create;
    destructor Destroy; override;
    procedure LoadConfig;
    procedure SaveMainParams;
    procedure StartThreads;
    procedure SendTerminateToThreads;
    procedure SystemTimeChanged;

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
  c_CoordConverter,
  c_InternalBrowser,
  u_SASMainConfigProvider,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile,
  u_ConfigDataProviderByPathConfig,
  i_InternalDomainInfoProvider,
  i_TextByVectorItem,
  i_ImageResamplerFactoryChangeable,
  u_MapTypeSet,
  u_MapTypeListStatic,
  i_InternalDebugConfig,
  u_TextByVectorItemHTMLByDescription,
  u_NotifierTime,
  i_FileNameIterator,
  u_AppearanceOfMarkFactory,
  u_ContentTypeManagerSimple,
  u_MarkSystem,
  u_MapCalibrationListBasic,
  u_XmlInfoSimpleParser,
  u_CoordConverterFactorySimple,
  u_CoordConverterListStaticSimple,
  u_DownloadInfoSimple,
  u_DatumFactory,
  u_GeoCalc,
  u_HashFunctionCityHash,
  u_HashFunctionWithCounter,
  u_MapVersionFactoryList,
  u_GeoCoderListSimple,
  u_MarkPictureListSimple,
  u_ImageResamplerFactoryListStaticSimple,
  u_GlobalBerkeleyDBHelper,
  u_GPSRecorder,
  u_GpsTrackRecorder,
  u_SatellitesInViewMapDrawSimple,
  u_GPSModuleFactoryByVSAGPS,
  u_GPSPositionFactory,
  u_ProjectionInfoFactory,
  u_GeoCodePlacemark,
  u_LocalCoordConverterFactorySimpe,
  u_TerrainProviderList,
  u_ProjConverterFactory,
  u_PathConfig,
  u_BatteryStatus,
  u_ZmpInfoSet,
  u_ZmpFileNamesIteratorFactory,
  u_HtmlToHintTextConverterStuped,
  u_InvisibleBrowserByFormSynchronize,
  u_InternalBrowserByForm,
  u_DebugInfoWindow,
  u_IeEmbeddedProtocolFactory,
  u_GeometryLonLatFactory,
  u_VectorDataFactorySimple,
  u_GeometryProjectedFactory,
  u_DownloadResultFactory,
  u_PathDetalizeProviderListSimple,
  u_InternalDomainInfoProviderList,
  u_InternalDomainInfoProviderByMapTypeList,
  u_InternalDomainInfoProviderByDataProvider,
  u_InternalDomainInfoProviderByMarksSystem,
  u_InternalDomainInfoProviderByMapData,
  u_InternalDomainInfoProviderByLastSearchResults,
  u_InternalDomainInfoProviderByLastContent,
  u_InternalDomainInfoProviderByTileStorageOptions,
  u_Bitmap32BufferFactory,
  u_VectorItemSubsetBuilder,
  u_GpsSystem,
  u_LastSelectionInfoSaver,
  u_ListenerByEvent,
  u_Synchronizer,
  u_GlobalConfig,
  u_GlobalInternetState,
  u_GlobalCacheConfig,
  u_InternalDebugConfig,
  u_MarkFactory,
  u_MarkCategoryFactory,
  u_GeometryProjectedProvider,
  u_SystemTimeProvider,
  u_BitmapTileSaveLoadFactory,
  u_ArchiveReadWriteFactory,
  u_DebugInfoSubSystem,
  u_LastSelectionInfo,
  u_LocalCoordConverterFactory,
  u_LastSearchResult,
  u_ImageResamplerFactoryChangeableByConfig,
  u_BuildInfo,
  u_VectorItemTreeExporterListSimple,
  u_VectorItemTreeImporterListSimple,
  u_BitmapPostProcessingChangeableByConfig,
  u_ValueToStringConverter,
  u_InternalBrowserLastContent,
  u_TileStorageTypeListSimple,
  u_TileFileNameParsersSimpleList,
  u_TileFileNameGeneratorsSimpleList;

{ TGlobalState }

constructor TGlobalState.Create;
var
  VViewCnonfig: IConfigDataProvider;
  VKmlLoader: IVectorDataLoader;
  VFilesIteratorFactory: IFileNameIteratorFactory;
  VFilesIterator: IFileNameIterator;
  VProgramPath: string;
  VSleepByClass: IConfigDataProvider;
  VResamplerFactoryList: IImageResamplerFactoryList;
  VInternalDebugConfig: IInternalDebugConfig;
  VTileLoadResampler: IImageResamplerFactoryChangeable;
  VTileGetPrevResampler: IImageResamplerFactoryChangeable;
  VTileReprojectResampler: IImageResamplerFactoryChangeable;
  VTileDownloadResampler: IImageResamplerFactoryChangeable;
  VNotifierSync: IReadWriteSync;
  VOneOperationSync: IReadWriteSync;
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

  FBuildInfo := TBuildInfo.Create;

  VInternalDebugConfig := TInternalDebugConfig.Create;

  FMainConfigProvider :=
    TSASMainConfigProvider.Create(
      FBaseConfigPath.FullPath,
      ExtractFileName(ParamStr(0)),
      HInstance
    );

  VInternalDebugConfig.ReadConfig(FMainConfigProvider.GetSubItem('Debug'));

  FDebugInfoSubSystem := TDebugInfoSubSystem.Create(VInternalDebugConfig);

  FHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      FDebugInfoSubSystem.RootCounterList.CreateAndAddNewSubList('HashFunction')
    );

  FImageResamplerFactoryList := TImageResamplerFactoryListStaticSimple.Create;
  FMapVersionFactoryList := TMapVersionFactoryList.Create(FHashFunction);

  FAppearanceOfMarkFactory := TAppearanceOfMarkFactory.Create(FHashFunction);
  FInternalBrowserContent := TInternalBrowserLastContent.Create;


  FGlobalConfig :=
    TGlobalConfig.Create(
      VInternalDebugConfig,
      FAppearanceOfMarkFactory,
      FBaseCahcePath,
      FBaseConfigPath,
      FBaseDataPath,
      FBaseApplicationPath
    );

  FGlobalConfig.ReadConfig(FMainConfigProvider);

  FVectorItemSubsetBuilderFactory :=
    TVectorItemSubsetBuilderFactory.Create(
      FHashFunction
    );
  FBGTimerNotifierInternal :=
    TNotifierTime.Create(
      GSync.SyncVariable.Make(Self.ClassName + 'BGTimerNotifier')
    );
  FBGTimerNotifier := FBGTimerNotifierInternal;
  FBitmapFactory :=
    TBitmap32BufferFactory.Create(
      FBGTimerNotifier,
      GSync.SyncVariable.Make(Self.ClassName)
    );
  FSystemTimeInternal := TSystemTimeProvider.Create;
  FSystemTime := FSystemTimeInternal;

  FBitmapTileSaveLoadFactory := TBitmapTileSaveLoadFactory.Create(FBitmapFactory);
  FArchiveReadWriteFactory := TArchiveReadWriteFactory.Create;

  VNotifierSync := GSync.SyncVariable.Make(Self.ClassName + 'Notifiers');
  VOneOperationSync := GSync.SyncVariable.Make(Self.ClassName + 'OneOperation');

  FAppStartedNotifierInternal :=
    TNotifierOneOperation.Create(
      VOneOperationSync,
      TNotifierBase.Create(VNotifierSync)
    );
  FAppStartedNotifier := FAppStartedNotifierInternal;
  FAppClosingNotifierInternal :=
    TNotifierOneOperation.Create(
      VOneOperationSync,
      TNotifierBase.Create(VNotifierSync)
    );
  FAppClosingNotifier := FAppClosingNotifierInternal;

  VSleepByClass := FMainConfigProvider.GetSubItem('SleepByClass');

  FResourceProvider := FMainConfigProvider.GetSubItem('sas:\Resource');
  FVectorGeometryProjectedFactory := TGeometryProjectedFactory.Create;
  FVectorGeometryLonLatFactory := TGeometryLonLatFactory.Create(FHashFunction);

  FGlobalInternetState := TGlobalInternetState.Create;

  FProjConverterFactory := TProjConverterFactory.Create;
  FLastSelectionInfo := TLastSelectionInfo.Create;
  FLastSearchResult := TLastSearchResult.Create;

  FDatumFactory := TDatumFactory.Create(FHashFunction);
  FCoordConverterFactory := TCoordConverterFactorySimple.Create(FHashFunction, FDatumFactory);
  FProjectionFactory := TProjectionInfoFactory.Create(FHashFunction, GSync.SyncVariable.Make(Self.ClassName));
  FCoordConverterList := TCoordConverterListStaticSimple.Create(FCoordConverterFactory);
  FLocalConverterFactory :=
    TLocalCoordConverterFactorySimpe.Create(
      TLocalCoordConverterFactory.Create(FHashFunction),
      FProjectionFactory
    );

  FCacheConfig := TGlobalCacheConfig.Create(FBaseCahcePath);
  FDownloadInfo := TDownloadInfoSimple.Create(nil);
  VViewCnonfig := FMainConfigProvider.GetSubItem('VIEW');

  FGUISyncronizedTimer := TTimer.Create(nil);
  FGUISyncronizedTimer.Enabled := False;
  FGUISyncronizedTimer.Interval := VSleepByClass.ReadInteger('GUISyncronizedTimer', 16);
  FGUISyncronizedTimer.OnTimer := Self.OnGUISyncronizedTimer;

  FGUISyncronizedTimerNotifierInternal :=
    TNotifierTime.Create(
      GSync.SyncVariable.Make(Self.ClassName + 'GUITimerNotifier')
    );
  FGUISyncronizedTimerNotifier := FGUISyncronizedTimerNotifierInternal;
  FGUISyncronizedTimerCounter := FDebugInfoSubSystem.RootCounterList.CreateAndAddNewCounter('GUITimer');

  FGlobalBerkeleyDBHelper := TGlobalBerkeleyDBHelper.Create(FBaseApplicationPath);

  FTerrainProviderList :=
    TTerrainProviderListSimple.Create(
      FProjConverterFactory,
      FCoordConverterFactory,
      FGlobalConfig.TerrainDataPath,
      FCacheConfig.GECachePath,
      FCacheConfig.GCCachePath
    );

  FMainThreadConfigListener := TNotifyEventListenerSync.Create(FGUISyncronizedTimerNotifier, 1000, Self.OnMainThreadConfigChange);
  FGlobalConfig.MainThreadConfig.ChangeNotifier.Add(FMainThreadConfigListener);
  OnMainThreadConfigChange;

  FGPSDatum := FDatumFactory.GetByCode(CYandexDatumEPSG);
  FGeoCalc := TGeoCalc.Create(FGPSDatum);

  VResamplerFactoryList := TImageResamplerFactoryListStaticSimple.Create;

  FGPSPositionFactory := TGPSPositionFactory.Create;
  FGPSRecorderInternal :=
    TGPSRecorder.Create(
      FGPSDatum,
      FGlobalConfig.GpsRecorderFileName,
      FGPSPositionFactory.BuildPositionEmpty
    );
  FGPSRecorder := FGPSRecorderInternal;

  FGpsTrackRecorderInternal :=
    TGpsTrackRecorder.Create(
      FVectorGeometryLonLatFactory,
      FGlobalConfig.GpsTrackRecorderFileName
    );
  FGpsTrackRecorder := FGpsTrackRecorderInternal;

  FTileNameGenerator := TTileFileNameGeneratorsSimpleList.Create;
  FTileNameParser := TTileFileNameParsersSimpleList.Create;

  FVectorDataItemMainInfoFactory := TVectorDataItemMainInfoFactory.Create(FHashFunction, THtmlToHintTextConverterStuped.Create);
  FVectorDataFactory := TVectorDataFactorySimple.Create(FHashFunction);

  FContentTypeManager :=
    TContentTypeManagerSimple.Create(
      FVectorGeometryLonLatFactory,
      FVectorDataFactory,
      FVectorItemSubsetBuilderFactory,
      FBitmapTileSaveLoadFactory,
      FArchiveReadWriteFactory,
      FDebugInfoSubSystem.RootCounterList.CreateAndAddNewSubList('Content')
    );

  FMapCalibrationList := TMapCalibrationListBasic.Create;
  FProjectedGeometryProvider :=
    TGeometryProjectedProvider.Create(
      FHashFunction,
      FVectorGeometryProjectedFactory
    );

  FValueToStringConverter :=
    TValueToStringConverterChangeable.Create(
      FGlobalConfig.ValueToStringConverterConfig,
      FGlobalConfig.LanguageManager.ChangeNotifier
    );

  FGCThread :=
    TGarbageCollectorThread.Create(
      FAppClosingNotifier,
      FDebugInfoSubSystem.RootCounterList.CreateAndAddNewCounter('GCTimer'),
      FBGTimerNotifierInternal,
      VSleepByClass.ReadInteger(TGarbageCollectorThread.ClassName, 1000)
    );
  FBitmapPostProcessing :=
    TBitmapPostProcessingChangeableByConfig.Create(
      FGlobalConfig.BitmapPostProcessingConfig,
      FBitmapFactory
    );
  FGpsSystem :=
    TGpsSystem.Create(
      FAppStartedNotifier,
      FAppClosingNotifier,
      TGPSModuleFactoryByVSAGPS.Create(FSystemTime, FGPSPositionFactory),
      FGlobalConfig.GPSConfig,
      FGPSRecorderInternal,
      FGpsTrackRecorderInternal,
      GUISyncronizedTimerNotifier,
      FDebugInfoSubSystem.RootCounterList
    );
  FGeoCodePlacemarkFactory :=
    TGeoCodePlacemarkFactory.Create(
      FVectorGeometryLonLatFactory,
      FHashFunction
    );
  FMarkPictureList :=
    TMarkPictureListSimple.Create(
      FHashFunction,
      FGlobalConfig.MarksIconsPath,
      FContentTypeManager
    );
  FMarkCategoryFactory :=
    TMarkCategoryFactory.Create(
      FGlobalConfig.MarksCategoryFactoryConfig
    );
  FMarkFactory :=
    TMarkFactory.Create(
      FGlobalConfig.MarksFactoryConfig,
      FMarkPictureList,
      FHashFunction,
      FAppearanceOfMarkFactory,
      THtmlToHintTextConverterStuped.Create
    );
  FMarkSystem :=
    TMarkSystem.Create(
      FGlobalConfig.MarksDbPath,
      FMarkPictureList,
      FMarkFactory,
      FMarkCategoryFactory,
      FHashFunction,
      FAppearanceOfMarkFactory,
      FVectorGeometryLonLatFactory,
      FVectorItemSubsetBuilderFactory,
      FDebugInfoSubSystem.RootCounterList.CreateAndAddNewSubList('MarksSystem'),
      FAppStartedNotifier,
      THtmlToHintTextConverterStuped.Create
    );

  FImporterList :=
    TVectorItemTreeImporterListSimple.Create(
      FValueToStringConverter,
      FVectorDataFactory,
      FVectorDataItemMainInfoFactory,
      FVectorGeometryLonLatFactory,
      FVectorItemSubsetBuilderFactory,
      FArchiveReadWriteFactory,
      FMarkPictureList,
      FHashFunction,
      FAppearanceOfMarkFactory,
      FMarkFactory,
      THtmlToHintTextConverterStuped.Create,
      FGlobalConfig.MediaDataPath,
      FDebugInfoSubSystem.RootCounterList.CreateAndAddNewSubList('Import')
    );

  FExporterList :=
    TVectorItemTreeExporterListSimple.Create(
      FArchiveReadWriteFactory,
      FMarkPictureList,
      FHashFunction,
      FAppearanceOfMarkFactory,
      FVectorGeometryLonLatFactory,
      FVectorItemSubsetBuilderFactory,
      FMarkFactory,
      FMarkCategoryFactory,
      THtmlToHintTextConverterStuped.Create,
      FDebugInfoSubSystem.RootCounterList.CreateAndAddNewSubList('Export')
    );

  FGeoCoderList :=
    TGeoCoderListSimple.Create(
      FGlobalConfig.InetConfig,
      BGTimerNotifier,
      FVectorItemSubsetBuilderFactory,
      FGeoCodePlacemarkFactory,
      TDownloadResultFactory.Create,
      FValueToStringConverter,
      FMarkSystem.MarkDb
    );
  VFilesIteratorFactory := TZmpFileNamesIteratorFactory.Create;
  VFilesIterator := VFilesIteratorFactory.CreateIterator(FGlobalConfig.MapsPath.FullPath, '');
  FZmpInfoSet :=
    TZmpInfoSet.Create(
      FGlobalConfig.ZmpConfig,
      FCoordConverterFactory,
      FArchiveReadWriteFactory,
      FContentTypeManager,
      FMapVersionFactoryList.GetSimpleVersionFactory,
      FBitmapFactory,
      FGlobalConfig.LanguageManager,
      VFilesIterator
    );

  FMapTypeSetBuilderFactory := TMapTypeSetBuilderFactory.Create(FHashFunction);
  FMapTypeListBuilderFactory := TMapTypeListBuilderFactory.Create(FHashFunction);
  VTileLoadResampler :=
    TImageResamplerFactoryChangeableByConfig.Create(
      FGlobalConfig.TileLoadResamplerConfig,
      FImageResamplerFactoryList
    );
  VTileGetPrevResampler :=
    TImageResamplerFactoryChangeableByConfig.Create(
      FGlobalConfig.TileGetPrevResamplerConfig,
      FImageResamplerFactoryList
    );
  VTileReprojectResampler :=
    TImageResamplerFactoryChangeableByConfig.Create(
      FGlobalConfig.TileReprojectResamplerConfig,
      FImageResamplerFactoryList
    );
  VTileDownloadResampler :=
    TImageResamplerFactoryChangeableByConfig.Create(
      FGlobalConfig.TileDownloadResamplerConfig,
      FImageResamplerFactoryList
    );

  FMainMapsList :=
    TMapTypesMainList.Create(
      FMapTypeSetBuilderFactory,
      FZmpInfoSet,
      VTileLoadResampler,
      VTileGetPrevResampler,
      VTileReprojectResampler,
      VTileDownloadResampler,
      FDebugInfoSubSystem.RootCounterList.CreateAndAddNewSubList('MapType')
    );
  FSkyMapDraw := TSatellitesInViewMapDrawSimple.Create;

  VKmlLoader :=
    TXmlInfoSimpleParser.Create(
      FVectorGeometryLonLatFactory,
      FVectorDataFactory,
      FVectorItemSubsetBuilderFactory,
      True
    );
  FPathDetalizeList :=
    TPathDetalizeProviderListSimple.Create(
      FGlobalConfig.LanguageManager,
      FGlobalConfig.InetConfig,
      FBGTimerNotifier,
      TDownloadResultFactory.Create,
      FVectorDataItemMainInfoFactory,
      FVectorGeometryLonLatFactory,
      VKmlLoader
    );

  InitProtocol;

  FInvisibleBrowser :=
    TInvisibleBrowserByFormSynchronize.Create(
      FGlobalConfig.LanguageManager,
      FGlobalConfig.InetConfig.ProxyConfig
    );
  FInternalBrowser :=
    TInternalBrowserByForm.Create(
      FGlobalConfig.LanguageManager,
      FInternalBrowserContent,
      FGlobalConfig.InternalBrowserConfig,
      FGlobalConfig.InetConfig.ProxyConfig,
      FContentTypeManager
    );
  FDebugInfoWindow :=
    TDebugInfoWindow.Create(
      FGlobalConfig.InternalDebugConfig,
      FDebugInfoSubSystem
    );
  FBatteryStatus := TBatteryStatus.Create;
  FLastSelectionSaver :=
    TLastSelectionInfoSaver.Create(
      FAppClosingNotifier,
      FVectorGeometryLonLatFactory,
      FLastSelectionInfo,
      FGlobalConfig.LastSelectionFileName
    );
  FTileStorageTypeList :=
    TTileStorageTypeListSimple.Create(
      FMapVersionFactoryList,
      FContentTypeManager,
      FCacheConfig,
      FGlobalBerkeleyDBHelper,
      FBGTimerNotifier
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
  FMarkSystem := nil;
  FGPSRecorder := nil;
  FreeAndNil(FMainMapsList);
  FCoordConverterFactory := nil;
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
  FProjConverterFactory := nil;
  FGlobalBerkeleyDBHelper := nil;
  inherited;
end;

function TGlobalState.GetPerfCounterList: IInternalPerformanceCounterList;
begin
  Result := FDebugInfoSubSystem.RootCounterList;
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
  VTextProivder := TTextByVectorItemHTMLByDescription.Create;

  VTextProviderList.AddObject(CVectorItemInfoSuffix, Pointer(VTextProivder));
  VTextProivder._AddRef;

  VTextProviderList.AddObject(CVectorItemDescriptionSuffix, Pointer(VTextProivder));
  VTextProivder._AddRef;

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByMarksSystem.Create(
      FMarkSystem,
      VTextProivder,
      VTextProviderList
    );
  VInternalDomainInfoProviderList.Add(
    CMarksSystemInternalDomain,
    VInternalDomainInfoProvider
  );

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByLastSearchResults.Create(
      FLastSearchResult,
      VTextProivder,
      nil
    );
  VInternalDomainInfoProviderList.Add(
    CLastSearchResultsInternalDomain,
    VInternalDomainInfoProvider
  );

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByLastContent.Create(
      FInternalBrowserContent
    );
  VInternalDomainInfoProviderList.Add(
    CShowMessageDomain,
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
    FInvisibleBrowser.NavigateAndWait('http://sasgis.org/stat/index.html');
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

procedure TGlobalState.SystemTimeChanged;
begin
  FSystemTimeInternal.SystemTimeChanged;
end;

procedure TGlobalState.LoadConfig;
var
  VLocalMapsConfig: IConfigDataProvider;
  VIniFile: TMeminifile;
  VMapsPath: String;
begin
  VMapsPath := IncludeTrailingPathDelimiter(FGlobalConfig.MapsPath.FullPath);
  ForceDirectories(VMapsPath);
  VIniFile := TMeminiFile.Create(VMapsPath + 'Maps.ini');
  try
    VLocalMapsConfig := TConfigDataProviderByIniFile.CreateWithOwn(VIniFile);
    VIniFile := nil;
  finally
    VIniFile.Free;
  end;

  FCacheConfig.ReadConfig(FMainConfigProvider);

  FMainMapsList.LoadMaps(
    FGlobalConfig.LanguageManager,
    FMapVersionFactoryList,
    FGlobalConfig.MainMemCacheConfig,
    FCacheConfig,
    FTileStorageTypeList,
    FHashFunction,
    FBGTimerNotifier,
    FAppClosingNotifier,
    FGlobalConfig.InetConfig,
    FGlobalConfig.DownloadConfig,
    FGlobalConfig.DownloaderThreadConfig,
    FBitmapFactory,
    FContentTypeManager,
    FCoordConverterFactory,
    FInvisibleBrowser,
    FProjConverterFactory,
    VLocalMapsConfig
  );

  FGPSRecorderInternal.Load;
  FGpsTrackRecorderInternal.Load;

  if (not ModuleIsLib) then begin
    FMarkPictureList.LoadList;
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
  SetThreadPriority(GetCurrentThread(), Priorities[FGlobalConfig.MainThreadConfig.Priority]);
end;

procedure TGlobalState.SaveMainParams;
var
  VIniFile: TMeminifile;
  VLocalMapsConfig: IConfigDataWriteProvider;
  VMapsPath: String;
begin
  if ModuleIsLib then begin
    Exit;
  end;
  VMapsPath := IncludeTrailingPathDelimiter(FGlobalConfig.MapsPath.FullPath);
  VIniFile := TMeminiFile.Create(VMapsPath + 'Maps.ini');
  try
    VLocalMapsConfig := TConfigDataWriteProviderByIniFile.CreateWithOwn(VIniFile);
    VIniFile := nil;
  finally
    VIniFile.Free;
  end;
  FMainMapsList.SaveMaps(VLocalMapsConfig);

  FGPSRecorderInternal.Save;
  FGpsTrackRecorderInternal.Save;
  FCacheConfig.WriteConfig(FMainConfigProvider);
  FGlobalConfig.WriteConfig(MainConfigProvider);
end;

procedure TGlobalState.SendTerminateToThreads;
begin
  if FGlobalConfig.MainThreadConfig <> nil then begin
    FGlobalConfig.MainThreadConfig.ChangeNotifier.Remove(FMainThreadConfigListener);
  end;

  FGUISyncronizedTimer.Enabled := False;
  FAppClosingNotifierInternal.ExecuteOperation;
  FGCThread.Terminate;
end;

end.


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
  i_ContentTypeManager,
  i_VectorDataLoader,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_ProxySettings,
  i_GSMGeoCodeConfig,
  i_MainFormConfig,
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
  i_ZmpConfig,
  i_ZmpInfoSet,
  i_GPSConfig,
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
    FMainConfigProvider: IConfigDataWriteProvider;
    FZmpConfig: IZmpConfig;
    FZmpInfoSet: IZmpInfoSet;
    FResourceProvider: IConfigDataProvider;
    FGlobalAppConfig: IGlobalAppConfig;
    FStartUpLogoConfig: IStartUpLogoConfig;
    FTileNameGenerator: ITileFileNameGeneratorsList;
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
    FGlobalInternetState: IGlobalInternetState;
    FProgramPath: string;
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
    FInternalBrowser: IInternalBrowser;
    FDebugInfoWindow: IDebugInfoWindow;
    FAppClosingNotifier: IJclNotifier;
    FTimeZoneDiffByLonLat: ITimeZoneDiffByLonLat;
    FVectorItmesFactory: IVectorItmesFactory;

    procedure OnGUISyncronizedTimer(Sender: TObject);
    function InternalGetPrimaryPath(const ASection: String; out APrimaryPathValue: String): Boolean;
    function GetMarkIconsPath: string;
    function GetMapsPath: string;
    function GetTrackLogPath: string;
    function GetMarksSystemPath: String;
    function GetMediaDataPath: string;
    {$IFDEF SasDebugWithJcl}
    procedure DoException(Sender: TObject; E: Exception);
    {$ENDIF SasDebugWithJcl}
    // Путь к папке с картами
    //property MapsPath: string read GetMapsPath;
  public
    property MapType: TMapTypesMainList read FMainMapsList;
    property CacheConfig: TGlobalCahceConfig read FCacheConfig;
    property GCThread: TGarbageCollectorThread read FGCThread;
    property MarksDB: TMarksSystem read FMarksDB;
    property GPSpar: TGPSpar read FGPSpar;
    property ProgramPath: string read FProgramPath;

    // Список генераторов имен файлов с тайлами
    property TileNameGenerator: ITileFileNameGeneratorsList read FTileNameGenerator;
    property ContentTypeManager: IContentTypeManager read FContentTypeManager;
    property CoordConverterFactory: ICoordConverterFactory read FCoordConverterFactory;
    property ProjectionFactory: IProjectionInfoFactory read FProjectionFactory;
    property LocalConverterFactory: ILocalCoordConverterFactorySimpe read FLocalConverterFactory;
    property MapCalibrationList: IMapCalibrationList read FMapCalibrationList;
    property AppClosingNotifier: IJclNotifier read FAppClosingNotifier;

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
  u_SASMainConfigProvider,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile,
  u_ConfigDataProviderByFolder,
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
  u_GPSRecorderStuped,
  u_SatellitesInViewMapDrawSimple,
  u_GPSModuleFactoryByVSAGPS,
  u_GPSPositionFactory,
  u_LocalCoordConverterFactorySimpe,
  u_LayerBitmapClearStrategyFactory,
  u_DownloadResultTextProvider,
  u_TimeZoneDiffByLonLatStuped,
  u_MainFormConfig,
  u_ZmpConfig,
  u_ZmpInfoSet,
  u_ZmpFileNamesIteratorFactory,
  u_SensorListStuped,
  u_HtmlToHintTextConverterStuped,
  u_InvisibleBrowserByFormSynchronize,
  u_InternalBrowserByForm,
  u_DebugInfoWindow,
  u_InternalPerformanceCounterList,
  u_IeEmbeddedProtocolFactory,
  u_VectorItmesFactorySimple,
  u_VectorDataFactorySimple,
  u_PathDetalizeProviderListSimple,
  u_InternalDomainInfoProviderList,
  u_InternalDomainInfoProviderByMapTypeList,
  u_InternalDomainInfoProviderByDataProvider,
  u_GlobalInternetState,
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
begin
  if ModuleIsLib then begin
    // run as DLL or PACKAGE
    FProgramPath := GetModuleName(HInstance);
    FProgramPath := ExtractFilePath(FProgramPath);
  end else begin
    // run as EXE
    FProgramPath := ExtractFilePath(ParamStr(0));
  end;
  FAppClosingNotifier := TJclBaseNotifier.Create;
  FMainConfigProvider := TSASMainConfigProvider.Create(FProgramPath, ExtractFileName(ParamStr(0)), HInstance);
  FResourceProvider := FMainConfigProvider.GetSubItem('sas:\Resource');
  FVectorItmesFactory := TVectorItmesFactorySimple.Create;
  FGUISyncronizedTimer := TTimer.Create(nil);
  FGUISyncronizedTimer.Enabled := False;
  FGUISyncronizedTimer.Interval := 500;
  FGUISyncronizedTimer.OnTimer := Self.OnGUISyncronizedTimer;

  FPerfCounterList := TInternalPerformanceCounterList.Create('Main');

  FGUISyncronizedTimerNotifier := TJclBaseNotifier.Create;

  FGlobalAppConfig := TGlobalAppConfig.Create;
  FGlobalInternetState := TGlobalInternetState.Create;

  VCoordConverterFactorySimple := TCoordConverterFactorySimple.Create;
  FCoordConverterFactory := VCoordConverterFactorySimple;
  FProjectionFactory := VCoordConverterFactorySimple;
  FLocalConverterFactory := TLocalCoordConverterFactorySimpe.Create(FProjectionFactory);

  FTimeZoneDiffByLonLat := TTimeZoneDiffByLonLatStuped.Create(FVectorItmesFactory);

  FCacheConfig := TGlobalCahceConfig.Create(ProgramPath);
  FDownloadInfo := TDownloadInfoSimple.Create(nil);
  VViewCnonfig := FMainConfigProvider.GetSubItem('VIEW');
  FLanguageManager := TLanguageManager.Create;
  FLanguageManager.ReadConfig(VViewCnonfig);
  if VViewCnonfig <> nil then begin
    FGlobalAppConfig.ReadConfig(VViewCnonfig);
  end;
  FDownloadConfig := TGlobalDownloadConfig.Create;
  FImageResamplerConfig :=
    TImageResamplerConfig.Create(
      TImageResamplerFactoryListStaticSimple.Create
    );

  FClearStrategyFactory := TLayerBitmapClearStrategyFactory.Create(FImageResamplerConfig, FPerfCounterList.CreateAndAddNewSubList('ClearStrategy'));

  FInetConfig := TInetConfig.Create;
  FGPSConfig := TGPSConfig.Create(GetTrackLogPath);
  FGPSPositionFactory := TGPSPositionFactory.Create;
  FGPSRecorder :=
    TGPSRecorderStuped.Create(
      FVectorItmesFactory,
      TDatum.Create(3395, 6378137, 6356752),
      FGPSPositionFactory
    );
  FGSMpar := TGSMGeoCodeConfig.Create;
  FMainMemCacheConfig := TMainMemCacheConfig.Create;
  FViewConfig := TGlobalViewMainConfig.Create;

  FTileNameGenerator := TTileFileNameGeneratorsSimpleList.Create(FCacheConfig);
  FContentTypeManager :=
    TContentTypeManagerSimple.Create(
      FVectorItmesFactory,
      THtmlToHintTextConverterStuped.Create,
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
  FGCThread := TGarbageCollectorThread.Create(VList, 1000);
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
  FGeoCoderList := TGeoCoderListSimple.Create(FInetConfig.ProxyConfig as IProxySettings);
  FMarkPictureList := TMarkPictureListSimple.Create(GetMarkIconsPath, FContentTypeManager);
  FMarksCategoryFactoryConfig := TMarkCategoryFactoryConfig.Create(FLanguageManager);
  FMarksDB :=
    TMarksSystem.Create(
      FLanguageManager,
      GetMarksSystemPath,
      FMarkPictureList,
      FVectorItmesFactory,
      THtmlToHintTextConverterStuped.Create,
      FMarksCategoryFactoryConfig
    );
  VFilesIteratorFactory := TZmpFileNamesIteratorFactory.Create;
  VFilesIterator := VFilesIteratorFactory.CreateIterator(GetMapsPath, '');
  FZmpConfig := TZmpConfig.Create;
  FZmpConfig.ReadConfig(FMainConfigProvider.GetSubItem('ZmpDefaultParams'));
  FZmpInfoSet :=
    TZmpInfoSet.Create(
      FZmpConfig,
      FCoordConverterFactory,
      FLanguageManager,
      VFilesIterator
    );
  FMainMapsList := TMapTypesMainList.Create(FZmpInfoSet);
  FSkyMapDraw := TSatellitesInViewMapDrawSimple.Create;
  FDownloadResultTextProvider := TDownloadResultTextProvider.Create(FLanguageManager);
  FPathDetalizeList :=
    TPathDetalizeProviderListSimple.Create(
      FLanguageManager,
      FInetConfig.ProxyConfig,
      TVectorDataFactorySimple.Create(THtmlToHintTextConverterStuped.Create),
      FVectorItmesFactory,
      VKmlLoader
    );
  VInternalDomainInfoProviderList := TInternalDomainInfoProviderList.Create;
  VInternalDomainInfoProviderList.Add(
    'ZmpInfo',
    TInternalDomainInfoProviderByMapTypeList.Create(FZmpInfoSet, FContentTypeManager)
  );
  VInternalDomainInfoProviderList.Add(
    'MediaData',
    TInternalDomainInfoProviderByDataProvider.Create(
      TConfigDataProviderByFolder.Create(GetMediaDataPath),
      FContentTypeManager
    )
  );


  FProtocol := TIeEmbeddedProtocolRegistration.Create('sas', TIeEmbeddedProtocolFactory.Create(VInternalDomainInfoProviderList));
  FInvisibleBrowser :=
    TInvisibleBrowserByFormSynchronize.Create(
      FLanguageManager,
      FInetConfig.ProxyConfig
    );
  FInternalBrowser :=
    TInternalBrowserByForm.Create(
      FLanguageManager,
      FInetConfig.ProxyConfig
    );
  FDebugInfoWindow :=
    TDebugInfoWindow.Create(
      FGlobalAppConfig,
      FPerfCounterList
    );
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
procedure TGlobalState.DoException(Sender: TObject; E: Exception);
var
  Str: TStringList;
begin
  Str := TStringList.Create;
  try
    JclLastExceptStackListToStrings(Str, True, True, True, True);
    Str.Insert(0, E.Message);
    Str.Insert(1, '');
    Application.MessageBox(PChar(Str.Text), 'Ошибка', MB_OK or MB_ICONSTOP);
  finally
    FreeAndNil(Str);
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

function TGlobalState.GetMarkIconsPath: string;
begin
  Result := FProgramPath + 'marksicons' + PathDelim;
end;

function TGlobalState.GetMarksSystemPath: String;
begin
  if not InternalGetPrimaryPath('PATHtoMARKS', Result) then
    Result := FProgramPath;
end;

function TGlobalState.GetMediaDataPath: string;
begin
  if not InternalGetPrimaryPath('PATHtoMediaData', Result) then
    Result := FProgramPath + 'MediaData' + PathDelim;
end;

function TGlobalState.GetMapsPath: string;
begin
  if not InternalGetPrimaryPath('PATHtoMAPS', Result) then
    Result := FProgramPath + 'Maps' + PathDelim;
end;

function TGlobalState.GetTrackLogPath: string;
begin
  if not InternalGetPrimaryPath('PATHtoTRACKS', Result) then
    Result := FProgramPath + 'TrackLog' + PathDelim;
end;

function TGlobalState.InternalGetPrimaryPath(const ASection: String; out APrimaryPathValue: String): Boolean;
var
  VConfig: IConfigDataProvider;
  VValue: String;
begin
  Result:=FALSE;
  APrimaryPathValue:='';
  VConfig:=FMainConfigProvider.GetSubItem(ASection);
  if Assigned(VConfig) then begin
    VValue:=VConfig.ReadString('PrimaryPath', '');
    if (0<Length(VValue)) then begin
      APrimaryPathValue:=ExpandFileName(VValue);
      if (0<Length(APrimaryPathValue)) then begin
        if (PathDelim<>APrimaryPathValue[Length(APrimaryPathValue)]) then
          APrimaryPathValue:=APrimaryPathValue+PathDelim;
        Inc(Result);
      end;
    end;
  end;
end;

procedure TGlobalState.LoadConfig;
var
  VLocalMapsConfig: IConfigDataProvider;
  Ini: TMeminifile;
  VMapsPath: String;
begin
  VMapsPath:=GetMapsPath;
  ForceDirectories(VMapsPath);
  Ini := TMeminiFile.Create(VMapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataProviderByIniFile.Create(Ini);

  FCacheConfig.LoadConfig(FMainConfigProvider);

  FMainMapsList.LoadMaps(
    FLanguageManager,
    FMainMemCacheConfig,
    FCacheConfig,
    FTileNameGenerator,
    FGCThread.List,
    FAppClosingNotifier,
    FInetConfig,
    FImageResamplerConfig,
    FDownloadConfig,
    FContentTypeManager,
    FDownloadResultTextProvider,
    FCoordConverterFactory,
    FInvisibleBrowser,
    VLocalMapsConfig
  );
  FMainFormConfig := TMainFormConfig.Create(
    FLocalConverterFactory,
    FContentTypeManager,
    FGeoCoderList,
    FMainMapsList.MapsSet,
    FMainMapsList.LayersSet,
    FMainMapsList.FirstMainMapGUID,
    FPerfCounterList.CreateAndAddNewSubList('ViewState')
  );

  FSensorList := TSensorListStuped.Create(
      FLanguageManager,
      FMainFormConfig.ViewPortState,
      FMainFormConfig.NavToPoint,
      FGPSRecorder,
      FValueToStringConverterConfig
    );

  FViewConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
  FGPSRecorder.ReadConfig(MainConfigProvider.GetSubItem('GPS'));
  FGPSConfig.ReadConfig(MainConfigProvider.GetSubItem('GPS'));
  FInetConfig.ReadConfig(MainConfigProvider.GetSubItem('Internet'));
  FDownloadConfig.ReadConfig(MainConfigProvider.GetSubItem('Internet'));
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
begin
  if ModuleIsLib then
    Exit;

  Ini := TMeminiFile.Create(GetMapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataWriteProviderByIniFile.Create(Ini);
  FMainMapsList.SaveMaps(VLocalMapsConfig);

  FGPSRecorder.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GPS'));
  FGPSConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GPS'));
  FInetConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Internet'));
  FDownloadConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Internet'));
  FZmpConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('ZmpDefaultParams'));
  FGSMpar.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GSM'));
  FViewConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
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
end;

procedure TGlobalState.SendTerminateToThreads;
begin
  FGUISyncronizedTimer.Enabled := False;
  FAppClosingNotifier.Notify(nil);
  GPSpar.SendTerminateToThreads;
  FGCThread.Terminate;
end;

end.

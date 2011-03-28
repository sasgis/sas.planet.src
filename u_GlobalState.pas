unit u_GlobalState;

interface

uses
  Windows,
  ExtCtrls,
  Graphics,
  Classes,
  IniFiles,
  SysUtils,
  GR32,
  {$IFDEF SasDebugWithJcl}
  JclDebug,
  {$ENDIF SasDebugWithJcl}
  i_JclNotify,
  i_ILanguageManager,
  i_IMemObjCache,
  i_IConfigDataWriteProvider,
  i_IConfigDataProvider,
  i_ITileFileNameGeneratorsList,
  i_BitmapTypeExtManager,
  i_IContentTypeManager,
  i_IKmlInfoSimpleLoader,
  i_MapTypeIconsList,
  i_ICoordConverterFactory,
  i_IProxySettings,
  i_IGSMGeoCodeConfig,
  i_MainFormConfig,
  i_BitmapPostProcessingConfig,
  i_IValueToStringConverter,
  u_GarbageCollectorThread,
  i_ILastSelectionInfo,
  i_IDownloadInfoSimple,
  i_IImageResamplerConfig,
  i_IGeoCoderList,
  i_IMainMemCacheConfig,
  i_IMarkPicture,
  u_LastSelectionInfo,
  u_MarksReadWriteSimple,
  UMapType,
  u_MapTypesMainList,
  u_MemFileCache,
  i_IGPSConfig,
  i_IMarksFactoryConfig,
  i_IMarkCategoryFactoryConfig,
  i_IGlobalViewMainConfig,
  i_IImportFile,
  i_IGPSRecorder,
  i_ISatellitesInViewMapDraw,
  u_GPSState,
  u_GlobalCahceConfig;

type
  TGlobalState = class
  private
    // Ini-файл с основными настройками
    MainIni: TMeminifile;
    FTileNameGenerator: ITileFileNameGeneratorsList;
    FGCThread: TGarbageCollectorThread;
    FBitmapTypeManager: IBitmapTypeExtManager;
    FContentTypeManager: IContentTypeManager;
    FMapCalibrationList: IInterfaceList;
    FKmlLoader: IKmlInfoSimpleLoader;
    FKmzLoader: IKmlInfoSimpleLoader;
    FCacheConfig: TGlobalCahceConfig;
    FMapTypeIcons18List: IMapTypeIconsList;
    FMapTypeIcons24List: IMapTypeIconsList;
    FLanguageManager: ILanguageManager;
    FMainConfigProvider: IConfigDataWriteProvider;
    FLastSelectionInfo: ILastSelectionInfo;
    FMarksDB: TMarksDB;
    FCoordConverterFactory: ICoordConverterFactory;
    FMainMapsList: TMapTypesMainList;
    FInetConfig: IInetConfig;
    FProxySettings: IProxySettings;
    FGPSConfig: IGPSConfig;
    FGSMpar: IGSMGeoCodeConfig;
    FMainFormConfig: IMainFormConfig;
    FBitmapPostProcessingConfig: IBitmapPostProcessingConfig;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FDownloadInfo: IDownloadInfoSimple;
    FProgramPath: string;
    FImageResamplerConfig: IImageResamplerConfig;
    FGeoCoderList: IGeoCoderList;
    FMainMemCache: IMemObjCache;
    FMainMemCacheConfig: IMainMemCacheConfig;
    FMarkPictureList: IMarkPictureList;
    FMarksFactoryConfig: IMarksFactoryConfig;
    FMarksCategoryFactoryConfig: IMarkCategoryFactoryConfig;
    FGPSpar: TGPSpar;
    FImportFileByExt: IImportFile;
    FViewConfig: IGlobalViewMainConfig;
    FGPSRecorder: IGPSRecorder;
    FSkyMapDraw: ISatellitesInViewMapDraw;
    FGUISyncronizedTimer: TTimer;
    FGUISyncronizedTimerNotifier: IJclNotifier;

    procedure OnGUISyncronizedTimer(Sender: TObject);
    function GetMarkIconsPath: string;
    function GetMapsPath: string;
    function GetTrackLogPath: string;
    function GetHelpFileName: string;
    function GetMainConfigFileName: string;
    procedure LoadMainParams;
    procedure LoadMapIconsList;
    procedure DoException(Sender: TObject; E: Exception);
  public
    // Отображать окошко с логотипом при запуске
    Show_logo: Boolean;
    // Заходить на сайт автора при старте программы
    WebReportToAuthor: Boolean;
    // Выводить отладочную инфромацию о производительности
    ShowDebugInfo: Boolean;

    //Записывать информацию о тайлах отсутствующих на сервере
    SaveTileNotExists: Boolean;
    // Делать вторую попытку скачать файл при ошибке скачивания
    TwoDownloadAttempt: Boolean;
    // Переходить к следующему тайлу если произошла ошибка закачки
    GoNextTileIfDownloadError: Boolean;

    //Начать сохраненную сессию загрузки с последнего удачно загруженного тайла
    SessionLastSuccess: boolean;

    property MapType: TMapTypesMainList read FMainMapsList;

    property CacheConfig: TGlobalCahceConfig read FCacheConfig;

    // Список генераторов имен файлов с тайлами
    property TileNameGenerator: ITileFileNameGeneratorsList read FTileNameGenerator;
    // Путь к папке с картами
    property MapsPath: string read GetMapsPath;
    // Имя файла со справкой по программе
    property HelpFileName: string read GetHelpFileName;
    // Менеджер типов растровых тайлов. Теоретически, каждая карта может иметь свой собственный.
    property BitmapTypeManager: IBitmapTypeExtManager read FBitmapTypeManager;
    property ContentTypeManager: IContentTypeManager read FContentTypeManager;
    property CoordConverterFactory: ICoordConverterFactory read FCoordConverterFactory;
    property MapCalibrationList: IInterfaceList read FMapCalibrationList;
    property KmlLoader: IKmlInfoSimpleLoader read FKmlLoader;
    property KmzLoader: IKmlInfoSimpleLoader read FKmzLoader;
    property MapTypeIcons18List: IMapTypeIconsList read FMapTypeIcons18List;
    property MapTypeIcons24List: IMapTypeIconsList read FMapTypeIcons24List;

    property GCThread: TGarbageCollectorThread read FGCThread;
    property LanguageManager: ILanguageManager read FLanguageManager;
    property MainConfigProvider: IConfigDataWriteProvider read FMainConfigProvider;
    property LastSelectionInfo: ILastSelectionInfo read FLastSelectionInfo;
    property MarksDB: TMarksDB read FMarksDB;
    property InetConfig: IInetConfig read FInetConfig;
    property ProxySettings: IProxySettings read FProxySettings;
    property GSMpar: IGSMGeoCodeConfig read FGSMpar;
    property MainFormConfig: IMainFormConfig read FMainFormConfig;
    property BitmapPostProcessingConfig: IBitmapPostProcessingConfig read FBitmapPostProcessingConfig;
    property ValueToStringConverterConfig: IValueToStringConverterConfig read FValueToStringConverterConfig;
    property DownloadInfo: IDownloadInfoSimple read FDownloadInfo;
    property ProgramPath: string read FProgramPath;
    property ImageResamplerConfig: IImageResamplerConfig read FImageResamplerConfig;
    property MainMemCache: IMemObjCache read FMainMemCache;
    property MainMemCacheConfig: IMainMemCacheConfig read FMainMemCacheConfig;
    property GPSConfig: IGPSConfig read FGPSConfig;
    property MarksFactoryConfig: IMarksFactoryConfig read FMarksFactoryConfig;
    property MarksCategoryFactoryConfig: IMarkCategoryFactoryConfig read FMarksCategoryFactoryConfig;
    property GPSpar: TGPSpar read FGPSpar;
    property ImportFileByExt: IImportFile read FImportFileByExt;
    property ViewConfig: IGlobalViewMainConfig read FViewConfig;
    property GPSRecorder: IGPSRecorder read FGPSRecorder;
    property SkyMapDraw: ISatellitesInViewMapDraw read FSkyMapDraw;
    property GUISyncronizedTimerNotifier: IJclNotifier read FGUISyncronizedTimerNotifier;

    constructor Create;
    destructor Destroy; override;
    procedure LoadConfig;
    procedure SaveMainParams;
    procedure StartThreads;
    procedure SendTerminateToThreads;

    procedure StartExceptionTracking;
    procedure StopExceptionTracking;

    procedure LoadBitmapFromRes(const Name: String; Abmp: TCustomBitmap32);
    procedure LoadBitmapFromJpegRes(const Name: String; Abmp: TCustomBitmap32);
  end;

var
  GState: TGlobalState;

implementation

uses
  Types,
  Forms,
  u_JclNotify,
  i_BitmapTileSaveLoad,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile,
  i_IListOfObjectsWithTTL,
  u_ListOfObjectsWithTTL,
  u_BitmapTypeExtManagerSimple,
  u_ContentTypeManagerSimple,
  u_MapCalibrationListBasic,
  u_KmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_MapTypeIconsList,
  u_CoordConverterFactorySimple,
  u_LanguageManager,
  u_DownloadInfoSimple,
  u_InetConfig,
  u_GSMGeoCodeConfig,
  u_GPSConfig,
  u_MarksFactoryConfig,
  u_MarkCategoryFactoryConfig,
  u_GeoCoderListSimple,
  u_BitmapPostProcessingConfig,
  u_ValueToStringConverterConfig,
  u_MainMemCacheConfig,
  u_MarkPictureListSimple,
  u_ImageResamplerConfig,
  u_ImageResamplerFactoryListStaticSimple,
  u_ImportByFileExt,
  u_GlobalViewMainConfig,
  u_GPSRecorderStuped,
  u_GPSLogWriterToPlt,
  u_SatellitesInViewMapDrawSimple,
  u_GPSModuleFactoryByZylGPS,
  u_MainFormConfig,
  UResStrings,
  u_TileFileNameGeneratorsSimpleList;

{ TGlobalState }

constructor TGlobalState.Create;
var
  VList: IListOfObjectsWithTTL;
  VViewCnonfig: IConfigDataProvider;
begin
  FGUISyncronizedTimer := TTimer.Create(nil);
  FGUISyncronizedTimer.Enabled := False;
  FGUISyncronizedTimer.Interval := 500;
  FGUISyncronizedTimer.OnTimer := Self.OnGUISyncronizedTimer;

  FGUISyncronizedTimerNotifier := TJclBaseNotifier.Create;
  Show_logo := True;
  ShowDebugInfo := False;
  FCacheConfig := TGlobalCahceConfig.Create;
  FDownloadInfo := TDownloadInfoSimple.Create(nil);
  FMainMapsList := TMapTypesMainList.Create;
  FProgramPath := ExtractFilePath(ParamStr(0));
  MainIni := TMeminifile.Create(GetMainConfigFileName);
  FMainConfigProvider := TConfigDataWriteProviderByIniFile.Create(MainIni);
  VViewCnonfig := FMainConfigProvider.GetSubItem('VIEW');
  FLanguageManager := TLanguageManager.Create;
  FLanguageManager.ReadConfig(VViewCnonfig);
  if VViewCnonfig <> nil then begin
    Show_logo := VViewCnonfig.ReadBool('Show_logo', Show_logo);
    ShowDebugInfo := VViewCnonfig.ReadBool('time_rendering', ShowDebugInfo);
  end;
  FImageResamplerConfig :=
    TImageResamplerConfig.Create(
      TImageResamplerFactoryListStaticSimple.Create
    );
  FInetConfig := TInetConfig.Create;
  FProxySettings := FInetConfig.ProxyConfig as IProxySettings;
  FGPSConfig := TGPSConfig.Create(GetTrackLogPath);
  FGPSRecorder := TGPSRecorderStuped.Create;
  FGSMpar := TGSMGeoCodeConfig.Create;
  FCoordConverterFactory := TCoordConverterFactorySimple.Create;
  FMainMemCacheConfig := TMainMemCacheConfig.Create;
  FViewConfig := TGlobalViewMainConfig.Create;

  FMainMemCache := TMemFileCache.Create(FMainMemCacheConfig);
  FTileNameGenerator := TTileFileNameGeneratorsSimpleList.Create;
  FBitmapTypeManager := TBitmapTypeExtManagerSimple.Create;
  FContentTypeManager := TContentTypeManagerSimple.Create;
  FMapCalibrationList := TMapCalibrationListBasic.Create;
  FKmlLoader := TKmlInfoSimpleParser.Create;
  FKmzLoader := TKmzInfoSimpleParser.Create;
  FImportFileByExt := TImportByFileExt.Create(FKmlLoader, FKmzLoader);
  VList := TListOfObjectsWithTTL.Create;
  FGCThread := TGarbageCollectorThread.Create(VList, 1000);
  FBitmapPostProcessingConfig := TBitmapPostProcessingConfig.Create;
  FValueToStringConverterConfig := TValueToStringConverterConfig.Create(FLanguageManager);
  FGPSpar :=
    TGPSpar.Create(
      TGPSModuleFactoryByZylGPS.Create,
      TPltLogWriter.Create(GetTrackLogPath),
      FGPSConfig,
      FGPSRecorder,
      GUISyncronizedTimerNotifier
    );
  FLastSelectionInfo := TLastSelectionInfo.Create;
  FGeoCoderList := TGeoCoderListSimple.Create(FProxySettings);
  FMarkPictureList := TMarkPictureListSimple.Create(GetMarkIconsPath, FBitmapTypeManager);
  FMarksFactoryConfig := TMarksFactoryConfig.Create(FMarkPictureList);
  FMarksCategoryFactoryConfig := TMarkCategoryFactoryConfig.Create(SAS_STR_NewCategory);
  FMarksDB := TMarksDB.Create(FProgramPath, FMarksFactoryConfig, FMarksCategoryFactoryConfig);
  FSkyMapDraw := TSatellitesInViewMapDrawSimple.Create;
end;

destructor TGlobalState.Destroy;
begin
  FGCThread.Terminate;
  FGCThread.WaitFor;
  FreeAndNil(FGCThread);
  FLanguageManager := nil;
  try
    MainIni.UpdateFile;
  except
  end;
  FMainConfigProvider := nil;
  FMainMemCache := nil;
  FTileNameGenerator := nil;
  FBitmapTypeManager := nil;
  FContentTypeManager := nil;
  FMapCalibrationList := nil;
  FKmlLoader := nil;
  FKmzLoader := nil;
  FreeAndNil(FMarksDB);
  FMapTypeIcons18List := nil;
  FMapTypeIcons24List := nil;
  FLastSelectionInfo := nil;
  FGPSConfig := nil;
  FGPSRecorder := nil;
  FreeAndNil(FGPSpar);
  FreeAndNil(FMainMapsList);
  FCoordConverterFactory := nil;
  FProxySettings := nil;
  FGSMpar := nil;
  FInetConfig := nil;
  FViewConfig := nil;
  FImageResamplerConfig := nil;
  FMainFormConfig := nil;
  FBitmapPostProcessingConfig := nil;
  FValueToStringConverterConfig := nil;
  FMainMemCacheConfig := nil;
  FMarksFactoryConfig := nil;
  FMarksCategoryFactoryConfig := nil;
  FMarkPictureList := nil;
  FreeAndNil(FCacheConfig);
  FSkyMapDraw := nil;
  FreeAndNil(FGUISyncronizedTimer);
  FGUISyncronizedTimerNotifier := nil;
  inherited;
end;

procedure TGlobalState.DoException(Sender: TObject; E: Exception);
var
  Str: TStringList;
begin
  {$IFDEF SasDebugWithJcl}
  Str := TStringList.Create;
  try
    JclLastExceptStackListToStrings(Str, True, True, True, True);
    Str.Insert(0, E.Message);
    Str.Insert(1, '');
    Application.MessageBox(PChar(Str.Text), 'Ошибка', MB_OK or MB_ICONSTOP);
  finally
    FreeAndNil(Str);
  end;
  {$ENDIF SasDebugWithJcl}
end;

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

function TGlobalState.GetMapsPath: string;
begin
  Result := FProgramPath + 'Maps' + PathDelim;
end;

function TGlobalState.GetTrackLogPath: string;
begin
  Result := FProgramPath + 'TrackLog' + PathDelim;
end;

function TGlobalState.GetHelpFileName: string;
begin
  Result := FProgramPath + 'help.chm';
end;

function TGlobalState.GetMainConfigFileName: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

procedure TGlobalState.LoadBitmapFromRes(const Name: String; Abmp: TCustomBitmap32);
var
  ResStream: TResourceStream;
  VImageLoader: IBitmapTileLoader;
begin
  VImageLoader := FBitmapTypeManager.GetBitmapLoaderForExt('.png');
  {Creates an especial stream to load from the resource}
  ResStream := TResourceStream.Create(HInstance, Name, RT_RCDATA);

  {Loads the png image from the resource}
  try
    VImageLoader.LoadFromStream(ResStream, Abmp);
  finally
    ResStream.Free;
  end;
end;

procedure TGlobalState.LoadConfig;
var
  VLocalMapsConfig: IConfigDataProvider;
  Ini: TMeminifile;
begin
  LoadMainParams;
  CreateDir(MapsPath);
  Ini := TMeminiFile.Create(MapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataProviderByIniFile.Create(Ini);
  FMainMapsList.LoadMaps(VLocalMapsConfig, MapsPath);
  FMainFormConfig := TMainFormConfig.Create(
    FGeoCoderList,
    FMainMapsList.MapsList,
    FMainMapsList.LayersList,
    FMainMapsList[0].GUID
  );
  FCacheConfig.LoadConfig(FMainConfigProvider);
  LoadMapIconsList;
  FViewConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
  FGPSRecorder.ReadConfig(MainConfigProvider.GetSubItem('GPS'));
  FGPSConfig.ReadConfig(MainConfigProvider.GetSubItem('GPS'));
  FInetConfig.ReadConfig(MainConfigProvider.GetSubItem('Internet'));
  FGSMpar.ReadConfig(MainConfigProvider.GetSubItem('GSM'));
  FBitmapPostProcessingConfig.ReadConfig(MainConfigProvider.GetSubItem('COLOR_LEVELS'));
  FValueToStringConverterConfig.ReadConfig(MainConfigProvider.GetSubItem('ValueFormats'));
  FMainFormConfig.ReadConfig(MainConfigProvider);
  FLastSelectionInfo.ReadConfig(MainConfigProvider.GetSubItem('LastSelection'));
  FImageResamplerConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
  FMainMemCacheConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
  FMarkPictureList.ReadConfig(MainConfigProvider);
  FMarksFactoryConfig.ReadConfig(MainConfigProvider);
  FMarksCategoryFactoryConfig.ReadConfig(MainConfigProvider.GetSubItem('MarkNewCategory'));
  FMarksDb.ReadConfig(MainConfigProvider);
end;

procedure TGlobalState.LoadBitmapFromJpegRes(const Name: String; Abmp: TCustomBitmap32);
var
  ResStream: TResourceStream;
  VImageLoader: IBitmapTileLoader;
begin
  VImageLoader := FBitmapTypeManager.GetBitmapLoaderForExt('.jpg');
  {Creates an especial stream to load from the resource}
  ResStream := TResourceStream.Create(HInstance, Name, RT_RCDATA);

  {Loads the png image from the resource}
  try
    VImageLoader.LoadFromStream(ResStream, Abmp);
  finally
    ResStream.Free;
  end;
end;

procedure TGlobalState.LoadMainParams;
begin
  WebReportToAuthor := MainIni.ReadBool('NPARAM', 'stat', true);
  SaveTileNotExists:=MainIni.ReadBool('INTERNET','SaveTileNotExists', false);

  TwoDownloadAttempt:=MainIni.ReadBool('INTERNET','DblDwnl',true);
  GoNextTileIfDownloadError:=MainIni.ReadBool('INTERNET','GoNextTile',false);
  SessionLastSuccess:=MainIni.ReadBool('INTERNET','SessionLastSuccess',false);
end;

procedure TGlobalState.LoadMapIconsList;
var
  i: Integer;
  VMapType: TMapType;
  VList18: TMapTypeIconsList;
  VList24: TMapTypeIconsList;
begin
  VList18 := TMapTypeIconsList.Create(18, 18);
  FMapTypeIcons18List := VList18;

  VList24 := TMapTypeIconsList.Create(24, 24);
  FMapTypeIcons24List := VList24;

  for i := 0 to MapType.Count - 1 do begin
    VMapType := MapType[i];
    VList18.Add(VMapType.GUID, VMapType.bmp18);
    VList24.Add(VMapType.GUID, VMapType.bmp24);
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
  Ini := TMeminiFile.Create(MapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataWriteProviderByIniFile.Create(Ini);
  FMainMapsList.SaveMaps(VLocalMapsConfig);
  MainIni.WriteBool('INTERNET','SaveTileNotExists',SaveTileNotExists);
  MainIni.WriteBool('INTERNET','DblDwnl',TwoDownloadAttempt);
  MainIni.Writebool('INTERNET','GoNextTile',GoNextTileIfDownloadError);
  MainIni.WriteBool('INTERNET','SessionLastSuccess',SessionLastSuccess);

  MainIni.Writebool('NPARAM','stat',WebReportToAuthor);
  FGPSRecorder.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GPS'));
  FGPSConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GPS'));
  FInetConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Internet'));
  FGSMpar.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GSM'));
  FViewConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FLastSelectionInfo.WriteConfig(MainConfigProvider.GetOrCreateSubItem('LastSelection'));
  FLanguageManager.WriteConfig(FMainConfigProvider.GetOrCreateSubItem('VIEW'));
  FBitmapPostProcessingConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('COLOR_LEVELS'));
  FValueToStringConverterConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('ValueFormats'));
  FMainFormConfig.WriteConfig(MainConfigProvider);
  FCacheConfig.SaveConfig(FMainConfigProvider);
  FImageResamplerConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FMainMemCacheConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FMarkPictureList.WriteConfig(MainConfigProvider);
  FMarksFactoryConfig.WriteConfig(MainConfigProvider);
  FMarksCategoryFactoryConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('MarkNewCategory'));
  FMarksDb.WriteConfig(MainConfigProvider);
end;

procedure TGlobalState.SendTerminateToThreads;
begin
  FGUISyncronizedTimer.Enabled := False;
  GPSpar.SendTerminateToThreads;
  FGCThread.Terminate;
end;

end.

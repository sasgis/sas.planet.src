unit u_GlobalState;

interface

uses
  Windows,
  Graphics,
  Classes,
  IniFiles,
  GR32,
  i_ILanguageManager,
  i_IMemObjCache,
  i_IConfigDataWriteProvider,
  i_IConfigDataProvider,
  i_ITileFileNameGeneratorsList,
  i_IBitmapTypeExtManager,
  i_IContentTypeManager,
  i_IKmlInfoSimpleLoader,
  i_MapTypeIconsList,
  i_ICoordConverterFactory,
  i_IProxySettings,
  i_IGSMGeoCodeConfig,
  i_MainFormConfig,
  i_IBitmapPostProcessingConfig,
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
  u_GPSState,
  u_GlobalCahceConfig;

type
  TGlobalState = class
  private
    // Ini-файл с основными настройками
    MainIni: TMeminifile;
    FScreenSize: TPoint;
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
    function GetMarkIconsPath: string;
    function GetMapsPath: string;
    function GetTrackLogPath: string;
    function GetHelpFileName: string;
    function GetMainConfigFileName: string;
    procedure LoadMainParams;
    procedure SetScreenSize(const Value: TPoint);
    procedure LoadMapIconsList;
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

    GPSpar: TGPSpar;

    // Цвет для отсутствующих тайлов в слое заполнения карты
    MapZapColor: TColor;
    // Показывать tne на слое заполнения карты
    MapZapShowTNE: Boolean;
    // Цвет для тайлов отсутсвтующих на сервере в слое заполнения карты
    MapZapTneColor: TColor;
    // Прозрачность слоя заполнения карты
    MapZapAlpha: byte;

    // Количество тайлов отображаемых за границей экрана
    TilesOut: Integer;

    //Использовать тайлы предыдущих уровней для отображения
    UsePrevZoom: Boolean;
    //Использовать тайлы предыдущих уровней для отображения (для слоев)
    UsePrevZoomLayer: Boolean;

    //Цвет фона
    BGround: TColor;

    //Начать сохраненную сессию загрузки с последнего удачно загруженного тайла
    SessionLastSuccess: boolean;

    property MapType: TMapTypesMainList read FMainMapsList;

    property CacheConfig: TGlobalCahceConfig read FCacheConfig;

    // Размеры экрана, что бы не дергать каждый раз объект TScreen
    property ScreenSize: TPoint read FScreenSize write SetScreenSize;

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

    constructor Create;
    destructor Destroy; override;
    procedure LoadConfig;
    procedure SaveMainParams;
    procedure StartThreads;
    procedure SendTerminateToThreads;
    procedure LoadBitmapFromRes(const Name: String; Abmp: TCustomBitmap32);
    procedure LoadBitmapFromJpegRes(const Name: String; Abmp: TCustomBitmap32);
  end;

var
  GState: TGlobalState;

implementation

uses
  Types,
  SysUtils,
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
  u_GeoCoderListSimple,
  u_BitmapPostProcessingConfig,
  u_ValueToStringConverterConfig,
  u_MainMemCacheConfig,
  u_MarkPictureListSimple,
  u_ImageResamplerConfig,
  u_ImageResamplerFactoryListStaticSimple,
  u_MainFormConfig,
  u_TileFileNameGeneratorsSimpleList;

{ TGlobalState }

constructor TGlobalState.Create;
var
  VList: IListOfObjectsWithTTL;
  VViewCnonfig: IConfigDataProvider;
begin
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
  FGSMpar := TGSMGeoCodeConfig.Create;
  FCoordConverterFactory := TCoordConverterFactorySimple.Create;
  FMainMemCacheConfig := TMainMemCacheConfig.Create;

  FMainMemCache := TMemFileCache.Create(FMainMemCacheConfig);
  FTileNameGenerator := TTileFileNameGeneratorsSimpleList.Create;
  FBitmapTypeManager := TBitmapTypeExtManagerSimple.Create;
  FContentTypeManager := TContentTypeManagerSimple.Create;
  FMapCalibrationList := TMapCalibrationListBasic.Create;
  FKmlLoader := TKmlInfoSimpleParser.Create;
  FKmzLoader := TKmzInfoSimpleParser.Create;
  VList := TListOfObjectsWithTTL.Create;
  FGCThread := TGarbageCollectorThread.Create(VList, 1000);
  FBitmapPostProcessingConfig := TBitmapPostProcessingConfig.Create;
  FValueToStringConverterConfig := TValueToStringConverterConfig.Create(FLanguageManager);
  GPSpar := TGPSpar.Create(GetTrackLogPath);
  FLastSelectionInfo := TLastSelectionInfo.Create;
  FGeoCoderList := TGeoCoderListSimple.Create(FProxySettings);
  FMarkPictureList := TMarkPictureListSimple.Create(GetMarkIconsPath, FBitmapTypeManager);
  FMarksDB := TMarksDB.Create(FProgramPath, FMarkPictureList);
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
  FreeAndNil(GPSpar);
  FreeAndNil(FMainMapsList);
  FCoordConverterFactory := nil;
  FProxySettings := nil;
  FGSMpar := nil;
  FInetConfig := nil;
  FImageResamplerConfig := nil;
  FMainFormConfig := nil;
  FBitmapPostProcessingConfig := nil;
  FValueToStringConverterConfig := nil;
  FMainMemCacheConfig := nil;
  FMarkPictureList := nil;
  FreeAndNil(FCacheConfig);
  inherited;
end;

procedure TGlobalState.StartThreads;
begin
  GPSpar.StartThreads;
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
    FMainMapsList.LayersList
  );
  FCacheConfig.LoadConfig(FMainConfigProvider);
  LoadMapIconsList;
  GPSpar.LoadConfig(MainConfigProvider);
  FInetConfig.ReadConfig(MainConfigProvider.GetSubItem('Internet'));
  FGSMpar.ReadConfig(MainConfigProvider.GetSubItem('GSM'));
  FBitmapPostProcessingConfig.ReadConfig(MainConfigProvider.GetSubItem('COLOR_LEVELS'));
  FValueToStringConverterConfig.ReadConfig(MainConfigProvider.GetSubItem('ValueFormats'));
  FMainFormConfig.ReadConfig(MainConfigProvider);
  FLastSelectionInfo.ReadConfig(MainConfigProvider.GetSubItem('LastSelection'));
  FImageResamplerConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
  FMainMemCacheConfig.ReadConfig(MainConfigProvider.GetSubItem('View'));
  FMarkPictureList.ReadConfig(MainConfigProvider);
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
  TilesOut:=MainIni.readInteger('VIEW','TilesOut',0);
  SaveTileNotExists:=MainIni.ReadBool('INTERNET','SaveTileNotExists', false);

  TwoDownloadAttempt:=MainIni.ReadBool('INTERNET','DblDwnl',true);
  GoNextTileIfDownloadError:=MainIni.ReadBool('INTERNET','GoNextTile',false);
  SessionLastSuccess:=MainIni.ReadBool('INTERNET','SessionLastSuccess',false);

  UsePrevZoom := MainIni.Readbool('VIEW','back_load',true);
  UsePrevZoomLayer := MainIni.Readbool('VIEW','back_load_layer',true);
  MapZapColor:=MainIni.Readinteger('VIEW','MapZapColor',clBlack);
  MapZapShowTNE:=MainIni.ReadBool('VIEW','MapZapShowTNE', True);
  MapZapTneColor:=MainIni.Readinteger('VIEW','MapZapTneColor',clRed);
  MapZapAlpha:=MainIni.Readinteger('VIEW','MapZapAlpha',110);

  BGround:=MainIni.ReadInteger('VIEW','Background',clSilver);
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

procedure TGlobalState.SetScreenSize(const Value: TPoint);
begin
  FScreenSize := Value;
end;

procedure TGlobalState.SaveMainParams;
var
  Ini: TMeminifile;
  VLocalMapsConfig: IConfigDataWriteProvider;
begin
  Ini := TMeminiFile.Create(MapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataWriteProviderByIniFile.Create(Ini);
  FMainMapsList.SaveMaps(VLocalMapsConfig);
  MainIni.WriteInteger('VIEW','TilesOut',TilesOut);
  MainIni.Writebool('VIEW','back_load',UsePrevZoom);
  MainIni.Writebool('VIEW','back_load_layer',UsePrevZoomLayer);
  MainIni.Writeinteger('VIEW','MapZapColor',MapZapColor);
  MainIni.WriteBool('VIEW','MapZapShowTNE',MapZapShowTNE);
  MainIni.Writeinteger('VIEW','MapZapTneColor',MapZapTneColor);
  MainIni.Writeinteger('VIEW','MapZapAlpha',MapZapAlpha);
  MainIni.WriteInteger('VIEW','Background',BGround);

  MainIni.WriteBool('INTERNET','SaveTileNotExists',SaveTileNotExists);
  MainIni.WriteBool('INTERNET','DblDwnl',TwoDownloadAttempt);
  MainIni.Writebool('INTERNET','GoNextTile',GoNextTileIfDownloadError);
  MainIni.WriteBool('INTERNET','SessionLastSuccess',SessionLastSuccess);

  MainIni.Writebool('NPARAM','stat',WebReportToAuthor);
  GPSpar.SaveConfig(MainConfigProvider);
  FInetConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Internet'));
  FGSMpar.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GSM'));
  FLastSelectionInfo.WriteConfig(MainConfigProvider.GetOrCreateSubItem('LastSelection'));
  FLanguageManager.WriteConfig(FMainConfigProvider.GetOrCreateSubItem('VIEW'));
  FBitmapPostProcessingConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('COLOR_LEVELS'));
  FValueToStringConverterConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('ValueFormats'));
  FMainFormConfig.WriteConfig(MainConfigProvider);
  FCacheConfig.SaveConfig(FMainConfigProvider);
  FImageResamplerConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FMainMemCacheConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('View'));
  FMarkPictureList.WriteConfig(MainConfigProvider);
  FMarksDb.WriteConfig(MainConfigProvider);
end;

procedure TGlobalState.SendTerminateToThreads;
begin
  GPSpar.SendTerminateToThreads;
  FGCThread.Terminate;
end;

end.

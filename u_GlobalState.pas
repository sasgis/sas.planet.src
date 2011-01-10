unit u_GlobalState;

interface

uses
  Windows,
  Graphics,
  Classes,
  IniFiles,
  GR32,
  t_CommonTypes,
  i_ILanguageManager,
  i_IMemObjCache,
  i_IConfigDataWriteProvider,
  i_IConfigDataProvider,
  i_ITileFileNameGeneratorsList,
  i_IBitmapTypeExtManager,
  i_IContentTypeManager,
  i_IKmlInfoSimpleLoader,
  i_IBitmapLayerProvider,
  i_MapTypeIconsList,
  i_ICoordConverterFactory,
  i_IProxySettings,
  i_IGSMGeoCodeConfig,
  i_MainFormConfig,
  i_IBitmapPostProcessingConfig,
  i_IValueToStringConverter,
  u_GarbageCollectorThread,
  u_MapViewPortState,
  i_ILastSelectionInfo,
  i_IDownloadInfoSimple,
  i_IImageResamplerConfig,
  i_IGeoCoderList,
  i_IMainMemCacheConfig,
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
    FViewState: TMapViewPortState;
    FScreenSize: TPoint;
    FTileNameGenerator: ITileFileNameGeneratorsList;
    FGCThread: TGarbageCollectorThread;
    FBitmapTypeManager: IBitmapTypeExtManager;
    FContentTypeManager: IContentTypeManager;
    FMapCalibrationList: IInterfaceList;
    FKmlLoader: IKmlInfoSimpleLoader;
    FKmzLoader: IKmlInfoSimpleLoader;
    FCacheConfig: TGlobalCahceConfig;
    FMarksBitmapProvider: IBitmapLayerProvider;
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
    function GetMarkIconsPath: string;
    function GetMarksFileName: string;
    function GetMarksBackUpFileName: string;
    function GetMarksCategoryBackUpFileName: string;
    function GetMarksCategoryFileName: string;
    function GetMapsPath: string;
    function GetTrackLogPath: string;
    function GetHelpFileName: string;
    function GetMainConfigFileName: string;
    procedure LoadMarkIcons;
    procedure LoadMainParams;
    procedure FreeMarkIcons;
    procedure SetScreenSize(const Value: TPoint);
    procedure LoadMapIconsList;
  public
    // Иконки для меток
    MarkIcons: TStringList;

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

    WikiMapMainColor: TColor;
    WikiMapFonColor: TColor;

    show_point: TMarksShowType;

    // Количество тайлов отображаемых за границей экрана
    TilesOut: Integer;

    //Использовать тайлы предыдущих уровней для отображения
    UsePrevZoom: Boolean;
    //Использовать тайлы предыдущих уровней для отображения (для слоев)
    UsePrevZoomLayer: Boolean;

    // Показывать хинты при нахождении мыши над меткой
    ShowHintOnMarks: Boolean;

    // Параетры касающиеся именно главного окна

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
    // Путь к иконкам меток
    property MarkIconsPath: string read GetMarkIconsPath;
    // Имя файла с метками
    property MarksFileName: string read GetMarksFileName;
    // Име резервной копии файла с метками
    property MarksBackUpFileName: string read GetMarksBackUpFileName;

    // Имя файла с категориями меток
    property MarksCategoryFileName: string read GetMarksCategoryFileName;
    // Име резервной копии файла с категориями меток
    property MarksCategoryBackUpFileName: string read GetMarksCategoryBackUpFileName;
    // Путь к папке с картами
    property MapsPath: string read GetMapsPath;
    // Путь к папке с треками
    property TrackLogPath: string read GetTrackLogPath;
    // Имя файла со справкой по программе
    property HelpFileName: string read GetHelpFileName;
    // Менеджер типов растровых тайлов. Теоретически, каждая карта может иметь свой собственный.
    property BitmapTypeManager: IBitmapTypeExtManager read FBitmapTypeManager;
    property ContentTypeManager: IContentTypeManager read FContentTypeManager;
    property CoordConverterFactory: ICoordConverterFactory read FCoordConverterFactory;
    property MapCalibrationList: IInterfaceList read FMapCalibrationList;
    property KmlLoader: IKmlInfoSimpleLoader read FKmlLoader;
    property KmzLoader: IKmlInfoSimpleLoader read FKmzLoader;
    property MarksBitmapProvider: IBitmapLayerProvider read FMarksBitmapProvider;
    property MapTypeIcons18List: IMapTypeIconsList read FMapTypeIcons18List;
    property MapTypeIcons24List: IMapTypeIconsList read FMapTypeIcons24List;

    property GCThread: TGarbageCollectorThread read FGCThread;
    property ViewState: TMapViewPortState read FViewState;
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
    procedure InitViewState(AScreenSize: TPoint);
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
  i_ICoordConverter,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile,
  i_IListOfObjectsWithTTL,
  u_ListOfObjectsWithTTL,
  u_BitmapTypeExtManagerSimple,
  u_ContentTypeManagerSimple,
  u_MapCalibrationListBasic,
  u_KmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_MapMarksBitmapLayerProviderStuped,
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
  FMarksDB := TMarksDB.Create;
  FMarksBitmapProvider := TMapMarksBitmapLayerProviderStuped.Create;
  FBitmapPostProcessingConfig := TBitmapPostProcessingConfig.Create;
  FValueToStringConverterConfig := TValueToStringConverterConfig.Create(FLanguageManager);
  GPSpar := TGPSpar.Create;
  FLastSelectionInfo := TLastSelectionInfo.Create;
  FGeoCoderList := TGeoCoderListSimple.Create(FProxySettings);
  FMainFormConfig := TMainFormConfig.Create(FGeoCoderList);
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
  FreeMarkIcons;
  FMainMemCache := nil;
  FTileNameGenerator := nil;
  FBitmapTypeManager := nil;
  FContentTypeManager := nil;
  FMapCalibrationList := nil;
  FKmlLoader := nil;
  FKmzLoader := nil;
  FMarksBitmapProvider := nil;
  FreeAndNil(FMarksDB);
  FMapTypeIcons18List := nil;
  FMapTypeIcons24List := nil;
  FLastSelectionInfo := nil;
  FreeAndNil(GPSpar);
  FreeAndNil(FViewState);
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

function TGlobalState.GetMarksBackUpFileName: string;
begin
  Result := FProgramPath + 'marks.~sml';
end;

function TGlobalState.GetMarksFileName: string;
begin
  Result := FProgramPath + 'marks.sml';
end;


function TGlobalState.GetMarksCategoryBackUpFileName: string;
begin
  Result := FProgramPath + 'Categorymarks.~sml';
end;

function TGlobalState.GetMarksCategoryFileName: string;
begin
  Result := FProgramPath + 'Categorymarks.sml';
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

procedure TGlobalState.LoadMarkIcons;
var
  SearchRec: TSearchRec;
  Vbmp: TCustomBitmap32;
  VLoader: IBitmapTileLoader;
begin
  MarkIcons := TStringList.Create;
  VLoader := FBitmapTypeManager.GetBitmapLoaderForExt('.png');
  if FindFirst(MarkIconsPath + '*.png', faAnyFile, SearchRec) = 0 then begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) <> faDirectory then begin
          Vbmp := TCustomBitmap32.Create;
          VLoader.LoadFromFile(MarkIconsPath + SearchRec.Name, Vbmp);
          MarkIcons.AddObject(SearchRec.Name, Vbmp);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

procedure TGlobalState.FreeMarkIcons;
var
  i: integer;
begin
  for i := 0 to MarkIcons.Count - 1 do begin
    MarkIcons.Objects[i].Free;
  end;
  FreeAndNil(MarkIcons);
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
  LoadMarkIcons;
  CreateDir(MapsPath);
  Ini := TMeminiFile.Create(MapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataProviderByIniFile.Create(Ini);
  FMainMapsList.LoadMaps(VLocalMapsConfig, MapsPath);
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

  show_point := TMarksShowType(MainIni.readinteger('VIEW','ShowPointType',2));

  UsePrevZoom := MainIni.Readbool('VIEW','back_load',true);
  UsePrevZoomLayer := MainIni.Readbool('VIEW','back_load_layer',true);
  MapZapColor:=MainIni.Readinteger('VIEW','MapZapColor',clBlack);
  MapZapShowTNE:=MainIni.ReadBool('VIEW','MapZapShowTNE', True);
  MapZapTneColor:=MainIni.Readinteger('VIEW','MapZapTneColor',clRed);
  MapZapAlpha:=MainIni.Readinteger('VIEW','MapZapAlpha',110);

  ShowHintOnMarks:=MainIni.ReadBool('VIEW','ShowHintOnMarks',true);
  BGround:=MainIni.ReadInteger('VIEW','Background',clSilver);
  WikiMapMainColor:=MainIni.Readinteger('Wikimapia','MainColor',$FFFFFF);
  WikiMapFonColor:=MainIni.Readinteger('Wikimapia','FonColor',$000001);
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
  VZoom: Byte;
  VScreenCenterPos: TPoint;
  Ini: TMeminifile;
  VLocalMapsConfig: IConfigDataWriteProvider;
begin
  Ini := TMeminiFile.Create(MapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataWriteProviderByIniFile.Create(Ini);
  FMainMapsList.SaveMaps(VLocalMapsConfig);
  ViewState.LockRead;
  try
    VZoom := ViewState.GetCurrentZoom;
    VScreenCenterPos := ViewState.GetCenterMapPixel;
  finally
    ViewState.UnLockRead;
  end;
  MainIni.WriteInteger('POSITION','zoom_size',VZoom + 1);
  MainIni.WriteInteger('POSITION','x',VScreenCenterPos.x);
  MainIni.WriteInteger('POSITION','y',VScreenCenterPos.y);
  MainIni.WriteInteger('VIEW','TilesOut',TilesOut);
  MainIni.Writebool('VIEW','back_load',UsePrevZoom);
  MainIni.Writebool('VIEW','back_load_layer',UsePrevZoomLayer);
  MainIni.WriteInteger('VIEW','ShowPointType',Byte(show_point));
  MainIni.Writeinteger('VIEW','MapZapColor',MapZapColor);
  MainIni.WriteBool('VIEW','MapZapShowTNE',MapZapShowTNE);
  MainIni.Writeinteger('VIEW','MapZapTneColor',MapZapTneColor);
  MainIni.Writeinteger('VIEW','MapZapAlpha',MapZapAlpha);
  MainIni.WriteBool('VIEW','ShowHintOnMarks', ShowHintOnMarks);
  MainIni.WriteInteger('VIEW','Background',BGround);
  MainIni.Writeinteger('Wikimapia','MainColor',WikiMapMainColor);
  MainIni.Writeinteger('Wikimapia','FonColor',WikiMapFonColor);

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
end;

procedure TGlobalState.SendTerminateToThreads;
begin
  GPSpar.SendTerminateToThreads;
  FGCThread.Terminate;
end;

procedure TGlobalState.InitViewState(AScreenSize: TPoint);
var
  VScreenCenterPos: TPoint;
  VZoom: Byte;
  VConverter: ICoordConverter;
begin
  if FViewState = nil then begin
    FViewState := TMapViewPortState.Create(
      FMainMapsList.MapsList,
      FMainMapsList.LayersList,
      AScreenSize
    );
  end else begin
    raise Exception.Create('Повторная инициализация объекта состояния отображаемого окна карты');
  end;
  FViewState.LockWrite;
  VConverter := FViewState.GetCurrentCoordConverter;
  VZoom := MainIni.ReadInteger('POSITION','zoom_size',1) - 1;
  VConverter.CheckZoom(VZoom);
  VScreenCenterPos.X := VConverter.PixelsAtZoom(VZoom) div 2;
  VScreenCenterPos.Y := VScreenCenterPos.X;
  VScreenCenterPos := Point(
    MainIni.ReadInteger('POSITION','x',VScreenCenterPos.X),
    MainIni.ReadInteger('POSITION','y',VScreenCenterPos.Y)
  );
  VConverter.CheckPixelPosStrict(VScreenCenterPos, VZoom, True);
  FViewState.ChangeZoomAndUnlock(VZoom, VScreenCenterPos);
end;

end.

unit u_GlobalState;

interface

uses
  Windows,
  Graphics,
  Classes,
  IniFiles,
  SyncObjs,
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
  i_ConfigMain,
  u_GarbageCollectorThread,
  u_GeoToStr,
  u_MapViewPortState,
  u_LastSelectionInfo,
  u_MarksReadWriteSimple,
  Uimgfun,
  UMapType,
  u_MapTypesMainList,
  u_MemFileCache,
  u_GPSState,
  u_GlobalCahceConfig;

type
  TSrchType = (stGoogle, stYandex);

  TGlobalState = class
  private
    // Ini-файл с основными настройками
    MainIni: TMeminifile;
    FViewState: TMapViewPortState;
    FMemFileCache: TMemFileCache;
    FScreenSize: TPoint;
    FDwnCS: TCriticalSection;
    FTileNameGenerator: ITileFileNameGeneratorsList;
    FGCThread: TGarbageCollectorThread;
    FBitmapTypeManager: IBitmapTypeExtManager;
    FContentTypeManager: IContentTypeManager;
    FMapCalibrationList: IInterfaceList;
    FKmlLoader: IKmlInfoSimpleLoader;
    FKmzLoader: IKmlInfoSimpleLoader;
    FCacheElemensMaxCnt: integer;
    FCacheConfig: TGlobalCahceConfig;
    FMarksBitmapProvider: IBitmapLayerProvider;
    FMapTypeIcons18List: IMapTypeIconsList;
    FMapTypeIcons24List: IMapTypeIconsList;
    FLanguageManager: ILanguageManager;
    FMainConfigProvider: IConfigDataWriteProvider;
    FLastSelectionInfo: TLastSelectionInfo;
    FMarksDB: TMarksDB;
    FCoordConverterFactory: ICoordConverterFactory;
    FMainMapsList: TMapTypesMainList;
    FInetConfig: IInetConfig;
    FProxySettings: IProxySettings;
    FGSMpar: IGSMGeoCodeConfig;
    FMainFormConfig: IMainFormConfig;
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
    procedure SetCacheElemensMaxCnt(const Value: integer);
    procedure LoadMapIconsList;
  public

    MainFileCache: IMemObjCache;
    // Параметры программы
    ProgramPath: string;
    // Иконки для меток
    MarkIcons: TStringList;

    // Отображать окошко с логотипом при запуске
    Show_logo: Boolean;
    // Заходить на сайт автора при старте программы
    WebReportToAuthor: Boolean;
    // Выводить отладочную инфромацию о производительности
    ShowDebugInfo: Boolean;

    // Способ отображения расстояний, и в частности масштаба
    num_format: TDistStrFormat;
    // Способ отображения координат в градусах
    llStrType: TDegrShowFormat;
    // Количество скачаных данных в килобайтах
    All_Dwn_Kb: Currency;
    // Количество скачанных тайлов
    All_Dwn_Tiles: Cardinal;

    //Записывать информацию о тайлах отсутствующих на сервере
    SaveTileNotExists: Boolean;
    // Делать вторую попытку скачать файл при ошибке скачивания
    TwoDownloadAttempt: Boolean;
    // Переходить к следующему тайлу если произошла ошибка закачки
    GoNextTileIfDownloadError: Boolean;

    // Способ ресамплинга картинки
    Resampling: TTileResamplingType;

    GPSpar: TGPSpar;

    BorderColor: TColor;
    BorderAlpha: byte;

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

    InvertColor: boolean;
    // Число для гамма преобразования тайлов перед отображением
    GammaN: Integer;
    // Число для изменения контрастности тайлов перед отображением
    ContrastN: Integer;

    show_point: TMarksShowType;
    FirstLat: Boolean;
    ShowMapName: Boolean;

    // Количество тайлов отображаемых за границей экрана
    TilesOut: Integer;

    //Использовать тайлы предыдущих уровней для отображения
    UsePrevZoom: Boolean;
    //Использовать тайлы предыдущих уровней для отображения (для слоев)
    UsePrevZoomLayer: Boolean;
    //Инвертировать направление при зуме колесом мышки
    MouseWheelInv: Boolean;
    //Анимированный зум
    AnimateZoom: Boolean;
    //При отображении сетки тайлов выводить подписи
    ShowBorderText: Boolean;
    // Масштаб отображаемой сетки генштаба
    GShScale: integer;


    // Показывать хинты при нахождении мыши над меткой
    ShowHintOnMarks: Boolean;

    // Параетры касающиеся именно главного окна

    // Отображать сетку тайлов для заданного зума
    TileGridZoom: byte;

    //Способ поиска
    SrchType: TSrchType;

    //Цвет фона
    BGround: TColor;

    //Начать сохраненную сессию загрузки с последнего удачно загруженного тайла
    SessionLastSuccess: boolean;

    property MapType: TMapTypesMainList read FMainMapsList;

    property CacheConfig: TGlobalCahceConfig read FCacheConfig;
    // Количество элементов в кэше в памяти
    property CacheElemensMaxCnt: integer read FCacheElemensMaxCnt write SetCacheElemensMaxCnt;

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
    // Имя основного файла конфигурации
    property MainConfigFileName: string read GetMainConfigFileName;
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
    property LastSelectionInfo: TLastSelectionInfo read FLastSelectionInfo;
    property MarksDB: TMarksDB read FMarksDB;
    property InetConfig: IInetConfig read FInetConfig;
    property ProxySettings: IProxySettings read FProxySettings;
    property GSMpar: IGSMGeoCodeConfig read FGSMpar;
    property MainFormConfig: IMainFormConfig read FMainFormConfig;

    constructor Create;
    destructor Destroy; override;
    procedure LoadConfig;
    procedure SaveMainParams;
    procedure IncrementDownloaded(ADwnSize: Currency; ADwnCnt: Cardinal);
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
  i_MapTypes,
  u_InetConfig,
  u_GSMGeoCodeConfig,
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
  FDwnCS := TCriticalSection.Create;
  FCacheConfig := TGlobalCahceConfig.Create;
  All_Dwn_Kb := 0;
  All_Dwn_Tiles := 0;
  FMainMapsList := TMapTypesMainList.Create;
  ProgramPath := ExtractFilePath(ParamStr(0));
  MainIni := TMeminifile.Create(MainConfigFileName);
  FMainConfigProvider := TConfigDataWriteProviderByIniFile.Create(MainIni);
  VViewCnonfig := FMainConfigProvider.GetSubItem('VIEW');
  FLanguageManager := TLanguageManager.Create;
  FLanguageManager.ReadConfig(VViewCnonfig);
  if VViewCnonfig <> nil then begin
    Show_logo := VViewCnonfig.ReadBool('Show_logo', Show_logo);
    ShowDebugInfo := VViewCnonfig.ReadBool('time_rendering', ShowDebugInfo);
  end;
  FInetConfig := TInetConfig.Create;
  FProxySettings := FInetConfig.ProxyConfig as IProxySettings;
  FGSMpar := TGSMGeoCodeConfig.Create;
  FCoordConverterFactory := TCoordConverterFactorySimple.Create;
  FMemFileCache := TMemFileCache.Create;
  MainFileCache := FMemFileCache;
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
  GPSpar := TGPSpar.Create;
  FMainFormConfig := TMainFormConfig.Create;
  FLastSelectionInfo := TLastSelectionInfo.Create;
end;

destructor TGlobalState.Destroy;
begin
  FGCThread.Terminate;
  FGCThread.WaitFor;
  FreeAndNil(FGCThread);
  FreeAndNil(FDwnCS);
  FLanguageManager := nil;
  try
    MainIni.UpdateFile;
  except
  end;
  FMainConfigProvider := nil;
  FreeMarkIcons;
  FMemFileCache := nil;
  MainFileCache := nil;
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
  FreeAndNil(FLastSelectionInfo);
  FreeAndNil(GPSpar);
  FreeAndNil(FViewState);
  FreeAndNil(FMainMapsList);
  FCoordConverterFactory := nil;
  FProxySettings := nil;
  FGSMpar := nil;
  FInetConfig := nil;
  FMainFormConfig := nil;
  FreeAndNil(FCacheConfig);
  inherited;
end;

procedure TGlobalState.StartThreads;
begin
  GPSpar.StartThreads;
end;

function TGlobalState.GetMarkIconsPath: string;
begin
  Result := ProgramPath + 'marksicons' + PathDelim;
end;

function TGlobalState.GetMarksBackUpFileName: string;
begin
  Result := ProgramPath + 'marks.~sml';
end;

function TGlobalState.GetMarksFileName: string;
begin
  Result := ProgramPath + 'marks.sml';
end;


function TGlobalState.GetMarksCategoryBackUpFileName: string;
begin
  Result := ProgramPath + 'Categorymarks.~sml';
end;

function TGlobalState.GetMarksCategoryFileName: string;
begin
  Result := ProgramPath + 'Categorymarks.sml';
end;

function TGlobalState.GetMapsPath: string;
begin
  Result := ProgramPath + 'Maps' + PathDelim;
end;

function TGlobalState.GetTrackLogPath: string;
begin
  Result := ProgramPath + 'TrackLog' + PathDelim;
end;

function TGlobalState.GetHelpFileName: string;
begin
  Result := ProgramPath + 'help.chm';
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

procedure TGlobalState.IncrementDownloaded(ADwnSize: Currency;
  ADwnCnt: Cardinal);
begin
  FDwnCS.Acquire;
  try
    All_Dwn_Kb := All_Dwn_Kb + ADwnSize;
    All_Dwn_Tiles := All_Dwn_Tiles + ADwnCnt;
  finally
    FDwnCS.Release;
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
  FMainFormConfig.ReadConfig(MainConfigProvider);
  FLastSelectionInfo.LoadConfig(MainConfigProvider.GetSubItem('LastSelection'));
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

  ShowMapName:=MainIni.readBool('VIEW','ShowMapNameOnPanel',true);

  show_point := TMarksShowType(MainIni.readinteger('VIEW','ShowPointType',2));
  TileGridZoom:=MainIni.readinteger('VIEW','grid',0);
  MouseWheelInv:=MainIni.readbool('VIEW','invert_mouse',false);

  num_format:= TDistStrFormat(MainIni.Readinteger('VIEW','NumberFormat',0));
  Resampling := TTileResamplingType(MainIni.Readinteger('VIEW','ResamlingType',1));
  llStrType:=TDegrShowFormat(MainIni.Readinteger('VIEW','llStrType',0));
  FirstLat:=MainIni.ReadBool('VIEW','FirstLat',false);
  BorderAlpha:=MainIni.Readinteger('VIEW','BorderAlpha',150);
  BorderColor:=MainIni.Readinteger('VIEW','BorderColor',$FFFFFF);
  ShowBorderText:=MainIni.ReadBool('VIEW','BorderText',true);
  UsePrevZoom := MainIni.Readbool('VIEW','back_load',true);
  UsePrevZoomLayer := MainIni.Readbool('VIEW','back_load_layer',true);
  AnimateZoom:=MainIni.Readbool('VIEW','animate',true);
  GShScale:=MainIni.Readinteger('VIEW','GShScale',0);
  if GShScale >= 1000000 then begin
    GShScale := 1000000;
  end else if GShScale >= 500000 then begin
    GShScale := 500000;
  end else if GShScale >= 200000 then begin
    GShScale := 200000;
  end else if GShScale >= 100000 then begin
    GShScale := 100000;
  end else if GShScale >= 50000 then begin
    GShScale := 50000;
  end else if GShScale >= 25000 then begin
    GShScale := 25000;
  end else if GShScale >= 10000 then begin
    GShScale := 10000;
  end else begin
    GShScale := 0;
  end;

  MapZapColor:=MainIni.Readinteger('VIEW','MapZapColor',clBlack);
  MapZapShowTNE:=MainIni.ReadBool('VIEW','MapZapShowTNE', True);
  MapZapTneColor:=MainIni.Readinteger('VIEW','MapZapTneColor',clRed);
  MapZapAlpha:=MainIni.Readinteger('VIEW','MapZapAlpha',110);

  CacheElemensMaxCnt:=MainIni.ReadInteger('VIEW','TilesOCache',150);
  ShowHintOnMarks:=MainIni.ReadBool('VIEW','ShowHintOnMarks',true);
  SrchType:=TSrchType(MainIni.ReadInteger('VIEW','SearchType',0));
  BGround:=MainIni.ReadInteger('VIEW','Background',clSilver);
  WikiMapMainColor:=MainIni.Readinteger('Wikimapia','MainColor',$FFFFFF);
  WikiMapFonColor:=MainIni.Readinteger('Wikimapia','FonColor',$000001);

  GammaN:=MainIni.Readinteger('COLOR_LEVELS','gamma',50);
  ContrastN:=MainIni.Readinteger('COLOR_LEVELS','contrast',0);
  InvertColor:=MainIni.ReadBool('COLOR_LEVELS','InvertColor',false);
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
  MainIni.WriteBool('VIEW','ShowMapNameOnPanel',ShowMapName);
  MainIni.WriteInteger('POSITION','zoom_size',VZoom + 1);
  MainIni.WriteInteger('POSITION','x',VScreenCenterPos.x);
  MainIni.WriteInteger('POSITION','y',VScreenCenterPos.y);
  MainIni.WriteInteger('VIEW','TilesOut',TilesOut);
  MainIni.Writeinteger('VIEW','grid', TileGridZoom);
  MainIni.Writebool('VIEW','invert_mouse',MouseWheelInv);
  MainIni.Writebool('VIEW','back_load',UsePrevZoom);
  MainIni.Writebool('VIEW','back_load_layer',UsePrevZoomLayer);
  MainIni.Writebool('VIEW','animate',AnimateZoom);
  MainIni.WriteInteger('VIEW','ShowPointType',Byte(show_point));
  MainIni.Writeinteger('VIEW','NumberFormat',byte(num_format));
  MainIni.Writeinteger('VIEW','ResamlingType',byte(resampling));
  MainIni.Writeinteger('VIEW','llStrType',byte(llStrType));
  MainIni.WriteBool('VIEW','FirstLat',FirstLat);
  MainIni.Writeinteger('VIEW','BorderAlpha',BorderAlpha);
  MainIni.Writeinteger('VIEW','BorderColor',BorderColor);
  MainIni.WriteBool('VIEW','BorderText',ShowBorderText);
  MainIni.Writeinteger('VIEW','GShScale',GShScale);
  MainIni.Writeinteger('VIEW','MapZapColor',MapZapColor);
  MainIni.WriteBool('VIEW','MapZapShowTNE',MapZapShowTNE);
  MainIni.Writeinteger('VIEW','MapZapTneColor',MapZapTneColor);
  MainIni.Writeinteger('VIEW','MapZapAlpha',MapZapAlpha);
  MainIni.WriteInteger('VIEW','TilesOCache', CacheElemensMaxCnt);
  MainIni.WriteBool('VIEW','ShowHintOnMarks', ShowHintOnMarks);
  MainIni.WriteInteger('VIEW','SearchType',integer(SrchType));
  MainIni.WriteInteger('VIEW','Background',BGround);
  MainIni.Writeinteger('Wikimapia','MainColor',WikiMapMainColor);
  MainIni.Writeinteger('Wikimapia','FonColor',WikiMapFonColor);

  MainIni.Writeinteger('COLOR_LEVELS','gamma', GammaN);
  MainIni.Writeinteger('COLOR_LEVELS','contrast',ContrastN);
  MainIni.WriteBool('COLOR_LEVELS','InvertColor',InvertColor);

  MainIni.WriteBool('INTERNET','SaveTileNotExists',SaveTileNotExists);
  MainIni.WriteBool('INTERNET','DblDwnl',TwoDownloadAttempt);
  MainIni.Writebool('INTERNET','GoNextTile',GoNextTileIfDownloadError);
  MainIni.WriteBool('INTERNET','SessionLastSuccess',SessionLastSuccess);

  MainIni.Writebool('NPARAM','stat',WebReportToAuthor);
  GPSpar.SaveConfig(MainConfigProvider);
  FInetConfig.WriteConfig(MainConfigProvider.GetOrCreateSubItem('Internet'));
  FGSMpar.WriteConfig(MainConfigProvider.GetOrCreateSubItem('GSM'));
  FLastSelectionInfo.SaveConfig(MainConfigProvider.GetOrCreateSubItem('LastSelection'));
  FLanguageManager.WriteConfig(FMainConfigProvider.GetOrCreateSubItem('VIEW'));
  FMainFormConfig.WriteConfig(MainConfigProvider);
  FCacheConfig.SaveConfig(FMainConfigProvider);
end;

procedure TGlobalState.SetCacheElemensMaxCnt(const Value: integer);
begin
  FCacheElemensMaxCnt := Value;
  FMemFileCache.CacheElemensMaxCnt := FCacheElemensMaxCnt;
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

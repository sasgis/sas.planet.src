unit u_GlobalState;

interface

uses
  Windows,
  Graphics,
  Classes,
  IniFiles,
  SyncObjs,
  GR32,
  t_GeoTypes,
  t_CommonTypes,
  i_IMemObjCache,
  i_ITileFileNameGeneratorsList,
  i_IBitmapTypeExtManager,
  i_IKmlInfoSimpleLoader,
  u_GarbageCollectorThread,
  u_GeoToStr,
  Uimgfun,
  UMapType,
  u_MemFileCache;

type
  TSrchType = (stGoogle,stYandex);

  TGlobalState = class
  private
    FMemFileCache: TMemFileCache;
    FScreenSize: TPoint;
    FDwnCS: TCriticalSection;
    FTileNameGenerator: ITileFileNameGeneratorsList;
    FGCThread: TGarbageCollectorThread;
    FBitmapTypeManager: IBitmapTypeExtManager;
    FMapCalibrationList: IInterfaceList;
    FKmlLoader: IKmlInfoSimpleLoader;
    FKmzLoader: IKmlInfoSimpleLoader;
    FCacheElemensMaxCnt: integer;
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
    procedure LoadResources;
    procedure LoadMainParams;
    procedure FreeAllMaps;
    procedure FreeMarkIcons;
    procedure SetScreenSize(const Value: TPoint);
    procedure SetCacheElemensMaxCnt(const Value: integer);
  public

    MainFileCache: IMemObjCache;
    // Ini-файл с основными настройками
    MainIni: TMeminifile;
    // Параметры программы
    ProgramPath: string;
    // Иконки для меток
    MarkIcons: TStringList;
    // Иконка для указания на точку куда выполнен переход.
    GOToSelIcon: TBitmap32;

    // Язык интерфейса программы
    Localization: Integer;

    // Заходить на сайт автора при старте программы
    WebReportToAuthor: Boolean;

    // Способ отображения расстояний, и в частности масштаба
    num_format: TDistStrFormat;
    // Способ отображения координат в градусах
    llStrType: TDegrShowFormat;
    // Количество скачаных данных в килобайтах
    All_Dwn_Kb: Currency;
    // Количество скачанных тайлов
    All_Dwn_Tiles: Cardinal;

    InetConnect: TInetConnect;
    //Записывать информацию о тайлах отсутствующих на сервере
    SaveTileNotExists: Boolean;
    // Загружать тайл дае есть информация о отсутствии его на сервере
    IgnoreTileNotExists: Boolean;
    // Делать вторую попытку скачать файл при ошибке скачивания
    TwoDownloadAttempt: Boolean;
    // Переходить к следующему тайлу если произошла ошибка закачки
    GoNextTileIfDownloadError: Boolean;

    // Способ ресамплинга картинки
    Resampling: TTileResamplingType;
    //Способ храения кеша по-умолчанию.
    DefCache: byte;

    GPS_enab: Boolean;

    //COM-порт, к которому подключен GPS
    GPS_COM: string;
    //Скорость GPS COM порта
    GPS_BaudRate: Integer;
    // Максимальное время ожидания данных от GPS
    GPS_TimeOut: integer;
    // Интервал между точками от GPS
    GPS_Delay: Integer;
    //Поправка GPS
    GPS_Correction: TExtendedPoint;
    //Размер указателя направления при GPS-навигации
    GPS_ArrowSize: Integer;
    //Цвет указателя направления при навигацци
    GPS_ArrowColor: TColor;
    //Отображать GPS трек
    GPS_ShowPath: Boolean;
    // Толщина отображемого GPS трека
    GPS_TrackWidth: Integer;
    //Центрировать карту на GPS позиции
    GPS_MapMove: Boolean;
    //Заисывать GPS трек в файл
    GPS_WriteLog: boolean;
    //Файл для записи GPS трека (Нужно будет заменить отдельным объектом)
    GPS_LogFile: TextFile;
    //Массив со значенимя скоростей полученными от GPS
    GPS_ArrayOfSpeed: array of Real;
    //Точки GPS трека
    GPS_TrackPoints: TExtendedPointArray;
    //Скрывать/показывать панель датчиков при подключении/отключении GPS
    GPS_SensorsAutoShow: boolean;

    LastSelectionColor: TColor;
    LastSelectionAlfa: Byte;

    BorderColor: TColor;
    BorderAlpha: byte;

    // Цвет для отсутствующих тайлов в слое заполнения карты
    MapZapColor:TColor;
    // Показывать tne на слое заполнения карты
    MapZapShowTNE: Boolean;
    // Цвет для тайлов отсутсвтующих на сервере в слое заполнения карты
    MapZapTneColor: TColor;
    // Прозрачность слоя заполнения карты
    MapZapAlpha:byte;

    WikiMapMainColor:TColor;
    WikiMapFonColor:TColor;

    InvertColor: boolean;
    // Число для гамма преобразования тайлов перед отображением
    GammaN: Integer;
    // Число для изменения контрастности тайлов перед отображением
    ContrastN: Integer;

    // Фиксировать центр изменения масштаба под курсором мыши
    ZoomingAtMousePos: Boolean;

    show_point: TMarksShowType;
    FirstLat: Boolean;
    ShowMapName: Boolean;
    // Покаызвать строку статуса
    ShowStatusBar: Boolean;

    //Зацикливать карту по горизонтали
    CiclMap: Boolean;

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


    //Пути к кешам разных типов
    NewCPath_: string;
    OldCPath_: string;
    ESCpath_: string;
    GMTilespath_: string;
    GECachepath_: string;

    // Показывать хинты при нахождении мыши над меткой
    ShowHintOnMarks: Boolean;

    // Параетры касающиеся именно главного окна

    FullScrean: Boolean;

    // Текущий зумм
    zoom_size: byte;

    // Отображать сетку тайлов для заданного зума
    TileGridZoom: byte;

    //Способ поиска
    SrchType: TSrchType;

    //параметры определения позиции по GSM
    GSMpar:TGSMpar;

    //Цвет фона
    BGround:TColor;

    //Начать сохраненную сессию загрузки с последнего удачно загруженного тайла
    SessionLastSuccess:boolean;

    MapType: array of TMapType;
    sat_map_both: TMapType;

    // Полигон последнего выделения при операциях с областью.
    LastSelectionPolygon: TExtendedPointArray;
    // Масштаб, на котором было последнее выделение
    poly_zoom_save: byte;

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
    property MapCalibrationList: IInterfaceList read FMapCalibrationList;
    property KmlLoader: IKmlInfoSimpleLoader read FKmlLoader;
    property KmzLoader: IKmlInfoSimpleLoader read FKmzLoader;

    property GCThread: TGarbageCollectorThread read FGCThread;
    constructor Create;
    destructor Destroy; override;
    procedure IncrementDownloaded(ADwnSize: Currency; ADwnCnt: Cardinal);
    procedure StopAllThreads;
  end;

const
  SASVersion='100222';
  CProgram_Lang_Default = LANG_RUSSIAN;

var
  GState: TGlobalState;
implementation

uses
  SysUtils,
  pngimage,
  i_IListOfObjectsWithTTL,
  u_ListOfObjectsWithTTL,
  u_BitmapTypeExtManagerSimple,
  u_MapCalibrationListBasic,
  u_KmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_TileFileNameGeneratorsSimpleList;

{ TGlobalState }

constructor TGlobalState.Create;
var
  VList: IListOfObjectsWithTTL;
begin
  FDwnCS := TCriticalSection.Create;
  All_Dwn_Kb := 0;
  All_Dwn_Tiles := 0;
  InetConnect := TInetConnect.Create;
  ProgramPath := ExtractFilePath(ParamStr(0));
  MainIni := TMeminifile.Create(MainConfigFileName);
  FMemFileCache := TMemFileCache.Create;
  MainFileCache := FMemFileCache;
  FTileNameGenerator := TTileFileNameGeneratorsSimpleList.Create;
  FBitmapTypeManager := TBitmapTypeExtManagerSimple.Create;
  FMapCalibrationList := TMapCalibrationListBasic.Create;
  FKmlLoader := TKmlInfoSimpleParser.Create;
  FKmzLoader := TKmzInfoSimpleParser.Create;
  VList := TListOfObjectsWithTTL.Create;
  FGCThread := TGarbageCollectorThread.Create(VList, 1000);
  LoadMainParams;
  LoadResources;
  LoadMarkIcons;
end;

destructor TGlobalState.Destroy;
begin
  FGCThread.Terminate;
  FGCThread.WaitFor;
  FreeAndNil(FGCThread);
  FreeAndNil(FDwnCS);
  MainIni.UpdateFile;
  FreeAndNil(MainIni);
  FreeMarkIcons;
  FreeAndNil(GOToSelIcon);
  FreeAndNil(InetConnect);
  FMemFileCache := nil;
  MainFileCache := nil;
  FTileNameGenerator := nil;
  FBitmapTypeManager := nil;
  FMapCalibrationList := nil;
  FKmlLoader := nil;
  FKmzLoader := nil;
  sat_map_both := nil;
  FreeAllMaps;
  inherited;
end;

function TGlobalState.GetMarkIconsPath: string;
begin
  Result := ProgramPath + 'marksicons\';
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
  Result := ProgramPath + 'Maps\';
end;

function TGlobalState.GetTrackLogPath: string;
begin
  Result := ProgramPath + 'TrackLog\';
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
  VPng: TPNGObject;
begin
  MarkIcons := TStringList.Create;
  if FindFirst(MarkIconsPath +'*.png', faAnyFile, SearchRec) = 0 then begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) <> faDirectory then begin
          VPng := TPNGObject.Create;
          VPng.LoadFromFile(MarkIconsPath+SearchRec.Name);
          MarkIcons.AddObject(SearchRec.Name, VPng);
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

procedure TGlobalState.LoadResources;
var
  b: TPNGObject;
begin
 b := TPNGObject.Create;
 try
   b.LoadFromResourceName(HInstance, 'ICONIII');
   GOToSelIcon := TBitmap32.Create;
   PNGintoBitmap32(GOToSelIcon, b);
   GOToSelIcon.DrawMode := dmBlend;
 finally
   FreeAndNil(b);
 end;
end;

procedure TGlobalState.FreeAllMaps;
var
  i: integer;
begin
  for i := 0 to Length(MapType) - 1 do begin
    FreeAndNil(MapType[i]);
  end;
  MapType := nil;
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

procedure TGlobalState.LoadMainParams;
var
  loc:integer;
begin
  if SysLocale.PriLangID <> CProgram_Lang_Default then begin
    loc := LANG_ENGLISH;
  end else begin
    loc := CProgram_Lang_Default;
  end;
  Localization := MainIni.Readinteger('VIEW','localization',loc);
  WebReportToAuthor := MainIni.ReadBool('NPARAM','stat',true);
end;

procedure TGlobalState.SetScreenSize(const Value: TPoint);
begin
  FScreenSize := Value;
end;

procedure TGlobalState.SetCacheElemensMaxCnt(const Value: integer);
begin
  FCacheElemensMaxCnt := Value;
  FMemFileCache.CacheElemensMaxCnt:= FCacheElemensMaxCnt;
end;

procedure TGlobalState.StopAllThreads;
begin
  FGCThread.Terminate;
end;

end.

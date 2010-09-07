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
  i_ActiveMapsConfigSaveLoad,
  i_IBitmapLayerProvider,
  i_MapTypeIconsList,
  u_GarbageCollectorThread,
  u_GeoToStr,
  u_MapViewPortState,
  Uimgfun,
  UMapType,
  u_MemFileCache,
  u_GlobalCahceConfig;

type
  TSrchType = (stGoogle, stYandex);

  TGlobalState = class
  private
    FViewState: TMapViewPortState;
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
    FMapConfigSaver: IActiveMapsConfigSaver;
    FMapConfigLoader: IActiveMapsConfigLoader;
    FCacheConfig: TGlobalCahceConfig;
    FMarksBitmapProvider: IBitmapLayerProvider;
    FMapTypeIcons18List: IMapTypeIconsList;
    FMapTypeIcons24List: IMapTypeIconsList;
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
    //Максимальное количество оотображаемых точек трека
    GPS_NumTrackPoints: integer;
    //Скрывать/показывать панель датчиков при подключении/отключении GPS
    GPS_SensorsAutoShow: boolean;
    //Писать лог NMEA
    GPS_NMEALog: boolean;

    LastSelectionColor: TColor;
    LastSelectionAlfa: Byte;

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

    // Фиксировать центр изменения масштаба под курсором мыши
    ZoomingAtMousePos: Boolean;

    show_point: TMarksShowType;
    FirstLat: Boolean;
    ShowMapName: Boolean;
    // Покаызвать строку статуса
    ShowStatusBar: Boolean;

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

    FullScrean: Boolean;

    // Отображать сетку тайлов для заданного зума
    TileGridZoom: byte;

    //Способ поиска
    SrchType: TSrchType;

    //параметры определения позиции по GSM
    GSMpar: TGSMpar;

    //Цвет фона
    BGround: TColor;

    //Начать сохраненную сессию загрузки с последнего удачно загруженного тайла
    SessionLastSuccess: boolean;

    MapType: array of TMapType;

    // Полигон последнего выделения при операциях с областью.
    LastSelectionPolygon: TExtendedPointArray;
    // Масштаб, на котором было последнее выделение
    poly_zoom_save: byte;

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
    property MapCalibrationList: IInterfaceList read FMapCalibrationList;
    property KmlLoader: IKmlInfoSimpleLoader read FKmlLoader;
    property KmzLoader: IKmlInfoSimpleLoader read FKmzLoader;
    property MarksBitmapProvider: IBitmapLayerProvider read FMarksBitmapProvider;
    property MapTypeIcons18List: IMapTypeIconsList read FMapTypeIcons18List;
    property MapTypeIcons24List: IMapTypeIconsList read FMapTypeIcons24List;

    property GCThread: TGarbageCollectorThread read FGCThread;
    property ViewState: TMapViewPortState read FViewState;
    constructor Create;
    destructor Destroy; override;
    procedure LoadMaps;
    function GetMapFromID(id: TGUID): TMapType;
    procedure SaveMaps;
    procedure LoadMapIconsList;
    procedure IncrementDownloaded(ADwnSize: Currency; ADwnCnt: Cardinal);
    procedure StopAllThreads;
    procedure InitViewState(AMainMap: TMapType; AZoom: Byte; ACenterPos: TPoint; AScreenSize: TPoint);
    procedure LoadBitmapFromRes(const Name: String; Abmp: TCustomBitmap32);
    procedure LoadBitmapFromJpegRes(const Name: String; Abmp: TCustomBitmap32);
  end;

const
  SASVersion = '100830.alfa';
  CProgram_Lang_Default = LANG_RUSSIAN;

var
  GState: TGlobalState;

implementation

uses
  SysUtils,
  Dialogs,
  i_MapTypes,
  i_BitmapTileSaveLoad,
  i_IConfigDataProvider,
  u_ConfigDataProviderByVCLZip,
  u_ConfigDataProviderByKaZip,
  u_ConfigDataProviderByIniFile,
  u_MapTypeBasic,
  u_MapTypeListGeneratorFromFullListBasic,
  u_MapsConfigInIniFileSection,
  i_IListOfObjectsWithTTL,
  u_ListOfObjectsWithTTL,
  u_BitmapTypeExtManagerSimple,
  u_MapCalibrationListBasic,
  u_KmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_MapMarksBitmapLayerProviderStuped,
  u_MapTypeIconsList,
  UResStrings,
  u_TileFileNameGeneratorsSimpleList;

{ TGlobalState }

constructor TGlobalState.Create;
var
  VList: IListOfObjectsWithTTL;
  VConfigLoadSave: TMapsConfigInIniFileSection;
begin
  FDwnCS := TCriticalSection.Create;
  FCacheConfig := TGlobalCahceConfig.Create;
  All_Dwn_Kb := 0;
  All_Dwn_Tiles := 0;
  InetConnect := TInetConnect.Create;
  ProgramPath := ExtractFilePath(ParamStr(0));
  MainIni := TMeminifile.Create(MainConfigFileName);

  VConfigLoadSave := TMapsConfigInIniFileSection.Create(MainIni, 'MainViewMaps');
  FMapConfigSaver := VConfigLoadSave;
  FMapConfigLoader := VConfigLoadSave;

  FMemFileCache := TMemFileCache.Create;
  MainFileCache := FMemFileCache;
  FTileNameGenerator := TTileFileNameGeneratorsSimpleList.Create;
  FBitmapTypeManager := TBitmapTypeExtManagerSimple.Create;
  FMapCalibrationList := TMapCalibrationListBasic.Create;
  FKmlLoader := TKmlInfoSimpleParser.Create;
  FKmzLoader := TKmzInfoSimpleParser.Create;
  VList := TListOfObjectsWithTTL.Create;
  FGCThread := TGarbageCollectorThread.Create(VList, 1000);
  FMarksBitmapProvider := TMapMarksBitmapLayerProviderStuped.Create;
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
  FMapConfigSaver := nil;
  FMapConfigLoader := nil;
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
  FMarksBitmapProvider := nil;
  FMapTypeIcons18List := nil;
  FMapTypeIcons24List := nil;
  FreeAndNil(FViewState);
  FreeAllMaps;
  FreeAndNil(FCacheConfig);
  inherited;
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

function TGlobalState.GetMapFromID(id: TGUID): TMapType;
var
  i: integer;
  VMapType: TMapType;
begin
  Result := nil;
  for i := 0 to length(MapType) - 1 do begin
    VMapType := MapType[i];
    if IsEqualGUID(VMapType.GUID, id) then begin
      result := VMapType;
      exit;
    end;
  end;
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

procedure TGlobalState.LoadResources;
begin
  GOToSelIcon := TBitmap32.Create;
  GOToSelIcon.DrawMode := dmBlend;
  LoadBitmapFromRes('ICONIII', GOToSelIcon);
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
var
  loc: integer;
begin
  if SysLocale.PriLangID <> CProgram_Lang_Default then begin
    loc := LANG_ENGLISH;
  end else begin
    loc := CProgram_Lang_Default;
  end;
  Localization := MainIni.Readinteger('VIEW', 'localization', loc);
  WebReportToAuthor := MainIni.ReadBool('NPARAM', 'stat', true);
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

  for i := 0 to length(MapType) - 1 do begin
    VMapType := MapType[i];
    VList18.Add(VMapType.GUID, VMapType.bmp18);
    VList24.Add(VMapType.GUID, VMapType.bmp24);
  end;
end;

procedure TGlobalState.LoadMaps;
  function FindGUIDInFirstMaps(AGUID: TGUID; Acnt: Cardinal; out AMapType: TMapType): Boolean;
  var
    i: Integer;
    VMapType: TMapType;
  begin
    AMapType := nil;
    Result := false;
    if Acnt > 0 then begin
      for i := 0 to Acnt - 1 do begin
        VMapType := MapType[i];
        if IsEqualGUID(AGUID, VMapType.GUID) then begin
          Result := True;
          AMapType := VMapType;
          Break;
        end;
      end;
    end;
  end;

var
  Ini: TMeminifile;
  i, j, k, pnum: integer;
  startdir: string;
  SearchRec: TSearchRec;
  MTb: TMapType;
  VGUIDString: String;
  VMapType: TMapType;
  VMapTypeLoaded: TMapType;
  VMapOnlyCount: integer;
  VMapConfig: IConfigDataProvider;
  VLocalMapsConfig: IConfigDataProvider;
  VFileName: string;
begin
  SetLength(MapType, 0);
  CreateDir(MapsPath);
  Ini := TMeminiFile.Create(MapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataProviderByIniFile.Create(Ini);
  i := 0;
  pnum := 0;
  startdir := MapsPath;
  if FindFirst(startdir + '*.zmp', faAnyFile, SearchRec) = 0 then begin
    repeat
      inc(i);
    until FindNext(SearchRec) <> 0;
  end;
  VMapOnlyCount := 0;
  SysUtils.FindClose(SearchRec);
  SetLength(MapType, i);
  if FindFirst(startdir + '*.zmp', faAnyFile, SearchRec) = 0 then begin
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then begin
        continue;
      end;
      try
        VFileName := startdir + SearchRec.Name;
        VMapType := TMapType.Create;
        try
          VMapConfig := TConfigDataProviderByKaZip.Create(VFileName);
          VMapType.LoadMapType(VMapConfig, VLocalMapsConfig, pnum);
        except
          on E: EBadGUID do begin
            raise Exception.CreateResFmt(@SAS_ERR_MapGUIDError, [VFileName, E.Message]);
          end;
        end;
        VGUIDString := VMapType.GUIDString;
        if FindGUIDInFirstMaps(VMapType.GUID, pnum, VMapTypeLoaded) then begin
          raise Exception.CreateFmt('В файлах %0:s и %1:s одинаковые GUID', [VMapTypeLoaded.ZmpFileName, VFileName]);
        end;
      except
        if ExceptObject <> nil then begin
          ShowMessage((ExceptObject as Exception).Message);
        end;
        FreeAndNil(VMapType);
      end;
      if VMapType <> nil then begin
        MapType[pnum] := VMapType;
        if not VMapType.asLayer then begin
          Inc(VMapOnlyCount);
        end;
        inc(pnum);
      end;
    until FindNext(SearchRec) <> 0;
    SetLength(MapType, pnum);
    SysUtils.FindClose(SearchRec);
  end;
  if Length(MapType) = 0 then begin
    raise Exception.Create(SAS_ERR_NoMaps);
  end;
  if VMapOnlyCount = 0 then begin
    raise Exception.Create('Среди ZMP должна быть хотя бы одна карта');
  end;

  k := length(MapType) shr 1;
  while k > 0 do begin
    for i := 0 to length(MapType) - k - 1 do begin
      j := i;
      while (j >= 0) and (MapType[j].id > MapType[j + k].id) do begin
        MTb := MapType[j];
        MapType[j] := MapType[j + k];
        MapType[j + k] := MTb;
        if j > k then begin
          Dec(j, k);
        end else begin
          j := 0;
        end;
      end;
    end;
    k := k shr 1;
  end;
  for i := 0 to length(MapType) - 1 do begin
    MapType[i].id := i + 1;
  end;
end;

procedure TGlobalState.SetScreenSize(const Value: TPoint);
begin
  FScreenSize := Value;
end;

procedure TGlobalState.SaveMaps;
var
  Ini: TMeminifile;
  i: integer;
  VGUIDString: string;
  VMapType: TMapType;
begin
  Ini := TMeminiFile.Create(MapsPath + 'Maps.ini');
  try
    for i := 0 to length(MapType) - 1 do begin
      VMapType := MapType[i];
      VGUIDString := VMapType.GUIDString;
      ini.WriteInteger(VGUIDString, 'pnum', VMapType.id);


      if VMapType.UrlGenerator.URLBase <> VMapType.UrlGenerator.DefURLBase then begin
        ini.WriteString(VGUIDString, 'URLBase', VMapType.UrlGenerator.URLBase);
      end else begin
        Ini.DeleteKey(VGUIDString, 'URLBase');
      end;

      if VMapType.HotKey <> VMapType.DefHotKey then begin
        ini.WriteInteger(VGUIDString, 'HotKey', VMapType.HotKey);
      end else begin
        Ini.DeleteKey(VGUIDString, 'HotKey');
      end;

      if VMapType.TileStorage.CacheConfig.cachetype <> VMapType.TileStorage.CacheConfig.defcachetype then begin
        ini.WriteInteger(VGUIDString, 'CacheType', VMapType.TileStorage.CacheConfig.CacheType);
      end else begin
        Ini.DeleteKey(VGUIDString, 'CacheType');
      end;

      if VMapType.separator <> VMapType.Defseparator then begin
        ini.WriteBool(VGUIDString, 'separator', VMapType.separator);
      end else begin
        Ini.DeleteKey(VGUIDString, 'separator');
      end;

      if VMapType.TileStorage.CacheConfig.NameInCache <> VMapType.TileStorage.CacheConfig.DefNameInCache then begin
        ini.WriteString(VGUIDString, 'NameInCache', VMapType.TileStorage.CacheConfig.NameInCache);
      end else begin
        Ini.DeleteKey(VGUIDString, 'NameInCache');
      end;

      if VMapType.DownloaderFactory.WaitInterval <> VMapType.DefSleep then begin
        ini.WriteInteger(VGUIDString, 'Sleep', VMapType.DownloaderFactory.WaitInterval);
      end else begin
        Ini.DeleteKey(VGUIDString, 'Sleep');
      end;

      if VMapType.ParentSubMenu <> VMapType.DefParentSubMenu then begin
        ini.WriteString(VGUIDString, 'ParentSubMenu', VMapType.ParentSubMenu);
      end else begin
        Ini.DeleteKey(VGUIDString, 'ParentSubMenu');
      end;
    end;
    Ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TGlobalState.SetCacheElemensMaxCnt(const Value: integer);
begin
  FCacheElemensMaxCnt := Value;
  FMemFileCache.CacheElemensMaxCnt := FCacheElemensMaxCnt;
end;

procedure TGlobalState.StopAllThreads;
begin
  FGCThread.Terminate;
end;

procedure TGlobalState.InitViewState(AMainMap: TMapType; AZoom: Byte;
  ACenterPos, AScreenSize: TPoint);
var
  VMapsList: IMapTypeList;
  VLayersList: IMapTypeList;
  VListFactory: IMapTypeListFactory;
  VItemFactory: IMapTypeFactory;
begin
  if FViewState = nil then begin
    VItemFactory := TMapTypeBasicFactory.Create;

    VListFactory := TMapTypeListGeneratorFromFullListBasic.Create(True, VItemFactory);
    VMapsList := VListFactory.CreateList;

    VListFactory := TMapTypeListGeneratorFromFullListBasic.Create(False, VItemFactory);
    VLayersList := VListFactory.CreateList;

    FViewState := TMapViewPortState.Create(VMapsList, VLayersList, AMainMap, AZoom, ACenterPos, AScreenSize, FMapConfigSaver, FMapConfigLoader);
  end else begin
    raise Exception.Create('Повторная инициализация объекта состояния отображаемого окна карты');
  end;
end;

end.

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
  i_ILanguageManager,
  i_IMemObjCache,
  i_IConfigDataWriteProvider,
  i_IConfigDataProvider,
  i_ITileFileNameGeneratorsList,
  i_IBitmapTypeExtManager,
  i_IKmlInfoSimpleLoader,
  i_IBitmapLayerProvider,
  i_MapTypeIconsList,
  u_GarbageCollectorThread,
  u_GeoToStr,
  u_MapViewPortState,
  Uimgfun,
  UMapType,
  u_MemFileCache,
  u_GPSState,
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
    FCacheConfig: TGlobalCahceConfig;
    FMarksBitmapProvider: IBitmapLayerProvider;
    FMapTypeIcons18List: IMapTypeIconsList;
    FMapTypeIcons24List: IMapTypeIconsList;
    FLanguageManager: ILanguageManager;
    FMainConfigProvider: IConfigDataWriteProvider;
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
    procedure FreeAllMaps;
    procedure FreeMarkIcons;
    procedure SetScreenSize(const Value: TPoint);
    procedure SetCacheElemensMaxCnt(const Value: integer);
    procedure SaveLastSelectionPolygon;
    procedure LoadLastSelectionPolygon;
  public

    MainFileCache: IMemObjCache;
    // Ini-файл с основными настройками
    MainIni: TMeminifile;
    // Параметры программы
    ProgramPath: string;
    // Иконки для меток
    MarkIcons: TStringList;

    // Отображать окошко с логотипом при запуске
    Show_logo: Boolean;
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

    GPSpar: TGPSpar;

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
    property LanguageManager: ILanguageManager read FLanguageManager;
    property MainConfigProvider: IConfigDataWriteProvider read FMainConfigProvider;

    constructor Create;
    destructor Destroy; override;
    procedure LoadMaps;
    procedure LoadCacheConfig;
    function GetMapFromID(id: TGUID): TMapType;
    procedure SaveMaps;
    procedure SaveMainParams;
    procedure LoadMapIconsList;
    procedure IncrementDownloaded(ADwnSize: Currency; ADwnCnt: Cardinal);
    procedure StopAllThreads;
    procedure InitViewState(AMainMap: TMapType; AZoom: Byte; ACenterPos: TPoint; AScreenSize: TPoint);
    procedure LoadBitmapFromRes(const Name: String; Abmp: TCustomBitmap32);
    procedure LoadBitmapFromJpegRes(const Name: String; Abmp: TCustomBitmap32);
  end;

const
  SASVersion = '101019.alfa';

var
  GState: TGlobalState;

implementation

uses
  SysUtils,
  Dialogs,
  gnugettext,
  i_MapTypes,
  i_BitmapTileSaveLoad,
  u_ConfigDataProviderByKaZip,
  u_ConfigDataProviderByFolder,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile,
  u_MapTypeBasic,
  u_MapTypeListGeneratorFromFullListBasic,
  i_IFileNameIterator,
  u_ZmpFileNamesIteratorFactory,
  i_IListOfObjectsWithTTL,
  u_ListOfObjectsWithTTL,
  u_BitmapTypeExtManagerSimple,
  u_MapCalibrationListBasic,
  u_KmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_MapMarksBitmapLayerProviderStuped,
  u_MapTypeIconsList,
  u_LanguageManager,
  UResStrings,
  u_TileFileNameGeneratorsSimpleList;

{ TGlobalState }

constructor TGlobalState.Create;
var
  VList: IListOfObjectsWithTTL;
begin
  FDwnCS := TCriticalSection.Create;
  FCacheConfig := TGlobalCahceConfig.Create;
  All_Dwn_Kb := 0;
  All_Dwn_Tiles := 0;
  InetConnect := TInetConnect.Create;
  ProgramPath := ExtractFilePath(ParamStr(0));
  MainIni := TMeminifile.Create(MainConfigFileName);
  FMainConfigProvider := TConfigDataWriteProviderByIniFile.Create(MainIni);
  FLanguageManager := TLanguageManager.Create(MainIni);

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
  GPSpar := TGPSpar.Create;
  LoadMainParams;
  GPSpar.LoadConfig(MainConfigProvider);
  LoadMarkIcons;
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
  FreeAndNil(GPSpar);
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

procedure TGlobalState.LoadCacheConfig;
begin
  CacheConfig.DefCache:=MainIni.readinteger('VIEW','DefCache',2);
  CacheConfig.OldCpath:=MainIni.Readstring('PATHtoCACHE','GMVC','cache_old' + PathDelim);
  CacheConfig.NewCpath:=MainIni.Readstring('PATHtoCACHE','SASC','cache' + PathDelim);
  CacheConfig.ESCpath:=MainIni.Readstring('PATHtoCACHE','ESC','cache_ES' + PathDelim);
  CacheConfig.GMTilesPath:=MainIni.Readstring('PATHtoCACHE','GMTiles','cache_gmt' + PathDelim);
  CacheConfig.GECachePath:=MainIni.Readstring('PATHtoCACHE','GECache','cache_GE' + PathDelim);
end;

procedure TGlobalState.LoadLastSelectionPolygon;
var
  i: Integer;
begin
  i:=1;
  while str2r(MainIni.ReadString('HIGHLIGHTING','pointx_'+inttostr(i),'2147483647'))<>2147483647 do begin
    setlength(LastSelectionPolygon,i);
    LastSelectionPolygon[i-1].x:=str2r(MainIni.ReadString('HIGHLIGHTING','pointx_'+inttostr(i),'2147483647'));
    LastSelectionPolygon[i-1].y:=str2r(MainIni.ReadString('HIGHLIGHTING','pointy_'+inttostr(i),'2147483647'));
    inc(i);
  end;
  if length(LastSelectionPolygon)>0 then begin
    poly_zoom_save:=MainIni.Readinteger('HIGHLIGHTING','zoom',1);
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
begin
  WebReportToAuthor := MainIni.ReadBool('NPARAM', 'stat', true);
  Show_logo := MainIni.ReadBool('VIEW','Show_logo',true);
  FullScrean:= MainIni.Readbool('VIEW','FullScreen',false);
  TilesOut:=MainIni.readInteger('VIEW','TilesOut',0);
  InetConnect.userwinset:=MainIni.Readbool('INTERNET','userwinset',true);
  InetConnect.uselogin:=MainIni.Readbool('INTERNET','uselogin',false);
  InetConnect.Proxyused:=MainIni.Readbool('INTERNET','used_proxy',false);
  InetConnect.proxystr:=MainIni.Readstring('INTERNET','proxy','');
  InetConnect.loginstr:=MainIni.Readstring('INTERNET','login','');
  InetConnect.passstr:=MainIni.Readstring('INTERNET','password','');
  SaveTileNotExists:=MainIni.ReadBool('INTERNET','SaveTileNotExists', True);
  IgnoreTileNotExists:=MainIni.ReadBool('INTERNET','IgnoreTileNotExists',false);

  TwoDownloadAttempt:=MainIni.ReadBool('INTERNET','DblDwnl',true);
  GoNextTileIfDownloadError:=MainIni.ReadBool('INTERNET','GoNextTile',false);
  InetConnect.TimeOut:=MainIni.ReadInteger('INTERNET','TimeOut',40000);
  SessionLastSuccess:=MainIni.ReadBool('INTERNET','SessionLastSuccess',false);

  ShowMapName:=MainIni.readBool('VIEW','ShowMapNameOnPanel',true);
  ZoomingAtMousePos:=MainIni.readBool('VIEW','ZoomingAtMousePos',true);

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

  LastSelectionColor:=MainIni.Readinteger('VIEW','LastSelectionColor',clBlack);
  LastSelectionAlfa:=MainIni.Readinteger('VIEW','LastSelectionAlpha',210);

  CacheElemensMaxCnt:=MainIni.ReadInteger('VIEW','TilesOCache',150);
  ShowHintOnMarks:=MainIni.ReadBool('VIEW','ShowHintOnMarks',true);
  SrchType:=TSrchType(MainIni.ReadInteger('VIEW','SearchType',0));
  BGround:=MainIni.ReadInteger('VIEW','Background',clSilver);
  WikiMapMainColor:=MainIni.Readinteger('Wikimapia','MainColor',$FFFFFF);
  WikiMapFonColor:=MainIni.Readinteger('Wikimapia','FonColor',$000001);

  GammaN:=MainIni.Readinteger('COLOR_LEVELS','gamma',50);
  ContrastN:=MainIni.Readinteger('COLOR_LEVELS','contrast',0);
  InvertColor:=MainIni.ReadBool('COLOR_LEVELS','InvertColor',false);

  GSMpar.Port:=MainIni.ReadString('GSM','port','COM1');
  GSMpar.BaudRate:=MainIni.ReadInteger('GSM','BaudRate',4800);
  GSMpar.auto:=MainIni.ReadBool('GSM','Auto',true);
  GSMpar.WaitingAnswer:=MainIni.ReadInteger('GSM','WaitingAnswer',200);
  LoadLastSelectionPolygon;
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
var
  Ini: TMeminifile;
  i, j, k: integer;
  MTb: TMapType;
  VMapType: TMapType;
  VMapTypeLoaded: TMapType;
  VMapOnlyCount: integer;
  VMapConfig: IConfigDataProvider;
  VLocalMapsConfig: IConfigDataProvider;
  VFileName: WideString;
  VFullFileName: string;
  VMapTypeCount: integer;
  VFilesIteratorFactory: IFileNameIteratorFactory;
  VFilesIterator: IFileNameIterator;
begin
  SetLength(MapType, 0);
  CreateDir(MapsPath);
  Ini := TMeminiFile.Create(MapsPath + 'Maps.ini');
  VLocalMapsConfig := TConfigDataProviderByIniFile.Create(Ini);
  VMapOnlyCount := 0;
  VMapTypeCount := 0;
  VFilesIteratorFactory := TZmpFileNamesIteratorFactory.Create;
  VFilesIterator := VFilesIteratorFactory.CreateIterator(MapsPath, '');
  while VFilesIterator.Next(VFileName) do begin
    VFullFileName := VFilesIterator.GetRootFolderName + VFileName;
    try
      VMapType := TMapType.Create;
      if FileExists(VFullFileName) then begin
        VMapConfig := TConfigDataProviderByKaZip.Create(VFullFileName);
      end else begin
        VMapConfig := TConfigDataProviderByFolder.Create(VFullFileName);
      end;
      try
        VMapType.LoadMapType(VMapConfig, VLocalMapsConfig, VMapTypeCount);
      except
        on E: EBadGUID do begin
          raise Exception.CreateResFmt(@SAS_ERR_MapGUIDError, [VFileName, E.Message]);
        end;
      end;
      VMapTypeLoaded := GetMapFromID(VMapType.GUID);
      if VMapTypeLoaded <> nil then begin
        raise Exception.CreateFmt(SAS_ERR_MapGUIDDuplicate, [VMapTypeLoaded.ZmpFileName, VFullFileName]);
      end;
    except
      if ExceptObject <> nil then begin
        ShowMessage((ExceptObject as Exception).Message);
      end;
      FreeAndNil(VMapType);
    end;
    if VMapType <> nil then begin
      SetLength(MapType, VMapTypeCount + 1);
      MapType[VMapTypeCount] := VMapType;
      if not VMapType.asLayer then begin
        Inc(VMapOnlyCount);
      end;
      inc(VMapTypeCount);
    end;
  end;

  if Length(MapType) = 0 then begin
    raise Exception.Create(SAS_ERR_NoMaps);
  end;
  if VMapOnlyCount = 0 then begin
    raise Exception.Create(SAS_ERR_MainMapNotExists);
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

procedure TGlobalState.SaveLastSelectionPolygon;
var
  i: Integer;
begin
  i:=1;
  while MainIni.ReadString('HIGHLIGHTING','pointx_'+inttostr(i),'2147483647')<>'2147483647' do begin
    MainIni.DeleteKey('HIGHLIGHTING','pointx_'+inttostr(i));
    MainIni.DeleteKey('HIGHLIGHTING','pointy_'+inttostr(i));
    inc(i);
  end;
  if length(LastSelectionPolygon)>0 then begin
    MainIni.WriteInteger('HIGHLIGHTING','zoom',poly_zoom_save);
    for i := 0 to length(LastSelectionPolygon) - 1 do begin
      MainIni.WriteFloat('HIGHLIGHTING','pointx_'+inttostr(i+1),LastSelectionPolygon[i].x);
      MainIni.WriteFloat('HIGHLIGHTING','pointy_'+inttostr(i+1),LastSelectionPolygon[i].y);
    end;
  end;
end;

procedure TGlobalState.SaveMainParams;
var
  VZoom: Byte;
  VScreenCenterPos: TPoint;
begin
  ViewState.LockRead;
  try
    VZoom := ViewState.GetCurrentZoom;
    VScreenCenterPos := ViewState.GetCenterMapPixel;
  finally
    ViewState.UnLockRead;
  end;
  MainIni.WriteBool('VIEW','ShowMapNameOnPanel',ShowMapName);
  MainIni.WriteBool('VIEW','ZoomingAtMousePos',ZoomingAtMousePos);
  MainIni.WriteInteger('POSITION','zoom_size',VZoom + 1);
  MainIni.WriteInteger('POSITION','x',VScreenCenterPos.x);
  MainIni.WriteInteger('POSITION','y',VScreenCenterPos.y);
  MainIni.Writeinteger('VIEW','DefCache',CacheConfig.DefCache);
  MainIni.WriteInteger('VIEW','TilesOut',TilesOut);
  MainIni.Writeinteger('VIEW','grid', TileGridZoom);
  MainIni.Writebool('VIEW','invert_mouse',MouseWheelInv);
  MainIni.Writebool('VIEW','back_load',UsePrevZoom);
  MainIni.Writebool('VIEW','back_load_layer',UsePrevZoomLayer);
  MainIni.Writebool('VIEW','animate',AnimateZoom);
  MainIni.Writebool('VIEW','FullScreen',FullScrean);
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
  MainIni.Writeinteger('VIEW','LastSelectionColor',LastSelectionColor);
  MainIni.Writeinteger('VIEW','LastSelectionAlfa',LastSelectionAlfa);
  MainIni.WriteInteger('VIEW','SearchType',integer(SrchType));
  MainIni.WriteInteger('VIEW','Background',BGround);
  MainIni.Writeinteger('Wikimapia','MainColor',WikiMapMainColor);
  MainIni.Writeinteger('Wikimapia','FonColor',WikiMapFonColor);

  MainIni.Writeinteger('COLOR_LEVELS','gamma', GammaN);
  MainIni.Writeinteger('COLOR_LEVELS','contrast',ContrastN);
  MainIni.WriteBool('COLOR_LEVELS','InvertColor',InvertColor);

  MainIni.WriteString('GSM','port',GSMpar.Port);
  MainIni.WriteInteger('GSM','BaudRate',GSMpar.BaudRate);
  MainIni.WriteBool('GSM','Auto',GSMpar.auto);
  MainIni.WriteInteger('GSM','WaitingAnswer',GSMpar.WaitingAnswer);

  MainIni.Writestring('PATHtoCACHE','GMVC',CacheConfig.OldCpath);
  MainIni.Writestring('PATHtoCACHE','SASC',CacheConfig.NewCpath);
  MainIni.Writestring('PATHtoCACHE','ESC',CacheConfig.ESCpath);
  MainIni.Writestring('PATHtoCACHE','GMTiles',CacheConfig.GMTilesPath);
  MainIni.Writestring('PATHtoCACHE','GECache',CacheConfig.GECachePath);
  MainIni.Writebool('INTERNET','userwinset',InetConnect.userwinset);
  MainIni.Writebool('INTERNET','uselogin',InetConnect.uselogin);
  MainIni.Writebool('INTERNET','used_proxy',InetConnect.Proxyused);
  MainIni.Writestring('INTERNET','proxy',InetConnect.proxystr);
  MainIni.Writestring('INTERNET','login',InetConnect.loginstr);
  MainIni.Writestring('INTERNET','password',InetConnect.passstr);
  MainIni.WriteBool('INTERNET','SaveTileNotExists',SaveTileNotExists);
  MainIni.WriteBool('INTERNET','IgnoreTileNotExists',IgnoreTileNotExists);
  MainIni.WriteBool('INTERNET','DblDwnl',TwoDownloadAttempt);
  MainIni.Writebool('INTERNET','GoNextTile',GoNextTileIfDownloadError);
  MainIni.WriteInteger('INTERNET','TimeOut',InetConnect.TimeOut);
  MainIni.WriteBool('INTERNET','SessionLastSuccess',SessionLastSuccess);

  MainIni.Writebool('NPARAM','stat',WebReportToAuthor);
  GPSpar.SaveConfig(MainConfigProvider);
  SaveLastSelectionPolygon;
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
  GPSpar.SendTerminateToThreads;
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

    FViewState := TMapViewPortState.Create(VMapsList, VLayersList, AMainMap, AZoom, ACenterPos, AScreenSize);
  end else begin
    raise Exception.Create('Повторная инициализация объекта состояния отображаемого окна карты');
  end;
end;

end.

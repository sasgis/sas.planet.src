unit u_GlobalState;

interface

uses
  Graphics,
  t_GeoTypes,
  u_GeoToStr,
  Uimgfun;
type
  TInetConnect = record
    proxyused,userwinset,uselogin:boolean;
    proxystr,loginstr,passstr:string;
  end;

  TMarksShowType = (mshAll = 1, mshChecked = 2, mshNone = 3);

  TGlobalState = class
  private

  public
    // Параметры программы

    // Заходить на сайт автора при старте программы
    WebReportToAuthor: Boolean;

    // Способ отображения расстояний, и в частности масштаба
    num_format: TDistStrFormat;
    // Способ отображения координат в градусах
    llStrType: TDegrShowFormat;
    // Количество скачаных данных в килобайтах
    All_Dwn_Kb: Currency;

    InetConnect:TInetConnect;

    // Способ ресамплинга картинки
    Resampling: TTileResamplingType;
    //Способ храения кеша по-умолчанию.
    DefCache: byte;

    GPS_enab: Boolean;
    //Скорость GPS COM порта
    GPS_BaudRate: Integer;
    //COM-порт, к которому подключен GPS
    GPS_COM: string;
    //Поправка GPS
    GPS_Correction: TExtendedPoint;
    //Размер указателя направления при GPS-навигации
    GPS_ArrowSize: Integer;
    //Цвет указателя направления при навигацци
    GPS_ArrowColor: TColor;
    //Отображать GPS трек
    GPS_ShowPath: Boolean;
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

    BorderColor: TColor;
    BorderAlpha: byte;

    MapZapColor:TColor;
    MapZapAlpha:byte;

    InvertColor: boolean;

    show_point: TMarksShowType;
    FirstLat: Boolean;
    ShowMapName: Boolean;

    //Зацикливать карту по горизонтали
    CiclMap: Boolean;

    //Использовать тайлы предыдущих уровней для отображения
    UsePrevZoom: Boolean;
    //Инвертировать направление при зуме колесом мышки
    MouseWheelInv: Boolean;
    //Анимированный зум
    AnimateZoom: Boolean;
    //При отображении сетки тайлов выводить подписи
    ShowBorderText: Boolean;


    //Пути к кешам разных типов
    NewCPath_: string;
    OldCPath_: string;
    ESCpath_: string;
    GMTilespath_: string;
    GECachepath_: string;

    //????
    ShowHintOnMarks: Boolean;

    // Параетры касающиеся именно главного окна

    FullScrean: Boolean;

    // Текущий зумм
    zoom_size: byte;

    // Зум карты заполения
    zoom_mapzap: byte;


    constructor Create;

  end;

var
  GState: TGlobalState;
implementation

{ TGlobalState }

constructor TGlobalState.Create;
begin
  All_Dwn_Kb := 0;
end;

end.

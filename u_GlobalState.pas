unit u_GlobalState;

interface

uses
  Graphics,
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

    BorderColor: TColor;
    BorderAlpha: byte;

    MapZapColor:TColor;
    MapZapAlpha:byte;

    InvertColor: boolean;

    show_point: TMarksShowType;
    FirstLat: Boolean;
    ShowMapName: Boolean;
    //Использовать тайлы предыдущих уровней для отображения
    UsePrevZoom: Boolean;
    //Инвертировать направление при зуме колесом мышки
    MouseWheelInv: Boolean;
    //Анимированный зум
    AnimateZoom: Boolean;


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

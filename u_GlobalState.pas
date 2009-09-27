unit u_GlobalState;

interface

uses
  Graphics,
  u_GeoToStr,
  Uimgfun;
type
  TGlobalState = class
  private

  public
    // Способ отображения расстояний, и в частности масштаба
    num_format: TDistStrFormat;
    // Способ отображения координат в градусах
    llStrType: TDegrShowFormat;
    // Количество скачаных данных в килобайтах
    All_Dwn_Kb: Currency;
    // Способ ресамплинга картинки
    Resampling: TTileResamplingType;
    //Способ храения кеша по-умолчанию.
    DefCache: byte;

    GPS_enab: Boolean;

    BorderColor: TColor;
    BorderAlpha: byte;
    //????
    ShowHintOnMarks: Boolean;

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

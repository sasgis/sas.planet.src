unit u_GlobalState;

interface

uses
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

    Resampling: TTileResamplingType;

//    PWL:TResObj;

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

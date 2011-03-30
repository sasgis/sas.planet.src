unit u_SensorsFromGPSRecorder;

interface

uses
  i_GPSRecorder,
  u_SensorTextFromGPSRecorder;

type
  TSensorFromGPSRecorderLastSpeed = class(TSensorTextFromGPSRecorder)
  protected
    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
  public
    constructor Create(
      AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderAvgSpeed = class(TSensorTextFromGPSRecorder)
  protected
    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      AGPSRecorder: IGPSRecorder
    );
  end;

implementation

uses
  c_SensorsGUIDSimple,
  u_GeoToStr;

{ TSensorFromGPSRecorderLastSpeed }

constructor TSensorFromGPSRecorderLastSpeed.Create(AGPSRecorder: IGPSRecorder);
begin
  inherited Create(CSensorLastSpeedGUID, 'Скорость, км/ч:', 'Отображает текущую скорость движения', 'Скорость', False, AGPSRecorder);
end;

function TSensorFromGPSRecorderLastSpeed.GetValue: Double;
begin
  Result := GPSRecorder.LastSpeed;
end;

function TSensorFromGPSRecorderLastSpeed.ValueToText(AValue: Double): string;
begin
  Result := RoundEx(AValue, 2);
end;

{ TSensorFromGPSRecorderAvgSpeed }

constructor TSensorFromGPSRecorderAvgSpeed.Create(AGPSRecorder: IGPSRecorder);
begin
  inherited Create(CSensorAvgSpeedGUID, 'Скорость сред., км/ч:', 'Отображает среднюю скорость движения', 'Скорость средняя', True, AGPSRecorder);
end;

function TSensorFromGPSRecorderAvgSpeed.GetValue: Double;
begin
  Result := GPSRecorder.AvgSpeed;
end;

procedure TSensorFromGPSRecorderAvgSpeed.Reset;
begin
  inherited;
  GPSRecorder.ResetAvgSpeed;
end;

function TSensorFromGPSRecorderAvgSpeed.ValueToText(AValue: Double): string;
begin
  Result := RoundEx(AValue, 2);
end;

end.

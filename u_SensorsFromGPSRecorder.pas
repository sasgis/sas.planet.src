unit u_SensorsFromGPSRecorder;

interface

uses
  i_GPSRecorder,
  i_ValueToStringConverter,
  u_SensorTextFromGPSRecorder;

type
  TSensorFromGPSRecorderLastSpeed = class(TSensorTextFromGPSRecorder)
  protected
    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
  public
    constructor Create(
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderAvgSpeed = class(TSensorTextFromGPSRecorder)
  protected
    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderMaxSpeed = class(TSensorTextFromGPSRecorder)
  protected
    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderDist = class(TSensorTextFromGPSRecorder)
  protected
    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderOdometer1 = class(TSensorTextFromGPSRecorder)
  protected
    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderOdometer2 = class(TSensorTextFromGPSRecorder)
  protected
    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  c_SensorsGUIDSimple,
  u_GeoToStr;

{ TSensorFromGPSRecorderLastSpeed }

constructor TSensorFromGPSRecorderLastSpeed.Create(
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorLastSpeedGUID,
    'Скорость, км/ч:',
    'Отображает текущую скорость движения',
    'Скорость',
    False,
    AGPSRecorder,
    AValueConverterConfig
  );
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

constructor TSensorFromGPSRecorderAvgSpeed.Create(
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorAvgSpeedGUID,
    'Скорость сред., км/ч:',
    'Отображает среднюю скорость движения',
    'Скорость средняя',
    True,
    AGPSRecorder,
    AValueConverterConfig
  );
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

{ TSensorFromGPSRecorderMaxSpeed }

constructor TSensorFromGPSRecorderMaxSpeed.Create(
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorMaxSpeedGUID,
    'Скорость макс., км/ч:',
    'Отображает максимальную скорость движения',
    'Скорость максимальная',
    True,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderMaxSpeed.GetValue: Double;
begin
  Result := GPSRecorder.MaxSpeed;
end;

procedure TSensorFromGPSRecorderMaxSpeed.Reset;
begin
  inherited;
  GPSRecorder.ResetMaxSpeed;
end;

function TSensorFromGPSRecorderMaxSpeed.ValueToText(AValue: Double): string;
begin
  Result := RoundEx(AValue, 2);
end;

{ TSensorFromGPSRecorderDist }

constructor TSensorFromGPSRecorderDist.Create(
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorDistGUID,
    'Пройденный путь:',
    'Отображает пройденный путь считаемый от подключения к GPS-приемнику',
    'Пройденный путь',
    True,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderDist.GetValue: Double;
begin
  Result := GPSRecorder.Dist;
end;

procedure TSensorFromGPSRecorderDist.Reset;
begin
  inherited;
  GPSRecorder.ResetDist;
end;

function TSensorFromGPSRecorderDist.ValueToText(AValue: Double): string;
begin
  Result := ValueConverter.DistConvert(AValue)
end;

{ TSensorFromGPSRecorderOdometer1 }

constructor TSensorFromGPSRecorderOdometer1.Create(AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig);
begin
  inherited Create(
    CSensorDistGUID,
    'Одометр, км:',
    'Отображает весь пройденный путь',
    'Одометр',
    True,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderOdometer1.GetValue: Double;
begin
  Result := GPSRecorder.Odometer1;
end;

procedure TSensorFromGPSRecorderOdometer1.Reset;
begin
  inherited;
  GPSRecorder.ResetOdometer1;
end;

function TSensorFromGPSRecorderOdometer1.ValueToText(AValue: Double): string;
begin
  Result := ValueConverter.DistConvert(AValue)
end;

{ TSensorFromGPSRecorderOdometer2 }

constructor TSensorFromGPSRecorderOdometer2.Create(AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig);
begin
  inherited Create(
    CSensorDistGUID,
    'Одометр №2, км:',
    'Отображает весь пройденный путь',
    'Одометр №2',
    True,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderOdometer2.GetValue: Double;
begin
  Result := GPSRecorder.Odometer2;
end;

procedure TSensorFromGPSRecorderOdometer2.Reset;
begin
  inherited;
  GPSRecorder.ResetOdometer2;
end;

function TSensorFromGPSRecorderOdometer2.ValueToText(AValue: Double): string;
begin
  Result := ValueConverter.DistConvert(AValue)
end;

end.

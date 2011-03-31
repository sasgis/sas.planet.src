unit u_SensorListGeneratorStuped;

interface

uses
  Classes,
  i_GPSRecorder,
  i_ValueToStringConverter,
  i_SensorListGenerator;

type
  TSensorListGeneratorStuped = class(TInterfacedObject, ISensorListGenerator)
  private
    FGPSRecorder: IGPSRecorder;
    FValueConverterConfig: IValueToStringConverterConfig;
  protected
    function CreateSensorsList: IInterfaceList;
  public
    constructor Create(
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  i_Sensor,
  u_SensorsFromGPSRecorder;

{ TSensorListGeneratorStuped }

constructor TSensorListGeneratorStuped.Create(
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
);
begin
  FGPSRecorder := AGPSRecorder;
  FValueConverterConfig := AValueConverterConfig;
end;

function TSensorListGeneratorStuped.CreateSensorsList: IInterfaceList;
var
  VSensor: ISensorText;
begin
  Result := TInterfaceList.Create;

  VSensor := TSensorFromGPSRecorderLastSpeed.Create(FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderAvgSpeed.Create(FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderMaxSpeed.Create(FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderDist.Create(FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderOdometer1.Create(FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderOdometer2.Create(FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderAltitude.Create(FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderHeading.Create(FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);
end;

end.

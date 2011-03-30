unit u_SensorListGeneratorStuped;

interface

uses
  i_GUIDList,
  i_GPSRecorder,
  i_SensorListGenerator;

type
  TSensorListGeneratorStuped = class(TInterfacedObject, ISensorListGenerator)
  private
    FGPSRecorder: IGPSRecorder;
  protected
    function CreateSensorsList: IGUIDInterfaceList;
  public
    constructor Create(AGPSRecorder: IGPSRecorder);
  end;

implementation

uses
  u_GUIDInterfaceList,
  i_Sensor,
  u_SensorsFromGPSRecorder;

{ TSensorListGeneratorStuped }

constructor TSensorListGeneratorStuped.Create(AGPSRecorder: IGPSRecorder);
begin
  FGPSRecorder := AGPSRecorder;
end;

function TSensorListGeneratorStuped.CreateSensorsList: IGUIDInterfaceList;
var
  VSensor: ISensorText;
begin
  Result := TGUIDInterfaceList.Create;

  VSensor := TSensorFromGPSRecorderLastSpeed.Create(FGPSRecorder);
  Result.Add(VSensor.GetGUID, VSensor);

  VSensor := TSensorFromGPSRecorderAvgSpeed.Create(FGPSRecorder);
  Result.Add(VSensor.GetGUID, VSensor);
end;

end.

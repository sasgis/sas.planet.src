unit u_SensorListGeneratorStuped;

interface

uses
  Classes,
  i_GPSRecorder,
  i_SensorListGenerator;

type
  TSensorListGeneratorStuped = class(TInterfacedObject, ISensorListGenerator)
  private
    FGPSRecorder: IGPSRecorder;
  protected
    function CreateSensorsList: IInterfaceList;
  public
    constructor Create(AGPSRecorder: IGPSRecorder);
  end;

implementation

uses
  i_Sensor,
  u_SensorsFromGPSRecorder;

{ TSensorListGeneratorStuped }

constructor TSensorListGeneratorStuped.Create(AGPSRecorder: IGPSRecorder);
begin
  FGPSRecorder := AGPSRecorder;
end;

function TSensorListGeneratorStuped.CreateSensorsList: IInterfaceList;
var
  VSensor: ISensorText;
begin
  Result := TInterfaceList.Create;

  VSensor := TSensorFromGPSRecorderLastSpeed.Create(FGPSRecorder);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderAvgSpeed.Create(FGPSRecorder);
  Result.Add(VSensor);
end;

end.

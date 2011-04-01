unit u_SensorListGeneratorStuped;

interface

uses
  Classes,
  i_NavigationToPoint,
  i_ViewPortState,
  i_GPSRecorder,
  i_ValueToStringConverter,
  i_LanguageManager,
  i_SensorListGenerator;

type
  TSensorListGeneratorStuped = class(TInterfacedObject, ISensorListGenerator)
  private
    FLanguageManager: ILanguageManager;
    FGPSRecorder: IGPSRecorder;
    FViewPortState: IViewPortState;
    FNavigationToPoint: INavigationToPoint;
    FValueConverterConfig: IValueToStringConverterConfig;
  protected
    function CreateSensorsList: IInterfaceList;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AViewPortState: IViewPortState;
      ANavigationToPoint: INavigationToPoint;
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  i_Sensor,
  u_SensorTextFromNavToPoint,
  u_SensorBatteryStatus,
  u_SensorsFromGPSRecorder;

{ TSensorListGeneratorStuped }

constructor TSensorListGeneratorStuped.Create(
  ALanguageManager: ILanguageManager;
  AViewPortState: IViewPortState;
  ANavigationToPoint: INavigationToPoint;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
);
begin
  FLanguageManager := ALanguageManager;
  FViewPortState := AViewPortState;
  FNavigationToPoint := ANavigationToPoint;
  FGPSRecorder := AGPSRecorder;
  FValueConverterConfig := AValueConverterConfig;
end;

function TSensorListGeneratorStuped.CreateSensorsList: IInterfaceList;
var
  VSensor: ISensorText;
begin
  Result := TInterfaceList.Create;

  VSensor := TSensorFromGPSRecorderLastSpeed.Create(FLanguageManager, FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderAvgSpeed.Create(FLanguageManager, FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderMaxSpeed.Create(FLanguageManager, FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderDist.Create(FLanguageManager, FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderOdometer1.Create(FLanguageManager, FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderOdometer2.Create(FLanguageManager, FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorTextFromNavToPoint.Create(FLanguageManager, FViewPortState, FNavigationToPoint, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorBatteryStatus.Create(FLanguageManager);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderAltitude.Create(FLanguageManager, FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);

  VSensor := TSensorFromGPSRecorderHeading.Create(FLanguageManager, FGPSRecorder, FValueConverterConfig);
  Result.Add(VSensor);
end;

end.

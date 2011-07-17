unit u_SensorListStuped;

interface

uses
  i_NavigationToPoint,
  i_ViewPortState,
  i_GPSRecorder,
  i_ValueToStringConverter,
  i_LanguageManager,
  u_SensorListBase;

type
  TSensorListStuped = class(TSensorListBase)
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
  i_SensorList,
  u_SensorTextFromNavToPoint,
  u_SensorBatteryStatus,
  u_SensorsFromGPSRecorder;


{ TSensorListStuped }

constructor TSensorListStuped.Create(
  ALanguageManager: ILanguageManager;
  AViewPortState: IViewPortState;
  ANavigationToPoint: INavigationToPoint;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
);
var
  VEntity: ISensorListEntity;
begin
  inherited Create;
  VEntity := TSensorFromGPSRecorderLastSpeed.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderAvgSpeed.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderMaxSpeed.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderDist.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderOdometer1.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderOdometer2.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorTextFromNavToPoint.Create(ALanguageManager, AViewPortState, ANavigationToPoint, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorBatteryStatus.Create(ALanguageManager);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderAltitude.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderHeading.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);
end;

end.

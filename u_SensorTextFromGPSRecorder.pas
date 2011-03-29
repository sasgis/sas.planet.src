unit u_SensorTextFromGPSRecorder;

interface

uses
  i_JclListenerNotifierLinksList,
  i_GPSRecorder,
  i_Sensor;

type
  TSensorTextFromGPSRecorder = class(TInterfacedObject, ISensor)
  private
    FInfo: ISensorInfo;
    FView: ISensorView;
    FGPSRecorder: IGPSRecorder;

    FLinksList: IJclListenerNotifierLinksList;

    procedure OnResetSensor(Sender: TObject);
    procedure OnRecorderChanged(Sender: TObject);
  protected
    function GetInfo: ISensorInfo;

    function GetView: ISensorView;
    procedure SetView(AView: ISensorView);
  public
    constructor Create(AInfo: ISensorInfo; AGPSRecorder: IGPSRecorder);
    destructor Destroy; override;
  end;

implementation

uses
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener;

{ TSensorTextFromGPSRecorder }

constructor TSensorTextFromGPSRecorder.Create(AInfo: ISensorInfo;
  AGPSRecorder: IGPSRecorder);
begin
  FInfo := AInfo;
  FGPSRecorder := AGPSRecorder;

  FLinksList := TJclListenerNotifierLinksList.Create;


end;

destructor TSensorTextFromGPSRecorder.Destroy;
begin

  inherited;
end;

function TSensorTextFromGPSRecorder.GetInfo: ISensorInfo;
begin

end;

function TSensorTextFromGPSRecorder.GetView: ISensorView;
begin

end;

procedure TSensorTextFromGPSRecorder.OnRecorderChanged(Sender: TObject);
begin

end;

procedure TSensorTextFromGPSRecorder.OnResetSensor(Sender: TObject);
begin

end;

procedure TSensorTextFromGPSRecorder.SetView(AView: ISensorView);
begin

end;

end.

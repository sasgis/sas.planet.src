unit u_SensorTextFromGPSRecorder;

interface

uses
  SyncObjs,
  i_JclNotify,
  i_JclListenerNotifierLinksList,
  i_GPSRecorder,
  i_Sensor;

type
  TSensorTextFromGPSRecorder = class(TInterfacedObject, ISensor)
  private
    FInfo: ISensorInfo;
    FGPSRecorder: IGPSRecorder;

    FViewText: ISensorViewText;
    FCS: TCriticalSection;
    FViewResetListener: IJclListener;
    FLinksList: IJclListenerNotifierLinksList;

    FLastValue: Double;
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
  SysUtils,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener,
  u_GeoToStr;

{ TSensorTextFromGPSRecorder }

constructor TSensorTextFromGPSRecorder.Create(AInfo: ISensorInfo;
  AGPSRecorder: IGPSRecorder);
begin
  FInfo := AInfo;
  FGPSRecorder := AGPSRecorder;

  FCS := TCriticalSection.Create;
  FLinksList := TJclListenerNotifierLinksList.Create;

  FViewResetListener := TNotifyEventListener.Create(Self.OnResetSensor);
end;

destructor TSensorTextFromGPSRecorder.Destroy;
var
  VNotifier: IJclNotifier;
begin
  if FViewText <> nil then begin
    if FInfo.CanReset then begin
      VNotifier := FViewText.GetResetNotifier;
      if VNotifier <> nil then begin
        VNotifier.Remove(FViewResetListener);
      end;
    end;
    FViewText := nil;
  end;

  FreeAndNil(FCS);
  inherited;
end;

function TSensorTextFromGPSRecorder.GetInfo: ISensorInfo;
begin
  Result := FInfo;
end;

function TSensorTextFromGPSRecorder.GetView: ISensorView;
begin
  FCS.Acquire;
  try
    Result := FViewText;
  finally
    FCS.Release;
  end;
end;

procedure TSensorTextFromGPSRecorder.OnRecorderChanged(Sender: TObject);
var
  VValue: Double;
  VNeedUpdate: Boolean;
  VStr: string;
  VView: ISensorViewText;
begin
  VValue := FGPSRecorder.LastSpeed;
  VNeedUpdate := False;
  FCS.Acquire;
  try
    VView := FViewText;
    if VView <> nil then begin
      if Abs(FLastValue - VValue) > 0.001 then begin
        FLastValue := VValue;
        VNeedUpdate := True;
      end;
    end;
  finally
    FCS.Release;
  end;
  if VNeedUpdate then begin
    VStr := RoundEx(VValue, 2);
    VView.SetText(VStr);
  end;
end;

procedure TSensorTextFromGPSRecorder.OnResetSensor(Sender: TObject);
begin
end;

procedure TSensorTextFromGPSRecorder.SetView(AView: ISensorView);
var
  VNotifier: IJclNotifier;
  VView: ISensorViewText;
begin
  FCS.Acquire;
  try
    if Supports(AView, ISensorViewText, VView) then begin
      if FViewText <> nil then begin
        if FInfo.CanReset then begin
          VNotifier := FViewText.GetResetNotifier;
          if VNotifier <> nil then begin
            VNotifier.Remove(FViewResetListener);
          end;
        end;
      end;
      FViewText := VView;
      if FViewText <> nil then begin
        if FInfo.CanReset then begin
          VNotifier := FViewText.GetResetNotifier;
          if VNotifier <> nil then begin
            VNotifier.Add(FViewResetListener);
          end;
        end;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

end.

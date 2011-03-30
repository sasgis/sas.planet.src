unit u_SensorTextFromGPSRecorder;

interface

uses
  i_GPSRecorder,
  i_Sensor,
  u_SensorBase;

type
  TSensorTextFromGPSRecorder = class(TSensorBase, ISensorText)
  private
    FGPSRecorder: IGPSRecorder;
    FLastValue: Double;
    procedure OnRecorderChanged(Sender: TObject);
  protected
    property GPSRecorder: IGPSRecorder read FGPSRecorder;

    function ValueToText(AValue: Double): string; virtual; abstract;
    function GetValue: Double; virtual; abstract;
  protected
    function GetText: string;
  public
    constructor Create(
      AGUID: TGUID;
      ACaption: string;
      ADescription: string;
      AMenuItemName: string;
      ACanReset: Boolean;
      AGPSRecorder: IGPSRecorder
    );
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener;

{ TSensorTextFromGPSRecorder }

constructor TSensorTextFromGPSRecorder.Create(
  AGUID: TGUID;
  ACaption: string;
  ADescription: string;
  AMenuItemName: string;
  ACanReset: Boolean;
  AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(AGUID, ACaption, ADescription, AMenuItemName, ACanReset, ISensorText);
  FGPSRecorder := AGPSRecorder;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnRecorderChanged),
    FGPSRecorder.GetChangeNotifier
  );
end;

function TSensorTextFromGPSRecorder.GetText: string;
var
  VValue: Double;
begin
  LockRead;
  try
    VValue := FLastValue;
  finally
    UnlockRead;
  end;
  Result := ValueToText(VValue);
end;

procedure TSensorTextFromGPSRecorder.OnRecorderChanged(Sender: TObject);
var
  VValue: Double;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := GetValue;
  LockWrite;
  try
    if Abs(FLastValue - VValue) > 0.001 then begin
      FLastValue := VValue;
      VNeedNotify := True;
    end;
  finally
    UnlockWrite;
  end;
  if VNeedNotify then begin
    NotifyDataUpdate;
  end;
end;

end.

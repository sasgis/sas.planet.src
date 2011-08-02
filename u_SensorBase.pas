unit u_SensorBase;

interface

uses
  i_JclNotify,
  i_LanguageManager,
  i_Sensor,
  i_SensorList,
  u_UserInterfaceItemBase,
  u_ConfigDataElementBase;

type
  TSensorBase = class(TUserInterfaceItemBase, ISensor, ISensorListEntity)
  private
    FCanReset: Boolean;
    FSensorTypeIID: TGUID;

    FDataUpdateNotifier: IJclNotifier;
  protected
    procedure NotifyDataUpdate;
  protected
    function CanReset: Boolean;
    procedure Reset; virtual;
    function GetSensorTypeIID: TGUID;
    function GetDataUpdateNotifier: IJclNotifier;
    function GetSensor: ISensor;
  public
    constructor Create(
      AGUID: TGUID;
      ACanReset: Boolean;
      ASensorTypeIID: TGUID;
      ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  u_JclNotify;

{ TSensorBase }

constructor TSensorBase.Create(
  AGUID: TGUID;
  ACanReset: Boolean;
  ASensorTypeIID: TGUID;
  ALanguageManager: ILanguageManager
);
begin
  inherited Create(AGUID, ALanguageManager);
  FCanReset := ACanReset;
  FSensorTypeIID := ASensorTypeIID;

  FDataUpdateNotifier := TJclBaseNotifier.Create;
end;

function TSensorBase.CanReset: Boolean;
begin
  Result := FCanReset;
end;

function TSensorBase.GetDataUpdateNotifier: IJclNotifier;
begin
  Result := FDataUpdateNotifier;
end;

function TSensorBase.GetSensor: ISensor;
begin
  Result := Self;
end;

function TSensorBase.GetSensorTypeIID: TGUID;
begin
  Result := FSensorTypeIID;
end;

procedure TSensorBase.NotifyDataUpdate;
begin
  FDataUpdateNotifier.Notify(nil);
end;

procedure TSensorBase.Reset;
begin
end;

end.

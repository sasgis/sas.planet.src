unit u_SensorViewConfigSimple;

interface

uses
  i_Sensor,
  u_ConfigDataElementBase;

type
  TSensorViewConfigSimple = class(TConfigDataElementBaseEmptySaveLoad, ISensorViewConfig)
  private
    FVisible: Boolean;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create;
  end;


implementation

{ TSensorViewConfigSimple }

constructor TSensorViewConfigSimple.Create;
begin
  inherited;
  FVisible := True;
end;

function TSensorViewConfigSimple.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TSensorViewConfigSimple.SetVisible(AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

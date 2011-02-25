unit u_GPSModuleByCOMPortConfigSatic;

interface

uses
  i_IGPSModuleByCOMPortSettings;

type
  TGPSModuleByCOMPortConfigSatic = class(TInterfacedObject, IGPSModuleByCOMPortSettings)
  private
    FPort: Integer;
    FBaudRate: Integer;
    FConnectionTimeout: Integer;
    FDelay: Integer;
    FNMEALog: Boolean;
    FLogPath: WideString;
  protected
    function GetPort: Integer; safecall;
    function GetBaudRate: Integer; safecall;
    function GetConnectionTimeout: Integer; safecall;
    function GetDelay: Integer; safecall;
    function GetNMEALog: Boolean; safecall;
    function GetLogPath: WideString; safecall;
  public
    constructor Create(
      APort: Integer;
      ABaudRate: Integer;
      AConnectionTimeout: Integer;
      ADelay: Integer;
      ANMEALog: Boolean;
      ALogPath: WideString
    );
  end;

implementation

{ TGPSModuleByCOMPortConfigSatic }

constructor TGPSModuleByCOMPortConfigSatic.Create(
  APort,
  ABaudRate,
  AConnectionTimeout,
  ADelay: Integer;
  ANMEALog: Boolean;
  ALogPath: WideString
);
begin
  inherited Create;
  FPort := APort;
  FBaudRate := ABaudRate;
  FConnectionTimeout := AConnectionTimeout;
  FDelay := ADelay;
  FNMEALog := ANMEALog;
  FLogPath := ALogPath;
end;

function TGPSModuleByCOMPortConfigSatic.GetBaudRate: Integer;
begin
  Result := FBaudRate;
end;

function TGPSModuleByCOMPortConfigSatic.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

function TGPSModuleByCOMPortConfigSatic.GetDelay: Integer;
begin
  Result := FDelay;
end;

function TGPSModuleByCOMPortConfigSatic.GetLogPath: WideString;
begin
  Result := FLogPath;
end;

function TGPSModuleByCOMPortConfigSatic.GetNMEALog: Boolean;
begin
  Result := FNMEALog;
end;

function TGPSModuleByCOMPortConfigSatic.GetPort: Integer;
begin
  Result := FPort;
end;

end.

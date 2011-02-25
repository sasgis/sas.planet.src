unit u_GPSModuleByCOMPortSettings;

interface

uses
  i_IGPSModuleByCOMPortSettings;

type
  TGPSModuleByCOMPortSettings = class(TInterfacedObject, IGPSModuleByCOMPortSettings)
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

{ TGPSModuleByCOMPortSettings }

constructor TGPSModuleByCOMPortSettings.Create(
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

function TGPSModuleByCOMPortSettings.GetBaudRate: Integer;
begin
  Result := FBaudRate;
end;

function TGPSModuleByCOMPortSettings.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

function TGPSModuleByCOMPortSettings.GetDelay: Integer;
begin
  Result := FDelay;
end;

function TGPSModuleByCOMPortSettings.GetLogPath: WideString;
begin
  Result := FLogPath;
end;

function TGPSModuleByCOMPortSettings.GetNMEALog: Boolean;
begin
  Result := FNMEALog;
end;

function TGPSModuleByCOMPortSettings.GetPort: Integer;
begin
  Result := FPort;
end;

end.

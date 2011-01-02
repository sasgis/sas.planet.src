unit u_GPSModuleByCOMPortSettings;

interface

uses
  i_JclNotify,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IGPSModuleByCOMPortSettings,
  u_ConfigDataElementBase;

type
  TGPSModuleByCOMPortSettings = class(TConfigDataElementBase, IGPSModuleByCOMPortConfig)
  private
    FPort: Integer;
    FBaudRate: Integer;
    FConnectionTimeout: Integer;
    FDelay: Integer;
    FNMEALog: Boolean;
    FLogPath: WideString;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
    procedure SetChanged; override;
  protected
    function GetPort: Integer;
    procedure SetPort(AValue: Integer);

    function GetBaudRate: Integer;
    procedure SetBaudRate(AValue: Integer);

    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(AValue: Integer);

    function GetDelay: Integer;
    procedure SetDelay(AValue: Integer);

    function GetNMEALog: Boolean;
    procedure SetNMEALog(AValue: Boolean);

    function GetLogPath: WideString;

    function GetStatic: IGPSModuleByCOMPortConfigSatic;
  public
    constructor Create;
  end;

implementation

uses
  u_JclNotify,
  u_GlobalState;

{ TGPSModuleByCOMPortSettings }

constructor TGPSModuleByCOMPortSettings.Create;
begin
  FPort := 0;
  FBaudRate := 4800;
  FConnectionTimeout := 300;
  FDelay := 1000;
  FNMEALog := False;
end;

procedure TGPSModuleByCOMPortSettings.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  FLogPath := GState.TrackLogPath;
  if AConfigData <> nil then begin
    FPort := AConfigData.ReadInteger('COM', FPort);
    FBaudRate := AConfigData.ReadInteger('BaudRate', FBaudRate);
    FConnectionTimeout := AConfigData.ReadInteger('timeout', FConnectionTimeout);
    FDelay := AConfigData.ReadInteger('update', FDelay);
    FNMEALog := AConfigData.ReadBool('NMEAlog', FNMEALog);
  end;
end;

procedure TGPSModuleByCOMPortSettings.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('COM', FPort);
  AConfigData.WriteInteger('BaudRate', FBaudRate);
  AConfigData.WriteInteger('timeout', FConnectionTimeout);
  AConfigData.WriteInteger('update', FDelay);
  AConfigData.WriteBool('NMEAlog', FNMEALog);
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

function TGPSModuleByCOMPortSettings.GetStatic: IGPSModuleByCOMPortConfigSatic;
begin

end;

procedure TGPSModuleByCOMPortSettings.SetBaudRate(AValue: Integer);
begin
  if FBaudRate <> AValue then begin
    FBaudRate := AValue;
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetChanged;
begin
  inherited;

end;

procedure TGPSModuleByCOMPortSettings.SetConnectionTimeout(AValue: Integer);
begin
  if FConnectionTimeout <> AValue then begin
    FConnectionTimeout := AValue;
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetDelay(AValue: Integer);
begin
  if FDelay <> AValue then begin
    FDelay := AValue;
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetNMEALog(AValue: Boolean);
begin
  if FNMEALog <> AValue then begin
    FNMEALog := AValue;
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetPort(AValue: Integer);
begin
  if FPort <> AValue then begin
    FPort := AValue;
  end;
end;

end.

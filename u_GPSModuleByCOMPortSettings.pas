unit u_GPSModuleByCOMPortSettings;

interface

uses
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
    FStatic: IGPSModuleByCOMPortConfigSatic;
    function CreateStatic: IGPSModuleByCOMPortConfigSatic;
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
    constructor Create(ALogPath: string);
  end;

implementation

uses
  u_GPSModuleByCOMPortConfigSatic;

{ TGPSModuleByCOMPortSettings }

constructor TGPSModuleByCOMPortSettings.Create(ALogPath: string);
begin
  inherited Create;
  FLogPath := ALogPath;
  FPort := 1;
  FBaudRate := 4800;
  FConnectionTimeout := 300;
  FDelay := 1000;
  FNMEALog := False;
  FStatic := CreateStatic;
end;

function TGPSModuleByCOMPortSettings.CreateStatic: IGPSModuleByCOMPortConfigSatic;
begin
  Result :=
    TGPSModuleByCOMPortConfigSatic.Create(
      FPort,
      FBaudRate,
      FConnectionTimeout,
      FDelay,
      FNMEALog,
      FLogPath
    );
end;

procedure TGPSModuleByCOMPortSettings.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetPort(AConfigData.ReadInteger('COM', FPort));
    SetBaudRate(AConfigData.ReadInteger('BaudRate', FBaudRate));
    SetConnectionTimeout(AConfigData.ReadInteger('timeout', FConnectionTimeout));
    SetDelay(AConfigData.ReadInteger('update', FDelay));
    SetNMEALog(AConfigData.ReadBool('NMEAlog', FNMEALog));
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
  LockRead;
  try
    Result := FBaudRate;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortSettings.GetConnectionTimeout: Integer;
begin
  LockRead;
  try
    Result := FConnectionTimeout;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortSettings.GetDelay: Integer;
begin
  LockRead;
  try
    Result := FDelay;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortSettings.GetLogPath: WideString;
begin
  LockRead;
  try
    Result := FLogPath;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortSettings.GetNMEALog: Boolean;
begin
  LockRead;
  try
    Result := FNMEALog;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortSettings.GetPort: Integer;
begin
  LockRead;
  try
    Result := FPort;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortSettings.GetStatic: IGPSModuleByCOMPortConfigSatic;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetBaudRate(AValue: Integer);
begin
  if (AValue > 0) and (AValue <= 1000000) then begin
    LockWrite;
    try
      if FBaudRate <> AValue then begin
        FBaudRate := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetChanged;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetConnectionTimeout(AValue: Integer);
begin
  if (AValue >= 0) and (AValue <= 600) then begin
    LockWrite;
    try
      if FConnectionTimeout <> AValue then begin
        FConnectionTimeout := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetDelay(AValue: Integer);
begin
  if (AValue >= 0) and (AValue <= 300000) then begin
    LockWrite;
    try
      if FDelay <> AValue then begin
        FDelay := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetNMEALog(AValue: Boolean);
begin
  LockWrite;
  try
    if FNMEALog <> AValue then begin
      FNMEALog := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSModuleByCOMPortSettings.SetPort(AValue: Integer);
begin
  if (AValue >= 1) and (AValue <= 255) then begin
    LockWrite;
    try
      if FPort <> AValue then begin
        FPort := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

end.

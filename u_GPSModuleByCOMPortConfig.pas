unit u_GPSModuleByCOMPortConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GPSModuleByCOMPortSettings,
  i_GPSModuleByCOMPortConfig,
  u_ConfigDataElementBase;

type
  TGPSModuleByCOMPortConfig = class(TConfigDataElementBase, IGPSModuleByCOMPortConfig)
  private
    FPort: Integer;
    FBaudRate: Integer;
    FConnectionTimeout: Integer;
    FDelay: Integer;
    FNMEALog: Boolean;
    FLogPath: WideString;
    FStatic: IGPSModuleByCOMPortSettings;
    function CreateStatic: IGPSModuleByCOMPortSettings;
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

    function GetStatic: IGPSModuleByCOMPortSettings;
  public
    constructor Create(ALogPath: string);
  end;

implementation

uses
  u_GPSModuleByCOMPortSettings;

{ TGPSModuleByCOMPortConfig }

constructor TGPSModuleByCOMPortConfig.Create(ALogPath: string);
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

function TGPSModuleByCOMPortConfig.CreateStatic: IGPSModuleByCOMPortSettings;
begin
  Result :=
    TGPSModuleByCOMPortSettings.Create(
      FPort,
      FBaudRate,
      FConnectionTimeout,
      FDelay,
      FNMEALog,
      FLogPath
    );
end;

procedure TGPSModuleByCOMPortConfig.DoReadConfig(
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

procedure TGPSModuleByCOMPortConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('COM', FPort);
  AConfigData.WriteInteger('BaudRate', FBaudRate);
  AConfigData.WriteInteger('timeout', FConnectionTimeout);
  AConfigData.WriteInteger('update', FDelay);
  AConfigData.WriteBool('NMEAlog', FNMEALog);
end;

function TGPSModuleByCOMPortConfig.GetBaudRate: Integer;
begin
  LockRead;
  try
    Result := FBaudRate;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetConnectionTimeout: Integer;
begin
  LockRead;
  try
    Result := FConnectionTimeout;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetDelay: Integer;
begin
  LockRead;
  try
    Result := FDelay;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetLogPath: WideString;
begin
  LockRead;
  try
    Result := FLogPath;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetNMEALog: Boolean;
begin
  LockRead;
  try
    Result := FNMEALog;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetPort: Integer;
begin
  LockRead;
  try
    Result := FPort;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetStatic: IGPSModuleByCOMPortSettings;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetBaudRate(AValue: Integer);
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

procedure TGPSModuleByCOMPortConfig.SetChanged;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetConnectionTimeout(AValue: Integer);
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

procedure TGPSModuleByCOMPortConfig.SetDelay(AValue: Integer);
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

procedure TGPSModuleByCOMPortConfig.SetNMEALog(AValue: Boolean);
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

procedure TGPSModuleByCOMPortConfig.SetPort(AValue: Integer);
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

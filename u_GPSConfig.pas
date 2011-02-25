unit u_GPSConfig;

interface

uses
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IGPSModuleByCOMPortConfig,
  i_IGPSConfig,
  u_ConfigDataElementComplexBase;

type
  TGPSConfig = class(TConfigDataElementComplexBase, IGPSConfig)
  private
    FGPSEnabled: Boolean;
    FWriteLog: Boolean;
    FLogPath: WideString;
    FModuleConfig: IGPSModuleByCOMPortConfig;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetGPSEnabled: Boolean;
    procedure SetGPSEnabled(AValue: Boolean);

    function GetWriteLog: Boolean;
    procedure SetWriteLog(AValue: Boolean);

    function GetLogPath: WideString;
    function GetModuleConfig: IGPSModuleByCOMPortConfig;
  public
    constructor Create(ALogPath: string);
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_GPSModuleByCOMPortSettings;

{ TGPSConfig }

constructor TGPSConfig.Create(ALogPath: string);
begin
  inherited Create;
  FLogPath := ALogPath;
  FGPSEnabled := False;
  FWriteLog := True;
  FModuleConfig := TGPSModuleByCOMPortSettings.Create(FLogPath);
  Add(FModuleConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Module'));
end;

procedure TGPSConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FGPSEnabled := AConfigData.ReadBool('Enabled', FGPSEnabled);
    FWriteLog := AConfigData.ReadBool('LogWrite', FWriteLog);
    SetChanged;
  end;
end;

procedure TGPSConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Enabled', FGPSEnabled);
  AConfigData.WriteBool('LogWrite', FWriteLog);
end;

function TGPSConfig.GetGPSEnabled: Boolean;
begin
  LockRead;
  try
    Result := FGPSEnabled;
  finally
    UnlockRead;
  end;
end;

function TGPSConfig.GetLogPath: WideString;
begin
  Result := FLogPath;
end;

function TGPSConfig.GetModuleConfig: IGPSModuleByCOMPortConfig;
begin
  Result := FModuleConfig;
end;

function TGPSConfig.GetWriteLog: Boolean;
begin
  LockRead;
  try
    Result := FWriteLog;
  finally
    UnlockRead;
  end;
end;

procedure TGPSConfig.SetGPSEnabled(AValue: Boolean);
begin
  LockWrite;
  try
    if FGPSEnabled <> AValue then begin
      FGPSEnabled := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSConfig.SetWriteLog(AValue: Boolean);
begin
  LockWrite;
  try
    if FWriteLog <> AValue then begin
      FWriteLog := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

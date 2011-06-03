unit u_InetConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ProxySettings,
  u_ConfigDataElementComplexBase;

type
  TInetConfig = class(TConfigDataElementComplexBase, IInetConfig)
  private
    FTimeOut: Cardinal;
    FProxyConfig: IProxyConfig;
    FSleepOnResetConnection: Cardinal;
    FDownloadTryCount: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetProxyConfig: IProxyConfig;
    function GetTimeOut: Cardinal;
    procedure SetTimeOut(AValue: Cardinal);

    function GetSleepOnResetConnection: Cardinal;
    procedure SetSleepOnResetConnection(AValue: Cardinal);

    function GetDownloadTryCount: Integer;
    procedure SetDownloadTryCount(AValue: Integer);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ProxyConfig;

{ TInetConfig }

constructor TInetConfig.Create;
begin
  inherited;
  FTimeOut := 40000;
  FSleepOnResetConnection := 30000;
  FDownloadTryCount := 2;
  FProxyConfig := TProxyConfig.Create;
  Add(FProxyConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Proxy'));
end;

procedure TInetConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FTimeOut := AConfigData.ReadInteger('TimeOut', FTimeOut);
    SetDownloadTryCount(AConfigData.ReadInteger('DownloadTryCount', FDownloadTryCount));
    FSleepOnResetConnection := AConfigData.ReadInteger('SleepOnResetConnection', FSleepOnResetConnection);
    SetChanged;
  end;
end;

procedure TInetConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('TimeOut', FTimeOut);
  AConfigData.WriteInteger('DownloadTryCount', FDownloadTryCount);
  AConfigData.WriteInteger('SleepOnResetConnection', FSleepOnResetConnection);
end;

function TInetConfig.GetDownloadTryCount: Integer;
begin
  LockRead;
  try
    Result := FDownloadTryCount;
  finally
    UnlockRead;
  end;
end;

function TInetConfig.GetProxyConfig: IProxyConfig;
begin
  Result := FProxyConfig;
end;

function TInetConfig.GetSleepOnResetConnection: Cardinal;
begin
  LockRead;
  try
    Result := FSleepOnResetConnection;
  finally
    UnlockRead;
  end;
end;

function TInetConfig.GetTimeOut: Cardinal;
begin
  LockRead;
  try
    Result := FTimeOut;
  finally
    UnlockRead;
  end;
end;

procedure TInetConfig.SetDownloadTryCount(AValue: Integer);
begin
  if (AValue > 0) and (AValue < 100) then begin
    LockWrite;
    try
      if FDownloadTryCount <> AValue then begin
        FDownloadTryCount := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TInetConfig.SetSleepOnResetConnection(AValue: Cardinal);
begin
  LockWrite;
  try
    if FSleepOnResetConnection <> AValue then begin
      FSleepOnResetConnection := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TInetConfig.SetTimeOut(AValue: Cardinal);
begin
  LockWrite;
  try
    if FTimeOut <> AValue then begin
      FTimeOut := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

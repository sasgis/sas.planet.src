unit u_InetConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_IProxySettings,
  u_ConfigDataElementComplexBase;

type
  TInetConfig = class(TConfigDataElementComplexBase, IInetConfig)
  private
    FTimeOut: Cardinal;
    FProxyConfig: IProxyConfig;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetProxyConfig: IProxyConfig; safecall;
    function GetTimeOut: Cardinal; safecall;
    procedure SetTimeOut(AValue: Cardinal); safecall;
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
  FProxyConfig := TProxyConfig.Create;
  Add(FProxyConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Proxy'));
end;

procedure TInetConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FTimeOut := AConfigData.ReadInteger('TimeOut', FTimeOut);
    SetChanged;
  end;
end;

procedure TInetConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('TimeOut', FTimeOut);
end;

function TInetConfig.GetProxyConfig: IProxyConfig;
begin
  Result := FProxyConfig;
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

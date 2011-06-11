unit u_ProxyConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ProxySettings,
  u_ConfigDataElementBase;

type
  TProxyConfigStatic = class(TInterfacedObject, IProxyConfigStatic)
  private
    FUseIESettings: Boolean;
    FUseProxy: Boolean;
    FHost: WideString;
    FUseLogin: boolean;
    FLogin: WideString;
    FPassword: WideString;
  protected
    function GetUseIESettings: Boolean;
    function GetUseProxy: Boolean;
    function GetHost: WideString;
    function GetUseLogin: boolean;
    function GetLogin: WideString;
    function GetPassword: WideString;
  public
    constructor Create(
      AUseIESettings: Boolean;
      AUseProxy: Boolean;
      AHost: WideString;
      AUseLogin: boolean;
      ALogin: WideString;
      APassword: WideString
    );
  end;

  TProxyConfig = class(TConfigDataElementBase, IProxyConfig, IProxySettings)
  private
    FUseIESettings: Boolean;
    FUseProxy: Boolean;
    FHost: WideString;
    FUseLogin: boolean;
    FLogin: WideString;
    FPassword: WideString;
    FStatic: IProxyConfigStatic;
    function CreateStatic: IProxyConfigStatic;
  protected
    procedure SetChanged; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetUseIESettings: Boolean; safecall;
    function GetUseProxy: Boolean; safecall;
    function GetHost: WideString; safecall;
    function GetUseLogin: boolean; safecall;
    function GetLogin: WideString; safecall;
    function GetPassword: WideString; safecall;

    procedure SetUseIESettings(AValue: Boolean);
    procedure SetUseProxy(AValue: Boolean);
    procedure SetHost(AValue: WideString);
    procedure SetUseLogin(AValue: Boolean);
    procedure SetLogin(AValue: WideString);
    procedure SetPassword(AValue: WideString);

    function GetStatic: IProxyConfigStatic;
  public
    constructor Create();
  end;

implementation

{ TProxyConfig }

constructor TProxyConfig.Create;
begin
  inherited;
  FUseIESettings := True;
  FUseProxy := False;
  FUseLogin := False;
  FHost := '';
  FLogin := '';
  FPassword := '';
  SetChanged;
end;

function TProxyConfig.CreateStatic: IProxyConfigStatic;
begin
  Result :=
    TProxyConfigStatic.Create(
      FUseIESettings,
      FUseProxy,
      FHost,
      FUseLogin,
      FLogin,
      FPassword
    );
end;

procedure TProxyConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUseIESettings := AConfigData.ReadBool('UseIEProxySettings', FUseIESettings);
    FUseProxy := AConfigData.ReadBool('UseProxy', FUseProxy);
    FHost := AConfigData.ReadString('Host', FHost);
    FUseLogin := AConfigData.ReadBool('UseAuth', FUseLogin);
    FLogin := AConfigData.ReadString('Login', FLogin);
    FPassword := AConfigData.ReadString('Password', FPassword);
    SetChanged;
  end;
end;

procedure TProxyConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UseIEProxySettings', FUseIESettings);
  AConfigData.WriteBool('UseProxy', FUseProxy);
  AConfigData.WriteString('Host', FHost);
  AConfigData.WriteBool('UseAuth', FUseLogin);
  AConfigData.WriteString('Login', FLogin);
  AConfigData.WriteString('Password', FPassword);
end;

function TProxyConfig.GetHost: WideString;
begin
  LockRead;
  try
    Result := FHost;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetLogin: WideString;
begin
  LockRead;
  try
    Result := FLogin;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetPassword: WideString;
begin
  LockRead;
  try
    Result := FPassword;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetStatic: IProxyConfigStatic;
begin
  Result := FStatic;
end;

function TProxyConfig.GetUseIESettings: Boolean;
begin
  LockRead;
  try
    Result := FUseIESettings;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetUseLogin: boolean;
begin
  LockRead;
  try
    Result := FUseLogin;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetUseProxy: Boolean;
begin
  LockRead;
  try
    Result := FUseProxy;
  finally
    UnlockRead;
  end;
end;

procedure TProxyConfig.SetChanged;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TProxyConfig.SetHost(AValue: WideString);
begin
  LockWrite;
  try
    if FHost <> AValue then begin
      FHost := AValue;
      if not FUseIESettings and FUseProxy then begin
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TProxyConfig.SetLogin(AValue: WideString);
begin
  LockWrite;
  try
    if FLogin <> AValue then begin
      FLogin := AValue;
      if not FUseIESettings and FUseProxy and FUseLogin then begin
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TProxyConfig.SetPassword(AValue: WideString);
begin
  LockWrite;
  try
    if FPassword <> AValue then begin
      FPassword := AValue;
      if not FUseIESettings and FUseProxy and FUseLogin then begin
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TProxyConfig.SetUseIESettings(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseIESettings <> AValue then begin
      FUseIESettings := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;

end;

procedure TProxyConfig.SetUseLogin(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseLogin <> AValue then begin
      FUseLogin := AValue;
      if not FUseIESettings and FUseProxy then begin
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TProxyConfig.SetUseProxy(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseProxy <> AValue then begin
      FUseProxy := AValue;
      if not FUseIESettings then begin
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TProxyConfigStatic }

constructor TProxyConfigStatic.Create(AUseIESettings, AUseProxy: Boolean;
  AHost: WideString; AUseLogin: boolean; ALogin, APassword: WideString);
begin
  FUseIESettings := AUseIESettings;
  FUseProxy := AUseProxy;
  FHost := AHost;
  FUseLogin := AUseLogin;
  FLogin := ALogin;
  FPassword := APassword;
end;

function TProxyConfigStatic.GetHost: WideString;
begin
  Result := FHost;
end;

function TProxyConfigStatic.GetLogin: WideString;
begin
  Result := FLogin;
end;

function TProxyConfigStatic.GetPassword: WideString;
begin
  Result := FPassword;
end;

function TProxyConfigStatic.GetUseIESettings: Boolean;
begin
  Result := FUseIESettings;
end;

function TProxyConfigStatic.GetUseLogin: boolean;
begin
  Result := FUseLogin;
end;

function TProxyConfigStatic.GetUseProxy: Boolean;
begin
  Result := FUseProxy;
end;

end.

unit u_ProxySettingsFromTInetConnect;

interface

uses
  t_CommonTypes,
  i_IProxySettings;

type
  TProxySettingsFromTInetConnect = class(TInterfacedObject, IProxySettings)
  private
    FInetParams: TInetConnect;
    function GetUseProxy(): boolean; safecall;
    function GetHost(): WideString; safecall;
    function GetUseLogin(): boolean; safecall;
    function GetLogin(): WideString; safecall;
    function GetPassword(): WideString; safecall;
  public
    constructor Create(AInetParams: TInetConnect);
    destructor Destroy; override;
  end;

implementation

{ TProxySettingsFromTInetConnect }

constructor TProxySettingsFromTInetConnect.Create(
  AInetParams: TInetConnect);
begin
  FInetParams := AInetParams;
end;

destructor TProxySettingsFromTInetConnect.Destroy;
begin
  FInetParams := nil;
  inherited;
end;

function TProxySettingsFromTInetConnect.GetHost: WideString;
begin
  if FInetParams.proxyused then begin
    Result := FInetParams.proxystr;
  end;
end;

function TProxySettingsFromTInetConnect.GetLogin: WideString;
begin
  if FInetParams.proxyused then begin
    if FInetParams.uselogin then begin
      Result := FInetParams.loginstr;
    end;
  end;
end;

function TProxySettingsFromTInetConnect.GetPassword: WideString;
begin
  if FInetParams.proxyused then begin
    if FInetParams.uselogin then begin
      Result := FInetParams.passstr;
    end;
  end;
end;

function TProxySettingsFromTInetConnect.GetUseLogin: boolean;
begin
  if FInetParams.proxyused then begin
    Result := FInetParams.uselogin;
  end else begin
    Result := False;
  end;

end;

function TProxySettingsFromTInetConnect.GetUseProxy: boolean;
begin
  Result := FInetParams.proxyused;
end;

end.

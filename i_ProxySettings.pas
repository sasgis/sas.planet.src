unit i_ProxySettings;

interface

uses
  i_ConfigDataElement;
  
type
  // Настройки прокси
  IProxySettings = interface(IInterface)
    ['{17F31D40-FCA0-4525-9820-A14BB61AA08A}']
    function GetUseProxy(): boolean; safecall;
    function GetHost(): WideString; safecall;
    function GetUseLogin(): boolean; safecall;
    function GetLogin(): WideString; safecall;
    function GetPassword(): WideString; safecall;

    property UseProxy: boolean read GetUseProxy;
    property Host: WideString read GetHost;
    property UseLogin: boolean read GetUseLogin;
    property Login: WideString read GetLogin;
    property Password: WideString read GetPassword;
  end;

  IProxyConfig = interface(IConfigDataElement)
  ['{0CE5A97E-471D-4A3E-93E3-D130DD1F50F5}']
    function GetUseIESettings: Boolean; safecall;
    function GetUseProxy(): Boolean; safecall;
    function GetHost(): WideString; safecall;
    function GetUseLogin(): boolean; safecall;
    function GetLogin(): WideString; safecall;
    function GetPassword(): WideString; safecall;

    procedure SetUseIESettings(AValue: Boolean);
    procedure SetUseProxy(AValue: Boolean);
    procedure SetHost(AValue: WideString);
    procedure SetUseLogin(AValue: Boolean);
    procedure SetLogin(AValue: WideString);
    procedure SetPassword(AValue: WideString);
  end;

  IInetConfig = interface(IConfigDataElement)
    ['{D025A3CE-2CC7-4DB3-BBF6-53DF14A2A2E7}']
    function GetProxyConfig: IProxyConfig; safecall;
    function GetTimeOut: Cardinal; safecall;
    procedure SetTimeOut(AValue: Cardinal); safecall;

    property ProxyConfig: IProxyConfig read GetProxyConfig;
  end;

implementation

end.

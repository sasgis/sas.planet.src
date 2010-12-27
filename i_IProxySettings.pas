unit i_IProxySettings;

interface

uses
  i_IConfigDataElement;
  
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

  IProxySettingsElement = interface(IConfigDataElement)
  ['{0CE5A97E-471D-4A3E-93E3-D130DD1F50F5}']
    function GetUseProxy(): boolean; safecall;
    function GetHost(): WideString; safecall;
    function GetUseLogin(): boolean; safecall;
    function GetLogin(): WideString; safecall;
    function GetPassword(): WideString; safecall;
  end;

  IInetSettings = interface(IConfigDataElement)
    ['{D025A3CE-2CC7-4DB3-BBF6-53DF14A2A2E7}']
    function GetProxySettings: IProxySettings; safecall;
    function GetTimeOut: cardinal; safecall;

    property ProxySettings: IProxySettings read GetProxySettings;
    property TimeOut: cardinal read GetTimeOut;
  end;

implementation

end.

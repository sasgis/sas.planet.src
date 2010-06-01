unit i_IProxySettings;

interface

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


implementation

end.

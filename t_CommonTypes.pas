unit t_CommonTypes;

interface

type
  TTileSource = (tsInternet,tsCache,tsCacheInternet);

  TInetConnect = class
  public
    userwinset: boolean;
    proxyused: boolean;
    proxystr: string;
    ProxyBypass: string;
    uselogin: boolean;
    loginstr: string;
    passstr: string;
    TimeOut: cardinal;
  end;

  TGSMpar = record
    BaudRate: integer;
    Port: string;
    auto: boolean;
    WaitingAnswer: integer;
  end;

  TMarksShowType = (mshAll = 1, mshChecked = 2, mshNone = 3);

implementation

end.

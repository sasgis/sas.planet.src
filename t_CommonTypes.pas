unit t_CommonTypes;

interface

type
  TInetConnect = class
  public
    userwinset: boolean;
    proxyused: boolean;
    proxystr: string;
    ProxyBypass: string;
    uselogin: boolean;
    loginstr: string;
    passstr: string;
  end;

  TMarksShowType = (mshAll = 1, mshChecked = 2, mshNone = 3);
implementation

end.
 
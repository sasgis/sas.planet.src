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
    TimeOut: cardinal;
  end;

  TGPSpar = record
   speed:real;
   len:extended;
   sspeed:extended;
   allspeed:extended;
   sspeednumentr:integer;
   altitude:extended;
   maxspeed:real;
   nap:integer;
   azimut:extended;
   Odometr:extended;
  end;


  TMarksShowType = (mshAll = 1, mshChecked = 2, mshNone = 3);
implementation

end.
 
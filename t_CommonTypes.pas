unit t_CommonTypes;

interface

type
  TTileSource = (tsInternet,tsCache,tsCacheInternet);

  TGSMpar = record
    BaudRate: integer;
    Port: string;
    auto: boolean;
    WaitingAnswer: integer;
  end;

  TMarksShowType = (mshAll = 1, mshChecked = 2, mshNone = 3);

implementation

end.

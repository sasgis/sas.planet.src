unit i_ILogSimple;

interface

type
  ILogSimple = interface
    ['{81895444-746B-4D79-9BF8-71D90A7B2437}']
    procedure WriteText(AMessage: WideString; ALogLevel: integer); safecall;
  end;

implementation

end.
 
unit i_LogForTaskThread;

interface

uses
  i_LogSimple;

type
  ILogForTaskThread = interface(ILogSimple)
    ['{8583BE82-1239-4E0F-9B96-D2810232282A}']
    function GetLastMessages(AMaxRowsCount: Cardinal; var ALastId: Cardinal; out AcntLines: Cardinal): WideString; safecall;
  end;

implementation

end.
 
unit i_ErrorLogger;

interface

uses
  i_ErrorInfo;

type
  IErrorLogger = interface
    ['{E3B0D877-75BA-487A-AF9C-5E9D0BF3F3B9}']
    procedure LogError(const AValue: IErrorInfoSimple);
  end;

implementation

end.

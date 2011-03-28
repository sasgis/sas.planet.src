unit i_Thread;

interface

uses
  Classes;

type
  IThread = interface
    ['{4FA6F3B4-32D4-4798-AF4F-3881F3321E40}']
    procedure Start;
    procedure Terminate;
    function GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    function WaitFor: LongWord;

    property Priority: TThreadPriority read GetPriority write SetPriority;
  end;

implementation

end.

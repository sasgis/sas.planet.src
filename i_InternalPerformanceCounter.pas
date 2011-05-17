unit i_InternalPerformanceCounter;

interface

uses
  ActiveX;

type
  TInternalPerformanceCounterContext = Int64;
  IInternalPerformanceCounter = interface
    ['{2D5EE758-A5EA-467D-A679-C3CD1B116973}']
    function GetName: string;
    property Name: string read GetName;

    function StartOperation: TInternalPerformanceCounterContext;
    procedure FinishOperation(AContext: TInternalPerformanceCounterContext);

    function GetCounter: Cardinal;
    property Counter: Cardinal  read GetCounter;

    function GetTotalTime: TDateTime;
    property TotalTime: TDateTime read GetTotalTime;

    function GetLastTimeInSeconds: Double;
    property LastTimeInSeconds: Double read GetLastTimeInSeconds;
  end;

  IInternalPerformanceCounterList =  interface
    ['{75567269-AD8D-443F-AA45-9336C9890719}']
    function GetName: string;
    property Name: string read GetName;

    function GetEunm: IEnumUnknown;
    function CreateAndAddNew(AName: string): IInternalPerformanceCounter;
  end;

implementation

end.

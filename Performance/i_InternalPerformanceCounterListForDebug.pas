unit i_InternalPerformanceCounterListForDebug;

interface

uses
  i_InternalPerformanceCounter;

type
  IInternalPerformanceCounterListForDebugOneClass = interface(IInternalPerformanceCounterList)
    function GetCounterCreate: IInternalPerformanceCounter;
    property CounterCreate: IInternalPerformanceCounter read GetCounterCreate;

    function GetCounterDestroy: IInternalPerformanceCounter;
    property CounterDestroy: IInternalPerformanceCounter read GetCounterDestroy;
  end;

  IInternalPerformanceCounterListForDebug = interface(IInternalPerformanceCounterList)
    function GetCounterByClass(AClass: TClass): IInternalPerformanceCounterListForDebugOneClass;
  end;

implementation

end.

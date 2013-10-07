unit i_InternalPerformanceCounterListForDebug;

interface

uses
  i_InterfaceListSimple,
  i_InternalPerformanceCounter;

type
  IInternalPerformanceCounterListForDebugOneClass = interface
    function GetCounterCreate: IInternalPerformanceCounter;
    property CounterCreate: IInternalPerformanceCounter read GetCounterCreate;

    function GetCounterDestroy: IInternalPerformanceCounter;
    property CounterDestroy: IInternalPerformanceCounter read GetCounterDestroy;
  end;

  IInternalPerformanceCounterListForDebug = interface
    function GetCounterByClass(AClass: TClass): IInternalPerformanceCounterListForDebugOneClass;
    procedure AddStaticDataToList(const AList: IInterfaceListSimple);
  end;

implementation

end.

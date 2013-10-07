unit i_DebugInfoSubSystem;

interface

uses
  i_InterfaceListStatic,
  i_InternalPerformanceCounter;

type
  IDebugInfoSubSystem = interface
    ['{373EFDD9-7529-4E43-B3AF-2E8C90BA043D}']
    function GetRootCounterList: IInternalPerformanceCounterList;
    property RootCounterList: IInternalPerformanceCounterList read GetRootCounterList;

    function GetStaticDataList: IInterfaceListStatic;
  end;


implementation

end.

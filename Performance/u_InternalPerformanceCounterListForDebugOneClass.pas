unit u_InternalPerformanceCounterListForDebugOneClass;

interface

uses
  i_InternalPerformanceCounter,
  i_InternalPerformanceCounterListForDebug;

type
  TInternalPerformanceCounterListForDebugOneClass = class(TInterfacedObject, IInternalPerformanceCounterListForDebugOneClass)
  private
    FCounterCreate: IInternalPerformanceCounter;
    FCounterDestroy: IInternalPerformanceCounter;
  private
    function GetCounterCreate: IInternalPerformanceCounter;
    function GetCounterDestroy: IInternalPerformanceCounter;
  public
    constructor Create(
      const ABaseName: string;
      const AFactory: IInternalPerformanceCounterFactory
    );
  end;

implementation

{ TInternalPerformanceCounterListForDebugOneClass }

constructor TInternalPerformanceCounterListForDebugOneClass.Create(
  const ABaseName: string;
  const AFactory: IInternalPerformanceCounterFactory
);
begin
  inherited Create;
  FCounterCreate := AFactory.Build(ABaseName + '/Create');
  FCounterDestroy := AFactory.Build(ABaseName + '/Destroy');
end;

function TInternalPerformanceCounterListForDebugOneClass.GetCounterCreate: IInternalPerformanceCounter;
begin
  Result := FCounterCreate;
end;

function TInternalPerformanceCounterListForDebugOneClass.GetCounterDestroy: IInternalPerformanceCounter;
begin
  Result := FCounterDestroy;
end;

end.

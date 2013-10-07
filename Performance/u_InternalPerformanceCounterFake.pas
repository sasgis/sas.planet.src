unit u_InternalPerformanceCounterFake;

interface

uses
  i_InternalPerformanceCounter;
  
type
  TInternalPerformanceCounterFake = class(TInterfacedObject, IInternalPerformanceCounterList, IInternalPerformanceCounter)
  private
    { IInternalPerformanceCounterList }
    function CreateAndAddNewCounter(const AName: string): IInternalPerformanceCounter;
    function CreateAndAddNewSubList(const AName: string): IInternalPerformanceCounterList;
  private
    { IInternalPerformanceCounter }
    function StartOperation: TInternalPerformanceCounterContext;
    procedure FinishOperation(const AContext: TInternalPerformanceCounterContext);
    function GetStaticData: IInternalPerformanceCounterStaticData;
  end;

implementation

{ TInternalPerformanceCounterFake }

function TInternalPerformanceCounterFake.CreateAndAddNewCounter(const AName: string): IInternalPerformanceCounter;
begin
  Result := Self;
end;

function TInternalPerformanceCounterFake.CreateAndAddNewSubList(const AName: string): IInternalPerformanceCounterList;
begin
  Result := Self;
end;

procedure TInternalPerformanceCounterFake.FinishOperation(const AContext: TInternalPerformanceCounterContext);
begin
  // empty
end;

function TInternalPerformanceCounterFake.GetStaticData: IInternalPerformanceCounterStaticData;
begin
  Result := nil;
end;

function TInternalPerformanceCounterFake.StartOperation: TInternalPerformanceCounterContext;
begin
  Result := 0;
end;

end.

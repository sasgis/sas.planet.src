unit u_BaseInterfacedObjectDebug;

interface

uses
  i_InternalPerformanceCounter,
  i_InternalPerformanceCounterListForDebug;

type
  TBaseInterfacedObjectDebug = class(TInterfacedObject)
  private class var
    FCounters: IInternalPerformanceCounterListForDebug;
  private
    class function GetCounter: IInternalPerformanceCounterListForDebugOneClass; virtual;
  private
    FContext: TInternalPerformanceCounterContext;
    FCounter: IInternalPerformanceCounter;
  public
    class function GetCounters: IInternalPerformanceCounterList;
    class procedure InitCounters;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

implementation

uses
  u_InternalPerformanceCounter,
  u_InternalPerformanceCounterListForDebug;

{ TBaseInterfacedObjectDebug }

procedure TBaseInterfacedObjectDebug.AfterConstruction;
begin
  inherited;
  if FCounter <> nil then begin
    FCounter.FinishOperation(FContext);
    FCounter := nil;
  end;
end;

procedure TBaseInterfacedObjectDebug.BeforeDestruction;
var
  VList: IInternalPerformanceCounterListForDebugOneClass;
begin
  inherited;
  VList := GetCounter;
  if VList <> nil then begin
    FCounter := VList.CounterDestroy;
    FContext := FCounter.StartOperation;
  end;
end;

procedure TBaseInterfacedObjectDebug.FreeInstance;
var
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := FCounter;
  VContext := FContext;
  inherited;
  if VCounter <> nil then begin
    VCounter.FinishOperation(VContext);
  end;
end;

class function TBaseInterfacedObjectDebug.GetCounter: IInternalPerformanceCounterListForDebugOneClass;
begin
  Result := nil;
  if FCounters <> nil then begin
    Result := FCounters.GetCounterByClass(Self);
  end;
end;

class function TBaseInterfacedObjectDebug.GetCounters: IInternalPerformanceCounterList;
begin
  Result := FCounters;
end;

class procedure TBaseInterfacedObjectDebug.InitCounters;
begin
  if FCounters <> nil then begin
    Assert(False);
  end else begin
    FCounters :=
      TInternalPerformanceCounterListForDebug.Create(
        'Objects',
        TInternalPerformanceCounterFactory.Create
      );
  end;
end;

class function TBaseInterfacedObjectDebug.NewInstance: TObject;
var
  VList: IInternalPerformanceCounterListForDebugOneClass;
  VCounter: IInternalPerformanceCounter;
  VContext: TInternalPerformanceCounterContext;
begin
  VList := GetCounter;
  if VList <> nil then begin
    VCounter := VList.CounterCreate;
    VContext := VCounter.StartOperation;
    Result := inherited NewInstance;
    TBaseInterfacedObjectDebug(Result).FCounter := VCounter;
    TBaseInterfacedObjectDebug(Result).FContext := VContext;
  end else begin
    Result := inherited NewInstance;
  end;
end;

end.

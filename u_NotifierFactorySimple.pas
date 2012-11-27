unit u_NotifierFactorySimple;

interface

uses
  i_InternalPerformanceCounter,
  i_ReadWriteSyncFactory,
  i_Notifier,
  i_NotifierFactory;

type
  TNotifierFactorySimple = class(TInterfacedObject, INotifierFactory)
  private
    FSyncFactory: IReadWriteSyncFactory;
  private
    function Make(const AName: string): INotifierInternal;
  public
    constructor Create(
      const ASyncFactory: IReadWriteSyncFactory
    );
  end;

  TNotifierFactoryWrapperWithCounter = class(TInterfacedObject, INotifierFactory)
  private
    FSource: INotifierFactory;
    FCounter: IInternalPerformanceCounter;
  private
    function Make(const AName: string): INotifierInternal;
  public
    constructor Create(
      const ASource: INotifierFactory;
      const ACounter: IInternalPerformanceCounter
    );
  end;


implementation

uses
  u_Notifier;

{ TNotifierFactorySimple }

constructor TNotifierFactorySimple.Create(
  const ASyncFactory: IReadWriteSyncFactory);
begin
  inherited Create;
  FSyncFactory := ASyncFactory;
end;

function TNotifierFactorySimple.Make(const AName: string): INotifierInternal;
begin
  Result := TNotifierBase.Create;
end;

{ TNotifierFactoryWrapperWithCounter }

constructor TNotifierFactoryWrapperWithCounter.Create(
  const ASource: INotifierFactory;
  const ACounter: IInternalPerformanceCounter
);
begin
  Assert(ASource <> nil);
  Assert(ACounter <> nil);
  inherited Create;
  FSource := ASource;
  FCounter := ACounter;
end;

function TNotifierFactoryWrapperWithCounter.Make(
  const AName: string
): INotifierInternal;
var
  VContext: TInternalPerformanceCounterContext;
begin
  VContext := FCounter.StartOperation;
  try
    Result := FSource.Make(AName);
  finally
    FCounter.FinishOperation(VContext);
  end;
end;

end.

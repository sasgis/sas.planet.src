unit u_VectorDataLoaderWithCounter;

interface

uses
  i_InternalPerformanceCounter,
  i_BinaryData,
  i_VectorItemSubset,
  i_VectorDataFactory,
  i_VectorDataLoader,
  u_BaseInterfacedObject;

type
  TVectorDataLoaderWithCounter = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FLoader: IVectorDataLoader;
    FLoadCounter: IInternalPerformanceCounter;
  private
    function Load(
      const AData: IBinaryData;
      const AIdData: Pointer;
      const AFactory: IVectorDataItemMainInfoFactory
    ): IVectorItemSubset;
  public
    constructor Create(
      const ALoader: IVectorDataLoader;
      const ALoadCounter: IInternalPerformanceCounter
    );
  end;

implementation

{ TVectorDataLoaderWithCounter }

constructor TVectorDataLoaderWithCounter.Create(
  const ALoader: IVectorDataLoader;
  const ALoadCounter: IInternalPerformanceCounter
);
begin
  Assert(Assigned(ALoader));
  Assert(Assigned(ALoadCounter));
  inherited Create;
  FLoader := ALoader;
  FLoadCounter := ALoadCounter;
end;

function TVectorDataLoaderWithCounter.Load(
  const AData: IBinaryData;
  const AIdData: Pointer;
  const AFactory: IVectorDataItemMainInfoFactory
): IVectorItemSubset;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := nil;
  VCounterContext := FLoadCounter.StartOperation;
  try
    // read from single simple source
    Result :=
      FLoader.Load(
        AData,
        AIdData,
        AFactory
      )
  finally
    FLoadCounter.FinishOperation(VCounterContext);
  end;
end;

end.

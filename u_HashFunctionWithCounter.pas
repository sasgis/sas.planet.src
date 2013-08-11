unit u_HashFunctionWithCounter;

interface

uses
  t_Hash,
  i_HashFunction,
  i_InternalPerformanceCounter,
  u_BaseInterfacedObject;

type
  THashFunctionWithCounter = class(TBaseInterfacedObject, IHashFunction)
  private
    FFunction: IHashFunction;
    FCounter: IInternalPerformanceCounter;
  private
    function CalcHash(
      ABuffer: Pointer;
      ASize: Integer
    ): THashValue;
    function CalcHashWithSeed(
      ABuffer: Pointer;
      ASize: Integer;
      const ASeed: THashValue
    ): THashValue;
    function CalcHashOfTwoHash(
      const AHash1: THashValue;
      const AHash2: THashValue
    ): THashValue;
  public
    constructor Create(
      const AFunction: IHashFunction;
      const ACounter: IInternalPerformanceCounter
    );
  end;

implementation

{ THashFunctionWithCounter }

constructor THashFunctionWithCounter.Create(
  const AFunction: IHashFunction;
  const ACounter: IInternalPerformanceCounter
);
begin
  Assert(Assigned(AFunction));
  Assert(Assigned(ACounter));
  inherited Create;
  FFunction := AFunction;
  FCounter := ACounter;
end;

function THashFunctionWithCounter.CalcHash(
  ABuffer: Pointer;
  ASize: Integer
): THashValue;
var
  VContext: TInternalPerformanceCounterContext;
begin
  VContext := FCounter.StartOperation;
  try
    Result := FFunction.CalcHash(ABuffer, ASize);
  finally
    FCounter.FinishOperation(VContext);
  end;
end;

function THashFunctionWithCounter.CalcHashOfTwoHash(const AHash1,
  AHash2: THashValue): THashValue;
var
  VContext: TInternalPerformanceCounterContext;
begin
  VContext := FCounter.StartOperation;
  try
    Result := FFunction.CalcHashOfTwoHash(AHash1, AHash2);
  finally
    FCounter.FinishOperation(VContext);
  end;
end;

function THashFunctionWithCounter.CalcHashWithSeed(
  ABuffer: Pointer;
  ASize: Integer;
  const ASeed: THashValue
): THashValue;
var
  VContext: TInternalPerformanceCounterContext;
begin
  VContext := FCounter.StartOperation;
  try
    Result := FFunction.CalcHashWithSeed(ABuffer, ASize, ASeed);
  finally
    FCounter.FinishOperation(VContext);
  end;
end;

end.

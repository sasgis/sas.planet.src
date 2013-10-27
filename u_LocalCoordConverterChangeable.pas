unit u_LocalCoordConverterChangeable;

interface

uses
  i_SimpleFlag,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  u_ConfigDataElementBase;

type
  TLocalCoordConverterChangeable = class(TConfigDataElementWithStaticBaseEmptySaveLoad, ILocalCoordConverterChangeable, ILocalCoordConverterChangeableInternal)
  private
    FConverter: ILocalCoordConverter;
    FChangeCounter: IInternalPerformanceCounter;
  private
    function GetStatic: ILocalCoordConverter;
    procedure SetConverter(const AValue: ILocalCoordConverter);
  protected
    function CreateStatic: IInterface; override;
    procedure DoChangeNotify; override;
  public
    constructor Create(
      const AChangedFlag: ISimpleFlag;
      const ASource: ILocalCoordConverter;
      const AChangeCounter: IInternalPerformanceCounter
    );
  end;

implementation

{ TLocalCoordConverterChangeable }

constructor TLocalCoordConverterChangeable.Create(
  const AChangedFlag: ISimpleFlag;
  const ASource: ILocalCoordConverter;
  const AChangeCounter: IInternalPerformanceCounter
);
begin
  inherited Create(AChangedFlag);
  FConverter := ASource;
  FChangeCounter := AChangeCounter;
end;

function TLocalCoordConverterChangeable.CreateStatic: IInterface;
begin
  Result := FConverter;
end;

procedure TLocalCoordConverterChangeable.DoChangeNotify;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FChangeCounter.StartOperation;
  try
    inherited;
  finally
    FChangeCounter.FinishOperation(VCounterContext);
  end;
end;

function TLocalCoordConverterChangeable.GetStatic: ILocalCoordConverter;
begin
  Result := ILocalCoordConverter(GetStaticInternal);
end;

procedure TLocalCoordConverterChangeable.SetConverter(
  const AValue: ILocalCoordConverter);
begin
  LockWrite;
  try
    if (Assigned(FConverter) and not FConverter.GetIsSameConverter(AValue))
      or (Assigned(AValue) and not Assigned(FConverter))
    then begin
      FConverter := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

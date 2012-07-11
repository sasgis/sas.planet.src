unit u_LocalCoordConverterChangeable;

interface

uses
  i_SimpleFlag,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  u_ConfigDataElementBase;

type
  TLocalCoordConverterChangeable = class(TConfigDataElementBaseEmptySaveLoad, ILocalCoordConverterChangeable, ILocalCoordConverterChangeableInternal)
  private
    FConverter: ILocalCoordConverter;
    FChangeCounter: IInternalPerformanceCounter;
  private
    function GetStatic: ILocalCoordConverter;
    procedure SetConverter(const AValue: ILocalCoordConverter);
  protected
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
  LockRead;
  Try
    Result := FConverter;
  Finally
    UnlockRead;
  End;
end;

procedure TLocalCoordConverterChangeable.SetConverter(
  const AValue: ILocalCoordConverter);
begin
  if AValue = nil then begin
    Exit;
  end;

  LockWrite;
  try
    if not FConverter.GetIsSameConverter(AValue) then begin
      FConverter := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

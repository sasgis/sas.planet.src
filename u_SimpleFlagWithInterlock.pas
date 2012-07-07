unit u_SimpleFlagWithInterlock;

interface

uses
  Windows,
  i_SimpleFlag;

type
  TSimpleFlagWithInterlock = class(TInterfacedObject, ISimpleFlag)
  private
    FSetCount: Integer;
  private
    procedure SetFlag;
    function CheckFlagAndReset: Boolean;
    function CheckFlag: Boolean;
  public
    constructor Create;
  end;

  TSimpleFlagWithParent = class(TInterfacedObject, ISimpleFlag)
  private
    FParent: ISimpleFlag;
    FSetCount: Integer;
  private
    procedure SetFlag;
    function CheckFlagAndReset: Boolean;
    function CheckFlag: Boolean;
  public
    constructor Create(AParent: ISimpleFlag);
  end;

  TCounterInterlock = class(TInterfacedObject, ICounter)
  private
    FCount: Integer;
  private
    function Inc: Integer;
    function Dec: Integer;
  public
    constructor Create;
  end;

implementation

{ TSimpleFlagWithInterlock }

constructor TSimpleFlagWithInterlock.Create;
begin
  inherited Create;
  FSetCount := 0;
end;

function TSimpleFlagWithInterlock.CheckFlag: Boolean;
begin
  Result := InterlockedCompareExchange(FSetCount, 0, 0) > 0;
end;

function TSimpleFlagWithInterlock.CheckFlagAndReset: Boolean;
begin
  Result := InterlockedExchange(FSetCount, 0) > 0;
end;

procedure TSimpleFlagWithInterlock.SetFlag;
begin
  InterlockedIncrement(FSetCount);
end;

{ TCounterInterlock }

constructor TCounterInterlock.Create;
begin
  inherited Create;
  FCount := 0;
end;

function TCounterInterlock.Dec: Integer;
begin
  Result := InterlockedDecrement(FCount);
end;

function TCounterInterlock.Inc: Integer;
begin
  Result := InterlockedIncrement(FCount);
end;

{ TSimpleFlagWithParent }

constructor TSimpleFlagWithParent.Create(AParent: ISimpleFlag);
begin
  inherited Create;
  FParent := AParent;
  FSetCount := 0;
end;

function TSimpleFlagWithParent.CheckFlag: Boolean;
begin
  Result := InterlockedCompareExchange(FSetCount, 0, 0) > 0;
end;

function TSimpleFlagWithParent.CheckFlagAndReset: Boolean;
begin
  Result := InterlockedExchange(FSetCount, 0) > 0;
end;

procedure TSimpleFlagWithParent.SetFlag;
begin
  FParent.SetFlag;
  InterlockedIncrement(FSetCount);
end;

end.

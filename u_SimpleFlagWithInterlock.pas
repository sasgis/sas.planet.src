unit u_SimpleFlagWithInterlock;

interface

uses
  Windows,
  i_SimpleFlag,
  u_BaseInterfacedObject;

type
  TSimpleFlagWithInterlock = class(TBaseInterfacedObject, ISimpleFlag)
  private
    FSetCount: Integer;
  private
    procedure SetFlag;
    function CheckFlagAndReset: Boolean;
    function CheckFlag: Boolean;
  public
    constructor Create;
  end;

  TSimpleFlagWithParent = class(TBaseInterfacedObject, ISimpleFlag)
  private
    FParent: ISimpleFlag;
    FSetCount: Integer;
  private
    procedure SetFlag;
    function CheckFlagAndReset: Boolean;
    function CheckFlag: Boolean;
  public
    constructor Create(const AParent: ISimpleFlag);
  end;

  TCounterInterlock = class(TBaseInterfacedObject, ICounter)
  private
    FCount: Integer;
  private
    function Inc: Integer;
    function Dec: Integer;
    function GetValue: Integer;
    function CheckEqual(AValue: Integer): Boolean;
    procedure Reset;
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

function TCounterInterlock.CheckEqual(AValue: Integer): Boolean;
begin
  Result := InterlockedCompareExchange(FCount, AValue, AValue) = AValue;
end;

constructor TCounterInterlock.Create;
begin
  inherited Create;
  FCount := 0;
end;

function TCounterInterlock.Dec: Integer;
begin
  Result := InterlockedDecrement(FCount);
end;

function TCounterInterlock.GetValue: Integer;
begin
  Result := InterlockedCompareExchange(FCount, 0, 0);
end;

function TCounterInterlock.Inc: Integer;
begin
  Result := InterlockedIncrement(FCount);
end;

procedure TCounterInterlock.Reset;
begin
  InterlockedExchange(FCount, 0)
end;

{ TSimpleFlagWithParent }

constructor TSimpleFlagWithParent.Create(const AParent: ISimpleFlag);
begin
  Assert(AParent <> nil);
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

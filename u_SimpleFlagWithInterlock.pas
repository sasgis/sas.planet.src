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

end.

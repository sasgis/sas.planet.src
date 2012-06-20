unit u_MainFormState;

interface

uses
  i_MainFormState,
  u_ChangeableBase;

type
  TMainFormState = class(TChangeableBase, IMainFormState)
  private
    FState: TStateEnum;
  private
    function GetState: TStateEnum;
    procedure SetState(AValue: TStateEnum);
  public
    constructor Create;
  end;

implementation

{ TMainFormState }

constructor TMainFormState.Create;
begin
  inherited Create;
  FState := ao_movemap;
end;

function TMainFormState.GetState: TStateEnum;
begin
  Result := FState;
end;

procedure TMainFormState.SetState(AValue: TStateEnum);
begin
  if FState <> AValue then begin
    FState := AValue;
    DoChangeNotify;
  end;
end;

end.

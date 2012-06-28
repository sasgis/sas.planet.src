unit u_ReadWriteStateStatic;

interface

uses
  t_CommonTypes,
  i_ReadWriteState;

type
  TReadWriteStateStatic = class(TInterfacedObject, IReadWriteStateStatic)
  private
    FReadAccess: TAccesState;
    FWriteAccess: TAccesState;
  protected
    function GetReadAccess: TAccesState;
    function GetWriteAccess: TAccesState;
  public
    constructor Create(
      AReadAccess: TAccesState;
      AWriteAccess: TAccesState
    );
  end;

implementation

{ TReadWriteStateStatic }

constructor TReadWriteStateStatic.Create(AReadAccess,
  AWriteAccess: TAccesState);
begin
  inherited Create;
  FReadAccess := AReadAccess;
  FWriteAccess := AWriteAccess;
end;

function TReadWriteStateStatic.GetReadAccess: TAccesState;
begin
  Result := FReadAccess;
end;

function TReadWriteStateStatic.GetWriteAccess: TAccesState;
begin
  Result := FWriteAccess;
end;

end.

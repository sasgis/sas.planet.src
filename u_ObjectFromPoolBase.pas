unit u_ObjectFromPoolBase;

interface

uses
  u_ObjectPoolBase,
  u_ObjectFromPoolAbstract;

type
  TObjectFromPoolBase = class(TObjectFromPoolAbstract, IInterface)
  private
    FOnFreeObject: IFreeObjectProcedure;
  protected
    function CheckNeedDestroyObject: Boolean; override;
  public
    constructor Create(const AOnFreeObject: IFreeObjectProcedure);
  end;

implementation

{ TObjectFromPoolBase }

constructor TObjectFromPoolBase.Create(
  const AOnFreeObject: IFreeObjectProcedure);
begin
  Assert(AOnFreeObject <> nil);
  inherited Create;
  FOnFreeObject := AOnFreeObject;
end;

function TObjectFromPoolBase.CheckNeedDestroyObject: Boolean;
begin
  Result := FOnFreeObject.IsNeedDestroyObject(Self);
end;

end.

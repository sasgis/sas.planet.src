unit u_MarkerRingsConfigStatic;

interface

uses
  i_MarkerRingsConfig,
  u_BaseInterfacedObject;

type
  TMarkerRingsConfigStatic = class(TBaseInterfacedObject, IMarkerRingsConfigStatic)
  private
    FCount: Integer;
    FStepDistance: Double;
  private
    function GetCount: Integer;
    function GetStepDistance: Double;
  public
    constructor Create(
      ACount: Integer;
      const AStepDistance: Double
    );
  end;

implementation

{ TMarkerRingsConfigStatic }

constructor TMarkerRingsConfigStatic.Create(ACount: Integer;
  const AStepDistance: Double);
begin
  inherited Create;
  FCount := ACount;
  FStepDistance := AStepDistance;
end;

function TMarkerRingsConfigStatic.GetCount: Integer;
begin
  Result := FCount;
end;

function TMarkerRingsConfigStatic.GetStepDistance: Double;
begin
  Result := FStepDistance;
end;

end.

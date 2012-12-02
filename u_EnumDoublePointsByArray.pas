unit u_EnumDoublePointsByArray;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  u_BaseInterfacedObject;

type
  TEnumDoublePointsByArray = class(TBaseInterfacedObject, IEnumDoublePoint)
  private
    FPoints: PDoublePointArray;
    FCount: Integer;
    FIndex: Integer;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TEnumLonLatPointsByArray = class(TEnumDoublePointsByArray, IEnumLonLatPoint)
  end;

implementation

uses
  u_GeoFun;

{ TEnumDoublePointsByArray }

constructor TEnumDoublePointsByArray.Create(
  const APoints: PDoublePointArray;
  ACount: Integer
);
begin
  inherited Create;
  FPoints := APoints;
  FCount := ACount;
  FIndex := 0;
end;

function TEnumDoublePointsByArray.Next(out APoint: TDoublePoint): Boolean;
begin
  if FIndex < FCount then begin
    APoint := FPoints[FIndex];
    Inc(FIndex);
    Result := True;
  end else begin
    APoint := CEmptyDoublePoint;
    Result := False;
  end;
end;

end.

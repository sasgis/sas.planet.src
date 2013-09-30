unit u_EnumDoublePointsByArray;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  u_BaseInterfacedObject;

type
  TEnumDoublePointsByArray = class(TBaseInterfacedObject, IEnumDoublePoint)
  private
    FPoints: PDoublePointArray;
    FCount: Integer;
    FIndex: Integer;
    FPointsAggregator: IDoublePointsAggregator;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const APoints: PDoublePointArray;
      const ACount: Integer
    ); overload;
    constructor Create(
      const APointsAggregator: IDoublePointsAggregator
    ); overload;
    destructor Destroy; override;
  end;

  TEnumLonLatPointsByArray = class(TEnumDoublePointsByArray, IEnumLonLatPoint)
  end;

implementation

uses
  u_GeoFun;

{ TEnumDoublePointsByArray }

constructor TEnumDoublePointsByArray.Create(
  const APoints: PDoublePointArray;
  const ACount: Integer
);
begin
  inherited Create;
  FPointsAggregator := nil;
  FPoints := APoints;
  FCount := ACount;
  FIndex := 0;
end;

constructor TEnumDoublePointsByArray.Create(
  const APointsAggregator: IDoublePointsAggregator
);
begin
  Assert(Assigned(APointsAggregator));
  inherited Create;                   
  FPointsAggregator := APointsAggregator;
  FPoints := FPointsAggregator.Points;
  FCount := FPointsAggregator.Count;
  FIndex := 0;
end;

destructor TEnumDoublePointsByArray.Destroy;
begin
  FPointsAggregator := nil;
  inherited;
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

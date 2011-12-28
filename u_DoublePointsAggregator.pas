unit u_DoublePointsAggregator;

interface

uses
  t_GeoTypes,
  i_DoublePointsAggregator;

type
  TDoublePointsAggregator = class(TInterfacedObject, IDoublePointsAggregator)
  private
    FPoints: TArrayOfDoublePoint;
    FCount: Integer;
    FCapacity: Integer;
  private
    procedure Add(const APoint: TDoublePoint);
    procedure Clear;

    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TDoublePointsAggregator }

constructor TDoublePointsAggregator.Create;
begin
  FCount := 0;
end;

destructor TDoublePointsAggregator.Destroy;
begin
  FPoints := nil;
  FCount := 0;
  FCapacity := 0;
  inherited;
end;

procedure TDoublePointsAggregator.Add(const APoint: TDoublePoint);
var
  VSize: Integer;
begin
  if FCount >= FCapacity then begin
    if FCapacity < 256 then begin
      VSize := 256;
    end else if FCapacity < 4*1024 then begin
      VSize := FCapacity * 2;
    end else begin
      VSize := FCapacity + 4*1024;
    end;
    SetLength(FPoints, VSize);
    FCapacity := VSize;
  end;
  FPoints[FCount] := APoint;
  Inc(FCount);
end;

procedure TDoublePointsAggregator.Clear;
begin
  FCount := 0;
end;

function TDoublePointsAggregator.GetCount: Integer;
begin
  Result := FCount;
end;

function TDoublePointsAggregator.GetPoints: PDoublePointArray;
begin
  Result := @FPoints[0];
end;

end.

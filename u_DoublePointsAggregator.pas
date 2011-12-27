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
  inherited;
end;

procedure TDoublePointsAggregator.Add(const APoint: TDoublePoint);
var
  VSize: Integer;
begin
  VSize := Length(FPoints);
  if FCount <= VSize then begin
    if VSize < 256 then begin
      VSize := 256;
    end else if VSize < 4*1024 then begin
      VSize := VSize * 2;
    end else begin
      VSize := VSize + 4*1024;
    end;
    SetLength(FPoints, VSize);
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

unit u_EnumDoublePointBySingleLine;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_VectorItemLonLat,
  i_VectorItemProjected;

type
  TEnumDoublePointBySingleLine = class(TInterfacedObject, IEnumDoublePoint)
  private
    FSourceLine: IInterface;
    FPoints: PDoublePointArray;
    FCount: Integer;
    FIndex: Integer;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      ADataOwner: IInterface;
      APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

implementation

uses
  u_GeoFun;

{ TEnumDoublePointBySingleLine }

constructor TEnumDoublePointBySingleLine.Create(
  ADataOwner: IInterface;
  APoints: PDoublePointArray;
  ACount: Integer
);
begin
  FSourceLine := ADataOwner;
  FPoints := APoints;
  FCount := ACount;
  FIndex := 0;
  Assert(FCount > 0, 'No points');
end;

function TEnumDoublePointBySingleLine.Next(out APoint: TDoublePoint): Boolean;
begin
  if FIndex < FCount then begin
    APoint := FPoints[FIndex];
    Inc(FIndex);
    Result := True;
  end else begin
    Result := False;
    if FIndex = FCount then begin
      FPoints := nil;
      FSourceLine := nil;
      Inc(FIndex);
    end;
  end;
end;

end.

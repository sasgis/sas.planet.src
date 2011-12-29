unit u_LonLatSingleLine;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_VectorItemLonLat;

type
  TLonLatLineBase = class(TInterfacedObject)
  private
    FCount: Integer;
    FPoints: TArrayOfDoublePoint;
  private
    function GetEnum: IEnumDoublePoint;
    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
  public
    constructor Create(
      AClosed: Boolean;
      APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TLonLatPathLine = class(TLonLatLineBase, ILonLatPathLine)
  public
    constructor Create(
      APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TLonLatPolygonLine = class(TLonLatLineBase, ILonLatPolygonLine)
  public
    constructor Create(
      APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

implementation

uses
  u_GeoFun,
  u_EnumDoublePointBySingleLine;

{ TLineBase }

constructor TLonLatLineBase.Create(
  AClosed: Boolean;
  APoints: PDoublePointArray;
  ACount: Integer
);
begin
  FCount := ACount;
  Assert(FCount > 0, 'Empty line');
  if AClosed and not DoublePointsEqual(APoints[0], APoints[ACount - 1]) then begin
    Inc(FCount);
    SetLength(FPoints, FCount);
    Move(APoints^, FPoints[0], ACount * SizeOf(TDoublePoint));
    FPoints[FCount - 1] := FPoints[0];
  end else begin
    SetLength(FPoints, FCount);
    Move(APoints^, FPoints[0], ACount * SizeOf(TDoublePoint));
  end;
end;

function TLonLatLineBase.GetCount: Integer;
begin
  Result := FCount;
end;

function TLonLatLineBase.GetEnum: IEnumDoublePoint;
begin
  Result := TEnumDoublePointBySingleLine.Create(Self, @FPoints[0], FCount);
end;

function TLonLatLineBase.GetPoints: PDoublePointArray;
begin
  Result := @FPoints[0];
end;

{ TLonLatPathLine }

constructor TLonLatPathLine.Create(APoints: PDoublePointArray; ACount: Integer);
begin
  inherited Create(False, APoints, ACount);
end;

{ TLonLatPolygonLine }

constructor TLonLatPolygonLine.Create(APoints: PDoublePointArray;
  ACount: Integer);
begin
  inherited Create(True, APoints, ACount);
end;

end.

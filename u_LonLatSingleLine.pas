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
    FBounds: TDoubleRect;
    FPoints: TArrayOfDoublePoint;
  private
    function GetEnum: IEnumLonLatPoint;
    function GetBounds: TDoubleRect;
    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
  public
    constructor Create(
      AClosed: Boolean;
      const ABounds: TDoubleRect;
      APoints: PDoublePointArray;
      ACount: Integer
    ); overload;
    constructor Create(
      AClosed: Boolean;
      APoints: PDoublePointArray;
      ACount: Integer
    ); overload;
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
var
  VBounds: TDoubleRect;
  i: Integer;
begin
  VBounds.TopLeft := APoints[0];
  VBounds.BottomRight := APoints[0];
  for i := 1 to ACount - 1 do begin
    if VBounds.Left > APoints[i].X then begin
      VBounds.Left := APoints[i].X;
    end;
    if VBounds.Top < APoints[i].Y then begin
      VBounds.Top := APoints[i].Y;
    end;
    if VBounds.Right < APoints[i].X then begin
      VBounds.Right := APoints[i].X;
    end;
    if VBounds.Bottom > APoints[i].Y then begin
      VBounds.Bottom := APoints[i].Y;
    end;
  end;
  Create(AClosed, VBounds, APoints, ACount);
end;

constructor TLonLatLineBase.Create(
  AClosed: Boolean;
  const ABounds: TDoubleRect;
  APoints: PDoublePointArray;
  ACount: Integer
);
begin
  FBounds := ABounds;
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

function TLonLatLineBase.GetBounds: TDoubleRect;
begin
  Result := FBounds;
end;

function TLonLatLineBase.GetCount: Integer;
begin
  Result := FCount;
end;

function TLonLatLineBase.GetEnum: IEnumLonLatPoint;
begin
  Result := TEnumDoublePointBySingleLonLatLine.Create(Self, @FPoints[0], FCount);
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

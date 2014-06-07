unit u_GeometryLocal;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  i_GeometryLocal,
  u_BaseInterfacedObject;

type
  TGeometryLocalBase = class(TBaseInterfacedObject, IGeometryLocal)
  private
    FCount: Integer;
    FPoints: array of TDoublePoint;
  private
    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
  public
    constructor Create(
      AClosed: Boolean;
      const APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TGeometryLocalLine = class(TGeometryLocalBase, IGeometryLocalLine)
  private
    function GetEnum: IEnumLocalPoint;
  public
    constructor Create(
      const APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TGeometryLocalPolygon = class(TGeometryLocalBase, IGeometryLocalPolygon)
  private
    function GetEnum: IEnumLocalPoint;
  public
    constructor Create(
      const APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

implementation

uses
  u_GeoFunc,
  u_EnumDoublePointBySingleLine;

{ TLocalLineBase }

constructor TGeometryLocalBase.Create(
  AClosed: Boolean;
  const APoints: PDoublePointArray;
  ACount: Integer
);
begin
  inherited Create;
  FCount := ACount;
  Assert(FCount > 0, 'Empty line');
  if AClosed and (FCount > 1) and DoublePointsEqual(APoints[0], APoints[ACount - 1]) then begin
    Dec(FCount);
  end;

  SetLength(FPoints, FCount);
  Move(APoints^, FPoints[0], FCount * SizeOf(TDoublePoint));
end;

function TGeometryLocalBase.GetCount: Integer;
begin
  Result := FCount;
end;

function TGeometryLocalBase.GetPoints: PDoublePointArray;
begin
  Result := @FPoints[0];
end;

{ TLocalPathLine }

constructor TGeometryLocalLine.Create(
  const APoints: PDoublePointArray;
  ACount: Integer
);
begin
  inherited Create(False, APoints, ACount);
end;

function TGeometryLocalLine.GetEnum: IEnumLocalPoint;
begin
  Result := TEnumLocalPointBySingleLocalLine.Create(Self, False, @FPoints[0], FCount);
end;

{ TLocalPolygonLine }

constructor TGeometryLocalPolygon.Create(
  const APoints: PDoublePointArray;
  ACount: Integer
);
begin
  inherited Create(True, APoints, ACount);
end;

function TGeometryLocalPolygon.GetEnum: IEnumLocalPoint;
begin
  Result := TEnumLocalPointBySingleLocalLine.Create(Self, True, @FPoints[0], FCount);
end;

end.

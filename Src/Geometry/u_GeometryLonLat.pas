unit u_GeometryLonLat;

interface

uses
  t_GeoTypes,
  t_Hash,
  i_InterfaceListStatic,
  i_DoublePoints,
  i_EnumDoublePoint,
  i_LonLatRect,
  i_GeometryLonLat,
  u_BaseInterfacedObject;

type
  TGeometryLonLatPoint = class(TBaseInterfacedObject, IGeometryLonLat, IGeometryLonLatPoint)
  private
    FBounds: ILonLatRect;
    FHash: THashValue;
  private
    function GetBounds: ILonLatRect;
    function GetHash: THashValue;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSame(const APoint: IGeometryLonLatPoint): Boolean;
    function GetGoToPoint: TDoublePoint;
    function GetPoint: TDoublePoint;
  public
    constructor Create(
      const AHash: THashValue;
      const ABounds: ILonLatRect
    );
  end;

  TGeometryLonLatBase = class(TBaseInterfacedObject)
  private
    FCount: Integer;
    FBounds: ILonLatRect;
    FHash: THashValue;
    FPoints: IDoublePoints;
  private
    function GetBounds: ILonLatRect;
    function GetHash: THashValue;
    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
  public
    constructor Create(
      AClosed: Boolean;
      const ABounds: ILonLatRect;
      const AHash: THashValue;
      const APoints: IDoublePoints
    ); overload;
  end;

  TGeometryLonLatSingleLine = class(TGeometryLonLatBase, IGeometryLonLat, IGeometryLonLatLine, IGeometryLonLatSingleLine)
  private
    function GetEnum: IEnumLonLatPoint;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSame(const ALine: IGeometryLonLatSingleLine): Boolean;
    function GetGoToPoint: TDoublePoint;
  public
    constructor Create(
      const ABounds: ILonLatRect;
      const AHash: THashValue;
      const APoints: IDoublePoints
    );
  end;

  TGeometryLonLatContour = class(TGeometryLonLatBase, IGeometryLonLat, IGeometryLonLatPolygon, IGeometryLonLatContour)
  private
    function GetEnum: IEnumLonLatPoint;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSame(const ALine: IGeometryLonLatContour): Boolean;
    function GetGoToPoint: TDoublePoint;
  public
    constructor Create(
      const ABounds: ILonLatRect;
      const AHash: THashValue;
      const APoints: IDoublePoints
    );
  end;

  TGeometryLonLatSinglePolygon = class(TGeometryLonLatContour, IGeometryLonLatSinglePolygon)
  private
    function IsSame(const ALine: IGeometryLonLatSinglePolygon): Boolean;
    function GetOuterBorder: IGeometryLonLatContour;
    function GetHoleCount: Integer;
    function GetHoleBorder(const AIndex: Integer): IGeometryLonLatContour;
  public
    constructor Create(
      const ABounds: ILonLatRect;
      const AHash: THashValue;
      const APoints: IDoublePoints
    );
  end;

  TGeometryLonLatSinglePolygonWithHoles = class(TBaseInterfacedObject, IGeometryLonLat, IGeometryLonLatPolygon, IGeometryLonLatSinglePolygon)
  private
    FBounds: ILonLatRect;
    FHash: THashValue;
    FOuterBorder: IGeometryLonLatContour;
    FHoleList: IInterfaceListStatic;
  private
    function GetBounds: ILonLatRect;
    function GetHash: THashValue;
    function GetGoToPoint: TDoublePoint;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
  private
    function IsSame(const ALine: IGeometryLonLatSinglePolygon): Boolean;
    function GetOuterBorder: IGeometryLonLatContour;
    function GetHoleCount: Integer;
    function GetHoleBorder(const AIndex: Integer): IGeometryLonLatContour;
  public
    constructor Create(
      const ABounds: ILonLatRect;
      const AHash: THashValue;
      const AOuterBorder: IGeometryLonLatContour;
      const AHoleList: IInterfaceListStatic
    );
  end;

implementation

uses
  Math,
  SysUtils,
  u_GeoFunc,
  u_EnumDoublePointBySingleLine;

{ TGeometryLonLatBase }

constructor TGeometryLonLatBase.Create(
  AClosed: Boolean;
  const ABounds: ILonLatRect;
  const AHash: THashValue;
  const APoints: IDoublePoints
);
begin
  Assert(Assigned(APoints));
  Assert(APoints.Count > 0, 'Empty line');
  inherited Create;
  FBounds := ABounds;
  FHash := AHash;
  FPoints := APoints;
  FCount := FPoints.Count;

  if AClosed and (FCount > 1) and DoublePointsEqual(FPoints.Points[0], FPoints.Points[FCount - 1]) then begin
    Dec(FCount);
  end;
end;

function TGeometryLonLatBase.GetBounds: ILonLatRect;
begin
  Result := FBounds;
end;

function TGeometryLonLatBase.GetCount: Integer;
begin
  Result := FCount;
end;

function TGeometryLonLatBase.GetHash: THashValue;
begin
  Result := FHash;
end;

function TGeometryLonLatBase.GetPoints: PDoublePointArray;
begin
  Result := FPoints.Points;
end;

{ TGeometryLonLatSingleLine }

constructor TGeometryLonLatSingleLine.Create(
  const ABounds: ILonLatRect;
  const AHash: THashValue;
  const APoints: IDoublePoints
);
begin
  inherited Create(False, ABounds, AHash, APoints);
end;

function TGeometryLonLatSingleLine.GetEnum: IEnumLonLatPoint;
begin
  Result := TEnumDoublePointBySingleLonLatLine.Create(FPoints, False, FPoints.Points, FCount);
end;

function TGeometryLonLatSingleLine.GetGoToPoint: TDoublePoint;
begin
  if GetCount > 0 then begin
    Result := GetPoints[0];
  end else begin
    Result := CEmptyDoublePoint;
  end;
end;

function TGeometryLonLatSingleLine.IsSame(const ALine: IGeometryLonLatSingleLine): Boolean;
begin
  if ALine = IGeometryLonLatSingleLine(Self) then begin
    Result := True;
    Exit;
  end;

  if FCount <> ALine.Count then begin
    Result := False;
    Exit;
  end;

  if (FHash <> 0) and (ALine.Hash <> 0) then begin
    Result := FHash = ALine.Hash;
  end else begin
    if not FBounds.IsEqual(ALine.Bounds) then begin
      Result := False;
      Exit;
    end;

    Result := CompareMem(FPoints.Points, ALine.Points, FCount * SizeOf(TDoublePoint));
  end;
end;

function TGeometryLonLatSingleLine.IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
var
  VLine: IGeometryLonLatSingleLine;
begin
  if AGeometry = nil then begin
    Result := False;
    Exit;
  end;
  if AGeometry = IGeometryLonLat(Self) then begin
    Result := True;
    Exit;
  end;
  if (FHash <> 0) and (AGeometry.Hash <> 0) and (FHash <> AGeometry.Hash) then begin
    Result := False;
    Exit;
  end;

  Result := False;
  if Supports(AGeometry, IGeometryLonLatSingleLine, VLine) then begin
    Result := IsSame(VLine);
  end;
end;

{ TGeometryLonLatContour }

constructor TGeometryLonLatContour.Create(
  const ABounds: ILonLatRect;
  const AHash: THashValue;
  const APoints: IDoublePoints
);
begin
  inherited Create(True, ABounds, AHash, APoints);
end;

function TGeometryLonLatContour.GetEnum: IEnumLonLatPoint;
begin
  Result := TEnumDoublePointBySingleLonLatLine.Create(FPoints, True, FPoints.Points, FCount);
end;

function TGeometryLonLatContour.GetGoToPoint: TDoublePoint;
begin
  Result := FBounds.CalcRectCenter;
end;

function TGeometryLonLatContour.IsSame(const ALine: IGeometryLonLatContour): Boolean;
begin
  if ALine = IGeometryLonLatContour(Self) then begin
    Result := True;
    Exit;
  end;

  if FCount <> ALine.Count then begin
    Result := False;
    Exit;
  end;

  if (FHash <> 0) and (ALine.Hash <> 0) then begin
    Result := FHash = ALine.Hash;
  end else begin
    if not FBounds.IsEqual(ALine.Bounds) then begin
      Result := False;
      Exit;
    end;
    Result := CompareMem(FPoints.Points, ALine.Points, FCount * SizeOf(TDoublePoint));
  end;
end;

function TGeometryLonLatContour.IsSameGeometry(
  const AGeometry: IGeometryLonLat
): Boolean;
var
  VLine: IGeometryLonLatContour;
begin
  if AGeometry = nil then begin
    Result := False;
    Exit;
  end;
  if AGeometry = IGeometryLonLat(Self) then begin
    Result := True;
    Exit;
  end;
  if (FHash <> 0) and (AGeometry.Hash <> 0) and (FHash <> AGeometry.Hash) then begin
    Result := False;
    Exit;
  end;

  Result := False;
  if Supports(AGeometry, IGeometryLonLatContour, VLine) then begin
    Result := IsSame(VLine);
  end;
end;

{ TGeometryLonLatSinglePolygon }

constructor TGeometryLonLatSinglePolygon.Create(
  const ABounds: ILonLatRect;
  const AHash: THashValue;
  const APoints: IDoublePoints
);
begin
  inherited Create(ABounds, AHash, APoints);
end;

function TGeometryLonLatSinglePolygon.GetHoleBorder(
  const AIndex: Integer
): IGeometryLonLatContour;
begin
  Result := nil;
  Assert(False);
end;

function TGeometryLonLatSinglePolygon.GetHoleCount: Integer;
begin
  Result := 0;
end;

function TGeometryLonLatSinglePolygon.GetOuterBorder: IGeometryLonLatContour;
begin
  Result := Self;
end;

function TGeometryLonLatSinglePolygon.IsSame(
  const ALine: IGeometryLonLatSinglePolygon
): Boolean;
begin
  Result := IsSameGeometry(ALine);
end;

{ TGeometryLonLatPoint }

constructor TGeometryLonLatPoint.Create(
  const AHash: THashValue;
  const ABounds: ILonLatRect
);
begin
  Assert(Assigned(ABounds));
  Assert(DoublePointsEqual(ABounds.TopLeft, ABounds.BottomRight));
  inherited Create;
  FHash := AHash;
  FBounds := ABounds;
end;

function TGeometryLonLatPoint.GetBounds: ILonLatRect;
begin
  Result := FBounds;
end;

function TGeometryLonLatPoint.GetGoToPoint: TDoublePoint;
begin
  Result := GetPoint;
end;

function TGeometryLonLatPoint.GetHash: THashValue;
begin
  Result := FHash;
end;

function TGeometryLonLatPoint.GetPoint: TDoublePoint;
begin
  Result := FBounds.TopLeft;
end;

function TGeometryLonLatPoint.IsSame(
  const APoint: IGeometryLonLatPoint
): Boolean;
begin
  if not Assigned(APoint) then begin
    Result := False;
    Exit;
  end;
  if APoint = IGeometryLonLatPoint(Self) then begin
    Result := True;
    Exit;
  end;
  if (FHash <> 0) and (APoint.Hash <> 0) and (FHash <> APoint.Hash) then begin
    Result := False;
    Exit;
  end;
  Result := FBounds.IsEqual(APoint.Bounds);
end;

function TGeometryLonLatPoint.IsSameGeometry(
  const AGeometry: IGeometryLonLat
): Boolean;
var
  VPoint: IGeometryLonLatPoint;
begin
  if not Assigned(AGeometry) then begin
    Result := False;
    Exit;
  end;
  if AGeometry = IGeometryLonLat(Self) then begin
    Result := True;
    Exit;
  end;
  if (FHash <> 0) and (AGeometry.Hash <> 0) and (FHash <> AGeometry.Hash) then begin
    Result := False;
    Exit;
  end;
  Result := False;
  if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
    Result := FBounds.IsEqual(VPoint.Bounds);
  end;
end;

{ TGeometryLonLatSinglePolygonWithHoles }

constructor TGeometryLonLatSinglePolygonWithHoles.Create(
  const ABounds: ILonLatRect;
  const AHash: THashValue;
  const AOuterBorder: IGeometryLonLatContour;
  const AHoleList: IInterfaceListStatic
);
begin
  Assert(Assigned(ABounds));
  Assert(Assigned(AOuterBorder));
  Assert(Assigned(AHoleList));
  Assert(AHoleList.Count > 0);
  inherited Create;
  FBounds := ABounds;
  FHash := AHash;
  FOuterBorder := AOuterBorder;
  FHoleList := AHoleList;
end;

function TGeometryLonLatSinglePolygonWithHoles.GetBounds: ILonLatRect;
begin
  Result := FBounds;
end;

function TGeometryLonLatSinglePolygonWithHoles.GetGoToPoint: TDoublePoint;
begin
  Result := FOuterBorder.GetGoToPoint;
end;

function TGeometryLonLatSinglePolygonWithHoles.GetHash: THashValue;
begin
  Result := FHash;
end;

function TGeometryLonLatSinglePolygonWithHoles.GetHoleBorder(
  const AIndex: Integer
): IGeometryLonLatContour;
begin
  Result := IGeometryLonLatContour(FHoleList.Items[AIndex]);
end;

function TGeometryLonLatSinglePolygonWithHoles.GetHoleCount: Integer;
begin
  Result := FHoleList.Count;
end;

function TGeometryLonLatSinglePolygonWithHoles.GetOuterBorder: IGeometryLonLatContour;
begin
  Result := FOuterBorder;
end;

function TGeometryLonLatSinglePolygonWithHoles.IsSame(
  const ALine: IGeometryLonLatSinglePolygon
): Boolean;
var
  i: Integer;
begin
  if ALine = nil then begin
    Result := False;
    Exit;
  end;

  if ALine = IGeometryLonLatSinglePolygon(Self) then begin
    Result := True;
    Exit;
  end;

  if (FHash <> 0) and (ALine.Hash <> 0) then begin
    Result := FHash = ALine.Hash;
  end else begin
    if FHoleList.Count <> ALine.HoleCount then begin
      Result := False;
      Exit;
    end;
    if not FOuterBorder.IsSame(ALine.OuterBorder) then begin
      Result := False;
      Exit;
    end;

    for i := 0 to FHoleList.Count - 1 do begin
      if not GetHoleBorder(i).IsSame(ALine.HoleBorder[i]) then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;
end;

function TGeometryLonLatSinglePolygonWithHoles.IsSameGeometry(
  const AGeometry: IGeometryLonLat
): Boolean;
var
  VLine: IGeometryLonLatSinglePolygon;
begin
  if AGeometry = nil then begin
    Result := False;
    Exit;
  end;
  if AGeometry = IGeometryLonLat(Self) then begin
    Result := True;
    Exit;
  end;
  if (FHash <> 0) and (AGeometry.Hash <> 0) and (FHash <> AGeometry.Hash) then begin
    Result := False;
    Exit;
  end;

  Result := False;
  if Supports(AGeometry, IGeometryLonLatSinglePolygon, VLine) then begin
    Result := IsSame(VLine);
  end;
end;

end.

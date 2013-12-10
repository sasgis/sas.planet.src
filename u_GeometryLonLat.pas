unit u_GeometryLonLat;

interface

uses
  t_GeoTypes,
  t_Hash,
  i_EnumDoublePoint,
  i_LonLatRect,
  i_Datum,
  i_NotifierOperation,
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
    function GetGoToLonLat: TDoublePoint;
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
    FPoints: array of TDoublePoint;
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
      const APoints: PDoublePointArray;
      ACount: Integer
    ); overload;
  end;

  TGeometryLonLatLine = class(TGeometryLonLatBase, IGeometryLonLat, IGeometryLonLatLine)
  private
    function GetEnum: IEnumLonLatPoint;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSame(const ALine: IGeometryLonLatLine): Boolean;
    function GetGoToLonLat: TDoublePoint;
    function CalcLength(const ADatum: IDatum): Double;
  public
    constructor Create(
      const ABounds: ILonLatRect;
      const AHash: THashValue;
      const APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TGeometryLonLatPolygon = class(TGeometryLonLatBase, IGeometryLonLat, IGeometryLonLatPolygon)
  private
    function GetEnum: IEnumLonLatPoint;
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSame(const ALine: IGeometryLonLatPolygon): Boolean;
    function GetGoToLonLat: TDoublePoint;
    function CalcPerimeter(const ADatum: IDatum): Double;
    function CalcArea(
      const ADatum: IDatum;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;
  public
    constructor Create(
      const ABounds: ILonLatRect;
      const AHash: THashValue;
      const APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

implementation

uses
  SysUtils,
  u_GeoFun,
  u_EnumDoublePointBySingleLine;

{ TLineBase }

constructor TGeometryLonLatBase.Create(
  AClosed: Boolean;
  const ABounds: ILonLatRect;
  const AHash: THashValue;
  const APoints: PDoublePointArray;
  ACount: Integer
);
begin
  inherited Create;
  FBounds := ABounds;
  FHash := AHash;
  FCount := ACount;
  Assert(FCount > 0, 'Empty line');

  if AClosed and (FCount > 1) and DoublePointsEqual(APoints[0], APoints[ACount - 1]) then begin
    Dec(FCount);
  end;

  SetLength(FPoints, FCount);
  Move(APoints^, FPoints[0], FCount * SizeOf(TDoublePoint));
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
  Result := @FPoints[0];
end;

{ TLonLatPathLine }

function TGeometryLonLatLine.CalcLength(const ADatum: IDatum): Double;
var
  VEnum: IEnumLonLatPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  Result := 0;
  VEnum := GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      Result := Result + ADatum.CalcDist(VPrevPoint, VCurrPoint);
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

constructor TGeometryLonLatLine.Create(
  const ABounds: ILonLatRect;
  const AHash: THashValue;
  const APoints: PDoublePointArray;
  ACount: Integer
);
begin
  inherited Create(False, ABounds, AHash, APoints, ACount);
end;

function TGeometryLonLatLine.GetEnum: IEnumLonLatPoint;
begin
  Result := TEnumDoublePointBySingleLonLatLine.Create(Self, False, @FPoints[0], FCount);
end;

function TGeometryLonLatLine.GetGoToLonLat: TDoublePoint;
begin
  if GetCount > 0 then begin
    Result := GetPoints[0];
  end else begin
    Result := CEmptyDoublePoint;
  end;
end;

function TGeometryLonLatLine.IsSame(const ALine: IGeometryLonLatLine): Boolean;
var
  i: Integer;
  VPoints: PDoublePointArray;
begin
  if ALine = IGeometryLonLatLine(Self) then begin
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

    VPoints := ALine.Points;

    for i := 0 to FCount - 1 do begin
      if not DoublePointsEqual(FPoints[i], VPoints[i]) then begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
  end;
end;

function TGeometryLonLatLine.IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
var
  VLine: IGeometryLonLatLine;
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
  if Supports(AGeometry, IGeometryLonLatLine, VLine) then begin
    Result := IsSame(VLine);
  end;
end;

{ TLonLatPolygonLine }

function TGeometryLonLatPolygon.CalcArea(
  const ADatum: IDatum;
  const ANotifier: INotifierOperation = nil;
  const AOperationID: Integer = 0
): Double;
begin
  if FCount < 3 then begin
    Result := 0;
  end else begin
    Result := ADatum.CalcPolygonArea(@FPoints[0], FCount, ANotifier, AOperationID);
  end;
end;

function TGeometryLonLatPolygon.CalcPerimeter(const ADatum: IDatum): Double;
var
  VEnum: IEnumLonLatPoint;
  VPrevPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
begin
  Result := 0;
  VEnum := GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      Result := Result + ADatum.CalcDist(VPrevPoint, VCurrPoint);
      VPrevPoint := VCurrPoint;
    end;
  end;
end;

constructor TGeometryLonLatPolygon.Create(
  const ABounds: ILonLatRect;
  const AHash: THashValue;
  const APoints: PDoublePointArray;
  ACount: Integer
);
begin
  inherited Create(True, ABounds, AHash, APoints, ACount);
end;

function TGeometryLonLatPolygon.GetEnum: IEnumLonLatPoint;
begin
  Result := TEnumDoublePointBySingleLonLatLine.Create(Self, True, @FPoints[0], FCount);
end;

function TGeometryLonLatPolygon.GetGoToLonLat: TDoublePoint;
begin
  Result := FBounds.CalcRectCenter;
end;

function TGeometryLonLatPolygon.IsSame(const ALine: IGeometryLonLatPolygon): Boolean;
var
  i: Integer;
  VPoints: PDoublePointArray;
begin
  if ALine = IGeometryLonLatPolygon(Self) then begin
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

    VPoints := ALine.Points;

    for i := 0 to FCount - 1 do begin
      if not DoublePointsEqual(FPoints[i], VPoints[i]) then begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
  end;
end;

function TGeometryLonLatPolygon.IsSameGeometry(
  const AGeometry: IGeometryLonLat
): Boolean;
var
  VLine: IGeometryLonLatPolygon;
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
  if Supports(AGeometry, IGeometryLonLatPolygon, VLine) then begin
    Result := IsSame(VLine);
  end;
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

function TGeometryLonLatPoint.GetGoToLonLat: TDoublePoint;
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

end.

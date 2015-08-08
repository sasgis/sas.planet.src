unit u_GeometryLonLatFactory;

interface

uses
  t_GeoTypes,
  i_HashFunction,
  i_ProjectionInfo,
  i_EnumDoublePoint,
  i_DoublePoints,
  i_DoublePointFilter,
  i_DoublePointsAggregator,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  u_BaseInterfacedObject;

type
  TGeometryLonLatFactory = class(TBaseInterfacedObject, IGeometryLonLatFactory)
  private
    FHashFunction: IHashFunction;
    function CreateLonLatLineInternal(
      const ARect: TDoubleRect;
      const APoints: IDoublePoints
    ): IGeometryLonLatSingleLine;
    function CreateLonLatPolygonInternal(
      const ARect: TDoubleRect;
      const APoints: IDoublePoints
    ): IGeometryLonLatSinglePolygon;
  private
    function CreateLonLatPoint(
      const APoint: TDoublePoint
    ): IGeometryLonLatPoint;
    function CreateLonLatSingleLine(
      const APoints: IDoublePoints
    ): IGeometryLonLatSingleLine;
    function CreateLonLatSinglePolygon(
      const APoints: IDoublePoints
    ): IGeometryLonLatSinglePolygon;

    function MakeLineBuilder(): IGeometryLonLatLineBuilder;
    function MakePolygonBuilder(): IGeometryLonLatPolygonBuilder;

    function CreateLonLatLine(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatLine;
    function CreateLonLatPolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatPolygon;
    function CreateLonLatLineByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatLine;
    function CreateLonLatPolygonByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatPolygon;

    function CreateLonLatPolygonByRect(
      const ARect: TDoubleRect
    ): IGeometryLonLatPolygon;

    function CreateLonLatPolygonCircleByPoint(
      const AProjection: IProjectionInfo;
      const APos: TDoublePoint;
      const ARadius: double
    ): IGeometryLonLatPolygon;

    function CreateLonLatPolygonByLonLatPathAndFilter(
      const ASource: IGeometryLonLatLine;
      const AFilter: ILonLatPointFilter
    ): IGeometryLonLatPolygon;
  public
    constructor Create(const AHashFunction: IHashFunction);
  end;

implementation

uses
  Math,
  SysUtils,
  t_Hash,
  i_LonLatRect,
  i_Datum,
  i_InterfaceListSimple,
  u_GeoFunc,
  u_InterfaceListSimple,
  u_DoublePointsAggregator,
  u_GeometryLonLat,
  u_DoublePoints,
  u_LonLatRect,
  u_LonLatRectByPoint,
  u_EnumDoublePointByLineSet,
  u_GeometryLonLatMulti;

type
  TGeometryLonLatLineBuilder = class(TBaseInterfacedObject, IGeometryLonLatLineBuilder)
  private
    FHashFunction: IHashFunction;
    FHash: THashValue;
    FBounds: TDoubleRect;
    FLine: IGeometryLonLatSingleLine;
    FList: IInterfaceListSimple;
  private
    procedure AddLine(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    ); overload;
    procedure AddLine(
      const APoints: IDoublePoints
    ); overload;

    function MakeStaticAndClear: IGeometryLonLatLine;
    function MakeStaticCopy: IGeometryLonLatLine;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

{ TGeometryLonLatLineBuilder }

constructor TGeometryLonLatLineBuilder.Create(
  const AHashFunction: IHashFunction
);
begin
  inherited Create;
  FHashFunction := AHashFunction;
end;

procedure TGeometryLonLatLineBuilder.AddLine(
  const ABounds: TDoubleRect;
  const APoints: IDoublePoints
);
var
  VLine: IGeometryLonLatSingleLine;
  VHash: THashValue;
  VRect: ILonLatRect;
begin
  Assert(Assigned(APoints));
  if APoints.Count > 1 then begin
    VRect := TLonLatRect.Create(ABounds);
  end else begin
    VRect := TLonLatRectByPoint.Create(ABounds.TopLeft);
  end;
  VHash := FHashFunction.CalcHashByBuffer(APoints.Points, APoints.Count * SizeOf(TDoublePoint));
  VLine := TGeometryLonLatSingleLine.Create(VRect, VHash, APoints);

  if not Assigned(FLine) then begin
    FLine := VLine;
    FHash := VHash;
    FBounds := ABounds;
  end else begin
    if not Assigned(FList) then begin
      FList := TInterfaceListSimple.Create;
      FList.Add(FLine);
    end else if FList.Count = 0 then begin
      FList.Add(FLine);
    end;
    FList.Add(VLine);
    FHashFunction.UpdateHashByHash(FHash, VHash);
    FBounds := VRect.UnionWithRect(ABounds);
  end;
end;

procedure TGeometryLonLatLineBuilder.AddLine(
  const APoints: IDoublePoints
);
begin
  Assert(Assigned(APoints));
  AddLine(LonLatMBRByPoints(APoints.Points, APoints.Count), APoints);
end;

function TGeometryLonLatLineBuilder.MakeStaticAndClear: IGeometryLonLatLine;
var
  VRect: ILonLatRect;
begin
  Result := nil;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      VRect := TLonLatRect.Create(FBounds);
      Result := TGeometryLonLatMultiLine.Create(VRect, FHash, FList.MakeStaticAndClear);
    end else begin
      Result := FLine;
    end;
    FLine := nil;
  end;
end;

function TGeometryLonLatLineBuilder.MakeStaticCopy: IGeometryLonLatLine;
var
  VRect: ILonLatRect;
begin
  Result := nil;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      VRect := TLonLatRect.Create(FBounds);
      Result := TGeometryLonLatMultiLine.Create(VRect, FHash, FList.MakeStaticCopy);
    end else begin
      Result := FLine;
    end;
  end;
end;

type
  TGeometryLonLatPolygonBuilder = class(TBaseInterfacedObject, IGeometryLonLatPolygonBuilder)
  private
    FHashFunction: IHashFunction;
    FHash: THashValue;
    FBounds: TDoubleRect;
    FLine: IGeometryLonLatSinglePolygon;
    FList: IInterfaceListSimple;
  private
    procedure AddOuter(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    ); overload;
    procedure AddOuter(
      const APoints: IDoublePoints
    ); overload;
    procedure AddHole(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    ); overload;
    procedure AddHole(
      const APoints: IDoublePoints
    ); overload;

    procedure Add(const AElement: IGeometryLonLatSinglePolygon);

    function MakeStaticAndClear: IGeometryLonLatPolygon;
    function MakeStaticCopy: IGeometryLonLatPolygon;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

{ TGeometryLonLatPolygonBuilder }

constructor TGeometryLonLatPolygonBuilder.Create(
  const AHashFunction: IHashFunction
);
begin
  inherited Create;
  FHashFunction := AHashFunction;
end;

procedure TGeometryLonLatPolygonBuilder.AddOuter(
  const APoints: IDoublePoints
);
begin
  Assert(Assigned(APoints));
  AddOuter(LonLatMBRByPoints(APoints.Points, APoints.Count), APoints);
end;

procedure TGeometryLonLatPolygonBuilder.AddOuter(
  const ABounds: TDoubleRect;
  const APoints: IDoublePoints
);
begin

end;

procedure TGeometryLonLatPolygonBuilder.AddHole(
  const APoints: IDoublePoints
);
begin
  Assert(Assigned(APoints));
  AddHole(LonLatMBRByPoints(APoints.Points, APoints.Count), APoints);
end;

procedure TGeometryLonLatPolygonBuilder.AddHole(
  const ABounds: TDoubleRect;
  const APoints: IDoublePoints
);
begin

end;

procedure TGeometryLonLatPolygonBuilder.Add(
  const AElement: IGeometryLonLatSinglePolygon
);
begin
  Assert(Assigned(AElement));
  if not Assigned(FLine) then begin
    FLine := AElement;
    FHash := FLine.Hash;
    FBounds := FLine.Bounds.Rect;
  end else begin
    if not Assigned(FList) then begin
      FList := TInterfaceListSimple.Create;
      FList.Add(FLine);
    end else if FList.Count = 0 then begin
      FList.Add(FLine);
    end;
    FList.Add(AElement);
    FHashFunction.UpdateHashByHash(FHash, AElement.Hash);
    FBounds := AElement.Bounds.UnionWithRect(FBounds);
  end;
end;

function TGeometryLonLatPolygonBuilder.MakeStaticAndClear: IGeometryLonLatPolygon;
var
  VRect: ILonLatRect;
begin
  Result := nil;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      VRect := TLonLatRect.Create(FBounds);
      Result := TGeometryLonLatMultiPolygon.Create(VRect, FHash, FList.MakeStaticAndClear);
    end else begin
      Result := FLine;
    end;
    FLine := nil;
  end;
end;

function TGeometryLonLatPolygonBuilder.MakeStaticCopy: IGeometryLonLatPolygon;
var
  VRect: ILonLatRect;
begin
  Result := nil;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      VRect := TLonLatRect.Create(FBounds);
      Result := TGeometryLonLatMultiPolygon.Create(VRect, FHash, FList.MakeStaticCopy);
    end else begin
      Result := FLine;
    end;
  end;
end;

{ TVectorGeometryLonLatFactory }

constructor TGeometryLonLatFactory.Create(
  const AHashFunction: IHashFunction);
begin
  Assert(Assigned(AHashFunction));
  inherited Create;
  FHashFunction := AHashFunction;
end;

function TGeometryLonLatFactory.CreateLonLatPolygonCircleByPoint(
  const AProjection: IProjectionInfo;
  const APos: TDoublePoint;
  const ARadius: double
): IGeometryLonLatPolygon;
const
  CPointCount = 64;
var
  VAggreagator: IDoublePointsAggregator;
  j: Integer;
  VDatum: IDatum;
  VAngle: Double;
  VPoint: TDoublePoint;
  VBounds: TDoubleRect;
begin
  Assert(not PointIsEmpty(APos));
  VAggreagator := TDoublePointsAggregator.Create(CPointCount);
  VBounds.TopLeft := APos;
  VBounds.BottomRight := APos;
  VDatum := AProjection.GeoConverter.Datum;
  for j := 0 to CPointCount - 1 do begin
    VAngle := j * 360 / CPointCount;
    VPoint := VDatum.CalcFinishPosition(APos, VAngle, ARadius);
    VAggreagator.Add(VPoint);
    UpdateLonLatMBRByPoint(VBounds, VPoint);
  end;
  Result := CreateLonLatPolygonInternal(VBounds, VAggreagator.MakeStaticCopy);
end;

function TGeometryLonLatFactory.CreateLonLatSingleLine(
  const APoints: IDoublePoints
): IGeometryLonLatSingleLine;
var
  i: Integer;
  VPoints: PDoublePointArray;
  VCount: Integer;
  VPoint: TDoublePoint;
  VBounds: TDoubleRect;
begin
  Result := nil;
  Assert(Assigned(APoints));
  if Assigned(APoints) then begin
    VPoints := APoints.Points;
    VCount := APoints.Count;
    VPoint := VPoints[0];
    Assert(not PointIsEmpty(VPoint));
    if PointIsEmpty(VPoint) then begin
      Exit;
    end;
    VBounds.TopLeft := VPoint;
    VBounds.BottomRight := VPoint;
    for i := 1 to VCount - 1 do begin
      VPoint := VPoints[i];
      Assert(not PointIsEmpty(VPoint));
      if PointIsEmpty(VPoint) then begin
        Exit;
      end;
      UpdateLonLatMBRByPoint(VBounds, VPoint);
    end;
    Result := CreateLonLatLineInternal(VBounds, APoints);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatSinglePolygon(
  const APoints: IDoublePoints
): IGeometryLonLatSinglePolygon;
var
  i: Integer;
  VPoints: PDoublePointArray;
  VCount: Integer;
  VPoint: TDoublePoint;
  VBounds: TDoubleRect;
begin
  Result := nil;
  Assert(Assigned(APoints));
  if Assigned(APoints) then begin
    VPoints := APoints.Points;
    VCount := APoints.Count;
    VPoint := VPoints[0];
    Assert(not PointIsEmpty(VPoint));
    if PointIsEmpty(VPoint) then begin
      Exit;
    end;
    VBounds.TopLeft := VPoint;
    VBounds.BottomRight := VPoint;
    for i := 1 to VCount - 1 do begin
      VPoint := VPoints[i];
      Assert(not PointIsEmpty(VPoint));
      if PointIsEmpty(VPoint) then begin
        Exit;
      end;
      UpdateLonLatMBRByPoint(VBounds, VPoint);
    end;
    Result := CreateLonLatPolygonInternal(VBounds, APoints);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatLineInternal(
  const ARect: TDoubleRect;
  const APoints: IDoublePoints
): IGeometryLonLatSingleLine;
var
  VHash: THashValue;
  VRect: ILonLatRect;
begin
  Result := nil;
  if Assigned(APoints) then begin
    if APoints.Count > 1 then begin
      VRect := TLonLatRect.Create(ARect);
    end else begin
      VRect := TLonLatRectByPoint.Create(ARect.TopLeft);
    end;
    VHash := FHashFunction.CalcHashByBuffer(APoints.Points, APoints.Count * SizeOf(TDoublePoint));
    Result := TGeometryLonLatSingleLine.Create(VRect, VHash, APoints);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatPolygonInternal(
  const ARect: TDoubleRect;
  const APoints: IDoublePoints
): IGeometryLonLatSinglePolygon;
var
  VHash: THashValue;
  VRect: ILonLatRect;
begin
  Result := nil;
  if Assigned(APoints) then begin
    if APoints.Count > 1 then begin
      VRect := TLonLatRect.Create(ARect);
    end else begin
      VRect := TLonLatRectByPoint.Create(ARect.TopLeft);
    end;
    VHash := FHashFunction.CalcHashByBuffer(APoints.Points, APoints.Count * SizeOf(TDoublePoint));
    Result := TGeometryLonLatSinglePolygon.Create(VRect, VHash, APoints);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatLine(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatLine;
var
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VPoint: TDoublePoint;
  VPoints: IDoublePoints;
  VLineBounds: TDoubleRect;
  VBuilder: IGeometryLonLatLineBuilder;
begin
  VBuilder := MakeLineBuilder;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        VPoints := TDoublePoints.Create(VStart, VLineLen);
        VBuilder.AddLine(VLineBounds, VPoints);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        UpdateLonLatMBRByPoint(VLineBounds, VPoint);
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    VPoints := TDoublePoints.Create(VStart, VLineLen);
    VBuilder.AddLine(VLineBounds, VPoints);
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryLonLatFactory.CreateLonLatLineByEnum(
  const AEnum: IEnumLonLatPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryLonLatLine;
var
  VPoint: TDoublePoint;
  VTemp: IDoublePointsAggregator;
  VLineBounds: TDoubleRect;
  VBuilder: IGeometryLonLatLineBuilder;
begin
  VBuilder := MakeLineBuilder;
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        VBuilder.AddLine(VLineBounds, VTemp.MakeStaticCopy);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        UpdateLonLatMBRByPoint(VLineBounds, VPoint);
      end;
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    VBuilder.AddLine(VLineBounds, ATemp.MakeStaticCopy);
    VTemp.Clear;
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryLonLatFactory.CreateLonLatPoint(
  const APoint: TDoublePoint
): IGeometryLonLatPoint;
var
  VHash: THashValue;
  VRect: ILonLatRect;
begin
  VHash := FHashFunction.CalcHashByDoublePoint(APoint);
  VRect := TLonLatRectByPoint.Create(APoint);
  Result := TGeometryLonLatPoint.Create(VHash, VRect);
end;

function TGeometryLonLatFactory.CreateLonLatPolygon(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatPolygon;
var
  VLine: IGeometryLonLatSinglePolygon;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VPoint: TDoublePoint;
  VPoints: IDoublePoints;
  VLineBounds: TDoubleRect;
  VBuilder: IGeometryLonLatPolygonBuilder;
begin
  VStart := APoints;
  VLineLen := 0;
  VBuilder := MakePolygonBuilder;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        VPoints := TDoublePoints.Create(VStart, VLineLen);
        VLine := CreateLonLatPolygonInternal(VLineBounds, VPoints);
        VBuilder.Add(VLine);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        UpdateLonLatMBRByPoint(VLineBounds, VPoint);
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    VPoints := TDoublePoints.Create(VStart, VLineLen);
    VLine := CreateLonLatPolygonInternal(VLineBounds, VPoints);
    VBuilder.Add(VLine);
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryLonLatFactory.CreateLonLatPolygonByEnum(
  const AEnum: IEnumLonLatPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryLonLatPolygon;
var
  VPoint: TDoublePoint;
  VLine: IGeometryLonLatSinglePolygon;
  VTemp: IDoublePointsAggregator;
  VLineBounds: TDoubleRect;
  VBuilder: IGeometryLonLatPolygonBuilder;
begin
  VBuilder := MakePolygonBuilder;
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.MakeStaticCopy);
        VBuilder.Add(VLine);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        UpdateLonLatMBRByPoint(VLineBounds, VPoint);
      end;
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.MakeStaticCopy);
    VBuilder.Add(VLine);
    VTemp.Clear;
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryLonLatFactory.CreateLonLatPolygonByLonLatPathAndFilter(
  const ASource: IGeometryLonLatLine;
  const AFilter: ILonLatPointFilter
): IGeometryLonLatPolygon;
var
  VLine: IGeometryLonLatSinglePolygon;
  VEnum: IEnumLonLatPoint;
  VTemp: IDoublePointsAggregator;
  VPoint: TDoublePoint;
  VLineBounds: TDoubleRect;
  VBuilder: IGeometryLonLatPolygonBuilder;
  VLineSingle: IGeometryLonLatSingleLine;
  VLineMulti: IGeometryLonLatMultiLine;
begin
  VBuilder := MakePolygonBuilder;

  VTemp := TDoublePointsAggregator.Create;
  if Supports(ASource, IGeometryLonLatSingleLine, VLineSingle) then begin
    VEnum := VLineSingle.GetEnum;
  end else if Supports(ASource, IGeometryLonLatMultiLine, VLineMulti) then begin
    VEnum := TEnumLonLatPointByPath.Create(VLineMulti);
  end;
  VEnum := AFilter.CreateFilteredEnum(VEnum);
  while VEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.MakeStaticCopy);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        UpdateLonLatMBRByPoint(VLineBounds, VPoint);
      end;
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.MakeStaticCopy);
    VBuilder.Add(VLine);
    VTemp.Clear;
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryLonLatFactory.CreateLonLatPolygonByRect(
  const ARect: TDoubleRect
): IGeometryLonLatPolygon;
var
  VPointsArray: array [0..4] of TDoublePoint;
  VPoints: IDoublePoints;
begin
  VPointsArray[0] := ARect.TopLeft;
  VPointsArray[1].X := ARect.Right;
  VPointsArray[1].Y := ARect.Top;
  VPointsArray[2] := ARect.BottomRight;
  VPointsArray[3].X := ARect.Left;
  VPointsArray[3].Y := ARect.Bottom;
  VPoints := TDoublePoints.Create(@VPointsArray[0], 4);
  Result := CreateLonLatPolygonInternal(ARect, VPoints);
end;

function TGeometryLonLatFactory.MakeLineBuilder: IGeometryLonLatLineBuilder;
begin
  Result := TGeometryLonLatLineBuilder.Create(FHashFunction);
end;

function TGeometryLonLatFactory.MakePolygonBuilder: IGeometryLonLatPolygonBuilder;
begin
  Result := TGeometryLonLatPolygonBuilder.Create(FHashFunction);
end;

end.

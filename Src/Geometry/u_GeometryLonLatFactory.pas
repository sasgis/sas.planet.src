unit u_GeometryLonLatFactory;

interface

uses
  t_GeoTypes,
  i_HashFunction,
  i_ProjectionInfo,
  i_EnumDoublePoint,
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
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatSingleLine;
    function CreateLonLatPolygonInternal(
      const ARect: TDoubleRect;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatSinglePolygon;
  private
    function CreateLonLatPoint(
      const APoint: TDoublePoint
    ): IGeometryLonLatPoint;
    function CreateLonLatSingleLine(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatSingleLine;
    function CreateLonLatSinglePolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatSinglePolygon;

    function MakeMultiLineBuilder(): IGeometryLonLatMultiLineBuilder;
    function MakeMultiPolygonBuilder(): IGeometryLonLatMultiPolygonBuilder;

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
  SysUtils,
  t_Hash,
  i_LonLatRect,
  i_Datum,
  i_InterfaceListSimple,
  u_GeoFunc,
  u_InterfaceListSimple,
  u_DoublePointsAggregator,
  u_GeometryLonLat,
  u_LonLatRect,
  u_LonLatRectByPoint,
  u_EnumDoublePointByLineSet,
  u_GeometryLonLatMulti;

type
  TGeometryLonLatMultiLineBuilder = class(TBaseInterfacedObject, IGeometryLonLatMultiLineBuilder)
  private
    FHashFunction: IHashFunction;
    FHash: THashValue;
    FBounds: TDoubleRect;
    FLine: IGeometryLonLatSingleLine;
    FList: IInterfaceListSimple;
  private
    procedure Add(const AElement: IGeometryLonLatSingleLine);

    function MakeStaticAndClear: IGeometryLonLatLine;
    function MakeStaticCopy: IGeometryLonLatLine;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

{ TGeometryLonLatMultiLineBuilder }

constructor TGeometryLonLatMultiLineBuilder.Create(
  const AHashFunction: IHashFunction
);
begin
  inherited Create;
  FHashFunction := AHashFunction;
end;

procedure TGeometryLonLatMultiLineBuilder.Add(
  const AElement: IGeometryLonLatSingleLine
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

function TGeometryLonLatMultiLineBuilder.MakeStaticAndClear: IGeometryLonLatLine;
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

function TGeometryLonLatMultiLineBuilder.MakeStaticCopy: IGeometryLonLatLine;
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
  TGeometryLonLatMultiPolygonBuilder = class(TBaseInterfacedObject, IGeometryLonLatMultiPolygonBuilder)
  private
    FHashFunction: IHashFunction;
    FHash: THashValue;
    FBounds: TDoubleRect;
    FLine: IGeometryLonLatSinglePolygon;
    FList: IInterfaceListSimple;
  private
    procedure Add(const AElement: IGeometryLonLatSinglePolygon);

    function MakeStaticAndClear: IGeometryLonLatPolygon;
    function MakeStaticCopy: IGeometryLonLatPolygon;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

{ TGeometryLonLatMultiPolygonBuilder }

constructor TGeometryLonLatMultiPolygonBuilder.Create(
  const AHashFunction: IHashFunction
);
begin
  inherited Create;
  FHashFunction := AHashFunction;
end;

procedure TGeometryLonLatMultiPolygonBuilder.Add(
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

function TGeometryLonLatMultiPolygonBuilder.MakeStaticAndClear: IGeometryLonLatPolygon;
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

function TGeometryLonLatMultiPolygonBuilder.MakeStaticCopy: IGeometryLonLatPolygon;
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
    if VBounds.Left > VPoint.X then begin
      VBounds.Left := VPoint.X;
    end;
    if VBounds.Top < VPoint.Y then begin
      VBounds.Top := VPoint.Y;
    end;
    if VBounds.Right < VPoint.X then begin
      VBounds.Right := VPoint.X;
    end;
    if VBounds.Bottom > VPoint.Y then begin
      VBounds.Bottom := VPoint.Y;
    end;
  end;
  Result := CreateLonLatPolygonInternal(VBounds, VAggreagator.Points, VAggreagator.Count);
end;

function TGeometryLonLatFactory.CreateLonLatSingleLine(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatSingleLine;
var
  i: Integer;
  VPoint: TDoublePoint;
  VBounds: TDoubleRect;
begin
  Result := nil;
  Assert(ACount > 0);
  if ACount > 0 then begin
    VPoint := APoints[0];
    Assert(not PointIsEmpty(VPoint));
    if PointIsEmpty(VPoint) then begin
      Exit;
    end;
    VBounds.TopLeft := VPoint;
    VBounds.BottomRight := VPoint;
    for i := 1 to ACount - 1 do begin
      VPoint := APoints[i];
      Assert(not PointIsEmpty(VPoint));
      if PointIsEmpty(VPoint) then begin
        Exit;
      end;
      if VBounds.Left > VPoint.X then begin
        VBounds.Left := VPoint.X;
      end;
      if VBounds.Top < VPoint.Y then begin
        VBounds.Top := VPoint.Y;
      end;
      if VBounds.Right < VPoint.X then begin
        VBounds.Right := VPoint.X;
      end;
      if VBounds.Bottom > VPoint.Y then begin
        VBounds.Bottom := VPoint.Y;
      end;
    end;
    Result := CreateLonLatLineInternal(VBounds, APoints, ACount);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatSinglePolygon(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatSinglePolygon;
var
  i: Integer;
  VPoint: TDoublePoint;
  VBounds: TDoubleRect;
begin
  Result := nil;
  Assert(ACount > 0);
  if ACount > 0 then begin
    VPoint := APoints[0];
    Assert(not PointIsEmpty(VPoint));
    if PointIsEmpty(VPoint) then begin
      Exit;
    end;
    VBounds.TopLeft := VPoint;
    VBounds.BottomRight := VPoint;
    for i := 1 to ACount - 1 do begin
      VPoint := APoints[i];
      Assert(not PointIsEmpty(VPoint));
      if PointIsEmpty(VPoint) then begin
        Exit;
      end;
      if VBounds.Left > VPoint.X then begin
        VBounds.Left := VPoint.X;
      end;
      if VBounds.Top < VPoint.Y then begin
        VBounds.Top := VPoint.Y;
      end;
      if VBounds.Right < VPoint.X then begin
        VBounds.Right := VPoint.X;
      end;
      if VBounds.Bottom > VPoint.Y then begin
        VBounds.Bottom := VPoint.Y;
      end;
    end;
    Result := CreateLonLatPolygonInternal(VBounds, APoints, ACount);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatLineInternal(
  const ARect: TDoubleRect;
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatSingleLine;
var
  VHash: THashValue;
  VRect: ILonLatRect;
begin
  if ACount > 1 then begin
    VRect := TLonLatRect.Create(ARect);
  end else begin
    VRect := TLonLatRectByPoint.Create(ARect.TopLeft);
  end;
  VHash := FHashFunction.CalcHashByBuffer(APoints, ACount * SizeOf(TDoublePoint));
  Result := TGeometryLonLatSingleLine.Create(VRect, VHash, APoints, ACount);
end;

function TGeometryLonLatFactory.CreateLonLatPolygonInternal(
  const ARect: TDoubleRect;
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatSinglePolygon;
var
  VHash: THashValue;
  VRect: ILonLatRect;
begin
  if (ACount > 1) and DoublePointsEqual(APoints[0], APoints[ACount - 1]) then begin
    Dec(ACount);
  end;
  if ACount > 1 then begin
    VRect := TLonLatRect.Create(ARect);
  end else begin
    VRect := TLonLatRectByPoint.Create(ARect.TopLeft);
  end;
  VHash := FHashFunction.CalcHashByBuffer(APoints, ACount * SizeOf(TDoublePoint));
  Result := TGeometryLonLatSinglePolygon.Create(VRect, VHash, APoints, ACount);
end;

function TGeometryLonLatFactory.CreateLonLatLine(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatLine;
var
  VLine: IGeometryLonLatSingleLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VPoint: TDoublePoint;
  VLineBounds: TDoubleRect;
  VBuilder: IGeometryLonLatMultiLineBuilder;
begin
  VBuilder := MakeMultiLineBuilder;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        VLine := CreateLonLatLineInternal(VLineBounds, VStart, VLineLen);
        VBuilder.Add(VLine);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        if VLineBounds.Left > VPoint.X then begin
          VLineBounds.Left := VPoint.X;
        end;
        if VLineBounds.Top < VPoint.Y then begin
          VLineBounds.Top := VPoint.Y;
        end;
        if VLineBounds.Right < VPoint.X then begin
          VLineBounds.Right := VPoint.X;
        end;
        if VLineBounds.Bottom > VPoint.Y then begin
          VLineBounds.Bottom := VPoint.Y;
        end;
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    VLine := CreateLonLatLineInternal(VLineBounds, VStart, VLineLen);
    VBuilder.Add(VLine);
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryLonLatFactory.CreateLonLatLineByEnum(
  const AEnum: IEnumLonLatPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryLonLatLine;
var
  VPoint: TDoublePoint;
  VLine: IGeometryLonLatSingleLine;
  VTemp: IDoublePointsAggregator;
  VLineBounds: TDoubleRect;
  VBuilder: IGeometryLonLatMultiLineBuilder;
begin
  VBuilder := MakeMultiLineBuilder;
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        VLine := CreateLonLatLineInternal(VLineBounds, VTemp.Points, VTemp.Count);
        VBuilder.Add(VLine);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        if VLineBounds.Left > VPoint.X then begin
          VLineBounds.Left := VPoint.X;
        end;
        if VLineBounds.Top < VPoint.Y then begin
          VLineBounds.Top := VPoint.Y;
        end;
        if VLineBounds.Right < VPoint.X then begin
          VLineBounds.Right := VPoint.X;
        end;
        if VLineBounds.Bottom > VPoint.Y then begin
          VLineBounds.Bottom := VPoint.Y;
        end;
      end;
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    VLine := CreateLonLatLineInternal(VLineBounds, VTemp.Points, VTemp.Count);
    VBuilder.Add(VLine);
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
  VLineBounds: TDoubleRect;
  VBuilder: IGeometryLonLatMultiPolygonBuilder;
begin
  VStart := APoints;
  VLineLen := 0;
  VBuilder := MakeMultiPolygonBuilder;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        VLine := CreateLonLatPolygonInternal(VLineBounds, VStart, VLineLen);
        VBuilder.Add(VLine);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        if VLineBounds.Left > VPoint.X then begin
          VLineBounds.Left := VPoint.X;
        end;
        if VLineBounds.Top < VPoint.Y then begin
          VLineBounds.Top := VPoint.Y;
        end;
        if VLineBounds.Right < VPoint.X then begin
          VLineBounds.Right := VPoint.X;
        end;
        if VLineBounds.Bottom > VPoint.Y then begin
          VLineBounds.Bottom := VPoint.Y;
        end;
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    VLine := CreateLonLatPolygonInternal(VLineBounds, VStart, VLineLen);
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
  VBuilder: IGeometryLonLatMultiPolygonBuilder;
begin
  VBuilder := MakeMultiPolygonBuilder;
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.Points, VTemp.Count);
        VBuilder.Add(VLine);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        if VLineBounds.Left > VPoint.X then begin
          VLineBounds.Left := VPoint.X;
        end;
        if VLineBounds.Top < VPoint.Y then begin
          VLineBounds.Top := VPoint.Y;
        end;
        if VLineBounds.Right < VPoint.X then begin
          VLineBounds.Right := VPoint.X;
        end;
        if VLineBounds.Bottom > VPoint.Y then begin
          VLineBounds.Bottom := VPoint.Y;
        end;
      end;
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.Points, VTemp.Count);
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
  VBuilder: IGeometryLonLatMultiPolygonBuilder;
begin
  VBuilder := MakeMultiPolygonBuilder;

  VTemp := TDoublePointsAggregator.Create;
  VEnum := AFilter.CreateFilteredEnum(ASource.GetEnum);
  while VEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.Points, VTemp.Count);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        if VLineBounds.Left > VPoint.X then begin
          VLineBounds.Left := VPoint.X;
        end;
        if VLineBounds.Top < VPoint.Y then begin
          VLineBounds.Top := VPoint.Y;
        end;
        if VLineBounds.Right < VPoint.X then begin
          VLineBounds.Right := VPoint.X;
        end;
        if VLineBounds.Bottom > VPoint.Y then begin
          VLineBounds.Bottom := VPoint.Y;
        end;
      end;
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    VLine := CreateLonLatPolygonInternal(VLineBounds, VTemp.Points, VTemp.Count);
    VBuilder.Add(VLine);
    VTemp.Clear;
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryLonLatFactory.CreateLonLatPolygonByRect(
  const ARect: TDoubleRect
): IGeometryLonLatPolygon;
var
  VPoints: array [0..4] of TDoublePoint;
begin
  VPoints[0] := ARect.TopLeft;
  VPoints[1].X := ARect.Right;
  VPoints[1].Y := ARect.Top;
  VPoints[2] := ARect.BottomRight;
  VPoints[3].X := ARect.Left;
  VPoints[3].Y := ARect.Bottom;
  Result := CreateLonLatPolygonInternal(ARect, @VPoints[0], 4);
end;

function TGeometryLonLatFactory.MakeMultiLineBuilder: IGeometryLonLatMultiLineBuilder;
begin
  Result := TGeometryLonLatMultiLineBuilder.Create(FHashFunction);
end;

function TGeometryLonLatFactory.MakeMultiPolygonBuilder: IGeometryLonLatMultiPolygonBuilder;
begin
  Result := TGeometryLonLatMultiPolygonBuilder.Create(FHashFunction);
end;

end.

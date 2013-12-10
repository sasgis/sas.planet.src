unit u_VectorItemsFactorySimple;

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
    FEmptyLonLatPath: IGeometryLonLatMultiLine;
    FEmptyLonLatPolygon: IGeometryLonLatMultiPolygon;
  private
    function CreateLonLatPoint(
      const APoint: TDoublePoint
    ): IGeometryLonLatPoint;
    function CreateLonLatPath(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatMultiLine;
    function CreateLonLatPolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatMultiPolygon;
    function CreateLonLatPathByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatMultiLine;
    function CreateLonLatPolygonByEnum(
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryLonLatMultiPolygon;

    function CreateLonLatPolygonLineByRect(
      const ARect: TDoubleRect
    ): IGeometryLonLatPolygon;
    function CreateLonLatPolygonByRect(
      const ARect: TDoubleRect
    ): IGeometryLonLatMultiPolygon;

    function CreateLonLatPolygonCircleByPoint(
      const AProjection: IProjectionInfo;
      const APos: TDoublePoint;
      const ARadius: double
    ): IGeometryLonLatMultiPolygon;

    function CreateLonLatPolygonByLonLatPathAndFilter(
      const ASource: IGeometryLonLatMultiLine;
      const AFilter: ILonLatPointFilter
    ): IGeometryLonLatMultiPolygon;
  public
    constructor Create(const AHashFunction: IHashFunction);
  end;

implementation

uses
  t_Hash,
  i_LonLatRect,
  i_Datum,
  i_InterfaceListSimple,
  u_GeoFun,
  u_InterfaceListSimple,
  u_DoublePointsAggregator,
  u_GeometryLonLat,
  u_LonLatRect,
  u_LonLatRectByPoint,
  u_VectorItemEmpty,
  u_GeometryLonLatMulti;

{ TVectorGeometryLonLatFactory }

constructor TGeometryLonLatFactory.Create(
  const AHashFunction: IHashFunction);
var
  VEmpty: TLineSetEmpty;
begin
  Assert(Assigned(AHashFunction));
  inherited Create;
  FHashFunction := AHashFunction;
  VEmpty := TLineSetEmpty.Create;
  FEmptyLonLatPath := VEmpty;
  FEmptyLonLatPolygon := VEmpty;
end;

function TGeometryLonLatFactory.CreateLonLatPolygonCircleByPoint(
  const AProjection: IProjectionInfo;
  const APos: TDoublePoint;
  const ARadius: double
): IGeometryLonLatMultiPolygon;
var
  VAggreagator: IDoublePointsAggregator;
  j: Integer;
  VDatum : IDatum;
  VAngle: Double;
  VPoint: TDoublePoint;
begin
  VAggreagator := TDoublePointsAggregator.Create;
  VDatum :=  AProjection.GeoConverter.Datum;
  for j := 0 to 64 do begin
    VAngle := j * 360 / 64;
    VPoint := VDatum.CalcFinishPosition(APos, VAngle, ARadius);
    VAggreagator.Add(VPoint);
  end;
  Result := CreateLonLatPolygon(VAggreagator.Points, VAggreagator.Count);
end;

function TGeometryLonLatFactory.CreateLonLatPath(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatMultiLine;
var
  VLine: IGeometryLonLatLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceListSimple;
  VPoint: TDoublePoint;
  VLineBounds: TDoubleRect;
  VBounds: TDoubleRect;
  VRect: ILonLatRect;
  VHash: THashValue;
  VLinesetHash: THashValue;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  VLinesetHash := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
            VLinesetHash := VLine.Hash;
          end else begin
            FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        if VLineLen > 1 then begin
          VRect := TLonLatRect.Create(VLineBounds);
        end else begin
          VRect := TLonLatRectByPoint.Create(VLineBounds.TopLeft);
        end;
        VHash := FHashFunction.CalcHashByBuffer(VStart, VLineLen * SizeOf(TDoublePoint));
        VLine := TGeometryLonLatLine.Create(VRect, VHash, VStart, VLineLen);
        if VLineCount > 0 then begin
          VBounds := UnionLonLatRects(VBounds, VLineBounds);
        end else begin
          VBounds := VLineBounds;
        end;
        Inc(VLineCount);
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
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
        VLinesetHash := VLine.Hash;
      end else begin
        FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    if VLineLen > 1 then begin
      VRect := TLonLatRect.Create(VLineBounds);
    end else begin
      VRect := TLonLatRectByPoint.Create(VLineBounds.TopLeft);
    end;
    VHash := FHashFunction.CalcHashByBuffer(VStart, VLineLen * SizeOf(TDoublePoint));
    VLine := TGeometryLonLatLine.Create(VRect, VHash, VStart, VLineLen);
    if VLineCount > 0 then begin
      VBounds := UnionLonLatRects(VBounds, VLineBounds);
    end else begin
      VBounds := VLineBounds;
    end;
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLonLatPath;
  end else if VLineCount = 1 then begin
    Result := TLonLatPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
    VRect := TLonLatRect.Create(VBounds);
    Result := TGeometryLonLatMultiLine.Create(VRect, VLinesetHash, VList.MakeStaticAndClear);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatPathByEnum(
  const AEnum: IEnumLonLatPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryLonLatMultiLine;
var
  VPoint: TDoublePoint;
  VLine: IGeometryLonLatLine;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  VLineBounds: TDoubleRect;
  VBounds: TDoubleRect;
  VRect: ILonLatRect;
  VHash: THashValue;
  VLinesetHash: THashValue;
begin
  VLinesetHash := 0;
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
            VLinesetHash := VLine.Hash;
          end else begin
            FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        if VTemp.Count > 1 then begin
          VRect := TLonLatRect.Create(VLineBounds);
        end else begin
          VRect := TLonLatRectByPoint.Create(VLineBounds.TopLeft);
        end;
        VHash := FHashFunction.CalcHashByBuffer(VTemp.Points, VTemp.Count * SizeOf(TDoublePoint));
        VLine := TGeometryLonLatLine.Create(VRect, VHash, VTemp.Points, VTemp.Count);
        if VLineCount > 0 then begin
          VBounds := UnionLonLatRects(VBounds, VLineBounds);
        end else begin
          VBounds := VLineBounds;
        end;
        Inc(VLineCount);
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
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
        VLinesetHash := VLine.Hash;
      end else begin
        FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    if VTemp.Count > 1 then begin
      VRect := TLonLatRect.Create(VLineBounds);
    end else begin
      VRect := TLonLatRectByPoint.Create(VLineBounds.TopLeft);
    end;
    VHash := FHashFunction.CalcHashByBuffer(VTemp.Points, VTemp.Count * SizeOf(TDoublePoint));
    VLine := TGeometryLonLatLine.Create(VRect, VHash, VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionLonLatRects(VBounds, VLineBounds);
    end else begin
      VBounds := VLineBounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLonLatPath;
  end else if VLineCount = 1 then begin
    Result := TLonLatPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
    VRect := TLonLatRect.Create(VBounds);
    Result := TGeometryLonLatMultiLine.Create(VRect, VLinesetHash, VList.MakeStaticAndClear);
  end;
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
): IGeometryLonLatMultiPolygon;
var
  VLine: IGeometryLonLatPolygon;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceListSimple;
  VPoint: TDoublePoint;
  VLineBounds: TDoubleRect;
  VBounds: TDoubleRect;
  VRect: ILonLatRect;
  VHash: THashValue;
  VLinesetHash: THashValue;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  VLinesetHash := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
            VLinesetHash := VLine.Hash;
          end else begin
            FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        if VLineLen > 1 then begin
          VRect := TLonLatRect.Create(VLineBounds);
        end else begin
          VRect := TLonLatRectByPoint.Create(VLineBounds.TopLeft);
        end;
        VHash := FHashFunction.CalcHashByBuffer(VStart, VLineLen * SizeOf(TDoublePoint));
        VLine := TGeometryLonLatPolygon.Create(VRect, VHash, VStart, VLineLen);
        if VLineCount > 0 then begin
          VBounds := UnionLonLatRects(VBounds, VLineBounds);
        end else begin
          VBounds := VLineBounds;
        end;
        Inc(VLineCount);
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
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
        VLinesetHash := VLine.Hash;
      end else begin
        FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    if VLineLen > 1 then begin
      VRect := TLonLatRect.Create(VLineBounds);
    end else begin
      VRect := TLonLatRectByPoint.Create(VLineBounds.TopLeft);
    end;
    VHash := FHashFunction.CalcHashByBuffer(VStart, VLineLen * SizeOf(TDoublePoint));
    VLine := TGeometryLonLatPolygon.Create(VRect, VHash, VStart, VLineLen);
    if VLineCount > 0 then begin
      VBounds := UnionLonLatRects(VBounds, VLineBounds);
    end else begin
      VBounds := VLineBounds;
    end;
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLonLatPolygon;
  end else if VLineCount = 1 then begin
    Result := TLonLatPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
    VRect := TLonLatRect.Create(VBounds);
    Result := TGeometryLonLatMultiPolygon.Create(VRect, VLinesetHash, VList.MakeStaticAndClear);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatPolygonByEnum(
  const AEnum: IEnumLonLatPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryLonLatMultiPolygon;
var
  VPoint: TDoublePoint;
  VLine: IGeometryLonLatPolygon;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  VLineBounds: TDoubleRect;
  VBounds: TDoubleRect;
  VRect: ILonLatRect;
  VHash: THashValue;
  VLinesetHash: THashValue;
begin
  VLinesetHash := 0;
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  while AEnum.Next(VPoint) do begin
    if PointIsEmpty(VPoint) then begin
      if VTemp.Count > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
            VLinesetHash := VLine.Hash;
          end else begin
            FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        if VTemp.Count > 1 then begin
          VRect := TLonLatRect.Create(VLineBounds);
        end else begin
          VRect := TLonLatRectByPoint.Create(VLineBounds.TopLeft);
        end;
        VHash := FHashFunction.CalcHashByBuffer(VTemp.Points, VTemp.Count * SizeOf(TDoublePoint));
        VLine := TGeometryLonLatPolygon.Create(VRect, VHash, VTemp.Points, VTemp.Count);
        if VLineCount > 0 then begin
          VBounds := UnionLonLatRects(VBounds, VLineBounds);
        end else begin
          VBounds := VLineBounds;
        end;
        Inc(VLineCount);
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
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
        VLinesetHash := VLine.Hash;
      end else begin
        FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    if VTemp.Count > 1 then begin
      VRect := TLonLatRect.Create(VLineBounds);
    end else begin
      VRect := TLonLatRectByPoint.Create(VLineBounds.TopLeft);
    end;
    VHash := FHashFunction.CalcHashByBuffer(VTemp.Points, VTemp.Count * SizeOf(TDoublePoint));
    VLine := TGeometryLonLatPolygon.Create(VRect, VHash, VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionLonLatRects(VBounds, VLineBounds);
    end else begin
      VBounds := VLineBounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLonLatPolygon;
  end else if VLineCount = 1 then begin
    Result := TLonLatPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
    VRect := TLonLatRect.Create(VBounds);
    Result := TGeometryLonLatMultiPolygon.Create(VRect, VLinesetHash, VList.MakeStaticAndClear);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatPolygonByLonLatPathAndFilter(
  const ASource: IGeometryLonLatMultiLine;
  const AFilter: ILonLatPointFilter
): IGeometryLonLatMultiPolygon;
var
  i: Integer;
  VLine: IGeometryLonLatPolygon;
  VEnum: IEnumLonLatPoint;
  VTemp: IDoublePointsAggregator;
  VPoint: TDoublePoint;
  VLineCount: Integer;
  VList: IInterfaceListSimple;
  VLineBounds: TDoubleRect;
  VBounds: TDoubleRect;
  VRect: ILonLatRect;
  VHash: THashValue;
  VLinesetHash: THashValue;
begin
  VLinesetHash := 0;
  VLineCount := 0;
  VTemp := TDoublePointsAggregator.Create;
  for i := 0 to ASource.Count - 1 do begin
    VEnum := AFilter.CreateFilteredEnum(ASource.Item[i].GetEnum);
    while VEnum.Next(VPoint) do begin
      if PointIsEmpty(VPoint) then begin
        if VTemp.Count > 0 then begin
          if VLineCount > 0 then begin
            if VLineCount = 1 then begin
              VList := TInterfaceListSimple.Create;
              VLinesetHash := VLine.Hash;
            end else begin
              FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
            end;
            VList.Add(VLine);
            VLine := nil;
          end;
          if VTemp.Count > 1 then begin
            VRect := TLonLatRect.Create(VLineBounds);
          end else begin
            VRect := TLonLatRectByPoint.Create(VLineBounds.TopLeft);
          end;
          VHash := FHashFunction.CalcHashByBuffer(VTemp.Points, VTemp.Count * SizeOf(TDoublePoint));
          VLine := TGeometryLonLatPolygon.Create(VRect, VHash, VTemp.Points, VTemp.Count);
          if VLineCount > 0 then begin
            VBounds := UnionLonLatRects(VBounds, VLineBounds);
          end else begin
            VBounds := VLineBounds;
          end;
          Inc(VLineCount);
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
      if VLineCount > 0 then begin
        if VLineCount = 1 then begin
          VList := TInterfaceListSimple.Create;
          VLinesetHash := VLine.Hash;
        end else begin
          FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
        end;
        VList.Add(VLine);
        VLine := nil;
      end;
      if VTemp.Count > 1 then begin
        VRect := TLonLatRect.Create(VLineBounds);
      end else begin
        VRect := TLonLatRectByPoint.Create(VLineBounds.TopLeft);
      end;
      VHash := FHashFunction.CalcHashByBuffer(VTemp.Points, VTemp.Count * SizeOf(TDoublePoint));
      VLine := TGeometryLonLatPolygon.Create(VRect, VHash, VTemp.Points, VTemp.Count);
      if VLineCount > 0 then begin
        VBounds := UnionLonLatRects(VBounds, VLineBounds);
      end else begin
        VBounds := VLineBounds;
      end;
      Inc(VLineCount);
      VTemp.Clear;
    end;
  end;
  if VLineCount = 0 then begin
    Result := FEmptyLonLatPolygon;
  end else if VLineCount = 1 then begin
    Result := TLonLatPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    FHashFunction.UpdateHashByHash(VLinesetHash, VLine.Hash);
    VRect := TLonLatRect.Create(VBounds);
    Result := TGeometryLonLatMultiPolygon.Create(VRect, VLinesetHash, VList.MakeStaticAndClear);
  end;
end;

function TGeometryLonLatFactory.CreateLonLatPolygonByRect(
  const ARect: TDoubleRect
): IGeometryLonLatMultiPolygon;
begin
  Result := TLonLatPolygonOneLine.Create(CreateLonLatPolygonLineByRect(ARect));
end;

function TGeometryLonLatFactory.CreateLonLatPolygonLineByRect(
  const ARect: TDoubleRect
): IGeometryLonLatPolygon;
var
  VPoints: array [0..4] of TDoublePoint;
  VRect: ILonLatRect;
  VHash: THashValue;
begin
  VPoints[0] := ARect.TopLeft;
  VPoints[1].X := ARect.Right;
  VPoints[1].Y := ARect.Top;
  VPoints[2] := ARect.BottomRight;
  VPoints[3].X := ARect.Left;
  VPoints[3].Y := ARect.Bottom;
  VRect := TLonLatRect.Create(ARect);
  VHash := FHashFunction.CalcHashByBuffer(@VPoints[0], 4 * SizeOf(TDoublePoint));
  Result := TGeometryLonLatPolygon.Create(VRect, VHash, @VPoints[0], 4);
end;

end.

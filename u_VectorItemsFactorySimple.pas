unit u_VectorItemsFactorySimple;

interface

uses
  t_GeoTypes,
  i_HashFunction,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_EnumDoublePoint,
  i_DoublePointFilter,
  i_DoublePointsAggregator,
  i_GeometryLonLat,
  i_VectorItemProjected,
  i_VectorItemLocal,
  i_VectorItemsFactory,
  u_BaseInterfacedObject;

type
  TVectorGeometryLonLatFactory = class(TBaseInterfacedObject, IVectorGeometryLonLatFactory)
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

  TVectorGeometryProjectedFactory = class(TBaseInterfacedObject, IVectorGeometryProjectedFactory)
  private
    function CreateProjectedPath(
      const AProjection: IProjectionInfo;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IProjectedPath;
    function CreateProjectedPolygon(
      const AProjection: IProjectionInfo;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IProjectedPolygon;

    function CreateProjectedPolygonLineByRect(
      const AProjection: IProjectionInfo;
      const ARect: TDoubleRect
    ): IProjectedPolygonLine;
    function CreateProjectedPolygonByRect(
      const AProjection: IProjectionInfo;
      const ARect: TDoubleRect
    ): IProjectedPolygon;

    function CreateProjectedPathByEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathByLonLatPath(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiLine;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatPolygon(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiPolygon;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathWithClipByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonWithClipByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathWithClipByLonLatPath(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiLine;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonWithClipByLonLatPolygon(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiPolygon;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function CreateProjectedPathByLonLatPathUseConverter(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiLine;
      const AConverter: ILonLatPointConverter;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function CreateProjectedPolygonByLonLatPolygonUseConverter(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiPolygon;
      const AConverter: ILonLatPointConverter;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;
  end;

  TVectorGeometryLocalFactory = class(TBaseInterfacedObject, IVectorGeometryLocalFactory)
  private
    function CreateLocalPath(
      const ALocalConverter: ILocalCoordConverter;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): ILocalPath;
    function CreateLocalPolygon(
      const ALocalConverter: ILocalCoordConverter;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): ILocalPolygon;
    function CreateLocalPathByEnum(
      const ALocalConverter: ILocalCoordConverter;
      const AEnum: IEnumLocalPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): ILocalPath;
    function CreateLocalPolygonByEnum(
      const ALocalConverter: ILocalCoordConverter;
      const AEnum: IEnumLocalPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): ILocalPolygon;
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
  u_ProjectedSingleLine,
  u_LocalSingleLine,
  u_EnumDoublePointLonLatToMapPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual,
  u_LonLatRect,
  u_LonLatRectByPoint,
  u_VectorItemEmpty,
  u_GeometryLonLatMulti,
  u_VectorItemProjected,
  u_VectorItemLocal;

{ TVectorGeometryLonLatFactory }

constructor TVectorGeometryLonLatFactory.Create(
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

function TVectorGeometryLonLatFactory.CreateLonLatPolygonCircleByPoint(
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

function TVectorGeometryLonLatFactory.CreateLonLatPath(
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

function TVectorGeometryLonLatFactory.CreateLonLatPathByEnum(
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

function TVectorGeometryLonLatFactory.CreateLonLatPoint(
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

function TVectorGeometryLonLatFactory.CreateLonLatPolygon(
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

function TVectorGeometryLonLatFactory.CreateLonLatPolygonByEnum(
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

function TVectorGeometryLonLatFactory.CreateLonLatPolygonByLonLatPathAndFilter(
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

function TVectorGeometryLonLatFactory.CreateLonLatPolygonByRect(
  const ARect: TDoubleRect
): IGeometryLonLatMultiPolygon;
begin
  Result := TLonLatPolygonOneLine.Create(CreateLonLatPolygonLineByRect(ARect));
end;

function TVectorGeometryLonLatFactory.CreateLonLatPolygonLineByRect(
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

{ TVectorGeometryProjectedFactory }

function TVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygonUseConverter(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiPolygon;
  const AConverter: ILonLatPointConverter;
  const ATemp: IDoublePointsAggregator =
  nil
): IProjectedPolygon;
var
  VPoint: TDoublePoint;
  VLine: IProjectedPolygonLine;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  i: Integer;
  VSourceLine: IGeometryLonLatPolygon;
  VEnumLonLat: IEnumLonLatPoint;
  VEnumProjected: IEnumProjectedPoint;
  VBounds: TDoubleRect;
begin
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  for i := 0 to ASource.Count - 1 do begin
    VSourceLine := ASource.Item[i];
    VEnumLonLat := VSourceLine.GetEnum;
    VEnumProjected := AConverter.CreateFilteredEnum(VEnumLonLat);
    while VEnumLonLat.Next(VPoint) do begin
      if PointIsEmpty(VPoint) then begin
        Break;
      end;
      VTemp.Add(VPoint);
    end;
    if VTemp.Count > 0 then begin
      if VLineCount > 0 then begin
        if VLineCount = 1 then begin
          VList := TInterfaceListSimple.Create;
        end;
        VList.Add(VLine);
        VLine := nil;
      end;
      VLine := TProjectedPolygonLine.Create(AProjection, VTemp.Points, VTemp.Count);
      if VLineCount > 0 then begin
        VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
      end else begin
        VBounds := VLine.Bounds;
      end;
      Inc(VLineCount);
      VTemp.Clear;
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPolygonLine.Create(AProjection, VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPolygonEmpty.Create(AProjection);
  end else if VLineCount = 1 then begin
    Result := TProjectedPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPolygon.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPath(
  const AProjection: IProjectionInfo;
  const APoints: PDoublePointArray;
  ACount: Integer
): IProjectedPath;
var
  VLine: IProjectedPathLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceListSimple;
  VPoint: TDoublePoint;
  VBounds: TDoubleRect;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TProjectedPathLine.Create(AProjection, VStart, VLineLen);
        if VLineCount > 0 then begin
          VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
        end else begin
          VBounds := VLine.Bounds;
        end;
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPathLine.Create(AProjection, VStart, VLineLen);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPathEmpty.Create(AProjection);
  end else if VLineCount = 1 then begin
    Result := TProjectedPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPath.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPathByEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IProjectedPath;
var
  VPoint: TDoublePoint;
  VLine: IProjectedPathLine;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  VBounds: TDoubleRect;
begin
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
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TProjectedPathLine.Create(AProjection, VTemp.Points, VTemp.Count);
        if VLineCount > 0 then begin
          VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
        end else begin
          VBounds := VLine.Bounds;
        end;
        Inc(VLineCount);
        VTemp.Clear;
      end;
    end else begin
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPathLine.Create(AProjection, VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPathEmpty.Create(AProjection);
  end else if VLineCount = 1 then begin
    Result := TProjectedPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPath.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPathByLonLatEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumLonLatPoint;
  const ATemp: IDoublePointsAggregator
): IProjectedPath;
var
  VEnum: IEnumProjectedPoint;
begin
  VEnum :=
    TEnumDoublePointLonLatToMapPixel.Create(
      AProjection.Zoom,
      AProjection.GeoConverter,
      AEnum
    );
  VEnum := TEnumProjectedPointFilterEqual.Create(VEnum);
  Result :=
    CreateProjectedPathByEnum(
      AProjection,
      VEnum,
      ATemp
    );
end;

function TVectorGeometryProjectedFactory.CreateProjectedPathByLonLatPath(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiLine;
  const ATemp: IDoublePointsAggregator
): IProjectedPath;
begin
  Result :=
    CreateProjectedPathByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      ATemp
    );
end;

function TVectorGeometryProjectedFactory.CreateProjectedPathByLonLatPathUseConverter(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiLine;
  const AConverter: ILonLatPointConverter;
  const ATemp: IDoublePointsAggregator =
  nil
): IProjectedPath;
var
  VPoint: TDoublePoint;
  VLine: IProjectedPathLine;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  i: Integer;
  VSourceLine: IGeometryLonLatLine;
  VEnumLonLat: IEnumLonLatPoint;
  VEnumProjected: IEnumProjectedPoint;
  VBounds: TDoubleRect;
begin
  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  VTemp.Clear;
  VLineCount := 0;
  for i := 0 to ASource.Count - 1 do begin
    VSourceLine := ASource.Item[i];
    VEnumLonLat := VSourceLine.GetEnum;
    VEnumProjected := AConverter.CreateFilteredEnum(VEnumLonLat);
    while VEnumLonLat.Next(VPoint) do begin
      if PointIsEmpty(VPoint) then begin
        Break;
      end;
      VTemp.Add(VPoint);
    end;
    if VTemp.Count > 0 then begin
      if VLineCount > 0 then begin
        if VLineCount = 1 then begin
          VList := TInterfaceListSimple.Create;
        end;
        VList.Add(VLine);
        VLine := nil;
      end;
      VLine := TProjectedPathLine.Create(AProjection, VTemp.Points, VTemp.Count);
      if VLineCount > 0 then begin
        VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
      end else begin
        VBounds := VLine.Bounds;
      end;
      Inc(VLineCount);
      VTemp.Clear;
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPathLine.Create(AProjection, VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPathEmpty.Create(AProjection);
  end else if VLineCount = 1 then begin
    Result := TProjectedPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPath.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPathWithClipByLonLatEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumLonLatPoint;
  const AMapPixelsClipRect: TDoubleRect;
  const ATemp: IDoublePointsAggregator
): IProjectedPath;
var
  VEnum: IEnumProjectedPoint;
begin
  VEnum :=
    TEnumDoublePointLonLatToMapPixel.Create(
      AProjection.Zoom,
      AProjection.GeoConverter,
      AEnum
    );
  VEnum := TEnumProjectedPointFilterEqual.Create(VEnum);
  VEnum :=
    TEnumProjectedPointClipByRect.Create(
      False,
      AMapPixelsClipRect,
      VEnum
    );
  Result :=
    CreateProjectedPathByEnum(
      AProjection,
      VEnum,
      ATemp
    );
end;

function TVectorGeometryProjectedFactory.CreateProjectedPathWithClipByLonLatPath(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiLine;
  const AMapPixelsClipRect: TDoubleRect;
  const ATemp: IDoublePointsAggregator
): IProjectedPath;
begin
  Result :=
    CreateProjectedPathWithClipByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      AMapPixelsClipRect,
      ATemp
    );
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygon(
  const AProjection: IProjectionInfo;
  const APoints: PDoublePointArray;
  ACount: Integer
): IProjectedPolygon;
var
  VLine: IProjectedPolygonLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceListSimple;
  VPoint: TDoublePoint;
  VBounds: TDoubleRect;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TProjectedPolygonLine.Create(AProjection, VStart, VLineLen);
        if VLineCount > 0 then begin
          VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
        end else begin
          VBounds := VLine.Bounds;
        end;
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPolygonLine.Create(AProjection, VStart, VLineLen);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPolygonEmpty.Create(AProjection);
  end else if VLineCount = 1 then begin
    Result := TProjectedPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPolygon.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygonByEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IProjectedPolygon;
var
  VPoint: TDoublePoint;
  VLine: IProjectedPolygonLine;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  VBounds: TDoubleRect;
begin
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
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TProjectedPolygonLine.Create(AProjection, VTemp.Points, VTemp.Count);
        if VLineCount > 0 then begin
          VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
        end else begin
          VBounds := VLine.Bounds;
        end;
        Inc(VLineCount);
        VTemp.Clear;
      end;
    end else begin
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TProjectedPolygonLine.Create(AProjection, VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPolygonEmpty.Create(AProjection);
  end else if VLineCount = 1 then begin
    Result := TProjectedPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TProjectedPolygon.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumLonLatPoint;
  const ATemp: IDoublePointsAggregator
): IProjectedPolygon;
var
  VEnum: IEnumProjectedPoint;
begin
  VEnum :=
    TEnumDoublePointLonLatToMapPixel.Create(
      AProjection.Zoom,
      AProjection.GeoConverter,
      AEnum
    );
  VEnum :=
    TEnumProjectedPointFilterEqual.Create(VEnum);
  Result :=
    CreateProjectedPolygonByEnum(
      AProjection,
      VEnum,
      ATemp
    );
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiPolygon;
  const ATemp: IDoublePointsAggregator
): IProjectedPolygon;
begin
  Result :=
    CreateProjectedPolygonByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      ATemp
    );
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygonByRect(
  const AProjection: IProjectionInfo;
  const ARect: TDoubleRect
): IProjectedPolygon;
begin
  Result := TProjectedPolygonOneLine.Create(CreateProjectedPolygonLineByRect(AProjection, ARect));
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygonLineByRect(
  const AProjection: IProjectionInfo;
  const ARect: TDoubleRect
): IProjectedPolygonLine;
var
  VPoints: array [0..4] of TDoublePoint;
begin
  VPoints[0] := ARect.TopLeft;
  VPoints[1].X := ARect.Right;
  VPoints[1].Y := ARect.Top;
  VPoints[2] := ARect.BottomRight;
  VPoints[3].X := ARect.Left;
  VPoints[3].Y := ARect.Bottom;
  Result := TProjectedPolygonLine.Create(AProjection, @VPoints[0], 4);
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygonWithClipByLonLatEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumLonLatPoint;
  const AMapPixelsClipRect: TDoubleRect;
  const ATemp: IDoublePointsAggregator
): IProjectedPolygon;
var
  VEnum: IEnumProjectedPoint;
begin
  VEnum :=
    TEnumDoublePointLonLatToMapPixel.Create(
      AProjection.Zoom,
      AProjection.GeoConverter,
      AEnum
    );
  VEnum := TEnumProjectedPointFilterEqual.Create(VEnum);

  VEnum :=
    TEnumProjectedPointClipByRect.Create(
      True,
      AMapPixelsClipRect,
      VEnum
    );
  Result :=
    CreateProjectedPolygonByEnum(
      AProjection,
      VEnum,
      ATemp
    );
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygonWithClipByLonLatPolygon(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiPolygon;
  const AMapPixelsClipRect: TDoubleRect;
  const ATemp: IDoublePointsAggregator
): IProjectedPolygon;
begin
  Result :=
    CreateProjectedPolygonWithClipByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      AMapPixelsClipRect,
      ATemp
    );
end;

{ TVectorGeometryLocalFactory }

function TVectorGeometryLocalFactory.CreateLocalPath(
  const ALocalConverter: ILocalCoordConverter;
  const APoints: PDoublePointArray;
  ACount: Integer
): ILocalPath;
var
  VLine: ILocalPathLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceListSimple;
  VPoint: TDoublePoint;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TLocalPathLine.Create(ALocalConverter, VStart, VLineLen);
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TLocalPathLine.Create(ALocalConverter, VStart, VLineLen);
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := TLocalPathEmpty.Create(ALocalConverter);
  end else if VLineCount = 1 then begin
    Result := TLocalPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TLocalPath.Create(ALocalConverter, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryLocalFactory.CreateLocalPathByEnum(
  const ALocalConverter: ILocalCoordConverter;
  const AEnum: IEnumLocalPoint;
  const ATemp: IDoublePointsAggregator
): ILocalPath;
var
  VPoint: TDoublePoint;
  VLine: ILocalPathLine;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
begin
  Assert(ALocalConverter <> nil);
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
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TLocalPathLine.Create(ALocalConverter, VTemp.Points, VTemp.Count);
        Inc(VLineCount);
        VTemp.Clear;
      end;
    end else begin
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TLocalPathLine.Create(ALocalConverter, VTemp.Points, VTemp.Count);
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TLocalPathEmpty.Create(ALocalConverter);
  end else if VLineCount = 1 then begin
    Result := TLocalPathOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TLocalPath.Create(ALocalConverter, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryLocalFactory.CreateLocalPolygon(
  const ALocalConverter: ILocalCoordConverter;
  const APoints: PDoublePointArray;
  ACount: Integer
): ILocalPolygon;
var
  VLine: ILocalPolygonLine;
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VLineCount: Integer;
  VList: IInterfaceListSimple;
  VPoint: TDoublePoint;
begin
  VLineCount := 0;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        if VLineCount > 0 then begin
          if VLineCount = 1 then begin
            VList := TInterfaceListSimple.Create;
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TLocalPolygonLine.Create(ALocalConverter, VStart, VLineLen);
        Inc(VLineCount);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TLocalPolygonLine.Create(ALocalConverter, VStart, VLineLen);
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := TLocalPolygonEmpty.Create(ALocalConverter);
  end else if VLineCount = 1 then begin
    Result := TLocalPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TLocalPolygon.Create(ALocalConverter, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryLocalFactory.CreateLocalPolygonByEnum(
  const ALocalConverter: ILocalCoordConverter;
  const AEnum: IEnumLocalPoint;
  const ATemp: IDoublePointsAggregator
): ILocalPolygon;
var
  VPoint: TDoublePoint;
  VLine: ILocalPolygonLine;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
begin
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
          end;
          VList.Add(VLine);
          VLine := nil;
        end;
        VLine := TLocalPolygonLine.Create(ALocalConverter, VTemp.Points, VTemp.Count);
        Inc(VLineCount);
        VTemp.Clear;
      end;
    end else begin
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    if VLineCount > 0 then begin
      if VLineCount = 1 then begin
        VList := TInterfaceListSimple.Create;
      end;
      VList.Add(VLine);
      VLine := nil;
    end;
    VLine := TLocalPolygonLine.Create(ALocalConverter, VTemp.Points, VTemp.Count);
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TLocalPolygonEmpty.Create(ALocalConverter);
  end else if VLineCount = 1 then begin
    Result := TLocalPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TLocalPolygon.Create(ALocalConverter, VList.MakeStaticAndClear);
  end;
end;

end.

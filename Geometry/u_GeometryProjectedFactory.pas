unit u_GeometryProjectedFactory;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_EnumDoublePoint,
  i_DoublePointFilter,
  i_DoublePointsAggregator,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_GeometryProjectedFactory,
  u_BaseInterfacedObject;

type
  TGeometryProjectedFactory = class(TBaseInterfacedObject, IGeometryProjectedFactory)
  private
    function CreateProjectedPath(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPolygonLineByRect(
      const ARect: TDoubleRect
    ): IGeometryProjectedSinglePolygon;
    function CreateProjectedPolygonByRect(
      const ARect: TDoubleRect
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathByEnum(
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonByEnum(
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathByLonLatPath(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiLine;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonByLonLatPolygon(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiPolygon;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathWithClipByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonWithClipByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathWithClipByLonLatPath(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiLine;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonWithClipByLonLatPolygon(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiPolygon;
      const AMapPixelsClipRect: TDoubleRect;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathByLonLatPathUseConverter(
      const ASource: IGeometryLonLatMultiLine;
      const AConverter: ILonLatPointConverter;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonByLonLatPolygonUseConverter(
      const ASource: IGeometryLonLatMultiPolygon;
      const AConverter: ILonLatPointConverter;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiPolygon;
  end;

implementation

uses
  i_InterfaceListSimple,
  u_GeoFunc,
  u_InterfaceListSimple,
  u_DoublePointsAggregator,
  u_GeometryProjected,
  u_EnumDoublePointLonLatToMapPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual,
  u_GeometryProjectedMulti;

{ TGeometryProjectedFactory }

function TGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygonUseConverter(
  const ASource: IGeometryLonLatMultiPolygon;
  const AConverter: ILonLatPointConverter;
  const ATemp: IDoublePointsAggregator =
  nil
): IGeometryProjectedMultiPolygon;
var
  VPoint: TDoublePoint;
  VLine: IGeometryProjectedSinglePolygon;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  i: Integer;
  VSourceLine: IGeometryLonLatSinglePolygon;
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
      VLine := TGeometryProjectedPolygon.Create(VTemp.Points, VTemp.Count);
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
    VLine := TGeometryProjectedPolygon.Create(VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPolygonEmpty.Create;
  end else if VLineCount = 1 then begin
    Result := TGeometryProjectedMultiPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiPolygon.Create(VBounds, VList.MakeStaticAndClear);
  end;
end;

function TGeometryProjectedFactory.CreateProjectedPath(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryProjectedMultiLine;
var
  VLine: IGeometryProjectedSingleLine;
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
        VLine := TGeometryProjectedLine.Create(VStart, VLineLen);
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
    VLine := TGeometryProjectedLine.Create(VStart, VLineLen);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPathEmpty.Create;
  end else if VLineCount = 1 then begin
    Result := TGeometryProjectedMultiLineOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiLine.Create(VBounds, VList.MakeStaticAndClear);
  end;
end;

function TGeometryProjectedFactory.CreateProjectedPathByEnum(
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiLine;
var
  VPoint: TDoublePoint;
  VLine: IGeometryProjectedSingleLine;
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
        VLine := TGeometryProjectedLine.Create(VTemp.Points, VTemp.Count);
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
    VLine := TGeometryProjectedLine.Create(VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPathEmpty.Create;
  end else if VLineCount = 1 then begin
    Result := TGeometryProjectedMultiLineOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiLine.Create(VBounds, VList.MakeStaticAndClear);
  end;
end;

function TGeometryProjectedFactory.CreateProjectedPathByLonLatEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumLonLatPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiLine;
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
      VEnum,
      ATemp
    );
end;

function TGeometryProjectedFactory.CreateProjectedPathByLonLatPath(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiLine;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiLine;
begin
  Result :=
    CreateProjectedPathByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      ATemp
    );
end;

function TGeometryProjectedFactory.CreateProjectedPathByLonLatPathUseConverter(
  const ASource: IGeometryLonLatMultiLine;
  const AConverter: ILonLatPointConverter;
  const ATemp: IDoublePointsAggregator =
  nil
): IGeometryProjectedMultiLine;
var
  VPoint: TDoublePoint;
  VLine: IGeometryProjectedSingleLine;
  VList: IInterfaceListSimple;
  VLineCount: Integer;
  VTemp: IDoublePointsAggregator;
  i: Integer;
  VSourceLine: IGeometryLonLatSingleLine;
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
      VLine := TGeometryProjectedLine.Create(VTemp.Points, VTemp.Count);
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
    VLine := TGeometryProjectedLine.Create(VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPathEmpty.Create;
  end else if VLineCount = 1 then begin
    Result := TGeometryProjectedMultiLineOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiLine.Create(VBounds, VList.MakeStaticAndClear);
  end;
end;

function TGeometryProjectedFactory.CreateProjectedPathWithClipByLonLatEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumLonLatPoint;
  const AMapPixelsClipRect: TDoubleRect;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiLine;
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
      VEnum,
      ATemp
    );
end;

function TGeometryProjectedFactory.CreateProjectedPathWithClipByLonLatPath(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiLine;
  const AMapPixelsClipRect: TDoubleRect;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiLine;
begin
  Result :=
    CreateProjectedPathWithClipByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      AMapPixelsClipRect,
      ATemp
    );
end;

function TGeometryProjectedFactory.CreateProjectedPolygon(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryProjectedMultiPolygon;
var
  VLine: IGeometryProjectedSinglePolygon;
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
        VLine := TGeometryProjectedPolygon.Create(VStart, VLineLen);
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
    VLine := TGeometryProjectedPolygon.Create(VStart, VLineLen);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPolygonEmpty.Create;
  end else if VLineCount = 1 then begin
    Result := TGeometryProjectedMultiPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiPolygon.Create(VBounds, VList.MakeStaticAndClear);
  end;
end;

function TGeometryProjectedFactory.CreateProjectedPolygonByEnum(
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiPolygon;
var
  VPoint: TDoublePoint;
  VLine: IGeometryProjectedSinglePolygon;
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
        VLine := TGeometryProjectedPolygon.Create(VTemp.Points, VTemp.Count);
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
    VLine := TGeometryProjectedPolygon.Create(VTemp.Points, VTemp.Count);
    if VLineCount > 0 then begin
      VBounds := UnionProjectedRects(VBounds, VLine.Bounds);
    end else begin
      VBounds := VLine.Bounds;
    end;
    Inc(VLineCount);
    VTemp.Clear;
  end;
  if VLineCount = 0 then begin
    Result := TProjectedPolygonEmpty.Create;
  end else if VLineCount = 1 then begin
    Result := TGeometryProjectedMultiPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiPolygon.Create(VBounds, VList.MakeStaticAndClear);
  end;
end;

function TGeometryProjectedFactory.CreateProjectedPolygonByLonLatEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumLonLatPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiPolygon;
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
      VEnum,
      ATemp
    );
end;

function TGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiPolygon;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiPolygon;
begin
  Result :=
    CreateProjectedPolygonByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      ATemp
    );
end;

function TGeometryProjectedFactory.CreateProjectedPolygonByRect(
  const ARect: TDoubleRect
): IGeometryProjectedMultiPolygon;
begin
  Result := TGeometryProjectedMultiPolygonOneLine.Create(CreateProjectedPolygonLineByRect(ARect));
end;

function TGeometryProjectedFactory.CreateProjectedPolygonLineByRect(
  const ARect: TDoubleRect
): IGeometryProjectedSinglePolygon;
var
  VPoints: array [0..4] of TDoublePoint;
begin
  VPoints[0] := ARect.TopLeft;
  VPoints[1].X := ARect.Right;
  VPoints[1].Y := ARect.Top;
  VPoints[2] := ARect.BottomRight;
  VPoints[3].X := ARect.Left;
  VPoints[3].Y := ARect.Bottom;
  Result := TGeometryProjectedPolygon.Create(@VPoints[0], 4);
end;

function TGeometryProjectedFactory.CreateProjectedPolygonWithClipByLonLatEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumLonLatPoint;
  const AMapPixelsClipRect: TDoubleRect;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiPolygon;
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
      VEnum,
      ATemp
    );
end;

function TGeometryProjectedFactory.CreateProjectedPolygonWithClipByLonLatPolygon(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiPolygon;
  const AMapPixelsClipRect: TDoubleRect;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiPolygon;
begin
  Result :=
    CreateProjectedPolygonWithClipByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      AMapPixelsClipRect,
      ATemp
    );
end;

end.

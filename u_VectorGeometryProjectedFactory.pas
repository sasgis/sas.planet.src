unit u_VectorGeometryProjectedFactory;

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
  TVectorGeometryProjectedFactory = class(TBaseInterfacedObject, IGeometryProjectedFactory)
  private
    function CreateProjectedPath(
      const AProjection: IProjectionInfo;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygon(
      const AProjection: IProjectionInfo;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPolygonLineByRect(
      const AProjection: IProjectionInfo;
      const ARect: TDoubleRect
    ): IGeometryProjectedPolygon;
    function CreateProjectedPolygonByRect(
      const AProjection: IProjectionInfo;
      const ARect: TDoubleRect
    ): IGeometryProjectedMultiPolygon;

    function CreateProjectedPathByEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonByEnum(
      const AProjection: IProjectionInfo;
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
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatMultiLine;
      const AConverter: ILonLatPointConverter;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedMultiLine;
    function CreateProjectedPolygonByLonLatPolygonUseConverter(
      const AProjection: IProjectionInfo;
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
  u_ProjectedSingleLine,
  u_EnumDoublePointLonLatToMapPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual,
  u_VectorItemProjected;

{ TVectorGeometryProjectedFactory }

function TVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygonUseConverter(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiPolygon;
  const AConverter: ILonLatPointConverter;
  const ATemp: IDoublePointsAggregator =
  nil
): IGeometryProjectedMultiPolygon;
var
  VPoint: TDoublePoint;
  VLine: IGeometryProjectedPolygon;
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
    Result := TGeometryProjectedMultiPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiPolygon.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPath(
  const AProjection: IProjectionInfo;
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryProjectedMultiLine;
var
  VLine: IGeometryProjectedLine;
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
    Result := TGeometryProjectedMultiLineOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiLine.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPathByEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiLine;
var
  VPoint: TDoublePoint;
  VLine: IGeometryProjectedLine;
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
    Result := TGeometryProjectedMultiLineOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiLine.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPathByLonLatEnum(
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
      AProjection,
      VEnum,
      ATemp
    );
end;

function TVectorGeometryProjectedFactory.CreateProjectedPathByLonLatPath(
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

function TVectorGeometryProjectedFactory.CreateProjectedPathByLonLatPathUseConverter(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatMultiLine;
  const AConverter: ILonLatPointConverter;
  const ATemp: IDoublePointsAggregator =
  nil
): IGeometryProjectedMultiLine;
var
  VPoint: TDoublePoint;
  VLine: IGeometryProjectedLine;
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
    Result := TGeometryProjectedMultiLineOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiLine.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPathWithClipByLonLatEnum(
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

function TVectorGeometryProjectedFactory.CreateProjectedPolygon(
  const AProjection: IProjectionInfo;
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryProjectedMultiPolygon;
var
  VLine: IGeometryProjectedPolygon;
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
    Result := TGeometryProjectedMultiPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiPolygon.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygonByEnum(
  const AProjection: IProjectionInfo;
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedMultiPolygon;
var
  VPoint: TDoublePoint;
  VLine: IGeometryProjectedPolygon;
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
    Result := TGeometryProjectedMultiPolygonOneLine.Create(VLine);
  end else begin
    VList.Add(VLine);
    Result := TGeometryProjectedMultiPolygon.Create(AProjection, VBounds, VList.MakeStaticAndClear);
  end;
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatEnum(
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
      AProjection,
      VEnum,
      ATemp
    );
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
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

function TVectorGeometryProjectedFactory.CreateProjectedPolygonByRect(
  const AProjection: IProjectionInfo;
  const ARect: TDoubleRect
): IGeometryProjectedMultiPolygon;
begin
  Result := TGeometryProjectedMultiPolygonOneLine.Create(CreateProjectedPolygonLineByRect(AProjection, ARect));
end;

function TVectorGeometryProjectedFactory.CreateProjectedPolygonLineByRect(
  const AProjection: IProjectionInfo;
  const ARect: TDoubleRect
): IGeometryProjectedPolygon;
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

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
    FEmptyPath: IGeometryProjectedMultiLine;
    FEmptyPolygon: IGeometryProjectedMultiPolygon;
  private
    function CreateProjectedPathEmpty: IGeometryProjectedLine;
    function CreateProjectedPolygonEmpty: IGeometryProjectedPolygon;

    function CreateProjectedPathByEnum(
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedLine;
    function CreateProjectedPolygonByEnum(
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedPolygon;

    function CreateProjectedPathByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedLine;
    function CreateProjectedPolygonByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedPolygon;

    function CreateProjectedPathByLonLatPath(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatLine;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedLine;
    function CreateProjectedPolygonByLonLatPolygon(
      const AProjection: IProjectionInfo;
      const ASource: IGeometryLonLatPolygon;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedPolygon;
  public
    constructor Create;
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

constructor TGeometryProjectedFactory.Create;
var
  VEmpty: TProjectedLineSetEmpty;
begin
  inherited Create;
  VEmpty := TProjectedLineSetEmpty.Create;
  FEmptyPath := VEmpty;
  FEmptyPolygon := VEmpty;
end;

function TGeometryProjectedFactory.CreateProjectedPathEmpty: IGeometryProjectedLine;
begin
  Result := FEmptyPath;
end;

function TGeometryProjectedFactory.CreateProjectedPathByEnum(
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedLine;
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
    Result := FEmptyPath;
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
): IGeometryProjectedLine;
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
  const ASource: IGeometryLonLatLine;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedLine;
begin
  Result :=
    CreateProjectedPathByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      ATemp
    );
end;

function TGeometryProjectedFactory.CreateProjectedPolygonEmpty: IGeometryProjectedPolygon;
begin
  Result := FEmptyPolygon;
end;

function TGeometryProjectedFactory.CreateProjectedPolygonByEnum(
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedPolygon;
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
    Result := FEmptyPolygon;
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
): IGeometryProjectedPolygon;
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
  const ASource: IGeometryLonLatPolygon;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedPolygon;
begin
  Result :=
    CreateProjectedPolygonByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      ATemp
    );
end;

end.

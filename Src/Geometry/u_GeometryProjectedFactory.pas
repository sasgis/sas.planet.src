unit u_GeometryProjectedFactory;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_EnumDoublePoint,
  i_DoublePoints,
  i_DoublePointFilter,
  i_DoublePointsAggregator,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_GeometryProjectedFactory,
  u_BaseInterfacedObject;

type
  TGeometryProjectedFactory = class(TBaseInterfacedObject, IGeometryProjectedFactory)
  private
    function MakeLineBuilder(): IGeometryProjectedLineBuilder;
    function MakePolygonBuilder(): IGeometryProjectedPolygonBuilder;

    function CreateProjectedLineByEnum(
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedLine;
    function CreateProjectedPolygonByEnum(
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedPolygon;

    function CreateProjectedLineByLonLatPath(
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
  Math,
  SysUtils,
  i_InterfaceListSimple,
  u_GeoFunc,
  u_InterfaceListSimple,
  u_DoublePointsAggregator,
  u_GeometryProjected,
  u_EnumDoublePointByLineSet,
  u_EnumDoublePointLonLatToMapPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual,
  u_GeometryProjectedMulti;

type
  TGeometryProjectedLineBuilder = class(TBaseInterfacedObject, IGeometryProjectedLineBuilder)
  private
    FBounds: TDoubleRect;
    FLine: IGeometryProjectedSingleLine;
    FList: IInterfaceListSimple;
  private
    procedure AddLine(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    );

    function MakeStaticAndClear: IGeometryProjectedLine;
    function MakeStaticCopy: IGeometryProjectedLine;
  public
    constructor Create;
  end;

{ TGeometryProjectedLineBuilder }

constructor TGeometryProjectedLineBuilder.Create;
begin
  inherited Create;
end;

procedure TGeometryProjectedLineBuilder.AddLine(
  const ABounds: TDoubleRect;
  const APoints: IDoublePoints
);
var
  VLine: IGeometryProjectedSingleLine;
begin
  Assert(Assigned(APoints));
  VLine := TGeometryProjectedLine.Create(ABounds, APoints);
  if not Assigned(FLine) then begin
    FLine := VLine;
    FBounds := ABounds;
  end else begin
    if not Assigned(FList) then begin
      FList := TInterfaceListSimple.Create;
      FList.Add(FLine);
      FLine := nil;
    end else if FList.Count = 0 then begin
      FList.Add(FLine);
      FLine := nil;
    end;
    FList.Add(VLine);
    FBounds := UnionProjectedRects(FBounds, ABounds);
  end;
end;

function TGeometryProjectedLineBuilder.MakeStaticAndClear: IGeometryProjectedLine;
begin
  Result := nil;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      Result := TGeometryProjectedMultiLine.Create(FBounds, FList.MakeStaticAndClear);
    end else begin
      Result := FLine;
    end;
    FLine := nil;
  end;
end;

function TGeometryProjectedLineBuilder.MakeStaticCopy: IGeometryProjectedLine;
begin
  Result := nil;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
       Result := TGeometryProjectedMultiLine.Create(FBounds, FList.MakeStaticCopy);
    end else begin
      Result := FLine;
    end;
  end;
end;

type
  TGeometryProjectedPolygonBuilder = class(TBaseInterfacedObject, IGeometryProjectedPolygonBuilder)
  private
    FPolygonBounds: TDoubleRect;
    FMultiPolygonBounds: TDoubleRect;
    FPoints: IDoublePoints;
    FPolygonList: IInterfaceListSimple;
    FHoleList: IInterfaceListSimple;
    function MakeCurrentSinglePolygon(const AIsClear: Boolean): IGeometryProjectedSinglePolygon;
  private
    procedure AddOuter(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    );
    procedure AddHole(
      const ABounds: TDoubleRect;
      const APoints: IDoublePoints
    );

    function MakeStaticAndClear: IGeometryProjectedPolygon;
    function MakeStaticCopy: IGeometryProjectedPolygon;
  public
    constructor Create;
  end;

{ TGeometryProjectedPolygonBuilder }

constructor TGeometryProjectedPolygonBuilder.Create;
begin
  inherited Create;
end;

procedure TGeometryProjectedPolygonBuilder.AddHole(const ABounds: TDoubleRect;
  const APoints: IDoublePoints);
begin

end;

procedure TGeometryProjectedPolygonBuilder.AddOuter(
  const ABounds: TDoubleRect;
  const APoints: IDoublePoints
);
var
  VPolygon: IGeometryProjectedSinglePolygon;
begin
  Assert(Assigned(APoints));
  if Assigned(FPoints) then begin
    VPolygon := MakeCurrentSinglePolygon(True);
    if not Assigned(FPolygonList) then begin
      FPolygonList := TInterfaceListSimple.Create;
    end;
    if FPolygonList.Count > 0 then begin
      FMultiPolygonBounds := UnionProjectedRects(FMultiPolygonBounds, FPolygonBounds);
    end else begin
      FMultiPolygonBounds := FPolygonBounds;
    end;
    FPolygonList.Add(VPolygon);
  end;
  FPoints := APoints;
  FPolygonBounds := ABounds;
end;

function TGeometryProjectedPolygonBuilder.MakeCurrentSinglePolygon(const AIsClear: Boolean): IGeometryProjectedSinglePolygon;
begin
  Assert(Assigned(FPoints));
  if Assigned(FHoleList) and (FHoleList.Count > 0) then begin
    Result :=
      TGeometryProjectedPolygonWithHoles.Create(
        FPolygonBounds,
        TGeometryProjectedContour.Create(FPolygonBounds, FPoints),
        FHoleList.MakeStaticAndClear
      );
    if AIsClear then begin
      FPoints := nil;
      FHoleList.Clear;
    end;
  end else begin
    Result := TGeometryProjectedPolygon.Create(FPolygonBounds, FPoints);
    if AIsClear then begin
      FPoints := nil;
    end;
  end;
end;

function TGeometryProjectedPolygonBuilder.MakeStaticAndClear: IGeometryProjectedPolygon;
begin
  Result := nil;
  if Assigned(FPoints) then begin
    if Assigned(FPolygonList) and (FPolygonList.Count > 0) then begin
      FPolygonList.Add(MakeCurrentSinglePolygon(True));
      FMultiPolygonBounds := UnionProjectedRects(FMultiPolygonBounds, FPolygonBounds);

      Result := TGeometryProjectedMultiPolygon.Create(FMultiPolygonBounds, FPolygonList.MakeStaticAndClear);
    end else begin
      Result := MakeCurrentSinglePolygon(True);
    end;
    FPoints := nil;
  end;
end;

function TGeometryProjectedPolygonBuilder.MakeStaticCopy: IGeometryProjectedPolygon;
begin
  Result := nil;
  if Assigned(FPoints) then begin
    if Assigned(FPolygonList) and (FPolygonList.Count > 0) then begin
      FPolygonList.Add(MakeCurrentSinglePolygon(False));
      FMultiPolygonBounds := UnionProjectedRects(FMultiPolygonBounds, FPolygonBounds);

      Result := TGeometryProjectedMultiPolygon.Create(FMultiPolygonBounds, FPolygonList.MakeStaticCopy);
    end else begin
      Result := MakeCurrentSinglePolygon(False);
    end;
  end;
end;

{ TGeometryProjectedFactory }

constructor TGeometryProjectedFactory.Create;
begin
  inherited Create;
end;

function TGeometryProjectedFactory.CreateProjectedLineByEnum(
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedLine;
var
  VPoint: TDoublePoint;
  VTemp: IDoublePointsAggregator;
  VBounds: TDoubleRect;
  VBuilder: IGeometryProjectedLineBuilder;
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
        VBuilder.AddLine(VBounds, VTemp.MakeStaticCopy);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VBounds.TopLeft := VPoint;
        VBounds.BottomRight := VPoint;
      end else begin
        UpdateProjectedMBRByPoint(VBounds, VPoint);
      end;
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    VBuilder.AddLine(VBounds, VTemp.MakeStaticCopy);
    VTemp.Clear;
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryProjectedFactory.CreateProjectedLineByLonLatPath(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatLine;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedLine;
var
  VEnum: IEnumProjectedPoint;
begin
  VEnum :=
    TEnumDoublePointLonLatToMapPixel.Create(
      AProjection.Zoom,
      AProjection.GeoConverter,
      ASource.GetEnum
    );
  VEnum := TEnumProjectedPointFilterEqual.Create(VEnum);
  Result :=
    CreateProjectedLineByEnum(
      VEnum,
      ATemp
    );
end;

function TGeometryProjectedFactory.MakeLineBuilder: IGeometryProjectedLineBuilder;
begin
  Result := TGeometryProjectedLineBuilder.Create;
end;

function TGeometryProjectedFactory.MakePolygonBuilder: IGeometryProjectedPolygonBuilder;
begin
  Result := TGeometryProjectedPolygonBuilder.Create;
end;

function TGeometryProjectedFactory.CreateProjectedPolygonByEnum(
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedPolygon;
var
  VPoint: TDoublePoint;
  VTemp: IDoublePointsAggregator;
  VBounds: TDoubleRect;
  VBuilder: IGeometryProjectedPolygonBuilder;
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
        VBuilder.AddOuter(VBounds, VTemp.MakeStaticCopy);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VBounds.TopLeft := VPoint;
        VBounds.BottomRight := VPoint;
      end else begin
        UpdateProjectedMBRByPoint(VBounds, VPoint);
      end;
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    VBuilder.AddOuter(VBounds, VTemp.MakeStaticCopy);
    VTemp.Clear;
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatPolygon;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedPolygon;
var
  VEnum: IEnumProjectedPoint;
begin
  VEnum :=
    TEnumDoublePointLonLatToMapPixel.Create(
      AProjection.Zoom,
      AProjection.GeoConverter,
      ASource.GetEnum
    );
  VEnum :=
    TEnumProjectedPointFilterEqual.Create(VEnum);
  Result :=
    CreateProjectedPolygonByEnum(
      VEnum,
      ATemp
    );
end;

end.

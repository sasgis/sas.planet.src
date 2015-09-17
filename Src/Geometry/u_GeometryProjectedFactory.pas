unit u_GeometryProjectedFactory;

interface

uses
  t_GeoTypes,
  i_Projection,
  i_EnumDoublePoint,
  i_DoublePoints,
  i_DoublePointsAggregator,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_GeometryProjectedFactory,
  u_BaseInterfacedObject;

type
  TGeometryProjectedFactory = class(TBaseInterfacedObject, IGeometryProjectedFactory)
  private
    procedure LonLatSinglePolygonToBuilder(
      const ABuilder: IGeometryProjectedPolygonBuilder;
      const AProjection: IProjection;
      const ASource: IGeometryLonLatSinglePolygon;
      const ATemp: IDoublePointsAggregator = nil
    );
    procedure LonLatSingleLineToBuilder(
      const ABuilder: IGeometryProjectedLineBuilder;
      const AProjection: IProjection;
      const ASource: IGeometryLonLatSingleLine;
      const ATemp: IDoublePointsAggregator = nil
    );
    function EnumToPoints(
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator;
      var ABounds: TDoubleRect
    ): IDoublePoints;

  private
    function MakeLineBuilder(): IGeometryProjectedLineBuilder;
    function MakePolygonBuilder(): IGeometryProjectedPolygonBuilder;

    function CreateProjectedLineByLonLatPath(
      const AProjection: IProjection;
      const ASource: IGeometryLonLatLine;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedLine;
    function CreateProjectedPolygonByLonLatPolygon(
      const AProjection: IProjection;
      const ASource: IGeometryLonLatPolygon;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedPolygon;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  i_InterfaceListSimple,
  u_GeoFunc,
  u_InterfaceListSimple,
  u_DoublePointsAggregator,
  u_GeometryProjected,
  u_EnumDoublePointLonLatToMapPixel,
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

procedure TGeometryProjectedPolygonBuilder.AddHole(
  const ABounds: TDoubleRect;
  const APoints: IDoublePoints
);
var
  VHole: IGeometryProjectedContour;
begin
  Assert(Assigned(APoints));
  if Assigned(FPoints) then begin
    if not Assigned(FHoleList) then begin
      FHoleList := TInterfaceListSimple.Create;
    end;
    VHole := TGeometryProjectedContour.Create(ABounds, APoints);
    FHoleList.Add(VHole);
  end else begin
    FPoints := APoints;
    FPolygonBounds := ABounds;
  end;
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

function TGeometryProjectedFactory.EnumToPoints(
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator;
  var ABounds: TDoubleRect
): IDoublePoints;
var
  VPoint: TDoublePoint;
begin
  Result := nil;
  if AEnum.Next(VPoint) then begin
    ABounds.TopLeft := VPoint;
    ABounds.BottomRight := VPoint;
    ATemp.Add(VPoint);
    while AEnum.Next(VPoint) do begin
      UpdateProjectedMBRByPoint(ABounds, VPoint);
      ATemp.Add(VPoint);
    end;
    Result := ATemp.MakeStaticAndClear;
  end;
end;

procedure TGeometryProjectedFactory.LonLatSingleLineToBuilder(
  const ABuilder: IGeometryProjectedLineBuilder;
  const AProjection: IProjection;
  const ASource: IGeometryLonLatSingleLine;
  const ATemp: IDoublePointsAggregator
);
var
  VEnum: IEnumProjectedPoint;
  VBounds: TDoubleRect;
  VPoints: IDoublePoints;
begin
  VEnum :=
    TEnumDoublePointLonLatToMapPixel.Create(
      AProjection,
      ASource.GetEnum
    );
  VEnum := TEnumProjectedPointFilterEqual.Create(VEnum);
  VPoints := EnumToPoints(VEnum, ATemp, VBounds);
  if Assigned(VPoints) then begin
    ABuilder.AddLine(VBounds, VPoints);
  end;
end;

function TGeometryProjectedFactory.CreateProjectedLineByLonLatPath(
  const AProjection: IProjection;
  const ASource: IGeometryLonLatLine;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedLine;
var
  VTemp: IDoublePointsAggregator;
  VBuilder: IGeometryProjectedLineBuilder;
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
  i: Integer;
begin
  VBuilder := MakeLineBuilder;

  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  if Supports(ASource, IGeometryLonLatSingleLine, VSingleLine) then begin
    LonLatSingleLineToBuilder(VBuilder, AProjection, VSingleLine, VTemp);
  end else if Supports(ASource, IGeometryLonLatMultiLine, VMultiLine) then begin
    for i := 0 to VMultiLine.Count - 1 do begin
      VSingleLine := VMultiLine.Item[i];
      LonLatSingleLineToBuilder(VBuilder, AProjection, VSingleLine, VTemp);
    end;
  end else begin
    Assert(False);
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryProjectedFactory.MakeLineBuilder: IGeometryProjectedLineBuilder;
begin
  Result := TGeometryProjectedLineBuilder.Create;
end;

function TGeometryProjectedFactory.MakePolygonBuilder: IGeometryProjectedPolygonBuilder;
begin
  Result := TGeometryProjectedPolygonBuilder.Create;
end;

procedure TGeometryProjectedFactory.LonLatSinglePolygonToBuilder(
  const ABuilder: IGeometryProjectedPolygonBuilder;
  const AProjection: IProjection;
  const ASource: IGeometryLonLatSinglePolygon;
  const ATemp: IDoublePointsAggregator
);
var
  VEnum: IEnumProjectedPoint;
  VBounds: TDoubleRect;
  VPoints: IDoublePoints;
  i: Integer;
begin
  VEnum :=
    TEnumDoublePointLonLatToMapPixel.Create(
      AProjection,
      ASource.OuterBorder.GetEnum
    );
  VEnum := TEnumProjectedPointFilterEqual.Create(VEnum);
  VPoints := EnumToPoints(VEnum, ATemp, VBounds);
  if Assigned(VPoints) then begin
    ABuilder.AddOuter(VBounds, VPoints);
    for i := 0 to ASource.HoleCount - 1 do begin
      VEnum :=
        TEnumDoublePointLonLatToMapPixel.Create(
          AProjection,
          ASource.HoleBorder[i].GetEnum
        );
      VEnum := TEnumProjectedPointFilterEqual.Create(VEnum);
      VPoints := EnumToPoints(VEnum, ATemp, VBounds);
      if Assigned(VPoints) then begin
        ABuilder.AddHole(VBounds, VPoints);
      end;
    end;
  end;
end;

function TGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
  const AProjection: IProjection;
  const ASource: IGeometryLonLatPolygon;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedPolygon;
var
  VTemp: IDoublePointsAggregator;
  VBuilder: IGeometryProjectedPolygonBuilder;
  VSinglePolygon: IGeometryLonLatSinglePolygon;
  VMultiPolygon: IGeometryLonLatMultiPolygon;
  i: Integer;
begin
  VBuilder := MakePolygonBuilder;

  VTemp := ATemp;
  if VTemp = nil then begin
    VTemp := TDoublePointsAggregator.Create;
  end;
  if Supports(ASource, IGeometryLonLatSinglePolygon, VSinglePolygon) then begin
    LonLatSinglePolygonToBuilder(VBuilder, AProjection, VSinglePolygon, VTemp);
  end else if Supports(ASource, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
    for i := 0 to VMultiPolygon.Count - 1 do begin
      VSinglePolygon := VMultiPolygon.Item[i];
      LonLatSinglePolygonToBuilder(VBuilder, AProjection, VSinglePolygon, VTemp);
    end;
  end else begin
    Assert(False);
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

end.

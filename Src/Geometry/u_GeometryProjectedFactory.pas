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
    function CreateProjectedLineInternal(
      const ARect: TDoubleRect;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryProjectedSingleLine;
    function CreateProjectedPolygonInternal(
      const ARect: TDoubleRect;
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryProjectedSinglePolygon;
  private
    function MakeMultiLineBuilder(): IGeometryProjectedMultiLineBuilder;
    function MakeMultiPolygonBuilder(): IGeometryProjectedMultiPolygonBuilder;

    function CreateProjectedLineEmpty: IGeometryProjectedLine;
    function CreateProjectedPolygonEmpty: IGeometryProjectedPolygon;

    function CreateProjectedLineByEnum(
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedLine;
    function CreateProjectedPolygonByEnum(
      const AEnum: IEnumProjectedPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedPolygon;

    function CreateProjectedLineByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
      const ATemp: IDoublePointsAggregator = nil
    ): IGeometryProjectedLine;
    function CreateProjectedPolygonByLonLatEnum(
      const AProjection: IProjectionInfo;
      const AEnum: IEnumLonLatPoint;
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
  i_InterfaceListSimple,
  u_GeoFunc,
  u_InterfaceListSimple,
  u_DoublePointsAggregator,
  u_GeometryProjected,
  u_EnumDoublePointLonLatToMapPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual,
  u_GeometryProjectedMulti;

type
  TGeometryProjectedMultiLineBuilder = class(TBaseInterfacedObject, IGeometryProjectedMultiLineBuilder)
  private
    FEmpty: IGeometryProjectedLine;
    FBounds: TDoubleRect;
    FLine: IGeometryProjectedSingleLine;
    FList: IInterfaceListSimple;
  private
    procedure Add(const AElement: IGeometryProjectedSingleLine);

    function MakeStaticAndClear: IGeometryProjectedLine;
    function MakeStaticCopy: IGeometryProjectedLine;
  public
    constructor Create(
      const AEmpty: IGeometryProjectedLine
    );
  end;

{ TGeometryProjectedMultiLineBuilder }

constructor TGeometryProjectedMultiLineBuilder.Create(
  const AEmpty: IGeometryProjectedLine
);
begin
  inherited Create;
  FEmpty := AEmpty;
end;

procedure TGeometryProjectedMultiLineBuilder.Add(
  const AElement: IGeometryProjectedSingleLine
);
begin
  Assert(Assigned(AElement));
  if not Assigned(FLine) then begin
    FLine := AElement;
    FBounds := FLine.Bounds;
  end else begin
    if not Assigned(FList) then begin
      FList := TInterfaceListSimple.Create;
      FList.Add(FLine);
    end else if FList.Count = 0 then begin
      FList.Add(FLine);
    end;
    FList.Add(AElement);
    FBounds := UnionProjectedRects(FBounds, AElement.Bounds);
  end;
end;

function TGeometryProjectedMultiLineBuilder.MakeStaticAndClear: IGeometryProjectedLine;
begin
  Result := FEmpty;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      Result := TGeometryProjectedMultiLine.Create(FBounds, FList.MakeStaticAndClear);
    end else begin
      Result := FLine;
    end;
    FLine := nil;
  end;
end;

function TGeometryProjectedMultiLineBuilder.MakeStaticCopy: IGeometryProjectedLine;
begin
  Result := FEmpty;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
       Result := TGeometryProjectedMultiLine.Create(FBounds, FList.MakeStaticCopy);
    end else begin
      Result := FLine;
    end;
  end;
end;

type
  TGeometryProjectedMultiPolygonBuilder = class(TBaseInterfacedObject, IGeometryProjectedMultiPolygonBuilder)
  private
    FEmpty: IGeometryProjectedPolygon;
    FBounds: TDoubleRect;
    FLine: IGeometryProjectedSinglePolygon;
    FList: IInterfaceListSimple;
  private
    procedure Add(const AElement: IGeometryProjectedSinglePolygon);

    function MakeStaticAndClear: IGeometryProjectedPolygon;
    function MakeStaticCopy: IGeometryProjectedPolygon;
  public
    constructor Create(
      const AEmpty: IGeometryProjectedPolygon
    );
  end;

{ TGeometryProjectedMultiPolygonBuilder }

constructor TGeometryProjectedMultiPolygonBuilder.Create(
  const AEmpty: IGeometryProjectedPolygon
);
begin
  inherited Create;
  FEmpty := AEmpty;
end;

procedure TGeometryProjectedMultiPolygonBuilder.Add(
  const AElement: IGeometryProjectedSinglePolygon
);
begin
  Assert(Assigned(AElement));
  if not Assigned(FLine) then begin
    FLine := AElement;
    FBounds := FLine.Bounds;
  end else begin
    if not Assigned(FList) then begin
      FList := TInterfaceListSimple.Create;
      FList.Add(FLine);
    end else if FList.Count = 0 then begin
      FList.Add(FLine);
    end;
    FList.Add(AElement);
    FBounds := UnionProjectedRects(FBounds, AElement.Bounds);
  end;
end;

function TGeometryProjectedMultiPolygonBuilder.MakeStaticAndClear: IGeometryProjectedPolygon;
begin
  Result := FEmpty;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      Result := TGeometryProjectedMultiPolygon.Create(FBounds, FList.MakeStaticAndClear);
    end else begin
      Result := FLine;
    end;
    FLine := nil;
  end;
end;

function TGeometryProjectedMultiPolygonBuilder.MakeStaticCopy: IGeometryProjectedPolygon;
begin
  Result := FEmpty;
  if Assigned(FLine) then begin
    if Assigned(FList) and (FList.Count > 0) then begin
      Result := TGeometryProjectedMultiPolygon.Create(FBounds, FList.MakeStaticCopy);
    end else begin
      Result := FLine;
    end;
  end;
end;

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

function TGeometryProjectedFactory.CreateProjectedLineEmpty: IGeometryProjectedLine;
begin
  Result := FEmptyPath;
end;

function TGeometryProjectedFactory.CreateProjectedLineInternal(
  const ARect: TDoubleRect;
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryProjectedSingleLine;
begin
  Result := TGeometryProjectedLine.Create(APoints, ACount);
end;

function TGeometryProjectedFactory.CreateProjectedLineByEnum(
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedLine;
var
  VPoint: TDoublePoint;
  VLine: IGeometryProjectedSingleLine;
  VTemp: IDoublePointsAggregator;
  VBounds: TDoubleRect;
  VBuilder: IGeometryProjectedMultiLineBuilder;
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
        VLine := CreateProjectedLineInternal(VBounds, VTemp.Points, VTemp.Count);
        VBuilder.Add(VLine);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VBounds.TopLeft := VPoint;
        VBounds.BottomRight := VPoint;
      end else begin
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
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    VLine := CreateProjectedLineInternal(VBounds, VTemp.Points, VTemp.Count);
    VBuilder.Add(VLine);
    VTemp.Clear;
  end;
  Result := VBuilder.MakeStaticAndClear;
end;

function TGeometryProjectedFactory.CreateProjectedLineByLonLatEnum(
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
    CreateProjectedLineByEnum(
      VEnum,
      ATemp
    );
end;

function TGeometryProjectedFactory.CreateProjectedLineByLonLatPath(
  const AProjection: IProjectionInfo;
  const ASource: IGeometryLonLatLine;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedLine;
begin
  Result :=
    CreateProjectedLineByLonLatEnum(
      AProjection,
      ASource.GetEnum,
      ATemp
    );
end;

function TGeometryProjectedFactory.CreateProjectedPolygonEmpty: IGeometryProjectedPolygon;
begin
  Result := FEmptyPolygon;
end;

function TGeometryProjectedFactory.CreateProjectedPolygonInternal(
  const ARect: TDoubleRect;
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryProjectedSinglePolygon;
begin
  Result := TGeometryProjectedPolygon.Create(APoints, ACount);
end;

function TGeometryProjectedFactory.MakeMultiLineBuilder: IGeometryProjectedMultiLineBuilder;
begin
  Result := TGeometryProjectedMultiLineBuilder.Create(FEmptyPath);
end;

function TGeometryProjectedFactory.MakeMultiPolygonBuilder: IGeometryProjectedMultiPolygonBuilder;
begin
  Result := TGeometryProjectedMultiPolygonBuilder.Create(FEmptyPolygon);
end;

function TGeometryProjectedFactory.CreateProjectedPolygonByEnum(
  const AEnum: IEnumProjectedPoint;
  const ATemp: IDoublePointsAggregator
): IGeometryProjectedPolygon;
var
  VPoint: TDoublePoint;
  VLine: IGeometryProjectedSinglePolygon;
  VTemp: IDoublePointsAggregator;
  VBounds: TDoubleRect;
  VBuilder: IGeometryProjectedMultiPolygonBuilder;
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
        VLine := CreateProjectedPolygonInternal(VBounds, VTemp.Points, VTemp.Count);
        VBuilder.Add(VLine);
        VTemp.Clear;
      end;
    end else begin
      if VTemp.Count = 0 then begin
        VBounds.TopLeft := VPoint;
        VBounds.BottomRight := VPoint;
      end else begin
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
      VTemp.Add(VPoint);
    end;
  end;
  if VTemp.Count > 0 then begin
    VLine := CreateProjectedPolygonInternal(VBounds, VTemp.Points, VTemp.Count);
    VBuilder.Add(VLine);
    VTemp.Clear;
  end;
  Result := VBuilder.MakeStaticAndClear;
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

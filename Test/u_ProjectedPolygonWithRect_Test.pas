unit u_ProjectedPolygonWithRect_Test;

interface

uses
  TestFramework,
  t_GeoTypes,
  i_Projection,
  i_GeometryProjected,
  i_DoublePointsAggregator,
  i_GeometryProjectedFactory;

type
  TestProjectedPolygonWithRect = class(TTestCase)
  private
    FMultiPolygon: IGeometryProjectedPolygon;
    FPolygonMain: IGeometryProjectedPolygon;
    FPolygonMainOuter: IGeometryProjectedPolygon;
    FPolygonMainHole: IGeometryProjectedPolygon;
    FPolygonSecond: IGeometryProjectedPolygon;
  protected
    procedure SetUp; override;
  published
    // See Polygon and rects in ProjectedPolygonWithRect.png
    procedure TestRectR01IntersectPolygonAndHole;
    procedure TestRectR02NearPolygon;
    procedure TestRectR03NearPolygon;
    procedure TestRectR04InHole;
    procedure TestRectR05IntersectHole;
    procedure TestRectR06InMainPolygon;
    procedure TestRectR07BetweenMainAndSecond;
    procedure TestRectR08InSecondPolygon;
    procedure TestRectR09IntersectSecondPolygon;
    procedure TestRectR10NearPolygon;
    procedure TestRectR11IntersectPolygon;
    procedure TestRectR12FarAway;
    procedure TestRectR13AroundPolygon;
    procedure TestRectR14AroundSecond;
  end;

implementation

uses
  u_GeoFunc,
  c_CoordConverter,
  i_HashFunction,
  i_DatumFactory,
  i_DoublePoints,
  i_ProjectionTypeFactory,
  i_ProjectionType,
  u_HashFunctionCityHash,
  u_HashFunctionWithCounter,
  u_DoublePointsAggregator,
  u_ProjectionBasic256x256,
  u_ProjectionTypeFactorySimple,
  u_GeometryProjectedFactory,
  u_InternalPerformanceCounterFake,
  u_DatumFactory;

{ TestProjectedPolygonWithRect }

procedure TestProjectedPolygonWithRect.SetUp;
var
  VHashFunction: IHashFunction;
  VProjectionTypeFactory: IProjectionTypeFactory;
  VProjectionType: IProjectionType;
  VDatumFactory: IDatumFactory;
  VFactory: IGeometryProjectedFactory;
  VPointsBuilder: IDoublePointsAggregator;
  VPoints: IDoublePoints;

  VProjection: IProjection;
  VPolygonBuilder: IGeometryProjectedPolygonBuilder;
  VPolygon: IGeometryProjectedMultiPolygon;
begin
  inherited;
  VHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      TInternalPerformanceCounterFake.Create
    );
  VDatumFactory := TDatumFactory.Create(VHashFunction);
  VProjectionTypeFactory :=
    TProjectionTypeFactorySimple.Create(
      VHashFunction,
      VDatumFactory
    );
  VProjectionType := VProjectionTypeFactory.GetByCode(CGoogleProjectionEPSG);
  VProjection := TProjectionBasic256x256.Create(0, VProjectionType, 20);
  VFactory := TGeometryProjectedFactory.Create;
  VPolygonBuilder := VFactory.MakePolygonBuilder;
  VPointsBuilder := TDoublePointsAggregator.Create;
  VPointsBuilder.Add(DoublePoint(50, 40));
  VPointsBuilder.Add(DoublePoint(220, 40));
  VPointsBuilder.Add(DoublePoint(160, 80));
  VPointsBuilder.Add(DoublePoint(230, 140));
  VPointsBuilder.Add(DoublePoint(230, 80));
  VPointsBuilder.Add(DoublePoint(280, 80));
  VPointsBuilder.Add(DoublePoint(280, 240));
  VPointsBuilder.Add(DoublePoint(170, 240));
  VPointsBuilder.Add(DoublePoint(170, 180));
  VPointsBuilder.Add(DoublePoint(50, 240));
  VPoints := VPointsBuilder.MakeStaticAndClear;
  VPolygonBuilder.AddOuter(ProjectedMBRByPoints(VPoints.Points, VPoints.Count), VPoints);

  VPointsBuilder.Add(DoublePoint(70, 60));
  VPointsBuilder.Add(DoublePoint(90, 80));
  VPointsBuilder.Add(DoublePoint(110, 50));
  VPointsBuilder.Add(DoublePoint(130, 80));
  VPointsBuilder.Add(DoublePoint(110, 80));
  VPointsBuilder.Add(DoublePoint(130, 120));
  VPointsBuilder.Add(DoublePoint(90, 130));
  VPointsBuilder.Add(DoublePoint(70, 160));
  VPointsBuilder.Add(DoublePoint(80, 90));
  VPoints := VPointsBuilder.MakeStaticAndClear;
  VPolygonBuilder.AddHole(ProjectedMBRByPoints(VPoints.Points, VPoints.Count), VPoints);

  VPointsBuilder.Add(DoublePoint(420, 40));
  VPointsBuilder.Add(DoublePoint(480, 40));
  VPointsBuilder.Add(DoublePoint(480, 240));
  VPointsBuilder.Add(DoublePoint(420, 240));
  VPoints := VPointsBuilder.MakeStaticAndClear;
  VPolygonBuilder.AddOuter(ProjectedMBRByPoints(VPoints.Points, VPoints.Count), VPoints);

  FMultiPolygon := VPolygonBuilder.MakeStaticAndClear;
  VPolygon := FMultiPolygon as IGeometryProjectedMultiPolygon;

  FPolygonMain := VPolygon.Item[0];
  FPolygonMainOuter := VPolygon.Item[0].OuterBorder;
  FPolygonMainHole := VPolygon.Item[0].HoleBorder[0];
  FPolygonSecond := VPolygon.Item[1];
end;

procedure TestProjectedPolygonWithRect.TestRectR01IntersectPolygonAndHole;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(20,10,90,90);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpNoIntersect);

  CheckTrue(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckTrue(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckTrue(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckTrue(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckTrue(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR02NearPolygon;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(240,40,280,70);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpNoIntersect);

  CheckFalse(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckFalse(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR03NearPolygon;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(190,80,220,90);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpNoIntersect);

  CheckFalse(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckFalse(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR04InHole;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(90,100,110,110);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpRectInPolygon);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpRectInPolygon);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpNoIntersect);

  CheckFalse(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckFalse(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR05IntersectHole;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(80,130,120,160);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpRectInPolygon);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpNoIntersect);

  CheckTrue(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckTrue(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckTrue(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckTrue(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR06InMainPolygon;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(220,160,260,190);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpRectInPolygon);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpRectInPolygon);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpRectInPolygon);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpNoIntersect);

  CheckTrue(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckFalse(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR07BetweenMainAndSecond;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(300,160,350,190);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpNoIntersect);

  CheckFalse(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckFalse(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR08InSecondPolygon;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(430,140,470,170);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpRectInPolygon);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpRectInPolygon);

  CheckTrue(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckFalse(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR09IntersectSecondPolygon;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(390,190,430,220);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpIntersectPartial);

  CheckTrue(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckTrue(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckTrue(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR10NearPolygon;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(130,210,160,230);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpNoIntersect);

  CheckFalse(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckFalse(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR11IntersectPolygon;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(40,220,80,250);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpNoIntersect);

  CheckTrue(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckTrue(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckTrue(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckTrue(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR12FarAway;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(540,40,580,70);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpNoIntersect);

  CheckFalse(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckFalse(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR13AroundPolygon;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(10,20,520,260);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpPolygonInRect);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpPolygonInRect);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpPolygonInRect);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpPolygonInRect);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpPolygonInRect);

  CheckTrue(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckFalse(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

procedure TestProjectedPolygonWithRect.TestRectR14AroundSecond;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(400,10,500,250);

  Check(FMultiPolygon.CheckRectIntersection(VRect) = rwpIntersectPartial);
  Check(FPolygonMain.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainOuter.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonMainHole.CheckRectIntersection(VRect) = rwpNoIntersect);
  Check(FPolygonSecond.CheckRectIntersection(VRect) = rwpPolygonInRect);

  CheckTrue(FMultiPolygon.IsRectIntersectPolygon(VRect));
  CheckFalse(FMultiPolygon.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMain.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainOuter.IsRectIntersectBorder(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonMainHole.IsRectIntersectBorder(VRect));
  CheckTrue(FPolygonSecond.IsRectIntersectPolygon(VRect));
  CheckFalse(FPolygonSecond.IsRectIntersectBorder(VRect));
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestProjectedPolygonWithRect.Suite);
end.

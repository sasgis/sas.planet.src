unit u_CalcTilesInPolygon_Test;

interface

uses
  TestFramework,
  t_GeoTypes,
  i_Projection,
  i_DoublePointsAggregator,
  i_GeometryProjectedFactory;

type
  TestCalcTilesInPolygon = class(TTestCase)
  private
    FProjection: IProjection;
    FPolygonBuilder: IGeometryProjectedPolygonBuilder;
    FPoints: IDoublePointsAggregator;
  protected
    procedure SetUp; override;
  published
    procedure TestSimpleRect;
    procedure TestSimplePolygon;
    procedure TestSimplePolygonWithHole;
    procedure TestMultiPolygon;
  end;

implementation

uses
  u_GeoFunc,
  c_CoordConverter,
  i_GeometryProjected,
  i_HashFunction,
  i_DatumFactory,
  i_ProjectionTypeFactory,
  i_ProjectionType,
  u_HashFunctionCityHash,
  u_HashFunctionWithCounter,
  u_DoublePointsAggregator,
  u_ProjectionBasic256x256,
  u_ProjectionTypeFactorySimple,
  u_GeometryProjectedFactory,
  u_GeometryFunc,
  u_InternalPerformanceCounterFake,
  u_DatumFactory;

{ TestCalcTilesInPolygon }

procedure TestCalcTilesInPolygon.SetUp;
var
  VHashFunction: IHashFunction;
  VProjectionTypeFactory: IProjectionTypeFactory;
  VProjectionType: IProjectionType;
  VDatumFactory: IDatumFactory;
  VFactory: IGeometryProjectedFactory;
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
  FProjection := TProjectionBasic256x256.Create(0, VProjectionType, 20);
  VFactory := TGeometryProjectedFactory.Create;
  FPolygonBuilder := VFactory.MakePolygonBuilder;
  FPoints := TDoublePointsAggregator.Create;
end;

procedure TestCalcTilesInPolygon.TestSimpleRect;
var
  VPolygon: IGeometryProjectedPolygon;
  VTilesCount: Int64;
begin
  FPoints.Add(DoublePoint(0, 0));
  FPoints.Add(DoublePoint(256*10, 0));
  FPoints.Add(DoublePoint(256*10, 256*50));
  FPoints.Add(DoublePoint(0, 256*50));
  FPolygonBuilder.AddOuter(DoubleRect(0, 0, 256*10, 256*50), FPoints.MakeStaticAndClear);
  VPolygon := FPolygonBuilder.MakeStaticAndClear;
  VTilesCount := CalcTileCountInProjectedPolygon(FProjection, VPolygon);
  CheckEquals(500, VTilesCount);
end;

procedure TestCalcTilesInPolygon.TestSimplePolygon;
var
  VPolygon: IGeometryProjectedPolygon;
  VTilesCount: Int64;
begin
  FPoints.Add(DoublePoint(0, 0));
  FPoints.Add(DoublePoint(256*9.9, 0));
  FPoints.Add(DoublePoint(256*9.9, 256*49.9));
  FPoints.Add(DoublePoint(256*3, 256*10));
  FPoints.Add(DoublePoint(0, 256*49.9));
  FPolygonBuilder.AddOuter(DoubleRect(0, 0, 256*9.9, 256*49.9), FPoints.MakeStaticAndClear);
  VPolygon := FPolygonBuilder.MakeStaticAndClear;
  VTilesCount := CalcTileCountInProjectedPolygon(FProjection, VPolygon);
  CheckEquals(345, VTilesCount);
end;

procedure TestCalcTilesInPolygon.TestSimplePolygonWithHole;
var
  VPolygon: IGeometryProjectedPolygon;
  VTilesCount: Int64;
begin
  FPoints.Add(DoublePoint(0, 0));
  FPoints.Add(DoublePoint(256*9.9, 0));
  FPoints.Add(DoublePoint(256*9.9, 256*49.9));
  FPoints.Add(DoublePoint(256*3, 256*10));
  FPoints.Add(DoublePoint(0, 256*49.9));
  FPolygonBuilder.AddOuter(DoubleRect(0, 0, 256*9.9, 256*49.9), FPoints.MakeStaticAndClear);

  FPoints.Add(DoublePoint(256*0.9, 256*0.9));
  FPoints.Add(DoublePoint(256*2.1, 256*0.9));
  FPoints.Add(DoublePoint(256*2.1, 256*2.1));
  FPoints.Add(DoublePoint(256*0.9, 256*2.1));
  FPolygonBuilder.AddHole(DoubleRect(256*0.9, 256*0.9, 256*2.1, 256*2.1), FPoints.MakeStaticAndClear);

  VPolygon := FPolygonBuilder.MakeStaticAndClear;
  VTilesCount := CalcTileCountInProjectedPolygon(FProjection, VPolygon);
  CheckEquals(344, VTilesCount);
end;

procedure TestCalcTilesInPolygon.TestMultiPolygon;
var
  VPolygon: IGeometryProjectedPolygon;
  VTilesCount: Int64;
begin
  FPoints.Add(DoublePoint(0, 0));
  FPoints.Add(DoublePoint(256*9.9, 0));
  FPoints.Add(DoublePoint(256*9.9, 256*49.9));
  FPoints.Add(DoublePoint(256*3, 256*10));
  FPoints.Add(DoublePoint(0, 256*49.9));
  FPolygonBuilder.AddOuter(DoubleRect(0, 0, 256*9.9, 256*49.9), FPoints.MakeStaticAndClear);

  FPoints.Add(DoublePoint(256*20.1, 256*20.1));
  FPoints.Add(DoublePoint(256*20.9, 256*20.1));
  FPoints.Add(DoublePoint(256*20.9, 256*20.9));
  FPoints.Add(DoublePoint(256*20.1, 256*20.9));
  FPolygonBuilder.AddOuter(DoubleRect(256*20.1, 256*20.1, 256*20.9, 256*20.9), FPoints.MakeStaticAndClear);

  VPolygon := FPolygonBuilder.MakeStaticAndClear;
  VTilesCount := CalcTileCountInProjectedPolygon(FProjection, VPolygon);
  CheckEquals(346, VTilesCount);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestCalcTilesInPolygon.Suite);
end.

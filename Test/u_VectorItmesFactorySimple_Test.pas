unit u_VectorItmesFactorySimple_Test;

{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  TestFramework,
  t_GeoTypes,
  i_DoublePointsAggregator,
  i_GeometryProjectedFactory,
  i_GeometryLonLatFactory;

type
  TestGeometryLonLatLineBuilderSimple = class(TTestCase)
  private
    FBuilder: IGeometryLonLatLineBuilder;
    FPoints: IDoublePointsAggregator;
  protected
    procedure SetUp; override;
  published
    procedure CreateLonLatPathSimple;
    procedure CreateLonLatPathTwoLines;
    procedure CreateLonLatPathNoLines;
  end;

  TestGeometryLonLatPolygonBuilderSimple = class(TTestCase)
  private
    FBuilder: IGeometryLonLatPolygonBuilder;
    FPoints: IDoublePointsAggregator;
  protected
    procedure SetUp; override;
  published
    procedure CreateLonLatPolygonSimple;
    procedure CreateLonLatPolygonTwoLines;
    procedure CreateLonLatPolygonNoLines;
  end;

  TestGeometryProjectedLineBuilderSimple = class(TTestCase)
  private
    FBuilder: IGeometryProjectedLineBuilder;
    FPoints: IDoublePointsAggregator;
  protected
    procedure SetUp; override;
  published
    procedure CreateProjectedPathSimple;
    procedure CreateProjectedPathTwoLines;
    procedure CreateProjectedPathNoLines;
  end;

  TestGeometryProjectedPolygonBuilderSimple = class(TTestCase)
  private
    FBuilder: IGeometryProjectedPolygonBuilder;
    FPoints: IDoublePointsAggregator;
  protected
    procedure SetUp; override;
  published
    procedure CreateProjectedPolygonSimple;
    procedure CreateProjectedPolygonTwoLines;
    procedure CreateProjectedPolygonNoLines;
  end;

implementation

uses
  Math,
  i_GeometryLonLat,
  i_Projection,
  i_HashFunction,
  i_GeometryProjected,
  i_EnumDoublePoint,
  u_GeoFunc,
  u_HashFunctionCityHash,
  u_HashFunctionWithCounter,
  u_DoublePointsAggregator,
  u_InternalPerformanceCounterFake,
  u_GeometryProjectedFactory,
  u_GeometryLonLatFactory;

{ TestGeometryLonLatLineBuilderSimple }

procedure TestGeometryLonLatLineBuilderSimple.SetUp;
var
  VHashFunction: IHashFunction;
  VFactory: IGeometryLonLatFactory;
begin
  FPoints := TDoublePointsAggregator.Create;
  VHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      TInternalPerformanceCounterFake.Create
    );
  VFactory := TGeometryLonLatFactory.Create(VHashFunction);
  FBuilder := VFactory.MakeLineBuilder;
end;

procedure TestGeometryLonLatLineBuilderSimple.CreateLonLatPathNoLines;
var
  VResult: IGeometryLonLatLine;
begin
  VResult := FBuilder.MakeStaticAndClear;
  CheckNull(VResult);
end;

procedure TestGeometryLonLatLineBuilderSimple.CreateLonLatPathSimple;
var
  VResult: IGeometryLonLatSingleLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  i: integer;
begin
  FPoints.Clear;
  FPoints.Add(DoublePoint(0, 1));
  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FBuilder.AddLine(FPoints.MakeStaticCopy);
  VResult := FBuilder.MakeStaticAndClear as IGeometryLonLatSingleLine;
  CheckNotNull(VResult);
  CheckEquals(3, VResult.Count);
  VEnum := VResult.GetEnum;
  CheckNotNull(VEnum);
  i := 0;
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestGeometryLonLatLineBuilderSimple.CreateLonLatPathTwoLines;
var
  VResult: IGeometryLonLatMultiLine;
  VLine: IGeometryLonLatSingleLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  i: integer;
begin
  FPoints.Clear;
  FPoints.Add(DoublePoint(0, 1));
  FBuilder.AddLine(FPoints.MakeStaticAndClear);
  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FBuilder.AddLine(FPoints.MakeStaticCopy);

  VResult := FBuilder.MakeStaticAndClear as IGeometryLonLatMultiLine;
  CheckNotNull(VResult);
  CheckEquals(2, VResult.Count);

  VLine := VResult.Item[0];
  CheckNotNull(VLine);
  CheckEquals(1, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(0, 1)));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[1];
  CheckNotNull(VLine);
  CheckEquals(2, VLine.Count);
  VEnum := VLine.GetEnum;
  i := 0;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestGeometryLonLatPolygonBuilderSimple.SetUp;
var
  VHashFunction: IHashFunction;
  VFactory: IGeometryLonLatFactory;
begin
  FPoints := TDoublePointsAggregator.Create;
  VHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      TInternalPerformanceCounterFake.Create
    );
  VFactory := TGeometryLonLatFactory.Create(VHashFunction);
  FBuilder := VFactory.MakePolygonBuilder;
end;

procedure TestGeometryLonLatPolygonBuilderSimple.CreateLonLatPolygonNoLines;
var
  VResult: IGeometryLonLatPolygon;
begin
  VResult := FBuilder.MakeStaticAndClear;
  CheckNull(VResult);
end;

procedure TestGeometryLonLatPolygonBuilderSimple.CreateLonLatPolygonSimple;
var
  VResult: IGeometryLonLatSinglePolygon;
  VLine: IGeometryLonLatContour;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  i: integer;
begin
  FPoints.Clear;
  FPoints.Add(DoublePoint(0, 1));
  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FBuilder.AddOuter(FPoints.MakeStaticCopy);
  VResult := FBuilder.MakeStaticAndClear as IGeometryLonLatSinglePolygon;
  CheckNotNull(VResult);
  CheckEquals(0, VResult.HoleCount);
  VLine := VResult.OuterBorder;
  CheckNotNull(VLine);
  CheckEquals(3, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  i := 0;
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[0]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestGeometryLonLatPolygonBuilderSimple.CreateLonLatPolygonTwoLines;
var
  VResult: IGeometryLonLatMultiPolygon;
  VLine: IGeometryLonLatSinglePolygon;
  VContour: IGeometryLonLatContour;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  i: integer;
begin
  FPoints.Clear;
  FPoints.Add(DoublePoint(0, 1));
  FBuilder.AddOuter(FPoints.MakeStaticAndClear);
  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FBuilder.AddOuter(FPoints.MakeStaticCopy);
  VResult := FBuilder.MakeStaticAndClear as IGeometryLonLatMultiPolygon;
  CheckNotNull(VResult);
  CheckEquals(2, VResult.Count);

  VLine := VResult.Item[0];
  CheckNotNull(VLine);
  CheckEquals(0, VLine.HoleCount);
  VContour := VLine.OuterBorder;
  CheckNotNull(VContour);
  CheckEquals(1, VContour.Count);

  VEnum := VContour.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(0, 1)));
  CheckFalse(VEnum.Next(VPoint));

  VLine := VResult.Item[1];
  CheckNotNull(VLine);
  CheckEquals(0, VLine.HoleCount);
  VContour := VLine.OuterBorder;
  CheckNotNull(VContour);
  CheckEquals(2, VContour.Count);

  VEnum := VContour.GetEnum;
  CheckNotNull(VEnum);
  i := 0;
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  i := 0;
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestGeometryProjectedLineBuilderSimple.SetUp;
var
  VFactory: IGeometryProjectedFactory;
begin
  inherited;
  FPoints := TDoublePointsAggregator.Create;
  VFactory := TGeometryProjectedFactory.Create;
  FBuilder := VFactory.MakeLineBuilder;
end;

procedure TestGeometryProjectedLineBuilderSimple.CreateProjectedPathNoLines;
var
  VResult: IGeometryProjectedLine;
begin
  VResult := FBuilder.MakeStaticAndClear;
  CheckNull(VResult);
end;

procedure TestGeometryProjectedLineBuilderSimple.CreateProjectedPathSimple;
var
  VResult: IGeometryProjectedSingleLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  i: Integer;
begin
  FPoints.Clear;
  FPoints.Add(DoublePoint(0, 1));
  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FBuilder.AddLine(DoubleRect(0, 0, 1, 1), FPoints.MakeStaticCopy);
  VResult := FBuilder.MakeStaticAndClear as IGeometryProjectedSingleLine;
  CheckNotNull(VResult);
  CheckEquals(3, VResult.Count);
  VEnum := VResult.GetEnum;
  CheckNotNull(VEnum);
  i := 0;
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestGeometryProjectedLineBuilderSimple.CreateProjectedPathTwoLines;
var
  VResult: IGeometryProjectedMultiLine;
  VLine: IGeometryProjectedSingleLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  i: Integer;
begin
  FPoints.Clear;

  FPoints.Add(DoublePoint(0, 1));
  FBuilder.AddLine(DoubleRect(0, 0, 1, 1), FPoints.MakeStaticAndClear);

  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FBuilder.AddLine(DoubleRect(0, 0, 1, 1), FPoints.MakeStaticCopy);
  VResult := FBuilder.MakeStaticAndClear as IGeometryProjectedMultiLine;

  CheckNotNull(VResult);
  CheckEquals(2, VResult.Count);

  VLine := VResult.Item[0];
  CheckEquals(1, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(0, 1)));
  CheckFalse(VEnum.Next(VPoint));

  VLine := VResult.Item[1];
  CheckNotNull(VLine);
  CheckEquals(2, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  i := 0;
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestGeometryProjectedPolygonBuilderSimple.SetUp;
var
  VFactory: IGeometryProjectedFactory;
begin
  inherited;
  FPoints := TDoublePointsAggregator.Create;
  VFactory := TGeometryProjectedFactory.Create;
  FBuilder := VFactory.MakePolygonBuilder;
end;

procedure TestGeometryProjectedPolygonBuilderSimple.CreateProjectedPolygonNoLines;
var
  VResult: IGeometryProjectedPolygon;
begin
  VResult := FBuilder.MakeStaticAndClear;
  CheckNull(VResult);
end;

procedure TestGeometryProjectedPolygonBuilderSimple.CreateProjectedPolygonSimple;
var
  VResult: IGeometryProjectedSinglePolygon;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  i: Integer;
begin
  FPoints.Clear;
  FPoints.Add(DoublePoint(0, 1));
  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FBuilder.AddOuter(DoubleRect(0, 0, 1, 1), FPoints.MakeStaticCopy);
  VResult := FBuilder.MakeStaticAndClear as IGeometryProjectedSinglePolygon;
  CheckNotNull(VResult);
  CheckEquals(3, VResult.OuterBorder.Count);
  VEnum := VResult.OuterBorder.GetEnum;
  CheckNotNull(VEnum);
  i := 0;
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  i := 0;
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestGeometryProjectedPolygonBuilderSimple.CreateProjectedPolygonTwoLines;
var
  VResult: IGeometryProjectedMultiPolygon;
  VLine: IGeometryProjectedSinglePolygon;
  VContour: IGeometryProjectedContour;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  i: integer;
begin
  FPoints.Clear;
  FPoints.Add(DoublePoint(0, 1));
  FBuilder.AddOuter(DoubleRect(0, 1, 0, 1), FPoints.MakeStaticAndClear);
  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FBuilder.AddOuter(DoubleRect(0, 1, 0, 1), FPoints.MakeStaticCopy);
  VResult := FBuilder.MakeStaticAndClear as IGeometryProjectedMultiPolygon;
  CheckNotNull(VResult);
  CheckEquals(2, VResult.Count);

  VLine := VResult.Item[0];
  CheckNotNull(VLine);
  CheckEquals(0, VLine.HoleCount);
  VContour := VLine.OuterBorder;
  CheckNotNull(VContour);
  CheckEquals(1, VContour.Count);

  VEnum := VContour.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(0, 1)));
  CheckFalse(VEnum.Next(VPoint));

  VLine := VResult.Item[1];
  CheckNotNull(VLine);
  CheckEquals(0, VLine.HoleCount);
  VContour := VLine.OuterBorder;
  CheckNotNull(VContour);
  CheckEquals(2, VContour.Count);

  VEnum := VContour.GetEnum;
  CheckNotNull(VEnum);
  i := 0;
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  Inc(i);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  i := 0;
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints.Points[i]));
  CheckFalse(VEnum.Next(VPoint));
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestGeometryLonLatLineBuilderSimple.Suite);
  RegisterTest(TestGeometryLonLatPolygonBuilderSimple.Suite);
  RegisterTest(TestGeometryProjectedLineBuilderSimple.Suite);
  RegisterTest(TestGeometryProjectedPolygonBuilderSimple.Suite);
end.

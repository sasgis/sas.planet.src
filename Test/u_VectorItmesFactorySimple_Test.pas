unit u_VectorItmesFactorySimple_Test;

{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  TestFramework,
  t_GeoTypes,
  i_VectorGeometryProjectedFactory,
  i_GeometryLonLatFactory;

type
  TestTVectorItmesLonLatFactorySimple = class(TTestCase)
  private
    FFactory: IGeometryLonLatFactory;
    FPoints: array of TDoublePoint;
  protected
    procedure SetUp; override;
  published
    procedure CreateLonLatPathSimple;
    procedure CreateLonLatPathTwoLines;
    procedure CreateLonLatPathNoLines;

    procedure CreateLonLatPolygonSimple;
    procedure CreateLonLatPolygonTwoLines;
    procedure CreateLonLatPolygonNoLines;
  end;

  TestTVectorItmesProjectedFactorySimple = class(TTestCase)
  private
    FFactory: IVectorGeometryProjectedFactory;
    FPoints: array of TDoublePoint;
  protected
    procedure SetUp; override;
  published
    procedure CreateProjectedPathSimple;
    procedure CreateProjectedPathTwoLines;
    procedure CreateProjectedPathNoLines;

    procedure CreateProjectedPolygonSimple;
    procedure CreateProjectedPolygonTwoLines;
    procedure CreateProjectedPolygonNoLines;
  end;

implementation

uses
  i_GeometryLonLat,
  i_ProjectionInfo,
  i_HashFunction,
  i_VectorItemProjected,
  i_EnumDoublePoint,
  u_GeoFun,
  u_ProjectionInfo,
  u_HashFunctionCityHash,
  u_HashFunctionWithCounter,
  u_InternalPerformanceCounterFake,
  u_VectorGeometryProjectedFactory,
  u_GeometryLonLatFactory;

{ TestTVectorItmesFactorySimple }

procedure TestTVectorItmesLonLatFactorySimple.SetUp;
var
  VHashFunction: IHashFunction;
begin
  VHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      TInternalPerformanceCounterFake.Create
    );
  FFactory := TGeometryLonLatFactory.Create(VHashFunction);
end;

procedure TestTVectorItmesLonLatFactorySimple.CreateLonLatPathNoLines;
var
  VResult: IGeometryLonLatMultiLine;
begin
  VResult := FFactory.CreateLonLatMultiLine(nil, 0);
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 0);
  VResult := FFactory.CreateLonLatMultiLine(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 1);
  FPoints[0] := CEmptyDoublePoint;
  VResult := FFactory.CreateLonLatMultiLine(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 2);
  FPoints[0] := CEmptyDoublePoint;
  FPoints[1] := CEmptyDoublePoint;
  VResult := FFactory.CreateLonLatMultiLine(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
end;

procedure TestTVectorItmesLonLatFactorySimple.CreateLonLatPathSimple;
var
  VResult: IGeometryLonLatMultiLine;
  VLine: IGeometryLonLatLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
begin
  SetLength(FPoints, 3);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := DoublePoint(1, 1);
  FPoints[2] := DoublePoint(1, 0);
  VResult := FFactory.CreateLonLatMultiLine(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(1, VResult.Count);
  VEnum := VResult.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[0];
  CheckNotNull(VLine);
  CheckEquals(3, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestTVectorItmesLonLatFactorySimple.CreateLonLatPathTwoLines;
var
  VResult: IGeometryLonLatMultiLine;
  VLine: IGeometryLonLatLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
begin
  SetLength(FPoints, 4);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := CEmptyDoublePoint;
  FPoints[2] := DoublePoint(1, 1);
  FPoints[3] := DoublePoint(1, 0);
  VResult := FFactory.CreateLonLatMultiLine(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(2, VResult.Count);
  VEnum := VResult.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[3]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[0];
  CheckNotNull(VLine);
  CheckEquals(1, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[1];
  CheckNotNull(VLine);
  CheckEquals(2, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[3]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestTVectorItmesLonLatFactorySimple.CreateLonLatPolygonNoLines;
var
  VResult: IGeometryLonLatMultiPolygon;
begin
  VResult := FFactory.CreateLonLatMultiPolygon(nil, 0);
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 0);
  VResult := FFactory.CreateLonLatMultiPolygon(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 1);
  FPoints[0] := CEmptyDoublePoint;
  VResult := FFactory.CreateLonLatMultiPolygon(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 2);
  FPoints[0] := CEmptyDoublePoint;
  FPoints[1] := CEmptyDoublePoint;
  VResult := FFactory.CreateLonLatMultiPolygon(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
end;

procedure TestTVectorItmesLonLatFactorySimple.CreateLonLatPolygonSimple;
var
  VResult: IGeometryLonLatMultiPolygon;
  VLine: IGeometryLonLatPolygon;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
begin
  SetLength(FPoints, 3);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := DoublePoint(1, 1);
  FPoints[2] := DoublePoint(1, 0);
  VResult := FFactory.CreateLonLatMultiPolygon(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(1, VResult.Count);
  VEnum := VResult.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[0];
  CheckNotNull(VLine);
  CheckEquals(3, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestTVectorItmesLonLatFactorySimple.CreateLonLatPolygonTwoLines;
var
  VResult: IGeometryLonLatMultiPolygon;
  VLine: IGeometryLonLatPolygon;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
begin
  SetLength(FPoints, 4);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := CEmptyDoublePoint;
  FPoints[2] := DoublePoint(1, 1);
  FPoints[3] := DoublePoint(1, 0);
  VResult := FFactory.CreateLonLatMultiPolygon(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(2, VResult.Count);
  VEnum := VResult.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[3]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[0];
  CheckNotNull(VLine);
  CheckEquals(1, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[1];
  CheckNotNull(VLine);
  CheckEquals(2, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[3]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestTVectorItmesProjectedFactorySimple.SetUp;
begin
  inherited;
  FFactory := TVectorGeometryProjectedFactory.Create;
end;

procedure TestTVectorItmesProjectedFactorySimple.CreateProjectedPathNoLines;
var
  VResult: IProjectedPath;
begin
  VResult := FFactory.CreateProjectedPath(nil, nil, 0);
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 0);
  VResult := FFactory.CreateProjectedPath(nil, @FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 1);
  FPoints[0] := CEmptyDoublePoint;
  VResult := FFactory.CreateProjectedPath(nil, @FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 2);
  FPoints[0] := CEmptyDoublePoint;
  FPoints[1] := CEmptyDoublePoint;
  VResult := FFactory.CreateProjectedPath(nil, @FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
end;

procedure TestTVectorItmesProjectedFactorySimple.CreateProjectedPathSimple;
var
  VResult: IProjectedPath;
  VLine: IProjectedPathLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
begin
  SetLength(FPoints, 3);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := DoublePoint(1, 1);
  FPoints[2] := DoublePoint(1, 0);
  VResult := FFactory.CreateProjectedPath(nil, @FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(1, VResult.Count);
  VEnum := VResult.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[0];
  CheckNotNull(VLine);
  CheckEquals(3, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestTVectorItmesProjectedFactorySimple.CreateProjectedPathTwoLines;
var
  VResult: IProjectedPath;
  VLine: IProjectedPathLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  VProjection: IProjectionInfo;
begin
  VProjection := TProjectionInfo.Create(0, nil, 0);
  SetLength(FPoints, 4);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := CEmptyDoublePoint;
  FPoints[2] := DoublePoint(1, 1);
  FPoints[3] := DoublePoint(1, 0);
  VResult := FFactory.CreateProjectedPath(VProjection, @FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(2, VResult.Count);
  VEnum := VResult.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[3]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[0];
  CheckNotNull(VLine);
  CheckEquals(1, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[1];
  CheckNotNull(VLine);
  CheckEquals(2, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[3]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestTVectorItmesProjectedFactorySimple.CreateProjectedPolygonNoLines;
var
  VResult: IProjectedPolygon;
begin
  VResult := FFactory.CreateProjectedPolygon(nil, nil, 0);
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 0);
  VResult := FFactory.CreateProjectedPolygon(nil, @FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 1);
  FPoints[0] := CEmptyDoublePoint;
  VResult := FFactory.CreateProjectedPolygon(nil, @FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 2);
  FPoints[0] := CEmptyDoublePoint;
  FPoints[1] := CEmptyDoublePoint;
  VResult := FFactory.CreateProjectedPolygon(nil, @FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
end;

procedure TestTVectorItmesProjectedFactorySimple.CreateProjectedPolygonSimple;
var
  VResult: IProjectedPolygon;
  VLine: IProjectedPolygonLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
begin
  SetLength(FPoints, 3);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := DoublePoint(1, 1);
  FPoints[2] := DoublePoint(1, 0);
  VResult := FFactory.CreateProjectedPolygon(nil, @FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(1, VResult.Count);
  VEnum := VResult.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[0];
  CheckNotNull(VLine);
  CheckEquals(3, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckFalse(VEnum.Next(VPoint));
end;

procedure TestTVectorItmesProjectedFactorySimple.CreateProjectedPolygonTwoLines;
var
  VResult: IProjectedPolygon;
  VLine: IProjectedPolygonLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  VProjection: IProjectionInfo;
begin
  VProjection := TProjectionInfo.Create(0, nil, 0);
  SetLength(FPoints, 4);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := CEmptyDoublePoint;
  FPoints[2] := DoublePoint(1, 1);
  FPoints[3] := DoublePoint(1, 0);
  VResult := FFactory.CreateProjectedPolygon(VProjection, @FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(2, VResult.Count);
  VEnum := VResult.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[1]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[3]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[0];
  CheckNotNull(VLine);
  CheckEquals(1, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[0]));
  CheckFalse(VEnum.Next(VPoint));
  VLine := VResult.Item[1];
  CheckNotNull(VLine);
  CheckEquals(2, VLine.Count);
  VEnum := VLine.GetEnum;
  CheckNotNull(VEnum);
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[3]));
  CheckTrue(VEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, FPoints[2]));
  CheckFalse(VEnum.Next(VPoint));
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTVectorItmesLonLatFactorySimple.Suite);
  RegisterTest(TestTVectorItmesProjectedFactorySimple.Suite);
end.

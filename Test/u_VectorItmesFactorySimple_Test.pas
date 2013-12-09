unit u_VectorItmesFactorySimple_Test;

{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  TestFramework,
  t_GeoTypes,
  i_VectorItemsFactory;

type
  TestTVectorItmesFactorySimple = class(TTestCase)
  private
    FFactory: IVectorItemsFactory;
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
  u_VectorItemsFactorySimple;

{ TestTVectorItmesFactorySimple }

procedure TestTVectorItmesFactorySimple.SetUp;
var
  VHashFunction: IHashFunction;
begin
  VHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      TInternalPerformanceCounterFake.Create
    );
  FFactory := TVectorItemsFactorySimple.Create(VHashFunction);
end;

procedure TestTVectorItmesFactorySimple.CreateLonLatPathNoLines;
var
  VResult: ILonLatPath;
begin
  VResult := FFactory.CreateLonLatPath(nil, 0);
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 0);
  VResult := FFactory.CreateLonLatPath(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 1);
  FPoints[0] := CEmptyDoublePoint;
  VResult := FFactory.CreateLonLatPath(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 2);
  FPoints[0] := CEmptyDoublePoint;
  FPoints[1] := CEmptyDoublePoint;
  VResult := FFactory.CreateLonLatPath(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
end;

procedure TestTVectorItmesFactorySimple.CreateLonLatPathSimple;
var
  VResult: ILonLatPath;
  VLine: ILonLatPathLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
begin
  SetLength(FPoints, 3);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := DoublePoint(1, 1);
  FPoints[2] := DoublePoint(1, 0);
  VResult := FFactory.CreateLonLatPath(@FPoints[0], Length(FPoints));
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

procedure TestTVectorItmesFactorySimple.CreateLonLatPathTwoLines;
var
  VResult: ILonLatPath;
  VLine: ILonLatPathLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
begin
  SetLength(FPoints, 4);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := CEmptyDoublePoint;
  FPoints[2] := DoublePoint(1, 1);
  FPoints[3] := DoublePoint(1, 0);
  VResult := FFactory.CreateLonLatPath(@FPoints[0], Length(FPoints));
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

procedure TestTVectorItmesFactorySimple.CreateLonLatPolygonNoLines;
var
  VResult: ILonLatPolygon;
begin
  VResult := FFactory.CreateLonLatPolygon(nil, 0);
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 0);
  VResult := FFactory.CreateLonLatPolygon(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 1);
  FPoints[0] := CEmptyDoublePoint;
  VResult := FFactory.CreateLonLatPolygon(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
  SetLength(FPoints, 2);
  FPoints[0] := CEmptyDoublePoint;
  FPoints[1] := CEmptyDoublePoint;
  VResult := FFactory.CreateLonLatPolygon(@FPoints[0], Length(FPoints));
  CheckNotNull(VResult);
  CheckEquals(0, VResult.Count);
  CheckNotNull(VResult.GetEnum);
end;

procedure TestTVectorItmesFactorySimple.CreateLonLatPolygonSimple;
var
  VResult: ILonLatPolygon;
  VLine: ILonLatPolygonLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
begin
  SetLength(FPoints, 3);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := DoublePoint(1, 1);
  FPoints[2] := DoublePoint(1, 0);
  VResult := FFactory.CreateLonLatPolygon(@FPoints[0], Length(FPoints));
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

procedure TestTVectorItmesFactorySimple.CreateLonLatPolygonTwoLines;
var
  VResult: ILonLatPolygon;
  VLine: ILonLatPolygonLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
begin
  SetLength(FPoints, 4);
  FPoints[0] := DoublePoint(0, 1);
  FPoints[1] := CEmptyDoublePoint;
  FPoints[2] := DoublePoint(1, 1);
  FPoints[3] := DoublePoint(1, 0);
  VResult := FFactory.CreateLonLatPolygon(@FPoints[0], Length(FPoints));
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

procedure TestTVectorItmesFactorySimple.CreateProjectedPathNoLines;
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

procedure TestTVectorItmesFactorySimple.CreateProjectedPathSimple;
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

procedure TestTVectorItmesFactorySimple.CreateProjectedPathTwoLines;
var
  VResult: IProjectedPath;
  VLine: IProjectedPathLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  VProjection: IProjectionInfo;
begin
  VProjection := TProjectionInfo.Create(nil, 0);
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

procedure TestTVectorItmesFactorySimple.CreateProjectedPolygonNoLines;
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

procedure TestTVectorItmesFactorySimple.CreateProjectedPolygonSimple;
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

procedure TestTVectorItmesFactorySimple.CreateProjectedPolygonTwoLines;
var
  VResult: IProjectedPolygon;
  VLine: IProjectedPolygonLine;
  VPoint: TDoublePoint;
  VEnum: IEnumDoublePoint;
  VProjection: IProjectionInfo;
begin
  VProjection := TProjectionInfo.Create(nil, 0);
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
  RegisterTest(TestTVectorItmesFactorySimple.Suite);
end.

unit u_GeometrySaveLoadWKB_Test;

{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  Classes,
  TestFramework,
  t_GeoTypes,
  i_DoublePointsAggregator,
  i_GeometryLonLat,
  i_GeometryToStream,
  i_GeometryFromStream,
  i_GeometryLonLatFactory;

type
  TestGeometrySaveLoadWKB = class(TTestCase)
  private
    FFactory: IGeometryLonLatFactory;
    FSaver: IGeometryToStream;
    FLoader: IGeometryFromStream;
    FPoints: IDoublePointsAggregator;
    FStream: TStream;
    FGeomerty: IGeometryLonLat;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPoint;
    procedure TestSingleLine;
    procedure TestSinglePolygon;
    procedure TestMultiLine;
    procedure TestMultiPolygon;
    procedure TestLoadMySQLPointBinary;
    procedure TestLoadMySQLSinglePolygonBinary;
    procedure TestLoadMySQLSingleLineBinary;
  end;
implementation

uses
  SysUtils,
  StrUtils,
  i_HashFunction,
  u_HashFunctionCityHash,
  u_HashFunctionWithCounter,
  u_InternalPerformanceCounterFake,
  u_GeometryLonLatFactory,
  u_DoublePointsAggregator,
  u_GeometryFromWKB,
  u_GeometryToWKB,
  u_GeoFunc;

{ TestGeometrySaveLoad }

procedure TestGeometrySaveLoadWKB.SetUp;
var
  VHashFunction: IHashFunction;
begin
  inherited;
  VHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      TInternalPerformanceCounterFake.Create
    );
  FFactory := TGeometryLonLatFactory.Create(VHashFunction);
  FLoader := TGeometryFromWKB.Create(FFactory);
  FSaver := TGeometryToWKB.Create;
  FStream := TMemoryStream.Create;
  FPoints := TDoublePointsAggregator.Create;
end;

procedure TestGeometrySaveLoadWKB.TearDown;
begin
  inherited;
  FreeAndNil(FStream);
end;

procedure TestGeometrySaveLoadWKB.TestLoadMySQLPointBinary;
var
  VStr: string;
  VMemStream: TMemoryStream;
  VGeometry: IGeometryLonLat;
begin
  // 'POINT(1 4)'
  VStr := LowerCase('0101000000000000000000F03F0000000000001040');
  VMemStream := TMemoryStream.Create;
  try
    VMemStream.SetSize(Length(VStr) div 2);
    HexToBin(PChar(VStr), VMemStream.Memory, VMemStream.Size);
    VMemStream.Position := 0;
    FGeomerty := FFactory.CreateLonLatPoint(DoublePoint(1, 4));
    VGeometry := FLoader.Parse(VMemStream);
    CheckNotNull(VGeometry, 'Must be not null');
    CheckTrue(FGeomerty.IsSameGeometry(VGeometry));
    CheckTrue(VGeometry.IsSameGeometry(FGeomerty));
  finally
    VMemStream.Free;
  end;
end;

procedure TestGeometrySaveLoadWKB.TestLoadMySQLSingleLineBinary;
var
  VStr: string;
  VMemStream: TMemoryStream;
  VGeometry: IGeometryLonLat;
begin
  // 'LINESTRING(0 0,10 10,20 25,50 60)'
  VStr := LowerCase('01020000000400000000000000000000000000000000000000000000000000244000000000000024400000000000003440000000000000394000000000000049400000000000004E40');
  VMemStream := TMemoryStream.Create;
  try
    VMemStream.SetSize(Length(VStr) div 2);
    HexToBin(PChar(VStr), VMemStream.Memory, VMemStream.Size);
    VMemStream.Position := 0;
    VGeometry := FLoader.Parse(VMemStream);
    CheckNotNull(VGeometry, 'Must be not null');

    FPoints.Add(DoublePoint(0, 0));
    FPoints.Add(DoublePoint(10, 10));
    FPoints.Add(DoublePoint(20, 25));
    FPoints.Add(DoublePoint(50, 60));
    FGeomerty := FFactory.CreateLonLatMultiLine(FPoints.Points, FPoints.Count);

    CheckTrue(FGeomerty.IsSameGeometry(VGeometry));
    CheckTrue(VGeometry.IsSameGeometry(FGeomerty));
  finally
    VMemStream.Free;
  end;
end;

procedure TestGeometrySaveLoadWKB.TestLoadMySQLSinglePolygonBinary;
var
  VStr: string;
  VMemStream: TMemoryStream;
  VGeometry: IGeometryLonLat;
begin
  // 'POLYGON((0 0,10 0,10 10,0 10,0 0))'
  VStr := LowerCase('010300000001000000050000000000000000000000000000000000000000000000000024400000000000000000000000000000244000000000000024400000000000000000000000000000244000000000000000000000000000000000');
  VMemStream := TMemoryStream.Create;
  try
    VMemStream.SetSize(Length(VStr) div 2);
    HexToBin(PChar(VStr), VMemStream.Memory, VMemStream.Size);
    VMemStream.Position := 0;
    VGeometry := FLoader.Parse(VMemStream);
    CheckNotNull(VGeometry, 'Must be not null');

    FPoints.Add(DoublePoint(0, 0));
    FPoints.Add(DoublePoint(10, 0));
    FPoints.Add(DoublePoint(10, 10));
    FPoints.Add(DoublePoint(0, 10));
    FGeomerty := FFactory.CreateLonLatMultiPolygon(FPoints.Points, FPoints.Count);

    CheckTrue(FGeomerty.IsSameGeometry(VGeometry));
    CheckTrue(VGeometry.IsSameGeometry(FGeomerty));
  finally
    VMemStream.Free;
  end;
end;

procedure TestGeometrySaveLoadWKB.TestMultiLine;
var
  VGeometry: IGeometryLonLat;
begin
  FPoints.Add(DoublePoint(0, 1));
  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FPoints.Add(CEmptyDoublePoint);
  FPoints.Add(DoublePoint(2, 2));
  FPoints.Add(DoublePoint(3, 3));
  FPoints.Add(DoublePoint(4, 4));
  FGeomerty := FFactory.CreateLonLatMultiLine(FPoints.Points, FPoints.Count);
  FSaver.Save(FGeomerty, FStream);
  FStream.Position := 0;
  VGeometry := FLoader.Parse(FStream);
  CheckNotNull(VGeometry, 'Must be not null');
  CheckTrue(FGeomerty.IsSameGeometry(VGeometry));
  CheckTrue(VGeometry.IsSameGeometry(FGeomerty));
end;

procedure TestGeometrySaveLoadWKB.TestMultiPolygon;
var
  VGeometry: IGeometryLonLat;
begin
  FPoints.Add(DoublePoint(0, 1));
  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FPoints.Add(CEmptyDoublePoint);
  FPoints.Add(DoublePoint(2, 2));
  FPoints.Add(DoublePoint(3, 3));
  FPoints.Add(DoublePoint(4, 2));
  FGeomerty := FFactory.CreateLonLatMultiPolygon(FPoints.Points, FPoints.Count);
  FSaver.Save(FGeomerty, FStream);
  FStream.Position := 0;
  VGeometry := FLoader.Parse(FStream);
  CheckNotNull(VGeometry, 'Must be not null');
  CheckTrue(FGeomerty.IsSameGeometry(VGeometry));
  CheckTrue(VGeometry.IsSameGeometry(FGeomerty));
end;

procedure TestGeometrySaveLoadWKB.TestPoint;
var
  VGeometry: IGeometryLonLat;
begin
  FGeomerty := FFactory.CreateLonLatPoint(DoublePoint(10, 15));
  FSaver.Save(FGeomerty, FStream);
  FStream.Position := 0;
  VGeometry := FLoader.Parse(FStream);
  CheckNotNull(VGeometry, 'Must be not null');
  CheckTrue(FGeomerty.IsSameGeometry(VGeometry));
  CheckTrue(VGeometry.IsSameGeometry(FGeomerty));
end;

procedure TestGeometrySaveLoadWKB.TestSingleLine;
var
  VGeometry: IGeometryLonLat;
begin
  FPoints.Add(DoublePoint(0, 1));
  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FGeomerty := FFactory.CreateLonLatMultiLine(FPoints.Points, FPoints.Count);
  FSaver.Save(FGeomerty, FStream);
  FStream.Position := 0;
  VGeometry := FLoader.Parse(FStream);
  CheckNotNull(VGeometry, 'Must be not null');
  CheckTrue(FGeomerty.IsSameGeometry(VGeometry));
  CheckTrue(VGeometry.IsSameGeometry(FGeomerty));
end;

procedure TestGeometrySaveLoadWKB.TestSinglePolygon;
var
  VGeometry: IGeometryLonLat;
begin
  FPoints.Add(DoublePoint(0, 1));
  FPoints.Add(DoublePoint(1, 1));
  FPoints.Add(DoublePoint(1, 0));
  FGeomerty := FFactory.CreateLonLatMultiPolygon(FPoints.Points, FPoints.Count);
  FSaver.Save(FGeomerty, FStream);
  FStream.Position := 0;
  VGeometry := FLoader.Parse(FStream);
  CheckNotNull(VGeometry, 'Must be not null');
  CheckTrue(FGeomerty.IsSameGeometry(VGeometry));
  CheckTrue(VGeometry.IsSameGeometry(FGeomerty));
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestGeometrySaveLoadWKB.Suite);
end.

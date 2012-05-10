unit u_EnumDoublePointClosePoly_Test;

{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  TestFramework,
  t_GeoTypes,
  i_EnumDoublePoint;

type
  TestTEnumDoublePointClosePoly = class(TTestCase)
  private
    function PrepareEnumByArray(AData: TArrayOfDoublePoint): IEnumDoublePoint;
  published
    procedure NoPoints;
    procedure OnePoint;
    procedure SimpleIfNeedAddPoint;
    procedure SimpleIfNoAddPoint;
    procedure WithEmtyAtEnd;
    procedure TwoPoly;
  end;

implementation

uses
  u_GeoFun,
  u_EnumDoublePointClosePoly,
  u_EnumDoublePointsByArray;

function TestTEnumDoublePointClosePoly.PrepareEnumByArray(
  AData: TArrayOfDoublePoint): IEnumDoublePoint;
var
  VDataEnum: IEnumDoublePoint;
begin
  VDataEnum := TEnumDoublePointsByArray.Create(@AData[0], Length(AData));
  Result := TEnumDoublePointClosePoly.Create(VDataEnum);
end;

procedure TestTEnumDoublePointClosePoly.NoPoints;
var
  VData: TArrayOfDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 0);
  VTestEnum := PrepareEnumByArray(VData);
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClosePoly.OnePoint;
var
  VData: TArrayOfDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 1);
  VData[0] := DoublePoint(1, 1);

  VTestEnum := PrepareEnumByArray(VData);

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClosePoly.SimpleIfNeedAddPoint;
var
  VData: TArrayOfDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 3);
  VData[0] := DoublePoint(1, 1);
  VData[1] := DoublePoint(0, 1);
  VData[2] := DoublePoint(1, 0);

  VTestEnum := PrepareEnumByArray(VData);

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClosePoly.SimpleIfNoAddPoint;
var
  VData: TArrayOfDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(1, 1);
  VData[1] := DoublePoint(0, 1);
  VData[2] := DoublePoint(1, 0);
  VData[3] := DoublePoint(1, 1);

  VTestEnum := PrepareEnumByArray(VData);

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClosePoly.TwoPoly;
var
  VData: TArrayOfDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 5);
  VData[0] := DoublePoint(1, 1);
  VData[1] := DoublePoint(0, 1);
  VData[2] := DoublePoint(1, 0);
  VData[3] := CEmptyDoublePoint;
  VData[4] := DoublePoint(4, 4);

  VTestEnum := PrepareEnumByArray(VData);

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(PointIsEmpty(VPoint));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[4]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClosePoly.WithEmtyAtEnd;
var
  VData: TArrayOfDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(1, 1);
  VData[1] := DoublePoint(0, 1);
  VData[2] := DoublePoint(1, 0);
  VData[3] := CEmptyDoublePoint;

  VTestEnum := PrepareEnumByArray(VData);

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  VTestEnum.Next(VPoint);
  CheckFalse(VTestEnum.Next(VPoint));
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTEnumDoublePointClosePoly.Suite);
end.

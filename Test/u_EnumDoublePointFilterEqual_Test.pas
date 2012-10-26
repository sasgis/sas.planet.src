unit u_EnumDoublePointFilterEqual_Test;

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
  TestTEnumDoublePointFilterEqual = class(TTestCase)
  private
    function PrepareEnumByArray(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IEnumDoublePoint;
  published
    procedure NoPoints;
    procedure OnePoint;
    procedure SimpleIfNeedDeletePoint;
    procedure SimpleIfNoDeletePoint;
    procedure DeleteThreePoints;
    procedure DeleteTwoPoints;
    procedure WithEmtyAtEnd;
    procedure TwoLines;
  end;

implementation

uses
  u_GeoFun,
  u_EnumDoublePointFilterEqual,
  u_EnumDoublePointsByArray;

{ TestTEnumDoublePointFilterEqual }

function TestTEnumDoublePointFilterEqual.PrepareEnumByArray(
  const APoints: PDoublePointArray;
  ACount: Integer
): IEnumDoublePoint;
var
  VDataEnum: IEnumDoublePoint;
begin
  VDataEnum := TEnumDoublePointsByArray.Create(APoints, ACount);
  Result := TEnumDoublePointFilterEqual.Create(VDataEnum);
end;

procedure TestTEnumDoublePointFilterEqual.DeleteThreePoints;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(2, 2);
  VData[1] := DoublePoint(2.1, 2.1);
  VData[2] := DoublePoint(2.4, 2.3);
  VData[3] := DoublePoint(2.1, 2.1);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointFilterEqual.DeleteTwoPoints;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(2, 2);
  VData[1] := DoublePoint(2.1, 2.1);
  VData[2] := DoublePoint(2.4, 2.3);
  VData[3] := DoublePoint(2, 0);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[3]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointFilterEqual.NoPoints;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 0);
  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointFilterEqual.OnePoint;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 1);
  VData[0] := DoublePoint(2, 2);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointFilterEqual.SimpleIfNeedDeletePoint;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 3);
  VData[0] := DoublePoint(2, 2);
  VData[1] := DoublePoint(2, 2);
  VData[2] := DoublePoint(2, 0);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointFilterEqual.SimpleIfNoDeletePoint;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(2, 2);
  VData[1] := DoublePoint(0, 2);
  VData[2] := DoublePoint(2, 0);
  VData[3] := DoublePoint(2, 2);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[3]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointFilterEqual.TwoLines;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 5);
  VData[0] := DoublePoint(2, 2);
  VData[1] := DoublePoint(0, 2);
  VData[2] := DoublePoint(2, 0);
  VData[3] := CEmptyDoublePoint;
  VData[4] := DoublePoint(4, 4);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(PointIsEmpty(VPoint));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[4]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointFilterEqual.WithEmtyAtEnd;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(2, 2);
  VData[1] := DoublePoint(0, 2);
  VData[2] := DoublePoint(2, 0);
  VData[3] := CEmptyDoublePoint;

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  VTestEnum.Next(VPoint);
  CheckFalse(VTestEnum.Next(VPoint));
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTEnumDoublePointFilterEqual.Suite);
end.

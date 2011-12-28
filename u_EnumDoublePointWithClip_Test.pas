unit u_EnumDoublePointWithClip_Test;

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
  TestTEnumDoublePointClipByLeftBorder = class(TTestCase)
  private
    function PrepareEnumByArray(AData: TArrayOfDoublePoint): IEnumDoublePoint;
  published
    procedure FirstPointOut;
    procedure FirstPointOnLine;
    procedure LastPointOut;
    procedure LastPointOnLine;
    procedure AllPointsOut;
    procedure AllPointsIn;
    procedure SecondPointOut;
    procedure SecondPointOnLine;
    procedure TwoPointsIn;
  end;

implementation

uses
  u_GeoFun,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointsByArray;

{ TestTEnumDoublePointClipByLeftBorder }

function TestTEnumDoublePointClipByLeftBorder.PrepareEnumByArray(
  AData: TArrayOfDoublePoint): IEnumDoublePoint;
var
  VDataEnum: IEnumDoublePoint;
begin
  VDataEnum := TEnumDoublePointsByArray.Create(@AData[0], Length(AData));
  Result := TEnumDoublePointClipByLeftBorder.Create(1, VDataEnum);
end;

procedure TestTEnumDoublePointClipByLeftBorder.AllPointsIn;
var
  VData: TArrayOfDoublePoint;
  VTestEnum:  IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(2, 2);
  VData[1] := DoublePoint(2.1, 2.1);
  VData[2] := DoublePoint(2.4, 2.3);
  VData[3] := DoublePoint(2.1, 2.1);

  VTestEnum := PrepareEnumByArray(VData);

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

procedure TestTEnumDoublePointClipByLeftBorder.AllPointsOut;
var
  VData: TArrayOfDoublePoint;
  VTestEnum:  IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(0, 0);
  VData[1] := DoublePoint(0.1, 0.1);
  VData[2] := DoublePoint(0.4, 0.3);
  VData[3] := DoublePoint(0.1, 0.1);

  VTestEnum := PrepareEnumByArray(VData);

  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClipByLeftBorder.FirstPointOnLine;
var
  VData: TArrayOfDoublePoint;
  VTestEnum:  IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(1, 1);
  VData[1] := DoublePoint(2.1, 2.1);
  VData[2] := DoublePoint(2.4, 2.3);
  VData[3] := DoublePoint(2.1, 2.1);

  VTestEnum := PrepareEnumByArray(VData);

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

procedure TestTEnumDoublePointClipByLeftBorder.FirstPointOut;
var
  VData: TArrayOfDoublePoint;
  VTestEnum:  IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(0, 2);
  VData[1] := DoublePoint(2, 2);
  VData[2] := DoublePoint(2.4, 2.3);
  VData[3] := DoublePoint(2.1, 2.1);

  VTestEnum := PrepareEnumByArray(VData);

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(1, 2)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[3]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClipByLeftBorder.LastPointOnLine;
var
  VData: TArrayOfDoublePoint;
  VTestEnum:  IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(2, 2);
  VData[1] := DoublePoint(3, 3);
  VData[2] := DoublePoint(2, 1);
  VData[3] := DoublePoint(1, 1);

  VTestEnum := PrepareEnumByArray(VData);

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

procedure TestTEnumDoublePointClipByLeftBorder.LastPointOut;
var
  VData: TArrayOfDoublePoint;
  VTestEnum:  IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(2, 2);
  VData[1] := DoublePoint(3, 3);
  VData[2] := DoublePoint(2, 1);
  VData[3] := DoublePoint(0, 1);

  VTestEnum := PrepareEnumByArray(VData);

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(1, 1)));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClipByLeftBorder.TwoPointsIn;
var
  VData: TArrayOfDoublePoint;
  VTestEnum:  IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(0, 2);
  VData[1] := DoublePoint(2, 2);
  VData[2] := DoublePoint(2, 1);
  VData[3] := DoublePoint(0, 1);

  VTestEnum := PrepareEnumByArray(VData);

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(1, 2)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(1, 1)));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClipByLeftBorder.SecondPointOnLine;
var
  VData: TArrayOfDoublePoint;
  VTestEnum:  IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 3);
  VData[0] := DoublePoint(2, 0);
  VData[1] := DoublePoint(1, 1);
  VData[2] := DoublePoint(2, 2);

  VTestEnum := PrepareEnumByArray(VData);

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClipByLeftBorder.SecondPointOut;
var
  VData: TArrayOfDoublePoint;
  VTestEnum:  IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 3);
  VData[0] := DoublePoint(2, 0);
  VData[1] := DoublePoint(0, 2);
  VData[2] := DoublePoint(2, 4);

  VTestEnum := PrepareEnumByArray(VData);

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(1, 1)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(1, 3)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTEnumDoublePointClipByLeftBorder.Suite);
end.

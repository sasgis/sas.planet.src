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
    function PrepareEnumByArray(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IEnumDoublePoint;
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

  TestTEnumDoublePointClipByRect = class(TTestCase)
  private
    function PrepareEnumByArray(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IEnumDoublePoint;
  published
    procedure ClosedFirstPointOut;
    procedure ClosedSecondPointOut;
    procedure ClosedAllPointsOut;
    procedure ClosedAllPointsAround;
  end;

implementation

uses
  u_GeoFunc,
  u_EnumDoublePointClipInternal,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointsByArray;

{ TestTEnumDoublePointClipByLeftBorder }

function TestTEnumDoublePointClipByLeftBorder.PrepareEnumByArray(
  const APoints: PDoublePointArray;
  ACount: Integer
): IEnumDoublePoint;
var
  VDataEnum: IEnumDoublePoint;
begin
  VDataEnum := TEnumDoublePointsByArray.Create(APoints, ACount);
  Result := TEnumDoublePointClipByLeftBorder.Create(1, VDataEnum);
end;

procedure TestTEnumDoublePointClipByLeftBorder.AllPointsIn;
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
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(0, 0);
  VData[1] := DoublePoint(0.1, 0.1);
  VData[2] := DoublePoint(0.4, 0.3);
  VData[3] := DoublePoint(0.1, 0.1);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClipByLeftBorder.FirstPointOnLine;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(1, 1);
  VData[1] := DoublePoint(2.1, 2.1);
  VData[2] := DoublePoint(2.4, 2.3);
  VData[3] := DoublePoint(2.1, 2.1);

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

procedure TestTEnumDoublePointClipByLeftBorder.FirstPointOut;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(0, 2);
  VData[1] := DoublePoint(2, 2);
  VData[2] := DoublePoint(2.4, 2.3);
  VData[3] := DoublePoint(2.1, 2.1);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

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
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(2, 2);
  VData[1] := DoublePoint(3, 3);
  VData[2] := DoublePoint(2, 1);
  VData[3] := DoublePoint(1, 1);

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

procedure TestTEnumDoublePointClipByLeftBorder.LastPointOut;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(2, 2);
  VData[1] := DoublePoint(3, 3);
  VData[2] := DoublePoint(2, 1);
  VData[3] := DoublePoint(0, 1);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

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
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(0, 2);
  VData[1] := DoublePoint(2, 2);
  VData[2] := DoublePoint(2, 1);
  VData[3] := DoublePoint(0, 1);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

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
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 3);
  VData[0] := DoublePoint(2, 0);
  VData[1] := DoublePoint(1, 1);
  VData[2] := DoublePoint(2, 2);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

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
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 3);
  VData[0] := DoublePoint(2, 0);
  VData[1] := DoublePoint(0, 2);
  VData[2] := DoublePoint(2, 4);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

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

{ TestTEnumDoublePointClipByRect }

procedure TestTEnumDoublePointClipByRect.ClosedAllPointsAround;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 5);
  VData[0] := DoublePoint(1, 0);
  VData[1] := DoublePoint(8, 0);
  VData[2] := DoublePoint(10, 10);
  VData[3] := DoublePoint(0, 10);
  VData[4] := DoublePoint(1, 0);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(2, 0)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(7, 0)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(7, 5)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(2, 5)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(2, 0)));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClipByRect.ClosedAllPointsOut;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(1, 0);
  VData[1] := DoublePoint(1, 1);
  VData[2] := DoublePoint(0, 1);
  VData[3] := DoublePoint(1, 0);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClipByRect.ClosedFirstPointOut;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(1, 0);
  VData[1] := DoublePoint(4, 3);
  VData[2] := DoublePoint(3, 4);
  VData[3] := DoublePoint(1, 0);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(2, 1)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[1]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(2, 2)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(2, 1)));
  CheckFalse(VTestEnum.Next(VPoint));
end;

procedure TestTEnumDoublePointClipByRect.ClosedSecondPointOut;
var
  VData: array of TDoublePoint;
  VTestEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  SetLength(VData, 4);
  VData[0] := DoublePoint(4, 3);
  VData[1] := DoublePoint(1, 0);
  VData[2] := DoublePoint(3, 4);
  VData[3] := DoublePoint(4, 3);

  VTestEnum := PrepareEnumByArray(@VData[0], Length(VData));

  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[0]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(2, 1)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, DoublePoint(2, 2)));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[2]));
  CheckTrue(VTestEnum.Next(VPoint));
  CheckTrue(DoublePointsEqual(VPoint, VData[3]));
  CheckFalse(VTestEnum.Next(VPoint));
end;

function TestTEnumDoublePointClipByRect.PrepareEnumByArray(
  const APoints: PDoublePointArray;
  ACount: Integer
): IEnumDoublePoint;
var
  VDataEnum: IEnumDoublePoint;
begin
  VDataEnum := TEnumDoublePointsByArray.Create(APoints, ACount);
  Result := TEnumDoublePointClipByRect.Create(True, DoubleRect(2, 0, 7, 5), VDataEnum);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTEnumDoublePointClipByLeftBorder.Suite);
  RegisterTest(TestTEnumDoublePointClipByRect.Suite);
end.

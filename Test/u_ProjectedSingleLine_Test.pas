unit u_ProjectedSingleLine_Test;

interface

uses
  TestFramework,
  i_VectorItemProjected,
  u_ProjectedSingleLine;

type
  TestTProjectedPolygonLine = class(TTestCase)
  private
    FPolygon: IProjectedPolygonLine;
  protected
    procedure SetUp; override;
  published
    procedure TestIsRectIntersectPolygonSimple;
  end;


implementation

uses
  t_GeoTypes,
  u_GeoFun;

{ TestTProjectedPolygonLine }

procedure TestTProjectedPolygonLine.SetUp;
var
  VPoints: array of TDoublePoint;
begin
  inherited;
  SetLength(VPoints, 12);
  VPoints[0] := DoublePoint(1, 8);
  VPoints[1] := DoublePoint(1, 4);
  VPoints[2] := DoublePoint(4, 1);
  VPoints[3] := DoublePoint(6, 1);
  VPoints[4] := DoublePoint(3, 4);
  VPoints[5] := DoublePoint(3, 7);
  VPoints[6] := DoublePoint(10, 7);
  VPoints[7] := DoublePoint(10, 4);
  VPoints[8] := DoublePoint(7, 1);
  VPoints[9] := DoublePoint(10, 1);
  VPoints[10] := DoublePoint(13, 4);
  VPoints[11] := DoublePoint(13, 8);

  FPolygon :=
    TProjectedPolygonLine.Create(
      nil,
      Addr(VPoints[0]),
      Length(VPoints)
    );
end;

procedure TestTProjectedPolygonLine.TestIsRectIntersectPolygonSimple;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(0, 0, 1, 1);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(100, 1, 101, 2);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(2, 100, 3, 101);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(1, 1, 2, 2);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(12, 1, 13, 2);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(4, 4, 5, 5);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(2, 4, 5, 6);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(11, 4, 12, 5);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(1, 1, 13, 8);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(2, 2, 3, 3);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(4, 0, 5, 1);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(4, 6, 5, 7);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(3, 5, 4, 6);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(9, 5, 10, 6);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(4, 8, 5, 9);
  FPolygon.IsRectIntersectPolygon(VRect); // Этот сулчай пока игнорим
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTProjectedPolygonLine.Suite);
end.

unit u_ProjectedSingleLine_Test;

interface

uses
  TestFramework,
  i_ProjectedCalc,
  i_GeometryProjected,
  u_GeometryProjected;

type
  TestTProjectedPolygonLine = class(TTestCase)
  private
    FPolygon: IGeometryProjectedSinglePolygon;
    FCalc: IProjectedCalc;
  protected
    procedure SetUp; override;
  published
    procedure TestIsRectIntersectPolygonOutsideFull;
    procedure TestIsRectIntersectPolygonOutsideMBRintersect;
    procedure TestIsRectIntersectPolygonInside;
    procedure TestIsRectIntersectPolygonFullInclude;
    procedure TestIsRectIntersectPolygonIntersectBorder;
    procedure TestIsRectIntersectPolygonTouch;
  end;


implementation

uses
  t_GeoTypes,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_ProjectedCalc,
  u_GeoFunc;

{ TestTProjectedPolygonLine }

procedure TestTProjectedPolygonLine.SetUp;
var
  VPoints: IDoublePointsAggregator;
begin
  inherited;
  FCalc := TProjectedCalc.Create;
  VPoints := TDoublePointsAggregator.Create(12);
  VPoints.Add(DoublePoint(1, 8));
  VPoints.Add(DoublePoint(1, 4));
  VPoints.Add(DoublePoint(4, 1));
  VPoints.Add(DoublePoint(6, 1));
  VPoints.Add(DoublePoint(3, 4));
  VPoints.Add(DoublePoint(3, 7));
  VPoints.Add(DoublePoint(10, 7));
  VPoints.Add(DoublePoint(10, 4));
  VPoints.Add(DoublePoint(7, 1));
  VPoints.Add(DoublePoint(10, 1));
  VPoints.Add(DoublePoint(13, 4));
  VPoints.Add(DoublePoint(13, 8));

  FPolygon :=
    TGeometryProjectedPolygon.Create(
      DoubleRect(1, 1, 13, 8),
      VPoints.MakeStaticAndClear
    );
end;

procedure TestTProjectedPolygonLine.TestIsRectIntersectPolygonOutsideFull;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(0, 0, 1, 1);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(100, 1, 101, 2);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(2, 100, 3, 101);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

end;

procedure TestTProjectedPolygonLine.TestIsRectIntersectPolygonOutsideMBRintersect;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(1, 1, 2, 2);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(12, 1, 13, 2);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(4, 4, 5, 5);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));
end;

procedure TestTProjectedPolygonLine.TestIsRectIntersectPolygonInside;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(2, 4, 5, 6);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(11, 4, 12, 5);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));
end;

procedure TestTProjectedPolygonLine.TestIsRectIntersectPolygonFullInclude;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(1, 1, 13, 8);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(0, 0, 100, 100);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));
end;

procedure TestTProjectedPolygonLine.TestIsRectIntersectPolygonIntersectBorder;
var
  VRect: TDoubleRect;
begin
  VRect := DoubleRect(2, 2, 3, 3);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));

end;

procedure TestTProjectedPolygonLine.TestIsRectIntersectPolygonTouch;
var
  VRect: TDoubleRect;
begin
  // Прямоугольник касается полигона. Случаи спорные. Можно менять поведение
  VRect := DoubleRect(4, 0, 5, 1);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(4, 6, 5, 7);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(3, 5, 4, 6);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(9, 5, 10, 6);
  CheckFalse(FPolygon.IsRectIntersectPolygon(VRect));

  VRect := DoubleRect(4, 8, 5, 9);
  CheckTrue(FPolygon.IsRectIntersectPolygon(VRect));
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTProjectedPolygonLine.Suite);
end.

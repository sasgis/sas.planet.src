unit u_SASTestCase;

interface

uses
  TestFramework,
  t_GeoTypes;

type
  TSASTestCase = class(TTestCase)
  public
    procedure CheckDoublePointsEquals(expected, actual: TDoublePoint; delta: extended; msg: string = ''); overload; virtual;
    procedure CheckDoublePointsEquals(expected, actual: TDoublePoint; msg: string = ''); overload; virtual;
  end;

implementation

uses
  SysUtils,
  Math;

{ TSASTestCase }

procedure TSASTestCase.CheckDoublePointsEquals(expected, actual: TDoublePoint;
  delta: extended; msg: string);
var
  VExpectedEmpty: Boolean;
  VActualEmpty: Boolean;
begin
  VExpectedEmpty := IsNan(expected.x) or IsNan(expected.Y);
  VActualEmpty := IsNan(actual.x) or IsNan(actual.Y);
  FCheckCalled := True;

  if not VExpectedEmpty and not VActualEmpty then begin
    if (abs(expected.X-actual.X) > delta) then
        FailNotEquals('x'+FloatToStr(expected.X), 'x'+FloatToStr(actual.X), msg, CallerAddr);
    if (abs(expected.Y-actual.Y) > delta) then
        FailNotEquals('y'+FloatToStr(expected.Y), 'y'+FloatToStr(actual.Y), msg, CallerAddr);
  end else begin
    if VExpectedEmpty and not VActualEmpty then begin
      Fail('Expected empty point', CallerAddr);
    end else if not VExpectedEmpty and VActualEmpty then begin
      Fail('Not expected empty point but get', CallerAddr);
    end
  end;
end;

procedure TSASTestCase.CheckDoublePointsEquals(expected, actual: TDoublePoint;
  msg: string);
begin
  CheckDoublePointsEquals(expected, actual, 0, msg);
end;

end.

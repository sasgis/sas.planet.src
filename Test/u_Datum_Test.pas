unit u_Datum_Test;

interface

uses
  TestFramework,
  i_Datum,
  u_Datum,
  t_GeoTypes;

type
  TestTDatum = class(TTestCase)
  published
    procedure TestCalcDist;
    procedure TestCalcFinishPosition;
  end;

implementation

uses
  Math,
  c_CoordConverter;

procedure TestTDatum.TestCalcDist;
var
  ReturnValue: Double;
  AFinish: TDoublePoint;
  AStart: TDoublePoint;
  AInitialBearing: Double;
  AFinalBearing: Double;
  VDatum: IDatum;
begin
  VDatum := TDatum.Create(0, CGELonLatProjectionEPSG, 6378137.0000, 6356752.3142);

  AStart.Y := 53.00; // Lat
  AStart.X := 30.00; // Lon

  AFinish.Y := 60.4826175;  // Lat
  AFinish.X := 39.09439537; // Lon

  ReturnValue := VDatum.CalcDist(AStart, AFinish);

  CheckEquals(Round(ReturnValue), 1000000);

  ReturnValue := VDatum.CalcDist(AStart, AFinish, AInitialBearing, AFinalBearing);

  CheckEquals(Round(ReturnValue), 1000000);
end;

procedure TestTDatum.TestCalcFinishPosition;
var
  ReturnValue: TDoublePoint;
  ADistance: Double;
  AStart: TDoublePoint;
  VDatum: IDatum;
begin
  VDatum := TDatum.Create(0, CGELonLatProjectionEPSG, 6378137.0000, 6356752.3142);

  AStart.Y := 53.00; // Lat
  AStart.X := 30.00; // Lon
  ADistance := 1000000; // 1000 km

  ReturnValue := VDatum.CalcFinishPosition(AStart, 30, ADistance);

  CheckEquals(RoundTo(ReturnValue.Y, -7), RoundTo(60.4826175, -7));
  CheckEquals(RoundTo(ReturnValue.X, -8), RoundTo(39.09439537, -8));
end;

initialization
  RegisterTest(TestTDatum.Suite);

end.


unit u_CoordToStringConverter_Test;

interface

uses
  TestFramework,
  t_GeoTypes,
  i_CoordToStringConverter;

type
  TestCoordToStringConverter = class(TTestCase)
  published
    procedure TestCoordToString;
  end;

implementation

uses
  t_CoordRepresentation,
  u_GeoFunc,
  u_CoordToStringConverter;

{ TestCoordToStringConverter }

procedure TestCoordToStringConverter.TestCoordToString;
var
  VResult: TCoordPartArray;
  VConverter: ICoordToStringConverter;
begin
  VConverter := TCoordToStringConverter.Create(
    True, dshSignDegr2, psfRoundedToTenth, msfSplitted, cstWGS84, csitDontShow
  );

  VResult := VConverter.LonLatConvertExt(DoublePoint(10.0, 40.101), [coCutZero]);

  CheckEquals('10', VResult[cpiLon]);
  CheckEquals('40.101', VResult[cpiLat]);
end;

initialization
  RegisterTest(TestCoordToStringConverter.Suite);

end.

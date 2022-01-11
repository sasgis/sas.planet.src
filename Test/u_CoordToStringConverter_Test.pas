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
  u_CoordToStringConverter;

{ TestCoordToStringConverter }

procedure TestCoordToStringConverter.TestCoordToString;
var
  VLon, VLat: string;
  VConverter: ICoordToStringConverter;
begin
  VConverter := TCoordToStringConverter.Create(
    True, dshSignDegr2, cstWGS84, csitDontShow
  );

  VConverter.LonLatConvert(10.0, 40.101, True, VLon, VLat);

  CheckEquals('10', VLon);
  CheckEquals('40.101', VLat);
end;

initialization
  RegisterTest(TestCoordToStringConverter.Suite);

end.

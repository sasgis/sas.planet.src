program TestCoordConverter;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  u_TesterCoordConvertAbstract in 'u_TesterCoordConvertAbstract.pas',
  t_GeoTypes in '..\t_GeoTypes.pas',
  u_CoordConverterAbstract in '..\u_CoordConverterAbstract.pas',
  u_CoordConverterMercatorOnEllipsoid in '..\u_CoordConverterMercatorOnEllipsoid.pas',
  u_CoordConverterMercatorOnSphere in '..\u_CoordConverterMercatorOnSphere.pas',
  u_CoordConverterSimpleLonLat in '..\u_CoordConverterSimpleLonLat.pas';

procedure Test;
var
  VTester: TTesterCoordConverterAbstract;
  radiusa, radiusb: double;
begin
  radiusa := 6378137;
  radiusb := 6356752;
  VTester := TTesterCoordConverterAbstract.Create(TCoordConverterMercatorOnEllipsoid.Create(sqrt(radiusa*radiusa-radiusb*radiusb)/radiusa,radiusa,radiusb));
  try
    VTester.CheckConverter;
    Writeln('Ok');
  except
    on E:Exception do begin
      writeln(E.Message);
      writeln('Fail');
    end;
  end;

end;
begin
  Test;
  readln;
end.

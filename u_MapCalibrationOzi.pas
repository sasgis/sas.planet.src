unit u_MapCalibrationOzi;

interface

uses
  Types,
  i_CoordConverter,
  i_MapCalibration;

type
  TMapCalibrationOzi = class(TInterfacedObject, IMapCalibration)
  public
    // Имя для вывода в листбоксе для выбора при экспорте.
    function GetName: WideString; safecall;
    // Более детальное описание привязки
    function GetDescription: WideString; safecall;
    // Генерирует привязку для склеенной карты.
    procedure SaveCalibrationInfo(AFileName: WideString; xy1, xy2: TPoint; Azoom: byte; AConverter: ICoordConverter); safecall;
  end;

implementation

uses
  Classes,
  SysUtils,
  t_GeoTypes;

{ TMapCalibrationOzi }

function TMapCalibrationOzi.GetDescription: WideString;
begin
  Result := 'Привязка *.map для программы OziExplorer';
end;

function TMapCalibrationOzi.GetName: WideString;
begin
  Result := '.map';
end;

procedure TMapCalibrationOzi.SaveCalibrationInfo(AFileName: WideString; xy1,
  xy2: TPoint; Azoom: byte; AConverter: ICoordConverter);
const
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы
var
  f: TextFile;
  xy: TPoint;
  rad: real;
  lat, lon: array[1..3] of real;
  i: integer;
  lats, lons: array[1..3] of string;
  VFormat: TFormatSettings;
  VFileName: String;
  VLL1, VLL2: TDoublePoint;
  VLL: TDoublePoint;
begin
  VFormat.DecimalSeparator := '.';
  VFileName := ChangeFileExt(AFileName, '.map');

  assignfile(f, VFileName);
  rewrite(f);
  try
    writeln(f, 'OziExplorer Map Data File Version 2.2');
    writeln(f, 'Created by SAS.Planet');
    writeln(f, ExtractFileName(AFileName));
    writeln(f, '1 ,Map Code,' + #13#10 + 'WGS 84,,   0.0000,   0.0000,WGS 84' + #13#10 + 'Reserved 1' + #13#10 +
      'Reserved 2' + #13#10 + 'Magnetic Variation,,,E' + #13#10 + 'Map Projection,Mercator,PolyCal,No,AutoCalOnly,No,BSBUseWPX,No');

    VLL1 := AConverter.PixelPos2LonLat(xy1, Azoom);
    VLL2 := AConverter.PixelPos2LonLat(xy2, Azoom);
    xy.Y := (xy2.y - ((xy2.Y - xy1.Y) div 2));
    xy.X := (xy2.x - ((xy2.x - xy1.x) div 2));
    VLL := AConverter.PixelPos2LonLat(xy, Azoom);

    lon[1] := VLL1.X;
    lat[1] := VLL1.Y;
    lon[2] := VLL.X;
    lat[2] := VLL.Y;
    lon[3] := VLL2.X;
    lat[3] := VLL2.Y;

    for i := 1 to 3 do begin
      lons[i] := inttostr(trunc(abs(lon[i]))) + ', ' + FloatToStr(Frac(abs(lon[i])) * 60, VFormat);
      if lon[i] < 0 then begin
        lons[i] := lons[i] + ',W';
      end else begin
        lons[i] := lons[i] + ',E';
      end;
    end;
    for i := 1 to 3 do begin
      lats[i] := inttostr(trunc(abs(lat[i]))) + ', ' + FloatToStr(Frac(abs(lat[i])) * 60, VFormat);
      if lat[i] < 0 then begin
        lats[i] := lats[i] + ',S';
      end else begin
        lats[i] := lats[i] + ',N';
      end;
    end;

    xy2 := Point(xy2.X - xy1.X, xy2.y - xy1.y);
    xy1 := Point(0, 0);

    writeln(f, 'Point01,xy,    ' + inttostr(xy1.x) + ', ' + inttostr(xy1.y) + ',in, deg, ' + lats[1] + ', ' + lons[1] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point02,xy,    ' + inttostr(xy2.x) + ', ' + inttostr(xy2.y) + ',in, deg, ' + lats[3] + ', ' + lons[3] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point03,xy,    ' + inttostr(xy1.x) + ', ' + inttostr(xy2.y) + ',in, deg, ' + lats[3] + ', ' + lons[1] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point04,xy,    ' + inttostr(xy2.x) + ', ' + inttostr(xy1.y) + ',in, deg, ' + lats[1] + ', ' + lons[3] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point05,xy,    ' + inttostr((xy2.x - xy1.X) div 2) + ', ' + inttostr((xy2.y - xy1.y) div 2) + ',in, deg, ' + lats[2] + ', ' + lons[2] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point06,xy,    ' + inttostr((xy2.x - xy1.X) div 2) + ', ' + inttostr(xy1.y) + ',in, deg, ' + lats[1] + ', ' + lons[2] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point07,xy,    ' + inttostr(xy1.x) + ', ' + inttostr((xy2.y - xy1.y) div 2) + ',in, deg, ' + lats[2] + ', ' + lons[1] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point08,xy,    ' + inttostr(xy2.x) + ', ' + inttostr((xy2.y - xy1.y) div 2) + ',in, deg, ' + lats[2] + ', ' + lons[3] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point09,xy,    ' + inttostr((xy2.x - xy1.X) div 2) + ', ' + inttostr(xy2.y) + ',in, deg, ' + lats[3] + ', ' + lons[2] + ', grid,   ,           ,           ,N');
    for i := 10 to 30 do begin
      writeln(f, 'Point' + inttostr(i) + ',xy,     ,     ,in, deg,    ,        ,N,    ,        ,W, grid,   ,           ,           ,N');
    end;

    writeln(f, 'Projection Setup,,,,,,,,,,' + #13#10 + 'Map Feature = MF ; Map Comment = MC     These follow if they exist' + #13#10 + 'Track File = TF      These follow if they exist' + #13#10 + 'Moving Map Parameters = MM?    These follow if they exist' + #13#10 + 'MM0,Yes' + #13#10 + 'MMPNUM,4');
    writeln(f, 'MMPXY,1,' + inttostr(xy1.X) + ',' + inttostr(xy1.y));
    writeln(f, 'MMPXY,2,' + inttostr(xy2.X) + ',' + inttostr(xy1.y));
    writeln(f, 'MMPXY,3,' + inttostr(xy2.X) + ',' + inttostr(xy2.y));
    writeln(f, 'MMPXY,4,' + inttostr(xy1.X) + ',' + inttostr(xy2.y));

    writeln(f, 'MMPLL,1, ' + FloatToStr(lon[1], VFormat) + ', ' + FloatToStr(lat[1], VFormat));
    writeln(f, 'MMPLL,2, ' + FloatToStr(lon[3], VFormat) + ', ' + FloatToStr(lat[1], VFormat));
    writeln(f, 'MMPLL,3, ' + FloatToStr(lon[3], VFormat) + ', ' + FloatToStr(lat[3], VFormat));
    writeln(f, 'MMPLL,4, ' + FloatToStr(lon[1], VFormat) + ', ' + FloatToStr(lat[3], VFormat));

    rad := AConverter.Datum.GetSpheroidRadiusA;

    writeln(f, 'MM1B,' + FloatToStr(1 / ((AConverter.PixelsAtZoomFloat(Azoom) / (2 * PI)) / (rad * cos(lat[2] * D2R))), VFormat));
    writeln(f, 'MOP,Map Open Position,0,0');
    writeln(f, 'IWH,Map Image Width/Height,' + inttostr(xy2.X) + ',' + inttostr(xy2.y));
  finally
    closefile(f);
  end;
end;

end.

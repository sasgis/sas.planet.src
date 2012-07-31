{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

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
    procedure SaveCalibrationInfo(
      const AFileName: WideString;
      const xy1, xy2: TPoint;
      AZoom: byte;
      const AConverter: ICoordConverter
    ); safecall;
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

procedure TMapCalibrationOzi.SaveCalibrationInfo(
  const AFileName: WideString;
  const xy1, xy2: TPoint;
  AZoom: byte;
  const AConverter: ICoordConverter
);
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
  VLocalRect: TRect;
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

    VLL1 := AConverter.PixelPos2LonLat(xy1, AZoom);
    VLL2 := AConverter.PixelPos2LonLat(xy2, AZoom);
    xy.Y := (xy2.y - ((xy2.Y - xy1.Y) div 2));
    xy.X := (xy2.x - ((xy2.x - xy1.x) div 2));
    VLL := AConverter.PixelPos2LonLat(xy, AZoom);

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
    VLocalRect.TopLeft := Point(0, 0);
    VLocalRect.BottomRight := Point(xy2.X - xy1.X, xy2.y - xy1.y);

    writeln(f, 'Point01,xy,    ' + inttostr(VLocalRect.Left) + ', ' + inttostr(VLocalRect.Top) + ',in, deg, ' + lats[1] + ', ' + lons[1] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point02,xy,    ' + inttostr(VLocalRect.Right) + ', ' + inttostr(VLocalRect.Bottom) + ',in, deg, ' + lats[3] + ', ' + lons[3] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point03,xy,    ' + inttostr(VLocalRect.Left) + ', ' + inttostr(VLocalRect.Bottom) + ',in, deg, ' + lats[3] + ', ' + lons[1] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point04,xy,    ' + inttostr(VLocalRect.Right) + ', ' + inttostr(VLocalRect.Top) + ',in, deg, ' + lats[1] + ', ' + lons[3] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point05,xy,    ' + inttostr((VLocalRect.Right - VLocalRect.Left) div 2) + ', ' + inttostr((VLocalRect.Bottom - VLocalRect.Top) div 2) + ',in, deg, ' + lats[2] + ', ' + lons[2] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point06,xy,    ' + inttostr((VLocalRect.Right - VLocalRect.Left) div 2) + ', ' + inttostr(VLocalRect.Top) + ',in, deg, ' + lats[1] + ', ' + lons[2] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point07,xy,    ' + inttostr(VLocalRect.Left) + ', ' + inttostr((VLocalRect.Bottom - VLocalRect.Top) div 2) + ',in, deg, ' + lats[2] + ', ' + lons[1] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point08,xy,    ' + inttostr(VLocalRect.Right) + ', ' + inttostr((VLocalRect.Bottom - VLocalRect.Top) div 2) + ',in, deg, ' + lats[2] + ', ' + lons[3] + ', grid,   ,           ,           ,N');
    writeln(f, 'Point09,xy,    ' + inttostr((VLocalRect.Right - VLocalRect.Left) div 2) + ', ' + inttostr(VLocalRect.Bottom) + ',in, deg, ' + lats[3] + ', ' + lons[2] + ', grid,   ,           ,           ,N');
    for i := 10 to 30 do begin
      writeln(f, 'Point' + inttostr(i) + ',xy,     ,     ,in, deg,    ,        ,N,    ,        ,W, grid,   ,           ,           ,N');
    end;

    writeln(f, 'Projection Setup,,,,,,,,,,' + #13#10 + 'Map Feature = MF ; Map Comment = MC     These follow if they exist' + #13#10 + 'Track File = TF      These follow if they exist' + #13#10 + 'Moving Map Parameters = MM?    These follow if they exist' + #13#10 + 'MM0,Yes' + #13#10 + 'MMPNUM,4');
    writeln(f, 'MMPXY,1,' + inttostr(VLocalRect.Left) + ',' + inttostr(VLocalRect.Top));
    writeln(f, 'MMPXY,2,' + inttostr(VLocalRect.Right) + ',' + inttostr(VLocalRect.Top));
    writeln(f, 'MMPXY,3,' + inttostr(VLocalRect.Right) + ',' + inttostr(VLocalRect.Bottom));
    writeln(f, 'MMPXY,4,' + inttostr(VLocalRect.Left) + ',' + inttostr(VLocalRect.Bottom));

    writeln(f, 'MMPLL,1, ' + FloatToStr(lon[1], VFormat) + ', ' + FloatToStr(lat[1], VFormat));
    writeln(f, 'MMPLL,2, ' + FloatToStr(lon[3], VFormat) + ', ' + FloatToStr(lat[1], VFormat));
    writeln(f, 'MMPLL,3, ' + FloatToStr(lon[3], VFormat) + ', ' + FloatToStr(lat[3], VFormat));
    writeln(f, 'MMPLL,4, ' + FloatToStr(lon[1], VFormat) + ', ' + FloatToStr(lat[3], VFormat));

    rad := AConverter.Datum.GetSpheroidRadiusA;

    writeln(f, 'MM1B,' + FloatToStr(1 / ((AConverter.PixelsAtZoomFloat(AZoom) / (2 * PI)) / (rad * cos(lat[2] * D2R))), VFormat));
    writeln(f, 'MOP,Map Open Position,0,0');
    writeln(f, 'IWH,Map Image Width/Height,' + inttostr(VLocalRect.Right) + ',' + inttostr(VLocalRect.Bottom));
  finally
    closefile(f);
  end;
end;

end.

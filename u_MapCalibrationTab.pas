{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_MapCalibrationTab;

interface

uses
  Types,
  i_CoordConverter,
  i_MapCalibration;

type
  TMapCalibrationTab = class(TInterfacedObject, IMapCalibration)
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
  SysUtils,
  t_GeoTypes,
  u_GeoToStr;

{ TMapCalibrationTab }

function TMapCalibrationTab.GetDescription: WideString;
begin
  Result := 'Привязка *.tab';
end;

function TMapCalibrationTab.GetName: WideString;
begin
  Result := '.tab';
end;

procedure TMapCalibrationTab.SaveCalibrationInfo(AFileName: WideString;
  xy1, xy2: TPoint; Azoom: byte; AConverter: ICoordConverter);
var
  f: TextFile;
  xy: TPoint;
  lat, lon: array[1..3] of real;
  VLL1, VLL2: TDoublePoint;
  VLL: TDoublePoint;
begin
  assignfile(f, ChangeFileExt(AFileName, '.tab'));
  rewrite(f);
  writeln(f, '!table');
  writeln(f, '!version 300');
  writeln(f, '!charset WindowsCyrillic' + #13#10);
  writeln(f, 'Definition Table');
  writeln(f, 'File "' + ExtractFileName(AFileName) + '"');
  writeln(f, 'Type "RASTER"');

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

  xy2 := Point(xy2.X - xy1.X, xy2.y - xy1.y);
  xy1 := Point(0, 0);

  writeln(f, '(' + R2StrPoint(lon[1]) + ',' + R2StrPoint(lat[1]) + ') (' + inttostr(xy1.x) + ', ' + inttostr(xy1.y) + ') Label "Точка 1",');
  writeln(f, '(' + R2StrPoint(lon[3]) + ',' + R2StrPoint(lat[3]) + ') (' + inttostr(xy2.x) + ', ' + inttostr(xy2.y) + ') Label "Точка 2",');
  writeln(f, '(' + R2StrPoint(lon[1]) + ',' + R2StrPoint(lat[3]) + ') (' + inttostr(xy1.x) + ', ' + inttostr(xy2.y) + ') Label "Точка 3",');
  writeln(f, '(' + R2StrPoint(lon[3]) + ',' + R2StrPoint(lat[1]) + ') (' + inttostr(xy2.x) + ', ' + inttostr(xy1.y) + ') Label "Точка 4",');
  writeln(f, '(' + R2StrPoint(lon[2]) + ',' + R2StrPoint(lat[2]) + ') (' + inttostr((xy2.x - xy1.X) div 2) + ', ' + inttostr((xy2.y - xy1.y) div 2) + ') Label "Точка 5",');
  writeln(f, '(' + R2StrPoint(lon[2]) + ',' + R2StrPoint(lat[1]) + ') (' + inttostr((xy2.x - xy1.X) div 2) + ', ' + inttostr(xy1.y) + ') Label "Точка 6",');
  writeln(f, '(' + R2StrPoint(lon[1]) + ',' + R2StrPoint(lat[2]) + ') (' + inttostr(xy1.x) + ', ' + inttostr((xy2.y - xy1.y) div 2) + ') Label "Точка 7",');
  writeln(f, '(' + R2StrPoint(lon[3]) + ',' + R2StrPoint(lat[2]) + ') (' + inttostr(xy2.x) + ', ' + inttostr((xy2.y - xy1.y) div 2) + ') Label "Точка 8",');
  writeln(f, '(' + R2StrPoint(lon[2]) + ',' + R2StrPoint(lat[3]) + ') (' + inttostr((xy2.x - xy1.X) div 2) + ', ' + inttostr(xy2.y) + ') Label "Точка 9"');

  writeln(f, 'CoordSys Earth Projection 1, 104');
  writeln(f, 'Units "degree"');
  closefile(f);
end;

end.
 
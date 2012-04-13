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

unit u_MapCalibrationKml;

interface

uses
  Types,
  i_CoordConverter,
  i_MapCalibration;

type
  TMapCalibrationKml = class(TInterfacedObject, IMapCalibration)
  public
    // Имя для вывода в листбоксе для выбора при экспорте.
    function GetName: WideString; safecall;
    // Более детальное описание привязки
    function GetDescription: WideString; safecall;
    // Генерирует привязку для склеенной карты.
    procedure SaveCalibrationInfo(
      const AFileName: WideString;
      const xy1, xy2: TPoint;
      Azoom: byte;
      const AConverter: ICoordConverter
    ); safecall;
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  u_GeoToStr;

{ TMapCalibrationKml }

function TMapCalibrationKml.GetDescription: WideString;
begin
  Result := 'Привязка *.kml для программы Google Earth';
end;

function TMapCalibrationKml.GetName: WideString;
begin
  Result := '.kml';
end;

procedure TMapCalibrationKml.SaveCalibrationInfo(
  const AFileName: WideString;
  const xy1, xy2: TPoint;
  Azoom: byte;
  const AConverter: ICoordConverter
);
var
  f: TextFile;
  LL1, LL2: TDoublePoint;
  str: UTF8String;
  VFileName: String;
begin
  assignfile(f, ChangeFileExt(AFileName, '.kml'));
  rewrite(f);
  VFileName := ExtractFileName(AFileName);
  str := ansiToUTF8('<?xml version="1.0" encoding="UTF-8"?>' + #13#10);
  str := str + ansiToUTF8('<kml><GroundOverlay><name>' + VFileName + '</name><color>88ffffff</color><Icon>' + #13#10);
  str := str + ansiToUTF8('<href>' + VFileName + '</href>' + #13#10);
  str := str + ansiToUTF8('<viewBoundScale>0.75</viewBoundScale></Icon><LatLonBox>' + #13#10);
  LL1 := AConverter.PixelPos2LonLat(xy1, Azoom);
  LL2 := AConverter.PixelPos2LonLat(xy2, Azoom);
  str := str + ansiToUTF8('<north>' + R2StrPoint(LL1.y) + '</north>' + #13#10);
  str := str + ansiToUTF8('<south>' + R2StrPoint(LL2.y) + '</south>' + #13#10);
  str := str + ansiToUTF8('<east>' + R2StrPoint(LL2.x) + '</east>' + #13#10);
  str := str + ansiToUTF8('<west>' + R2StrPoint(LL1.x) + '</west>' + #13#10);
  str := str + ansiToUTF8('</LatLonBox></GroundOverlay></kml>');
  writeln(f, str);
  closefile(f);
end;

end.
 
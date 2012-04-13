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

unit u_MapCalibrationDat;

interface

uses
  Types,
  i_CoordConverter,
  i_MapCalibration;

type
  TMapCalibrationDat = class(TInterfacedObject, IMapCalibration)
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

{ TMapCalibrationDat }

function TMapCalibrationDat.GetDescription: WideString;
begin
  Result := 'Привязка *.dat для программы Radio Mobile';
end;

function TMapCalibrationDat.GetName: WideString;
begin
  Result := '.dat';
end;

procedure TMapCalibrationDat.SaveCalibrationInfo(
  const AFileName: WideString;
  const xy1, xy2: TPoint;
  Azoom: byte;
  const AConverter: ICoordConverter
);
var
  f: TextFile;
  LL1, LL2: TDoublePoint;
begin
  assignfile(f, ChangeFileExt(AFileName, '.dat'));
  rewrite(f);
  writeln(f, '2');
  LL1 := AConverter.PixelPos2LonLat(xy1, Azoom);
  LL2 := AConverter.PixelPos2LonLat(xy2, Azoom);
  writeln(f, R2StrPoint(LL1.x) + ',' + R2StrPoint(LL1.y));
  writeln(f, R2StrPoint(LL2.x) + ',' + R2StrPoint(LL1.y));
  writeln(f, R2StrPoint(LL2.x) + ',' + R2StrPoint(LL2.y));
  writeln(f, R2StrPoint(LL1.x) + ',' + R2StrPoint(LL2.y));
  writeln(f, '(SASPlanet)');
  closefile(f);
end;

end.
 
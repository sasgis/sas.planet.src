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
  i_MapCalibration,
  u_BaseInterfacedObject;

type
  TMapCalibrationDat = class(TBaseInterfacedObject, IMapCalibration)
  private
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
  AZoom: byte;
  const AConverter: ICoordConverter
);
var
  LL1, LL2: TDoublePoint;
  VFileName: string;
  VFileStream: TFileStream;
  VText: AnsiString;
begin
  VFileName := ChangeFileExt(AFileName, '.dat');
  VFileStream := TFileStream.Create(VFileName, fmCreate);
  try
    VText := '';
    VText := VText + '2' + #13#10;
    LL1 := AConverter.PixelPos2LonLat(xy1, AZoom);
    LL2 := AConverter.PixelPos2LonLat(xy2, AZoom);
    VText := VText + R2AnsiStrPoint(LL1.x) + ',' + R2AnsiStrPoint(LL1.y) + #13#10;
    VText := VText + R2AnsiStrPoint(LL2.x) + ',' + R2AnsiStrPoint(LL1.y) + #13#10;
    VText := VText + R2AnsiStrPoint(LL2.x) + ',' + R2AnsiStrPoint(LL2.y) + #13#10;
    VText := VText + R2AnsiStrPoint(LL1.x) + ',' + R2AnsiStrPoint(LL2.y) + #13#10;
    VText := VText + '(SASPlanet)' + #13#10;

    VFileStream.WriteBuffer(VText[1], Length(VText));
  finally
    VFileStream.Free;
  end;
end;

end.

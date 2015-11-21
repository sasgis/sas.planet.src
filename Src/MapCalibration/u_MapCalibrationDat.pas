{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_MapCalibrationDat;

interface

uses
  Types,
  i_Projection,
  i_MapCalibration,
  u_BaseInterfacedObject;

type
  TMapCalibrationDat = class(TBaseInterfacedObject, IMapCalibration)
  private
    { IMapCalibration }
    function GetName: string;
    function GetDescription: string; 
    procedure SaveCalibrationInfo(
      const AFileName: string;
      const ATopLeft: TPoint;
      const ABottomRight: TPoint;
      const AProjection: IProjection
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  gnugettext,
  t_GeoTypes,
  u_GeoToStrFunc;

{ TMapCalibrationDat }

function TMapCalibrationDat.GetDescription: string;
begin
  Result := _('Calibration for Radio Mobile programm (*.dat)');
end;

function TMapCalibrationDat.GetName: string;
begin
  Result := '.dat';
end;

procedure TMapCalibrationDat.SaveCalibrationInfo(
  const AFileName: string;
  const ATopLeft: TPoint;
  const ABottomRight: TPoint;
  const AProjection: IProjection
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
    LL1 := AProjection.PixelPos2LonLat(ATopLeft);
    LL2 := AProjection.PixelPos2LonLat(ABottomRight);
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

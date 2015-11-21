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

unit u_MapCalibrationKml;

interface

uses
  Types,
  i_Projection,
  i_MapCalibration,
  u_BaseInterfacedObject;

type
  TMapCalibrationKml = class(TBaseInterfacedObject, IMapCalibration)
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

{ TMapCalibrationKml }

function TMapCalibrationKml.GetDescription: string;
begin
  Result := _('Calibration for Google Earth programm (*.kml)');
end;

function TMapCalibrationKml.GetName: string;
begin
  Result := '.kml';
end;

procedure TMapCalibrationKml.SaveCalibrationInfo(
  const AFileName: string;
  const ATopLeft: TPoint;
  const ABottomRight: TPoint;
  const AProjection: IProjection
);
var
  LL1, LL2: TDoublePoint;
  VText: UTF8String;
  VFileName: string;
  VFileNameOnly: string;
  VFileStream: TFileStream;
begin
  LL1 := AProjection.PixelPos2LonLat(ATopLeft);
  LL2 := AProjection.PixelPos2LonLat(ABottomRight);

  VFileNameOnly := ExtractFileName(AFileName);

  VFileName := ChangeFileExt(AFileName, '.kml');

  VFileStream := TFileStream.Create(VFileName, fmCreate);
  try
    VText :=
      UTF8Encode(
        '<?xml version="1.0" encoding="UTF-8"?>' + #13#10 +
        '<kml><GroundOverlay><name>' + VFileNameOnly + '</name><color>88ffffff</color><Icon>' + #13#10 +
        '<href>' + VFileNameOnly + '</href>' + #13#10 +
        '<viewBoundScale>0.75</viewBoundScale></Icon><LatLonBox>' + #13#10 +
        '<north>' + R2StrPoint(LL1.y) + '</north>' + #13#10 +
        '<south>' + R2StrPoint(LL2.y) + '</south>' + #13#10 +
        '<east>' + R2StrPoint(LL2.x) + '</east>' + #13#10 +
        '<west>' + R2StrPoint(LL1.x) + '</west>' + #13#10 +
        '</LatLonBox></GroundOverlay></kml>'
      );

    VFileStream.WriteBuffer(VText[1], Length(VText));
  finally
    VFileStream.Free;
  end;
end;

end.

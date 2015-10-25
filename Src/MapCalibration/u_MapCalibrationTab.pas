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

unit u_MapCalibrationTab;

interface

uses
  Types,
  ALString,
  i_Projection,
  i_MapCalibration,
  u_BaseInterfacedObject;

type
  TMapCalibrationTab = class(TBaseInterfacedObject, IMapCalibration)
  private
    FFormatSettings: TALFormatSettings;
    function PointToStr(
      const ANumber: Integer;
      const ALon, ALat: Double;
      const X, Y: Integer
    ): AnsiString;
    function GetCoordSysStr(const AProjection: IProjection): AnsiString;
  private
    { IMapCalibration }
    function GetName: WideString; safecall;
    function GetDescription: WideString; safecall;
    procedure SaveCalibrationInfo(
      const AFileName: WideString;
      const ATopLeft: TPoint;
      const ABottomRight: TPoint;
      const AProjection: IProjection
    ); safecall;
  public
    constructor Create;
  end;

implementation

uses
  Classes,
  SysUtils,
  t_GeoTypes,
  c_CoordConverter;

const
  cTabFileExt = '.tab';
  cCoordFmtStr: AnsiString = '%.8f';
  cPointFmtStr: AnsiString = '  (%s, %s) (%d, %d) Label "Point %d"';

resourcestring
  rsTabMapCalibrationDescription = 'Calibration for MapInfo programm (*.tab)';

{ TMapCalibrationTab }

constructor TMapCalibrationTab.Create;
begin
  inherited Create;
  FFormatSettings.DecimalSeparator := '.';
end;

function TMapCalibrationTab.GetDescription: WideString;
begin
  Result := rsTabMapCalibrationDescription;
end;

function TMapCalibrationTab.GetName: WideString;
begin
  Result := cTabFileExt;
end;

function TMapCalibrationTab.PointToStr(
  const ANumber: Integer;
  const ALon, ALat: Double;
  const X, Y: Integer
): AnsiString;
var
  VLon, VLat: AnsiString;
begin
  VLon := ALFormat(cCoordFmtStr, [ALon], FFormatSettings);
  VLat := ALFormat(cCoordFmtStr, [ALat], FFormatSettings);
  Result := ALFormat(cPointFmtStr, [VLon, VLat, X, Y, ANumber], FFormatSettings);
end;

function TMapCalibrationTab.GetCoordSysStr(
  const AProjection: IProjection
): AnsiString;
begin
  case AProjection.ProjectionType.ProjectionEPSG of
    CGoogleProjectionEPSG: begin
      Result := '10, 157, 7, 0';
    end;
    CYandexProjectionEPSG: begin
      Result := '10, 104, 7, 0';
    end;
    CGELonLatProjectionEPSG: begin
      Result := '1, 104, 7, 0';
    end;
  else begin
    Assert(False, 'Unexpected projection EPSG code: ' + IntToStr(AProjection.ProjectionType.ProjectionEPSG));
    Result := '1, 104, 7, 0';
      // For more projections see page 403 of UserGuide:
      // http://reference.mapinfo.com/software/mapinfo_pro/english/10/MapInfoProfessionalUserGuide.pdf
  end;
  end;
end;

procedure TMapCalibrationTab.SaveCalibrationInfo(
  const AFileName: WideString;
  const ATopLeft: TPoint;
  const ABottomRight: TPoint;
  const AProjection: IProjection
);
var
  VCenter: TPoint;
  VLL, VLL1, VLL2: TDoublePoint;
  VLocalRect: TRect;
  VFileName: string;
  VFileStream: TFileStream;
  VText, VName: AnsiString;
begin
  VCenter.Y := (ABottomRight.Y - ((ABottomRight.Y - ATopLeft.Y) div 2));
  VCenter.X := (ABottomRight.X - ((ABottomRight.X - ATopLeft.X) div 2));

  VLL1 := AProjection.PixelPos2LonLat(ATopLeft);
  VLL2 := AProjection.PixelPos2LonLat(ABottomRight);
  VLL := AProjection.PixelPos2LonLat(VCenter);

  VLocalRect.TopLeft := Point(0, 0);
  VLocalRect.BottomRight := Point(ABottomRight.X - ATopLeft.X, ABottomRight.Y - ATopLeft.Y);

  VFileName := ChangeFileExt(AFileName, cTabFileExt);

  VFileStream := TFileStream.Create(VFileName, fmCreate);
  try
    VName := AnsiString(ExtractFileName(AFileName));

    VText :=
      '!table' + #13#10 +
      '!version 300' + #13#10 +
      '!charset Neutral' + #13#10 + #13#10 +
      'Definition Table' + #13#10 +
      '  File "' + VName + '"' + #13#10 +
      '  Type "RASTER"' + #13#10 +
      PointToStr(1, VLL1.X, VLL1.Y, VLocalRect.Left, VLocalRect.Top) + ',' + #13#10 +
      PointToStr(2, VLL2.X, VLL2.Y, VLocalRect.Right, VLocalRect.Bottom) + ',' + #13#10 +
      PointToStr(3, VLL1.X, VLL2.Y, VLocalRect.Left, VLocalRect.Bottom) + ',' + #13#10 +
      PointToStr(4, VLL2.X, VLL1.Y, VLocalRect.Right, VLocalRect.Top) + ',' + #13#10 +
      PointToStr(5, VLL.X, VLL.Y, ((VLocalRect.Right - VLocalRect.Left) div 2), ((VLocalRect.Bottom - VLocalRect.Top) div 2)) + ',' + #13#10 +
      PointToStr(6, VLL.X, VLL1.Y, ((VLocalRect.Right - VLocalRect.Left) div 2), VLocalRect.Top) + ',' + #13#10 +
      PointToStr(7, VLL1.X, VLL.Y, VLocalRect.Left, ((VLocalRect.Bottom - VLocalRect.Top) div 2)) + ',' + #13#10 +
      PointToStr(8, VLL2.X, VLL.Y, VLocalRect.Right, ((VLocalRect.Bottom - VLocalRect.Top) div 2)) + ',' + #13#10 +
      PointToStr(9, VLL.X, VLL2.Y, ((VLocalRect.Right - VLocalRect.Left) div 2), VLocalRect.Bottom) + #13#10 +
      ' CoordSys Earth Projection ' + GetCoordSysStr(AProjection) + #13#10 +
      ' Units "degree"' + #13#10;

    VFileStream.WriteBuffer(VText[1], Length(VText));
  finally
    VFileStream.Free;
  end;
end;

end.

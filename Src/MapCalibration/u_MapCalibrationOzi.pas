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

unit u_MapCalibrationOzi;

interface

uses
  Types,
  ALString,
  i_Projection,
  i_MapCalibration,
  u_BaseInterfacedObject;

type
  TMapCalibrationOzi = class(TBaseInterfacedObject, IMapCalibration)
  private
    FFormatSettings: TALFormatSettings;
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
  cOziFileExt = '.map';
  cCalibrationStrFormat: AnsiString = 'Point%.2d,xy,    %d, %d,in, deg, %s, %s, grid,   ,           ,           ,N' + #13#10;
  cCalibrationEmptyStrFormat: AnsiString = 'Point%.2d,xy,     ,     ,in, deg,    ,        ,N,    ,        ,W, grid,   ,           ,           ,N' + #13#10;

resourcestring
  rsOziMapCalibrationDescription = 'Calibration for OziExplorer programm (*.map)';

{ TMapCalibrationOzi }

constructor TMapCalibrationOzi.Create;
begin
  inherited Create;
  FFormatSettings.DecimalSeparator := '.';
end;

function TMapCalibrationOzi.GetDescription: WideString;
begin
  Result := rsOziMapCalibrationDescription;
end;

function TMapCalibrationOzi.GetName: WideString;
begin
  Result := cOziFileExt;
end;

procedure TMapCalibrationOzi.SaveCalibrationInfo(
  const AFileName: WideString;
  const ATopLeft: TPoint;
  const ABottomRight: TPoint;
  const AProjection: IProjection
);

  function GetDegrees(const ACoord: Double): AnsiString;
  begin
    Result := ALIntToStr(Trunc(Abs(ACoord)));
  end;

  function GetMinutes(const ACoord: Double): AnsiString;
  begin
    Result := ALFormat('%.4f', [Frac(Abs(ACoord)) * 60], FFormatSettings);
  end;

  function DoubleToAnsiStr(const AValue: Double): AnsiString;
  begin
    Result := ALFormat('%.6f', [AValue], FFormatSettings);
  end;

  function GetPointCalibrationStr(const APointID, X, Y: Integer; const ALon, ALat: Double): AnsiString;
  var
    VLonStr, VLatStr: AnsiString;
  begin
    VLonStr := GetDegrees(ALon) + ', ' + GetMinutes(ALon);
    if ALon < 0 then begin
      VLonStr := VLonStr + ',W';
    end else begin
      VLonStr := VLonStr + ',E';
    end;

    VLatStr := GetDegrees(ALat) + ', ' + GetMinutes(ALat);
    if ALat < 0 then begin
      VLatStr := VLatStr + ',S';
    end else begin
      VLatStr := VLatStr + ',N';
    end;

    Result := ALFormat(cCalibrationStrFormat, [APointID, X, Y, VLatStr, VLonStr], FFormatSettings);
  end;

const
  cDegreeToRadCoeff: Double = 0.017453292519943295769236907684886;
var
  I: Integer;
  VCenter: TPoint;
  VRadius: Double;
  VFileName: string;
  VLL, VLL1, VLL2: TDoublePoint;
  VLocalRect: TRect;
  VFileStream: TFileStream;
  VText: AnsiString;
  VMapName: AnsiString;
  VProjection: AnsiString;
begin
  VLL1 := AProjection.PixelPos2LonLat(ATopLeft);
  VLL2 := AProjection.PixelPos2LonLat(ABottomRight);

  VCenter.Y := (ABottomRight.Y - ((ABottomRight.Y - ATopLeft.Y) div 2));
  VCenter.X := (ABottomRight.X - ((ABottomRight.X - ATopLeft.X) div 2));
  VLL := AProjection.PixelPos2LonLat(VCenter);

  VLocalRect.TopLeft := Point(0, 0);
  VLocalRect.BottomRight := Point(ABottomRight.X - ATopLeft.X, ABottomRight.Y - ATopLeft.Y);

  VRadius := AProjection.ProjectionType.Datum.GetSpheroidRadiusA;

  if AProjection.ProjectionType.ProjectionEPSG = CGELonLatProjectionEPSG then begin
    VProjection := 'Latitude/Longitude';
  end else begin
    VProjection := 'Mercator';
  end;

  VFileName := ChangeFileExt(AFileName, cOziFileExt);
  VMapName := AnsiString(ExtractFileName(AFileName));

  VFileStream := TFileStream.Create(VFileName, fmCreate);
  try
    VText :=
      'OziExplorer Map Data File Version 2.2' + #13#10 +
      VMapName + #13#10 +
      VMapName + #13#10 +
      '1 ,Map Code,' + #13#10 +
      'WGS 84,,   0.0000,   0.0000,WGS 84' + #13#10 +
      'Reserved 1' + #13#10 +
      'Reserved 2' + #13#10 +
      'Magnetic Variation,,,E' + #13#10 +
      'Map Projection,' + VProjection + ',PolyCal,No,AutoCalOnly,No,BSBUseWPX,No' + #13#10 +
      GetPointCalibrationStr(1, VLocalRect.Left, VLocalRect.Top, VLL1.X, VLL1.Y) +
      GetPointCalibrationStr(2, ((VLocalRect.Right - VLocalRect.Left) div 2), VLocalRect.Top, VLL.X, VLL1.Y) +
      GetPointCalibrationStr(3, VLocalRect.Right, VLocalRect.Top, VLL2.X, VLL1.Y) +
      GetPointCalibrationStr(4, VLocalRect.Left, ((VLocalRect.Bottom - VLocalRect.Top) div 2), VLL1.X, VLL.Y) +
      GetPointCalibrationStr(5, ((VLocalRect.Right - VLocalRect.Left) div 2), ((VLocalRect.Bottom - VLocalRect.Top) div 2), VLL.X, VLL.Y) +
      GetPointCalibrationStr(6, VLocalRect.Right, ((VLocalRect.Bottom - VLocalRect.Top) div 2), VLL2.X, VLL.Y) +
      GetPointCalibrationStr(7, VLocalRect.Left, VLocalRect.Bottom, VLL1.X, VLL2.Y) +
      GetPointCalibrationStr(8, ((VLocalRect.Right - VLocalRect.Left) div 2), VLocalRect.Bottom, VLL.X, VLL2.Y) +
      GetPointCalibrationStr(9, VLocalRect.Right, VLocalRect.Bottom, VLL2.X, VLL2.Y);

    for I := 10 to 30 do begin
      VText := VText + ALFormat(cCalibrationEmptyStrFormat, [I], FFormatSettings);
    end;

    VText := VText +
      'Projection Setup,,,,,,,,,,' + #13#10 +
      'Map Feature = MF ; Map Comment = MC     These follow if they exist' + #13#10 +
      'Track File = TF      These follow if they exist' + #13#10 +
      'Moving Map Parameters = MM?    These follow if they exist' + #13#10 +
      'MM0,Yes' + #13#10 +
      'MMPNUM,4' + #13#10 +
      'MMPXY,1,' + ALIntToStr(VLocalRect.Left) + ',' + ALIntToStr(VLocalRect.Top) + #13#10 +
      'MMPXY,2,' + ALIntToStr(VLocalRect.Right) + ',' + ALIntToStr(VLocalRect.Top) + #13#10 +
      'MMPXY,3,' + ALIntToStr(VLocalRect.Right) + ',' + ALIntToStr(VLocalRect.Bottom) + #13#10 +
      'MMPXY,4,' + ALIntToStr(VLocalRect.Left) + ',' + ALIntToStr(VLocalRect.Bottom) + #13#10 +
      'MMPLL,1, ' + DoubleToAnsiStr(VLL1.X) + ', ' + DoubleToAnsiStr(VLL1.Y) + #13#10 +
      'MMPLL,2, ' + DoubleToAnsiStr(VLL2.X) + ', ' + DoubleToAnsiStr(VLL1.Y) + #13#10 +
      'MMPLL,3, ' + DoubleToAnsiStr(VLL2.X) + ', ' + DoubleToAnsiStr(VLL2.Y) + #13#10 +
      'MMPLL,4, ' + DoubleToAnsiStr(VLL1.X) + ', ' + DoubleToAnsiStr(VLL2.Y) + #13#10 +
      'MM1B,' + DoubleToAnsiStr(1 / ((AProjection.GetPixelsFloat / (2 * PI)) / (VRadius * Cos(VLL.Y * cDegreeToRadCoeff)))) + #13#10 +
      'MOP,Map Open Position,0,0' + #13#10 +
      'IWH,Map Image Width/Height,' + ALIntToStr(VLocalRect.Right) + ',' + ALIntToStr(VLocalRect.Bottom) + #13#10;

    VFileStream.WriteBuffer(VText[1], Length(VText));
  finally
    VFileStream.Free;
  end;
end;

end.

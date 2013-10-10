{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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
  ALfcnString,
  i_CoordConverter,
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
  private
    { IMapCalibration }
    function GetName: WideString; safecall;
    function GetDescription: WideString; safecall;
    procedure SaveCalibrationInfo(
      const AFileName: WideString;
      const ATopLeft: TPoint;
      const ABottomRight: TPoint;
      const AZoom: Byte;
      const AConverter: ICoordConverter
    ); safecall;
  public
    constructor Create;
  end;

implementation

uses
  Classes,
  SysUtils,
  t_GeoTypes,
  u_GeoToStr;

const
  cTabFileExt = '.tap';
  cCoordFmtStr: AnsiString = '%.8f';
  cPointFmtStr: AnsiString = '  (%s, %s) (%s, %s) Label "Point %d",' + #13#10;

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

procedure TMapCalibrationTab.SaveCalibrationInfo(
  const AFileName: WideString;
  const ATopLeft: TPoint;
  const ABottomRight: TPoint;
  const AZoom: Byte;
  const AConverter: ICoordConverter
);
var
  VCenter: TPoint;
  VLL, VLL1, VLL2: TDoublePoint;
  VLocalRect: TRect;
  VFileName: string;
  VFileStream: TFileStream;
  VText: AnsiString;
begin
  VCenter.Y := (ABottomRight.Y - ((ABottomRight.Y - ATopLeft.Y) div 2));
  VCenter.X := (ABottomRight.X - ((ABottomRight.X - ATopLeft.X) div 2));

  VLL1 := AConverter.PixelPos2LonLat(ATopLeft, AZoom);
  VLL2 := AConverter.PixelPos2LonLat(ABottomRight, AZoom);
  VLL := AConverter.PixelPos2LonLat(VCenter, AZoom);

  VLocalRect.TopLeft := Point(0, 0);
  VLocalRect.BottomRight := Point(ABottomRight.X - ATopLeft.X, ABottomRight.Y - ATopLeft.Y);

  VFileName := ChangeFileExt(AFileName, cTabFileExt);

  VFileStream := TFileStream.Create(VFileName, fmCreate);
  try
    VText :=
      '!table' + #13#10 +
      '!version 300' + #13#10 +
      '!charset WindowsCyrillic' + #13#10 + #13#10 +
      'Definition Table' + #13#10 +
      '  File "' + UTF8Encode(ExtractFileName(AFileName)) + '"' + #13#10 +
      '  Type "RASTER"' + #13#10 +
      PointToStr(1, VLL1.X, VLL1.Y, VLocalRect.Left, VLocalRect.Top) +
      PointToStr(2, VLL2.X, VLL2.Y, VLocalRect.Right, VLocalRect.Bottom) +
      PointToStr(3, VLL1.X, VLL2.Y, VLocalRect.Left, VLocalRect.Bottom) +
      PointToStr(4, VLL2.X, VLL1.Y, VLocalRect.Right, VLocalRect.Top) +
      PointToStr(5, VLL.X, VLL.Y, ((VLocalRect.Right - VLocalRect.Left) div 2), ((VLocalRect.Bottom - VLocalRect.Top) div 2)) +
      PointToStr(6, VLL.X, VLL1.Y, ((VLocalRect.Right - VLocalRect.Left) div 2), VLocalRect.Top) +
      PointToStr(7, VLL1.X, VLL.Y, VLocalRect.Left, ((VLocalRect.Bottom - VLocalRect.Top) div 2)) +
      PointToStr(8, VLL2.X, VLL.Y, VLocalRect.Right, ((VLocalRect.Bottom - VLocalRect.Top) div 2)) +
      PointToStr(9, VLL.X, VLL2.Y, ((VLocalRect.Right - VLocalRect.Left) div 2), VLocalRect.Bottom) +
      ' CoordSys Earth Projection 1, 104' + #13#10 +
      ' Units "degree"' + #13#10;

    VFileStream.WriteBuffer(VText[1], Length(VText));
  finally
    VFileStream.Free;
  end;
end;

end.

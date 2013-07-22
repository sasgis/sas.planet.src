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
  i_MapCalibration,
  u_BaseInterfacedObject;

type
  TMapCalibrationOzi = class(TBaseInterfacedObject, IMapCalibration)
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
  ALfcnString,
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
  xy: TPoint;
  rad: Double;
  lat, lon: array[1..3] of Double;
  i: integer;
  lats, lons: array[1..3] of AnsiString;
  VFormat: TALFormatSettings;
  VFileName: String;
  VLL1, VLL2: TDoublePoint;
  VLL: TDoublePoint;
  VLocalRect: TRect;
  VFileStream: TFileStream;
  VText: AnsiString;
begin
  VFormat.DecimalSeparator := '.';
  VFileName := ChangeFileExt(AFileName, '.map');
  VFileStream := TFileStream.Create(VFileName, fmCreate);
  try
    VText := '';
    VText := VText + 'OziExplorer Map Data File Version 2.2' + #13#10;
    VText := VText + 'Created by SAS.Planet' + #13#10;
    VText := VText + UTF8Encode(ExtractFileName(AFileName)) + #13#10;
    VText := VText + '1 ,Map Code,' + #13#10;
    VText := VText + 'WGS 84,,   0.0000,   0.0000,WGS 84' + #13#10;
    VText := VText + 'Reserved 1' + #13#10;
    VText := VText + 'Reserved 2' + #13#10;
    VText := VText + 'Magnetic Variation,,,E' + #13#10;
    VText := VText + 'Map Projection,Mercator,PolyCal,No,AutoCalOnly,No,BSBUseWPX,No' + #13#10;


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
      lons[i] := ALinttostr(trunc(abs(lon[i]))) + ', ' + ALFloatToStr(Frac(abs(lon[i])) * 60, VFormat);
      if lon[i] < 0 then begin
        lons[i] := lons[i] + ',W';
      end else begin
        lons[i] := lons[i] + ',E';
      end;
    end;
    for i := 1 to 3 do begin
      lats[i] := ALinttostr(trunc(abs(lat[i]))) + ', ' + ALFloatToStr(Frac(abs(lat[i])) * 60, VFormat);
      if lat[i] < 0 then begin
        lats[i] := lats[i] + ',S';
      end else begin
        lats[i] := lats[i] + ',N';
      end;
    end;
    VLocalRect.TopLeft := Point(0, 0);
    VLocalRect.BottomRight := Point(xy2.X - xy1.X, xy2.y - xy1.y);

    VText := VText + 'Point01,xy,    ' + ALinttostr(VLocalRect.Left) + ', ' + ALinttostr(VLocalRect.Top) + ',in, deg, ' + lats[1] + ', ' + lons[1] + ', grid,   ,           ,           ,N' + #13#10;
    VText := VText + 'Point02,xy,    ' + ALinttostr(VLocalRect.Right) + ', ' + ALinttostr(VLocalRect.Bottom) + ',in, deg, ' + lats[3] + ', ' + lons[3] + ', grid,   ,           ,           ,N' + #13#10;
    VText := VText + 'Point03,xy,    ' + ALinttostr(VLocalRect.Left) + ', ' + ALinttostr(VLocalRect.Bottom) + ',in, deg, ' + lats[3] + ', ' + lons[1] + ', grid,   ,           ,           ,N' + #13#10;
    VText := VText + 'Point04,xy,    ' + ALinttostr(VLocalRect.Right) + ', ' + ALinttostr(VLocalRect.Top) + ',in, deg, ' + lats[1] + ', ' + lons[3] + ', grid,   ,           ,           ,N' + #13#10;
    VText := VText + 'Point05,xy,    ' + ALinttostr((VLocalRect.Right - VLocalRect.Left) div 2) + ', ' + ALinttostr((VLocalRect.Bottom - VLocalRect.Top) div 2) + ',in, deg, ' + lats[2] + ', ' + lons[2] + ', grid,   ,           ,           ,N' + #13#10;
    VText := VText + 'Point06,xy,    ' + ALinttostr((VLocalRect.Right - VLocalRect.Left) div 2) + ', ' + ALinttostr(VLocalRect.Top) + ',in, deg, ' + lats[1] + ', ' + lons[2] + ', grid,   ,           ,           ,N' + #13#10;
    VText := VText + 'Point07,xy,    ' + ALinttostr(VLocalRect.Left) + ', ' + ALinttostr((VLocalRect.Bottom - VLocalRect.Top) div 2) + ',in, deg, ' + lats[2] + ', ' + lons[1] + ', grid,   ,           ,           ,N' + #13#10;
    VText := VText + 'Point08,xy,    ' + ALinttostr(VLocalRect.Right) + ', ' + ALinttostr((VLocalRect.Bottom - VLocalRect.Top) div 2) + ',in, deg, ' + lats[2] + ', ' + lons[3] + ', grid,   ,           ,           ,N' + #13#10;
    VText := VText + 'Point09,xy,    ' + ALinttostr((VLocalRect.Right - VLocalRect.Left) div 2) + ', ' + ALinttostr(VLocalRect.Bottom) + ',in, deg, ' + lats[3] + ', ' + lons[2] + ', grid,   ,           ,           ,N' + #13#10;
    for i := 10 to 30 do begin
      VText := VText + 'Point' + ALinttostr(i) + ',xy,     ,     ,in, deg,    ,        ,N,    ,        ,W, grid,   ,           ,           ,N' + #13#10;
    end;

    VText := VText + 'Projection Setup,,,,,,,,,,' + #13#10;
    VText := VText + 'Map Feature = MF ; Map Comment = MC     These follow if they exist' + #13#10;
    VText := VText + 'Track File = TF      These follow if they exist' + #13#10;
    VText := VText + 'Moving Map Parameters = MM?    These follow if they exist' + #13#10;
    VText := VText + 'MM0,Yes' + #13#10;
    VText := VText + 'MMPNUM,4' + #13#10;
    VText := VText + 'MMPXY,1,' + ALinttostr(VLocalRect.Left) + ',' + ALinttostr(VLocalRect.Top) + #13#10;
    VText := VText + 'MMPXY,2,' + ALinttostr(VLocalRect.Right) + ',' + ALinttostr(VLocalRect.Top) + #13#10;
    VText := VText + 'MMPXY,3,' + ALinttostr(VLocalRect.Right) + ',' + ALinttostr(VLocalRect.Bottom) + #13#10;
    VText := VText + 'MMPXY,4,' + ALinttostr(VLocalRect.Left) + ',' + ALinttostr(VLocalRect.Bottom) + #13#10;

    VText := VText + 'MMPLL,1, ' + ALFloatToStr(lon[1], VFormat) + ', ' + ALFloatToStr(lat[1], VFormat) + #13#10;
    VText := VText + 'MMPLL,2, ' + ALFloatToStr(lon[3], VFormat) + ', ' + ALFloatToStr(lat[1], VFormat) + #13#10;
    VText := VText + 'MMPLL,3, ' + ALFloatToStr(lon[3], VFormat) + ', ' + ALFloatToStr(lat[3], VFormat) + #13#10;
    VText := VText + 'MMPLL,4, ' + ALFloatToStr(lon[1], VFormat) + ', ' + ALFloatToStr(lat[3], VFormat) + #13#10;

    rad := AConverter.Datum.GetSpheroidRadiusA;

    VText := VText + 'MM1B,' + ALFloatToStr(1 / ((AConverter.PixelsAtZoomFloat(AZoom) / (2 * PI)) / (rad * cos(lat[2] * D2R))), VFormat) + #13#10;
    VText := VText + 'MOP,Map Open Position,0,0' + #13#10;
    VText := VText + 'IWH,Map Image Width/Height,' + ALinttostr(VLocalRect.Right) + ',' + ALinttostr(VLocalRect.Bottom) + #13#10;

    VFileStream.WriteBuffer(VText[1], Length(VText));
  finally
    VFileStream.Free;
  end;
end;

end.

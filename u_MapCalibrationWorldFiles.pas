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

unit u_MapCalibrationWorldFiles;

interface

uses
  Types,
  i_CoordConverter,
  i_MapCalibration,
  u_BaseInterfacedObject;

type
  TMapCalibrationWorldFiles = class(TBaseInterfacedObject, IMapCalibration)
  private
    procedure SavePrjFile(
      const AFileName: WideString;
      const AConverter: ICoordConverter
    );
    procedure SaveAuxXmlFile(
      const AFileName: WideString;
      const AConverter: ICoordConverter
    );
    procedure SaveWFile(
      const AFileName: WideString;
      const xy1, xy2: TPoint;
      AZoom: byte;
      const AConverter: ICoordConverter
    );
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
  t_GeoTypes,
  u_GeoToStr,
  u_GeoFun;

function GetProj(const AConverter: ICoordConverter): UTF8String;
begin
  case AConverter.GetProjectionEPSG of
    3785: begin
      Result :=
        'PROJCS["Popular Visualisation CRS / Mercator",' + #13#10 +
        'GEOGCS["Popular Visualisation CRS",' + #13#10 +
        'DATUM["Popular_Visualisation_Datum",' + #13#10 +
        'SPHEROID["Popular Visualisation Sphere",6378137,0,' + #13#10 +
        'AUTHORITY["EPSG","7059"]],' + #13#10 +
        'TOWGS84[0,0,0,0,0,0,0],' + #13#10 +
        'AUTHORITY["EPSG","6055"]],' + #13#10 +
        'PRIMEM["Greenwich",0,' + #13#10 +
        'AUTHORITY["EPSG","8901"]],' + #13#10 +
        'UNIT["degree",0.01745329251994328,' + #13#10 +
        'AUTHORITY["EPSG","9122"]],' + #13#10 +
        'AUTHORITY["EPSG","4055"]],' + #13#10 +
        'UNIT["metre",1,' + #13#10 +
        'AUTHORITY["EPSG","9001"]],' + #13#10 +
        'PROJECTION["Mercator_1SP"],' + #13#10 +
        'PARAMETER["central_meridian",0],' + #13#10 +
        'PARAMETER["scale_factor",1],' + #13#10 +
        'PARAMETER["false_easting",0],' + #13#10 +
        'PARAMETER["false_northing",0],' + #13#10 +
        'AUTHORITY["EPSG","3785"],' + #13#10 +
        'AXIS["X",EAST],' + #13#10 +
        'AXIS["Y",NORTH]]';
    end;
    53004: begin
      Result :=
        'PROJCS["Sphere_Mercator",' + #13#10 +
        'GEOGCS["GCS_Sphere",' + #13#10 +
        'DATUM["Not_specified_based_on_Authalic_Sphere",' + #13#10 +
        'SPHEROID["Sphere",6371000,0]],' + #13#10 +
        'PRIMEM["Greenwich",0],' + #13#10 +
        'UNIT["Degree",0.017453292519943295]],' + #13#10 +
        'PROJECTION["Mercator_1SP"],' + #13#10 +
        'PARAMETER["False_Easting",0],' + #13#10 +
        'PARAMETER["False_Northing",0],' + #13#10 +
        'PARAMETER["Central_Meridian",0],' + #13#10 +
        'PARAMETER["Standard_Parallel_1",0],' + #13#10 +
        'UNIT["Meter",1],' + #13#10 +
        'AUTHORITY["EPSG","53004"]]';
    end;
    3395: begin
      Result :=
        'PROJCS["WGS 84 / World Mercator",' + #13#10 +
        'GEOGCS["WGS 84",' + #13#10 +
        'DATUM["WGS_1984",' + #13#10 +
        'SPHEROID["WGS 84",6378137,298.257223563,' + #13#10 +
        'AUTHORITY["EPSG","7030"]],' + #13#10 +
        'AUTHORITY["EPSG","6326"]],' + #13#10 +
        'PRIMEM["Greenwich",0,' + #13#10 +
        'AUTHORITY["EPSG","8901"]],' + #13#10 +
        'UNIT["degree",0.01745329251994328,' + #13#10 +
        'AUTHORITY["EPSG","9122"]],' + #13#10 +
        'AUTHORITY["EPSG","4326"]],' + #13#10 +
        'UNIT["metre",1,' + #13#10 +
        'AUTHORITY["EPSG","9001"]],' + #13#10 +
        'PROJECTION["Mercator_1SP"],' + #13#10 +
        'PARAMETER["central_meridian",0],' + #13#10 +
        'PARAMETER["scale_factor",1],' + #13#10 +
        'PARAMETER["false_easting",0],' + #13#10 +
        'PARAMETER["false_northing",0],' + #13#10 +
        'AUTHORITY["EPSG","3395"],' + #13#10 +
        'AXIS["Easting",EAST],' + #13#10 +
        'AXIS["Northing",NORTH]]';
    end;
    4326: begin
      Result :=
        'GEOGCS["WGS 84",' + #13#10 +
        'DATUM["WGS_1984",' + #13#10 +
        'SPHEROID["WGS 84",6378137,298.257223563,' + #13#10 +
        'AUTHORITY["EPSG","7030"]],' + #13#10 +
        'AUTHORITY["EPSG","6326"]],' + #13#10 +
        'PRIMEM["Greenwich",0,' + #13#10 +
        'AUTHORITY["EPSG","8901"]],' + #13#10 +
        'UNIT["degree",0.01745329251994328,' + #13#10 +
        'AUTHORITY["EPSG","9122"]],' + #13#10 +
        'AUTHORITY["EPSG","4326"]]';
    end;
  else begin
    Result := '';
  end;
  end;
end;

{ TMapCalibrationWorldFiles }

function TMapCalibrationWorldFiles.GetDescription: WideString;
begin
  Result := 'Привязка при помощи World файла и файлов с описанием проекции';
end;

function TMapCalibrationWorldFiles.GetName: WideString;
begin
  Result := '.w';
end;

procedure TMapCalibrationWorldFiles.SaveAuxXmlFile(
  const AFileName: WideString;
  const AConverter: ICoordConverter
);
var
  AuxXmkfile: TMemoryStream;
  VStr: UTF8String;
begin
  AuxXmkfile := TMemoryStream.create;
  try
    VStr := AnsiToUtf8('<PAMDataset>' + #13#10 + '<SRS>');
    VStr := VStr + GetProj(AConverter);
    VStr := VStr + AnsiToUtf8('</SRS>' + #13#10 + '<Metadata>' + #13#10 + '<MDI key="PyramidResamplingType">NEAREST</MDI>' + #13#10 + '</Metadata>' + #13#10 + '</PAMDataset>');
    AuxXmkfile.Write(VStr[1], length(VStr));
    AuxXmkfile.SaveToFile(AFileName + '.aux.xml');
  finally
    AuxXmkfile.Free;
  end;
end;

procedure TMapCalibrationWorldFiles.SaveCalibrationInfo(
  const AFileName: WideString;
  const xy1, xy2: TPoint;
  AZoom: byte;
  const AConverter: ICoordConverter
);
begin
  SaveWFile(AFileName, xy1, xy2, AZoom, AConverter);
  SavePrjFile(AFileName, AConverter);
  SaveAuxXmlFile(AFileName, AConverter);
end;

procedure TMapCalibrationWorldFiles.SavePrjFile(
  const AFileName: WideString;
  const AConverter: ICoordConverter
);
var
  VprojInfo: UTF8String;
  VFileName: string;
  VFileStream: TFileStream;
begin
  VFileName := ChangeFileExt(AFileName, '.prj');
  VFileStream := TFileStream.Create(VFileName, fmCreate);
  try
    VprojInfo := GetProj(AConverter);
    VFileStream.WriteBuffer(VprojInfo[1], Length(VprojInfo));
  finally
    VFileStream.Free;
  end;
end;

procedure TMapCalibrationWorldFiles.SaveWFile(
  const AFileName: WideString;
  const xy1, xy2: TPoint;
  AZoom: byte;
  const AConverter: ICoordConverter
);
var
  ll1, ll2: TDoublePoint;
  CellX, CellY, OrigX, OrigY: Double;
  VText: AnsiString;
  VFileName: string;
  VFileStream: TFileStream;
begin
  VFileName := AFileName + 'w';
  VFileStream := TFileStream.Create(VFileName, fmCreate);
  try
    ll1 := AConverter.PixelPos2LonLat(xy1, AZoom);
    ll2 := AConverter.PixelPos2LonLat(xy2, AZoom);
    CalculateWFileParams(ll1, ll2, xy2.X - xy1.X, xy2.Y - xy1.Y, AConverter, CellX, CellY, OrigX, OrigY);
    VText := '';
    VText := VText + R2AnsiStrPoint(CellX) + #13#10;
    VText := VText + '0' + #13#10;
    VText := VText + '0' + #13#10;
    VText := VText + R2AnsiStrPoint(CellY) + #13#10;
    VText := VText + R2AnsiStrPoint(OrigX) + #13#10;
    VText := VText + R2AnsiStrPoint(OrigY) + #13#10;
    VFileStream.WriteBuffer(VText[1], Length(VText));
  finally
    VFileStream.Free;
  end;
end;

end.

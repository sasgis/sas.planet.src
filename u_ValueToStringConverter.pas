{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_ValueToStringConverter;

interface

uses
  t_GeoTypes,
  t_CommonTypes,
  i_ValueToStringConverter;

type
  TValueToStringConverter = class(TInterfacedObject, IValueToStringConverter)
  private
    FDistStrFormat: TDistStrFormat;
    FIsLatitudeFirst: Boolean;
    FDegrShowFormat: TDegrShowFormat;
    FUnitsKb: string;
    FUnitsMb: string;
    FUnitsGb: string;
    FUnitsKm: string;
    FUnitsMeters: string;
    FUnitsKmph: string;
    FUnitsSqKm: string;
    FUnitsSqMeters: string;
    FEastMarker: string;
    FWestMarker: string;
    FNorthMarker: string;
    FSouthMarker: string;
  private
    function DegrToStr(ADegr: Double): string;
    function GetLatitudeMarker(ADegr: Double): string;
    function GetLongitudeMarker(ADegr: Double): string;
  protected
    function DataSizeConvert(ASizeInKb: Double): string;
    function DistConvert(ADistInMeters: Double): string;
    function DistPerPixelConvert(ADistPerPixelInMeters: Double): string;
    function AreaConvert(AAreaInSqm: Double): string;
    function SpeedConvert(AKmph: Double): string;
    function AltitudeConvert(AMeters: Double): string;
    function LonLatConvert(ALonLat: TDoublePoint): string;
    function LonConvert(ALon: Double): string;
    function LatConvert(ALat: Double): string;
  public
    constructor Create(
      ADistStrFormat: TDistStrFormat;
      AIsLatitudeFirst: Boolean;
      ADegrShowFormat: TDegrShowFormat
    );
  end;
implementation

uses
  SysUtils,
  u_ResStrings;

{ TValueToStringConverter }

constructor TValueToStringConverter.Create(ADistStrFormat: TDistStrFormat;
  AIsLatitudeFirst: Boolean; ADegrShowFormat: TDegrShowFormat);
begin
  inherited Create;
  FDistStrFormat := ADistStrFormat;
  FIsLatitudeFirst := AIsLatitudeFirst;
  FDegrShowFormat := ADegrShowFormat;
  FUnitsKb := SAS_UNITS_kb;
  FUnitsMb := SAS_UNITS_mb;
  FUnitsGb := SAS_UNITS_gb;
  FUnitsKm := SAS_UNITS_km;
  FUnitsMeters := SAS_UNITS_m;
  FUnitsKmph := SAS_UNITS_kmperh;
  FUnitsSqKm := SAS_UNITS_km2;
  FUnitsSqMeters := SAS_UNITS_m2;
  FNorthMarker := 'N';
  FEastMarker := 'E';
  FWestMarker := 'W';
  FSouthMarker := 'S'
end;

function TValueToStringConverter.AltitudeConvert(AMeters: Double): string;
begin
  Result := FormatFloat('0.0', AMeters) + ' ' + FUnitsMeters; 
end;

function TValueToStringConverter.AreaConvert(AAreaInSqm: Double): string;
begin
  if AAreaInSqm <= 1000000 then begin
    Result := FormatFloat('0.00', AAreaInSqm) + ' ' + FUnitsSqMeters;
  end else begin
    Result := FormatFloat('0.00', AAreaInSqm / 1000000) + ' ' + FUnitsSqKm;
  end;
end;

function TValueToStringConverter.DataSizeConvert(ASizeInKb: Double): string;
begin
  if ASizeInKb > 1048576 then begin
    result := FormatFloat('0.0', ASizeInKb/1048576) + ' ' + FUnitsGb;
  end else begin
    if ASizeInKb > 1024 then begin
      result := FormatFloat('0.0', ASizeInKb/1024) + ' ' + FUnitsMb;
    end else begin
      result := FormatFloat('0.0', ASizeInKb) + ' ' + FUnitsKb;
    end;
  end;
end;

function TValueToStringConverter.DegrToStr(ADegr: Double): string;
var
  VDegr: Double;
  VInt: Integer;
  VValue: Integer;
begin
  VDegr := abs(ADegr);
  case FDegrShowFormat of
    dshCharDegrMinSec, dshSignDegrMinSec: begin
      VValue := Trunc(VDegr * 60 * 60 * 100 + 0.005);
      VInt := Trunc(VValue / (60 * 60 * 100));
      VValue := VValue - VInt * (60 * 60 * 100);
      result := IntToStr(VInt) + '°';

      VInt := Trunc(VValue / (60 * 100));
      VValue := VValue - VInt * (60 * 100);

      if VInt < 10 then begin
        Result := result + '0' + IntToStr(VInt) + '''';
      end else begin
        Result := result + IntToStr(VInt) + '''';
      end;

      Result := Result + FormatFloat('00.00', VValue / 100) + '"';
    end;
    dshCharDegrMin, dshSignDegrMin: begin
      VValue := Trunc(VDegr * 60 * 10000 + 0.00005);
      VInt := Trunc(VValue / (60 * 10000));
      VValue := VValue - VInt * (60 * 10000);
      Result := IntToStr(VInt)+'°';
      Result := Result + FormatFloat('00.0000', VValue / 10000) + '''';
    end;
    dshCharDegr, dshSignDegr: begin
      Result := FormatFloat('0.000000', VDegr) + '°';
    end;
  end;
end;

function TValueToStringConverter.DistConvert(ADistInMeters: Double): string;
var
  VKmDist: Double;
begin
  Result := '';
  case FDistStrFormat of
    dsfKmAndM: begin
      if ADistInMeters > 1000 then begin
        VKmDist :=ADistInMeters/1000;
        Result := IntToStr(Trunc(VKmDist)) + ' ' + FUnitsKm + ' ';
        Result := Result + FormatFloat('0.00', frac(VKmDist)*1000) + ' ' + FUnitsMeters;
      end else begin
        Result := FormatFloat('0.00', ADistInMeters) + ' ' + FUnitsMeters;
      end;
    end;
    dsfSimpleKM: begin
      if ADistInMeters<10000 then begin
        Result := FormatFloat('0.00', ADistInMeters) + ' ' + FUnitsMeters;
      end else begin
        Result := FormatFloat('0.00', ADistInMeters/1000) + ' ' + FUnitsKm;
      end;
    end;
  end;
end;

function TValueToStringConverter.DistPerPixelConvert(
  ADistPerPixelInMeters: Double): string;
begin
  Result := DistConvert(ADistPerPixelInMeters) + SAS_UNITS_mperp;
end;

function TValueToStringConverter.GetLatitudeMarker(ADegr: Double): string;
begin
  case FDegrShowFormat of
    dshCharDegrMinSec, dshCharDegrMin, dshCharDegr: begin
      if ADegr > 0 then begin
        result := FNorthMarker;
      end else begin
        result := FSouthMarker;
      end;
    end;
    dshSignDegrMinSec, dshSignDegrMin, dshSignDegr: begin
      if ADegr > 0 then begin
        result := '';
      end else begin
        result := '-';
      end;
    end;
  end;
end;

function TValueToStringConverter.GetLongitudeMarker(ADegr: Double): string;
begin
  case FDegrShowFormat of
    dshCharDegrMinSec, dshCharDegrMin, dshCharDegr: begin
      if ADegr > 0 then begin
        result := FEastMarker;
      end else begin
        result := FWestMarker;
      end;
    end;
    dshSignDegrMinSec, dshSignDegrMin, dshSignDegr: begin
      if ADegr > 0 then begin
        result := '';
      end else begin
        result := '-';
      end;
    end;
  end;
end;

function TValueToStringConverter.LonLatConvert(ALonLat: TDoublePoint): string;
var
  VLatStr: string;
  VLonStr: string;
begin
  VLatStr := GetLatitudeMarker(ALonLat.Y) + DegrToStr(ALonLat.Y);
  VLonStr := GetLongitudeMarker(ALonLat.X) + DegrToStr(ALonLat.X);
  if FIsLatitudeFirst then begin
    Result := VLatStr + ' ' + VLonStr;
  end else begin
    Result := VLonStr + ' ' + VLatStr;
  end;
end;

function TValueToStringConverter.LonConvert(ALon: Double): string;
begin
  result := GetLongitudeMarker(ALon) + DegrToStr(ALon);
end;

function TValueToStringConverter.LatConvert(ALat: Double): string;
begin
  result := GetLatitudeMarker(ALat) + DegrToStr(ALat);
end;

function TValueToStringConverter.SpeedConvert(AKmph: Double): string;
begin
  Result := FormatFloat('0.0', AKmph) + ' ' + FUnitsKmph;
end;

end.

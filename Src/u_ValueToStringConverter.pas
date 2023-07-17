{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ValueToStringConverter;

interface

uses
  t_GeoTypes,
  t_CommonTypes,
  i_ValueToStringConverter,
  u_BaseInterfacedObject;

type
  TValueToStringConverter = class(TBaseInterfacedObject, IValueToStringConverter)
  private
    FDistStrFormat: TDistStrFormat;
    FAreaShowFormat: TAreaStrFormat;

    FUnitsKb: string;
    FUnitsMb: string;
    FUnitsGb: string;

    FUnitsKm: string;
    FUnitsMeter: string;
    FUnitsCentimeter: string;

    FUnitsMile: string;
    FUnitsYard: string;
    FUnitsFoot: string;
    FUnitsInch: string;
    FUnitsNauticalMile: string;

    FUnitsKmph: string;

    FUnitsHa: string;
    FUnitsSqKm: string;
    FUnitsSqMeters: string;
  private
    function DataSizeConvert(const ASizeInKb: Double): string;
    function DistConvert(const ADistInMeters: Double): string;
    function DistPerPixelConvert(const ADistPerPixelInMeters: Double): string;
    function AreaConvert(const AAreaInSqm: Double): string;
    function SpeedConvert(const AKmph: Double): string;
    function AltitudeConvert(const AMeters: Double): string;
  public
    constructor Create(
      ADistStrFormat: TDistStrFormat;
      AAreaShowFormat: TAreaStrFormat
    );
  end;

implementation

uses
  Math,
  SysUtils,
  StrUtils,
  u_ResStrings;

{ TValueToStringConverter }

constructor TValueToStringConverter.Create(
  ADistStrFormat: TDistStrFormat;
  AAreaShowFormat: TAreaStrFormat
);
begin
  inherited Create;
  FDistStrFormat := ADistStrFormat;
  FAreaShowFormat := AAreaShowFormat;

  FUnitsKb := SAS_UNITS_kb;
  FUnitsMb := SAS_UNITS_mb;
  FUnitsGb := SAS_UNITS_gb;

  FUnitsKm := SAS_UNITS_km;
  FUnitsMeter := SAS_UNITS_m;
  FUnitsCentimeter := SAS_UNITS_sm;

  FUnitsMile := SAS_UNITS_mile;
  FUnitsYard := SAS_UNITS_yard;
  FUnitsFoot := SAS_UNITS_foot;
  FUnitsInch := SAS_UNITS_inch;
  FUnitsNauticalMile := SAS_UNITS_nauticalmile;

  FUnitsKmph := SAS_UNITS_kmperh;

  FUnitsSqKm := SAS_UNITS_km2;
  FUnitsHa := SAS_UNITS_Ha;
  FUnitsSqMeters := SAS_UNITS_m2;
end;

function TValueToStringConverter.AltitudeConvert(const AMeters: Double): string;
begin
  if IsNan(AMeters) then begin
    Result := 'NAN';
    Exit;
  end;

  Result := FormatFloat('0.0', AMeters) + ' ' + FUnitsMeter;
end;

function TValueToStringConverter.AreaConvert(const AAreaInSqm: Double): string;
begin
  if IsNan(AAreaInSqm) then begin
    Result := 'NAN';
    Exit;
  end;

  case FAreaShowFormat of
    asfAuto: begin
      if AAreaInSqm <= 1000000 then begin
        Result := FormatFloat('0.00', AAreaInSqm) + ' ' + FUnitsSqMeters;
      end else begin
        Result := FormatFloat('0.00', AAreaInSqm / 1000000) + ' ' + FUnitsSqKm;
      end;
    end;
    asfSqM: begin
      if AAreaInSqm <= 100 then begin
        Result := FormatFloat('0.00', AAreaInSqm) + ' ' + FUnitsSqMeters;
      end else begin
        Result := FormatFloat('0.', AAreaInSqm) + ' ' + FUnitsSqMeters;
      end;
    end;
    asfSqKm: begin
      if AAreaInSqm <= 1000000 then begin
        Result := FormatFloat('0.000000', AAreaInSqm / 1000000) + ' ' + FUnitsSqKm;
      end else begin
        Result := FormatFloat('0.00', AAreaInSqm / 1000000) + ' ' + FUnitsSqKm;
      end;
    end;
    asfHa: begin
      if AAreaInSqm <= 1000000 then begin
        Result := FormatFloat('0.0000', AAreaInSqm / 10000) + ' ' + FUnitsHa;
      end else begin
        Result := FormatFloat('0.00', AAreaInSqm / 10000) + ' ' + FUnitsHa;
      end;
    end;
  end;
end;

function TValueToStringConverter.DataSizeConvert(const ASizeInKb: Double): string;
begin
  if ASizeInKb > 1048576 then begin
    result := FormatFloat('0.0', ASizeInKb / 1048576) + ' ' + FUnitsGb;
  end else begin
    if ASizeInKb > 1024 then begin
      result := FormatFloat('0.0', ASizeInKb / 1024) + ' ' + FUnitsMb;
    end else begin
      result := FormatFloat('0.0', ASizeInKb) + ' ' + FUnitsKb;
    end;
  end;
end;

function TValueToStringConverter.DistConvert(const ADistInMeters: Double): string;
var
  VDistInMeters: Double;
  VKmDist: Double;
begin
  if IsNan(ADistInMeters) then begin
    Result := 'NAN';
    Exit;
  end;
  VDistInMeters := Abs(ADistInMeters);
  Result := '';
  case FDistStrFormat of
    dsfKmAndM: begin
      if VDistInMeters > 1000 then begin
        VKmDist := VDistInMeters / 1000;
        Result := IntToStr(Trunc(VKmDist)) + ' ' + FUnitsKm + ' ';
        Result := Result + FormatFloat('0.00', frac(VKmDist) * 1000) + ' ' + FUnitsMeter;
      end else begin
        Result := FormatFloat('0.00', VDistInMeters) + ' ' + FUnitsMeter;
      end;
    end;

    dsfKmOrM: begin
      if VDistInMeters < 10000 then begin
        Result := FormatFloat('0.00', VDistInMeters) + ' ' + FUnitsMeter;
      end else begin
        Result := FormatFloat('0.00', VDistInMeters / 1000) + ' ' + FUnitsKm;
      end;
    end;

    dsfSimpleKm: begin
      Result := FloatToStrF(VDistInMeters * 1 / 1000, ffNumber, 15, 2) + ' ' + FUnitsKm;
    end;

    dsfSimpleMeter: begin
      Result := FloatToStrF(VDistInMeters * 1, ffNumber, 15, 2) + ' ' + FUnitsMeter;
    end;

    dsfSimpleCentimeter: begin
      Result := FloatToStrF(VDistInMeters * 100, ffNumber, 15, 2) + ' ' + FUnitsCentimeter;
    end;

    dsfSimpleMile: begin
      Result := FloatToStrF(VDistInMeters * 1 / 1609.344, ffNumber, 15, 2) + ' ' + FUnitsMile;
    end;

    dsfSimpleYard: begin
      Result := FloatToStrF(VDistInMeters * 1 / 0.9144, ffNumber, 15, 2) + ' ' + FUnitsYard;
    end;

    dsfSimpleFoot: begin
      Result := FloatToStrF(VDistInMeters * 1 / 0.3048, ffNumber, 15, 2) + ' ' + FUnitsFoot;
    end;

    dsfSimpleInch: begin
      Result := FloatToStrF(VDistInMeters * 1 / 0.0254, ffNumber, 15, 2) + ' ' + FUnitsInch;
    end;

    dsfSimpleNauticalMile: begin
      Result := FloatToStrF(VDistInMeters * 1 / 1852.0, ffNumber, 15, 2) + ' ' + FUnitsNauticalMile;
    end;
  end;

  if ADistInMeters < 0 then begin
    Result := '-' + Result;
  end;
end;

function TValueToStringConverter.DistPerPixelConvert(
  const ADistPerPixelInMeters: Double
): string;
begin
  Result := DistConvert(ADistPerPixelInMeters) + SAS_UNITS_mperp;
end;

function TValueToStringConverter.SpeedConvert(const AKmph: Double): string;
begin
  if IsNan(AKmph) then begin
    Result := 'NAN';
    Exit;
  end;

  Result := FormatFloat('0.0', AKmph) + ' ' + FUnitsKmph;
end;

end.

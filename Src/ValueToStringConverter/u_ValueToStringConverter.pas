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
  SysUtils,
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

    FUnitsAcr: string;
    FUnitsSqMi: string;
    FUnitsSqNMi: string;
    FUnitsSqFt: string;
    FUnitsSqYd: string;
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
  StrUtils,
  u_ResStrings;

const
  CInvalidValue = 'NaN';

function ValToNum(const AValue: Double; const AUnits: string; const ADigits: Integer = 2): string; inline;
begin
  Result := FloatToStrF(AValue, ffNumber, 15, ADigits) + ' ' + AUnits;
end;

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

  FUnitsHa := SAS_UNITS_Ha;
  FUnitsSqKm := SAS_UNITS_km2;
  FUnitsSqMeters := SAS_UNITS_m2;

  FUnitsAcr := SAS_UNITS_acr;
  FUnitsSqMi := SAS_UNITS_mi2;
  FUnitsSqNMi := SAS_UNITS_nmi2;
  FUnitsSqFt := SAS_UNITS_ft2;
  FUnitsSqYd := SAS_UNITS_yd2;
end;

function TValueToStringConverter.AltitudeConvert(const AMeters: Double): string;
begin
  if IsNan(AMeters) then begin
    Result := CInvalidValue;
    Exit;
  end;

  Result := ValToNum(AMeters, FUnitsMeter, 1);
end;

function TValueToStringConverter.AreaConvert(const AAreaInSqm: Double): string;
begin
  if IsNan(AAreaInSqm) then begin
    Result := CInvalidValue;
    Exit;
  end;

  case FAreaShowFormat of
    asfAuto: begin
      if AAreaInSqm <= 1000000 then begin
        Result := ValToNum(AAreaInSqm, FUnitsSqMeters);
      end else begin
        Result := ValToNum(AAreaInSqm / 1000000, FUnitsSqKm);
      end;
    end;

    asfSqM: begin
      if AAreaInSqm <= 100 then begin
        Result := ValToNum(AAreaInSqm, FUnitsSqMeters);
      end else begin
        Result := ValToNum(AAreaInSqm, FUnitsSqMeters, 0);
      end;
    end;

    asfSqKm: begin
      if AAreaInSqm <= 1000000 then begin
        Result := ValToNum(AAreaInSqm / 1000000, FUnitsSqKm, 6);
      end else begin
        Result := ValToNum(AAreaInSqm / 1000000, FUnitsSqKm);
      end;
    end;

    asfHa: begin
      if AAreaInSqm <= 1000000 then begin
        Result := ValToNum(AAreaInSqm / 10000, FUnitsHa, 4);
      end else begin
        Result := ValToNum(AAreaInSqm / 10000, FUnitsHa);
      end;
    end;

    asfSqFoot: begin
      Result := ValToNum(AAreaInSqm / 0.09290304, FUnitsSqFt);
    end;

    asfSqYard: begin
      Result := ValToNum(AAreaInSqm / 0.83612736, FUnitsSqYd);
    end;

    asfSqMile: begin
      Result := ValToNum(AAreaInSqm / 2589988.110336, FUnitsSqMi);
    end;

    asfSqNauticalMile: begin
      Result := ValToNum(AAreaInSqm / 3429904, FUnitsSqNMi);
    end;

    asfSqAcr: begin
      Result := ValToNum(AAreaInSqm / 4046.8564224, FUnitsAcr);
    end;
  else
    Assert(False);
  end;
end;

function TValueToStringConverter.DataSizeConvert(const ASizeInKb: Double): string;
begin
  if ASizeInKb > 1048576 then begin
    Result := ValToNum(ASizeInKb / 1048576, FUnitsGb, 1);
  end else begin
    if ASizeInKb > 1024 then begin
      Result := ValToNum(ASizeInKb / 1024, FUnitsMb, 1);
    end else begin
      Result := ValToNum(ASizeInKb, FUnitsKb, 1);
    end;
  end;
end;

function TValueToStringConverter.DistConvert(const ADistInMeters: Double): string;
var
  VDistInMeters: Double;
  VKmDist: Double;
begin
  if IsNan(ADistInMeters) then begin
    Result := CInvalidValue;
    Exit;
  end;

  Result := '';
  VDistInMeters := Abs(ADistInMeters);

  case FDistStrFormat of
    dsfKmAndM: begin
      if VDistInMeters > 1000 then begin
        VKmDist := VDistInMeters / 1000;
        Result := ValToNum(Trunc(VKmDist), FUnitsKm, 0) + ' ' +
                  ValToNum(Frac(VKmDist) * 1000, FUnitsMeter);
      end else begin
        Result := ValToNum(VDistInMeters, FUnitsMeter);
      end;
    end;

    dsfKmOrM: begin
      if VDistInMeters < 10000 then begin
        Result := ValToNum(VDistInMeters, FUnitsMeter);
      end else begin
        Result := ValToNum(VDistInMeters / 1000, FUnitsKm);
      end;
    end;

    dsfSimpleKm: begin
      Result := ValToNum(VDistInMeters / 1000, FUnitsKm);
    end;

    dsfSimpleMeter: begin
      Result := ValToNum(VDistInMeters, FUnitsMeter);
    end;

    dsfSimpleCentimeter: begin
      Result := ValToNum(VDistInMeters * 100, FUnitsCentimeter);
    end;

    dsfSimpleMile: begin
      Result := ValToNum(VDistInMeters / 1609.344, FUnitsMile);
    end;

    dsfSimpleYard: begin
      Result := ValToNum(VDistInMeters / 0.9144, FUnitsYard);
    end;

    dsfSimpleFoot: begin
      Result := ValToNum(VDistInMeters / 0.3048, FUnitsFoot);
    end;

    dsfSimpleInch: begin
      Result := ValToNum(VDistInMeters / 0.0254, FUnitsInch);
    end;

    dsfSimpleNauticalMile: begin
      Result := ValToNum(VDistInMeters / 1852.0, FUnitsNauticalMile);
    end;
  else
    Assert(False);
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
    Result := CInvalidValue;
    Exit;
  end;

  Result := ValToNum(AKmph, FUnitsKmph, 1);
end;

end.

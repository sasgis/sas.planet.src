{******************************************************************************}
{* SAS.Planet (SAS.œÎ‡ÌÂÚ‡)                                                   *}
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

unit u_ValueToStringConverter;

interface

uses
  t_GeoTypes,
  t_CommonTypes,
  i_Notifier,
  i_Listener,
  i_ValueToStringConverter,
  i_ValueToStringConverterConfig,
  u_BaseInterfacedObject,
  u_ConfigDataElementBase;

type
  TValueToStringConverter = class(TBaseInterfacedObject, IValueToStringConverter)
  private
    FDistStrFormat: TDistStrFormat;
    FIsLatitudeFirst: Boolean;
    FDegrShowFormat: TDegrShowFormat;
    FAreaShowFormat: TAreaStrFormat;
    FUnitsKb: string;
    FUnitsMb: string;
    FUnitsGb: string;
    FUnitsKm: string;
    FUnitsMeters: string;
    FUnitsKmph: string;
    FUnitsHa: string;
    FUnitsSqKm: string;
    FUnitsSqMeters: string;
    FEastMarker: string;
    FWestMarker: string;
    FNorthMarker: string;
    FSouthMarker: string;
  private
    function DegrToStr(
      const ADegr: Double;
      ACutZero: boolean
    ): string;
    function GetLatitudeMarker(const ADegr: Double): string;
    function GetLongitudeMarker(const ADegr: Double): string;
  private
    function DataSizeConvert(const ASizeInKb: Double): string;
    function DistConvert(const ADistInMeters: Double): string;
    function DistPerPixelConvert(const ADistPerPixelInMeters: Double): string;
    function AreaConvert(const AAreaInSqm: Double): string;
    function SpeedConvert(const AKmph: Double): string;
    function AltitudeConvert(const AMeters: Double): string;
    function LonLatConvert(const ALonLat: TDoublePoint): string;
    function LonConvert(
      const ALon: Double;
      ACutZero: boolean
    ): string;
    function LatConvert(
      const ALat: Double;
      ACutZero: boolean
    ): string;
  public
    constructor Create(
      ADistStrFormat: TDistStrFormat;
      AIsLatitudeFirst: Boolean;
      ADegrShowFormat: TDegrShowFormat;
      AAreaShowFormat: TAreaStrFormat
    );
  end;

  TValueToStringConverterChangeable = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IValueToStringConverterChangeable)
  private
    FConfig: IValueToStringConverterConfig;
    FDependentNotifier: INotifier;
    FDependentListener: IListener;
    procedure OnDependentNotifier;
  protected
    function CreateStatic: IInterface; override;
  private
    function GetStatic: IValueToStringConverter;
  public
    constructor Create(
      const AConfig: IValueToStringConverterConfig;
      const ADependentNotifier: INotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,
  StrUtils,
  u_ListenerByEvent,
  u_ResStrings;

{ TValueToStringConverter }

constructor TValueToStringConverter.Create(
  ADistStrFormat: TDistStrFormat;
  AIsLatitudeFirst: Boolean;
  ADegrShowFormat: TDegrShowFormat;
  AAreaShowFormat: TAreaStrFormat
);
begin
  inherited Create;
  FDistStrFormat := ADistStrFormat;
  FIsLatitudeFirst := AIsLatitudeFirst;
  FDegrShowFormat := ADegrShowFormat;
  FAreaShowFormat := AAreaShowFormat;
  FUnitsKb := SAS_UNITS_kb;
  FUnitsMb := SAS_UNITS_mb;
  FUnitsGb := SAS_UNITS_gb;
  FUnitsKm := SAS_UNITS_km;
  FUnitsMeters := SAS_UNITS_m;
  FUnitsKmph := SAS_UNITS_kmperh;
  FUnitsSqKm := SAS_UNITS_km2;
  FUnitsHa := SAS_UNITS_Ha;
  FUnitsSqMeters := SAS_UNITS_m2;
  FNorthMarker := 'N';
  FEastMarker := 'E';
  FWestMarker := 'W';
  FSouthMarker := 'S';
end;

function TValueToStringConverter.AltitudeConvert(const AMeters: Double): string;
begin
  if IsNan(AMeters) then begin
    Result := 'NAN';
    Exit;
  end;

  Result := FormatFloat('0.0', AMeters) + ' ' + FUnitsMeters;
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

function TValueToStringConverter.DegrToStr(
  const ADegr: Double;
  ACutZero: boolean
): string;
var
  VDegr: Double;
  VInt: Integer;
  VValue: Integer;
  res: string;
begin
  VDegr := abs(ADegr);
  Res := '-';
  case FDegrShowFormat of
    dshCharDegrMinSec, dshSignDegrMinSec: begin
      VValue := Trunc(VDegr * 60 * 60 * 100 + 0.005);
      VInt := Trunc(VValue / (60 * 60 * 100));
      VValue := VValue - VInt * (60 * 60 * 100);
      Res := IntToStr(VInt) + '∞';
      VInt := Trunc(VValue / (60 * 100));
      VValue := VValue - VInt * (60 * 100);

      if VInt < 10 then begin
        Res := Res + '0' + IntToStr(VInt) + '''';
      end else begin
        Res := Res + IntToStr(VInt) + '''';
      end;

      Res := Res + FormatFloat('00.00', VValue / 100) + '"';

      if ACutZero then begin
        if copy(Res, length(Res) - 3, 4) = decimalseparator + '00"' then begin // X12∞30'45,00" -> X12∞30'45"
          Res := ReplaceStr(Res, decimalseparator + '00"', '"');
          if copy(Res, length(Res) - 2, 3) = '00"' then begin   // X12∞30'00" -> X12∞30'
            Res := ReplaceStr(Res, '00"', '');
            if copy(Res, length(Res) - 2, 3) = '00''' then begin  // X12∞00' -> X12∞
              Res := ReplaceStr(Res, '00''', '');
            end;
          end;
        end;
      end;
    end;
    dshCharDegrMin, dshSignDegrMin: begin
      VValue := Trunc(VDegr * 60 * 10000 + 0.00005);
      VInt := Trunc(VValue / (60 * 10000));
      VValue := VValue - VInt * (60 * 10000);
      Res := IntToStr(VInt) + '∞';
      Res := Res + FormatFloat('00.0000', VValue / 10000) + '''';
      if ACutZero then begin
        while copy(Res, length(Res) - 1, 2) = '0''' do begin
          Res := ReplaceStr(Res, '0''', '''');
        end;   // 12∞34,50000' -> 12∞34,5'   12∞00,00000' -> 12∞00,'
        if copy(Res, length(Res) - 1, 2) = decimalseparator + '''' then begin
          Res := ReplaceStr(Res, decimalseparator + '''', '''');
        end; // 12∞40,' -> 12∞40'
        if copy(Res, length(Res) - 2, 3) = '00''' then begin
          Res := ReplaceStr(Res, '00''', '');
        end; //  12∞00' -> 12∞
      end;
    end;
    dshCharDegr, dshSignDegr: begin
      Res := FormatFloat('0.000000', VDegr) + '∞';
      if ACutZero then begin
        while copy(Res, length(Res) - 1, 2) = '0∞' do begin
          Res := ReplaceStr(Res, '0∞', '∞');
        end;   // 12,3450000∞ -> 12,345∞   12,0000000∞ -> 12,∞
        if copy(Res, length(Res) - 1, 2) = decimalseparator + '∞' then begin
          Res := ReplaceStr(Res, decimalseparator + '∞', '∞');
        end; //12,∞ -> 12∞
      end;
    end;
  end;
  Result := res;
end;

function TValueToStringConverter.DistConvert(const ADistInMeters: Double): string;
var
  VKmDist: Double;
begin
  if IsNan(ADistInMeters) then begin
    Result := 'NAN';
    Exit;
  end;

  Result := '';
  case FDistStrFormat of
    dsfKmAndM: begin
      if ADistInMeters > 1000 then begin
        VKmDist := ADistInMeters / 1000;
        Result := IntToStr(Trunc(VKmDist)) + ' ' + FUnitsKm + ' ';
        Result := Result + FormatFloat('0.00', frac(VKmDist) * 1000) + ' ' + FUnitsMeters;
      end else begin
        Result := FormatFloat('0.00', ADistInMeters) + ' ' + FUnitsMeters;
      end;
    end;
    dsfSimpleKM: begin
      if ADistInMeters < 10000 then begin
        Result := FormatFloat('0.00', ADistInMeters) + ' ' + FUnitsMeters;
      end else begin
        Result := FormatFloat('0.00', ADistInMeters / 1000) + ' ' + FUnitsKm;
      end;
    end;
  end;
end;

function TValueToStringConverter.DistPerPixelConvert(
  const ADistPerPixelInMeters: Double
): string;
begin
  Result := DistConvert(ADistPerPixelInMeters) + SAS_UNITS_mperp;
end;

function TValueToStringConverter.GetLatitudeMarker(const ADegr: Double): string;
begin
  case FDegrShowFormat of
    dshCharDegrMinSec, dshCharDegrMin, dshCharDegr: begin
      if ADegr > 0 then begin
        result := FNorthMarker;
      end else if ADegr < 0 then begin
        result := FSouthMarker;
      end else begin
        result := '';
      end;
    end;
    dshSignDegrMinSec, dshSignDegrMin, dshSignDegr: begin
      if ADegr >= 0 then begin
        result := '';
      end else begin
        result := '-';
      end;
    end;
  end;
end;

function TValueToStringConverter.GetLongitudeMarker(const ADegr: Double): string;
begin
  case FDegrShowFormat of
    dshCharDegrMinSec, dshCharDegrMin, dshCharDegr: begin
      if ADegr > 0 then begin
        result := FEastMarker;
      end else if ADegr < 0 then begin
        result := FWestMarker;
      end else begin
        result := '';
      end;
    end;
    dshSignDegrMinSec, dshSignDegrMin, dshSignDegr: begin
      if ADegr >= 0 then begin
        result := '';
      end else begin
        result := '-';
      end;
    end;
  end;
end;

function TValueToStringConverter.LonLatConvert(const ALonLat: TDoublePoint): string;
var
  VLatStr: string;
  VLonStr: string;
begin
  VLatStr := GetLatitudeMarker(ALonLat.Y) + DegrToStr(ALonLat.Y, false);
  VLonStr := GetLongitudeMarker(ALonLat.X) + DegrToStr(ALonLat.X, false);
  if FIsLatitudeFirst then begin
    Result := VLatStr + ' ' + VLonStr;
  end else begin
    Result := VLonStr + ' ' + VLatStr;
  end;
end;

function TValueToStringConverter.LonConvert(
  const ALon: Double;
  ACutZero: boolean
): string;
begin
  result := GetLongitudeMarker(ALon) + DegrToStr(ALon, ACutZero);
end;

function TValueToStringConverter.LatConvert(
  const ALat: Double;
  ACutZero: boolean
): string;
begin
  result := GetLatitudeMarker(ALat) + DegrToStr(ALat, ACutZero);
end;

function TValueToStringConverter.SpeedConvert(const AKmph: Double): string;
begin
  if IsNan(AKmph) then begin
    Result := 'NAN';
    Exit;
  end;

  Result := FormatFloat('0.0', AKmph) + ' ' + FUnitsKmph;
end;

{ TValueToStringConverterChangeable }

constructor TValueToStringConverterChangeable.Create(
  const AConfig: IValueToStringConverterConfig;
  const ADependentNotifier: INotifier
);
begin
  inherited Create;
  FConfig := AConfig;
  FDependentNotifier := ADependentNotifier;
  FDependentListener := TNotifyNoMmgEventListener.Create(Self.OnDependentNotifier);
  FDependentNotifier.Add(FDependentListener);
  FConfig.ChangeNotifier.Add(FDependentListener);
end;

destructor TValueToStringConverterChangeable.Destroy;
begin
  if Assigned(FDependentNotifier) and Assigned(FDependentListener) then begin
    FDependentNotifier.Remove(FDependentListener);
    FDependentNotifier := nil;
  end;
  if Assigned(FConfig) and Assigned(FDependentListener) then begin
    FConfig.ChangeNotifier.Remove(FDependentListener);
    FConfig := nil;
  end;
  inherited;
end;

function TValueToStringConverterChangeable.CreateStatic: IInterface;
var
  VConfig: IValueToStringConverterConfigStatic;
  VStatic: IValueToStringConverter;
begin
  VConfig := FConfig.GetStatic;
  VStatic :=
    TValueToStringConverter.Create(
      VConfig.DistStrFormat,
      VConfig.IsLatitudeFirst,
      VConfig.DegrShowFormat,
      VConfig.AreaShowFormat
    );
  Result := VStatic;
end;

function TValueToStringConverterChangeable.GetStatic: IValueToStringConverter;
begin
  Result := IValueToStringConverter(GetStaticInternal);
end;

procedure TValueToStringConverterChangeable.OnDependentNotifier;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.

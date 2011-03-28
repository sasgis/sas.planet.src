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
begin
  VDegr := abs(ADegr);
  case FDegrShowFormat of
    dshCharDegrMinSec, dshSignDegrMinSec: begin
      result := IntToStr(Trunc(VDegr)) + '°';
      VDegr := Frac(VDegr) * 60;
      result := result + IntToStr(Trunc(VDegr)) + '''';
      VDegr := Frac(VDegr) * 60;
      Result := Result + FormatFloat('0.00', VDegr) + '"';
    end;
    dshCharDegrMin, dshSignDegrMin: begin
      result := IntToStr(Trunc(VDegr))+'°';
      VDegr := Frac(VDegr) * 60;
      result := result + FormatFloat('0.0000', VDegr) + '''';
    end;
    dshCharDegr, dshSignDegr: begin
      result := FormatFloat('0.000000', VDegr) + '°';
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

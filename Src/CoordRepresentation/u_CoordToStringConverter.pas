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

unit u_CoordToStringConverter;

interface

uses
  SysUtils,
  t_GeoTypes,
  t_CoordRepresentation,
  i_CoordToStringConverter,
  u_BaseInterfacedObject;

type
  TCoordToStringConverter = class(TBaseInterfacedObject, ICoordToStringConverter)
  private
    FIsLatitudeFirst: Boolean;
    FGeogCoordShowFormat: TGeogCoordShowFormat;
    FProjCoordShowFormat: TProjCoordShowFormat;
    FCoordSysType: TCoordSysType;
    FCoordSysInfoType: TCoordSysInfoType;
    FEastMarker: string;
    FWestMarker: string;
    FNorthMarker: string;
    FSouthMarker: string;
    FFormatSettings: TFormatSettings;
  private
    function FloatToStr(const AFormat: string; const AValue: Double): string; inline;
    function DegrToStr(
      const ADegr: Double;
      const ACutZero: Boolean
    ): string;
    function GetLatitudeMarker(const ADegr: Double): string;
    function GetLongitudeMarker(const ADegr: Double): string;
  private
    { ICoordToStringConverter }
    function GetCoordSysInfo(
      const ALonLat: TDoublePoint
    ): string;

    function LonLatConvert(
      const ALonLat: TDoublePoint;
      const AOptions: TCoordToStringConverterOptions = []
    ): string;

    function LonLatConvertExt(
      const ALonLat: TDoublePoint;
      const AOptions: TCoordToStringConverterOptions = []
    ): TCoordPartArray; overload; inline;

    function LonLatConvertExt(
      const ALonLat: TDoublePoint;
      const ACoordSysType: TCoordSysType;
      const AOptions: TCoordToStringConverterOptions = []
    ): TCoordPartArray; overload;
  public
    constructor Create(
      const AIsLatitudeFirst: Boolean;
      const AGeogCoordShowFormat: TGeogCoordShowFormat;
      const AProjCoordShowFormat: TProjCoordShowFormat;
      const ACoordSysType: TCoordSysType;
      const ACoordSysInfoType: TCoordSysInfoType
    );
  end;

implementation

uses
  Math,
  StrUtils,
  gnugettext,
  Proj4.UTM,
  Proj4.GaussKruger,
  u_CoordRepresentation,
  u_GeoFunc,
  u_StrFunc,
  u_ResStrings;

{ TCoordToStringConverter }

constructor TCoordToStringConverter.Create(
  const AIsLatitudeFirst: Boolean;
  const AGeogCoordShowFormat: TGeogCoordShowFormat;
  const AProjCoordShowFormat: TProjCoordShowFormat;
  const ACoordSysType: TCoordSysType;
  const ACoordSysInfoType: TCoordSysInfoType
);
begin
  inherited Create;
  FIsLatitudeFirst := AIsLatitudeFirst;
  FGeogCoordShowFormat := AGeogCoordShowFormat;
  FProjCoordShowFormat := AProjCoordShowFormat;
  FCoordSysType := ACoordSysType;
  FCoordSysInfoType := ACoordSysInfoType;
  FNorthMarker := 'N';
  FEastMarker := 'E';
  FWestMarker := 'W';
  FSouthMarker := 'S';
  FFormatSettings.DecimalSeparator := '.';
end;

function TCoordToStringConverter.FloatToStr(
  const AFormat: string;
  const AValue: Double
): string;
begin
  Result := FormatFloat(AFormat, AValue, FFormatSettings);
end;

function TCoordToStringConverter.DegrToStr(
  const ADegr: Double;
  const ACutZero: Boolean
): string;
var
  VDegr: Double;
  VInt: Int64;
  VValue: Int64;
  VCurr, VEnd: PChar;
begin
  Result := '';
  VDegr := Abs(ADegr);
  case FGeogCoordShowFormat of

    dshCharDegrMinSec, dshSignDegrMinSec: begin
      VValue := Trunc(VDegr * 60 * 60 * 10000 + 0.005);
      VInt := Trunc(VValue / (60 * 60 * 10000));
      VValue := VValue - VInt * (60 * 60 * 10000);
      Result := IntToStr(VInt) + '°';
      VInt := Trunc(VValue / (60 * 10000));
      VValue := VValue - VInt * (60 * 10000);

      if VInt < 10 then begin
        Result := Result + '0' + IntToStr(VInt) + '''';
      end else begin
        Result := Result + IntToStr(VInt) + '''';
      end;

      Result := Result + FloatToStr('00.0000', VValue / 10000) + '"';

      if ACutZero then begin
        if Copy(Result, Length(Result) - 3, 4) = '.00"' then begin // X12°30'45.00" -> X12°30'45"
          Result := ReplaceStr(Result, '.00"', '"');
          if Copy(Result, Length(Result) - 2, 3) = '00"' then begin   // X12°30'00" -> X12°30'
            Result := ReplaceStr(Result, '00"', '');
            if Copy(Result, Length(Result) - 2, 3) = '00''' then begin  // X12°00' -> X12°
              Result := ReplaceStr(Result, '00''', '');
            end;
          end;
        end;
      end;
    end;

    dshCharDegrMin, dshSignDegrMin: begin
      VValue := Trunc(VDegr * 60 * 1000000 + 0.00005);
      VInt := Trunc(VValue / (60 * 1000000));
      VValue := VValue - VInt * (60 * 1000000);
      Result := IntToStr(VInt) + '°';
      Result := Result + FloatToStr('00.000000', VValue / 1000000) + '''';
      if ACutZero then begin
        while Copy(Result, Length(Result) - 1, 2) = '0''' do begin
          Result := ReplaceStr(Result, '0''', '''');
        end;   // 12°34.50000' -> 12°34.5'   12°00.00000' -> 12°00.'
        if Copy(Result, Length(Result) - 1, 2) = '.''' then begin
          Result := ReplaceStr(Result, '.''', '''');
        end; // 12°40,' -> 12°40'
        if Copy(Result, Length(Result) - 2, 3) = '00''' then begin
          Result := ReplaceStr(Result, '00''', '');
        end; //  12°00' -> 12°
      end;
    end;

    dshCharDegr, dshCharDegr2, dshSignDegr, dshSignDegr2: begin
      Result := FloatToStr('0.00000000', VDegr);

      if ACutZero then begin
        VEnd := Pointer(Result);
        Inc(VEnd, Length(Result) - 1);
        VCurr := VEnd;

        // 12.3450000 -> 12.345
        while VCurr^ = '0' do begin
          Dec(VCurr);
        end;

        // 12. -> 12
        if VCurr^ = '.' then begin
          Dec(VCurr);
        end;

        if VCurr <> VEnd then begin
          SetLength(Result, Length(Result) - (VEnd - VCurr));
        end;
      end;

      if FGeogCoordShowFormat in [dshCharDegr, dshSignDegr] then begin
        Result := Result + '°';
      end;
    end;
  else
    raise Exception.CreateFmt(
      'Unexpected degree format value: %d', [Integer(FGeogCoordShowFormat)]
    );
  end;
end;

function TCoordToStringConverter.GetLatitudeMarker(const ADegr: Double): string;
begin
  case FGeogCoordShowFormat of
    dshCharDegrMinSec, dshCharDegrMin, dshCharDegr, dshCharDegr2: begin
      if ADegr > 0 then begin
        Result := FNorthMarker;
      end else if ADegr < 0 then begin
        Result := FSouthMarker;
      end else begin
        Result := '';
      end;
    end;
    dshSignDegrMinSec, dshSignDegrMin, dshSignDegr, dshSignDegr2: begin
      if ADegr >= 0 then begin
        Result := '';
      end else begin
        Result := '-';
      end;
    end;
  end;
end;

function TCoordToStringConverter.GetLongitudeMarker(const ADegr: Double): string;
begin
  case FGeogCoordShowFormat of
    dshCharDegrMinSec, dshCharDegrMin, dshCharDegr, dshCharDegr2: begin
      if ADegr > 0 then begin
        Result := FEastMarker;
      end else if ADegr < 0 then begin
        Result := FWestMarker;
      end else begin
        Result := '';
      end;
    end;
    dshSignDegrMinSec, dshSignDegrMin, dshSignDegr, dshSignDegr2: begin
      if ADegr >= 0 then begin
        Result := '';
      end else begin
        Result := '-';
      end;
    end;
  end;
end;

function TCoordToStringConverter.GetCoordSysInfo(
  const ALonLat: TDoublePoint
): string;
begin
  if FCoordSysInfoType = csitDontShow then begin
    Result := '';
    Exit;
  end;

  Result := GetCoordSysTypeCaptionShort[FCoordSysType];

  case FCoordSysType of
    cstWGS84: begin
      if FCoordSysInfoType = csitShowExceptWGS84 then begin
        Result := '';
      end;
    end;

    cstUTM: begin
      if (ALonLat.Y >= 84) or (ALonLat.Y <= -80) then begin
        Result := GetUPSCoordSysTypeCaptionShort;
      end;
    end;
  end;
end;

function TCoordToStringConverter.LonLatConvert(
  const ALonLat: TDoublePoint;
  const AOptions: TCoordToStringConverterOptions
): string;
const
  CSep: array [Boolean] of string = ('', ' ');
var
  I: TCoordPartItem;
  VArr: TCoordPartArray;
  VOptions: TCoordToStringConverterOptions;
begin
  VOptions := AOptions;

  if FIsLatitudeFirst then begin
    Include(VOptions, coLatitudeFirst);
  end;

  VArr := LonLatConvertExt(ALonLat, FCoordSysType, VOptions);

  Result := '';
  for I := Low(TCoordPartItem) to High(TCoordPartItem) do begin
    if VArr[I] <> '' then begin
      Result := Result + CSep[Result <> ''] + VArr[I];
    end;
  end;
end;

function TCoordToStringConverter.LonLatConvertExt(
  const ALonLat: TDoublePoint;
  const AOptions: TCoordToStringConverterOptions
): TCoordPartArray;
begin
  Result := LonLatConvertExt(ALonLat, FCoordSysType, AOptions);
end;

function TCoordToStringConverter.LonLatConvertExt(
  const ALonLat: TDoublePoint;
  const ACoordSysType: TCoordSysType;
  const AOptions: TCoordToStringConverterOptions = []
): TCoordPartArray;

  procedure _GeodeticCoordToStr(const ALonLat: TDoublePoint; out ALon, ALat: string);
  begin
    ALon := GetLongitudeMarker(ALonLat.X) + DegrToStr(ALonLat.X, coCutZero in AOptions);
    ALat := GetLatitudeMarker(ALonLat.Y) + DegrToStr(ALonLat.Y, coCutZero in AOptions);

    if coLatitudeFirst in AOptions then begin
      SwapStr(ALon, ALat);
    end;
  end;

  procedure _ProjectedCoordToStr(const X, Y: Double; const ASwapXY: Boolean; out AX, AY: string);
  const
    CFloatFormat: array[TProjCoordShowFormat] of string = ('0', '0.000');
  begin
    AX := FloatToStr(CFloatFormat[FProjCoordShowFormat], X);
    AY := FloatToStr(CFloatFormat[FProjCoordShowFormat], Y);

    if ASwapXY then begin
      SwapStr(AX, AY);
    end;
  end;

  function _ZoneInfoToStr(const AZone: Integer; const AIsNorth: Boolean): string;
  const
    CNorth: array[Boolean] of string = ('S', 'N');
  begin
    Result := '';

    if not (coIncludeZone in AOptions) then begin
      Exit;
    end;

    if AZone > 0 then begin
      Result := IntToStr(AZone);
    end;

    Result := Result + CNorth[AIsNorth];
  end;

var
  VUtm: TUtmCoord;
  VMgrs: TMgrsCoord;
  VGauss: TGaussKrugerCoord;
  VGeogLonLat: TDoublePoint;
  VLonStr, VLatStr, VZoneStr: string;
begin
  VLonStr := 'NaN';
  VLatStr := 'NaN';
  VZoneStr := '';

  case ACoordSysType of
    cstWGS84: begin // WGS 84 / Geographic
      _GeodeticCoordToStr(ALonLat, VLonStr, VLatStr);
    end;

    cstSK42: begin // Pulkovo-1942 / Geographic
      VGeogLonLat := ALonLat;
      if geodetic_wgs84_to_sk42(VGeogLonLat.X, VGeogLonLat.Y) then begin
        _GeodeticCoordToStr(VGeogLonLat, VLonStr, VLatStr);
      end;
    end;

    cstSK42GK: begin // Pulkovo-1942 / Gauss-Kruger
      if geodetic_wgs84_to_gauss_kruger(ALonLat.X, ALonLat.Y, VGauss) then begin
        _ProjectedCoordToStr(VGauss.X, VGauss.Y, {Swap=}True, VLonStr, VLatStr);
        VZoneStr := _ZoneInfoToStr(VGauss.Zone, VGauss.IsNorth);
      end;
    end;

    cstUTM: begin
      if geodetic_wgs84_to_utm(ALonLat.X, ALonLat.Y, VUtm) then begin
        // WGS 84 / UTM
        _ProjectedCoordToStr(VUtm.X, VUtm.Y, {Swap=}False, VLonStr, VLatStr);
        VZoneStr := _ZoneInfoToStr(VUtm.Zone, VUtm.IsNorth);
      end else
      if geodetic_wgs84_to_ups(ALonLat.X, ALonLat.Y, VUtm) then begin
        // WGS 84 / UPS
        _ProjectedCoordToStr(VUtm.X, VUtm.Y, {Swap=}True, VLonStr, VLatStr);
        VZoneStr := _ZoneInfoToStr(VUtm.Zone, VUtm.IsNorth);
      end;
    end;

    cstMGRS: begin
      if geodetic_wgs84_to_mgrs(ALonLat.X, ALonLat.Y, VMgrs) then begin
        VLonStr := Format('%.5d', [VMgrs.X]);
        VLatStr := Format('%.5d', [VMgrs.Y]);
        if VMgrs.Zone <> 0 then begin
          VZoneStr := Format('%d%s %s%s', [VMgrs.Zone, VMgrs.Band, VMgrs.Digraph[0], VMgrs.Digraph[1]]);
        end else begin
          VZoneStr := Format('%s %s%s', [VMgrs.Band, VMgrs.Digraph[0], VMgrs.Digraph[1]]);
        end;
      end;
    end;
  end;

  Result[cpiZone] := VZoneStr;
  Result[cpiLon] := VLonStr;
  Result[cpiLat] := VLatStr;
end;

end.

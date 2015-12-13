{******************************************************************************}
{* SAS.Planet (SAS.œÎ‡ÌÂÚ‡)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_CoordToStringConverter;

interface

uses
  t_GeoTypes,
  t_CoordRepresentation,
  i_CoordToStringConverter,
  u_BaseInterfacedObject;

type
  TCoordToStringConverter = class(TBaseInterfacedObject, ICoordToStringConverter)
  private
    FIsLatitudeFirst: Boolean;
    FDegrShowFormat: TDegrShowFormat;
    FCoordSysType: TCoordSysType;
    FEastMarker: string;
    FWestMarker: string;
    FNorthMarker: string;
    FSouthMarker: string;
  private
    function DegrToStr(
      const ADegr: Double;
      const ACutZero: Boolean
    ): string;
    function GetLatitudeMarker(const ADegr: Double): string;
    function GetLongitudeMarker(const ADegr: Double): string;
  private
    function LonLatConvert(
      const ALonLat: TDoublePoint
    ): string;
    function LonConvert(
      const ALon: Double;
      const ACutZero: Boolean
    ): string;
    function LatConvert(
      const ALat: Double;
      const ACutZero: Boolean
    ): string;
  public
    constructor Create(
      const AIsLatitudeFirst: Boolean;
      const ADegrShowFormat: TDegrShowFormat;
      const ACoordSysType: TCoordSysType
    );
  end;

implementation

uses
  Math,
  SysUtils,
  StrUtils,
  u_ResStrings;

{ TCoordToStringConverter }

constructor TCoordToStringConverter.Create(
  const AIsLatitudeFirst: Boolean;
  const ADegrShowFormat: TDegrShowFormat;
  const ACoordSysType: TCoordSysType
);
begin
  inherited Create;
  FIsLatitudeFirst := AIsLatitudeFirst;
  FDegrShowFormat := ADegrShowFormat;
  FCoordSysType := ACoordSysType;
  FNorthMarker := 'N';
  FEastMarker := 'E';
  FWestMarker := 'W';
  FSouthMarker := 'S';
end;

function TCoordToStringConverter.DegrToStr(
  const ADegr: Double;
  const ACutZero: Boolean
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

function TCoordToStringConverter.GetLatitudeMarker(const ADegr: Double): string;
begin
  case FDegrShowFormat of
    dshCharDegrMinSec, dshCharDegrMin, dshCharDegr: begin
      if ADegr > 0 then begin
        Result := FNorthMarker;
      end else if ADegr < 0 then begin
        Result := FSouthMarker;
      end else begin
        Result := '';
      end;
    end;
    dshSignDegrMinSec, dshSignDegrMin, dshSignDegr: begin
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
  case FDegrShowFormat of
    dshCharDegrMinSec, dshCharDegrMin, dshCharDegr: begin
      if ADegr > 0 then begin
        Result := FEastMarker;
      end else if ADegr < 0 then begin
        Result := FWestMarker;
      end else begin
        Result := '';
      end;
    end;
    dshSignDegrMinSec, dshSignDegrMin, dshSignDegr: begin
      if ADegr >= 0 then begin
        Result := '';
      end else begin
        Result := '-';
      end;
    end;
  end;
end;

function TCoordToStringConverter.LonLatConvert(
  const ALonLat: TDoublePoint
): string;
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

function TCoordToStringConverter.LonConvert(
  const ALon: Double;
  const ACutZero: Boolean
): string;
begin
  Result := GetLongitudeMarker(ALon) + DegrToStr(ALon, ACutZero);
end;

function TCoordToStringConverter.LatConvert(
  const ALat: Double;
  const ACutZero: Boolean
): string;
begin
  Result := GetLatitudeMarker(ALat) + DegrToStr(ALat, ACutZero);
end;

end.

{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
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

unit u_GeoToStrFunc;

interface

uses
  t_GeoTypes;

function RoundEx(const chislo: Double; const Precision: Integer): string;
function RoundExAnsi(const chislo: Double; const Precision: Integer): AnsiString;
function R2StrPoint(const r: Double): string;
function R2AnsiStrPoint(const r: Double): AnsiString;
function R2AnsiStrPointF(const r: Double; const AFormat: AnsiString): AnsiString;
function LonLat2GShListName(const ALonLat: TDoublePoint; AScale: Integer; APrec: Integer): string;
function str2r(const AStrValue: string): Double;

// forced with point
function StrPointToFloat(const S: String): Double;
function TryStrPointToFloat(const S: String; out AValue: Double): Boolean;

implementation

uses
  SysUtils,
  Math,
  ALString;

var
  GFormatSettings : TFormatSettings;
  GAnsiFormatSettings : TALFormatSettings;

function RoundEx(const chislo: Double; const Precision: Integer): string;
begin
  if IsNan(chislo) then
    Result := '-'
  else
    Result := FloatToStrF(chislo, ffFixed, 18, Precision, GFormatSettings);
end;

function RoundExAnsi(const chislo: Double; const Precision: Integer): AnsiString;
begin
  if IsNan(chislo) then
    Result := '-'
  else
    Result := ALFloatToStrF(chislo, ffFixed, 18, Precision, GAnsiFormatSettings);
end;

function str2r(const AStrValue: string): Double;
var
  VPos: integer;
  VFormatSettings : TFormatSettings;
begin
  if Length(AStrValue) = 0 then begin
    Result := 0
  end else begin
    VPos := System.Pos(',', AStrValue);
    if VPos > 0 then begin
      VFormatSettings.DecimalSeparator := ',';
    end else begin
      VPos := System.pos('.', AStrValue);
      if VPos > 0 then begin
        VFormatSettings.DecimalSeparator := '.';
      end else begin
        VFormatSettings.DecimalSeparator := DecimalSeparator;
      end;
    end;
    Result := StrToFloatDef(AStrValue, 0, VFormatSettings);
  end;
end;

function StrPointToFloat(const S: String): Double;
begin
  Result := StrToFloat(S, GFormatSettings);
end;

function TryStrPointToFloat(const S: String; out AValue: Double): Boolean;
begin
  Result := TryStrToFloat(S, AValue, GFormatSettings);
end;

function R2StrPoint(const r: Double): string;
begin
  Result := FloatToStr(r, GFormatSettings);
end;

function R2AnsiStrPoint(const r: Double): AnsiString;
begin
  Result := ALFloatToStr(r, GAnsiFormatSettings);
end;

function R2AnsiStrPointF(const r: Double; const AFormat: AnsiString): AnsiString;
begin
  Result := ALFormat(AFormat, [r], GAnsiFormatSettings);
end;

function LonLat2GShListName(const ALonLat: TDoublePoint; AScale: Integer; APrec: Integer): string;
const
  cRomans: array[1..36] of string = (
    'I','II','III','IV','V','VI','VII','VIII','IX','X','XI',
    'XII','XIII','XIV','XV','XVI','XVII','XVIII','XIX','XX',
    'XXI','XXII','XXIII','XXIV','XXV','XXVI','XXVII','XXVIII',
    'XXIX','XXX','XXXI','XXXII','XXXIII','XXXIV','XXXV','XXXVI'
  );

var
  VLon, VLat: Int64;

  function GetNameAtom(divr, modl: Integer): Integer;
  begin
    Result :=
      ((VLon div Round(6/divr*APrec)) mod modl)+
      (Abs(integer(ALonLat.Y>0)*(modl-1)-((VLat div Round(4/divr*APrec)) mod modl)))*modl;
  end;

  function AChr(const AValue: Integer): AnsiChar;
  begin
    Result := {$IFDEF UNICODE} AnsiChar(AValue) {$ELSE} Chr(AValue) {$ENDIF} ;
  end;

begin
  VLon := Round((ALonLat.X+180)*APrec);
  VLat := Round(Abs(ALonLat.Y*APrec));
  Result := AChr(65+(VLat div (4*APrec)))+'-'+IntToStr(1+(VLon div (6*APrec)));
  if ALonLat.Y < 0 then Result := 'x'+ Result;
  if AScale = 500000  then Result := Result +'-'+AChr(192+GetNameAtom(2,2));
  if AScale = 200000  then Result := Result +'-'+cRomans[1+GetNameAtom(6,6)];
  if AScale <= 100000 then Result := Result +'-'+IntToStr(1+GetNameAtom(12,12));
  if AScale <= 50000  then Result := Result +'-'+AChr(192+GetNameAtom(24,2));
  if AScale <= 25000  then Result := Result +'-'+AChr(224+GetNameAtom(48,2));
  if AScale = 10000   then Result := Result+'-'+IntToStr(1+GetNameAtom(96,2));

  if AScale = 5000 then
    Result :=
      AChr(65+(VLat div (4*APrec)))+'-'+
      IntToStr(1+(VLon div (6*APrec)))+'-'+
      IntToStr(1+GetNameAtom(12,12))+'-'+
      '('+IntToStr(1+GetNameAtom(192,16))+')';

  if AScale = 2500 then
    Result :=
      AChr(65+(VLat div (4*APrec)))+'-'+
      IntToStr(1+(VLon div (6*APrec)))+'-'+
      IntToStr(1+GetNameAtom(12,12))+'-'+
      '('+IntToStr(1+GetNameAtom(192,16))+'-'+AChr(224+GetNameAtom(384,2))+')';
end;

initialization
  GFormatSettings.DecimalSeparator := '.';
  GAnsiFormatSettings.DecimalSeparator := '.';
end.

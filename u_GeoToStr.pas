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

unit u_GeoToStr;

interface

uses
  t_GeoTypes;

function RoundEx(chislo: Double; Precision: Integer): string;
function R2StrPoint(r: Double): string;
function LonLat2GShListName(LL: TDoublePoint; Scale: integer; Prec: integer):string;
function str2r(inp:string):Double;

// forced with point
function StrPointToFloat(const S: String): Double;
function TryStrPointToFloat(const S: String; out AValue: Double): Boolean;

implementation

uses
  SysUtils,
  vsagps_public_base;

var
  GFormatSettings : TFormatSettings;

function RoundEx(chislo: Double; Precision: Integer): string;
begin
  if NoData_Float64(chislo) then
    Result := '-'
  else
    Result := FloatToStrF(chislo, ffFixed, 18, Precision, GFormatSettings);
end;

function str2r(inp:string):Double;
var p:integer;
begin
 if length(inp)=0 then result := 0 else
 begin
 p:=System.pos(DecimalSeparator,inp);
 if p=0 then begin
              if DecimalSeparator='.' then p:=System.pos(',',inp)
                                      else p:=System.pos('.',inp);
              inp[p]:=DecimalSeparator;
             end;
 result:=strtofloat(inp);
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

function R2StrPoint(r: Double): string;
begin
  Result := FloatToStr(r, GFormatSettings);
end;

function LonLat2GShListName(LL: TDoublePoint; Scale: integer; Prec: integer): string;
const
  Roman: array[1..36] of string = ('I','II','III','IV','V','VI','VII','VIII','IX','X','XI',
             'XII','XIII','XIV','XV','XVI','XVII','XVIII','XIX','XX','XXI','XXII','XXIII','XXIV','XXV',
             'XXVI','XXVII','XXVIII','XXIX','XXX','XXXI','XXXII','XXXIII','XXXIV','XXXV','XXXVI');
var
  Lon,Lat:int64;
 function GetNameAtom(divr,modl:integer):integer;
 begin
  result:=((Lon div round(6/divr*prec))mod modl)+(abs(integer(LL.Y>0)*(modl-1)-((Lat div round(4/divr*prec))mod modl)))*modl;
 end;
begin
 Lon:=round((LL.X+180)*prec);
 Lat:=round(abs(LL.Y*prec));
 result:=chr(65+(Lat div (4*prec)))+'-'+inttostr(1+(Lon div (6*prec)));
 if LL.Y<0 then result:='x'+result;
 if Scale=500000  then result:=result+'-'+chr(192+GetNameAtom(2,2));
 if Scale=200000  then result:=result+'-'+Roman[1+GetNameAtom(6,6)];
 if Scale<=100000 then result:=result+'-'+inttostr(1+GetNameAtom(12,12));
 if Scale<=50000  then result:=result+'-'+chr(192+GetNameAtom(24,2));
 if Scale<=25000  then result:=result+'-'+chr(224+GetNameAtom(48,2));
 if Scale=10000   then result:=result+'-'+inttostr(1+GetNameAtom(96,2));
 if Scale=5000    then result:=chr(65+(Lat div (4*prec)))+'-'+inttostr(1+(Lon div (6*prec)))+'-('+inttostr(1+GetNameAtom(192,16))+')';
 if Scale=2500    then result:=chr(65+(Lat div (4*prec)))+'-'+inttostr(1+(Lon div (6*prec)))+'-('+inttostr(1+GetNameAtom(192,16))+'-'+chr(224+GetNameAtom(384,2))+')';
end;

initialization
  GFormatSettings.DecimalSeparator := '.';
end.

{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
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

implementation

uses
  SysUtils;

var
  GFormatSettings : TFormatSettings;

function RoundEx(chislo: Double; Precision: Integer): string;
begin
  Result := FloatToStrF(chislo, ffFixed, 18, Precision, GFormatSettings);
end;

function str2r(inp:string):Double;
var p:integer;
begin
 p:=System.pos(DecimalSeparator,inp);
 if p=0 then begin
              if DecimalSeparator='.' then p:=System.pos(',',inp)
                                      else p:=System.pos('.',inp);
              inp[p]:=DecimalSeparator;
             end;
 result:=strtofloat(inp);
end;


function R2StrPoint(r: Double): string;
begin
  Result := FloatToStr(r, GFormatSettings);
end;

function LonLat2GShListName(LL: TDoublePoint; Scale: integer; Prec: integer): string;
const
  Roman: array[1..36] of string[6] = ('I','II','III','IV','V','VI','VII','VIII','IX','X','XI',
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
end;

initialization
  GFormatSettings.DecimalSeparator := '.';
end.

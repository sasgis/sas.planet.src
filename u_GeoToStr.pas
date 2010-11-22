unit u_GeoToStr;

interface

uses
  t_GeoTypes;

type
  { Способ отображения расстояний
  dsfKmAndM - в виде 12 км 299 м
  dsfSimpleKM - в виед 12.299 км
  }
  TDistStrFormat = (dsfKmAndM = 0, dsfSimpleKM = 1);

  TDegrShowFormat = (dshCharDegrMinSec = 0, dshCharDegrMin = 1, dshCharDegr = 2, dshSignDegrMinSec = 3, dshSignDegrMin = 4, dshSignDegr = 5);

function RoundEx(chislo: Extended; Precision: Integer): string;
function R2StrPoint(r: Extended): string;
function LonLat2GShListName(LL: TExtendedPoint; Scale: integer; Prec: integer):string;
function kb2KbMbGb(kb: Extended): string;
function DistToStrWithUnits(r: Real; AFormat: TDistStrFormat): string;
function lon2str(Alon: real; AFormatType: TDegrShowFormat): string;
function lat2str(Alat: real; AFormatType: TDegrShowFormat): string;
function str2r(inp:string):Extended;

implementation

uses
  SysUtils,
  UResStrings;

var
  GFormatSettings : TFormatSettings;

function RoundEx(chislo: Extended; Precision: Integer): string;
begin
  Result := FloatToStrF(chislo, ffFixed, 18, Precision, GFormatSettings);
end;

function str2r(inp:string):Extended;
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


function R2StrPoint(r: Extended): string;
begin
  Result := FloatToStr(r, GFormatSettings);
end;

function LonLat2GShListName(LL: TExtendedPoint; Scale: integer; Prec: integer): string;
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

function kb2KbMbGb(kb: Extended): string;
begin
  if kb > 1048576 then begin
    result := RoundEx(kb/1048576, 1) + ' ' + SAS_UNITS_gb;
  end else begin
    if kb > 1024 then begin
      result := RoundEx(kb/1024, 1) + ' ' + SAS_UNITS_mb;
    end else begin
      result := RoundEx(kb, 1) + ' ' + SAS_UNITS_kb;
    end;
  end;
end;


function DistToStrWithUnits(r: Real; AFormat: TDistStrFormat): string;
var
  VKmDist: Real;
begin
  Result := '';
  case AFormat of
    dsfKmAndM: begin
      if r > 1000 then begin
        VKmDist :=r/1000;
        Result := IntToStr(Trunc(VKmDist)) + ' ' + SAS_UNITS_km + ' ';
        Result := Result + RoundEx(frac(VKmDist)*1000, 2) + ' ' + SAS_UNITS_m
      end else begin
        Result := RoundEx(r, 2) + ' ' + SAS_UNITS_m;
      end;
    end;
    dsfSimpleKM: begin
      if r<10000 then begin
        Result := RoundEx(r, 2) + ' ' + SAS_UNITS_m;
      end else begin
        Result := RoundEx(r/1000, 2) + ' ' + SAS_UNITS_km;
      end;
    end;
  end;
end;

function Degris2str(Alon: real; AFormatType: TDegrShowFormat): string;
var
  num: real;
begin
  Alon := abs(Alon);
  case AFormatType of
    dshCharDegrMinSec, dshSignDegrMinSec: begin
      result := result + IntToStr(trunc(ALon)) + '°';
      num := Frac(ALon) * 60;
      result := result + IntToStr(trunc(num)) + '''';
      num := Frac(num) * 60;
      Result := Result + RoundEx(Num, 2) + '"';
    end;
    dshCharDegrMin, dshSignDegrMin: begin
      result := result + IntToStr(trunc(ALon))+'°';
      num := Frac(ALon) * 60;
      result := result + RoundEx(num, 4) + '''';
    end;
    dshCharDegr, dshSignDegr: begin
      result := result + RoundEx(ALon, 6) + '°';
    end;
  end;
end;

function lon2str(Alon: real; AFormatType: TDegrShowFormat): string;
begin
  case AFormatType of
    dshCharDegrMinSec, dshCharDegrMin, dshCharDegr: begin
      if ALon > 0 then begin
        result := 'E';
      end else begin
        result := 'W';
      end;
    end;
    dshSignDegrMinSec, dshSignDegrMin, dshSignDegr: begin
      if ALon > 0 then begin
        result := '';
      end else begin
        result := '-';
      end;
    end;
  end;
  Result := Result + Degris2str(Alon, AFormatType);
end;

function lat2str(Alat: real; AFormatType: TDegrShowFormat): string;
begin
  case AFormatType of
    dshCharDegrMinSec, dshCharDegrMin, dshCharDegr: begin
      if ALat > 0 then begin
        result := 'N';
      end else begin
        result := 'S';
      end;
    end;
    dshSignDegrMinSec, dshSignDegrMin, dshSignDegr: begin
      if ALat > 0 then begin
        result := '';
      end else begin
        result := '-';
      end;
    end;
  end;
  Result := Result + Degris2str(Alat, AFormatType);
end;

initialization
  GFormatSettings.DecimalSeparator := '.';
end.

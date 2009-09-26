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

  TDegrShowFormat = (dshDegrMinSec = 0, dshDegrMin = 1, dshDegr = 2);

function RoundEx(chislo: Extended; Precision: Integer): string;
function R2StrPoint(r: Extended): string;
function LonLat2GShListName(LL: TExtendedPoint; Scale: integer; Prec: integer):string;
function kb2KbMbGb(kb: real): string;
function DistToStrWithUnits(r: Real; AFormat: TDistStrFormat): string;
function lon2str(Alon: real; AFormatType: TDegrShowFormat): string;
function lat2str(Alat: real; AFormatType: TDegrShowFormat): string;

implementation

uses
  SysUtils,
  UResStrings;

var
  GFormatSettings : TFormatSettings;

function RoundEx(chislo: Extended; Precision: Integer): string;
begin
  Result := FloatToStrF(chislo, ffFixed, Precision,Precision, GFormatSettings);
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

function kb2KbMbGb(kb: real): string;
begin
  if kb > (1 shl 20) then begin
    result := RoundEx(kb/(1 shl 20), 1) + ' ' + SAS_UNITS_gb;
  end else begin
    if kb > (1 shl 10) then begin
      result := RoundEx(kb/(1 shl 20), 1) + ' ' + SAS_UNITS_mb;
    end else begin
      result := RoundEx(kb, 1) + ' ' + SAS_UNITS_kb;
    end;
  end;
end;


function DistToStrWithUnits(r: Real; AFormat: TDistStrFormat): string;
var
  VKmDist: Integer;
begin
  Result := '';
  case AFormat of
    dsfKmAndM: begin
      if r > 1000 then begin
        VKmDist := Trunc(r/1000);
        Result := IntToStr(VKmDist) +SAS_UNITS_km + ' ';
        Result := Result + RoundEx(r - VKmDist*1000, 2) + SAS_UNITS_m
      end else begin
        Result := RoundEx(r, 2) + SAS_UNITS_m;
      end;
    end;
    dsfSimpleKM: begin
      if r<10000 then begin
        Result := RoundEx(r, 2) + SAS_UNITS_m;
      end else begin
        Result := RoundEx(r/1000, 2) + SAS_UNITS_km;
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
    dshDegrMinSec: begin
      result := result + IntToStr(trunc(ALon)) + '°';
      num := Frac(ALon) * 60;
      result := result + IntToStr(trunc(num)) + '''';
      num := Frac(num) * 60;
      Result := Result + RoundEx(Num, 5) + '"';
    end;
    dshDegrMin: begin
      result := result + IntToStr(trunc(ALon))+'°';
      num := Frac(ALon) * 60;
      result := result + RoundEx(num, 7) + '''';
    end;
    dshDegr: begin
      result := result + RoundEx(ALon, 9) + '°';
    end;
  end;
end;

function lon2str(Alon: real; AFormatType: TDegrShowFormat): string;
begin
  if ALon > 0 then begin
    result := 'E';
  end else begin
    result := 'W';
  end;
  Result := Result + Degris2str(Alon, AFormatType);
end;

function lat2str(Alat: real; AFormatType: TDegrShowFormat): string;
begin
  if ALat > 0 then begin
    result := 'N';
  end else begin
    result := 'S';
  end;
  Result := Result + Degris2str(Alat, AFormatType);
end;

initialization
  GFormatSettings.DecimalSeparator := '.';
end.

unit u_GeoToStr;

interface

function RoundEx(chislo: Extended; Precision: Integer): string;

implementation

uses
  SysUtils;

var
  GFormatSettings : TFormatSettings;

function RoundEx(chislo: Extended; Precision: Integer): string;
begin
  Result := FloatToStrF(chislo, ffFixed, Precision,Precision, GFormatSettings);
end;


initialization
  GFormatSettings.DecimalSeparator := '.';
end.

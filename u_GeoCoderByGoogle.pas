unit u_GeoCoderByGoogle;

interface

uses
  Classes,
  u_GeoCoderBasic;

type
  TGeoCoderByGoogle = class(TGeoCoderBasic)
  protected
    function PrepareURL(ASearch: WideString): string; override;
    function ParseStringToPlacemarksList(AStr: string; ASearch: WideString): IInterfaceList; override;
  public
  end;

implementation

uses
  SysUtils,
  StrUtils,
  t_GeoTypes,
  i_GeoCoder,
  u_ResStrings,
  u_GeoCodePlacemark;

{ TGeoCoderByGoogle }

function TGeoCoderByGoogle.ParseStringToPlacemarksList(
  AStr: string; ASearch: WideString): IInterfaceList;
var
  slat, slon: string;
  i, j: integer;
  strr: string;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceList;
  VFormatSettings: TFormatSettings;
begin
  if AStr = '' then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  VFormatSettings.DecimalSeparator := '.';
  if not (PosEx(AnsiToUtf8('Placemark'), AStr) < 1) then begin
    i := PosEx('<address>', AStr);
    j := PosEx('</address>', AStr);
    strr := Utf8ToAnsi(Copy(AStr, i + 9, j - (i + 9)));
    i := PosEx('<coordinates>', AStr);
    j := PosEx(',', AStr, i + 13);
    slon := Copy(AStr, i + 13, j - (i + 13));
    i := PosEx(',0</coordinates>', AStr, j);
    slat := Copy(AStr, j + 1, i - (j + 1));
    if slat[1] = '\' then begin
      delete(slat, 1, 1);
    end;
    if slon[1] = '\' then begin
      delete(slon, 1, 1);
    end;
    try
      VPoint.Y := StrToFloat(slat, VFormatSettings);
      VPoint.X := StrToFloat(slon, VFormatSettings);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
    end;
    VPlace := TGeoCodePlacemark.Create(VPoint, strr, 4);
    VList := TInterfaceList.Create;
    VList.Add(VPlace);
    Result := VList;
  end;
end;

function TGeoCoderByGoogle.PrepareURL(ASearch: WideString): string;
var
  VSearch: String;
  i: integer;
begin
  VSearch := ASearch;
  for i := 1 to length(VSearch) do begin
    if VSearch[i] = ' ' then begin
      VSearch[i] := '+';
    end;
  end;
  Result := 'http://maps.google.com/maps/geo?q=' +
    URLEncode(AnsiToUtf8(VSearch)) +
    '&output=xml' + SAS_STR_GoogleSearchLanguage + '&key=ABQIAAAA5M1y8mUyWUMmpR1jcFhV0xSHfE-V63071eGbpDusLfXwkeh_OhT9fZIDm0qOTP0Zey_W5qEchxtoeA';
end;

end.

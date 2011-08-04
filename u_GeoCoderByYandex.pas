unit u_GeoCoderByYandex;

interface

uses
  Classes,
  u_GeoCoderBasic;

type
  TGeoCoderByYandex = class(TGeoCoderBasic)
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

{ TGeoCoderByYandex }

function TGeoCoderByYandex.ParseStringToPlacemarksList(
  AStr: string; ASearch: WideString): IInterfaceList;
var
  slat, slon, sname: string;
  i, j: integer;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceList;
  VFormatSettings: TFormatSettings;
begin
  if AStr = '' then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceList.Create;
  i:=PosEx('"items":[{', AStr);
  while (PosEx('"name":"', AStr, i) > i)and(i>0) do begin
    i := PosEx('"name":"', AStr, i);
    j := PosEx('",', AStr, i + 8);
    sname:= Utf8ToAnsi(Copy(AStr, i + 8, j - (i + 8)));
    i := PosEx('"text":"', AStr, i+10);
    j := PosEx('",', AStr, i + 8);
    sname:=sname+', '+Utf8ToAnsi(Copy(AStr, i + 8, j - (i + 8)));
    i := PosEx('"point":[', AStr, j);
    j := PosEx(',', AStr, i + 9);
    slon := Copy(AStr, i + 9, j - (i + 9));
    i := PosEx(']', AStr, j);
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
    VPlace := TGeoCodePlacemark.Create(VPoint, sname, 4);
    VList.Add(VPlace);
  end;
  Result := VList;
end;

function TGeoCoderByYandex.PrepareURL(ASearch: WideString): string;
var
  VSearch: String;
begin
  VSearch := ASearch;
  Result := 'http://maps.yandex.ru/?text=' +
    URLEncode(AnsiToUtf8(VSearch));
end;

end.

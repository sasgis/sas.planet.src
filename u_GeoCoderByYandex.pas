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
  u_GeoCodePalcemark;

{ TGeoCoderByYandex }

function TGeoCoderByYandex.ParseStringToPlacemarksList(
  AStr: string; ASearch: WideString): IInterfaceList;
var
  slat, slon, sname: string;
  i, j: integer;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePalcemark;
  VList: IInterfaceList;
  VFormatSettings: TFormatSettings;
begin
  if AStr = '' then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  VFormatSettings.DecimalSeparator := '.';
  i:=PosEx('"items":[{', AStr);
  if i > 0 then begin
    i := PosEx('"text":"', AStr, i+10);
    j := PosEx('",', AStr, i + 8);
    sname:= Utf8ToAnsi(Copy(AStr, i + 8, j - (i + 8)));
    i := PosEx('"point":[', AStr);
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
    VPlace := TGeoCodePalcemark.Create(VPoint, sname, 4);
    VList := TInterfaceList.Create;
    VList.Add(VPlace);
    Result := VList;
  end;
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

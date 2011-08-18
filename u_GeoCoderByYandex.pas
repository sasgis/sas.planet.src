unit u_GeoCoderByYandex;

interface

uses
  Classes,
  u_GeoTostr,
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
  slat, slon, sname, sdesc, sfulldesc: string;
  i, j: integer;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceList;
  VFormatSettings: TFormatSettings;
begin
  sfulldesc:='';
  if AStr = '' then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceList.Create;
  i:=PosEx('"items":[{', AStr);
  while (PosEx('"name":"', AStr, i) > i)and(i>0) do begin
    j := i;
    i := PosEx('"CompanyMetaData":{"id":"', AStr, i);
    if i>j then begin
      j := PosEx('",', AStr, i + 25);
      sfulldesc:='http://maps.yandex.ru/sprav/'+Copy(AStr, i + 25, j - (i + 25))+'/';
    end;

    i := PosEx('"name":"', AStr, j);
    j := PosEx('",', AStr, i + 8);
    sname:= Utf8ToAnsi(Copy(AStr, i + 8, j - (i + 8)));
    i := PosEx('"address":"', AStr, j);
    if i>j then begin
      j := PosEx('",', AStr, i + 11);
      sdesc:=Utf8ToAnsi(Copy(AStr, i + 11, j - (i + 11)));
    end;
    i := PosEx('"description":"', AStr, j);
    if i>j then begin
      j := PosEx('",', AStr, i + 15);
      sdesc:=Utf8ToAnsi(Copy(AStr, i + 15, j - (i + 15)));
    end;
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
    VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
    VList.Add(VPlace);
  end;
  Result := VList;
end;

function TGeoCoderByYandex.PrepareURL(ASearch: WideString): string;
var
  VSearch: String;
begin
  VSearch := ASearch;
  Result := 'http://maps.yandex.ru/?text='+URLEncode(AnsiToUtf8(VSearch))+'&ll='+R2StrPoint(FCurrentPos.x)+','+R2StrPoint(FCurrentPos.y);
end;

end.

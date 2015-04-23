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

unit u_GeoCoderByURL;

interface

uses
  Classes,
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_DownloadRequest,
  i_DownloadResult,
  i_InetConfig,
  i_NotifierTime,
  i_DownloadResultFactory,
  i_ValueToStringConverter,
  i_VectorDataItemSimple,
  i_VectorItemSubsetBuilder,
  i_GeoCoder,
  u_GeoCoderBasic;

type
  TGeoCoderByURL = class(TGeoCoderBasic)
  private
    FValueToStringConverter: IValueToStringConverterChangeable;
    function GetPointFromFullLink(
      const Astr: AnsiString;
      const ALocalConverter: ILocalCoordConverter
    ): IVectorDataItem;
    function GetPointFromShortLink(
      const Astr, AhttpData: AnsiString;
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer
    ): IVectorDataItem;
  protected
    function PrepareRequest(
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IDownloadRequest; override;
    function ParseResultToPlacemarksList(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AResult: IDownloadResultOk;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
    constructor Create(
      const AInetSettings: IInetConfig;
      const AGCNotifier: INotifierTime;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AResultFactory: IDownloadResultFactory;
      const AValueToStringConverter: IValueToStringConverterChangeable
    );
  end;

implementation

uses
  windows,
  SysUtils,
  StrUtils,
  ALString,
  RegExprUtils,
  t_GeoTypes,
  u_InterfaceListSimple,
  u_ResStrings,
  u_DownloadRequest;

{ TGeoCoderByExtLink }


procedure meters_to_lonlat(
  Ain_x, Ain_y: Double;
  var Vout_x, Vout_y: AnsiString;
  const AFormatSettings: TALFormatSettings
);
begin
  Vout_X := ALFloatToStr(Ain_X / 6378137 * 180 / pi, AFormatSettings);
  Vout_Y := ALFloatToStr(((arctan(exp(Ain_Y / 6378137)) - pi / 4) * 360) / pi, AFormatSettings);
end;

constructor TGeoCoderByURL.Create(
  const AInetSettings: IInetConfig;
  const AGCNotifier: INotifierTime;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AResultFactory: IDownloadResultFactory;
  const AValueToStringConverter: IValueToStringConverterChangeable
);
begin
  inherited Create(
    AInetSettings,
    AGCNotifier,
    AVectorItemSubsetBuilderFactory,
    APlacemarkFactory,
    AResultFactory
  );
  FValueToStringConverter := AValueToStringConverter;
end;

function TGeoCoderByURL.GetPointFromShortLink(
  const Astr, AhttpData: AnsiString;
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer
): IVectorDataItem;
var
  VPlace: IVectorDataItem;
  VPoint: TDoublePoint;
  VSLat, VSLon: AnsiString;
  VSName, VSDesc, VSFullDesc: String;
  VUrl: AnsiString;
  VFormatSettings: TALFormatSettings;
  VLink: AnsiString;
  I, J: Integer;
  VHeader: AnsiString;
  VRequest: IDownloadRequest;
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
  VValueConverter: IValueToStringConverter;
begin
  VValueConverter := FValueToStringConverter.GetStatic;
  VLink := ALStringReplace(AStr, '%2C', ',', [rfReplaceAll]);
  VFormatSettings.DecimalSeparator := '.';
  VSName := '';
  VSDesc := '';

  if ALPosEx('http://g.co/', VLink, 1) > 0 then begin
    VSName := 'google';
    VLink := AhttpData;
    I := ALPosEx('ll', VLink, 1);
    J := ALPosEx(',', VLink, I);
    VSLat := Copy(VLink, I + 3, J - (I + 3));
    I := J;
    J := ALPosEx('&', VLink, I);
    VSLon := Copy(VLink, I + 1, J - (I + 1));
  end;

  if ALPosEx('http://goo.gl/maps/', VLink, 1) > 0 then begin
    VSName := 'google';
    VLink := AhttpData;
    I := ALPosEx('ll', VLink, 1);
    J := ALPosEx(',', VLink, I);
    VSLat := Copy(VLink, I + 3, J - (I + 3));
    I := J;
    J := ALPosEx('&', VLink, I);
    VSLon := Copy(VLink, I + 1, J - (I + 1));
  end;

  if (RegExprGetMatchSubStr(VLink, '\.yandex\..+/-/', 0) <> '') or
    (ALPosEx('maps.yandex.ru/?oid=', VLink, 1) > 0) then begin
    VSName := 'yandex';
    VLink := ALStringReplace(AhttpData, '''', '', [rfReplaceAll]);
    I := ALPosEx('{ll:', VLink, 1);
    if I = 0 then begin
      I := ALPosEx(',ll:', VLink, 1);
    end;
    J := ALPosEx(',', VLink, I + 1);
    VSLon := Copy(VLink, I + 4, J - (I + 4));
    I := J;
    J := ALPosEx(',', VLink, I + 1);
    VSLat := Copy(VLink, I + 1, J - (I + 1));
  end;

  if (ALPosEx('maps.yandex.ru/?um=', VLink, 1) > 0) then begin // need 2 more test
    VSName := 'yandex';
    VLink := AhttpData;
    I := ALPosEx('{''bounds'':[[', VLink, 1);
    if I = 0 then begin
      I := ALPosEx(',ll:', VLink, 1);
    end;
    J := ALPosEx(',', VLink, I + 1);
    VSLon := Copy(VLink, I + 12, J - (I + 12));
    I := J;
    J := ALPosEx(']', VLink, I + 1);
    VSLat := Copy(VLink, I + 1, J - (I + 1));
  end;

  if ALPosEx('binged.it', VLink, 1) > 0 then begin
    VSName := 'bing';
    VLink := ALStringReplace(AhttpData, '%2c', ',', [rfReplaceAll]);
    if RegExprGetMatchSubStr(VLink, 'bing\.com.+cp=[0-9]+', 0) <> '' then begin
      I := ALPosEx('cp=', VLink, 1);
      J := ALPosEx('~', VLink, I);
      VSLat := Copy(VLink, I + 3, J - (I + 3));
      I := J;
      J := ALPosEx('&', VLink, I);
      VSLon := Copy(VLink, I + 1, J - (I + 1));
    end;
    if RegExprGetMatchSubStr(VLink, 'where1=[0-9]+', 0) <> '' then begin
      I := ALPosEx('where1=', VLink, 1);
      J := ALPosEx(',', VLink, I);
      VSLat := Copy(VLink, I + 7, J - (I + 7));
      I := J + 1;
      J := ALPosEx('"', VLink, I);
      VSLon := Copy(VLink, I + 1, J - (I + 1));
      VSLon := ALStringReplace(VSLon, ',', '.', [rfReplaceAll]);
      VSLat := ALStringReplace(VSLat, ',', '.', [rfReplaceAll]);
    end;
  end;

  if ALPosEx('osm.org', VLink, 1) > 0 then begin
    VSName := 'osm';
    VLink := AhttpData;
    I := ALPosEx('LonLat(', VLink, 1);
    J := ALPosEx(',', VLink, I);
    VSLon := Copy(VLink, I + 7, J - (I + 7));
    I := J + 1;
    J := ALPosEx(')', VLink, I);
    VSLat := Copy(VLink, I + 1, J - (I + 1));
  end;

  if ALPosEx('rambler.ru', VLink, 1) > 0 then begin
    VSName := 'rambler';
    VLink := ALStringReplace(AhttpData, '\"', '', [rfReplaceAll]);
    I := ALPosEx('lon:', VLink, 1);
    J := ALPosEx(',', VLink, I + 1);
    VSLon := Copy(VLink, I + 4, J - (I + 4));
    I := ALPosEx('lat:', VLink, J);
    J := ALPosEx('}', VLink, I + 1);
    VSLat := Copy(VLink, I + 4, J - (I + 4));
  end;

  if ALPosEx('permalink.html', VLink, 1) > 0 then begin
    VUrl := 'http://kosmosnimki.ru/TinyReference.ashx?id=' + Copy(VLink, 38, 9);
    VHeader := 'Referer: ' + VLink + ' Cookie: TinyReference=' + Copy(VLink, 38, 9);
    VLink := '';
    VRequest := TDownloadRequest.Create(VUrl, VHeader, InetSettings.GetStatic);
    VResult := Downloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
    if Supports(VResult, IDownloadResultOk, VResultOk) then begin
      SetLength(VLink, VResultOk.Data.Size);
      Move(VResultOk.Data.Buffer^, VLink[1], VResultOk.Data.Size);
      I := ALPosEx('"x":', VLink, 1);
      J := ALPosEx(',', VLink, I + 4);
      VSLon := Copy(VLink, I + 4, J - (I + 4));
      I := ALPosEx('"y":', VLink, J);
      J := ALPosEx(',', VLink, I + 4);
      VSLat := Copy(VLink, I + 4, J - (I + 4));
      VSFullDesc := String(VLink);
      VSName := 'kosmosnimki';
      meters_to_lonlat(ALStrToFloat(VSLon, VFormatSettings), ALStrToFloat(VSLat, VFormatSettings), VSLon, VSLat, VFormatSettings);
    end;
  end;

  if ALPosEx('api/index.html?permalink=', VLink, 1) > 0 then begin
    VSLat := Copy(VLink, 53, 5);
    VSLon := Copy(VLink, 59, 5);
    VUrl := 'http://maps.kosmosnimki.ru/TinyReference/Get.ashx?id=' + VSLat;
    VHeader := 'Referer: http://maps.kosmosnimki.ru/api/index.html?' + VSLon;
    VRequest := TDownloadRequest.Create(VUrl, VHeader, InetSettings.GetStatic);
    VResult := Downloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
    if Supports(VResult, IDownloadResultOk, VResultOk) then begin
      SetLength(VLink, VResultOk.Data.Size);
      Move(VResultOk.Data.Buffer^, VLink[1], VResultOk.Data.Size);
      VLink := ALStringReplace(VLink, '\', '', [rfReplaceAll]);
      I := ALPosEx('"x":', VLink, 1);
      J := ALPosEx(',', VLink, I + 4);
      VSLon := Copy(VLink, I + 4, J - (I + 4));
      I := ALPosEx('"y":', VLink, J);
      J := ALPosEx(',', VLink, I + 4);
      VSLat := Copy(VLink, I + 4, J - (I + 4));
      VSFullDesc := String(VLink);
      VSName := 'maps.kosmosnimki';
      meters_to_lonlat(ALStrToFloat(VSLon, VFormatSettings), ALStrToFloat(VSLat, VFormatSettings), VSLon, VSLat, VFormatSettings);
    end;
  end;

  if ALPosEx('go.2gis.ru', VLink, 1) > 0 then begin
    VUrl := VLink;
    VHeader := 'Cookie: 2gisAPI=c2de06c2dd3109de8ca09a59ee197a4210495664eeae8d4075848.943590';
    VLink := '';
    VRequest := TDownloadRequest.Create(VUrl, VHeader, InetSettings.GetStatic);
    VResult := Downloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
    if Supports(VResult, IDownloadResultOk, VResultOk) then begin
      SetLength(VLink, VResultOk.Data.Size);
      Move(VResultOk.Data.Buffer^, VLink[1], VResultOk.Data.Size);
      I := ALPosEx('center/', VLink, 1);
      J := ALPosEx(',', VLink, I);
      VSLon := Copy(VLink, I + 7, J - (I + 7));
      I := J;
      J := ALPosEx('/', VLink, I);
      VSLat := Copy(VLink, I + 1, J - (I + 1));
      VSName := '2gis';
    end;
  end;

  if VSName <> '' then begin
    try
      VPoint.Y := ALStrToFloat(VSLat, VFormatSettings);
      VPoint.X := ALStrToFloat(VSLon, VFormatSettings);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [VSLat, VSLon]);
    end;
    VSDesc := '[ ' + VValueConverter.LonLatConvert(VPoint) + ' ]';
    VSFullDesc := '<a href=' + String(Astr) + '>' + String(Astr) + '</a><br>' + ReplaceStr(VSName + #$D#$A + VSDesc, #$D#$A, '<br>');
    VPlace := PlacemarkFactory.Build(VPoint, VSName, VSDesc, VSFullDesc, 4);
    Result := VPlace;
  end else begin
    Result := nil;
  end;
end;

function TGeoCoderByURL.GetPointFromFullLink(
  const Astr: AnsiString;
  const ALocalConverter: ILocalCoordConverter
): IVectorDataItem;
var
  I, J: Integer;
  VPlace: IVectorDataItem;
  VPoint: TDoublePoint;
  VSLat, VSLon: AnsiString;
  VSName, VSDesc, VSFullDesc: String;
  VLink: AnsiString;
  VFormatSettings: TALFormatSettings;
  VZoom, Vilon, Vilat: Integer;
  VXYPoint: TPoint;
  VXYRect: TRect;
  VValueConverter: IValueToStringConverter;
begin
  VValueConverter := FValueToStringConverter.GetStatic;
  VLink := ALStringReplace(AStr, '%2C', ',', [rfReplaceAll]);
  VFormatSettings.DecimalSeparator := '.';
  VSName := '';
  VSDesc := '';
  // http://maps.google.com/?ll=48.718079,44.504639&spn=0.722115,1.234589&t=h&z=10
  // http://maps.google.ru/maps?hl=ru&ll=43.460987,39.948606&spn=0.023144,0.038581&t=m&z=15&vpsrc=6
  if RegExprGetMatchSubStr(VLink, 'maps\.google\..+ll=[0-9]+', 0) <> '' then begin
    VSName := 'Google';
    I := ALPosEx('ll', VLink, 1);
    J := ALPosEx(',', VLink, I);
    VSLat := Copy(VLink, I + 3, J - (I + 3));
    I := J;
    J := ALPosEx('&', VLink, I);
    VSLon := Copy(VLink, I + 1, J - (I + 1));
  end;

  // http://maps.navitel.su/?zoom=16&lat=45.03446&lon=38.96867&fl=J&rId=hN21H5ByVER8e4A%3D&rp=5
  if RegExprGetMatchSubStr(VLink, 'maps\.navitel\.su.+lat=.+lon=', 0) <> '' then begin
    VSName := 'Navitel';
    I := ALPosEx('lat=', VLink, 1);
    J := ALPosEx('&', VLink, I);
    VSLat := Copy(VLink, I + 4, J - (I + 4));
    I := ALPosEx('lon=', VLink, J);
    J := ALPosEx('&', VLink, I);
    if J = 0 then begin
      J := length(VLink) + 1;
    end;
    VSLon := Copy(VLink, I + 4, J - (I + 4));
  end;

  // http://kosmosnimki.ru/?x=44.1053254382903&y=45.6876903573303&z=6&fullscreen=false&mode=satellite
  if RegExprGetMatchSubStr(VLink, 'kosmosnimki\.ru.+x=.+y=', 0) <> '' then begin
    VSName := 'Kosmosnimki';
    I := ALPosEx('x=', VLink, 1);
    J := ALPosEx('&', VLink, I);
    VSLon := Copy(VLink, I + 2, J - (I + 2));
    I := ALPosEx('y=', VLink, J);
    J := ALPosEx('&', VLink, I);
    VSLat := Copy(VLink, I + 2, J - (I + 2));
  end;

  // http://www.bing.com/maps/default.aspx?v=2&cp=45.5493750107145~41.6883332507903&style=h&lvl=6
  if RegExprGetMatchSubStr(VLink, 'bing\.com.+cp=[0-9]+', 0) <> '' then begin
    VSName := 'Bing';
    I := ALPosEx('cp=', VLink, 1);
    J := ALPosEx('~', VLink, I);
    VSLat := Copy(VLink, I + 3, J - (I + 3));
    I := J;
    J := ALPosEx('&', VLink, I);
    VSLon := Copy(VLink, I + 1, J - (I + 1));
  end;

  // http://wikimapia.org#lat=45.0328&lon=38.9769&z=10&l=1&m=b
  if RegExprGetMatchSubStr(VLink, 'wikimapia\.org.+lat=.+lon=', 0) <> '' then begin
    VSName := 'WikiMapia';
    I := ALPosEx('lat=', VLink, 1);
    J := ALPosEx('&', VLink, I);
    VSLat := Copy(VLink, I + 4, J - (I + 4));
    I := ALPosEx('lon=', VLink, J);
    J := ALPosEx('&', VLink, I);
    VSLon := Copy(VLink, I + 4, J - (I + 4));
  end;

  // http://maps.rosreestr.ru/Portal/?l=11&x=4595254.155000001&y=5398402.163800001&mls=map|anno&cls=cadastre
  if RegExprGetMatchSubStr(VLink, 'maps\.rosreestr\.ru.+x=.+y=', 0) <> '' then begin
    VSName := 'Rosreestr';
    I := ALPosEx('x=', VLink, 1);
    J := ALPosEx('&', VLink, I);
    VSLon := Copy(VLink, I + 2, J - (I + 2));
    I := ALPosEx('y=', VLink, J);
    J := ALPosEx('&', VLink, I);
    VSLat := Copy(VLink, I + 2, J - (I + 2));
    meters_to_lonlat(ALStrToFloat(VSLon, VFormatSettings), ALStrToFloat(VSLat, VFormatSettings), VSLon, VSLat, VFormatSettings);
  end;

  // http://maps.mail.ru/?z=10&ll=37.619948,55.750023&J=1
  if RegExprGetMatchSubStr(VLink, 'maps\.mail\.ru.+ll=', 0) <> '' then begin
    VSName := 'Mail.ru';
    I := ALPosEx('ll=', VLink, 1);
    J := ALPosEx(',', VLink, I);
    VSLon := Copy(VLink, I + 3, J - (I + 3));
    I := J;
    J := ALPosEx('&', VLink, I);
    if J = 0 then begin
      J := length(VLink) + 1;
    end;
    VSLat := Copy(VLink, I + 1, J - (I + 1));
  end;

  // http://maps.nokia.com/#|43.5669132|41.2836342|14|0|0|hybrid.day
  // http://maps.nokia.com/mapcreator/?ns=true#|55.32530472503459|37.811186150077816|18|0|0|
  if RegExprGetMatchSubStr(VLink, 'maps\.nokia\.com.+\#\|', 0) <> '' then begin
    I := ALPosEx('#|', VLink, 1);
    J := ALPosEx('|', VLink, I + 2);
    VSLat := Copy(VLink, I + 2, J - (I + 2));
    I := J;
    J := ALPosEx('|', VLink, I + 1);
    if J = 0 then begin
      J := length(VLink) + 1;
    end;
    VSLon := Copy(VLink, I + 1, J - (I + 1));
  end;

  // http://maps.yandex.ru/?ll=44.514541%2C48.708958&spn=0.322723%2C0.181775&z=12&l=map
  // http://harita.yandex.com.tr/?ll=29.086777%2C41.000749&spn=0.005043%2C0.003328&z=18&l=sat%2Ctrf&trfm=cur
  // https://n.maps.yandex.ru/#!/?z=15&ll=37.438471%2C55.816492
  // https://n.maps.yandex.ru/?ll=37.43843%2C55.817359&spn=0.037723%2C0.017035&z=15&l=wmap&oid=105810
  if RegExprGetMatchSubStr(VLink, '\.yandex\..+ll=[0-9]+', 0) <> '' then begin
    VSName := 'Yandex';
    I := ALPosEx('ll', VLink, 1);
    J := ALPosEx(',', VLink, I);
    VSLon := Copy(VLink, I + 3, J - (I + 3));
    I := J;
    J := ALPosEx('&', VLink, I);
    if J = 0 then J := Length(VLink);
    VSLat := Copy(VLink, I + 1, J - (I + 1));
  end;

  // http://mobile.maps.yandex.net/ylocation/?lat=55.870155&lon=37.665367&desc=dima%40dzhus.org
  if RegExprGetMatchSubStr(VLink, '\.yandex\..+lat=.+lon=', 0) <> '' then begin
    VSName := 'Yandex';
    I := ALPosEx('lat=', VLink, 1);
    J := ALPosEx('&', VLink, I + 3);
    VSLat := Copy(VLink, I + 4, J - (I + 4));
    I := ALPosEx('lon=', VLink, J);
    J := ALPosEx('&', VLink, I);
    if J = 0 then begin
      J := length(VLink) + 1;
    end;
    VSLon := Copy(VLink, I + 4, J - (I + 4));
  end;

  //http://maps.2gis.ru/#/?history=project/krasnodar/center/38.993668%2C45.197055/zoom/17/state/index/sort/relevance
  if RegExprGetMatchSubStr(VLink, 'maps\.2gis\.ru.+zoom', 0) <> '' then begin
    VSName := '2Gis';
    I := ALPosEx('center/', VLink, 1);
    J := ALPosEx(',', VLink, I);
    VSLon := Copy(VLink, I + 7, J - (I + 7));
    I := J;
    J := ALPosEx('/', VLink, I);
    if J = 0 then begin
      J := length(VLink) + 1;
    end;
    VSLat := Copy(VLink, I + 1, J - (I + 1));
  end;

  // http://www.openstreetmap.org/#map=17/45.12333/38.98709
  // http://www.openstreetmap.org/#map=17/45.12333/38.98576&layers=C
  if (RegExprGetMatchSubStr(VLink, '(openstreetmap|osm)\..+map=', 0) <> '') then begin
    VSName := 'OpenStreetMap';
    I := ALPosEx('map=', VLink, 1);
    I := ALPosEx('/', VLink, I);
    J := ALPosEx('/', VLink, I + 1);
    VSLat := Copy(VLink, I + 1, J - (I + 1));

    I := ALPosEx('/', VLink, J);
    J := ALPosEx('&', VLink, I);

    if J = 0 then begin
      J := length(VLink) + 1;
    end;
    VSLon := Copy(VLink, I + 1, J - (I + 1));
  end;

  // http://www.openstreetmap.org/?lat=45.227&lon=39.001&zoom=10&layers=M
  // http://osm.org.ru/#layer=M&zoom=3&lat=61.98&lon=88
  if (RegExprGetMatchSubStr(VLink, '(openstreetmap|osm)\..+lat=', 0) <> '') then begin
    VSName := 'OpenStreetMap';
    I := ALPosEx('lat=', VLink, 1);
    J := ALPosEx('&', VLink, I);
    VSLat := Copy(VLink, I + 4, J - (I + 4));
    I := ALPosEx('lon=', VLink, J);
    J := ALPosEx('&', VLink, I);
    if J = 0 then begin
      J := length(VLink) + 1;
    end;
    VSLon := Copy(VLink, I + 4, J - (I + 4));
  end;

  // http://khm0.google.com/kh/v=127&src=app&x=24398&s=&y=10570&z=15&s=Gali
  if RegExprGetMatchSubStr(VLink, 'khm.+google\..+x=[0-9]+', 0) <> '' then begin
    VSName := 'Google tile';

    I := ALPosEx('y=', VLink, 1);
    J := ALPosEx('&', VLink, I);
    VSLat := Copy(VLink, I + 2, J - (I + 2));
    Vilat := ALStrToInt(VSLat);

    I := ALPosEx('x=', VLink, 1);
    J := ALPosEx('&', VLink, I);
    VSLon := Copy(VLink, I + 2, J - (I + 2));
    Vilon := ALStrToInt(VSLon);

    I := ALPosEx('z=', VLink, 1);
    J := ALPosEx('&', VLink, I);
    VSLon := Copy(VLink, I + 2, J - (I + 2));
    VZoom := ALStrToInt(VSLon);
    Inc(VZoom);

    VXYPoint.X := ViLon;
    VXYPoint.Y := ViLat;
    VSDesc := 'z=' + IntToStr(VZoom) + ' x=' + IntToStr(Vilon) + ' y=' + IntToStr(Vilat) + #$D#$A;
    VXYRect := ALocalConverter.GetGeoConverter.TilePos2PixelRect(VXYPoint, VZoom - 1);
    VXYPoint := Point((VXYRect.Right + VXYRect.Left) div 2, (VXYRect.Bottom + VXYRect.top) div 2);
    VPoint := ALocalConverter.GetGeoConverter.PixelPos2LonLat(VXYPoint, VZoom - 1);
    VSLat := ALFloatToStr(VPoint.Y, VFormatSettings);
    VSLon := ALFloatToStr(VPoint.X, VFormatSettings);
  end;

  // http://c.tile.openstreetmap.org/10/622/367.png
  if RegExprGetMatchSubStr(VLink, '\.(openstreetmap|opencyclemap|osm).+\.png', 0) <> '' then begin
    VSName := 'OpenStreetMap';
    I := PosEx(RegExprGetMatchSubStr(VLink, '/[0-9]?[0-9]/', 0), VLink, 1); // Z значение
    J := ALPosEx('/', VLink, I + 1);
    VZoom := (ALStrToInt(Copy(VLink, I + 1, J - (I + 1))));
    Inc(VZoom);
    I := J;
    J := ALPosEx('/', VLink, I + 1);
    VSLon := Copy(VLink, I + 1, J - (I + 1));
    Vilon := ALStrToInt(VSLon);
    I := J;
    J := ALPosEx('.', VLink, I + 1);
    VSLat := Copy(VLink, I + 1, J - (I + 1));
    Vilat := ALStrToInt(VSLat);
    VXYPoint.X := ViLon;
    VXYPoint.Y := ViLat;
    VSDesc := 'z=' + IntToStr(VZoom) + ' x=' + IntToStr(Vilon) + ' y=' + IntToStr(Vilat) + #$D#$A;
    VXYRect := ALocalConverter.GetGeoConverter.TilePos2PixelRect(VXYPoint, VZoom - 1);
    VXYPoint := Point((VXYRect.Right + VXYRect.Left) div 2, (VXYRect.Bottom + VXYRect.top) div 2);
    VPoint := ALocalConverter.GetGeoConverter.PixelPos2LonLat(VXYPoint, VZoom - 1);
    VSLat := ALFloatToStr(VPoint.Y, VFormatSettings);
    VSLon := ALFloatToStr(VPoint.X, VFormatSettings);
  end;

  // http://188.95.188.28/cgi-bin/webfile_mgr.cgi?cmd=cgi_download&path=/mnt/HD/HD_a2/pub/genshtab250m/z12/1302/2506.jpg&path1=/mnt/HD/HD_a2/pub/genshtab250m/z12/1302/2506.jpg&name=2506.jpg&type=JPEG+Image&browser=iee)
  if RegExprGetMatchSubStr(VLink, '/z[0-9]+/.+\.(png|jpg)+', 0) <> '' then begin
    if VSName = '' then begin
      VSName := String(RegExprGetMatchSubStr(VLink, 'http://[0-9a-zа-я\.]+', 0));
      I := ALPosEx('/z', VLink, 1);
      if I > 0 then begin
        J := ALPosEx('/', VLink, I + 1);
        VSLat := Copy(VLink, I + 2, J - (I + 2));
        try
          VZoom := ALStrToInt(VSLat);
        except
          VZoom := 0;
        end;
        I := ALPosEx('/', VLink, J); // X значение
        J := ALPosEx('/', VLink, I + 1);
        VSLon := Copy(VLink, I + 1, J - (I + 1));
        Vilat := ALStrToInt(VSLon);
        I := J; // Y значение
        J := ALPosEx('.', VLink, I + 1);
        VSLat := Copy(VLink, I + 1, J - (I + 1));
        Vilon := ALStrToInt(VSLat);
        Inc(VZoom); // зум отличается на 1
        VXYPoint.X := ViLon;
        VXYPoint.Y := ViLat;
        VSDesc := 'z=' + IntToStr(VZoom) + ' x=' + IntToStr(Vilon) + ' y=' + IntToStr(Vilat) + #$D#$A;
        VXYRect := ALocalConverter.GetGeoConverter.TilePos2PixelRect(VXYPoint, VZoom - 1);
        VXYPoint := Point((VXYRect.Right + VXYRect.Left) div 2, (VXYRect.Bottom + VXYRect.top) div 2);
        VPoint := ALocalConverter.GetGeoConverter.PixelPos2LonLat(VXYPoint, VZoom - 1);
        VSLat := ALFloatToStr(VPoint.Y, VFormatSettings);
        VSLon := ALFloatToStr(VPoint.X, VFormatSettings);
      end;
    end;
  end;

  // http://wikimapia.org/d?lng=1&BBOX=42.84668,43.26121,42.89063,43.29320
  // http://www.openstreetmap.org/?box=yes&bbox=41.73729%2C44.25345%2C41.73729%2C44.25345
  if RegExprGetMatchSubStr(AlUpperCase(VLink), 'BBOX=([0-9]+.[0-9]+\,)+([0-9]+.[0-9]+)', 0) <> '' then begin
    if VSName = '' then begin
      VSName := String(RegExprGetMatchSubStr(VLink, 'http://[0-9a-zа-я\.]+', 0));
      I := ALPosEx('BBOX=', AlUpperCase(VLink)) + 4;
      J := ALPosEx(',', VLink, I + 1);
      VSLon := Copy(VLink, I + 1, J - (I + 1));
      I := J;
      J := ALPosEx(',', VLink, I + 1);
      VSLat := Copy(VLink, I + 1, J - (I + 1));
      I := J;
      J := ALPosEx(',', VLink, I + 1);
      VSLon := ALFloatToStr((ALStrToFloat(Copy(VLink, I + 1, J - (I + 1)), VFormatSettings) + ALStrToFloat(VSLon, VFormatSettings)) / 2, VFormatSettings);
      I := J;
      J := ALPosEx('&', VLink, I + 1);
      if J = 0 then begin
        J := length(VLink);
      end;
      VSLat := ALFloatToStr((ALStrToFloat(Copy(VLink, I + 1, J - (I + 1)), VFormatSettings) + ALStrToFloat(VSLat, VFormatSettings)) / 2, VFormatSettings);
      VSLat := ALStringReplace(VSLat, ',', '.', [rfReplaceAll]);
      VSLon := ALStringReplace(VSLon, ',', '.', [rfReplaceAll]);
      if (ALStrToFloat(VSLat, VFormatSettings) > 360) or (ALStrToFloat(VSLon, VFormatSettings) > 360) then begin
        meters_to_lonlat(ALStrToFloat(VSLon, VFormatSettings), ALStrToFloat(VSLat, VFormatSettings), VSLon, VSLat, VFormatSettings);
      end;
    end;
  end;
 // http://чепецк.net/?zoom=15&lat=43.94165&lon=40.14849&layers=BFFFT
  if RegExprGetMatchSubStr(AlUpperCase(VLink), 'LAT=.+LON=', 0) <> '' then begin
    if VSName = '' then begin
      VSName := String(RegExprGetMatchSubStr(VLink, 'http://[0-9a-zа-я\.]+', 0));
      I := ALPosEx('LAT=', AlUpperCase(VLink), 1);
      J := ALPosEx('&', VLink, I);
      VSLat := Copy(VLink, I + 4, J - (I + 4));
      I := ALPosEx('LON=', AlUpperCase(VLink), J);
      J := ALPosEx('&', VLink, I);
      if J = 0 then begin
        J := length(VLink) + 1;
      end;
      VSLon := Copy(VLink, I + 4, J - (I + 4));
    end;
  end;

  if VSName <> '' then begin
    try
      VPoint.Y := ALStrToFloat(VSLat, VFormatSettings);
      VPoint.X := ALStrToFloat(VSLon, VFormatSettings);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [VSLat, VSLon]);
    end;
    VSDesc := VSDesc + '[ ' + VValueConverter.LonLatConvert(VPoint) + ' ]';
    VSFullDesc := '<a href=' + String(Astr) + '>' + String(Astr) + '</a><br>' + ReplaceStr(VSName + #$D#$A + VSDesc, #$D#$A, '<br>');
    VPlace := PlacemarkFactory.Build(VPoint, VSName, VSDesc, VSFullDesc, 4);
    Result := VPlace;
  end else begin
    Result := nil;
  end;
end;

function TGeoCoderByURL.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  VPlace: IVectorDataItem;
  VList: IInterfaceListSimple;
  VStr: AnsiString;
  VUrl: AnsiString;
begin
  VUrl := AnsiString(ASearch);
  if AResult = nil then // ничего не скачивали, полная ссылка
  begin
    VPlace := GetPointFromFullLink(VUrl, ALocalConverter);
  end else begin // короткая ссылка, и данные уже скачаны
    SetLength(Vstr, AResult.Data.Size);
    Move(AResult.Data.Buffer^, Vstr[1], AResult.Data.Size);
    VPlace := GetPointFromShortLink(VUrl, VStr, ACancelNotifier, AOperationID);
  end;

  if VPlace <> nil then begin
    VList := TInterfaceListSimple.Create;
    VList.Add(VPlace);
  end else begin
    VList := nil;
  end;
  Result := VList;
end;

function TGeoCoderByURL.PrepareRequest(
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
var
  VUrl: AnsiString;
begin
  VUrl := AnsiString(ASearch);
  Result := nil;
  if (ALPosEx('http://g.co/', VUrl, 1) > 0) or
    (ALPosEx('http://goo.gl/maps/', VUrl, 1) > 0) or
    (ALPosEx('yandex.ru/?oid=', VUrl, 1) > 0) or
    (ALPosEx('binged.it', VUrl, 1) > 0) or
    (ALPosEx('osm.org', VUrl, 1) > 0) or
    (ALPosEx('permalink.html', VUrl, 1) > 0) or
    (ALPosEx('api/index.html?permalink=', VUrl, 1) > 0) or
    (ALPosEx('rambler.ru/?', VUrl, 1) > 0) or
    (ALPosEx('yandex.ru/?um=', VUrl, 1) > 0) or
    (RegExprGetMatchSubStr(VUrl, '\.yandex\..+/-/', 0) <> '') then begin
    Result := PrepareRequestByURL(VUrl);
  end;
end;

// Полные ссылки
// http://maps.google.com/?ll=48.718079,44.504639&spn=0.722115,1.234589&t=h&z=10
// http://maps.yandex.ru/?ll=44.514541%2C48.708958&spn=0.322723%2C0.181775&z=12&l=map
// http://maps.navitel.su/?zoom=6&lat=55.8&lon=37.6
// http://kosmosnimki.ru/?x=44.1053254382903&y=45.6876903573303&z=6&fullscreen=false&mode=satellite
// http://www.bing.com/maps/default.aspx?v=2&cp=45.5493750107145~41.6883332507903&style=h&lvl=6
// http://www.openstreetmap.org/?lat=45.227&lon=39.001&zoom=10&layers=M
// http://wikimapia.org#lat=45.0328&lon=38.9769&z=10&l=1&m=b
// http://maps.rosreestr.ru/Portal/?l=11&x=4595254.155000001&y=5398402.163800001&mls=map|anno&cls=cadastre
// http://maps.mail.ru/?z=10&ll=37.619948,55.750023
// http://maps.nokia.com/#|43.5669132|41.2836342|14|0|0|hybrid.day
// http://maps.nokia.com/mapcreator/?ns=true#|55.32530472503459|37.811186150077816|18|0|0|
// http://mobile.maps.yandex.net/ylocation/?lat=55.870155&lon=37.665367&desc=dima%40dzhus.org
// http://maps.2gis.ru/#/?history=project/krasnodar/center/38.993668%2C45.197055/zoom/17/state/index/sort/relevance
// http://harita.yandex.com.tr/?ll=29.086777%2C41.000749&spn=0.005043%2C0.003328&z=18&l=sat%2Ctrf&trfm=cur
// http://osm.org.ru/#layer=M&zoom=3&lat=61.98&lon=88
// https://n.maps.yandex.ru/#!/?z=15&ll=37.438471%2C55.816492

// тайловые ссылки
// http://a.tile.openstreetmap.org/15/19928/11707.png
// http://khm0.google.com/kh/v=127&src=app&x=24398&s=&y=10570&z=15&s=Gali

// Короткие
// http://g.co/maps/7anbg
// http://maps.yandex.ru/-/CBa6ZCOt
// http://maps.yandex.ru/-/CFVIfLi-#
// http://osm.org/go/0oqbju
// http://binged.it/vqaOQQ
// http://kosmosnimki.ru/permalink.html?Na1d0e33d
// http://maps.kosmosnimki.ru/api/index.html?permalink=ZWUJK&SA5JU
// http://go.2gis.ru/1hox// http://maps.rambler.ru/?6rJJy58
// http://maps.yandex.ru/?um=m4VoZPqVSEwQ3YdT5Lmley6KrBsHb2oh&l=sat
// http://harita.yandex.com.tr/-/CFXxAO3m

end.


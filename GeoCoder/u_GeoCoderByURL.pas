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
  i_GeoCoder,
  u_GeoCoderBasic;

type
  TGeoCoderByURL = class(TGeoCoderBasic)
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    function GetPointFromFullLink(
      const Astr: AnsiString;
      const ALocalConverter: ILocalCoordConverter
      ):IVectorDataItemPoint;
    function GetPointFromShortLink(
      const Astr,AhttpData: AnsiString;
      const ACancelNotifier: INotifierOperation; AOperationID: Integer
      ):IVectorDataItemPoint;
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
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AResultFactory: IDownloadResultFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  windows,
  SysUtils,
  StrUtils,
  ALFcnString,
  RegExprUtils,
  t_GeoTypes,
  u_InterfaceListSimple,
  u_ResStrings,
  u_DownloadRequest;

{ TGeoCoderByExtLink }


procedure meters_to_lonlat( in_x,in_y : Double; var out_x, out_y : AnsiString; const AFormatSettings: TALFormatSettings);
begin
  out_X := ALFloatToStr(in_X/6378137*180/pi, AFormatSettings);
  out_Y := ALFloatToStr(((arctan(exp(in_Y/6378137))-pi/4)*360)/pi, AFormatSettings);
end;

constructor TGeoCoderByURL.Create(const AInetSettings: IInetConfig;
  const AGCNotifier: INotifierTime;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AResultFactory: IDownloadResultFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(AInetSettings, AGCNotifier, APlacemarkFactory, AResultFactory);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;

function TGeoCoderByURL.GetPointFromShortLink(
  const Astr,AhttpData: AnsiString;
  const ACancelNotifier: INotifierOperation; AOperationID: Integer
  ): IVectorDataItemPoint;
var
 VPlace : IVectorDataItemPoint;
 VPoint : TDoublePoint;
 slat, slon: AnsiString;
 sname, sdesc, sfulldesc : string;
 VUrl: AnsiString;
 VFormatSettings: TALFormatSettings;
 VLink : AnsiString;
 i, j : integer;
 VHeader: AnsiString;
 VRequest: IDownloadRequest;
 VResult: IDownloadResult;
 VResultOk: IDownloadResultOk;
 VValueConverter: IValueToStringConverter;
begin
 VValueConverter := FValueToStringConverterConfig.GetStatic;
 VLink := ALStringReplace(AStr,'%2C',',', [rfReplaceAll]);
 VFormatSettings.DecimalSeparator := '.';
 sname := '';
 sdesc := '';

 if ALPosEx('http://g.co/', VLink, 1) > 0 then begin
  sname := 'google';
  VLink := AhttpData;
  i := ALPosEx('ll', VLink, 1);
  j := ALPosEx(',', VLink, i);
  slat := Copy(VLink, i + 3, j - (i + 3));
  i := j;
  j := ALPosEx('&', VLink, i);
  slon := Copy(VLink, i + 1, j - (i + 1));
 end;

 if ALPosEx('http://goo.gl/maps/', VLink, 1) > 0 then begin
  sname := 'google';
  VLink := AhttpData;
  i := ALPosEx('ll', VLink, 1);
  j := ALPosEx(',', VLink, i);
  slat := Copy(VLink, i + 3, j - (i + 3));
  i := j;
  j := ALPosEx('&', VLink, i);
  slon := Copy(VLink, i + 1, j - (i + 1));
 end;

 if (RegExprGetMatchSubStr(VLink,'\.yandex\..+/-/',0)<>'' ) or
    (ALPosEx('maps.yandex.ru/?oid=', VLink, 1) > 0 )then begin
  sname := 'yandex';
  VLink := ALStringReplace(AhttpData,'''','', [rfReplaceAll]);
  i := ALPosEx('{ll:', VLink, 1);
  if i=0 then i := ALPosEx(',ll:', VLink, 1);
  j := ALPosEx(',', VLink, i+1);
  slon := Copy(VLink, i + 4, j - (i + 4));
  i := j;
  j := ALPosEx(',', VLink, i+1);
  slat := Copy(VLink, i + 1, j - (i + 1));
 end;

 if (ALPosEx('maps.yandex.ru/?um=', VLink, 1) > 0 ) then begin // need 2 more test
  sname := 'yandex';
  VLink := AhttpData;
  i := ALPosEx('{''bounds'':[[', VLink, 1);
  if i=0 then i := ALPosEx(',ll:', VLink, 1);
  j := ALPosEx(',', VLink, i+1);
  slon := Copy(VLink, i + 12, j - (i + 12));
  i := j;
  j := ALPosEx(']', VLink, i+1);
  slat := Copy(VLink, i + 1, j - (i + 1));
 end;

 if ALPosEx('binged.it', VLink, 1) > 0then begin
  sname := 'bing';
  VLink := ALStringReplace(AhttpData,'%2c',',', [rfReplaceAll]);
  if RegExprGetMatchSubStr(VLink,'bing\.com\..+cp=[0-9]+', 0) <> '' then begin
   i := ALPosEx('cp=', VLink, 1);
   j := ALPosEx('~', VLink, i);
   slat := Copy(VLink, i + 3, j - (i + 3));
   i := j;
   j := ALPosEx('&', VLink, i);
   slon := Copy(VLink, i + 1, j - (i + 1));
  end;
  if RegExprGetMatchSubStr(VLink,'where1=[0-9]+', 0) <> '' then begin
   i := ALPosEx('where1=', VLink, 1);
   j := ALPosEx(',', VLink, i);
   slat := Copy(VLink, i + 7, j - (i + 7));
   i := j+1;
   j := ALPosEx('"', VLink, i);
   slon := Copy(VLink, i + 1, j - (i + 1));
   slon := ALStringReplace(slon,',','.', [rfReplaceAll]);
   slat := ALStringReplace(slat,',','.', [rfReplaceAll]);
  end;
 end;

 if ALPosEx('osm.org', VLink, 1) > 0then begin
  sname := 'osm';
  VLink := AhttpData;
  i := ALPosEx('LonLat(', VLink, 1);
  j := ALPosEx(',', VLink, i);
  slon := Copy(VLink, i + 7, j - (i + 7));
  i := j+1;
  j := ALPosEx(')', VLink, i);
  slat := Copy(VLink, i + 1, j - (i + 1));
 end;

 if ALPosEx('rambler.ru', VLink, 1) > 0then begin
  sname := 'rambler';
  VLink := ALStringReplace(AhttpData,'\"','', [rfReplaceAll]);
  i := ALPosEx('lon:', VLink, 1);
  j := ALPosEx(',', VLink, i+1);
  slon := Copy(VLink, i + 4, j - (i + 4));
  i := ALPosEx('lat:', VLink, j);
  j := ALPosEx('}', VLink, i+1);
  slat := Copy(VLink, i + 4, j - (i + 4));
 end;

 if ALPosEx('permalink.html', VLink, 1) > 0then begin
  VUrl := 'http://kosmosnimki.ru/TinyReference.ashx?id='+Copy(VLink,38,9);
  VHeader := 'Referer: '+VLink+' Cookie: TinyReference='+Copy(VLink,38,9);
  VLink := '';
  VRequest := TDownloadRequest.Create(VUrl, VHeader, InetSettings.GetStatic);
  VResult := Downloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
  if Supports(VResult, IDownloadResultOk, VResultOk) then begin
    SetLength(VLink, VResultOk.Data.Size);
    Move(VResultOk.Data.Buffer^, VLink[1], VResultOk.Data.Size);
    i := ALPosEx('"x":', VLink, 1);
    j := ALPosEx(',', VLink, i + 4 );
    slon := Copy(VLink, i + 4, j - (i + 4));
    i := ALPosEx('"y":', VLink, j);
    j := ALPosEx(',', VLink, i + 4 );
    slat := Copy(VLink, i + 4, j - (i + 4));
    sfulldesc := string(VLink);
    sname := 'kosmosnimki';
    meters_to_lonlat(ALStrToFloat(slon, VFormatSettings),ALStrToFloat(slat, VFormatSettings),slon,slat, VFormatSettings);
  end;
 end ;

 if ALPosEx('api/index.html?permalink=', VLink, 1) > 0 then begin
  slat := Copy(VLink,53,5);
  slon := Copy(VLink,59,5);
  VUrl := 'http://maps.kosmosnimki.ru/TinyReference/Get.ashx?id='+slat;
  VHeader := 'Referer: http://maps.kosmosnimki.ru/api/index.html?'+slon;
  VRequest := TDownloadRequest.Create(VUrl, VHeader, InetSettings.GetStatic);
  VResult := Downloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
  if Supports(VResult, IDownloadResultOk, VResultOk) then begin
    SetLength(VLink, VResultOk.Data.Size);
    Move(VResultOk.Data.Buffer^, VLink[1], VResultOk.Data.Size);
    VLink := ALStringReplace(VLink,'\','', [rfReplaceAll]);
    i := ALPosEx('"x":', VLink, 1);
    j := ALPosEx(',', VLink, i + 4 );
    slon := Copy(VLink, i + 4, j - (i + 4));
    i := ALPosEx('"y":', VLink, j);
    j := ALPosEx(',', VLink, i + 4 );
    slat := Copy(VLink, i + 4, j - (i + 4));
    sfulldesc := string(VLink);
    sname := 'maps.kosmosnimki';
    meters_to_lonlat(ALStrToFloat(slon, VFormatSettings),ALStrToFloat(slat, VFormatSettings),slon,slat, VFormatSettings);
  end;
 end;

 if ALPosEx('go.2gis.ru', VLink, 1) > 0then begin
  VUrl := VLink;
  VHeader := 'Cookie: 2gisAPI=c2de06c2dd3109de8ca09a59ee197a4210495664eeae8d4075848.943590';
  VLink := '';
  VRequest := TDownloadRequest.Create(VUrl, VHeader, InetSettings.GetStatic);
  VResult := Downloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
  if Supports(VResult, IDownloadResultOk, VResultOk) then begin
    SetLength(VLink, VResultOk.Data.Size);
    Move(VResultOk.Data.Buffer^, VLink[1], VResultOk.Data.Size);
    i := ALPosEx('center/', VLink, 1);
    j := ALPosEx(',', VLink, i );
    slon := Copy(VLink, i + 7, j - (i + 7));
    i := j;
    j := ALPosEx('/', VLink, i );
    slat := Copy(VLink, i + 1, j - (i + 1));
    sname := '2gis';
  end;
 end;

 if sname <> '' then begin
  try
    VPoint.Y := ALStrToFloat(slat, VFormatSettings);
    VPoint.X := ALStrToFloat(slon, VFormatSettings);
  except
    raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
  end;
  sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
  sfulldesc := '<a href=' + string(Astr) + '>' +string(Astr)+ '</a><br>' + ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
  VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
  Result := VPlace;
 end else
 Result := nil;
end;

function TGeoCoderByURL.GetPointFromFullLink(const Astr: AnsiString; const ALocalConverter: ILocalCoordConverter): IVectorDataItemPoint;
var
 i, j : integer;
 VPlace : IVectorDataItemPoint;
 VPoint : TDoublePoint;
 slat, slon: AnsiString;
 sname, sdesc, sfulldesc : string;
 VLink : AnsiString;
 VFormatSettings: TALFormatSettings;
 VZoom, Vilon, Vilat: integer;
 XYPoint:TPoint;
 XYRect:TRect;
 VValueConverter: IValueToStringConverter;
begin
 VValueConverter := FValueToStringConverterConfig.GetStatic;
 VLink := ALStringReplace(AStr,'%2C',',', [rfReplaceAll]);
 VFormatSettings.DecimalSeparator := '.';
 sname := '';
 sdesc := '';
 // http://maps.google.com/?ll=48.718079,44.504639&spn=0.722115,1.234589&t=h&z=10
 // http://maps.google.ru/maps?hl=ru&ll=43.460987,39.948606&spn=0.023144,0.038581&t=m&z=15&vpsrc=6
 if RegExprGetMatchSubStr(VLink,'maps\.google\..+ll=[0-9]+', 0) <> '' then begin
  sname := 'Google';
  i := ALPosEx('ll', VLink, 1);
  j := ALPosEx(',', VLink, i);
  slat := Copy(VLink, i + 3, j - (i + 3));
  i := j;
  j := ALPosEx('&', VLink, i);
  slon := Copy(VLink, i + 1, j - (i + 1));
 end;

 // http://maps.navitel.su/?zoom=16&lat=45.03446&lon=38.96867&fl=J&rId=hN21H5ByVER8e4A%3D&rp=5
 if RegExprGetMatchSubStr(VLink,'maps\.navitel\.su.+lat=.+lon=',0) <> '' then begin
  sname := 'Navitel';
  i := ALPosEx('lat=', VLink, 1);
  j := ALPosEx('&', VLink, i);
  slat := Copy(VLink, i + 4, j - (i + 4));
  i := ALPosEx('lon=', VLink, j);
  j := ALPosEx('&', VLink, i);
  if j = 0 then j := length(VLink) +1;
  slon := Copy(VLink, i + 4, j - (i + 4));
 end;

 // http://kosmosnimki.ru/?x=44.1053254382903&y=45.6876903573303&z=6&fullscreen=false&mode=satellite
 if RegExprGetMatchSubStr(VLink,'kosmosnimki\.ru.+x=.+y=',0) <> '' then begin
  sname := 'Kosmosnimki';
  i := ALPosEx('x=', VLink, 1);
  j := ALPosEx('&', VLink, i);
  slon := Copy(VLink, i + 2, j - (i + 2));
  i := ALPosEx('y=', VLink, j);
  j := ALPosEx('&', VLink, i);
  slat := Copy(VLink, i + 2, j - (i + 2));
 end;

 // http://www.bing.com/maps/default.aspx?v=2&cp=45.5493750107145~41.6883332507903&style=h&lvl=6
 if RegExprGetMatchSubStr(VLink,'bing\.com.+cp=[0-9]+',0) <> '' then begin
  sname := 'Bing';
  i := ALPosEx('cp=', VLink, 1);
  j := ALPosEx('~', VLink, i);
  slat := Copy(VLink, i + 3, j - (i + 3));
  i := j;
  j :=ALPosEx('&', VLink, i);
  slon := Copy(VLink, i + 1, j - (i + 1));
 end;

 // http://wikimapia.org#lat=45.0328&lon=38.9769&z=10&l=1&m=b
 if RegExprGetMatchSubStr(VLink,'wikimapia\.org.+lat=.+lon=',0) <> '' then begin
  sname := 'WikiMapia';
  i := ALPosEx('lat=', VLink, 1);
  j := ALPosEx('&', VLink, i);
  slat := Copy(VLink, i + 4, j - (i + 4));
  i := ALPosEx('lon=', VLink, j);
  j := ALPosEx('&', VLink, i);
  slon := Copy(VLink, i + 4, j - (i + 4));
 end;

 // http://maps.rosreestr.ru/Portal/?l=11&x=4595254.155000001&y=5398402.163800001&mls=map|anno&cls=cadastre
 if RegExprGetMatchSubStr(VLink,'maps\.rosreestr\.ru.+x=.+y=',0) <> '' then begin
  sname := 'Rosreestr';
  i := ALPosEx('x=', VLink, 1);
  j := ALPosEx('&', VLink, i);
  slon := Copy(VLink, i + 2, j - (i + 2));
  i := ALPosEx('y=', VLink, j);
  j := ALPosEx('&', VLink, i);
  slat := Copy(VLink, i + 2, j - (i + 2));
  meters_to_lonlat(ALStrToFloat(slon, VFormatSettings),ALStrToFloat(slat, VFormatSettings),slon,slat, VFormatSettings);
 end;

 // http://maps.mail.ru/?z=10&ll=37.619948,55.750023&j=1
 if RegExprGetMatchSubStr(VLink,'maps\.mail\.ru.+ll=',0) <> '' then begin
  sname := 'Mail.ru';
  i := ALPosEx('ll=', VLink, 1);
  j := ALPosEx(',', VLink, i);
  slon := Copy(VLink, i + 3, j - (i + 3));
  i := j;
  j := ALPosEx('&', VLink, i);
  if j = 0 then j := length(VLink) +1;
  slat := Copy(VLink, i + 1, j - (i + 1));
 end;

 // http://maps.nokia.com/#|43.5669132|41.2836342|14|0|0|hybrid.day
 // http://maps.nokia.com/mapcreator/?ns=true#|55.32530472503459|37.811186150077816|18|0|0|
 if RegExprGetMatchSubStr(VLink,'maps\.nokia\.com.+\#\|',0) <> '' then begin
  i := ALPosEx('#|', VLink, 1);
  j := ALPosEx('|', VLink, i+2);
  slat := Copy(VLink, i + 2, j - (i + 2));
  i := j;
  j := ALPosEx('|', VLink, i+1);
  if j = 0 then j := length(VLink) +1;
  slon := Copy(VLink, i + 1, j - (i + 1));
 end;

 // http://maps.yandex.ru/?ll=44.514541%2C48.708958&spn=0.322723%2C0.181775&z=12&l=map
 // http://harita.yandex.com.tr/?ll=29.086777%2C41.000749&spn=0.005043%2C0.003328&z=18&l=sat%2Ctrf&trfm=cur
 if RegExprGetMatchSubStr(VLink,'\.yandex\..+/\?ll=[0-9]+',0) <> '' then begin
  sname := 'Yandex';
  i := ALPosEx('ll', VLink, 1);
  j := ALPosEx(',', VLink, i);
  slon := Copy(VLink, i + 3, j - (i + 3));
  i := j;
  j := ALPosEx('&', VLink, i);
  slat := Copy(VLink, i + 1, j - (i + 1));
 end;

 // http://mobile.maps.yandex.net/ylocation/?lat=55.870155&lon=37.665367&desc=dima%40dzhus.org
 if RegExprGetMatchSubStr(VLink,'\.yandex\..+lat=.+lon=',0) <> '' then begin
  sname := 'Yandex';
  i := ALPosEx('lat=', VLink, 1);
  j := ALPosEx('&', VLink, i+3);
  slat := Copy(VLink, i + 4, j - (i + 4));
  i := ALPosEx('lon=', VLink, j);
  j := ALPosEx('&', VLink, i);
  if j = 0 then j := length(VLink) +1;
  slon := Copy(VLink, i + 4, j - (i + 4));
 end;

 //http://maps.2gis.ru/#/?history=project/krasnodar/center/38.993668%2C45.197055/zoom/17/state/index/sort/relevance
 if RegExprGetMatchSubStr(VLink,'maps\.2gis\.ru.+zoom',0) <> '' then begin
  sname := '2Gis';
  i := ALPosEx('center/', VLink, 1);
  j := ALPosEx(',', VLink, i);
  slon := Copy(VLink, i + 7, j - (i + 7));
  i:=j;
  j := ALPosEx('/', VLink, i);
  if j = 0 then j := length(VLink) +1;
  slat := Copy(VLink, i + 1, j - (i + 1));
 end;

 // http://www.openstreetmap.org/#map=17/45.12333/38.98709
 // http://www.openstreetmap.org/#map=17/45.12333/38.98576&layers=C
 if (RegExprGetMatchSubStr(VLink, '(openstreetmap|osm)\..+map=', 0) <> '') then begin
  sname := 'OpenStreetMap';
  i := ALPosEx('map=', VLink, 1);
  i := ALPosEx('/', VLink, i);
  j := ALPosEx('/', VLink, i + 1);
  slat := Copy(VLink, i + 1, j - (i + 1));

  i := ALPosEx('/', VLink, j);
  j := ALPosEx('&', VLink, i);

  if j = 0 then j := length(VLink) + 1;
  slon := Copy(VLink, i + 1, j - (i + 1));
 end;

 // http://www.openstreetmap.org/?lat=45.227&lon=39.001&zoom=10&layers=M
 // http://osm.org.ru/#layer=M&zoom=3&lat=61.98&lon=88
 if  (RegExprGetMatchSubStr(VLink,'(openstreetmap|osm)\..+lat=',0)<>'') then begin
  sname := 'OpenStreetMap';
  i := ALPosEx('lat=', VLink, 1);
  j := ALPosEx('&', VLink, i);
  slat := Copy(VLink, i + 4, j - (i + 4));
  i := ALPosEx('lon=', VLink, j);
  j := ALPosEx('&', VLink, i);
  if j = 0 then j := length(VLink) +1;
  slon := Copy(VLink, i + 4, j - (i + 4));
 end;

 // http://khm0.google.com/kh/v=127&src=app&x=24398&s=&y=10570&z=15&s=Gali
 if RegExprGetMatchSubStr(VLink,'khm.+google\..+x=[0-9]+', 0) <> '' then begin
  sname := 'Google tile';

  i := ALPosEx('y=', VLink, 1);
  j := ALPosEx('&', VLink, i);
  slat := Copy(VLink, i + 2, j - (i + 2));
  Vilat := ALStrToInt(slat);

  i := ALPosEx('x=', VLink, 1);
  j := ALPosEx('&', VLink, i);
  slon := Copy(VLink, i + 2, j - (i + 2));
  Vilon := ALStrToInt(slon);

  i := ALPosEx('z=', VLink, 1);
  j := ALPosEx('&', VLink, i);
  slon := Copy(VLink, i + 2, j - (i + 2));
  VZoom := ALStrToInt(slon);
  inc(VZoom);

  XYPoint.X:=ViLon;
  XYPoint.Y:=ViLat;
  sdesc := 'z='+inttostr(VZoom)+' x='+inttostr(Vilon)+' y='+inttostr(Vilat)+#$D#$A;
  XYRect := ALocalConverter.GetGeoConverter.TilePos2PixelRect(XYPoint,VZoom-1);
  XYPoint := Point((XYRect.Right+XYRect.Left)div 2,(XYRect.Bottom+XYRect.top)div 2);
  VPoint := ALocalConverter.GetGeoConverter.PixelPos2LonLat(XYPoint,VZoom-1);
  slat := ALFloatToStr(VPoint.Y, VFormatSettings);
  slon := ALFloatToStr(VPoint.X, VFormatSettings);
 end;


 // http://c.tile.openstreetmap.org/10/622/367.png
 if RegExprGetMatchSubStr(VLink,'\.(openstreetmap|opencyclemap|osm).+\.png',0)<>''  then begin
  sname := 'OpenStreetMap';
  i := ALPosEx(RegExprGetMatchSubStr(VLink,'/[0-9]?[0-9]/',0), VLink, 1); // Z значение
  j := ALPosEx('/', VLink, i+1);
  VZoom := (ALStrToInt(Copy(VLink, i + 1, j - (i + 1))));
  inc(VZoom);
  i:= j;
  j := ALPosEx('/', VLink, i+1);
  slon := Copy(VLink, i + 1, j - (i + 1));
  Vilon := ALStrToInt(slon);
  i:= j;
  j := ALPosEx('.', VLink, i+1);
  slat := Copy(VLink, i + 1, j - (i + 1));
  Vilat := ALStrToInt(slat);
  XYPoint.X:=ViLon;
  XYPoint.Y:=ViLat;
  sdesc := 'z='+inttostr(VZoom)+' x='+inttostr(Vilon)+' y='+inttostr(Vilat)+#$D#$A;
  XYRect := ALocalConverter.GetGeoConverter.TilePos2PixelRect(XYPoint,VZoom-1);
  XYPoint := Point((XYRect.Right+XYRect.Left)div 2,(XYRect.Bottom+XYRect.top)div 2);
  VPoint := ALocalConverter.GetGeoConverter.PixelPos2LonLat(XYPoint,VZoom-1);
  slat := ALFloatToStr(VPoint.Y, VFormatSettings);
  slon := ALFloatToStr(VPoint.X, VFormatSettings);
 end;

 // http://188.95.188.28/cgi-bin/webfile_mgr.cgi?cmd=cgi_download&path=/mnt/HD/HD_a2/pub/genshtab250m/z12/1302/2506.jpg&path1=/mnt/HD/HD_a2/pub/genshtab250m/z12/1302/2506.jpg&name=2506.jpg&type=JPEG+Image&browser=iee)
 if RegExprGetMatchSubStr(VLink,'/z[0-9]+/.+\.(png|jpg)+',0)<>''  then
 if sname = '' then begin
  sname := string(RegExprGetMatchSubStr(VLink,'http://[0-9a-zа-я\.]+',0));
  i := ALPosEx('/z', VLink, 1);
  if i>0 then begin
   j := ALPosEx('/', VLink, i+1);
   slat := Copy(VLink, i + 2, j - (i + 2));
    try
     VZoom := ALStrToInt(slat);
    except
     VZoom := 0 ;
    end;
   i := ALPosEx('/', VLink, j); // X значение
   j := ALPosEx('/', VLink, i+1);
   slon := Copy(VLink, i + 1, j - (i + 1));
   Vilat := ALStrToInt(slon);
   i := j; // Y значение
   j := ALPosEx('.', VLink, i+1);
   slat := Copy(VLink, i + 1, j - (i + 1));
   Vilon := ALStrToInt(slat);
   inc(VZoom); // зум отличается на 1
   XYPoint.X:=ViLon;
   XYPoint.Y:=ViLat;
   sdesc := 'z='+inttostr(VZoom)+' x='+inttostr(Vilon)+' y='+inttostr(Vilat)+#$D#$A;
   XYRect := ALocalConverter.GetGeoConverter.TilePos2PixelRect(XYPoint,VZoom-1);
   XYPoint := Point((XYRect.Right+XYRect.Left)div 2,(XYRect.Bottom+XYRect.top)div 2);
   VPoint := ALocalConverter.GetGeoConverter.PixelPos2LonLat(XYPoint,VZoom-1);
   slat := ALFloatToStr(VPoint.Y, VFormatSettings);
   slon := ALFloatToStr(VPoint.X, VFormatSettings);
  end ;
 end ;

 // http://wikimapia.org/d?lng=1&BBOX=42.84668,43.26121,42.89063,43.29320
 // http://www.openstreetmap.org/?box=yes&bbox=41.73729%2C44.25345%2C41.73729%2C44.25345
 if RegExprGetMatchSubStr(AlUpperCase(VLink),'BBOX=([0-9]+.[0-9]+\,)+([0-9]+.[0-9]+)',0)<>''  then
  if sname = '' then begin
   sname := string(RegExprGetMatchSubStr(VLink,'http://[0-9a-zа-я\.]+',0));
   i := ALPosEx('BBOX=', AlUpperCase(VLink))+4;
   j := ALPosEx(',', VLink, i+1);
   slon := Copy(VLink, i + 1, j - (i + 1));
   i := j;
   j := ALPosEx(',', VLink, i+1);
   slat := Copy(VLink, i + 1, j - (i + 1));
   i := j;
   j := ALPosEx(',', VLink, i+1);
   slon := ALFloatToStr((ALStrToFloat(Copy(VLink, i + 1, j - (i + 1)),VFormatSettings)+ALStrToFloat(slon,VFormatSettings))/2, VFormatSettings);
   i := j;
   j := ALPosEx('&', VLink, i+1);
   if j=0 then j:=length(VLink);
   slat := ALFloatToStr((ALStrToFloat(Copy(VLink, i + 1, j - (i + 1)),VFormatSettings)+ALStrToFloat(slat,VFormatSettings))/2, VFormatSettings);
   slat := ALStringReplace(slat,',','.', [rfReplaceAll]);
   slon := ALStringReplace(slon,',','.', [rfReplaceAll]);
   if (ALStrToFloat(slat,VFormatSettings)>360)or(ALStrToFloat(slon,VFormatSettings)>360) then
   begin
     meters_to_lonlat(ALStrToFloat(slon, VFormatSettings),ALStrToFloat(slat, VFormatSettings),slon,slat, VFormatSettings);
   end;
  end;

 // http://чепецк.net/?zoom=15&lat=43.94165&lon=40.14849&layers=BFFFT
 if  RegExprGetMatchSubStr(AlUpperCase(VLink),'LAT=.+LON=',0)<>''  then
  if sname = '' then begin
   sname := string(RegExprGetMatchSubStr(VLink,'http://[0-9a-zа-я\.]+',0));
   i := ALPosEx('LAT=', AlUpperCase(VLink), 1);
   j := ALPosEx('&', VLink, i);
   slat := Copy(VLink, i + 4, j - (i + 4));
   i := ALPosEx('LON=', AlUpperCase(VLink), j);
   j := ALPosEx('&', VLink, i);
   if j = 0 then j := length(VLink) +1;
   slon := Copy(VLink, i + 4, j - (i + 4));
  end;

 if sname <> '' then begin
  try
    VPoint.Y := ALStrToFloat(slat, VFormatSettings);
    VPoint.X := ALStrToFloat(slon, VFormatSettings);
  except
    raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
  end;
  sdesc := sdesc + '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
  sfulldesc := '<a href=' + string(Astr) + '>' +string(Astr)+ '</a><br>' + ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
  VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
  Result := VPlace;
 end else
 Result := nil;

end;

function TGeoCoderByURL.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation; AOperationID: Integer;
  const AResult: IDownloadResultOk; const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter): IInterfaceListSimple;
var
 VPlace: IVectorDataItemPoint;
 VList: IInterfaceListSimple;
 VStr: AnsiString;
 VUrl: AnsiString;
begin
 VUrl := AnsiString(ASearch);
 if AResult = nil then // ничего не скачивали, полная ссылка
  VPlace := GetPointFromFullLink(VUrl, ALocalConverter)
 else begin // короткая ссылка, и данные уже скачаны
  SetLength(Vstr, AResult.Data.Size);
  Move(AResult.Data.Buffer^, Vstr[1], AResult.Data.Size);
  VPlace := GetPointFromShortLink(VUrl,VStr,ACancelNotifier,AOperationID);
 end;

  if VPlace <> nil then begin
    VList := TInterfaceListSimple.Create;
    VList.Add(VPlace)
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
  if (ALPosEx('http://g.co/', VUrl, 1) > 0 )or
     (ALPosEx('http://goo.gl/maps/', VUrl, 1) > 0 )or
     (ALPosEx('yandex.ru/?oid=', VUrl, 1) > 0 )or
     (ALPosEx('binged.it', VUrl, 1) > 0 )or
     (ALPosEx('osm.org', VUrl, 1) > 0 )or
     (ALPosEx('permalink.html', VUrl, 1) > 0 )or
     (ALPosEx('api/index.html?permalink=', VUrl, 1) > 0 ) or
     (ALPosEx('rambler.ru/?', VUrl, 1) > 0 ) or
     (ALPosEx('yandex.ru/?um=', VUrl, 1) > 0 ) or
     (RegExprGetMatchSubStr(VUrl,'\.yandex\..+/-/',0)<>'' )
  then begin
   Result := PrepareRequestByURL(VUrl);
  end;
end;
end.


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
// http://go.2gis.ru/1hox
// http://maps.rambler.ru/?6rJJy58
// http://maps.yandex.ru/?um=m4VoZPqVSEwQ3YdT5Lmley6KrBsHb2oh&l=sat
// http://harita.yandex.com.tr/-/CFXxAO3m


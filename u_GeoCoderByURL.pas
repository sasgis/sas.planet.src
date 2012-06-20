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
  i_OperationNotifier,
  i_LocalCoordConverter,
  i_DownloadRequest,
  i_DownloadResult,
  i_InetConfig,
  i_TTLCheckNotifier,
  i_DownloadResultFactory,
  i_ValueToStringConverter,
  i_GeoCoder,
  u_GeoCoderBasic;

type
  TGeoCoderByURL = class(TGeoCoderBasic)
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    function deg2strvalue(
      aDeg: Double;
      Alat, NeedChar: boolean
      ):string;
    function GetPointFromFullLink(
      const Astr: string;
      const ALocalConverter: ILocalCoordConverter
      ):IGeoCodePlacemark;
    function GetPointFromShortLink(
      const Astr,AhttpData: string;
      const ACancelNotifier: IOperationNotifier; AOperationID: Integer
      ):IGeoCodePlacemark;
  protected
    function PrepareRequest(
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
      ): IDownloadRequest; override;
    function ParseResultToPlacemarksList(
      const ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      const AResult: IDownloadResultOk;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceList; override;
  public
    constructor Create(
      const AInetSettings: IInetConfig;
      const AGCList: ITTLCheckNotifier;
      const AResultFactory: IDownloadResultFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  windows,
  SysUtils,
  Math,
  StrUtils,
  RegExprUtils,
  t_GeoTypes,
  t_CommonTypes,
  u_ResStrings,
  u_DownloadRequest,
  u_GeoCodePlacemark,
  u_GeoToStr;

{ TGeoCoderByExtLink }


procedure meters_to_lonlat( in_x,in_y : Double; var out_x, out_y : string);
begin
  out_X := floattostr(in_X/6378137*180/pi);
  out_Y := floattostr(((arctan(exp(in_Y/6378137))-pi/4)*360)/pi);
end;

function TGeoCoderByURL.deg2strvalue( aDeg:Double; Alat,NeedChar:boolean):string;
var
  VDegr: Double;
  VInt: Integer;
  VValue: Integer;
begin
  VDegr := abs(ADeg);

  case FValueToStringConverterConfig.DegrShowFormat of
    dshCharDegrMinSec, dshSignDegrMinSec: begin
      VValue := Trunc(VDegr * 60 * 60 * 100 + 0.005);
      VInt := Trunc(VValue / (60 * 60 * 100));
      VValue := VValue - VInt * (60 * 60 * 100);
      result := IntToStr(VInt) + '°';

      VInt := Trunc(VValue / (60 * 100));
      VValue := VValue - VInt * (60 * 100);

      if VInt < 10 then begin
        Result := result + '0' + IntToStr(VInt) + '''';
      end else begin
        Result := result + IntToStr(VInt) + '''';
      end;

      Result := Result + FormatFloat('00.00', VValue / 100) + '"';
    end;
    dshCharDegrMin, dshSignDegrMin: begin
      VValue := Trunc(VDegr * 60 * 10000 + 0.00005);
      VInt := Trunc(VValue / (60 * 10000));
      VValue := VValue - VInt * (60 * 10000);
      Result := IntToStr(VInt) + '°';
      Result := Result + FormatFloat('00.0000', VValue / 10000) + '''';
    end;
    dshCharDegr, dshSignDegr: begin
      Result := FormatFloat('0.000000', VDegr) + '°';
    end;
  end;

   if NeedChar then
    if Alat then begin
    if aDeg>0 then Result := 'N'+ Result else Result := 'S'+ Result ;
    end else
    if aDeg>0 then Result := 'E'+ Result else Result := 'W'+ Result ;
end;

constructor TGeoCoderByURL.Create(const AInetSettings: IInetConfig;
  const AGCList: ITTLCheckNotifier;
  const AResultFactory: IDownloadResultFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(AInetSettings, AGCList, AResultFactory);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;

function TGeoCoderByURL.GetPointFromShortLink(
  const Astr,AhttpData: string;
  const ACancelNotifier: IOperationNotifier; AOperationID: Integer
  ):IGeoCodePlacemark;
var
 VPlace : IGeoCodePlacemark;
 VPoint : TDoublePoint;
 slat, slon, sname, sdesc, sfulldesc : string;
 VFormatSettings: TFormatSettings;
 Vlink : string;
 i, j : integer;
 VHeader: string;
 VRequest: IDownloadRequest;
 VResult: IDownloadResult;
 VResultOk: IDownloadResultOk;
begin
 Vlink := ReplaceStr(AStr,'%2C',',');
 VFormatSettings.DecimalSeparator := '.';
 sname := '';
 sdesc := '';

 if PosEx('http://g.co/', Vlink, 1) > 0then begin
  sname := 'google';
  Vlink := AhttpData;
  i := PosEx('ll', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
 end;

 if (RegExprGetMatchSubStr(vlink,'\.yandex\..+/-/',0)<>'' ) or
    (PosEx('maps.yandex.ru/?oid=', Vlink, 1) > 0 )then begin
  sname := 'yandex';
  Vlink := ReplaceStr(AhttpData,'''','');
  i := PosEx('{ll:', Vlink, 1);
  if i=0 then i := PosEx(',ll:', Vlink, 1);
  j := PosEx(',', Vlink, i+1);
  slon := Copy(Vlink, i + 4, j - (i + 4));
  i := j;
  j := PosEx(',', Vlink, i+1);
  slat := Copy(Vlink, i + 1, j - (i + 1));
 end;

 if (PosEx('maps.yandex.ru/?um=', Vlink, 1) > 0 ) then begin // need 2 more test
  sname := 'yandex';
  Vlink := AhttpData;
  i := PosEx('{''bounds'':[[', Vlink, 1);
  if i=0 then i := PosEx(',ll:', Vlink, 1);
  j := PosEx(',', Vlink, i+1);
  slon := Copy(Vlink, i + 12, j - (i + 12));
  i := j;
  j := PosEx(']', Vlink, i+1);
  slat := Copy(Vlink, i + 1, j - (i + 1));
 end;

 if PosEx('binged.it', Vlink, 1) > 0then begin
  sname := 'bing';
  Vlink := AhttpData;
  i := PosEx('cp=', Vlink, 1);
  j := PosEx('~', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
 end;

 if PosEx('osm.org', Vlink, 1) > 0then begin
  sname := 'osm';
  Vlink := AhttpData;
  i := PosEx('LonLat(', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slon := Copy(Vlink, i + 7, j - (i + 7));
  i := j+1;
  j := PosEx(')', Vlink, i);
  slat := Copy(Vlink, i + 1, j - (i + 1));
 end;

 if PosEx('rambler.ru', Vlink, 1) > 0then begin
  sname := 'rambler';
  Vlink := ReplaceStr(AhttpData,'\"','');
  i := PosEx('lon:', Vlink, 1);
  j := PosEx(',', Vlink, i+1);
  slon := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lat:', Vlink, j);
  j := PosEx('}', Vlink, i+1);
  slat := Copy(Vlink, i + 4, j - (i + 4));
 end;

 if PosEx('permalink.html', Vlink, 1) > 0then begin
  sdesc := 'http://kosmosnimki.ru/TinyReference.ashx?id='+Copy(vlink,38,9);
  VHeader := 'Referer: '+vlink+' Cookie: TinyReference='+Copy(vlink,38,9);
  Vlink := '';
  VRequest := TDownloadRequest.Create(sdesc, VHeader, InetSettings.GetStatic);
  VResult := Downloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
  if Supports(VResult, IDownloadResultOk, VResultOk) then begin
    SetLength(Vlink, VResultOk.Data.Size);
    Move(VResultOk.Data.Buffer^, Vlink[1], VResultOk.Data.Size);
    i := PosEx('"x":', Vlink, 1);
    j := PosEx(',', Vlink, i + 4 );
    slon := Copy(Vlink, i + 4, j - (i + 4));
    i := PosEx('"y":', Vlink, j);
    j := PosEx(',', Vlink, i + 4 );
    slat := Copy(Vlink, i + 4, j - (i + 4));
    sfulldesc := Vlink;
    sname := 'kosmosnimki';
    meters_to_lonlat(StrToFloat(slon, VFormatSettings),StrToFloat(slat, VFormatSettings),slon,slat);
    slon := ReplaceStr(slon,',','.');
    slat := ReplaceStr(slat,',','.');
  end;
 end ;

 if PosEx('api/index.html?permalink=', Vlink, 1) > 0 then begin
  slat := Copy(vlink,53,5);
  slon := Copy(vlink,59,5);
  sdesc := 'http://maps.kosmosnimki.ru/TinyReference/Get.ashx?id='+slat;
  VHeader := 'Referer: http://maps.kosmosnimki.ru/api/index.html?'+slon;
  VRequest := TDownloadRequest.Create(sdesc, VHeader, InetSettings.GetStatic);
  VResult := Downloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
  if Supports(VResult, IDownloadResultOk, VResultOk) then begin
    SetLength(Vlink, VResultOk.Data.Size);
    Move(VResultOk.Data.Buffer^, Vlink[1], VResultOk.Data.Size);
    Vlink := ReplaceStr(Vlink,'\','');
    i := PosEx('"x":', Vlink, 1);
    j := PosEx(',', Vlink, i + 4 );
    slon := Copy(Vlink, i + 4, j - (i + 4));
    i := PosEx('"y":', Vlink, j);
    j := PosEx(',', Vlink, i + 4 );
    slat := Copy(Vlink, i + 4, j - (i + 4));
    sfulldesc := Vlink;
    sname := 'maps.kosmosnimki';
    meters_to_lonlat(StrToFloat(slon, VFormatSettings),StrToFloat(slat, VFormatSettings),slon,slat);
    slon := ReplaceStr(slon,',','.');
    slat := ReplaceStr(slat,',','.');
  end;
 end;

 if PosEx('go.2gis.ru', Vlink, 1) > 0then begin
  sdesc := vlink;
  VHeader := 'Cookie: 2gisAPI=c2de06c2dd3109de8ca09a59ee197a4210495664eeae8d4075848.943590';
  Vlink := '';
  VRequest := TDownloadRequest.Create(sdesc, VHeader, InetSettings.GetStatic);
  VResult := Downloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
  if Supports(VResult, IDownloadResultOk, VResultOk) then begin
    SetLength(Vlink, VResultOk.Data.Size);
    Move(VResultOk.Data.Buffer^, Vlink[1], VResultOk.Data.Size);
    i := PosEx('center/', sname, 1);
    j := PosEx(',', sname, i );
    slon := Copy(sname, i + 7, j - (i + 7));
    i := j;
    j := PosEx('/', sname, i );
    slat := Copy(sname, i + 1, j - (i + 1));
    sname := '2gis';
  end;
 end;

 if sname <> '' then begin
  try
    VPoint.Y := StrToFloat(slat, VFormatSettings);
    VPoint.X := StrToFloat(slon, VFormatSettings);
  except
    raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
  end;
  if FValueToStringConverterConfig.IsLatitudeFirst = true then
   sdesc := sdesc + '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
    sdesc := sdesc + '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
  sfulldesc := '<a href=' + Astr + '>' +Astr+ '</a><br>' + ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
  VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
  Result := VPlace;
 end else
 Result := nil;
end;

function TGeoCoderByURL.GetPointFromFullLink(const Astr:string; const ALocalConverter: ILocalCoordConverter):IGeoCodePlacemark;
var
 i, j : integer;
 VPlace : IGeoCodePlacemark;
 VPoint : TDoublePoint;
 slat, slon, sname, sdesc, sfulldesc : string;
 Vlink : string;
 VFormatSettings: TFormatSettings;
 VZoom, Vilon, Vilat: integer;
 XYPoint:TPoint;
 XYRect:TRect;
begin
 Vlink := ReplaceStr(AStr,'%2C',',');
 VFormatSettings.DecimalSeparator := '.';
 sname := '';
 sdesc := '';
 // http://maps.google.com/?ll=48.718079,44.504639&spn=0.722115,1.234589&t=h&z=10
 // http://maps.google.ru/maps?hl=ru&ll=43.460987,39.948606&spn=0.023144,0.038581&t=m&z=15&vpsrc=6
 if RegExprGetMatchSubStr(Vlink,'maps\.google\..+ll=[0-9]+', 0) <> '' then begin
  sname := 'Google';
  i := PosEx('ll', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
 end;

 // http://maps.navitel.su/?zoom=16&lat=45.03446&lon=38.96867&fl=J&rId=hN21H5ByVER8e4A%3D&rp=5
 if RegExprGetMatchSubStr(Vlink,'maps\.navitel\.su.+lat=.+lon=',0) <> '' then begin
  sname := 'Navitel';
  i := PosEx('lat=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lon=', Vlink, j);
  j := PosEx('&', Vlink, i);
  if j = 0 then j := length(Vlink) +1;
  slon := Copy(Vlink, i + 4, j - (i + 4));
 end;

 // http://kosmosnimki.ru/?x=44.1053254382903&y=45.6876903573303&z=6&fullscreen=false&mode=satellite
 if RegExprGetMatchSubStr(Vlink,'kosmosnimki\.ru.+x=.+y=',0) <> '' then begin
  sname := 'Kosmosnimki';
  i := PosEx('x=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 2, j - (i + 2));
  i := PosEx('y=', Vlink, j);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 2, j - (i + 2));
 end;

 // http://www.bing.com/maps/default.aspx?v=2&cp=45.5493750107145~41.6883332507903&style=h&lvl=6
 if RegExprGetMatchSubStr(Vlink,'bing\.com.+cp=[0-9]+',0) <> '' then begin
  sname := 'Bing';
  i := PosEx('cp=', Vlink, 1);
  j := PosEx('~', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j :=PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
 end;

 // http://wikimapia.org#lat=45.0328&lon=38.9769&z=10&l=1&m=b
 if RegExprGetMatchSubStr(Vlink,'wikimapia\.org.+lat=.+lon=',0) <> '' then begin
  sname := 'WikiMapia';
  i := PosEx('lat=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lon=', Vlink, j);
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 4, j - (i + 4));
 end;

 // http://maps.rosreestr.ru/Portal/?l=11&x=4595254.155000001&y=5398402.163800001&mls=map|anno&cls=cadastre
 if RegExprGetMatchSubStr(Vlink,'maps\.rosreestr\.ru.+x=.+y=',0) <> '' then begin
  sname := 'Rosreestr';
  i := PosEx('x=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 2, j - (i + 2));
  i := PosEx('y=', Vlink, j);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 2, j - (i + 2));
  meters_to_lonlat(StrToFloat(slon, VFormatSettings),StrToFloat(slat, VFormatSettings),slon,slat);
  slon := ReplaceStr(slon,',','.');
  slat := ReplaceStr(slat,',','.');
 end;

 // http://maps.mail.ru/?z=10&ll=37.619948,55.750023&j=1
 if RegExprGetMatchSubStr(Vlink,'maps\.mail\.ru.+ll=',0) <> '' then begin
  sname := 'Mail.ru';
  i := PosEx('ll=', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slon := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  if j = 0 then j := length(Vlink) +1;
  slat := Copy(Vlink, i + 1, j - (i + 1));
 end;

 // http://maps.nokia.com/#|43.5669132|41.2836342|14|0|0|hybrid.day
 // http://maps.nokia.com/mapcreator/?ns=true#|55.32530472503459|37.811186150077816|18|0|0|
 if RegExprGetMatchSubStr(Vlink,'maps\.nokia\.com.+\#\|',0) <> '' then begin
  i := PosEx('#|', Vlink, 1);
  j := PosEx('|', Vlink, i+2);
  slat := Copy(Vlink, i + 2, j - (i + 2));
  i := j;
  j := PosEx('|', Vlink, i+1);
  if j = 0 then j := length(Vlink) +1;
  slon := Copy(Vlink, i + 1, j - (i + 1));
 end;

 // http://maps.yandex.ru/?ll=44.514541%2C48.708958&spn=0.322723%2C0.181775&z=12&l=map
 // http://harita.yandex.com.tr/?ll=29.086777%2C41.000749&spn=0.005043%2C0.003328&z=18&l=sat%2Ctrf&trfm=cur
 if RegExprGetMatchSubStr(Vlink,'\.yandex\..+/\?ll=[0-9]+',0) <> '' then begin
  sname := 'Yandex';
  i := PosEx('ll', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slon := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 1, j - (i + 1));
 end;

 // http://mobile.maps.yandex.net/ylocation/?lat=55.870155&lon=37.665367&desc=dima%40dzhus.org
 if RegExprGetMatchSubStr(Vlink,'\.yandex\..+lat=.+lon=',0) <> '' then begin
  sname := 'Yandex';
  i := PosEx('lat=', Vlink, 1);
  j := PosEx('&', Vlink, i+3);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lon=', Vlink, j);
  j := PosEx('&', Vlink, i);
  if j = 0 then j := length(Vlink) +1;
  slon := Copy(Vlink, i + 4, j - (i + 4));
 end;

 //http://maps.2gis.ru/#/?history=project/krasnodar/center/38.993668%2C45.197055/zoom/17/state/index/sort/relevance
 if RegExprGetMatchSubStr(Vlink,'maps\.2gis\.ru.+zoom',0) <> '' then begin
  sname := '2Gis';
  i := PosEx('center/', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slon := Copy(Vlink, i + 7, j - (i + 7));
  i:=j;
  j := PosEx('/', Vlink, i);
  if j = 0 then j := length(Vlink) +1;
  slat := Copy(Vlink, i + 1, j - (i + 1));
 end;

 // http://www.openstreetmap.org/?lat=45.227&lon=39.001&zoom=10&layers=M
 // http://osm.org.ru/#layer=M&zoom=3&lat=61.98&lon=88
 if  (RegExprGetMatchSubStr(Vlink,'(openstreetmap|osm)\..+lat=',0)<>'') then begin
  sname := 'OpenStreetMap';
  i := PosEx('lat=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lon=', Vlink, j);
  j := PosEx('&', Vlink, i);
  if j = 0 then j := length(Vlink) +1;
  slon := Copy(Vlink, i + 4, j - (i + 4));
 end;

 // http://c.tile.openstreetmap.org/10/622/367.png
 if RegExprGetMatchSubStr(Vlink,'\.(openstreetmap|opencyclemap|osm).+\.png',0)<>''  then begin
  sname := 'OpenStreetMap';
  i := PosEx(RegExprGetMatchSubStr(Vlink,'/[0-9]?[0-9]/',0), Vlink, 1); // Z значение
  j := PosEx('/', Vlink, i+1);
  VZoom := (strtoint(Copy(Vlink, i + 1, j - (i + 1))));
  inc(VZoom);
  i:= j;
  j := PosEx('/', Vlink, i+1);
  slon := Copy(Vlink, i + 1, j - (i + 1));
  Vilon := strtoint(slon);
  i:= j;
  j := PosEx('.', Vlink, i+1);
  slat := Copy(Vlink, i + 1, j - (i + 1));
  Vilat := strtoint(slat);
  XYPoint.X:=ViLon;
  XYPoint.Y:=ViLat;
  sdesc := 'z='+inttostr(vzoom)+' x='+inttostr(Vilon)+' y='+inttostr(Vilat)+#$D#$A;
  XYRect := ALocalConverter.GetGeoConverter.TilePos2PixelRect(XYPoint,VZoom-1);
  XYPoint := Point((XYRect.Right+XYRect.Left)div 2,(XYRect.Bottom+XYRect.top)div 2);
  VPoint := ALocalConverter.GetGeoConverter.PixelPos2LonLat(XYPoint,VZoom-1);
  slat := ReplaceStr(FloatToStr(VPoint.Y),',','.');
  slon := ReplaceStr(FloatToStr(VPoint.X),',','.');
 end;

 // http://188.95.188.28/cgi-bin/webfile_mgr.cgi?cmd=cgi_download&path=/mnt/HD/HD_a2/pub/genshtab250m/z12/1302/2506.jpg&path1=/mnt/HD/HD_a2/pub/genshtab250m/z12/1302/2506.jpg&name=2506.jpg&type=JPEG+Image&browser=iee)
 if RegExprGetMatchSubStr(Vlink,'/z[0-9]+/.+\.(png|jpg)+',0)<>''  then begin
  sname := RegExprGetMatchSubStr(Vlink,'http://[0-9a-zа-я\.]+',0);
  i := PosEx('/z', Vlink, 1);
  if i>0 then begin
   j := PosEx('/', Vlink, i+1);
   slat := Copy(Vlink, i + 2, j - (i + 2));
    try
     vZoom := strtoint(slat);
    except
     vZoom := 0 ;
    end;
   i := PosEx('/', Vlink, j); // X значение
   j := PosEx('/', Vlink, i+1);
   slon := Copy(Vlink, i + 1, j - (i + 1));
   Vilat := strtoint(slon);
   i := j; // Y значение
   j := PosEx('.', Vlink, i+1);
   slat := Copy(Vlink, i + 1, j - (i + 1));
   Vilon := strtoint(slat);
   inc(VZoom); // зум отличается на 1
   XYPoint.X:=ViLon;
   XYPoint.Y:=ViLat;
   sdesc := 'z='+inttostr(vzoom)+' x='+inttostr(Vilon)+' y='+inttostr(Vilat)+#$D#$A;
   XYRect := ALocalConverter.GetGeoConverter.TilePos2PixelRect(XYPoint,VZoom-1);
   XYPoint := Point((XYRect.Right+XYRect.Left)div 2,(XYRect.Bottom+XYRect.top)div 2);
   VPoint := ALocalConverter.GetGeoConverter.PixelPos2LonLat(XYPoint,VZoom-1);
   slat := ReplaceStr(FloatToStr(VPoint.Y),',','.');
   slon := ReplaceStr(FloatToStr(VPoint.X),',','.');
  end ;
 end ;



 if sname <> '' then begin
  try
    VPoint.Y := StrToFloat(slat, VFormatSettings);
    VPoint.X := StrToFloat(slon, VFormatSettings);
  except
    raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
  end;
  if FValueToStringConverterConfig.IsLatitudeFirst = true then
   sdesc := sdesc + '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
    sdesc := sdesc + '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
  sfulldesc := '<a href=' + Astr + '>' +Astr+ '</a><br>' + ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
  VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
  Result := VPlace;
 end else
 Result := nil;

end;

function TGeoCoderByURL.ParseResultToPlacemarksList(
  const ACancelNotifier: IOperationNotifier; AOperationID: Integer;
  const AResult: IDownloadResultOk; const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter): IInterfaceList;
var
 VPlace : IGeoCodePlacemark;
 VList: IInterfaceList;
 VStr: string;
begin
 VList := TInterfaceList.Create;

 if AResult = nil then // ничего не скачивали, полная ссылка
  VPlace := GetPointFromFullLink(ASearch, ALocalConverter)
 else begin // короткая ссылка, и данные уже скачаны
  SetLength(Vstr, AResult.Data.Size);
  Move(AResult.Data.Buffer^, Vstr[1], AResult.Data.Size);
  VPlace := GetPointFromShortLink(ASearch,VStr,ACancelNotifier,AOperationID);
 end;

 if VPlace<> nil then VList.Add(VPlace)
                  else VList := nil;
 Result := VList;
end;

function TGeoCoderByURL.PrepareRequest(
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
begin
  Result := nil;
  if (PosEx('http://g.co/', ASearch, 1) > 0 )or
     (PosEx('yandex.ru/?oid=', ASearch, 1) > 0 )or
     (PosEx('binged.it', ASearch, 1) > 0 )or
     (PosEx('osm.org', ASearch, 1) > 0 )or
     (PosEx('permalink.html', ASearch, 1) > 0 )or
     (PosEx('api/index.html?permalink=', ASearch, 1) > 0 ) or
     (PosEx('rambler.ru/?', ASearch, 1) > 0 ) or
     (PosEx('yandex.ru/?um=', ASearch, 1) > 0 ) or
     (RegExprGetMatchSubStr(ASearch,'\.yandex\..+/-/',0)<>'' )
  then begin
   Result := PrepareRequestByURL(ASearch);
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


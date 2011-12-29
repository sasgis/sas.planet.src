{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  u_GeoCoderBasic,
  i_GeoCoder;

type
  TGeoCoderByURL = class(TGeoCoderBasic)
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
  u_ResStrings,
  u_GeoCodePlacemark,
  ALHTTPCommon,
  ALHttpClient,
  ALWinInetHttpClient,
  i_InetConfig,
  i_ProxySettings,
  u_GlobalState;

{ TGeoCoderByExtLink }

function DoHttpRequest(const ARequestUrl, ARequestHeader, APostData: string; out AResponseHeader, AResponseData: string): Cardinal;
var
  VHttpClient: TALWinInetHTTPClient;
  VHttpResponseHeader: TALHTTPResponseHeader;
  VHttpResponseBody: TMemoryStream;
  VHttpPostData: TMemoryStream;
  VInetConfig: IInetConfigStatic;
  VProxyConfig: IProxyConfigStatic;
  VTmp:TStringList;
begin
  try
    VHttpClient := TALWinInetHTTPClient.Create(nil);
    try
      VHttpResponseHeader := TALHTTPResponseHeader.Create;
      try
        // config
        VInetConfig := GState.InetConfig.GetStatic;
        VHttpClient.RequestHeader.RawHeaderText := ARequestHeader;
        VHttpClient.RequestHeader.Accept := '*/*';
        VHttpClient.ConnectTimeout := VInetConfig.TimeOut;
        VHttpClient.SendTimeout := VInetConfig.TimeOut;
        VHttpClient.ReceiveTimeout := VInetConfig.TimeOut;
        VHttpClient.InternetOptions := [  wHttpIo_No_cache_write,
                                          wHttpIo_Pragma_nocache,
                                          wHttpIo_No_cookies,
                                          wHttpIo_Ignore_cert_cn_invalid,
                                          wHttpIo_Ignore_cert_date_invalid,
                                          wHttpIo_No_auto_redirect
                                       ];
        VProxyConfig := VInetConfig.ProxyConfigStatic;
        if Assigned(VProxyConfig) then begin
          if VProxyConfig.UseIESettings then begin
            VHttpClient.AccessType := wHttpAt_Preconfig
          end else if VProxyConfig.UseProxy then begin
            VHttpClient.AccessType := wHttpAt_Proxy;
            VHttpClient.ProxyParams.ProxyServer :=
              Copy(VProxyConfig.Host, 0, Pos(':', VProxyConfig.Host) - 1);
            VHttpClient.ProxyParams.ProxyPort :=
              StrToInt(Copy(VProxyConfig.Host, Pos(':', VProxyConfig.Host) + 1));
            if VProxyConfig.UseLogin then begin
              VHttpClient.ProxyParams.ProxyUserName := VProxyConfig.Login;
              VHttpClient.ProxyParams.ProxyPassword := VProxyConfig.Password;
            end;
          end else begin
            VHttpClient.AccessType := wHttpAt_Direct;
          end;
        end;
        // request
        VHttpResponseBody := TMemoryStream.Create;
        try
          VTmp := TStringList.Create;
          try
            if APostData <> '' then begin
              VHttpPostData := TMemoryStream.Create;
              try
                VHttpPostData.Position := 0;
                VTmp.Text := APostData;
                VTmp.SaveToStream(VHttpPostData);
                VHttpClient.Post(ARequestUrl, VHttpPostData, VHttpResponseBody, VHttpResponseHeader);
              finally
                VHttpPostData.Free;
              end;
            end else begin
              VHttpClient.Get(ARequestUrl, VHttpResponseBody, VHttpResponseHeader);
            end;
            Result := StrToIntDef(VHttpResponseHeader.StatusCode, 0);
            AResponseHeader := VHttpResponseHeader.RawHeaderText;
            if VHttpResponseBody.Size > 0 then begin
              VHttpResponseBody.Position := 0;
              VTmp.Clear;
              VTmp.LoadFromStream(VHttpResponseBody);
              AResponseData := VTmp.Text;
            end;
          finally
            AResponseHeader := VHttpResponseHeader.RawHeaderText; // save redirect header
            VTmp.Free;
          end;
        finally
          VHttpResponseBody.Free;
        end;
      finally
        VHttpResponseHeader.Free;
      end;
    finally
      VHttpClient.Free;
    end;
  except
    on E: EALHTTPClientException do begin
      Result := E.StatusCode;
      AResponseData := E.Message;
    end;
    on E: EOSError do begin
      Result := E.ErrorCode;
      AResponseHeader := '';
      AResponseData := E.Message;
    end;
  end;
end;


procedure meters_to_lonlat( in_x,in_y : Double; var out_x, out_y : string);
const
 pi = 3.1415926535897932384626433832795;
begin
  out_X := floattostr(in_X/6378137*180/pi);
  out_Y := floattostr(((arctan(exp(in_Y/6378137))-pi/4)*360)/pi);
end;

function TGeoCoderByURL.ParseStringToPlacemarksList(
  AStr: string; ASearch: WideString): IInterfaceList;
var
 VFormatSettings: TFormatSettings;
 VPlace : IGeoCodePlacemark;
 VPoint : TDoublePoint;
 slat, slon, sname, sdesc, sfulldesc : string;
 Vlink : string;
 VList: IInterfaceList;
 VLinkErr : boolean;
 vErrCode: cardinal;
 VHeader: string;
 i, j : integer;
begin
 VLinkErr := false;
 VList := TInterfaceList.Create;
 VFormatSettings.DecimalSeparator := '.';
 Vlink := ReplaceStr(ASearch,'%2C',',');
 vErrCode := 200;
 // http://maps.google.com/?ll=48.718079,44.504639&spn=0.722115,1.234589&t=h&z=10
 // http://maps.google.ru/maps?hl=ru&ll=43.460987,39.948606&spn=0.023144,0.038581&t=m&z=15&vpsrc=6
 if PosEx('maps.google.', Vlink, 1) > 0 then begin
  sname := 'Google';
  i := PosEx('ll', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end else
  // http://maps.yandex.ru/?ll=44.514541%2C48.708958&spn=0.322723%2C0.181775&z=12&l=map
 if PosEx('maps.yandex.ru/?ll=', Vlink, 1) > 0 then begin
  sname := 'Yandex';
  i := PosEx('ll', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slon := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end else
// http://maps.navitel.su/?zoom=16&lat=45.03446&lon=38.96867&fl=J&rId=hN21H5ByVER8e4A%3D&rp=5
 if copy(Vlink,1,22) = 'http://maps.navitel.su' then begin
  sname := 'Navitel';
  i := PosEx('lat=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lon=', Vlink, j);
  j := PosEx('&', Vlink, i);
  if j = 0 then j := length(Vlink) +1;

  slon := Copy(Vlink, i + 4, j - (i + 4));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end else
// http://kosmosnimki.ru/?x=44.1053254382903&y=45.6876903573303&z=6&fullscreen=false&mode=satellite
 if copy(Vlink,1,23) = 'http://kosmosnimki.ru/?' then begin
  sname := 'Kosmosnimki';
  i := PosEx('x=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 2, j - (i + 2));
  i := PosEx('y=', Vlink, j);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 2, j - (i + 2));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end else
// http://www.bing.com/maps/default.aspx?v=2&cp=45.5493750107145~41.6883332507903&style=h&lvl=6
 if PosEx('bing.com', Vlink, 1) > 0 then begin
  sname := 'Bing';
  i := PosEx('cp=', Vlink, 1);
  j := PosEx('~', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j :=PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end  else
// http://www.openstreetmap.org/?lat=45.227&lon=39.001&zoom=10&layers=M
 if PosEx('openstreetmap.org', Vlink, 1) > 0 then begin
  sname := 'OpenStreetMap';
  i := PosEx('lat=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lon=', Vlink, j);
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 4, j - (i + 4));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end  else
// http://wikimapia.org#lat=45.0328&lon=38.9769&z=10&l=1&m=b
 if PosEx('wikimapia.org', Vlink, 1) > 0 then begin
  sname := 'WikiMapia';
  i := PosEx('lat=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lon=', Vlink, j);
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 4, j - (i + 4));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end
 else  // short link
 if PosEx('http://g.co/', Vlink, 1) > 0then begin
  Vlink := Astr;
  sname := 'google';
  i := PosEx('ll', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if (PosEx('maps.yandex.ru/-/', Vlink, 1) > 0 )or
    (PosEx('maps.yandex.ru/?oid=', Vlink, 1) > 0 )then begin
  Vlink := ReplaceStr(astr,'''','');
  sname := 'yandex';
  i := PosEx('{ll:', Vlink, 1);
  if i=0 then i := PosEx(',ll:', Vlink, 1); //
  j := PosEx(',', Vlink, i+1);
  slon := Copy(Vlink, i + 4, j - (i + 4));
  i := j;
  j := PosEx(',', Vlink, i+1);
  slat := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if PosEx('binged.it', Vlink, 1) > 0then begin
  Vlink := Astr;
  sname := 'bing';
  i := PosEx('cp=', Vlink, 1);
  j := PosEx('~', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if PosEx('osm.org', Vlink, 1) > 0then begin
  Vlink := Astr;
  sname := 'osm';
  i := PosEx('LonLat(', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slon := Copy(Vlink, i + 7, j - (i + 7));
  i := j+1;
  j := PosEx(')', Vlink, i);
  slat := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if PosEx('permalink.html', Vlink, 1) > 0then begin
  sdesc := 'http://kosmosnimki.ru/TinyReference.ashx?id='+Copy(vlink,38,9);
  VHeader := 'Referer: '+vlink+' Cookie: TinyReference='+Copy(vlink,38,9);
  Vlink := '';
  vErrCode := DoHttpRequest(sdesc, VHeader ,'',sname,Vlink);
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
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if PosEx('api/index.html?permalink=', Vlink, 1) > 0then begin
  slat := Copy(vlink,53,5);
  slon := Copy(vlink,59,5);
  sdesc := 'http://maps.kosmosnimki.ru/TinyReference/Get.ashx?id='+slat;
  VHeader := 'Referer: http://maps.kosmosnimki.ru/api/index.html?'+slon;
  Vlink := '';
  vErrCode := DoHttpRequest(sdesc, VHeader ,'',sname,Vlink);
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
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if PosEx('2gis.ru', Vlink, 1) > 0then begin
  sdesc := vlink;
  VHeader := 'Cookie: 2gisAPI=c2de06c2dd3109de8ca09a59ee197a4210495664eeae8d4075848.943590';
  Vlink := '';
  vErrCode := DoHttpRequest(sdesc, VHeader ,'',sname,Vlink);
  i := PosEx('center/', sname, 1);
  j := PosEx(',', sname, i );
  slon := Copy(sname, i + 7, j - (i + 7));
  i := j;
  j := PosEx('/', sname, i );
  slat := Copy(sname, i + 1, j - (i + 1));
  sname := '2gis';
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 VLinkErr := true;

 if (vErrCode <> 200)and(vErrCode <> 302) then VLinkErr := true;
 if (slat='') or (slon='') then VLinkErr := true;

 if VLinkErr <> true then begin
  try
    VPoint.Y := StrToFloat(slat, VFormatSettings);
    VPoint.X := StrToFloat(slon, VFormatSettings);
  except
    raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
  end;
  VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
  VList.Add(VPlace);
  Result := VList;
 end
 else
  Result := VList;
end;

function TGeoCoderByURL.PrepareURL(ASearch: WideString): string;
 var
  VlocalLink :boolean;
begin
  VlocalLink := true;
  if (PosEx('http://g.co/', ASearch, 1) > 0 )or
     (PosEx('maps.yandex.ru/-/', ASearch, 1) > 0)or
     (PosEx('yandex.ru/?oid=', ASearch, 1) > 0 )or
     (PosEx('binged.it', ASearch, 1) > 0 )or
     (PosEx('osm.org', ASearch, 1) > 0 )or
     (PosEx('permalink.html', ASearch, 1) > 0 )or
     (PosEx('api/index.html?permalink=', ASearch, 1) > 0 )
   then begin
   VlocalLink := false;
   Result := ASearch;
  end;
  if VlocalLink = true then Result := '';
end;


begin
end.

// Полные ссылки
// http://maps.google.com/?ll=48.718079,44.504639&spn=0.722115,1.234589&t=h&z=10
// http://maps.yandex.ru/?ll=44.514541%2C48.708958&spn=0.322723%2C0.181775&z=12&l=map
// http://maps.navitel.su/?zoom=6&lat=55.8&lon=37.6
// http://kosmosnimki.ru/?x=44.1053254382903&y=45.6876903573303&z=6&fullscreen=false&mode=satellite
// http://www.bing.com/maps/default.aspx?v=2&cp=45.5493750107145~41.6883332507903&style=h&lvl=6
// http://www.openstreetmap.org/?lat=45.227&lon=39.001&zoom=10&layers=M
// http://wikimapia.org#lat=45.0328&lon=38.9769&z=10&l=1&m=b

// Короткие
// http://g.co/maps/7anbg
// http://maps.yandex.ru/-/CBa6ZCOt
// http://maps.yandex.ru/-/CFVIfLi-#
// http://osm.org/go/0oqbju
// http://binged.it/vqaOQQ
// http://binged.it/sCjEwT
// http://kosmosnimki.ru/permalink.html?Na1d0e33d
// http://maps.kosmosnimki.ru/api/index.html?permalink=ZWUJK&SA5JU
// http://go.2gis.ru/1hox


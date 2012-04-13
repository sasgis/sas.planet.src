{******************************************************************************}
{* SAS.Planet (SAS.Планета)     					      *}
{* Copyright (C) 2007-2012, SAS.Planet development team.		      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.  				      *}
{*      								      *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	      *}
{* GNU General Public License for more details. 			      *}
{*      								      *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*      								      *}
{* http://sasgis.ru     						      *}
{* az@sasgis.ru 							      *}
{******************************************************************************}

unit u_GeoCoderByOSM;

interface

uses
  Classes,
  forms,
  u_GeoTostr,
  XMLIntf,
  msxmldom,
  XMLDoc,
  i_CoordConverter,
  u_GeoCoderBasic;

type
  TGeoCoderByOSM = class(TGeoCoderBasic)
  protected
    function PrepareURL(const ASearch: WideString): string; override;
    function ParseStringToPlacemarksList(const AStr: string; const ASearch: WideString): IInterfaceList; override;
  public
  end;

implementation

uses
  SysUtils,
  StrUtils,
  t_GeoTypes,
  i_GeoCoder,
  u_ResStrings,
  dialogs,
  u_GeoCodePlacemark;


{ TGeoCoderByOSM }

function TGeoCoderByOSM.ParseStringToPlacemarksList(
  const AStr: string; const ASearch: WideString): IInterfaceList;
var
  slat, slon, sname, sdesc, sfulldesc, osm_type, osm_id: string;
  i, j , k: integer;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceList;
  VFormatSettings: TFormatSettings;
begin
  sfulldesc:='';
  sdesc:='';
  if AStr = '' then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceList.Create;
  i:=PosEx('<searchresults', AStr);

  while (PosEx('<place', AStr, i) > i)and(i>0) do begin
    j := i;

    i := PosEx('osm_type=''', AStr, j);
    j := PosEx('''', AStr, i + 10 );
    osm_type := Copy(AStr, i + 10, j - (i + 10));

    i := PosEx('osm_id=''', AStr, j);
    j := PosEx('''', AStr, i + 8 );
    osm_id := Copy(AStr, i + 8, j - (i + 8));

    i := PosEx('lat=''', AStr, j);
    j := PosEx('''', AStr, i + 5 );
    slat := Copy(AStr, i + 5, j - (i + 5));

    i := PosEx('lon=''', AStr, j);
    j := PosEx('''', AStr, i + 5 );
    slon := Copy(AStr, i + 5, j - (i + 5));

    i := PosEx('display_name=''', AStr, j);
    j := PosEx('''', AStr, i + 14);
    sname:= Utf8ToAnsi(Copy(AStr, i + 14, j - (i + 14)));

    i := PosEx('class=''', AStr, j);
    if i>j then begin
      j := PosEx('''', AStr, i + 7);
      sdesc:=Utf8ToAnsi(Copy(AStr, i + 7, j - (i + 7)));
    end;

    i := PosEx('type=''', AStr, j);
    if i>j then begin
      j := PosEx('''', AStr, i + 6);
      sdesc:=sdesc+'='+Utf8ToAnsi(Copy(AStr, i + 6, j - (i + 6)));
    end;

    // финт ушам, дабы не занимать много места
    // будем разбивать "Кураж, 84, Вокзальная улица, Магнитогорск, Челябинская область, Уральский федеральный округ, 455000, Российская Федерация"
    // до первой запятой, остальное пихать в переменную sdesc
    k:=posEx(',',sname,1);
    sdesc:=sdesc+(copy(sname,k,length(sname)-k+1));
    sname:=(copy(sname,1,k-1));
    // конец финта ушами


    sfulldesc:='http://www.openstreetmap.org/browse/'+osm_type+'/'+osm_id;

//    Получение ссылки на иконку объекта, (на будущее), дабы обозначать найденные объекты...
//    k := PosEx('icon=''', AStr, i);
//    j := PosEx('><', AStr, i); // бывает что нету иконки тут проверяем на конец блока
//    if k<j then begin
//      j := PosEx('''', AStr, k + 6);
//      sfulldesc:='<img src='''+Copy(AStr, k + 6, j - (k + 6))+'''>';
//    end else sfulldesc:='';

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

function TGeoCoderByOSM.PrepareURL(const ASearch: WideString): string;
var
  VSearch: String;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
begin

  VSearch := ASearch;
  VConverter:=FLocalConverter.GetGeoConverter;
  VZoom := FLocalConverter.GetZoom;
  VMapRect := FLocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);

  //http://nominatim.openstreetmap.org/search?q=%D0%A2%D1%8E%D0%BC%D0%B5%D0%BD%D1%8C&format=xml
  Result := 'http://nominatim.openstreetmap.org/search?q='+URLEncode(AnsiToUtf8(VSearch))+'&format=xml';
end;

end.

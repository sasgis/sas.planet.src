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

unit u_GeoCoderByYandex;

interface

uses
  Classes,
  i_CoordConverter,
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
    i := PosEx('"coordinates":[', AStr, j);
    j := PosEx(',', AStr, i + 15);
    slon := Copy(AStr, i + 15, j - (i + 15));
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
  Result := 'http://maps.yandex.ru/?text='+URLEncode(AnsiToUtf8(VSearch))+
            '&sll='+R2StrPoint(FLocalConverter.GetCenterLonLat.x)+','+R2StrPoint(FLocalConverter.GetCenterLonLat.y)+
            '&sspn='+R2StrPoint(VLonLatRect.Right-VLonLatRect.Left)+','+R2StrPoint(VLonLatRect.Top-VLonLatRect.Bottom)+
            '&z='+inttostr(VZoom)+'&source=form&output=json';

end;
end.
// examples
//  http://maps.yandex.ru/?text=%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D1%8F%2C%20%D0%A2%D1%8E%D0%BC%D0%B5%D0%BD%D1%81%D0%BA%D0%B0%D1%8F%20%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D1%8C%2C%20%D0%A2%D1%8E%D0%BC%D0%B5%D0%BD%D1%8C&sll=65.558412%2C57.182627&ll=38.975277%2C45.035407&spn=0.469666%2C0.261446&z=11&l=map
//  http://maps.yandex.ru/?text=%D0%B1%D0%B0%D0%BB%D0%BE%D1%87%D0%BA%D0%B0&sll=38.975276999999984%2C45.03540700001939&sspn=0.469666%2C0.261446&z=11&source=form&output=json
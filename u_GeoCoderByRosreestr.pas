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

unit u_GeoCoderByRosreestr;

interface

uses
  Classes,
  i_CoordConverter,
  u_GeoCoderBasic;

type
  TGeoCoderByRosreestr = class(TGeoCoderBasic)
  protected
    function PrepareURL(const ASearch: WideString): string; override;
    function ParseStringToPlacemarksList(const AStr: string; const ASearch: WideString): IInterfaceList; override;
  public
  end;

implementation

uses
  SysUtils,
  StrUtils,
  RegExprUtils,
  t_GeoTypes,
  i_GeoCoder,
  u_ResStrings,
  u_GeoCodePlacemark;

{ TGeoCoderByRosreestr }
procedure meters_to_lonlat( in_x,in_y : Double; var outout : TDoublePoint);
const
 pi = 3.1415926535897932384626433832795;
begin
  outout.X := in_X/6378137*180/pi;
  outout.Y := ((arctan(exp(in_Y/6378137))-pi/4)*360)/pi;
end;

function TGeoCoderByRosreestr.ParseStringToPlacemarksList(
  const AStr: string; const ASearch: WideString): IInterfaceList;

var
  slat, slon, sname, sdesc, sfulldesc, VtempString: string;
  i, j : integer;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceList;
  VFormatSettings: TFormatSettings;
  VStr: string;
begin
  sfulldesc := '';
  sdesc := '';
  VtempString:= '';

  if AStr = '' then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  VStr := AStr;
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceList.Create;
  i:=PosEx('_jsonpCallback', VStr);
  VStr := ReplaceStr(VStr,'\"','''');
  VStr := ReplaceStr(VStr,'\/','/');
  //по кадастровому номеру
  while (PosEx('{"attributes"', VStr, i) > i)and(i>0) do begin
    j := PosEx('{"attributes"', VStr, i);
    sdesc := '';

    i := PosEx('"PARCELID":"', VStr, j);
    if i>0 then begin
     j := PosEx('"', VStr, i + 12);
     sname:= Utf8ToAnsi(Copy(VStr, i + 12, j - (i + 12)));
     sname:=copy(sname,1,2)+':'+copy(sname,3,2)+':'+copy(sname,5,7)+':'+copy(sname,12,5);
    end else begin
     i := PosEx('"KVARTALID":"', VStr, j);
     j := PosEx('"', VStr, i + 13);
     sname:= Utf8ToAnsi(Copy(VStr, i + 13, j - (i + 13)));
     sname:=copy(sname,1,2)+':'+copy(sname,3,2)+':'+copy(sname,5,7){+':'+copy(sname,12,5)};
    end;

    i := PosEx('"FULLADDRESS":"', VStr, j);
    if i>j  then begin
     j := PosEx('"', VStr, i + 15);
     sdesc:= sdesc + Utf8ToAnsi(Copy(VStr, i + 15, j - (i + 15)));
    end;

    i := PosEx('"UTILIZATION_BYDOCUMENT":"', VStr, j);
    if i>j then begin
     j := PosEx('"', VStr, i + 27);
     VtempString := Utf8ToAnsi(Copy(VStr, i + 27, j - (i + 27)));
     if VtempString <> ':null,' then sdesc := sdesc +' '+ VtempString;
    end;

    i := PosEx('"CATEGORY":"', VStr, j);
    if i>j then begin
     j := PosEx('"', VStr, i + 12);
     VtempString := Utf8ToAnsi(Copy(VStr, i + 12, j - (i + 12)));
     if VtempString <> ':null,' then sdesc := sdesc +' '+ VtempString;
    end;

    i := PosEx('"x":', VStr, j);
    j := PosEx(',', VStr, i + 4 );
    slon := Copy(VStr, i + 4, j - (i + 4));

    i := PosEx('"y":', VStr, j);
    j := PosEx('}', VStr, i + 4 );
    slat := Copy(VStr, i + 4, j - (i + 4));


    try
      meters_to_lonlat(StrToFloat(slon, VFormatSettings),StrToFloat(slat, VFormatSettings),Vpoint);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
    end;
    i := (PosEx('}}', VStr, i));
    VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
    VList.Add(VPlace);
  end;

  // по наименованию
  while (PosEx('address', VStr, i) > i)and(i>0) do begin
    j := i;

    i := PosEx('"address":"', VStr, j);
    j := PosEx('"', VStr, i + 11);
    sname:= Utf8ToAnsi(Copy(VStr, i + 11, j - (i + 11)));

    i := PosEx('"x":', VStr, j);
    j := PosEx('.', VStr, i + 4 );
    slon := Copy(VStr, i + 4, j - (i + 4));

    i := PosEx('"y":', VStr, j);
    j := PosEx('.', VStr, i + 4 );
    slat := Copy(VStr, i + 4, j - (i + 4));

    i := PosEx('"ParentName":"', VStr, j);
    j := PosEx('"', VStr, i + 14);
    sdesc:=Utf8ToAnsi(Copy(VStr, i + 14, j - (i + 14)));
    try
      meters_to_lonlat(StrToFloat(slon, VFormatSettings),StrToFloat(slat, VFormatSettings),Vpoint);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
    end;
    i := (PosEx('}}', VStr, i));
    VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
    VList.Add(VPlace);
  end;
  Result := VList;
end;

function TGeoCoderByRosreestr.PrepareURL(const ASearch: WideString): string;
var
  VSearch: String;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  i: integer;
  S1, S2, S3, S4: string;
  i1, i2, i3, i4: integer;
begin
  VSearch := ASearch;
  VConverter:=FLocalConverter.GetGeoConverter;
  VZoom := FLocalConverter.GetZoom;
  VMapRect := FLocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
  VSearch := ReplaceStr(ReplaceStr(VSearch,'*',''),':','');// убираем * и : из строки кадастрового номера

  if ''= RegExprReplaceMatchSubStr(VSearch,'[0-9]','') then begin //cadastre number
   VSearch := ASearch;
   i := PosEx(':', VSearch, 1);
   s1 := copy(VSearch,1,i-1);
   VSearch := copy(VSearch,i+1,length(VSearch)-i+1);
   i1:=strtoint(s1);

   i := PosEx(':', VSearch, 1);
   s2 := copy(VSearch,1,i-1);
   VSearch := copy(VSearch,i+1,length(VSearch)-i+1);
   i2:=strtoint(s2);

   i := PosEx(':', VSearch, 1);
   if i=0 then i:= length(VSearch)+1;
   s3 := copy(VSearch,1,i-1);
   VSearch := copy(VSearch,i+1,length(VSearch)-i+1);
   try
    i3:=strtoint(s3);
   except
    i3:=0;
   end;

   s4 := VSearch;
   try
    i4 := strtoint(s4);
   except
    i4 := 0;
   end;

   if(''= RegExprReplaceMatchSubStr(s1,'[0-9]',''))and(i1>0)then while length(s1)<2 do s1:='0'+s1;
   if(''= RegExprReplaceMatchSubStr(s2,'[0-9]',''))and(i2>0)then while length(s2)<2 do s2:='0'+s2;
   if(''= RegExprReplaceMatchSubStr(s3,'[0-9]',''))and(i3>0)then while length(s3)<7 do s3:='0'+s3;
   if(''= RegExprReplaceMatchSubStr(s4,'[0-9]',''))and(i4>0)then while length(s4)<5 do s4:='0'+s4;

   VSearch := s1+s2+s3+s4;

   if PosEx('*', VSearch, 1)>0 then
   begin
    VSearch := ReplaceStr(VSearch,'*','');// убираем * из строки кадастрового номера
    Result := 'http://maps.rosreestr.ru/ArcGIS/rest/services/Cadastre/CadastreInfo/MapServer/2/query?f=json&where=PARCELID%20like%20'''+URLEncode(AnsiToUtf8(VSearch))+'%25''&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&callback=dojo.io.script.jsonp_dojoIoScript23._jsonpCallback'
   end
   else
   if i4=0 then // Кварталы
     Result := 'http://maps.rosreestr.ru/ArcGIS/rest/services/Cadastre/CadastreInfo/MapServer/6/query?f=json&where=KVARTALID%20like%20'''+URLEncode(AnsiToUtf8(VSearch))+'%25''&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&callback=dojo.io.script.jsonp_dojoIoScript40._jsonpCallback'
    else // участки
     Result := 'http://maps.rosreestr.ru/ArcGIS/rest/services/Cadastre/CadastreInfo/MapServer/2/query?f=json&where=PARCELID%20like%20'''+URLEncode(AnsiToUtf8(VSearch))+'%25''&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&callback=dojo.io.script.jsonp_dojoIoScript37._jsonpCallback';

  end else begin //name
   VSearch := ASearch;
   Result := 'http://maps.rosreestr.ru/ArcGIS/rest/services/Address/Locator_Composite/GeocodeServer/findAddressCandidates?SingleLine='+URLEncode(AnsiToUtf8(VSearch))+'&f=json&outFields=*&callback=dojo.io.script.jsonp_dojoIoScript10._jsonpCallback';
  end;

//  http://maps.rosreestr.ru/ArcGIS/rest/services/Cadastre/CadastreInfo/MapServer/2/query?f=json&where=PARCELID%20like%20'23430116030%25'&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&callback=dojo.io.script.jsonp_dojoIoScript17._jsonpCallback
//  http://maps.rosreestr.ru/ArcGIS/rest/services/Address/Locator_Composite/GeocodeServer/findAddressCandidates?SingleLine=%D0%BD%D0%BE%D0%B2%D1%8B%D0%B9&f=json&outFields=*&callback=dojo.io.script.jsonp_dojoIoScript10._jsonpCallback
end;

end.
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

unit u_GeoCoderByWikiMapia;

interface

uses
  Classes,
  i_CoordConverter,
  u_GeoCoderBasic;

type
  TGeoCoderByWikiMapia = class(TGeoCoderBasic)
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
  u_GeoCodePlacemark;


{ TGeoCoderByOSM }

function TGeoCoderByWikiMapia.ParseStringToPlacemarksList(
  const AStr: string; const ASearch: WideString): IInterfaceList;
var
  slat, slon, sname, sdesc, sfulldesc{, vzoom}: string;
  i, j : integer;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceList;
  VFormatSettings: TFormatSettings;
begin
  sfulldesc:='';
  sdesc:='';
  if (AStr = '')then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceList.Create;
  i:=PosEx('<div style="overflow: hidden; width:100%;width:3px;height:2px;"', AStr,1);
  while (PosEx('<div class="sdiv" onclick="parent.jevals=', AStr, i) > i)and(i>0) do begin
    j := i;

//    зум для внешней ссылки оставлен для потом
//    i := PosEx('parent.zoom_from_inf(', AStr, j);
//    i := PosEx(',', AStr, i);
//    i := PosEx(',', AStr, i);
//    j := PosEx(')', AStr, i);
//    VZoom:=Copy(AStr, i + 1, j - (i + 1));

    i := PosEx('{parent.searchvis(', AStr, j);
    j := PosEx(',', AStr, i + 18 );
    slon := Copy(AStr, i + 18, j - (i + 18));

    i := j;
    j := PosEx(')', AStr, i + 1 );
    slat := Copy(AStr, i + 1, j - (i + 1));

    i := PosEx('<span class="sname">', AStr, j);
    j := PosEx('<', AStr, i + 20);
    sname:= Utf8ToAnsi(Copy(AStr, i + 20, j - (i + 20)));

    i := PosEx('<span class="desc">', AStr, j);
    j := PosEx('<', AStr, i + 19);
    sdesc:= Utf8ToAnsi(Copy(AStr, i + 19, j - (i + 19)));

//    оставим до лучших времён
//    sfulldesc:='http://www.wikimapia.org/#lat='+slat+'&lon='+slon+'&z='+VZoom;

    try
      VPoint.Y := StrToFloat(slat, VFormatSettings);
      VPoint.X := StrToFloat(slon, VFormatSettings);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
    end;

  if not((slat='0')or(slon='0')) then begin // пропускаем точки без координат
    VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
    VList.Add(VPlace);
  end;

  end;
  Result := VList;
end;

function TGeoCoderByWikiMapia.PrepareURL(const ASearch: WideString): string;
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

  //http://wikimapia.org/search/?q=%D0%9A%D1%80%D0%B0%D1%81%D0%BD%D0%BE%D0%B4%D0%B0%D1%80
  Result := 'http://wikimapia.org/search/?q='+URLEncode(AnsiToUtf8(VSearch));
end;

end.

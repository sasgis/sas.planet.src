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

unit u_GeoCoderByWikimapia;

interface

uses
  Classes,
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_DownloadRequest,
  i_DownloadResult,
  u_GeoCoderBasic;

type
  TGeoCoderByWikiMapia = class(TGeoCoderBasic)
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
  end;

implementation

uses
  SysUtils,
  StrUtils,
  ALFcnString,
  t_GeoTypes,
  i_GeoCoder,
  i_CoordConverter,
  u_InterfaceListSimple,
  u_ResStrings;


{ TGeoCoderByWikiMapia }


function TGeoCoderByWikiMapia.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  slat, slon: AnsiString;
  sname, sdesc, sfulldesc: string;
  i, j: integer;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceListSimple;
  VFormatSettings: TALFormatSettings;
  VStr: AnsiString;
begin
  sfulldesc := '';
  sdesc := '';
  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  SetLength(Vstr, AResult.Data.Size);
  Move(AResult.Data.Buffer^, Vstr[1], AResult.Data.Size);
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceListSimple.Create;
  i := ALPosEx('<ul class="nav searchlist">', VStr, 1);
  while (ALPosEx('<li onclick="parent.Search.zoomTo', VStr, i) > i) and (i > 0) do begin
    j := i;

    //    зум для внешней ссылки оставлен для потом
    //    i := PosEx('parent.zoom_from_inf(', AStr, j);
    //    i := PosEx(',', AStr, i);
    //    i := PosEx(',', AStr, i);
    //    j := PosEx(')', AStr, i);
    //    VZoom:=Copy(AStr, i + 1, j - (i + 1));

    i := ALPosEx('parent.Search.zoomTo(', VStr, j);
    j := ALPosEx(',', VStr, i + 21);
    slon := Copy(VStr, i + 21, j - (i + 21));

    i := j;
    j := ALPosEx(',', VStr, i + 1);
    slat := Copy(VStr, i + 1, j - (i + 1));

    i := ALPosEx('<strong>', VStr, j);
    j := ALPosEx('</strong>', VStr, i + 8);
    sname := Utf8ToAnsi(Copy(VStr, i + 8, j - (i + 8)));

    i := ALPosEx('<small>', VStr, j);
    j := ALPosEx('<', VStr, i + 7);
    sdesc := Utf8ToAnsi(Copy(VStr, i + 7, j - (i + 7)));
    sdesc := Trim(ReplaceStr(sdesc,#$0A,''));

    //    оставим до лучших времён
    //    sfulldesc:='http://www.wikimapia.org/#lat='+slat+'&lon='+slon+'&z='+VZoom;

    try
      VPoint.Y := ALStrToFloat(slat, VFormatSettings);
      VPoint.X := ALStrToFloat(slon, VFormatSettings);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
    end;

    if not ((slat = '0') or (slon = '0')) then begin // пропускаем точки без координат
      VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
      VList.Add(VPlace);
    end;

  end;
  if VList.GetCount>1 then SortIt(VList, ALocalConverter);
  Result := VList;
end;

function TGeoCoderByWikiMapia.PrepareRequest(
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
var
  VSearch: String;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
begin

  VSearch := ASearch;
  VConverter := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapRect := ALocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);

  //http://wikimapia.org/search/?q=%D0%9A%D1%80%D0%B0%D1%81%D0%BD%D0%BE%D0%B4%D0%B0%D1%80
  Result :=
    PrepareRequestByURL(
      'http://wikimapia.org/search/?q=' + URLEncode(AnsiToUtf8(VSearch))
    );
end;

end.

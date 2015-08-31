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

unit u_GeoCoderByOSM;

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
  TGeoCoderByOSM = class(TGeoCoderBasic)
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
  ALString,
  t_GeoTypes,
  i_GeoCoder,
  i_VectorDataItemSimple,
  i_ProjectionInfo,
  u_InterfaceListSimple,
  u_ResStrings;


{ TGeoCoderByOSM }

function TGeoCoderByOSM.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  slat, slon: AnsiString;
  sname, sdesc, sfulldesc: string;
  osm_type, osm_id: AnsiString;
  i, j, k: integer;
  VPoint: TDoublePoint;
  VPlace: IVectorDataItem;
  VList: IInterfaceListSimple;
  VFormatSettings: TALFormatSettings;
  VStr: AnsiString;
begin
  sfulldesc := '';
  sdesc := '';
  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceListSimple.Create;
  SetLength(Vstr, AResult.Data.Size);
  Move(AResult.Data.Buffer^, Vstr[1], AResult.Data.Size);
  i := ALPosEx('<searchresults', VStr);

  while (ALPosEx('<place', VStr, i) > i) and (i > 0) do begin
    j := i;

    i := ALPosEx('osm_type=''', VStr, j);
    j := ALPosEx('''', VStr, i + 10);
    osm_type := Copy(VStr, i + 10, j - (i + 10));

    i := ALPosEx('osm_id=''', VStr, j);
    j := ALPosEx('''', VStr, i + 8);
    osm_id := Copy(VStr, i + 8, j - (i + 8));

    i := ALPosEx('lat=''', VStr, j);
    j := ALPosEx('''', VStr, i + 5);
    slat := Copy(VStr, i + 5, j - (i + 5));

    i := ALPosEx('lon=''', VStr, j);
    j := ALPosEx('''', VStr, i + 5);
    slon := Copy(VStr, i + 5, j - (i + 5));

    i := ALPosEx('display_name=''', VStr, j);
    j := ALPosEx('''', VStr, i + 14);
    sname := Utf8ToAnsi(Copy(VStr, i + 14, j - (i + 14)));

    i := ALPosEx('class=''', VStr, j);
    if i > j then begin
      j := ALPosEx('''', VStr, i + 7);
      sdesc := Utf8ToAnsi(Copy(VStr, i + 7, j - (i + 7)));
    end;

    i := ALPosEx('type=''', VStr, j);
    if i > j then begin
      j := ALPosEx('''', VStr, i + 6);
      sdesc := sdesc + '=' + Utf8ToAnsi(Copy(VStr, i + 6, j - (i + 6)));
    end;

    // финт ушам, дабы не занимать много места
    // будем разбивать "Кураж, 84, Вокзальная улица, Магнитогорск, Челябинская область, Уральский федеральный округ, 455000, Российская Федерация"
    // до первой запятой, остальное пихать в переменную sdesc
    k := PosEx(',', sname, 1);
    sdesc := sdesc + (copy(sname, k, length(sname) - k + 1));
    sname := (copy(sname, 1, k - 1));
    // конец финта ушами


    sfulldesc := 'http://www.openstreetmap.org/browse/' + string(osm_type) + '/' + string(osm_id);

    //    Получение ссылки на иконку объекта, (на будущее), дабы обозначать найденные объекты...
    //    k := PosEx('icon=''', AStr, i);
    //    j := PosEx('><', AStr, i); // бывает что нету иконки тут проверяем на конец блока
    //    if k<j then begin
    //      j := PosEx('''', AStr, k + 6);
    //      sfulldesc:='<img src='''+Copy(AStr, k + 6, j - (k + 6))+'''>';
    //    end else sfulldesc:='';

    try
      VPoint.Y := ALStrToFloat(slat, VFormatSettings);
      VPoint.X := ALStrToFloat(slon, VFormatSettings);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
    end;
    VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
    VList.Add(VPlace);
  end;
  Result := VList;
end;

function TGeoCoderByOSM.PrepareRequest(
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
var
  VSearch: String;
  VProjection: IProjectionInfo;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
begin
  VSearch := ASearch;
  VProjection := ALocalConverter.ProjectionInfo;
  VMapRect := ALocalConverter.GetRectInMapPixelFloat;
  VProjection.ValidatePixelRectFloat(VMapRect);
  VLonLatRect := VProjection.PixelRectFloat2LonLatRect(VMapRect);

  //http://nominatim.openstreetmap.org/search?q=%D0%A2%D1%8E%D0%BC%D0%B5%D0%BD%D1%8C&format=xml
  Result :=
    PrepareRequestByURL(
      'http://nominatim.openstreetmap.org/search?q=' + URLEncode(AnsiToUtf8(VSearch)) + '&format=xml'
    );
end;

end.

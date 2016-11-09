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

unit u_GeoCoderByIp2geolocation;

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
  TGeoCoderByIp2geolocation = class(TGeoCoderBasic)
  protected
    function PrepareRequest(
      const ASearch: string;
      const ALocalConverter: ILocalCoordConverter
    ): IDownloadRequest; override;
    function ParseResultToPlacemarksList(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AResult: IDownloadResultOk;
      const ASearch: string;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
  end;

implementation

uses
  SysUtils,
  StrUtils,
  ALString,
  RegExpr,
  t_GeoTypes,
  i_GeoCoder,
  i_VectorDataItemSimple,
  i_Projection,
  u_InterfaceListSimple,
  u_ResStrings;

{ TGeoCoderByIp2geolocation }

function TGeoCoderByIp2geolocation.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  slat, slon: AnsiString;
  sname, sdesc, sfulldesc: string;
  VPoint: TDoublePoint;
  VPlace: IVectorDataItem;
  VList: IInterfaceListSimple;
  VFormatSettings: TALFormatSettings;
  VStr: AnsiString;
  VRegExpr: TRegExpr;
begin
  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceListSimple.Create;
  SetLength(Vstr, AResult.Data.Size);
  Move(AResult.Data.Buffer^, Vstr[1], AResult.Data.Size);
  sname := '';
  sdesc := '';
  sfulldesc := '';

  VRegExpr := TRegExpr.Create;
  //IP Адрес в sname
  try
    VRegExpr.Expression := '<strong>IP</strong>.+?([0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3})';
    if VRegExpr.Exec(VStr) then begin
      sname := 'IP: ' + VRegExpr.Match[1];
    end;

    if not (sname = 'IP: 127.0.0.1') then begin
      //Страна в sdesc
      VRegExpr.Expression := '<strong>Country Name</strong>.+?>([^<]+)</a></td></tr>';
      if VRegExpr.Exec(VStr) then begin
        sdesc := 'Country Name: ' + VRegExpr.Match[1];
        sfulldesc := sdesc;
      end;
      //Регион в sdesc
      VRegExpr.Expression := '<strong>Region Name</strong>.+?>([^<]+)</td></tr>';
      if VRegExpr.Exec(VStr) then begin
        sdesc := sdesc + #$D#$A + 'Region Name: ' + VRegExpr.Match[1];
        sfulldesc := sfulldesc + ', Region Name: ' + VRegExpr.Match[1];
      end;
      //Все остальное в sfulldesc
      //Город
      VRegExpr.Expression := '<strong>City</strong>.+?>([^<]+)</td></tr>';
      if VRegExpr.Exec(VStr) then begin
        sfulldesc := sfulldesc + ', City: ' + VRegExpr.Match[1];
      end;
      //Почтовый код
      VRegExpr.Expression := '<strong>Postal Code</strong>.+?>([^<]+)</td></tr>';
      if VRegExpr.Exec(VStr) then begin
        sfulldesc := sfulldesc + ', Postal Code: ' + VRegExpr.Match[1];
      end;
      //Провайдер
      VRegExpr.Expression := '<strong>ISP</strong>.+?>([^<]+)</td></tr>';
      if VRegExpr.Exec(VStr) then begin
        sfulldesc := sfulldesc + ', ISP: ' + VRegExpr.Match[1];
      end;
      //Часовой пояс
      VRegExpr.Expression := '<strong>Time Zone</strong>.+?>([^<]+)</td></tr>';
      if VRegExpr.Exec(VStr) then begin
        sfulldesc := sfulldesc + ', Time Zone: ' + VRegExpr.Match[1];
      end;
      //Широта и долгота
      VRegExpr.Expression := '<strong>Latitude</strong>.+?([-\d\.]+)';
      if VRegExpr.Exec(VStr) then begin
        slat := VRegExpr.Match[1];
        sfulldesc := sfulldesc + ', Latitude: ' + slat;
      end;
      VRegExpr.Expression := '<strong>Longitude</strong>.+?([-\d\.]+)';
      if VRegExpr.Exec(VStr) then begin
        slon := VRegExpr.Match[1];
        sfulldesc := sfulldesc + ', Longitude: ' + slon;
      end;

      try
        VPoint.Y := ALStrToFloat(slat, VFormatSettings);
        VPoint.X := ALStrToFloat(slon, VFormatSettings);
      except
        raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
      end;
      VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
      VList.Add(VPlace);
      Result := VList;
    end;
  finally
    VRegExpr.Free;
  end;
end;

function TGeoCoderByIp2geolocation.PrepareRequest(
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
begin
  // http://ip2geolocation.com/?ip=37.78.14.148&lang=en
  Result :=
    PrepareRequestByURL(
      'http://ip2geolocation.com/?ip=' + URLEncode(AnsiToUtf8(ASearch)) + '&lang=en'
    );
end;

end.

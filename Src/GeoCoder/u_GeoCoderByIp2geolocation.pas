{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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
  end;

implementation

uses
  ALString,
  RegExpr,
  t_GeoTypes,
  i_VectorDataItemSimple,
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
const
  cLocalHost_v4 = '127.0.0.1';
var
  VLat, VLon: AnsiString;
  VName, VDesc, VFullDesc: string;
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

  SetLength(VStr, AResult.Data.Size);
  Move(AResult.Data.Buffer^, VStr[1], AResult.Data.Size);

  VName := '';
  VDesc := '';
  VFullDesc := '';

  VRegExpr := TRegExpr.Create;
  try
    VRegExpr.Expression := '<strong>IP</strong>.+?([0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3})';
    if VRegExpr.Exec(VStr) then begin
      if VRegExpr.Match[1] <> cLocalHost_v4 then begin
        VName := 'IP: ' + VRegExpr.Match[1];
      end else begin
        Exit;
      end;
    end;

    VRegExpr.Expression := '<strong>Country Name</strong>.+?>([^<]+)</a></td></tr>';
    if VRegExpr.Exec(VStr) then begin
      VDesc := 'Country Name: ' + VRegExpr.Match[1];
      VFullDesc := VDesc;
    end;

    VRegExpr.Expression := '<strong>Region Name</strong>.+?>([^<]+)</td></tr>';
    if VRegExpr.Exec(VStr) then begin
      VDesc := VDesc + #$D#$A + 'Region Name: ' + VRegExpr.Match[1];
      VFullDesc := VFullDesc + ', Region Name: ' + VRegExpr.Match[1];
    end;

    VRegExpr.Expression := '<strong>City</strong>.+?>([^<]+)</td></tr>';
    if VRegExpr.Exec(VStr) then begin
      VFullDesc := VFullDesc + ', City: ' + VRegExpr.Match[1];
    end;

    VRegExpr.Expression := '<strong>Postal Code</strong>.+?>([^<]+)</td></tr>';
    if VRegExpr.Exec(VStr) then begin
      VFullDesc := VFullDesc + ', Postal Code: ' + VRegExpr.Match[1];
    end;

    VRegExpr.Expression := '<strong>ISP</strong>.+?>([^<]+)</td></tr>';
    if VRegExpr.Exec(VStr) then begin
      VFullDesc := VFullDesc + ', ISP: ' + VRegExpr.Match[1];
    end;

    VRegExpr.Expression := '<strong>Time Zone</strong>.+?>([^<]+)</td></tr>';
    if VRegExpr.Exec(VStr) then begin
      VFullDesc := VFullDesc + ', Time Zone: ' + VRegExpr.Match[1];
    end;

    VRegExpr.Expression := '<strong>Latitude</strong>.+?([-\d\.]+)';
    if VRegExpr.Exec(VStr) then begin
      VLat := VRegExpr.Match[1];
      VFullDesc := VFullDesc + ', Latitude: ' + VLat;
    end;

    VRegExpr.Expression := '<strong>Longitude</strong>.+?([-\d\.]+)';
    if VRegExpr.Exec(VStr) then begin
      VLon := VRegExpr.Match[1];
      VFullDesc := VFullDesc + ', Longitude: ' + VLon;
    end;

    try
      VFormatSettings.DecimalSeparator := '.';
      VPoint.X := ALStrToFloat(VLon, VFormatSettings);
      VPoint.Y := ALStrToFloat(VLat, VFormatSettings);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [VLat, VLon]);
    end;

    VPlace := PlacemarkFactory.Build(VPoint, VName, VDesc, VFullDesc, 4);

    VList := TInterfaceListSimple.Create;
    VList.Add(VPlace);

    Result := VList;
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

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

unit u_GeoCoderByGoogle;

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
  TGeoCoderByGoogle = class(TGeoCoderBasic)
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
  superobject,
  t_GeoTypes,
  i_GeoCoder,
  i_VectorDataItemSimple,
  i_CoordConverter,
  u_InterfaceListSimple,
  u_ResStrings,
  u_GeoToStrFunc;

{ TGeoCoderByGoogle }

function TGeoCoderByGoogle.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  I: Integer;
  VTmpBuf: UTF8String;
  VJsonObject: ISuperObject;
  VJsonArray: TSuperArray;
  VResultItem: ISuperObject;
  VPoint: TDoublePoint;
  VPlace: IVectorDataItemSimple;
  VList: IInterfaceListSimple;
  VFormatSettings: TFormatSettings;
  VStatus, VName, VLon, VLat: string;
begin
  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  VFormatSettings.DecimalSeparator := '.';

  VList := TInterfaceListSimple.Create;

  SetLength(VTmpBuf, AResult.Data.Size);
  Move(AResult.Data.Buffer^, VTmpBuf[1], AResult.Data.Size);

  VJsonObject := SO(VTmpBuf);

  if not Assigned(VJsonObject) then begin
    raise EParserError.Create('JSON parser error');
  end;

  VStatus := Utf8ToAnsi(VJsonObject.S['status']);

  (*
    https://developers.google.com/maps/documentation/geocoding/index#StatusCodes

    "OK" indicates that no errors occurred; the address was successfully parsed
    and at least one geocode was returned.

    "ZERO_RESULTS" indicates that the geocode was successful but returned no
    results. This may occur if the geocode was passed a non-existent address or
    a latlng in a remote location.

    "OVER_QUERY_LIMIT" indicates that you are over your quota.

    "REQUEST_DENIED" indicates that your request was denied, generally because
    of lack of a sensor parameter.

    "INVALID_REQUEST" generally indicates that the query (address or latlng)
    is missing.

    UNKNOWN_ERROR indicates that the request could not be processed due to a
    server error. The request may succeed if you try again.
  *)

  if VStatus <> 'OK' then begin
    if VStatus = 'ZERO_RESULTS' then begin
      Exit;
    end else begin
      raise Exception.CreateFmt('Unexpected status value: %s', [VStatus]);
    end;
  end;

  VJsonArray := VJsonObject.A['results'];
  Assert(VJsonArray <> nil);

  for I := 0 to VJsonArray.Length - 1 do begin
    VResultItem := VJsonArray.O[I];
    Assert(VResultItem <> nil);
    VName := Utf8ToAnsi(VResultItem.S['formatted_address']);
    VLat := Utf8ToAnsi(VResultItem.S['geometry.location.lat']);
    VLon := Utf8ToAnsi(VResultItem.S['geometry.location.lng']);
    try
      VPoint.X := StrToFloat(VLon, VFormatSettings);
      VPoint.Y := StrToFloat(VLat, VFormatSettings);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [VLon, VLat]);
    end;
    VPlace := PlacemarkFactory.Build(VPoint, VName, '', '', 4);
    VList.Add(VPlace);
  end;

  Result := VList;
end;

function TGeoCoderByGoogle.PrepareRequest(
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
var
  VSearch: string;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
begin
  VSearch := StringReplace(ASearch, ' ', '+', [rfReplaceAll]);
  VConverter := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapRect := ALocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);

  // https://developers.google.com/maps/documentation/geocoding/index
  Result :=
    PrepareRequestByURL(
      'http://maps.googleapis.com/maps/api/geocode/json?address=' +
      URLEncode(AnsiToUtf8(VSearch)) +
      '&sensor=false' +
      '&language=' + StringReplace(SAS_STR_GoogleSearchLanguage, '&hl=', '', [rfIgnoreCase]) +
      '&bounds=' +
        R2AnsiStrPoint(VLonLatRect.Bottom) + ',' + R2AnsiStrPoint(VLonLatRect.Left) + '|' +
        R2AnsiStrPoint(VLonLatRect.Top) + ',' + R2AnsiStrPoint(VLonLatRect.Right)
    );
end;

end.

{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_GeoCoderByRosreestr;

interface

uses
  Windows,
  Classes,
  i_InterfaceListSimple,
  i_InetConfig,
  i_DownloadResult,
  i_DownloadRequest,
  i_NotifierTime,
  i_NotifierOperation,
  i_GeoCoder,
  i_LocalCoordConverter,
  i_VectorItemSubsetBuilder,
  i_DownloaderFactory,
  i_CoordToStringConverter,
  u_GeoCoderBasic;

type
  TGeoCoderByRosreestr = class(TGeoCoderBasic)
  private
    FCoordToStringConverter: ICoordToStringConverterChangeable;
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
    constructor Create(
      const AInetSettings: IInetConfig;
      const AGCNotifier: INotifierTime;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const ADownloaderFactory: IDownloaderFactory;
      const ACoordToStringConverter: ICoordToStringConverterChangeable
    );
  end;

implementation

uses
  Math,
  SysUtils,
  StrUtils,
  DateUtils,
  RegExpr,
  superobject,
  t_GeoTypes,
  i_VectorDataItemSimple,
  u_AnsiStr,
  u_GeoFunc,
  u_InterfaceListSimple,
  u_DownloadRequest,
  u_ResStrings;

{ TGeoCoderByRosreestr }

function MetersToLonLatPoint(const APoint: TDoublePoint): TDoublePoint; inline;
begin
  // mercator on sphere (epsg: 3857)
  Result.X := APoint.X / 6378137 * 180 / Pi;
  Result.Y := ((ArcTan(Exp(APoint.Y / 6378137)) - Pi / 4) * 360) / Pi;
end;

function GetCenterPos(const AGeoJsonGeometry: ISuperObject): TDoublePoint;

  function _ReadPoint(const AJsonArrar: TSuperArray): TDoublePoint;
  begin
    if AJsonArrar.Length >= 2 then begin
      Result.X := AJsonArrar.D[0];
      Result.Y := AJsonArrar.D[1];
    end else begin
      Assert(False);
      Result := CEmptyDoublePoint;
    end;
  end;

var
  VType: string;
  VCoordinates: TSuperArray;
  VLine: TSuperArray;
begin
  Result := CEmptyDoublePoint;

  VType := LowerCase(AGeoJsonGeometry.S['type']);
  VCoordinates := AGeoJsonGeometry.A['coordinates'];

  if VType = 'point' then begin
    Result := _ReadPoint(VCoordinates);
  end else
  if VType = 'linestring' then begin
    VLine := VCoordinates.O[0].AsArray;
    Result := _ReadPoint(VLine);
  end else
  if VType = 'polygon' then begin
    VLine := VCoordinates.O[0].AsArray;
    Result := _ReadPoint(VLine.O[0].AsArray);
  end;

  // todo: calc geometry center point for line and polygon
  // todo: add support for multi-point, multi-line, mult-polygon geometries
end;

constructor TGeoCoderByRosreestr.Create(
  const AInetSettings: IInetConfig;
  const AGCNotifier: INotifierTime;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const ADownloaderFactory: IDownloaderFactory;
  const ACoordToStringConverter: ICoordToStringConverterChangeable
);
begin
  inherited Create(
    AInetSettings,
    AGCNotifier,
    AVectorItemSubsetBuilderFactory,
    APlacemarkFactory,
    ADownloaderFactory
  );
  FCoordToStringConverter := ACoordToStringConverter;
end;

function TGeoCoderByRosreestr.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  I: Integer;
  VPoint: TDoublePoint;
  VPlace: IVectorDataItem;
  VList: IInterfaceListSimple;
  VCoordToStringConverter: ICoordToStringConverter;
  VJsonObject: ISuperObject;
  VJsonArray: TSuperArray;
  VItem: ISuperObject;
  VOptions: ISuperObject;
  VGeometry: ISuperObject;
  VTmpBuf: UTF8String;
  VCRS: string;
  VName, VFullDesc, VDescription: string;
begin
  if AResult = nil then begin
    Result := nil;
    Exit;
  end;

  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  SetLength(VTmpBuf, AResult.Data.Size);
  Move(AResult.Data.Buffer^, VTmpBuf[1], AResult.Data.Size);

  VJsonObject := SO(Utf8ToAnsi(VTmpBuf));
  if not Assigned(VJsonObject) then begin
    raise EParserError.Create('JSON parser error');
  end;

  VJsonArray := VJsonObject.A['data.features'];
  if VJsonArray = nil then begin
    raise EParserError.Create('"features" array is not found');
  end;

  VName := ASearch;
  VList := TInterfaceListSimple.Create;
  VCoordToStringConverter := FCoordToStringConverter.GetStatic;

  for I := 0 to VJsonArray.Length - 1 do begin
    VItem := VJsonArray.O[I];
    Assert(VItem <> nil);

    VFullDesc := '';
    VDescription := '';

    VOptions := VItem.O['properties.options'];
    if Assigned(VOptions) then begin
      VDescription := VOptions.AsJSon(True, False);

      VDescription := StringReplace(VDescription, '{', '', [rfReplaceAll]);
      VDescription := StringReplace(VDescription, '}', '', [rfReplaceAll]);
      VDescription := StringReplace(VDescription, '"', '', [rfReplaceAll]);
      VDescription := StringReplace(VDescription, ',' + #13#10, #13#10, [rfReplaceAll]);
    end;

    VGeometry := VItem.O['geometry'];
    if Assigned(VGeometry) then begin
      VPoint := GetCenterPos(VGeometry);

      if PointIsEmpty(VPoint) then begin
        Assert(False, 'Failed to parse geometry!');
        Continue;
      end;

      VCRS := Trim(VItem.S['crs.properties.name']);
      if LowerCase(VCRS) <> 'epsg:3857' then begin
        Assert(False, 'Unsupported CRS: "' + VCRS + '"');
      end;

      VPoint := MetersToLonLatPoint(VPoint);

      VDescription := VDescription + #$D#$A + '[ ' + VCoordToStringConverter.LonLatConvert(VPoint) + ' ]';
      VFullDesc := ReplaceStr(VName + #$D#$A + VDescription, #$D#$A, '<br>');

      VPlace := PlacemarkFactory.Build(VPoint, VName, VDescription, VFullDesc, 4);
      VList.Add(VPlace);
    end;
  end;

  Result := VList;
end;

function TGeoCoderByRosreestr.PrepareRequest(
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
const
  cUrlBase =
    'https://nspd.gov.ru/api/geoportal/v2/search/geoportal?thematicSearchId=1&query=';
  cRequestHeader =
    'Accept: */*' + #13#10 +
    'Accept-Language: ru,en;q=0.9' + #13#10 +
    'Accept-Encoding: gzip, deflate' + #13#10 +
    'Referer: https://nspd.gov.ru/';
var
  VQuery: AnsiString;
  VSearch: AnsiString;
  VRegExpr: TRegExpr;
begin
  VRegExpr  := TRegExpr.Create;
  try
    VSearch := AnsiString(ASearch);
    VRegExpr.Expression := '^([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,7})(:([0-9]{1,5}))?$';

    if VRegExpr.Exec(VSearch) then begin
      VSearch :=
        IntToStrA(StrToIntA(VRegExpr.Match[1])) + ':' +
        IntToStrA(StrToIntA(VRegExpr.Match[2])) + ':' +
        IntToStrA(StrToIntA(VRegExpr.Match[3]));

      if VRegExpr.Match[4] <> '' then begin
        VSearch := VSearch + ':' + IntToStrA(StrToIntA(VRegExpr.Match[5]));
      end;

      VQuery := URLEncode(VSearch);

      Result :=
        TDownloadRequest.Create(
          cUrlBase + VQuery,
          cRequestHeader,
          InetSettings.GetStatic
        );
    end else begin
      Result := nil;
      Assert(False, 'Unexpected input format: ' + ASearch);
    end;
  finally
    VRegExpr.Free;
  end;
end;

end.


{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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
  SysUtils,
  StrUtils,
  DateUtils,
  ALString,
  RegExpr,
  superobject,
  t_GeoTypes,
  i_VectorDataItemSimple,
  u_InterfaceListSimple,
  u_DownloadRequest,
  u_ResStrings;

{ TGeoCoderByRosreestr }

function UtcNow: TDateTime;
var
  VSystemTime: TSystemTime;
begin
  GetSystemTime(VSystemTime);
  Result := SystemTimeToDateTime(VSystemTime);
end;

procedure MetersToLonLat(const AX, AY: Double; out ALonLat: TDoublePoint);
begin
  ALonLat.X := AX / 6378137 * 180 / Pi;
  ALonLat.Y := ((arctan(exp(AY / 6378137)) - Pi / 4) * 360) / Pi;
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
  X, Y: Double;
  VStr: string;
  VPoint: TDoublePoint;
  VPlace: IVectorDataItem;
  VList: IInterfaceListSimple;
  VCoordToStringConverter: ICoordToStringConverter;
  VJsonObject: ISuperObject;
  VJsonArray: TSuperArray;
  VItem: ISuperObject;
  VTmpBuf: UTF8String;
  VName, VFullDesc, VDescription: string;
begin
  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  SetLength(VTmpBuf, AResult.Data.Size);
  Move(AResult.Data.Buffer^, VTmpBuf[1], AResult.Data.Size);

  VJsonObject := SO(Utf8ToAnsi(VTmpBuf));
  if not Assigned(VJsonObject) then begin
    raise EParserError.Create('JSON parser error');
  end;

  VStr := VJsonObject.S['status'];
  if VStr <> '200' then begin
    raise Exception.CreateFmt('Unexpected "status" value: %s', [VStr]);
  end;

  VJsonArray := VJsonObject.A['features'];
  Assert(VJsonArray <> nil);

  VName := ASearch;
  VList := TInterfaceListSimple.Create;
  VCoordToStringConverter := FCoordToStringConverter.GetStatic;

  for I := 0 to VJsonArray.Length - 1 do begin
    VItem := VJsonArray.O[I];
    Assert(VItem <> nil);

    VFullDesc := '';
    VDescription := '';

    if Assigned(VItem.O['attrs']) then begin
      VStr := VItem.S['attrs.address'];
      if VStr <> '' then VDescription := 'address: ' + VStr;
      VStr := VItem.S['attrs.cn'];
      if VStr <> '' then VDescription := VDescription + #$D#$A + 'cn: ' + VStr;
      VStr := VItem.S['attrs.adate'];
      if VStr <> '' then VDescription := VDescription + #$D#$A + 'adate: ' + VStr;
      VStr := VItem.S['attrs.pubdate'];
      if VStr <> '' then VDescription := VDescription + #$D#$A + 'pubdate: ' + VStr;
      VStr := VItem.S['attrs.cad_record_date'];
      if VStr <> '' then VDescription := VDescription + #$D#$A + 'cad_record_date: ' + VStr;
      VStr := VItem.S['attrs.util_by_doc'];
      if VStr <> '' then VDescription := VDescription + #$D#$A + 'util_by_doc: ' + VStr;
      VStr := VItem.S['attrs.cad_cost'];
      if VStr <> '' then VDescription := VDescription + #$D#$A + 'cad_cost: ' + VStr;
      VStr := VItem.S['attrs.area_value'];
      if VStr <> '' then VDescription := VDescription + #$D#$A + 'area_value: ' + VStr;

      VDescription := VDescription + #$D#$A + '[ ' + VCoordToStringConverter.LonLatConvert(VPoint) + ' ]';
      VFullDesc := ReplaceStr(VName + #$D#$A + VDescription, #$D#$A, '<br>');
    end;

    if Assigned(VItem.O['center']) then begin
      X := VItem.D['center.x'];
      Y := VItem.D['center.y'];

      try
        MetersToLonLat(X, Y, VPoint);
      except
        raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [FloatToStr(X), FloatToStr(Y)]);
      end;

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
  cURLFmt = 'https://pkk5.rosreestr.ru/api/features/%d?text=%s&tolerance=262259&limit=11&_=%d000';
  cRequestHeader =
    'Accept: text/javascript, application/javascript, application/ecmascript, application/x-ecmascript, */*; q=0.01' + #13#10 +
    'Accept-Language: en-US' + #13#10 +
    'Accept-Encoding: gzip, deflate' + #13#10 +
    'Referer: https://pkk5.rosreestr.ru/' + #13#10 +
    'X-Requested-With: XMLHttpRequest';
var
  I: Integer;
  VRequestStr: UTF8String;
  VRegExpr: TRegExpr;
  VSearch: AnsiString;
begin
  VRegExpr  := TRegExpr.Create;
  try
    VSearch := AnsiString(ASearch);
    VRegExpr.Expression := '^([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,7})(:([0-9]{1,5}))?$';

    if VRegExpr.Exec(VSearch) then begin
      VSearch :=
        ALIntToStr(ALStrToInt(VRegExpr.Match[1])) + ':' +
        ALIntToStr(ALStrToInt(VRegExpr.Match[2])) + ':' +
        ALIntToStr(ALStrToInt(VRegExpr.Match[3]));
      I := 2;
      if VRegExpr.Match[4] <> '' then begin
        VSearch := VSearch + ':' + ALIntToStr(ALStrToInt(VRegExpr.Match[5]));
        I := 1;
      end;
      VRequestStr := URLEncode(VSearch);
      Result :=
        TDownloadRequest.Create(
          ALFormat(cURLFmt, [I, VRequestStr, DateTimeToUnix(UtcNow)]),
          cRequestHeader,
          InetSettings.GetStatic
        );
    end else begin
      Result := nil;
    end;
  finally
    VRegExpr.Free;
  end;
end;

end.


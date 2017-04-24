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

unit u_GeoCoderByRosreestr;

interface

uses
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
  i_DownloadResultFactory,
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
      const AResultFactory: IDownloadResultFactory;
      const ACoordToStringConverter: ICoordToStringConverterChangeable
    );

  end;

implementation

uses
  SysUtils,
  StrUtils,
  ALString,
  RegExpr,
  superobject,
  t_GeoTypes,
  i_VectorDataItemSimple,
  u_InterfaceListSimple,
  u_ResStrings;

{ TGeoCoderByRosreestr }

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
  const AResultFactory: IDownloadResultFactory;
  const ACoordToStringConverter: ICoordToStringConverterChangeable
);
begin
  inherited Create(
    AInetSettings,
    AGCNotifier,
    AVectorItemSubsetBuilderFactory,
    APlacemarkFactory,
    AResultFactory
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
  X, Y: Double;
  VTempString: string;
  VPoint: TDoublePoint;
  VPlace: IVectorDataItem;
  VList: IInterfaceListSimple;
  VCoordToStringConverter: ICoordToStringConverter;
  VJsonObject: ISuperObject;
  VTmpBuf: UTF8String;
  VName, VFullDesc, VDescription: string;
begin
  VCoordToStringConverter := FCoordToStringConverter.GetStatic;
  VTempString := '';
  VList := TInterfaceListSimple.Create;
  SetLength(VTmpBuf, AResult.Data.Size);
  Move(AResult.Data.Buffer^, VTmpBuf[1], AResult.Data.Size);
  VJsonObject := SO(Utf8ToAnsi(VTmpBuf));

  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  if not Assigned(VJsonObject) then begin
    raise EParserError.Create('JSON parser error');
  end;

  if Assigned(VJsonObject) and Assigned(VJsonObject.O['feature.attrs']) then
  begin
    VName := ASearch;
    VTempString := VJsonObject.S['feature.attrs.address'];
    if VTempString <> '' then VDescription := 'address: ' + VTempString;
    VTempString := VJsonObject.S['feature.attrs.cn'];
    if VTempString <> '' then VDescription := VDescription + #$D#$A + 'cn: ' + VTempString;
    VTempString := VJsonObject.S['feature.attrs.adate'];
    if VTempString <> '' then VDescription := VDescription + #$D#$A + 'adate: ' + VTempString;
    VTempString := VJsonObject.S['feature.attrs.pubdate'];
    if VTempString <> '' then VDescription := VDescription + #$D#$A + 'pubdate: ' + VTempString;
    VTempString := VJsonObject.S['feature.attrs.cad_record_date'];
    if VTempString <> '' then VDescription := VDescription + #$D#$A + 'cad_record_date: ' + VTempString;
    VTempString := VJsonObject.S['feature.attrs.util_by_doc'];
    if VTempString <> '' then VDescription := VDescription + #$D#$A + 'util_by_doc: ' + VTempString;
    VTempString := VJsonObject.S['feature.attrs.cad_cost'];
    if VTempString <> '' then VDescription := VDescription + #$D#$A + 'cad_cost: ' + VTempString;
    VTempString := VJsonObject.S['feature.attrs.area_value'];
    if VTempString <> '' then VDescription := VDescription + #$D#$A + 'area_value: ' + VTempString;

    X := VJsonObject.D['feature.center.x'];
    Y := VJsonObject.D['feature.center.y'];

    try
      MetersToLonLat(X, Y, Vpoint);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [FloatToStr(X), FloatToStr(Y)]);
    end;

    VDescription := VDescription + #$D#$A + '[ ' + VCoordToStringConverter.LonLatConvert(VPoint) + ' ]';
    VFullDesc := ReplaceStr(VName + #$D#$A + VDescription, #$D#$A, '<br>');
    VPlace := PlacemarkFactory.Build(VPoint, VName, VDescription, VFullDesc, 4);
    VList.Add(VPlace);
  end;
  Result := VList;
end;

function TGeoCoderByRosreestr.PrepareRequest(
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
const
  cURLFmt = 'http://pkk5.rosreestr.ru/api/features/%d/%s?f=pjson';
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
      Result := PrepareRequestByURL(ALFormat(cURLFmt, [I, VRequestStr]));
    end else begin
      Result := nil;
    end;
  finally
    FreeAndNil(VRegExpr);
  end;
end;

end.

// "http://pkk5.rosreestr.ru/api/typeahead?text="%"D0"%"BD"%"D0"%"BE"%"D0"%"B2"%"D0"%"BE&limit=10&skip=0&type=-1"
// "http://pkk5.rosreestr.ru/api/typeahead?text=vjcrdf&limit=10&skip=0&type=-1"
// http://pkk5.rosreestr.ru/api/typeahead?text=%D0%BA%D1%80%D0%B0%D1%81%D0%BD%D0%BE%D0%B4%D0%B0&limit=10&skip=0&type=-1

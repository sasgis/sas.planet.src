{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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
  Classes,
  i_InterfaceListSimple,
  i_InetConfig,
  i_DownloadResult,
  i_DownloadRequest,
  i_NotifierTime,
  i_NotifierOperation,
  i_GeoCoder,
  i_LocalCoordConverter,
  i_ProjConverter,
  i_VectorDataLoader,
  i_GeometryLonLatFactory,
  i_VectorDataFactory,
  i_VectorItemSubsetBuilder,
  i_DownloaderFactory,
  i_CoordToStringConverter,
  u_GeoCoderBasic;

type
  TGeoCoderByRosreestr = class(TGeoCoderBasic)
  private
    FGeoJsonParser: IVectorDataLoader;
    FParserContext: TVectorLoadContext;
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
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AProjConverterFactory: IProjConverterFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  RegExpr,
  superobject,
  t_GeoTypes,
  i_ImportConfig,
  i_VectorItemSubset,
  i_VectorDataItemSimple,
  u_AnsiStr,
  u_StrFunc,
  u_GeoJsonParser,
  u_BinaryData,
  u_InterfaceListSimple,
  u_DownloadRequest,
  u_ResStrings;

{ TGeoCoderByRosreestr }

constructor TGeoCoderByRosreestr.Create(
  const AInetSettings: IInetConfig;
  const AGCNotifier: INotifierTime;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const ADownloaderFactory: IDownloaderFactory;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AProjConverterFactory: IProjConverterFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
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

  FGeoJsonParser := TGeoJsonParser.Create(
    AVectorItemSubsetBuilderFactory,
    AVectorDataFactory,
    AVectorGeometryLonLatFactory,
    AProjConverterFactory
  );

  FParserContext.Init;
  FParserContext.MainInfoFactory := AVectorDataItemMainInfoFactory;
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
  VGeoJsonObject: ISuperObject;
  VItems: IVectorItemSubset;
  VItem: IVectorDataItem;
  VStr: string;
  VStrA: AnsiString;
  VName, VFullDesc, VDescription: string;
begin
  if AResult = nil then begin
    Result := nil;
    Exit;
  end;

  if (AResult.Data = nil) or (AResult.Data.Size <= 0) then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  VStr := Utf8DataToUnicodeString(AResult.Data.Buffer, AResult.Data.Size);

  VJsonObject := SO(VStr);
  if not Assigned(VJsonObject) then begin
    raise EParserError.Create(Self.ClassName + ': ' + 'JSON parser error');
  end;

  VGeoJsonObject := VJsonObject.O['data'];
  if VGeoJsonObject = nil then begin
    raise EParserError.Create(Self.ClassName + ': ' + '"data" object is not found!');
  end;

  VStrA := UTF8Encode(VGeoJsonObject.AsJSon(False, False));

  VItems :=
    FGeoJsonParser.Load(
      FParserContext,
      TBinaryData.CreateByAnsiString(VStrA)
    );

  if (VItems = nil) or (VItems.Count <= 0) then begin
    Result := nil;
    Exit;
  end;

  VName := ASearch;
  VList := TInterfaceListSimple.Create;
  VCoordToStringConverter := FCoordToStringConverter.GetStatic;

  for I := 0 to VItems.Count - 1 do begin
    VItem := VItems[I];

    Assert(VItem <> nil);
    Assert(VItem.Geometry <> nil);

    VPoint := VItem.Geometry.GetGoToPoint;

    VDescription := VItem.Desc + #$D#$A + '[ ' + VCoordToStringConverter.LonLatConvert(VPoint) + ' ]';
    VFullDesc := ReplaceStr(VName + #$D#$A + VDescription, #$D#$A, '<br>');

    VPlace := Self.PlacemarkFactory.Build(VPoint, VName, VDescription, VFullDesc, 4);
    VList.Add(VPlace);
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
    'Referer: https://nspd.gov.ru/map?thematic=PKK';
var
  VQuery: AnsiString;
  VRegExpr: TRegExpr;
begin
  VRegExpr  := TRegExpr.Create;
  try
    VQuery := AnsiString(ASearch);
    VRegExpr.Expression := '^([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,7})(:([0-9]{1,5}))?$';

    if VRegExpr.Exec(VQuery) then begin
      VQuery :=
        VRegExpr.Match[1] + ':' +
        VRegExpr.Match[2] + ':' +
        VRegExpr.Match[3];

      if VRegExpr.Match[4] <> '' then begin
        VQuery := VQuery + ':' + VRegExpr.Match[5];
      end;

      Result :=
        TDownloadRequest.Create(
          cUrlBase + VQuery,
          cRequestHeader,
          InetSettings.GetStatic
        );
    end else begin
      raise Exception.CreateFmt('Unexpected query format: "%s"', [ASearch]);
    end;
  finally
    VRegExpr.Free;
  end;
end;

end.


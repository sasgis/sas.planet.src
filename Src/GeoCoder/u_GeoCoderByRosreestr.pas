{******************************************************************************}
{* SAS.Planet (SAS.ѕланета)                                                   *}
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
  RegExprUtils,
  superobject,
  t_GeoTypes,
  i_VectorDataItemSimple,
  i_Projection,
  u_InterfaceListSimple,
  u_ResStrings;

{ TGeoCoderByRosreestr }

procedure meters_to_lonlat(
  in_x, in_y: Double;
  out outout: TDoublePoint
);
begin
  outout.X := in_X / 6378137 * 180 / Pi;
  outout.Y := ((arctan(exp(in_Y / 6378137)) - Pi / 4) * 360) / Pi;
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
  x, y: double;
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

  if Assigned(VJsonObject) then begin
    VName := ASearch;
    VTempString := VJsonObject.S['feature.attrs.address'];
    if VTempString <> '' then VDescription := 'address: ' + VTempString;
    VTempString := VJsonObject.S['feature.attrs.adate'];
    if VTempString <> '' then VDescription := VDescription + #$D#$A + 'adate: ' + VTempString;
    VTempString := VJsonObject.S['feature.attrs.pubdate'];
    if VTempString <> '' then VDescription := VDescription + #$D#$A + 'pubdate: ' + VTempString;
    VTempString := VJsonObject.S['feature.attrs.cad_record_date'];
    if VTempString <> '' then VDescription := VDescription + #$D#$A + 'cad_record_date: ' + VTempString;
    VTempString := VJsonObject.S['feature.attrs.util_by_doc'];
    if VTempString <> '' then VDescription := VDescription + #$D#$A + 'util_by_doc: ' + VTempString;
    X := VJsonObject.D['feature.center.x'];
    Y := VJsonObject.D['feature.center.y'];

    try
      meters_to_lonlat(x, y, Vpoint);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [floattostr(x), floattostr(y)]);
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
var
  VSearch: AnsiString;
  i, j: integer;
  Vcount: Integer;
begin

  VSearch := AnsiString(ASearch);
  Vcount := 0;
  i := 1;
  j := ALPosEx(':', VSearch, i);
  while  j > i do
  begin
    inc(Vcount);
    i := j+1;
    j := ALPosEx(':', VSearch, i);
  end;

  if Vcount <> 0 then begin //cadastre number
    if Vcount = 2 then //  варталы
    begin
      Result :=
       PrepareRequestByURL(
         'http://pkk5.rosreestr.ru/api/features/2/' + URLEncode(AnsiToUtf8(VSearch))
       );
    end else // участки
    begin
     Result :=
       PrepareRequestByURL(
          'http://pkk5.rosreestr.ru/api/features/1/' + URLEncode(AnsiToUtf8(VSearch))
       );
    end;
  end else begin
    Result := nil;
  end;
end;
end.

// "http://pkk5.rosreestr.ru/api/typeahead?text="%"D0"%"BD"%"D0"%"BE"%"D0"%"B2"%"D0"%"BE&limit=10&skip=0&type=-1"
// "http://pkk5.rosreestr.ru/api/typeahead?text=vjcrdf&limit=10&skip=0&type=-1"
// http://pkk5.rosreestr.ru/api/typeahead?text=%D0%BA%D1%80%D0%B0%D1%81%D0%BD%D0%BE%D0%B4%D0%B0&limit=10&skip=0&type=-1

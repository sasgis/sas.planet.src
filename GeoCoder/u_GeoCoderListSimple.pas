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

unit u_GeoCoderListSimple;

interface

uses
  i_NotifierTime,
  i_InetConfig,
  i_GeoCoder,
  i_MarkDb,
  i_VectorItemSubsetBuilder,
  i_DownloadResultFactory,
  i_ValueToStringConverter,
  u_GeoCoderListBase;

type
  TGeoCoderListSimple = class(TGeoCoderListBase)
  public
    constructor Create(
      const AInetConfig: IInetConfig;
      const AGCNotifier: INotifierTime;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AResultFactory: IDownloadResultFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      const AMarksDb: IMarkDb
    );
  end;

implementation

uses
  c_GeoCoderGUIDSimple,
  i_GeoCoderList,
  u_GeoCoderListEntity,
  u_GeoCoderByGoogle,
  u_GeoCoderByYandex,
  u_GeoCoderBy2GIS,
  u_GeoCoderByOSM,
  u_GeoCoderByWikiMapia,
  u_GeoCoderByRosreestr,
  u_GeoCoderByNavitel,
  u_GeoCoderByURL,
  u_GeoCoderByPolishMap,
  u_GeoCoderByGpx,
  u_GeoCoderByTXT,
  u_GeoCoderByCoord,
  u_GeoCoderByMarks;

{ TGeoCoderListSimple }

constructor TGeoCoderListSimple.Create(
  const AInetConfig: IInetConfig;
  const AGCNotifier: INotifierTime;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AResultFactory: IDownloadResultFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  const AMarksDb: IMarkDb
);
var
  VItem: IGeoCoderListEntity;
begin
  inherited Create;
  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderGoogleGUID,
      'Google',
      TGeoCoderByGoogle.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderYandexGUID,
      'Yandex',
      TGeoCoderByYandex.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoder2GISGUID,
      '2GIS',
      TGeoCoderBy2GIS.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderOSMGUID,
      'OSM',
      TGeoCoderByOSM.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderWikiMapiaGUID,
      'WikiMapia',
      TGeoCoderByWikiMapia.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderRosreestrGUID,
      'Rosreestr',
      TGeoCoderByRosreestr.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory, AValueToStringConverterConfig)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderNavitelGUID,
      'Navitel',
      TGeoCoderByNavitel.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderURLGUID,
      'URL',
      TGeoCoderByURL.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory, AValueToStringConverterConfig)
    );
  Add(VItem);

  try
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderGpxGUID,
        'Offline search (*.gpx)',
        TGeoCoderByGpx.Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory, AValueToStringConverterConfig)
      );
    Add(VItem);
  Except
  end;

  try
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderPolishMapGUID,
        'Offline search (*.mp)',
        TGeoCoderByPolishMap.Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory, AValueToStringConverterConfig)
      );
    Add(VItem);
  Except
  end;

  try
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderGeonamesTXTGUID,
        'Offline search (*.txt)',
        TGeoCoderByTXT.Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory, AValueToStringConverterConfig)
      );
    Add(VItem);
  Except
  end;

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderCoordGUID,
      'Coordinates',
      TGeoCoderByCoord.Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory, AValueToStringConverterConfig)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderMarksGUID,
      'Marks',
      TGeoCoderByMarks.Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory, AMarksDb)
    );
  Add(VItem);

end;

end.

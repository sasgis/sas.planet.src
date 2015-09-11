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
  i_CoordConverterFactory,
  i_DownloadResultFactory,
  i_ValueToStringConverter,
  u_GeoCoderListBase;

type
  TGeoCoderListSimple = class(TGeoCoderListStatic)
  public
    constructor Create(
      const AUserDataPath: string;
      const AInetConfig: IInetConfig;
      const AGCNotifier: INotifierTime;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AResultFactory: IDownloadResultFactory;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AMarksDb: IMarkDb;
      const AProjectionSetFactory: IProjectionSetFactory
    );
  end;

implementation

uses
  SysUtils,
  c_GeoCoderGUIDSimple,
  i_InterfaceListSimple,
  i_GeoCoderList,
  u_InterfaceListSimple,
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
  u_GeoCoderByGPSies,
  u_GeoCoderByMarks;

{ TGeoCoderListSimple }

constructor TGeoCoderListSimple.Create(
  const AUserDataPath: string;
  const AInetConfig: IInetConfig;
  const AGCNotifier: INotifierTime;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AResultFactory: IDownloadResultFactory;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AMarksDb: IMarkDb;
  const AProjectionSetFactory: IProjectionSetFactory
);
var
  VItem: IGeoCoderListEntity;
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderGoogleGUID,
      'Google',
      TGeoCoderByGoogle.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderYandexGUID,
      'Yandex',
      TGeoCoderByYandex.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoder2GISGUID,
      '2GIS',
      TGeoCoderBy2GIS.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderOSMGUID,
      'OSM',
      TGeoCoderByOSM.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderWikiMapiaGUID,
      'WikiMapia',
      TGeoCoderByWikiMapia.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderRosreestrGUID,
      'Rosreestr',
      TGeoCoderByRosreestr.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory, AValueToStringConverter)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderNavitelGUID,
      'Navitel',
      TGeoCoderByNavitel.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderURLGUID,
      'URL',
      TGeoCoderByURL.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory, AValueToStringConverter)
    );
  VList.Add(VItem);

  try
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderGpxGUID,
        'Offline search (*.gpx)',
        TGeoCoderByGpx.Create(AUserDataPath + 'gpx' + PathDelim, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AValueToStringConverter)
      );
    VList.Add(VItem);
  Except
  end;

  try
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderPolishMapGUID,
        'Offline search (*.mp)',
        TGeoCoderByPolishMap.Create(AUserDataPath + 'mp' + PathDelim, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AValueToStringConverter)
      );
    VList.Add(VItem);
  Except
  end;

  try
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderGeonamesTXTGUID,
        'Offline search (*.txt)',
        TGeoCoderByTXT.Create(AUserDataPath + 'txt' + PathDelim, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AValueToStringConverter)
      );
    VList.Add(VItem);
  Except
  end;

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderCoordGUID,
      'Coordinates',
      TGeoCoderByCoord.Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory, AValueToStringConverter, AProjectionSetFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderMarksGUID,
      'Marks',
      TGeoCoderByMarks.Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory, AMarksDb)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderGPSiesGUID,
      'www.GPSies.com',
      TGeoCoderByGPSies.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, AResultFactory)
    );
  VList.Add(VItem);

  inherited Create(VList.MakeStaticAndClear);
end;

end.

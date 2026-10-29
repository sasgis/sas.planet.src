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

unit u_GeoCoderListSimple;

interface

uses
  i_NotifierTime,
  i_InetConfig,
  i_GeoCoder,
  i_GeoCoderConfig,
  i_MarkDb,
  i_MapTypeSetChangeable,
  i_VectorItemSubsetBuilder,
  i_VectorDataFactory,
  i_GeometryLonLatFactory,
  i_ProjectionSetFactory,
  i_ProjConverter,
  i_DownloaderFactory,
  i_CoordToStringConverter,
  u_GeoCoderListBase;

type
  TGeoCoderListSimple = class(TGeoCoderListStatic)
  public
    constructor Create(
      const AGeoCoderConfig: IGeoCoderConfig;
      const AInetConfig: IInetConfig;
      const AGCNotifier: INotifierTime;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const ADownloaderFactory: IDownloaderFactory;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AMarksDb: IMarkDb;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AProjConverterFactory: IProjConverterFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AKmlLayersSet: IMapTypeSetChangeable
    );
  end;

implementation

uses
  SysUtils,
  gnugettext,
  c_GeoCoderGUIDSimple,
  i_InterfaceListSimple,
  i_GeoCoderList,
  u_InterfaceListSimple,
  u_GeoCoderListEntity,
  u_GeoCoderByGoogle,
  u_GeoCoderByGoogleEarth,
  u_GeoCoderByYandex,
  u_GeoCoderByOSM,
  u_GeoCoderByIp2geolocation,
  u_GeoCoderByWikiMapia,
  u_GeoCoderByRosreestr,
  u_GeoCoderByURL,
  u_GeoCoderByPolishMap,
  u_GeoCoderByGpx,
  u_GeoCoderByTXT,
  u_GeoCoderByCoord,
  u_GeoCoderByMarks,
  u_GeoCoderByVectorLayers;

resourcestring
  rsOfflineSearchFmt = 'Offline search (%s)';

{ TGeoCoderListSimple }

constructor TGeoCoderListSimple.Create(
  const AGeoCoderConfig: IGeoCoderConfig;
  const AInetConfig: IInetConfig;
  const AGCNotifier: INotifierTime;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const ADownloaderFactory: IDownloaderFactory;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AMarksDb: IMarkDb;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AProjConverterFactory: IProjConverterFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AKmlLayersSet: IMapTypeSetChangeable

);
var
  VItem: IGeoCoderListEntity;
  VList: IInterfaceListSimple;
  VPath: string;
  VApiKey: string;
begin
  VList := TInterfaceListSimple.Create;

  VApiKey := AGeoCoderConfig.GoogleApiKey;
  if VApiKey <> '' then begin
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderGoogleGUID,
        'Google',
        TGeoCoderByGoogle.Create(
          AInetConfig,
          AGCNotifier,
          AVectorItemSubsetBuilderFactory,
          APlacemarkFactory,
          ADownloaderFactory,
          VApiKey
        ) as IGeoCoder
      );
    VList.Add(VItem);
  end;

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderGoogleEarthGUID,
      'Google Earth',
      TGeoCoderByGoogleEarth.Create(
        AInetConfig,
        AGCNotifier,
        AVectorItemSubsetBuilderFactory,
        APlacemarkFactory,
        ADownloaderFactory
      ) as IGeoCoder
    );
  VList.Add(VItem);

  VApiKey := AGeoCoderConfig.YandexApiKey;
  if VApiKey <> '' then begin
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderYandexGUID,
        _('Yandex'),
        TGeoCoderByYandex.Create(
          AInetConfig,
          AGCNotifier,
          AVectorItemSubsetBuilderFactory,
          APlacemarkFactory,
          ADownloaderFactory,
          VApiKey
        ) as IGeoCoder
      );
    VList.Add(VItem);
  end;

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderOSMGUID,
      'OSM',
      TGeoCoderByOSM.Create(
        AInetConfig,
        AGCNotifier,
        AVectorItemSubsetBuilderFactory,
        APlacemarkFactory,
        ADownloaderFactory
      ) as IGeoCoder
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderWikiMapiaGUID,
      'WikiMapia',
      TGeoCoderByWikiMapia.Create(
        AInetConfig,
        AGCNotifier,
        AVectorItemSubsetBuilderFactory,
        APlacemarkFactory,
        ADownloaderFactory
      ) as IGeoCoder
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderRosreestrGUID,
      _('Rosreestr'),
      TGeoCoderByRosreestr.Create(
        AInetConfig,
        AGCNotifier,
        AVectorItemSubsetBuilderFactory,
        APlacemarkFactory,
        ADownloaderFactory,
        ACoordToStringConverter,
        AVectorDataFactory,
        AVectorGeometryLonLatFactory,
        AProjConverterFactory,
        AVectorDataItemMainInfoFactory
      ) as IGeoCoder
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderIp2geolocationGUID,
      'IP2GeoLocation',
      TGeoCoderByIp2geolocation.Create(
        AInetConfig,
        AGCNotifier,
        AVectorItemSubsetBuilderFactory,
        APlacemarkFactory,
        ADownloaderFactory,
        '',
        True // AllowUseCookie
      ) as IGeoCoder
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderURLGUID,
      'URL',
      TGeoCoderByURL.Create(
        AInetConfig,
        AGCNotifier,
        AProjectionSetFactory,
        AVectorItemSubsetBuilderFactory,
        APlacemarkFactory,
        ADownloaderFactory,
        ACoordToStringConverter
      ) as IGeoCoder
    );
  VList.Add(VItem);

  VPath := AGeoCoderConfig.DataPath + 'gpx' + PathDelim;
  if DirectoryExists(VPath) then begin
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderGpxGUID,
        Format(rsOfflineSearchFmt, ['*.gpx']),
        TGeoCoderByGpx.Create(
          VPath,
          AVectorItemSubsetBuilderFactory,
          APlacemarkFactory,
          ACoordToStringConverter,
          AVectorGeometryLonLatFactory,
          AVectorDataFactory,
          AVectorDataItemMainInfoFactory
        ) as IGeoCoder
      );
    VList.Add(VItem);
  end;

  VPath := AGeoCoderConfig.DataPath + 'mp' + PathDelim;
  if DirectoryExists(VPath) then begin
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderPolishMapGUID,
        Format(rsOfflineSearchFmt, ['*.mp']),
        TGeoCoderByPolishMap.Create(
          VPath,
          AVectorItemSubsetBuilderFactory,
          APlacemarkFactory,
          ACoordToStringConverter
        ) as IGeoCoder
      );
    VList.Add(VItem);
  end;

  VPath := AGeoCoderConfig.DataPath + 'txt' + PathDelim;
  if DirectoryExists(VPath) then begin
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderGeonamesTXTGUID,
        Format(rsOfflineSearchFmt, ['*.txt']),
        TGeoCoderByTXT.Create(
          VPath,
          AVectorItemSubsetBuilderFactory,
          APlacemarkFactory,
          ACoordToStringConverter
        ) as IGeoCoder
      );
    VList.Add(VItem);
  end;

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderCoordGUID,
      _('Coordinates'),
      TGeoCoderByCoord.Create(
        AVectorItemSubsetBuilderFactory,
        APlacemarkFactory,
        ACoordToStringConverter,
        AProjectionSetFactory
      ) as IGeoCoder
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderMarksGUID,
      _('Marks'),
      TGeoCoderByMarks.Create(
        AVectorItemSubsetBuilderFactory,
        APlacemarkFactory,
        AMarksDb
      ) as IGeoCoder
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderVectorLayersGUID,
      _('Vector layers'),
      TGeoCoderByVectorLayers.Create(
        AVectorItemSubsetBuilderFactory,
        APlacemarkFactory,
        AKmlLayersSet
      ) as IGeoCoder
    );
  VList.Add(VItem);

  inherited Create(VList.MakeStaticAndClear);
end;

end.

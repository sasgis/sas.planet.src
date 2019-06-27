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
  i_VectorDataFactory,
  i_GeometryLonLatFactory,
  i_ProjectionSetFactory,
  i_DownloaderFactory,
  i_CoordToStringConverter,
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
      const ADownloaderFactory: IDownloaderFactory;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AMarksDb: IMarkDb;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
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
  u_GeoCoderByYandex,
  u_GeoCoderBy2GIS,
  u_GeoCoderByOSM,
  u_GeoCoderByIp2geolocation,
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
  const ADownloaderFactory: IDownloaderFactory;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AMarksDb: IMarkDb;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory

);
var
  VItem: IGeoCoderListEntity;
  VList: IInterfaceListSimple;
  VPath: string;
begin
  VList := TInterfaceListSimple.Create;

  VPath := AUserDataPath + cGoogleApiKeyFileName;
  if FileExists(VPath) then begin
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderGoogleGUID,
        'Google',
        TGeoCoderByGoogle.Create(
          VPath,
          AInetConfig,
          AGCNotifier,
          AVectorItemSubsetBuilderFactory,
          APlacemarkFactory,
          ADownloaderFactory
        )
      );
    VList.Add(VItem);
  end;

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderYandexGUID,
      _('Yandex'),
      TGeoCoderByYandex.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ADownloaderFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoder2GISGUID,
      '2GIS',
      TGeoCoderBy2GIS.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ADownloaderFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderOSMGUID,
      'OSM',
      TGeoCoderByOSM.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ADownloaderFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderWikiMapiaGUID,
      'WikiMapia',
      TGeoCoderByWikiMapia.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ADownloaderFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderRosreestrGUID,
      _('Rosreestr'),
      TGeoCoderByRosreestr.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ADownloaderFactory, ACoordToStringConverter)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderIp2geolocationGUID,
      'IP2GeoLocation',
      TGeoCoderByIp2geolocation.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ADownloaderFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderNavitelGUID,
      _('Navitel'),
      TGeoCoderByNavitel.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ADownloaderFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderURLGUID,
      'URL',
      TGeoCoderByURL.Create(AInetConfig, AGCNotifier, AProjectionSetFactory, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ADownloaderFactory, ACoordToStringConverter)
    );
  VList.Add(VItem);

  VPath := AUserDataPath + 'gpx' + PathDelim;
  if DirectoryExists(VPath) then begin
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderGpxGUID,
        Format(_('Offline search (%s)'), ['*.gpx']),
        TGeoCoderByGpx.Create(VPath, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ACoordToStringConverter, AVectorGeometryLonLatFactory, AVectorDataFactory, AVectorDataItemMainInfoFactory)
      );
    VList.Add(VItem);
  end;

  VPath := AUserDataPath + 'mp' + PathDelim;
  if DirectoryExists(VPath) then begin
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderPolishMapGUID,
        Format(_('Offline search (%s)'), ['*.mp']),
        TGeoCoderByPolishMap.Create(VPath, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ACoordToStringConverter)
      );
    VList.Add(VItem);
  end;

  VPath := AUserDataPath + 'txt' + PathDelim;
  if DirectoryExists(VPath) then begin
    VItem :=
      TGeoCoderListEntity.Create(
        CGeoCoderGeonamesTXTGUID,
        Format(_('Offline search (%s)'), ['*.txt']),
        TGeoCoderByTXT.Create(VPath, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ACoordToStringConverter)
      );
    VList.Add(VItem);
  end;

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderCoordGUID,
      _('Coordinates'),
      TGeoCoderByCoord.Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory, ACoordToStringConverter, AProjectionSetFactory)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderMarksGUID,
      _('Marks'),
      TGeoCoderByMarks.Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory, AMarksDb)
    );
  VList.Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderGPSiesGUID,
      'www.GPSies.com',
      TGeoCoderByGPSies.Create(AInetConfig, AGCNotifier, AVectorItemSubsetBuilderFactory, APlacemarkFactory, ADownloaderFactory)
    );
  VList.Add(VItem);

  inherited Create(VList.MakeStaticAndClear);
end;

end.

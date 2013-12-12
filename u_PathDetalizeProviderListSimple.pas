{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_PathDetalizeProviderListSimple;

interface

uses
  i_LanguageManager,
  i_InetConfig,
  i_NotifierTime,
  i_DownloadResultFactory,
  i_VectorDataFactory,
  i_GeometryLonLatFactory,
  i_VectorDataLoader,
  u_PathDetalizeProviderListBase;

type
  TPathDetalizeProviderListSimple = class(TPathDetalizeProviderListBase)
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AInetConfig: IInetConfig;
      const AGCNotifier: INotifierTime;
      const AResultFactory: IDownloadResultFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AKmlLoader: IVectorDataLoader
    );
  end;

implementation

uses
  gnugettext,
  c_PathDetalizeProvidersGUID,
  i_StringConfigDataElement,
  i_Downloader,
  i_PathDetalizeProvider,
  i_PathDetalizeProviderList,
  u_StringConfigDataElementWithDefByGetText,
  u_DownloaderHttpWithTTL,
  u_PathDetalizeProviderListEntity,
  u_PathDetalizeProviderYourNavigation,
  u_PathDetalizeProviderMailRu,
  u_PathDetalizeProviderCloudMade;

{ TPathDetalizeProviderListSimple }

constructor TPathDetalizeProviderListSimple.Create(
  const ALanguageManager: ILanguageManager;
  const AInetConfig: IInetConfig;
  const AGCNotifier: INotifierTime;
  const AResultFactory: IDownloadResultFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AKmlLoader: IVectorDataLoader
);
var
  VEntity: IPathDetalizeProviderListEntity;
  VCaption: IStringConfigDataElement;
  VDescription: IStringConfigDataElement;
  VMenuItemName: IStringConfigDataElement;
  VProvider: IPathDetalizeProvider;
  VDownloader: IDownloader;
begin
  inherited Create;
  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_noop('By car (Shortest) with Maps@mail.ru')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by car (Shortest) with Maps@mail.ru')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('Maps@mail.ru') + '|0020~\' + _('By Car (Shortest)') + '|0010'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderMailRu.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      'http://maps.mail.ru/stamperx/getPath.aspx?mode=distance'
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderMailRuShortest,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By car (Fastest) with Maps@mail.ru')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by car (Fastest) with Maps@mail.ru')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('Maps@mail.ru') + '|0020~\' + _('By Car (Fastest)') + '|0020'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderMailRu.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      'http://maps.mail.ru/stamperx/getPath.aspx?mode=time'
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderMailRuFastest,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By car (Fastest with traffic) with Maps@mail.ru')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by car (Fastest with traffic) with Maps@mail.ru')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      False,
      '',
      False,
      _('Maps@mail.ru') + '|0020~\' + _('By Car (Fastest with traffic)') + '|0030'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderMailRu.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      'http://maps.mail.ru/stamperx/getPath.aspx?mode=deftime'
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderMailRuFastestWithTraffic,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By car (Fastest) with yournavigation.org')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by car (Fastest) with yournavigation.org')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('yournavigation.org (OSM)') + '|0030~\' + _('By Car (Fastest)') + '|0010'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderYourNavigation.Create(
      AInetConfig,
      VDownloader,
      AVectorDataItemMainInfoFactory,
      AVectorGeometryLonLatFactory,
      AKmlLoader,
      'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=1&layer=mapnik'
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderYourNavigationFastestByCar,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By car (Shortest) with yournavigation.org')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by car (Shortest) with yournavigation.org')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('yournavigation.org (OSM)') + '|0030~\' + _('By Car (Shortest)') + '|0020'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderYourNavigation.Create(
      AInetConfig,
      VDownloader,
      AVectorDataItemMainInfoFactory,
      AVectorGeometryLonLatFactory,
      AKmlLoader,
      'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=0&layer=mapnik'
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderYourNavigationShortestByCar,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By bicycle (Fastest) with yournavigation.org')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by bicycle (Fastest) with yournavigation.org')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('yournavigation.org (OSM)') + '|0030~\' + _('By Bicycle (Fastest)') + '|0030'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderYourNavigation.Create(
      AInetConfig,
      VDownloader,
      AVectorDataItemMainInfoFactory,
      AVectorGeometryLonLatFactory,
      AKmlLoader,
      'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=1&layer=mapnik'
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderYourNavigationFastestByBicycle,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By bicycle (Shortest) with yournavigation.org')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by bicycle (Shortest) with yournavigation.org')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('yournavigation.org (OSM)') + '|0030~\' + _('By Bicycle (Shortest)') + '|0040'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderYourNavigation.Create(
      AInetConfig,
      VDownloader,
      AVectorDataItemMainInfoFactory,
      AVectorGeometryLonLatFactory,
      AKmlLoader,
      'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=0&layer=mapnik'
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderYourNavigationShortestByBicycle,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By car (Fastest) with cloudmade.com')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by car (Fastest) with cloudmade.com')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Car (Fastest)') + '|0010'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      car,
      fastest
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderCloudMadeFastestByCar,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By foot (Fastest) with cloudmade.com')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by foot (Fastest) with cloudmade.com')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Foot (Fastest)') + '|0020'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      foot,
      fastest
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderCloudMadeFastestByFoot,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By bicycle (Fastest) with cloudmade.com')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by bicycle (Fastest) with cloudmade.com')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Bicycle (Fastest)') + '|0030'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      bicycle,
      fastest
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderCloudMadeFastestByBicycle,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By car (Shortest) with cloudmade.com')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by car (Shortest) with cloudmade.com')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Car (Shortest)') + '|0040'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      car,
      shortest
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderCloudMadeShortestByCar,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By foot (Shortest) with cloudmade.com')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by foot (Shortest) with cloudmade.com')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Foot (Shortest)') + '|0050'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      foot,
      shortest
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderCloudMadeShortestByFoot,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);

  VCaption :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('By bicycle (Shortest) with cloudmade.com')
    );
  VDescription :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      gettext_NoOp('Detalize route by bicycle (Shortest) with cloudmade.com')
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByGetText.Create(
      ALanguageManager,
      _('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Bicycle (Shortest)') + '|0060'
    );
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      bicycle,
      shortest
    );
  VEntity :=
    TPathDetalizeProviderListEntity.Create(
      CPathDetalizeProviderCloudMadeShortestByBicycle,
      VCaption,
      VDescription,
      VMenuItemName,
      VProvider
    );
  Add(VEntity);
end;

end.

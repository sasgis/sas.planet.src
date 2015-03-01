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

unit u_PathDetalizeProviderTreeSimple;

interface

uses
  i_StaticTreeItem,
  i_LanguageManager,
  i_InetConfig,
  i_NotifierTime,
  i_DownloadResultFactory,
  i_VectorDataFactory,
  i_GeometryLonLatFactory,
  i_VectorDataLoader,
  i_PathDetalizeProviderTreeEntity,
  i_GUIDSet,
  u_TreeChangeableBase;

type
  TPathDetalizeProviderTreeSimple = class(TTreeChangeableBase)
  private
    FProvidersSet: IGUIDInterfaceSet;
    function CreateProvidersSet(
      const AInetConfig: IInetConfig;
      const AGCNotifier: INotifierTime;
      const AResultFactory: IDownloadResultFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AKmlLoader: IVectorDataLoader
    ): IGUIDInterfaceSet;

    function CreateMailRu: IStaticTreeItem;
    function CreateYourNavigation: IStaticTreeItem;
    function CreateCloudmade: IStaticTreeItem;
  protected
    function CreateStatic: IStaticTreeItem; override;
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
  i_Downloader,
  i_PathDetalizeProvider,
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_StaticTreeItem,
  u_DownloaderHttpWithTTL,
  u_GUIDInterfaceSet,
  u_PathDetalizeProviderTreeEntity,
  u_PathDetalizeProviderYourNavigation,
  u_PathDetalizeProviderMailRu,
  u_PathDetalizeProviderCloudMade;

{ TPathDetalizeProviderTreeSimple }

constructor TPathDetalizeProviderTreeSimple.Create(
  const ALanguageManager: ILanguageManager;
  const AInetConfig: IInetConfig;
  const AGCNotifier: INotifierTime;
  const AResultFactory: IDownloadResultFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(ALanguageManager.ChangeNotifier);
  FProvidersSet :=
    CreateProvidersSet(
      AInetConfig,
      AGCNotifier,
      AResultFactory,
      AVectorDataItemMainInfoFactory,
      AVectorGeometryLonLatFactory,
      AKmlLoader
    );
end;

function TPathDetalizeProviderTreeSimple.CreateProvidersSet(
  const AInetConfig: IInetConfig;
  const AGCNotifier: INotifierTime;
  const AResultFactory: IDownloadResultFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AKmlLoader: IVectorDataLoader
): IGUIDInterfaceSet;
var
  VProvider: IPathDetalizeProvider;
  VDownloader: IDownloader;
begin
  Result := TGUIDInterfaceSet.Create;

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderMailRu.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      'http://maps.mail.ru/stamperx/getPath.aspx?mode=distance'
    );

  Result.Add(CPathDetalizeProviderMailRuShortest, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderMailRu.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      'http://maps.mail.ru/stamperx/getPath.aspx?mode=time'
    );
  Result.Add(CPathDetalizeProviderMailRuFastest, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderMailRu.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      'http://maps.mail.ru/stamperx/getPath.aspx?mode=deftime'
    );
  Result.Add(CPathDetalizeProviderMailRuFastestWithTraffic, VProvider);

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
  Result.Add(CPathDetalizeProviderYourNavigationFastestByCar, VProvider);

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
  Result.Add(CPathDetalizeProviderYourNavigationShortestByCar, VProvider);

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
  Result.Add(CPathDetalizeProviderYourNavigationFastestByBicycle, VProvider);

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
  Result.Add(CPathDetalizeProviderYourNavigationShortestByBicycle, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      car,
      fastest
    );
  Result.Add(CPathDetalizeProviderCloudMadeFastestByCar, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      foot,
      fastest
    );
  Result.Add(CPathDetalizeProviderCloudMadeFastestByFoot, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      bicycle,
      fastest
    );
  Result.Add(CPathDetalizeProviderCloudMadeFastestByBicycle, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      car,
      shortest
    );
  Result.Add(CPathDetalizeProviderCloudMadeShortestByCar, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      foot,
      shortest
    );
  Result.Add(CPathDetalizeProviderCloudMadeShortestByFoot, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, AResultFactory);
  VProvider :=
    TPathDetalizeProviderCloudMade.Create(
      AInetConfig,
      VDownloader,
      AVectorGeometryLonLatFactory,
      bicycle,
      shortest
    );
  Result.Add(CPathDetalizeProviderCloudMadeShortestByBicycle, VProvider);
end;

function TPathDetalizeProviderTreeSimple.CreateMailRu: IStaticTreeItem;
var
  VList: IInterfaceListSimple;
  VGUID: TGUID;
  VProvider: IPathDetalizeProvider;
  VEntity: IPathDetalizeProviderTreeEntity;
  VItem: IStaticTreeItem;
begin
  VList := TInterfaceListSimple.Create;

  VGUID := CPathDetalizeProviderMailRuShortest;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by car (Shortest) with Maps@mail.ru'),
      _('By Car (Shortest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0010',
      nil
    );
  VList.Add(VItem);

  VGUID := CPathDetalizeProviderMailRuFastest;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by car (Fastest) with Maps@mail.ru'),
      _('By Car (Fastest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0020',
      nil
    );
  VList.Add(VItem);

  VGUID := CPathDetalizeProviderMailRuFastest;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by car (Fastest with traffic) with Maps@mail.ru'),
      _('By Car (Fastest with traffic)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0030',
      nil
    );
  VList.Add(VItem);

  Result :=
    TStaticTreeItem.Create(
      nil,
      _('Maps@mail.ru'),
      '0020~',
      VList.MakeStaticAndClear
    );
end;

function TPathDetalizeProviderTreeSimple.CreateYourNavigation: IStaticTreeItem;
var
  VList: IInterfaceListSimple;
  VGUID: TGUID;
  VProvider: IPathDetalizeProvider;
  VEntity: IPathDetalizeProviderTreeEntity;
  VItem: IStaticTreeItem;
begin
  VList := TInterfaceListSimple.Create;

  VGUID := CPathDetalizeProviderYourNavigationFastestByCar;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by car (Fastest) with yournavigation.org'),
      _('By Car (Fastest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0010',
      nil
    );
  VList.Add(VItem);

  VGUID := CPathDetalizeProviderYourNavigationShortestByCar;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by car (Shortest) with yournavigation.org'),
      _('By Car (Shortest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0020',
      nil
    );
  VList.Add(VItem);

  VGUID := CPathDetalizeProviderYourNavigationFastestByBicycle;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by bicycle (Fastest) with yournavigation.org'),
      _('By Bicycle (Fastest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0030',
      nil
    );
  VList.Add(VItem);

  VGUID := CPathDetalizeProviderYourNavigationShortestByBicycle;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by bicycle (Shortest) with yournavigation.org'),
      _('By Bicycle (Shortest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0040',
      nil
    );
  VList.Add(VItem);

  Result :=
    TStaticTreeItem.Create(
      nil,
      _('yournavigation.org (OSM)'),
      '0030~',
      VList.MakeStaticAndClear
    );
end;

function TPathDetalizeProviderTreeSimple.CreateCloudmade: IStaticTreeItem;
var
  VList: IInterfaceListSimple;
  VGUID: TGUID;
  VProvider: IPathDetalizeProvider;
  VEntity: IPathDetalizeProviderTreeEntity;
  VItem: IStaticTreeItem;
begin
  VList := TInterfaceListSimple.Create;

  VGUID := CPathDetalizeProviderCloudMadeFastestByCar;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by car (Fastest) with cloudmade.com'),
      _('By Car (Fastest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0010',
      nil
    );
  VList.Add(VItem);

  VGUID := CPathDetalizeProviderCloudMadeFastestByFoot;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by foot (Fastest) with cloudmade.com'),
      _('By Foot (Fastest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0020',
      nil
    );
  VList.Add(VItem);

  VGUID := CPathDetalizeProviderCloudMadeFastestByBicycle;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by bicycle (Fastest) with cloudmade.com'),
      _('By Bicycle (Fastest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0030',
      nil
    );
  VList.Add(VItem);

  VGUID := CPathDetalizeProviderCloudMadeShortestByCar;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by car (Shortest) with cloudmade.com'),
      _('By Car (Shortest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0040',
      nil
    );
  VList.Add(VItem);

  VGUID := CPathDetalizeProviderCloudMadeShortestByFoot;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by foot (Shortest) with cloudmade.com'),
      _('By Foot (Shortest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0050',
      nil
    );
  VList.Add(VItem);

  VGUID := CPathDetalizeProviderCloudMadeShortestByBicycle;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by bicycle (Shortest) with cloudmade.com'),
      _('By Bicycle (Shortest)'),
      VProvider
    );
  VItem :=
    TStaticTreeItem.Create(
      VEntity,
      VEntity.MenuItemName,
      '0060',
      nil
    );
  VList.Add(VItem);

  Result :=
    TStaticTreeItem.Create(
      nil,
      _('maps.cloudmade.com (OSM)'),
      '0010~',
      VList.MakeStaticAndClear
    );
end;

function TPathDetalizeProviderTreeSimple.CreateStatic: IStaticTreeItem;
var
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;

  VList.Add(CreateCloudmade);
  VList.Add(CreateMailRu);
  VList.Add(CreateYourNavigation);

  Result :=
    TStaticTreeItem.Create(
      nil,
      '',
      '',
      VList.MakeStaticAndClear
    );
end;

end.

{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2020, SAS.Planet development team.                      *}
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
  i_DownloaderFactory,
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
      const ADownloaderFactory: IDownloaderFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AKmlLoader: IVectorDataLoader
    ): IGUIDInterfaceSet;

    function CreateYourNavigation: IStaticTreeItem;
    function CreateOSRM: IStaticTreeItem;
  protected
    function CreateStatic: IStaticTreeItem; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AInetConfig: IInetConfig;
      const AGCNotifier: INotifierTime;
      const ADownloaderFactory: IDownloaderFactory;
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
  u_PathDetalizeProviderOSRM,
  u_PathDetalizeProviderYourNavigation;

{ TPathDetalizeProviderTreeSimple }

constructor TPathDetalizeProviderTreeSimple.Create(
  const ALanguageManager: ILanguageManager;
  const AInetConfig: IInetConfig;
  const AGCNotifier: INotifierTime;
  const ADownloaderFactory: IDownloaderFactory;
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
      ADownloaderFactory,
      AVectorDataItemMainInfoFactory,
      AVectorGeometryLonLatFactory,
      AKmlLoader
    );
end;

function TPathDetalizeProviderTreeSimple.CreateProvidersSet(
  const AInetConfig: IInetConfig;
  const AGCNotifier: INotifierTime;
  const ADownloaderFactory: IDownloaderFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AKmlLoader: IVectorDataLoader
): IGUIDInterfaceSet;
var
  VProvider: IPathDetalizeProvider;
  VDownloader: IDownloader;
begin
  Result := TGUIDInterfaceSet.Create;

{$REGION 'YourNavigation'}
  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
  VProvider :=
    TPathDetalizeProviderYourNavigation.Create(
      'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=1&layer=mapnik',
      VDownloader,
      AInetConfig,
      AVectorGeometryLonLatFactory,
      AVectorDataItemMainInfoFactory,
      AKmlLoader
    );
  Result.Add(CPathDetalizeProviderYourNavigationFastestByCar, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
  VProvider :=
    TPathDetalizeProviderYourNavigation.Create(
      'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=0&layer=mapnik',
      VDownloader,
      AInetConfig,
      AVectorGeometryLonLatFactory,
      AVectorDataItemMainInfoFactory,
      AKmlLoader
    );
  Result.Add(CPathDetalizeProviderYourNavigationShortestByCar, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
  VProvider :=
    TPathDetalizeProviderYourNavigation.Create(
      'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=1&layer=mapnik',
      VDownloader,
      AInetConfig,
      AVectorGeometryLonLatFactory,
      AVectorDataItemMainInfoFactory,
      AKmlLoader
    );
  Result.Add(CPathDetalizeProviderYourNavigationFastestByBicycle, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
  VProvider :=
    TPathDetalizeProviderYourNavigation.Create(
      'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=0&layer=mapnik',
      VDownloader,
      AInetConfig,
      AVectorGeometryLonLatFactory,
      AVectorDataItemMainInfoFactory,
      AKmlLoader
    );
  Result.Add(CPathDetalizeProviderYourNavigationShortestByBicycle, VProvider);
{$ENDREGION}

{$REGION 'Project OSRM'}

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
  VProvider :=
    TPathDetalizeProviderOSRM.Create(
      'http://router.project-osrm.org/route/v1/car/',
      VDownloader,
      AInetConfig,
      AVectorGeometryLonLatFactory
    );
  Result.Add(CPathDetalizeProviderOSRMByCar, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
  VProvider :=
    TPathDetalizeProviderOSRM.Create(
      'https://routing.openstreetmap.de/routed-bike/route/v1/driving/',
      VDownloader,
      AInetConfig,
      AVectorGeometryLonLatFactory
    );
  Result.Add(CPathDetalizeProviderOSRMByBike, VProvider);

  VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
  VProvider :=
    TPathDetalizeProviderOSRM.Create(
      'https://routing.openstreetmap.de/routed-foot/route/v1/driving/',
      VDownloader,
      AInetConfig,
      AVectorGeometryLonLatFactory
    );
  Result.Add(CPathDetalizeProviderOSRMByFoot, VProvider);
{$ENDREGION}
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
      'yournavigation.org (OSM)',
      '0010~',
      VList.MakeStaticAndClear
    );
end;

function TPathDetalizeProviderTreeSimple.CreateOSRM: IStaticTreeItem;
var
  VList: IInterfaceListSimple;
  VGUID: TGUID;
  VProvider: IPathDetalizeProvider;
  VEntity: IPathDetalizeProviderTreeEntity;
  VItem: IStaticTreeItem;
begin
  VList := TInterfaceListSimple.Create;

  VGUID := CPathDetalizeProviderOSRMByCar;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by Car with project-osrm.org'),
      _('By Car'),
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

  VGUID := CPathDetalizeProviderOSRMByBike;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by Bike with project-osrm.org'),
      _('By Bike'),
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

  VGUID := CPathDetalizeProviderOSRMByFoot;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      _('Detalize route by Foot with project-osrm.org'),
      _('By Foot'),
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
      'Project OSRM',
      '0020~',
      VList.MakeStaticAndClear
    );
end;

function TPathDetalizeProviderTreeSimple.CreateStatic: IStaticTreeItem;
var
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;

  VList.Add(CreateYourNavigation);
  VList.Add(CreateOSRM);

  Result :=
    TStaticTreeItem.Create(
      nil,
      '',
      '',
      VList.MakeStaticAndClear
    );
end;

end.

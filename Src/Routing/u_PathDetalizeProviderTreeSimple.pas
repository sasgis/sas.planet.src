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
  i_PathDetalizeConfig,
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
    FArrayOfProjectOSRM: TArrayOfProjectOSRM;
    FPathDetalizeConfig: IPathDetalizeConfig;
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
    function CreateCustomOSRM(const AItemId: Integer): IStaticTreeItem;
    function CreateOsmScout: IStaticTreeItem;
  protected
    function CreateStatic: IStaticTreeItem; override;
  public
    constructor Create(
      const APathDetalizeConfig: IPathDetalizeConfig;
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
  SysUtils,
  gnugettext,
  libosmscout_route,
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
  u_PathDetalizeProviderOsmScout,
  u_PathDetalizeProviderYourNavigation;

{ TPathDetalizeProviderTreeSimple }

constructor TPathDetalizeProviderTreeSimple.Create(
  const APathDetalizeConfig: IPathDetalizeConfig;
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
  FPathDetalizeConfig := APathDetalizeConfig;
  FArrayOfProjectOSRM := FPathDetalizeConfig.ArrayOfProjectOSRM;
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
  I: Integer;
  VAddress: string;
  VProvider: IPathDetalizeProvider;
  VDownloader: IDownloader;
  VOsmScoutRouteContext: IOsmScoutRouteContext;
begin
  Result := TGUIDInterfaceSet.Create;

{$REGION 'YourNavigation'}
  if FPathDetalizeConfig.EnableYourNavigation then begin
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
  end;
{$ENDREGION}

{$REGION 'Project OSRM'}
  if FPathDetalizeConfig.EnableProjectOSRM then begin
    VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
    VProvider :=
      TPathDetalizeProviderOSRM.Create(
        'https://routing.openstreetmap.de/routed-car/route/v1/driving/',
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
  end;
{$ENDREGION}

{$REGION 'Project OSRM (with custom server)'}
  for I := 0 to Length(FArrayOfProjectOSRM) - 1 do begin
    VAddress := FArrayOfProjectOSRM[I].Address;
    if VAddress <> '' then begin
      if Pos('://', VAddress) <= 0 then begin
        VAddress := 'http://' + VAddress;
      end;
      if VAddress[Length(VAddress)] <> '/' then begin
        VAddress := VAddress + '/';
      end;

      VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
      VProvider :=
        TPathDetalizeProviderOSRM.Create(
          AnsiString(VAddress) + 'route/v1/car/',
          VDownloader,
          AInetConfig,
          AVectorGeometryLonLatFactory
        );
      Result.Add(FArrayOfProjectOSRM[I].Guid[0], VProvider);

      VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
      VProvider :=
        TPathDetalizeProviderOSRM.Create(
          AnsiString(VAddress) + 'route/v1/bike/',
          VDownloader,
          AInetConfig,
          AVectorGeometryLonLatFactory
        );
      Result.Add(FArrayOfProjectOSRM[I].Guid[1], VProvider);

      VDownloader := TDownloaderHttpWithTTL.Create(AGCNotifier, ADownloaderFactory);
      VProvider :=
        TPathDetalizeProviderOSRM.Create(
          AnsiString(VAddress) + 'route/v1/foot/',
          VDownloader,
          AInetConfig,
          AVectorGeometryLonLatFactory
        );
      Result.Add(FArrayOfProjectOSRM[I].Guid[2], VProvider);
    end;
  end;
{$ENDREGION}

{$REGION 'OSM Scout (offline)'}
  if IsLibOsmScoutRouteAvailable then begin

    VOsmScoutRouteContext :=
      NewOsmScoutRouteContext(
        ExtractFilePath(ParamStr(0)) + 'OsmScout\'
      );

    VProvider :=
      TPathDetalizeProviderOsmScout.Create(
        ROUTE_PROFILE_CAR,
        VOsmScoutRouteContext,
        AVectorGeometryLonLatFactory
      );
    Result.Add(CPathDetalizeProviderOsmScoutByCar, VProvider);

    VProvider :=
      TPathDetalizeProviderOsmScout.Create(
        ROUTE_PROFILE_BIKE,
        VOsmScoutRouteContext,
        AVectorGeometryLonLatFactory
      );
    Result.Add(CPathDetalizeProviderOsmScoutByBike, VProvider);

    VProvider :=
      TPathDetalizeProviderOsmScout.Create(
        ROUTE_PROFILE_FOOT,
        VOsmScoutRouteContext,
        AVectorGeometryLonLatFactory
      );
    Result.Add(CPathDetalizeProviderOsmScoutByFoot, VProvider);
  end;
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

function TPathDetalizeProviderTreeSimple.CreateCustomOSRM(
  const AItemId: Integer
): IStaticTreeItem;

  function _GetCaption: string;
  var
    I: Integer;
  begin
    Result := FArrayOfProjectOSRM[AItemId].Address;
    I := Pos('://', Result);
    if I > 0 then begin
      Result := Copy(Result, I+3);
    end;
    I := Length(Result);
    if (I > 0) and (Result[I] = '/') then begin
      SetLength(Result, I-1);
    end;
    Result := Result + ' (OSRM)';
  end;

var
  VList: IInterfaceListSimple;
  VGUID: TGUID;
  VProvider: IPathDetalizeProvider;
  VEntity: IPathDetalizeProviderTreeEntity;
  VItem: IStaticTreeItem;
begin
  VList := TInterfaceListSimple.Create;

  VGUID := FArrayOfProjectOSRM[AItemId].Guid[0];
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      '',
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

  VGUID := FArrayOfProjectOSRM[AItemId].Guid[1];
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      '',
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

  VGUID := FArrayOfProjectOSRM[AItemId].Guid[2];
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      '',
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
      _GetCaption,
      '00' + IntToStr(30 + AItemId + 1) + '~',
      VList.MakeStaticAndClear
    );
end;

function TPathDetalizeProviderTreeSimple.CreateOsmScout: IStaticTreeItem;
var
  VList: IInterfaceListSimple;
  VGUID: TGUID;
  VProvider: IPathDetalizeProvider;
  VEntity: IPathDetalizeProviderTreeEntity;
  VItem: IStaticTreeItem;
begin
  VList := TInterfaceListSimple.Create;

  VGUID := CPathDetalizeProviderOsmScoutByCar;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      '',
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

  VGUID := CPathDetalizeProviderOsmScoutByBike;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      '',
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

  VGUID := CPathDetalizeProviderOsmScoutByFoot;
  VProvider := IPathDetalizeProvider(FProvidersSet.GetByGUID(VGUID));
  VEntity :=
    TPathDetalizeProviderTreeEntity.Create(
      VGUID,
      '',
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
      'OSM Scout (offline)',
      '0040~',
      VList.MakeStaticAndClear
    );
end;

function TPathDetalizeProviderTreeSimple.CreateStatic: IStaticTreeItem;
var
  I: Integer;
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;

  if FPathDetalizeConfig.EnableYourNavigation then begin
    VList.Add(CreateYourNavigation);
  end;

  if FPathDetalizeConfig.EnableProjectOSRM then begin
    VList.Add(CreateOSRM);
  end;

  for I := 0 to Length(FArrayOfProjectOSRM) - 1 do begin
    VList.Add( CreateCustomOSRM(I) );
  end;

  if IsLibOsmScoutRouteAvailable then begin
    VList.Add(CreateOsmScout);
  end;

  Result :=
    TStaticTreeItem.Create(
      nil,
      '',
      '',
      VList.MakeStaticAndClear
    );
end;

end.

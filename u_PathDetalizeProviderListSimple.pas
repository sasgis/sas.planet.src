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
  i_ProxySettings,
  i_VectorDataFactory,
  i_VectorItmesFactory,
  i_VectorDataLoader,
  u_PathDetalizeProviderListBase;

type
  TPathDetalizeProviderListSimple = class(TPathDetalizeProviderListBase)
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig;
      AVectorDataFactory: IVectorDataFactory;
      AFactory: IVectorItmesFactory;
      AKmlLoader: IVectorDataLoader
    );
  end;

implementation

uses
  i_PathDetalizeProviderList,
  u_PathDetalizeProviderYourNavigation,
  u_PathDetalizeProviderMailRu,
  u_PathDetalizeProviderCloudMade;

{ TPathDetalizeProviderListSimple }

constructor TPathDetalizeProviderListSimple.Create(
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig;
  AVectorDataFactory: IVectorDataFactory;
  AFactory: IVectorItmesFactory;
  AKmlLoader: IVectorDataLoader
);
var
  VEntity: IPathDetalizeProviderListEntity;
begin
  inherited Create;
  VEntity := TPathDetalizeProviderMailRuShortest.Create(ALanguageManager, AProxyConfig, AFactory);
  Add(VEntity);
  VEntity := TPathDetalizeProviderMailRuFastest.Create(ALanguageManager, AProxyConfig, AFactory);
  Add(VEntity);
  VEntity := TPathDetalizeProviderMailRuFastestWithTraffic.Create(ALanguageManager, AProxyConfig, AFactory);
  Add(VEntity);
  VEntity := TPathDetalizeProviderYourNavigationFastestByCar.Create(ALanguageManager, AProxyConfig, AVectorDataFactory, AFactory, AKmlLoader);
  Add(VEntity);
  VEntity := TPathDetalizeProviderYourNavigationShortestByCar.Create(ALanguageManager, AProxyConfig, AVectorDataFactory, AFactory, AKmlLoader);
  Add(VEntity);
  VEntity := TPathDetalizeProviderYourNavigationFastestByBicycle.Create(ALanguageManager, AProxyConfig, AVectorDataFactory, AFactory, AKmlLoader);
  Add(VEntity);
  VEntity := TPathDetalizeProviderYourNavigationShortestByBicycle.Create(ALanguageManager, AProxyConfig, AVectorDataFactory, AFactory, AKmlLoader);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeFastestByCar.Create(ALanguageManager, AProxyConfig, AFactory);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeFastestByFoot.Create(ALanguageManager, AProxyConfig, AFactory);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeFastestByBicycle.Create(ALanguageManager, AProxyConfig, AFactory);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeShortestByCar.Create(ALanguageManager, AProxyConfig, AFactory);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeShortestByFoot.Create(ALanguageManager, AProxyConfig, AFactory);
  Add(VEntity);
  VEntity := TPathDetalizeProviderCloudMadeShortestByBicycle.Create(ALanguageManager, AProxyConfig, AFactory);
  Add(VEntity);
end;

end.

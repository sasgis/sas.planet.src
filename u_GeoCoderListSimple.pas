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

unit u_GeoCoderListSimple;

interface

uses
  i_ProxySettings,
  u_GeoCoderListBase;

type
  TGeoCoderListSimple = class(TGeoCoderListBase)
  public
    constructor Create(const AProxy: IProxySettings);
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
  u_GeoCoderByURL;

{ TGeoCoderListSimple }

constructor TGeoCoderListSimple.Create(const AProxy: IProxySettings);
var
  VItem: IGeoCoderListEntity;
begin
  inherited Create;
  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderGoogleGUID,
      'Google',
      TGeoCoderByGoogle.Create(AProxy)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderYandexGUID,
      'Yandex',
      TGeoCoderByYandex.Create(AProxy)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoder2GISGUID,
      '2GIS',
      TGeoCoderBy2GIS.Create(AProxy)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderOSMGUID,
      'OSM',
      TGeoCoderByOSM.Create(AProxy)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderWikiMapiaGUID,
      'WikiMapia',
      TGeoCoderByWikiMapia.Create(AProxy)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderRosreestrGUID,
      'Rosreestr',
      TGeoCoderByRosreestr.Create(AProxy)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderNavitelGUID,
      'Navitel',
      TGeoCoderByNavitel.Create(AProxy)
    );
  Add(VItem);

  VItem :=
    TGeoCoderListEntity.Create(
      CGeoCoderURLGUID,
      'Link and URL',
      TGeoCoderByURL.Create(AProxy)
    );
  Add(VItem);


end;

end.

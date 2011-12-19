{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_PathDetalizeProviderYourNavigation;

interface

uses
  t_GeoTypes,
  i_VectorDataLoader,
  i_LanguageManager,
  i_ProxySettings,
  u_PathDetalizeProviderListEntity;

type
  TPathDetalizeProviderYourNavigation = class(TPathDetalizeProviderListEntity)
  private
    FBaseUrl: string;
    FProxyConfig: IProxyConfig;
    FKmlLoader: IVectorDataLoader;
  protected { IPathDetalizeProvider }
    function GetPath(ASource: TArrayOfDoublePoint; var AComment: string): TArrayOfDoublePoint; override;
  public
    constructor Create(
      AGUID: TGUID;
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig;
      AKmlLoader: IVectorDataLoader;
      ABaseUrl: string
    );
  end;

type
  TPathDetalizeProviderYourNavigationFastestByCar = class(TPathDetalizeProviderYourNavigation)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig;
      AKmlLoader: IVectorDataLoader
    );
  end;

type
  TPathDetalizeProviderYourNavigationShortestByCar = class(TPathDetalizeProviderYourNavigation)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig;
      AKmlLoader: IVectorDataLoader
    );
  end;

type
  TPathDetalizeProviderYourNavigationFastestByBicycle = class(TPathDetalizeProviderYourNavigation)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig;
      AKmlLoader: IVectorDataLoader
    );
  end;

type
  TPathDetalizeProviderYourNavigationShortestByBicycle = class(TPathDetalizeProviderYourNavigation)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig;
      AKmlLoader: IVectorDataLoader
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  gnugettext,
  c_PathDetalizeProvidersGUID,
  u_GeoToStr,
  i_VectorDataItemSimple,
  u_InetFunc;

{ TPathDetalizeProviderYourNavigation }

constructor TPathDetalizeProviderYourNavigation.Create(
  AGUID: TGUID;
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig;
  AKmlLoader: IVectorDataLoader;
  ABaseUrl: string
);
begin
  inherited Create(AGUID, ALanguageManager);
  FBaseUrl := ABaseUrl;
  FProxyConfig := AProxyConfig;
  FKmlLoader := AKmlLoader;
end;

function TPathDetalizeProviderYourNavigation.GetPath(ASource: TArrayOfDoublePoint;
  var AComment: string): TArrayOfDoublePoint;
var
  ms:TMemoryStream;
  url:string;
  i:integer;
  kml:IVectorDataItemList;
  s,VPointsCount:integer;
  conerr:boolean;
  add_line_arr_b:TArrayOfDoublePoint;
  VItem: IVectorDataItemLine;
  VPoints: TArrayOfDoublePoint;
begin
  AComment := '';
  ms:=TMemoryStream.Create;
  try
    url := FBaseUrl;
    conerr:=false;
    for i:= 0 to length(ASource)-2 do begin
      if conerr then Continue;
      url:=url+'&flat='+R2StrPoint(ASource[i].y)+'&flon='+R2StrPoint(ASource[i].x)+
          '&tlat='+R2StrPoint(ASource[i+1].y)+'&tlon='+R2StrPoint(ASource[i+1].x);
      if GetStreamFromURL(ms, url, 'text/xml', FProxyConfig.GetStatic)>0 then begin
        FKmlLoader.LoadFromStream(ms, kml);
        if kml <> nil then begin
          ms.SetSize(0);
          if kml.Count > 0 then begin
            if Supports(kml.GetItem(0), IVectorDataItemLine, VItem) then begin
              VPoints := VItem.Points;
              VPointsCount := Length(VPoints);
              if VPointsCount > 0 then begin
                s := Length(add_line_arr_b);
                SetLength(add_line_arr_b, (s + VPointsCount));
                Move(VPoints[0], add_line_arr_b[s], VPointsCount * sizeof(TDoublePoint));
              end;
            end;
          end;
        end;
      end else begin
        conerr:=true;
      end;
    end;
  finally
    ms.Free;
  end;
  if not conerr then begin
    Result := add_line_arr_b;
  end;
end;

{ TPathDetalizeProviderYourNavigationFastestByCar }

constructor TPathDetalizeProviderYourNavigationFastestByCar.Create(
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig;
  AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(
    CPathDetalizeProviderYourNavigationFastestByCar,
    ALanguageManager,
    AProxyConfig,
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=1&layer=mapnik'
  );
end;

function TPathDetalizeProviderYourNavigationFastestByCar.GetCaptionTranslated: string;
begin
  Result := _('By car (Fastest) with yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationFastestByCar.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route by car (Fastest) with yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationFastestByCar.GetMenuItemNameTranslated: string;
begin
  Result := _('yournavigation.org (OSM)') + '|0030~\' +  _('By Car (Fastest)') + '|0010';
end;

{ TPathDetalizeProviderYourNavigationShortestByCar }

constructor TPathDetalizeProviderYourNavigationShortestByCar.Create(
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig;
  AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(
    CPathDetalizeProviderYourNavigationShortestByCar,
    ALanguageManager,
    AProxyConfig,
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=motorcar&fast=0&layer=mapnik'
  );
end;

function TPathDetalizeProviderYourNavigationShortestByCar.GetCaptionTranslated: string;
begin
  Result := _('By car (Shortest) with yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationShortestByCar.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route by car (Shortest) with yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationShortestByCar.GetMenuItemNameTranslated: string;
begin
  Result := _('yournavigation.org (OSM)') + '|0030~\' +  _('By Car (Shortest)') + '|0020';
end;

{ TPathDetalizeProviderYourNavigationFastestByBicycle }

constructor TPathDetalizeProviderYourNavigationFastestByBicycle.Create(
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig;
  AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(
    CPathDetalizeProviderYourNavigationFastestByBicycle,
    ALanguageManager,
    AProxyConfig,
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=1&layer=mapnik'
  );
end;

function TPathDetalizeProviderYourNavigationFastestByBicycle.GetCaptionTranslated: string;
begin
  Result := _('By bicycle (Fastest) with yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationFastestByBicycle.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route by bicycle (Fastest) with yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationFastestByBicycle.GetMenuItemNameTranslated: string;
begin
  Result := _('yournavigation.org (OSM)') + '|0030~\' +  _('By Bicycle (Fastest)') + '|0030';
end;

{ TPathDetalizeProviderYourNavigationShortestByBicycle }

constructor TPathDetalizeProviderYourNavigationShortestByBicycle.Create(
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig;
  AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(
    CPathDetalizeProviderYourNavigationShortestByBicycle,
    ALanguageManager,
    AProxyConfig,
    AKmlLoader,
    'http://www.yournavigation.org/api/1.0/gosmore.php?format=kml&v=bicycle&fast=0&layer=mapnik'
  );
end;

function TPathDetalizeProviderYourNavigationShortestByBicycle.GetCaptionTranslated: string;
begin
  Result := _('By bicycle (Shortest) with yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationShortestByBicycle.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route by bicycle (Shortest) with yournavigation.org');
end;

function TPathDetalizeProviderYourNavigationShortestByBicycle.GetMenuItemNameTranslated: string;
begin
  Result := _('yournavigation.org (OSM)') + '|0030~\' +  _('By Bicycle (Shortest)') + '|0040';
end;

end.

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

unit u_PathDetalizeProviderYourNavigation;

interface

uses
  t_GeoTypes,
  i_OperationNotifier,
  i_VectorDataLoader,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_VectorDataFactory,
  i_ProxySettings,
  i_PathDetalizeProvider;

type
  TPathDetalizeProviderYourNavigation = class(TInterfacedObject, IPathDetalizeProvider)
  private
    FFactory: IVectorItmesFactory;
    FVectorDataFactory: IVectorDataFactory;
    FBaseUrl: string;
    FProxyConfig: IProxyConfig;
    FKmlLoader: IVectorDataLoader;
  protected { IPathDetalizeProvider }
    function GetPath(
      const ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      const ASource: ILonLatPath;
      var AComment: string
    ): ILonLatPath;
  public
    constructor Create(
      const AProxyConfig: IProxyConfig;
      const AVectorDataFactory: IVectorDataFactory;
      const AFactory: IVectorItmesFactory;
      const AKmlLoader: IVectorDataLoader;
      const ABaseUrl: string
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_GeoToStr,
  i_VectorDataItemSimple,
  u_InetFunc;

{ TPathDetalizeProviderYourNavigation }

constructor TPathDetalizeProviderYourNavigation.Create(
  const AProxyConfig: IProxyConfig;
  const AVectorDataFactory: IVectorDataFactory;
  const AFactory: IVectorItmesFactory;
  const AKmlLoader: IVectorDataLoader;
  const ABaseUrl: string
);
begin
  inherited Create;
  FBaseUrl := ABaseUrl;
  FProxyConfig := AProxyConfig;
  FVectorDataFactory := AVectorDataFactory;
  FFactory := AFactory;
  FKmlLoader := AKmlLoader;
end;

function TPathDetalizeProviderYourNavigation.GetPath(
  const ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  const ASource: ILonLatPath;
  var AComment: string
): ILonLatPath;
var
  ms: TMemoryStream;
  url: string;
  kml: IVectorDataItemList;
  conerr: boolean;
  VPointsAggregator: IDoublePointsAggregator;
  VItem: IVectorDataItemLine;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VEnum: IEnumLonLatPoint;
  VLine: ILonLatPathLine;
begin
  Result := nil;
  AComment := '';
  ms := TMemoryStream.Create;
  try
    url := FBaseUrl;
    conerr := false;
    VPointsAggregator := TDoublePointsAggregator.Create;
    VEnum := ASource.GetEnum;
    if VEnum.Next(VPrevPoint) then begin
      while VEnum.Next(VCurrPoint) do begin
        if conerr then begin
          Continue;
        end;
        url := url + '&flat=' + R2StrPoint(VPrevPoint.y) + '&flon=' + R2StrPoint(VPrevPoint.x) +
          '&tlat=' + R2StrPoint(VCurrPoint.y) + '&tlon=' + R2StrPoint(VCurrPoint.x);
        if GetStreamFromURL(ms, url, 'text/xml', FProxyConfig.GetStatic) > 0 then begin
          kml := FKmlLoader.LoadFromStream(ms, FVectorDataFactory);
          if kml <> nil then begin
            ms.SetSize(0);
            if kml.Count > 0 then begin
              if Supports(kml.GetItem(0), IVectorDataItemLine, VItem) then begin
                if VItem.Line.Count > 0 then begin
                  VLine := VItem.Line.Item[0];
                  if VLine.Count > 0 then begin
                    VPointsAggregator.AddPoints(VLine.Points, VLine.Count);
                  end;
                end;
              end;
            end;
          end;
        end else begin
          conerr := true;
        end;
        VPrevPoint := VCurrPoint;
      end;
    end;
  finally
    ms.Free;
  end;
  if not conerr then begin
    Result := FFactory.CreateLonLatPath(VPointsAggregator.Points, VPointsAggregator.Count);
  end;
end;

end.

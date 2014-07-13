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

unit u_PathDetalizeProviderYourNavigation;

interface

uses
  t_GeoTypes,
  i_NotifierOperation,
  i_InetConfig,
  i_Downloader,
  i_VectorDataLoader,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_VectorDataFactory,
  i_PathDetalizeProvider,
  u_BaseInterfacedObject;

type
  TPathDetalizeProviderYourNavigation = class(TBaseInterfacedObject, IPathDetalizeProvider)
  private
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
    FBaseUrl: string;
    FKmlLoader: IVectorDataLoader;
    FDownloader: IDownloader;
    FInetConfig: IInetConfig;
  private
    function GetPath(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASource: IGeometryLonLatLine;
      var AComment: string
    ): IGeometryLonLatLine;
  public
    constructor Create(
      const AInetConfig: IInetConfig;
      const ADownloader: IDownloader;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AKmlLoader: IVectorDataLoader;
      const ABaseUrl: string
    );
  end;

implementation

uses
  SysUtils,
  i_DownloadRequest,
  i_DownloadResult,
  i_EnumDoublePoint,
  i_VectorItemSubset,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_DownloadRequest,
  u_GeoToStrFunc;

{ TPathDetalizeProviderYourNavigation }

constructor TPathDetalizeProviderYourNavigation.Create(
  const AInetConfig: IInetConfig;
  const ADownloader: IDownloader;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AKmlLoader: IVectorDataLoader;
  const ABaseUrl: string
);
begin
  inherited Create;
  FBaseUrl := ABaseUrl;
  FDownloader := ADownloader;
  FInetConfig := AInetConfig;
  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FKmlLoader := AKmlLoader;
end;

function TPathDetalizeProviderYourNavigation.GetPath(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASource: IGeometryLonLatLine;
  var AComment: string
): IGeometryLonLatLine;
var
  url: string;
  kml: IVectorItemSubset;
  conerr: boolean;
  VPointsAggregator: IDoublePointsAggregator;
  VMultiLine: IGeometryLonLatMultiLine;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VEnum: IEnumLonLatPoint;
  VLine: IGeometryLonLatSingleLine;
  VRequest: IDownloadRequest;
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
begin
  Result := nil;
  AComment := '';
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
      VRequest := TDownloadRequest.Create(url, '', FInetConfig.GetStatic);
      VResult := FDownloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
      if Supports(VResult, IDownloadResultOk, VResultOk) then begin
        kml := FKmlLoader.Load(VResultOk.Data, nil, FVectorDataItemMainInfoFactory);
        if kml <> nil then begin
          if kml.Count > 0 then begin
            if Supports(kml.GetItem(0).Geometry, IGeometryLonLatMultiLine, VMultiLine) then begin
              if VMultiLine.Count > 0 then begin
                VLine := VMultiLine.Item[0];
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
  if not conerr then begin
    Result := FVectorGeometryLonLatFactory.CreateLonLatMultiLine(VPointsAggregator.Points, VPointsAggregator.Count);
  end;
end;

end.

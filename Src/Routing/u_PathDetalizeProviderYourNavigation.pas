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
  i_DoublePointsAggregator,
  u_PathDetalizeProviderBase;

type
  TPathDetalizeProviderYourNavigation = class(TPathDetalizeProviderBase)
  private
    FKmlLoader: IVectorDataLoader;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  protected
    function ProcessSinglePath(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ASource: IGeometryLonLatSingleLine;
      const APointsAggregator: IDoublePointsAggregator;
      const ABuilder: IGeometryLonLatLineBuilder
    ): Boolean; override;
  public
    constructor Create(
      const ABaseUrl: AnsiString;
      const ADownloader: IDownloader;
      const AInetConfig: IInetConfig;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AKmlLoader: IVectorDataLoader
    );
  end;

implementation

uses
  SysUtils,
  i_DownloadRequest,
  i_DownloadResult,
  i_EnumDoublePoint,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  u_DownloadRequest,
  u_GeoToStrFunc;

{ TPathDetalizeProviderYourNavigation }

constructor TPathDetalizeProviderYourNavigation.Create(
  const ABaseUrl: AnsiString;
  const ADownloader: IDownloader;
  const AInetConfig: IInetConfig;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AKmlLoader: IVectorDataLoader
);
begin
  inherited Create(
    ABaseUrl,
    ADownloader,
    AInetConfig,
    AVectorGeometryLonLatFactory
  );

  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;
  FKmlLoader := AKmlLoader;
end;

function TPathDetalizeProviderYourNavigation.ProcessSinglePath(
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const ASource: IGeometryLonLatSingleLine;
  const APointsAggregator: IDoublePointsAggregator;
  const ABuilder: IGeometryLonLatLineBuilder
): Boolean;
var
  url: AnsiString;
  kml: IVectorItemSubset;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VEnum: IEnumLonLatPoint;
  VRequest: IDownloadRequest;
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
  VItem: IVectorDataItem;
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
  VContext: TVectorLoadContext;
begin
  Result := True;
  VEnum := ASource.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Result := false;
        exit;
      end;
      url := FBaseUrl + '&flat=' + R2AnsiStrPoint(VPrevPoint.y) + '&flon=' + R2AnsiStrPoint(VPrevPoint.x) +
        '&tlat=' + R2AnsiStrPoint(VCurrPoint.y) + '&tlon=' + R2AnsiStrPoint(VCurrPoint.x);
      VRequest := TDownloadRequest.Create(url, '', FInetConfig.GetStatic);
      VResult := FDownloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Result := false;
        exit;
      end;
      if Supports(VResult, IDownloadResultOk, VResultOk) then begin
        VContext.MainInfoFactory := FVectorDataItemMainInfoFactory;
        kml := FKmlLoader.Load(VContext, VResultOk.Data);
        if kml <> nil then begin
          if kml.Count > 0 then begin
            VItem := kml.GetItem(0);
            if not Supports(VItem.Geometry, IGeometryLonLatSingleLine, VSingleLine) then begin
              VSingleLine := nil;
              if Supports(VItem.Geometry, IGeometryLonLatMultiLine, VMultiLine) then begin
                if VMultiLine.Count > 0 then begin
                  VSingleLine := VMultiLine.Item[0];
                end;
              end;
            end;
            if Assigned(VSingleLine) and (VSingleLine.Count > 0) then begin
              APointsAggregator.AddPoints(VSingleLine.Points, VSingleLine.Count);
            end;
          end;
        end;
      end else begin
        Result := False;
        exit;
      end;
      VPrevPoint := VCurrPoint;
    end;
  end;
  if APointsAggregator.Count > 0 then begin
    ABuilder.AddLine(APointsAggregator.MakeStaticAndClear);
  end;
end;

end.

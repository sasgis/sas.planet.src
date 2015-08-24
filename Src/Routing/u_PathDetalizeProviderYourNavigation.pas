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
    FBaseUrl: AnsiString;
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
      const ABaseUrl: AnsiString
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
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_DownloadRequest,
  u_GeoFunc,
  u_GeoToStrFunc;

{ TPathDetalizeProviderYourNavigation }

constructor TPathDetalizeProviderYourNavigation.Create(
  const AInetConfig: IInetConfig;
  const ADownloader: IDownloader;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AKmlLoader: IVectorDataLoader;
  const ABaseUrl: AnsiString
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

function ProcessSinglePath(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASource: IGeometryLonLatSingleLine;
  const APointsAggregator: IDoublePointsAggregator;
  const ABuilder: IGeometryLonLatLineBuilder;
  const ABaseUrl: AnsiString;
  const ADownloader: IDownloader;
  const AInetConfig: IInetConfig;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AKmlLoader: IVectorDataLoader
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
begin
  Result := True;
  VEnum := ASource.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Result := false;
        exit;
      end;
      url := ABaseUrl + '&flat=' + R2AnsiStrPoint(VPrevPoint.y) + '&flon=' + R2AnsiStrPoint(VPrevPoint.x) +
        '&tlat=' + R2AnsiStrPoint(VCurrPoint.y) + '&tlon=' + R2AnsiStrPoint(VCurrPoint.x);
      VRequest := TDownloadRequest.Create(url, '', AInetConfig.GetStatic);
      VResult := ADownloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Result := false;
        exit;
      end;
      if Supports(VResult, IDownloadResultOk, VResultOk) then begin
        kml := AKmlLoader.Load(VResultOk.Data, nil, AVectorDataItemMainInfoFactory);
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

function TPathDetalizeProviderYourNavigation.GetPath(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASource: IGeometryLonLatLine;
  var AComment: string
): IGeometryLonLatLine;
var
  conerr: boolean;
  VPointsAggregator: IDoublePointsAggregator;
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
  VBuilder: IGeometryLonLatLineBuilder;
  i: Integer;
begin
  Result := nil;
  AComment := '';
  conerr := false;
  VPointsAggregator := TDoublePointsAggregator.Create;
  VBuilder := FVectorGeometryLonLatFactory.MakeLineBuilder;
  if Supports(ASource, IGeometryLonLatSingleLine, VSingleLine) then begin
    conerr :=
      not ProcessSinglePath(
        ACancelNotifier,
        AOperationID,
        VSingleLine,
        VPointsAggregator,
        VBuilder,
        FBaseUrl,
        FDownloader,
        FInetConfig,
        FVectorDataItemMainInfoFactory,
        FKmlLoader
      );
  end else if Supports(ASource, IGeometryLonLatMultiLine, VMultiLine) then begin
    for i := 0 to VMultiLine.Count - 1 do begin
      VSingleLine := VMultiLine.Item[i];
      conerr :=
        not ProcessSinglePath(
          ACancelNotifier,
          AOperationID,
          VSingleLine,
          VPointsAggregator,
          VBuilder,
          FBaseUrl,
          FDownloader,
          FInetConfig,
          FVectorDataItemMainInfoFactory,
          FKmlLoader
        );
      if conerr then break;

      VPointsAggregator.Add(CEmptyDoublePoint);
    end;
  end else begin
    Assert(false);
  end;

  if not conerr then begin
    Result := VBuilder.MakeStaticAndClear;
  end;
end;

end.

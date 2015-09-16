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

unit u_PathDetalizeProviderCloudMade;

interface

uses
  StrUtils,
  SysUtils,
  DateUtils,
  t_GeoTypes,
  i_NotifierOperation,
  i_InetConfig,
  i_Downloader,
  i_DoublePointsAggregator,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_PathDetalizeProvider,
  u_BaseInterfacedObject;

type
  TRouteVehicle = (car, foot, bicycle);
  TRouteCalcType = (fastest, shortest);

type
  TPathDetalizeProviderCloudMade = class(TBaseInterfacedObject, IPathDetalizeProvider)
  private
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FBaseUrl: AnsiString;
    FVehicle: TRouteVehicle;
    FRouteCalcType: TRouteCalcType;
    FDownloader: IDownloader;
    FInetConfig: IInetConfig;
    function ProcessSinglePath(
      const ASource: IGeometryLonLatSingleLine;
      const APointsAggregator: IDoublePointsAggregator;
      const ABuilder: IGeometryLonLatLineBuilder;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      var seconds: Integer;
      var meters: Integer
    ): Boolean;
  private
    function SecondToTime(const Seconds: Cardinal): Double;
  private { IPathDetalizeProvider }
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
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      AVehicle: TRouteVehicle;
      ARouteCalcType: TRouteCalcType
    );
  end;

implementation

uses
  i_DownloadRequest,
  i_DownloadResult,
  i_EnumDoublePoint,
  u_DoublePointsAggregator,
  u_GeoToStrFunc,
  u_DownloadRequest,
  u_ResStrings;

{ TPathDetalizeProviderCloudMade }

constructor TPathDetalizeProviderCloudMade.Create(
  const AInetConfig: IInetConfig;
  const ADownloader: IDownloader;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  AVehicle: TRouteVehicle;
  ARouteCalcType: TRouteCalcType
);
begin
  inherited Create;
  FBaseUrl := 'http://routes.cloudmade.com/BC9A493B41014CAABB98F0471D759707/api/0.3/';
  FVehicle := AVehicle;
  FRouteCalcType := ARouteCalcType;
  FInetConfig := AInetConfig;
  FDownloader := ADownloader;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
end;

function TPathDetalizeProviderCloudMade.ProcessSinglePath(
  const ASource: IGeometryLonLatSingleLine;
  const APointsAggregator: IDoublePointsAggregator;
  const ABuilder: IGeometryLonLatLineBuilder;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  var seconds: Integer;
  var meters: Integer
): Boolean;
var
  VPoint: TDoublePoint;
  VEnum: IEnumLonLatPoint;
  VPrevPoint: TDoublePoint;
  VResultOk: IDownloadResultOk;
  posit: Integer;
  url: string;
  VCurrPoint: TDoublePoint;
  posit2: Integer;
  VResult: IDownloadResult;
  pathstr: string;
  VRequest: IDownloadRequest;
begin
  Result := true;
  VEnum := ASource.GetEnum;
  if VEnum.Next(VPrevPoint) then
  begin
    while VEnum.Next(VCurrPoint) do
    begin
      if ACancelNotifier.IsOperationCanceled(AOperationID) then
      begin
        Break;
      end;
      url := FBaseUrl;
      url := url + R2AnsiStrPoint(VPrevPoint.y) + ',' + R2AnsiStrPoint(VPrevPoint.x) + ',' + R2AnsiStrPoint(VCurrPoint.y) + ',' + R2AnsiStrPoint(VCurrPoint.x);
      case FVehicle of
        car:
          begin
            url := url + '/car';
          end;
        foot:
          begin
            url := url + '/foot';
          end;
        bicycle:
          begin
            url := url + '/bicycle';
          end;
      end;
      case FRouteCalcType of
        fastest:
          begin
            url := url + '.js?units=km&lang=en&callback=getRoute6&translation=common';
          end;
        shortest:
          begin
            url := url + '/shortest.js?units=km&lang=en&callback=getRoute6&translation=common';
          end;
      end;
      VRequest := TDownloadRequest.Create(url, '', FInetConfig.GetStatic);
      VResult := FDownloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
      if Supports(VResult, IDownloadResultOk, VResultOk) then
      begin
        SetLength(pathstr, VResultOk.Data.Size);
        Move(VResultOk.Data.Buffer^, pathstr[1], VResultOk.Data.Size);
        try
          posit := PosEx('[', pathstr, 1);
          posit := PosEx('[', pathstr, posit + 1);
          if posit > 0 then
          begin
            while (posit > 0) do
            begin
              posit2 := PosEx(',', pathstr, posit);
              VPoint.Y := str2r(copy(pathstr, posit + 1, posit2 - (posit + 1)));
              posit := PosEx(']', pathstr, posit2);
              VPoint.X := str2r(copy(pathstr, posit2 + 1, posit - (posit2 + 1)));
              APointsAggregator.Add(VPoint);
              if pathstr[posit + 1] = ']' then
              begin
                posit := -1;
              end
              else
              begin
                posit := PosEx('[', pathstr, posit + 1);
              end;
            end;
            posit := PosEx('"total_distance":', pathstr, 1);
            posit2 := PosEx(',', pathstr, posit);
            meters := meters + strtoint(copy(pathstr, posit + 17, posit2 - (posit + 17)));
            posit := PosEx('"total_time":', pathstr, 1);
            posit2 := PosEx(',', pathstr, posit);
            seconds := seconds + strtoint(copy(pathstr, posit + 13, posit2 - (posit + 13)));
          end;
        except
        end;
      end
      else
      begin
        Result := False;
        break;
      end;
      VPrevPoint := VCurrPoint;
    end;
  end;
  if APointsAggregator.Count > 0 then begin
    ABuilder.AddLine(APointsAggregator.MakeStaticAndClear);
  end;
end;

function TPathDetalizeProviderCloudMade.GetPath(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASource: IGeometryLonLatLine;
  var AComment: string
): IGeometryLonLatLine;
var
  conerr: boolean;
  VPointsAggregator: IDoublePointsAggregator;
  timeT1: string;
  dd, seconds, meters: integer;
  VDateT1: TDateTime;
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
  VBuilder: IGeometryLonLatLineBuilder;
  i: integer;
begin
  Result := nil;
  AComment := '';
  meters := 0;
  seconds := 0;
  VPointsAggregator := TDoublePointsAggregator.Create;
  VBuilder := FVectorGeometryLonLatFactory.MakeLineBuilder;
  if Supports(ASource, IGeometryLonLatSingleLine, VSingleLine) then begin
    conerr :=
      ProcessSinglePath(
        VSingleLine,
        VPointsAggregator,
        VBuilder,
        ACancelNotifier,
        AOperationID,
        seconds,
        meters
      );
    if not conerr then begin
      Exit;
    end;
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
  end else if Supports(ASource, IGeometryLonLatMultiLine, VMultiLine) then begin
    for i := 0 to VMultiLine.Count - 1 do begin
      VSingleLine := VMultiLine.Item[i];
      conerr :=
        ProcessSinglePath(
          VSingleLine,
          VPointsAggregator,
          VBuilder,
          ACancelNotifier,
          AOperationID,
          seconds,
          meters
        );
      if not conerr then begin
        Exit;
      end;
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
    end;
  end;
  if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Result := VBuilder.MakeStaticAndClear;
    if meters > 1000 then begin
      AComment := SAS_STR_MarshLen + ' ' + RoundEx(meters / 1000, 2) + ' ' + SAS_UNITS_km;
    end else begin
      AComment := SAS_STR_MarshLen + ' ' + inttostr(meters) + ' ' + SAS_UNITS_m;
    end;
    VDateT1 := SecondToTime(seconds);
    dd := DaysBetween(0, VDateT1);
    timeT1 := '';
    if dd > 0 then begin
      timeT1 := inttostr(dd) + ' дней, ';
    end;
    timeT1 := timeT1 + TimeToStr(VDateT1);
    AComment := AComment + #13#10 + SAS_STR_Marshtime + timeT1;
  end;
end;

function TPathDetalizeProviderCloudMade.SecondToTime(const Seconds: Cardinal): Double;
const
  SecPerDay = 86400;
  SecPerHour = 3600;
  SecPerMinute = 60;
var
  ms, ss, mm, hh, dd: Cardinal;
begin
  dd := Seconds div SecPerDay;
  hh := (Seconds mod SecPerDay) div SecPerHour;
  mm := ((Seconds mod SecPerDay) mod SecPerHour) div SecPerMinute;
  ss := ((Seconds mod SecPerDay) mod SecPerHour) mod SecPerMinute;
  ms := 0;
  Result := dd + EncodeTime(hh, mm, ss, ms);
end;

end.

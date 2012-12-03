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
  i_VectorItemLonLat,
  i_VectorItemsFactory,
  i_PathDetalizeProvider,
  u_BaseInterfacedObject;

type
  TRouteVehicle = (car, foot, bicycle);
  TRouteCalcType = (fastest, shortest);

type
  TPathDetalizeProviderCloudMade = class(TBaseInterfacedObject, IPathDetalizeProvider)
  private
    FFactory: IVectorItemsFactory;
    FBaseUrl: string;
    FVehicle: TRouteVehicle;
    FRouteCalcType: TRouteCalcType;
    FDownloader: IDownloader;
    FInetConfig: IInetConfig;
  private
    function SecondToTime(const Seconds: Cardinal): Double;
  private { IPathDetalizeProvider }
    function GetPath(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASource: ILonLatPath;
      var AComment: string
    ): ILonLatPath;
  public
    constructor Create(
      const AInetConfig: IInetConfig;
      const ADownloader: IDownloader;
      const AFactory: IVectorItemsFactory;
      AVehicle: TRouteVehicle;
      ARouteCalcType: TRouteCalcType
    );
  end;

implementation

uses
  i_DownloadRequest,
  i_DownloadResult,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_GeoToStr,
  u_DownloadRequest,
  u_ResStrings;

{ TPathDetalizeProviderCloudMade }

constructor TPathDetalizeProviderCloudMade.Create(
  const AInetConfig: IInetConfig;
  const ADownloader: IDownloader;
  const AFactory: IVectorItemsFactory;
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
  FFactory := AFactory;
end;

function TPathDetalizeProviderCloudMade.GetPath(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASource: ILonLatPath;
  var AComment: string
): ILonLatPath;
var
  url: string;
  conerr: boolean;
  VPointsAggregator: IDoublePointsAggregator;
  VPoint: TDoublePoint;
  pathstr, timeT1: string;
  posit, posit2, dd, seconds, meters: integer;
  VDateT1: TDateTime;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VEnum: IEnumLonLatPoint;
  VRequest: IDownloadRequest;
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
begin
  Result := nil;
  AComment := '';
  meters := 0;
  seconds := 0;
  VPointsAggregator := TDoublePointsAggregator.Create;
  conerr := false;
  VEnum := ASource.GetEnum;
  if VEnum.Next(VPrevPoint) then begin
    while VEnum.Next(VCurrPoint) do begin
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Break;
      end;
      if conerr then begin
        Continue;
      end;
      url := FBaseUrl;
      url := url + R2StrPoint(VPrevPoint.y) + ',' + R2StrPoint(VPrevPoint.x) +
        ',' + R2StrPoint(VCurrPoint.y) + ',' + R2StrPoint(VCurrPoint.x);
      case FVehicle of
        car: begin
          url := url + '/car';
        end;
        foot: begin
          url := url + '/foot';
        end;
        bicycle: begin
          url := url + '/bicycle';
        end;
      end;
      case FRouteCalcType of
        fastest: begin
          url := url + '.js?units=km&lang=en&callback=getRoute6&translation=common';
        end;
        shortest: begin
          url := url + '/shortest.js?units=km&lang=en&callback=getRoute6&translation=common';
        end;
      end;
      VRequest := TDownloadRequest.Create(url, '', FInetConfig.GetStatic);
      VResult := FDownloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
      if Supports(VResult, IDownloadResultOk, VResultOk) then begin
        SetLength(pathstr, VResultOk.Data.Size);
        Move(VResultOk.Data.Buffer^, pathstr[1], VResultOk.Data.Size);
        try
          posit := PosEx('[', pathstr, 1);
          posit := PosEx('[', pathstr, posit + 1);
          if posit > 0 then begin
            While (posit > 0) do begin
              posit2 := PosEx(',', pathstr, posit);
              VPoint.Y := str2r(copy(pathstr, posit + 1, posit2 - (posit + 1)));
              posit := PosEx(']', pathstr, posit2);
              VPoint.X := str2r(copy(pathstr, posit2 + 1, posit - (posit2 + 1)));

              VPointsAggregator.Add(VPoint);
              if pathstr[posit + 1] = ']' then begin
                posit := -1;
              end else begin
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
      end else begin
        conerr := true;
      end;
      VPrevPoint := VCurrPoint;
    end;
  end;
  if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    if not conerr then begin
      Result := FFactory.CreateLonLatPath(VPointsAggregator.Points, VPointsAggregator.Count);
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

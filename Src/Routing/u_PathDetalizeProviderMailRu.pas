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

unit u_PathDetalizeProviderMailRu;

interface

uses
  t_GeoTypes,
  i_NotifierOperation,
  i_InetConfig,
  i_Downloader,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_PathDetalizeProvider,
  u_BaseInterfacedObject;

type
  TPathDetalizeProviderMailRu = class(TBaseInterfacedObject, IPathDetalizeProvider)
  private
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FBaseUrl: AnsiString;
    FDownloader: IDownloader;
    FInetConfig: IInetConfig;

    function SecondToTime(const Seconds: Cardinal): Double;
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
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const ABaseUrl: AnsiString
    );
  end;

implementation

uses
  Math,
  SysUtils,
  StrUtils,
  DateUtils,
  ALString,
  i_DownloadRequest,
  i_DownloadResult,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_GeoFunc,
  u_DownloadRequest,
  u_GeoToStrFunc,
  u_ResStrings;

{ TPathDetalizeProviderMailRu }

constructor TPathDetalizeProviderMailRu.Create(
  const AInetConfig: IInetConfig;
  const ADownloader: IDownloader;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const ABaseUrl: AnsiString
);
begin
  inherited Create;
  FBaseUrl := ABaseUrl;
  FDownloader := ADownloader;
  FInetConfig := AInetConfig;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
end;

function TPathDetalizeProviderMailRu.GetPath(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASource: IGeometryLonLatLine;
  var AComment: string
): IGeometryLonLatLine;
var
  pathstr, timeT1: string;
  url: AnsiString;
  i, posit, posit2, endpos, dd, seconds, meters: integer;
  VDateT1: TDateTime;
  VPoint: TDoublePoint;
  VEnum: IEnumLonLatPoint;
  VPointsAggregator: IDoublePointsAggregator;
  VRequest: IDownloadRequest;
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
begin
  Result := nil;
  url := FBaseUrl;
  if Supports(ASource, IGeometryLonLatSingleLine, VSingleLine) then begin
    VEnum := VSingleLine.GetEnum;
  end else if Supports(ASource, IGeometryLonLatMultiLine, VMultiLine) then begin
    VEnum := VMultiLine.Item[0].GetEnum;
  end else begin
    Assert(False);
  end;
  i := 0;
  while VEnum.Next(VPoint) do begin
    url := url + '&x' + ALIntToStr(i) + '=' + R2AnsiStrPoint(VPoint.x) + '&y' + ALIntToStr(i) + '=' + R2AnsiStrPoint(VPoint.y);
    Inc(i);
  end;
  VRequest := TDownloadRequest.Create(url, '', FInetConfig.GetStatic);
  VResult := FDownloader.DoRequest(VRequest, ACancelNotifier, AOperationID);
  if Supports(VResult, IDownloadResultOk, VResultOk) then begin
    SetLength(pathstr, VResultOk.Data.Size);
    Move(VResultOk.Data.Buffer^, pathstr[1], VResultOk.Data.Size);
    VPointsAggregator := TDoublePointsAggregator.Create;
    meters := 0;
    seconds := 0;

    try
      posit := PosEx('"totalLength"', pathstr, 1);
      While (posit > 0) do begin
        try
          posit2 := PosEx('"', pathstr, posit + 17);
          meters := meters + strtoint(copy(pathstr, posit + 17, posit2 - (posit + 17)));
          posit := PosEx('"totalTime"', pathstr, posit);
          posit2 := PosEx('"', pathstr, posit + 15);
          seconds := seconds + strtoint(copy(pathstr, posit + 15, posit2 - (posit + 15)));
        except
        end;
        posit := PosEx('"points"', pathstr, posit);
        endpos := PosEx(']', pathstr, posit);
        while (posit > 0) and (posit < endpos) do begin
          try
            posit := PosEx('"x" : "', pathstr, posit);
            posit2 := PosEx('", "y" : "', pathstr, posit);
            VPoint.X := str2r(copy(pathstr, posit + 7, posit2 - (posit + 7)));
            posit := PosEx('"', pathstr, posit2 + 10);
            VPoint.y := str2r(copy(pathstr, posit2 + 10, posit - (posit2 + 10)));
            posit := PosEx('{', pathstr, posit);
          except
            VPoint := CEmptyDoublePoint;
          end;
          if not PointIsEmpty(VPoint) then begin
            VPointsAggregator.Add(VPoint);
          end;
        end;
        posit := PosEx('"totalLength"', pathstr, posit);
      end;
    except
    end;
    Result := FVectorGeometryLonLatFactory.CreateLonLatLine(VPointsAggregator.Points, VPointsAggregator.Count);
    if meters > 1000 then begin
      AComment := SAS_STR_MarshLen + RoundEx(meters / 1000, 2) + ' ' + SAS_UNITS_km;
    end else begin
      AComment := SAS_STR_MarshLen + inttostr(meters) + ' ' + SAS_UNITS_m;
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

function TPathDetalizeProviderMailRu.SecondToTime(
  const Seconds: Cardinal): Double;
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

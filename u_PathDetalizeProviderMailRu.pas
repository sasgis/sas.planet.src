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

unit u_PathDetalizeProviderMailRu;

interface

uses
  t_GeoTypes,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_ProxySettings,
  i_PathDetalizeProvider;

type
  TPathDetalizeProviderMailRu = class(TInterfacedObject, IPathDetalizeProvider)
  private
    FFactory: IVectorItmesFactory;
    FBaseUrl: string;
    FProxyConfig: IProxyConfig;

    function SecondToTime(const Seconds: Cardinal): Double;
  protected { IPathDetalizeProvider }
    function GetPath(
      const ASource: ILonLatPath;
      var AComment: string
    ): ILonLatPath;
  public
    constructor Create(
      const AProxyConfig: IProxyConfig;
      const AFactory: IVectorItmesFactory;
      const ABaseUrl: string
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  StrUtils,
  DateUtils,
  gnugettext,
  c_PathDetalizeProvidersGUID,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_GeoFun,
  u_GeoToStr,
  u_ResStrings,
  u_InetFunc;

{ TPathDetalizeProviderMailRu }

constructor TPathDetalizeProviderMailRu.Create(
  const AProxyConfig: IProxyConfig;
  const AFactory: IVectorItmesFactory;
  const ABaseUrl: string
);
begin
  inherited Create;
  FBaseUrl := ABaseUrl;
  FProxyConfig := AProxyConfig;
  FFactory := AFactory;
end;

function TPathDetalizeProviderMailRu.GetPath(
  const ASource: ILonLatPath;
  var AComment: string
): ILonLatPath;
var
  ms: TMemoryStream;
  pathstr, timeT1: string;
  url: string;
  i, posit, posit2, endpos, dd, seconds, meters: integer;
  dateT1: TDateTime;
  VPoint: TDoublePoint;
  VEnum: IEnumLonLatPoint;
  VPointsAggregator: IDoublePointsAggregator;
begin
  Result := nil;
  url := FBaseUrl;
  if ASource.Count > 0 then begin
    VEnum := ASource.Item[0].GetEnum;
  end else begin
    VEnum := ASource.GetEnum;
  end;
  i := 0;
  while VEnum.Next(VPoint) do begin
    url := url + '&x' + inttostr(i) + '=' + R2StrPoint(VPoint.x) + '&y' + inttostr(i) + '=' + R2StrPoint(VPoint.y);
    Inc(i);
  end;
  ms := TMemoryStream.Create;
  try
    if GetStreamFromURL(ms, url, 'text/javascript; charset=utf-8', FProxyConfig.GetStatic) > 0 then begin
      ms.Position := 0;
      SetLength(pathstr, ms.Size);
      ms.ReadBuffer(pathstr[1], ms.Size);
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
      Result := FFactory.CreateLonLatPath(VPointsAggregator.Points, VPointsAggregator.Count);
      if meters > 1000 then begin
        AComment := SAS_STR_MarshLen + RoundEx(meters / 1000, 2) + ' ' + SAS_UNITS_km;
      end else begin
        AComment := SAS_STR_MarshLen + inttostr(meters) + ' ' + SAS_UNITS_m;
      end;
      DateT1 := SecondToTime(seconds);
      dd := DaysBetween(0, DateT1);
      timeT1 := '';
      if dd > 0 then begin
        timeT1 := inttostr(dd) + ' дней, ';
      end;
      timeT1 := timeT1 + TimeToStr(DateT1);
      AComment := AComment + #13#10 + SAS_STR_Marshtime + timeT1;
    end;
  finally
    ms.Free;
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

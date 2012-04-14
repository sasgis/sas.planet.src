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
  i_LanguageManager,
  i_ProxySettings,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  u_PathDetalizeProviderListEntity;

type
  TRouteVehicle = (car,foot,bicycle);
  TRouteCalcType = (fastest,shortest);

type
  TPathDetalizeProviderCloudMade = class(TPathDetalizeProviderListEntity)
  private
    FFactory: IVectorItmesFactory;
    FBaseUrl: string;
    FVehicle: TRouteVehicle;
    FRouteCalcType: TRouteCalcType;
    FProxyConfig: IProxyConfig;
  protected
    function SecondToTime(const Seconds: Cardinal): Double;
  protected { IPathDetalizeProvider }
    function GetPath(const ASource: ILonLatPath; var AComment: string): ILonLatPath; override;
  public
    constructor Create(
      const AGUID: TGUID;
      const ALanguageManager: ILanguageManager;
      const AProxyConfig: IProxyConfig;
      const AFactory: IVectorItmesFactory;
      AVehicle: TRouteVehicle;
      ARouteCalcType: TRouteCalcType
    );
  end;

  TPathDetalizeProviderCloudMadeFastestByCar = class(TPathDetalizeProviderCloudMade)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProxyConfig: IProxyConfig;
      const AFactory: IVectorItmesFactory
    );
  end;

  TPathDetalizeProviderCloudMadeFastestByFoot = class(TPathDetalizeProviderCloudMade)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProxyConfig: IProxyConfig;
      const AFactory: IVectorItmesFactory
    );
  end;

  TPathDetalizeProviderCloudMadeFastestByBicycle = class(TPathDetalizeProviderCloudMade)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProxyConfig: IProxyConfig;
      const AFactory: IVectorItmesFactory
    );
  end;

  TPathDetalizeProviderCloudMadeShortestByCar = class(TPathDetalizeProviderCloudMade)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProxyConfig: IProxyConfig;
      const AFactory: IVectorItmesFactory
    );
  end;

  TPathDetalizeProviderCloudMadeShortestByFoot = class(TPathDetalizeProviderCloudMade)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProxyConfig: IProxyConfig;
      const AFactory: IVectorItmesFactory
    );
  end;

  TPathDetalizeProviderCloudMadeShortestByBicycle = class(TPathDetalizeProviderCloudMade)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProxyConfig: IProxyConfig;
      const AFactory: IVectorItmesFactory
    );
  end;

implementation

uses
  Classes,
  gnugettext,
  c_PathDetalizeProvidersGUID,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_GeoToStr,
  u_ResStrings,
  u_InetFunc;

{ TPathDetalizeProviderCloudMade }

constructor TPathDetalizeProviderCloudMade.Create(
  const AGUID: TGUID;
  const ALanguageManager: ILanguageManager;
  const AProxyConfig: IProxyConfig;
  const AFactory: IVectorItmesFactory;
  AVehicle: TRouteVehicle;
  ARouteCalcType: TRouteCalcType
);
begin
  inherited Create(AGUID, ALanguageManager);
  FBaseUrl := 'http://routes.cloudmade.com/BC9A493B41014CAABB98F0471D759707/api/0.3/';
  FVehicle := AVehicle;
  FRouteCalcType := ARouteCalcType;
  FProxyConfig := AProxyConfig;
  FFactory := AFactory;
end;

function TPathDetalizeProviderCloudMade.GetPath(const ASource: ILonLatPath; var AComment: string): ILonLatPath;
var
  ms:TMemoryStream;
  url:string;
  conerr:boolean;
  VPointsAggregator: IDoublePointsAggregator;
  VPoint: TDoublePoint;
  pathstr,timeT1:string;
  posit,posit2,dd,seconds,meters:integer;
  dateT1:TDateTime;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VEnum: IEnumLonLatPoint;
begin
  Result := nil;
  AComment := '';
  meters:=0;
  seconds:=0;
  VPointsAggregator := TDoublePointsAggregator.Create;
  ms:=TMemoryStream.Create;
  try
    conerr:=false;
    VEnum := ASource.GetEnum;
    if VEnum.Next(VPrevPoint) then begin
      while VEnum.Next(VCurrPoint) do begin
        if conerr then Continue;
        url := FBaseUrl;
        url:=url+R2StrPoint(VPrevPoint.y)+','+R2StrPoint(VPrevPoint.x)+
            ','+R2StrPoint(VCurrPoint.y)+','+R2StrPoint(VCurrPoint.x);
        case FVehicle of
          car: url:=url+'/car';
          foot: url:=url+'/foot';
          bicycle: url:=url+'/bicycle';
        end;
        case FRouteCalcType of
          fastest: url:=url+'.js?units=km&lang=en&callback=getRoute6&translation=common';
          shortest: url:=url+'/shortest.js?units=km&lang=en&callback=getRoute6&translation=common';
        end;
        ms.Clear;
        if GetStreamFromURL(ms, url, 'application/json;charset=UTF-8', FProxyConfig.GetStatic)>0 then begin
          ms.Position:=0;
          SetLength(pathstr, ms.Size);
          ms.ReadBuffer(pathstr[1], ms.Size);
          try
            posit:=PosEx('[',pathstr,1);
            posit:=PosEx('[',pathstr,posit+1);
            if posit>0 then  begin
              While (posit>0) do begin
                posit2:=PosEx(',',pathstr,posit);
                VPoint.Y := str2r(copy(pathstr,posit+1,posit2-(posit+1)));
                posit:=PosEx(']',pathstr,posit2);
                VPoint.X := str2r(copy(pathstr,posit2+1,posit-(posit2+1)));

                VPointsAggregator.Add(VPoint);
                if pathstr[posit+1]=']' then begin
                  posit:=-1;
                end else begin
                  posit:=PosEx('[',pathstr,posit+1);
                end;
              end;
              posit:=PosEx('"total_distance":',pathstr,1);
              posit2:=PosEx(',',pathstr,posit);
              meters:=meters+strtoint(copy(pathstr,posit+17,posit2-(posit+17)));
              posit:=PosEx('"total_time":',pathstr,1);
              posit2:=PosEx(',',pathstr,posit);
              seconds:=seconds+strtoint(copy(pathstr,posit+13,posit2-(posit+13)));
            end;
          except
          end;
        end else begin
          conerr:=true;
        end;
        VPrevPoint := VCurrPoint;
      end;
    end;
  finally
    ms.Free;
  end;
  if not conerr then begin
    Result := FFactory.CreateLonLatPath(VPointsAggregator.Points, VPointsAggregator.Count);
    if meters>1000 then begin
      AComment:=SAS_STR_MarshLen+' '+RoundEx(meters/1000,2)+' '+SAS_UNITS_km;
    end else begin
      AComment:=SAS_STR_MarshLen+' '+inttostr(meters)+' '+SAS_UNITS_m;
    end;
    DateT1:=SecondToTime(seconds);
    dd:=DaysBetween(0,DateT1);
    timeT1:='';
    if dd>0 then begin
      timeT1:=inttostr(dd)+' дней, ';
    end;
    timeT1:=timeT1+TimeToStr(DateT1);
    AComment:=AComment+#13#10+SAS_STR_Marshtime+timeT1;
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

{ TPathDetalizeProviderCloudMadeFastestByCar }

constructor TPathDetalizeProviderCloudMadeFastestByCar.Create(
  const ALanguageManager: ILanguageManager;
  const AProxyConfig: IProxyConfig;
  const AFactory: IVectorItmesFactory
);
begin
  inherited Create(
    CPathDetalizeProviderCloudMadeFastestByCar,
    ALanguageManager,
    AProxyConfig,
    AFactory,
    car,
    fastest
  );
end;

function TPathDetalizeProviderCloudMadeFastestByCar.GetCaptionTranslated: string;
begin
  Result := _('By car (Fastest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByCar.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route by car (Fastest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByCar.GetMenuItemNameTranslated: string;
begin
  Result :=_('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Car (Fastest)') + '|0010';
end;

{ TPathDetalizeProviderCloudMadeFastestByFoot }

constructor TPathDetalizeProviderCloudMadeFastestByFoot.Create(
  const ALanguageManager: ILanguageManager;
  const AProxyConfig: IProxyConfig;
  const AFactory: IVectorItmesFactory
);
begin
  inherited Create(
    CPathDetalizeProviderCloudMadeFastestByFoot,
    ALanguageManager,
    AProxyConfig,
    AFactory,
    foot,
    fastest
  );
end;

function TPathDetalizeProviderCloudMadeFastestByFoot.GetCaptionTranslated: string;
begin
  Result := _('By foot (Fastest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByFoot.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route by foot (Fastest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByFoot.GetMenuItemNameTranslated: string;
begin
  Result := _('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Foot (Fastest)') + '|0020';
end;

{ TPathDetalizeProviderCloudMadeFastestByBicycle }

constructor TPathDetalizeProviderCloudMadeFastestByBicycle.Create(
  const ALanguageManager: ILanguageManager;
  const AProxyConfig: IProxyConfig;
  const AFactory: IVectorItmesFactory
);
begin
  inherited Create(
    CPathDetalizeProviderCloudMadeFastestByBicycle,
    ALanguageManager,
    AProxyConfig,
    AFactory,
    bicycle,
    fastest
  );
end;

function TPathDetalizeProviderCloudMadeFastestByBicycle.GetCaptionTranslated: string;
begin
  Result := _('By bicycle (Fastest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByBicycle.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route by bicycle (Fastest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByBicycle.GetMenuItemNameTranslated: string;
begin
  Result := _('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Bicycle (Fastest)') + '|0030';
end;

{ TPathDetalizeProviderCloudMadeShortestByCar }

constructor TPathDetalizeProviderCloudMadeShortestByCar.Create(
  const ALanguageManager: ILanguageManager;
  const AProxyConfig: IProxyConfig;
  const AFactory: IVectorItmesFactory
);
begin
  inherited Create(
    CPathDetalizeProviderCloudMadeShortestByCar,
    ALanguageManager,
    AProxyConfig,
    AFactory,
    car,
    shortest
  );
end;

function TPathDetalizeProviderCloudMadeShortestByCar.GetCaptionTranslated: string;
begin
  Result := _('By car (Shortest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByCar.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route by car (Shortest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByCar.GetMenuItemNameTranslated: string;
begin
  Result := _('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Car (Shortest)') + '|0040';
end;

{ TPathDetalizeProviderCloudMadeShortestByFoot }

constructor TPathDetalizeProviderCloudMadeShortestByFoot.Create(
  const ALanguageManager: ILanguageManager;
  const AProxyConfig: IProxyConfig;
  const AFactory: IVectorItmesFactory
);
begin
  inherited Create(
    CPathDetalizeProviderCloudMadeShortestByFoot,
    ALanguageManager,
    AProxyConfig,
    AFactory,
    foot,
    shortest
  );
end;

function TPathDetalizeProviderCloudMadeShortestByFoot.GetCaptionTranslated: string;
begin
  Result := _('By foot (Shortest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByFoot.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route by foot (Shortest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByFoot.GetMenuItemNameTranslated: string;
begin
  Result := _('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Foot (Shortest)') + '|0050';
end;

{ TPathDetalizeProviderCloudMadeShortestByBicycle }

constructor TPathDetalizeProviderCloudMadeShortestByBicycle.Create(
  const ALanguageManager: ILanguageManager;
  const AProxyConfig: IProxyConfig;
  const AFactory: IVectorItmesFactory
);
begin
  inherited Create(
    CPathDetalizeProviderCloudMadeShortestByBicycle,
    ALanguageManager,
    AProxyConfig,
    AFactory,
    bicycle,
    shortest
  );
end;

function TPathDetalizeProviderCloudMadeShortestByBicycle.GetCaptionTranslated: string;
begin
  Result := _('By bicycle (Shortest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByBicycle.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route by bicycle (Shortest) with cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByBicycle.GetMenuItemNameTranslated: string;
begin
  Result := _('maps.cloudmade.com (OSM)') + '|0010~\' + _('By Bicycle (Shortest)') + '|0060';
end;

end.

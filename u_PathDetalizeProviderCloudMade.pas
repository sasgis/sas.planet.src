unit u_PathDetalizeProviderCloudMade;

interface

uses
  StrUtils,
  SysUtils,
  DateUtils,
  t_GeoTypes,
  i_LanguageManager,
  u_UserInterfaceItemBase,
  i_PathDetalizeProvider;

type
  TRouteVehicle = (car,foot,bicycle);
  TRouteCalcType = (fastest,shortest);

type
  TPathDetalizeProviderCloudMade = class(TUserInterfaceItemBase, IPathDetalizeProvider)
  private
    FBaseUrl: string;
    FVehicle: TRouteVehicle;
    FRouteCalcType: TRouteCalcType;
  protected
    function GetPath(ASource: TArrayOfDoublePoint; var AComment: string): TArrayOfDoublePoint;
    function SecondToTime(const Seconds: Cardinal): Double;
  public
    constructor Create(
      AGUID: TGUID;
      ALanguageManager: ILanguageManager;
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
      ALanguageManager: ILanguageManager
    );
  end;

  TPathDetalizeProviderCloudMadeFastestByFoot = class(TPathDetalizeProviderCloudMade)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager
    );
  end;

  TPathDetalizeProviderCloudMadeFastestByBicycle = class(TPathDetalizeProviderCloudMade)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager
    );
  end;

  TPathDetalizeProviderCloudMadeShortestByCar = class(TPathDetalizeProviderCloudMade)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager
    );
  end;

  TPathDetalizeProviderCloudMadeShortestByFoot = class(TPathDetalizeProviderCloudMade)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager
    );
  end;

  TPathDetalizeProviderCloudMadeShortestByBicycle = class(TPathDetalizeProviderCloudMade)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  Classes,
  gnugettext,
  u_GeoToStr,
  u_ResStrings,
  i_VectorDataItemSimple,
  u_GlobalState,
  frm_InvisibleBrowser;

{ TPathDetalizeProviderCloudMade }

constructor TPathDetalizeProviderCloudMade.Create(
  AGUID: TGUID;
  ALanguageManager: ILanguageManager;
  AVehicle: TRouteVehicle;
  ARouteCalcType: TRouteCalcType
);
begin
  inherited Create(AGUID, ALanguageManager);
  FBaseUrl := 'http://routes.cloudmade.com/BC9A493B41014CAABB98F0471D759707/api/0.3/';
  FVehicle := AVehicle;
  FRouteCalcType := ARouteCalcType;
end;

function TPathDetalizeProviderCloudMade.GetPath(ASource: TArrayOfDoublePoint;
  var AComment: string): TArrayOfDoublePoint;
var
  ms:TMemoryStream;
  url:string;
  i:integer;
  s:integer;
  conerr:boolean;
  add_line_arr_b:TArrayOfDoublePoint;
  
  pathstr,timeT1:string;
  posit,posit2,dd,seconds,meters:integer;
  dateT1:TDateTime;

begin
  AComment := '';
  meters:=0;
  seconds:=0;
  SetLength(add_line_arr_b,0);
  ms:=TMemoryStream.Create;
  try
    conerr:=false;
    for i:= 0 to length(ASource)-2 do begin
      if conerr then Continue;
      url := FBaseUrl;
      url:=url+R2StrPoint(ASource[i].y)+','+R2StrPoint(ASource[i].x)+
          ','+R2StrPoint(ASource[i+1].y)+','+R2StrPoint(ASource[i+1].x);
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
      if GetStreamFromURL(ms, url, 'application/json;charset=UTF-8')>0 then begin
        ms.Position:=0;
        SetLength(pathstr, ms.Size);
        ms.ReadBuffer(pathstr[1], ms.Size);
        s:=length(add_line_arr_b);
        try
          posit:=PosEx('[',pathstr,1);
          posit:=PosEx('[',pathstr,posit+1);
          if posit>0 then  begin
            While (posit>0) do begin
              SetLength(add_line_arr_b,(s+1));
              s:=length(add_line_arr_b);

              posit2:=PosEx(',',pathstr,posit);
              add_line_arr_b[s-1].Y:=str2r(copy(pathstr,posit+1,posit2-(posit+1)));
              posit:=PosEx(']',pathstr,posit2);
              add_line_arr_b[s-1].X:=str2r(copy(pathstr,posit2+1,posit-(posit2+1)));

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
    end;
  finally
    ms.Free;
  end;
  if not conerr then begin
    Result := add_line_arr_b;
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
  ALanguageManager: ILanguageManager);
begin
  inherited Create(StringToGUID('{EA19A8A3-B79F-4CB2-81E7-18365FAF6163}'), ALanguageManager, car, fastest);
end;

function TPathDetalizeProviderCloudMadeFastestByCar.GetCaptionTranslated: string;
begin
  Result := _('On car (Fastest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByCar.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on car (Fastest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByCar.GetMenuItemNameTranslated: string;
begin
  Result := _('On car (Fastest)');
end;

{ TPathDetalizeProviderCloudMadeFastestByFoot }

constructor TPathDetalizeProviderCloudMadeFastestByFoot.Create(
  ALanguageManager: ILanguageManager);
begin
  inherited Create(StringToGUID('{038DA1B0-F36B-463D-8914-32CEDC8791DB}'), ALanguageManager, foot, fastest);
end;

function TPathDetalizeProviderCloudMadeFastestByFoot.GetCaptionTranslated: string;
begin
  Result := _('On foot (Fastest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByFoot.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on foot (Fastest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByFoot.GetMenuItemNameTranslated: string;
begin
  Result := _('On foot (Fastest)');
end;

{ TPathDetalizeProviderCloudMadeFastestByBicycle }

constructor TPathDetalizeProviderCloudMadeFastestByBicycle.Create(
  ALanguageManager: ILanguageManager);
begin
  inherited Create(StringToGUID('{77999C1C-176E-4F25-BEF3-69F5A7D52D40}'), ALanguageManager, bicycle, fastest);
end;

function TPathDetalizeProviderCloudMadeFastestByBicycle.GetCaptionTranslated: string;
begin
  Result := _('On bicycle (Fastest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByBicycle.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on bicycle (Fastest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeFastestByBicycle.GetMenuItemNameTranslated: string;
begin
  Result := _('On bicycle (Fastest)');
end;

{ TPathDetalizeProviderCloudMadeShortestByCar }

constructor TPathDetalizeProviderCloudMadeShortestByCar.Create(
  ALanguageManager: ILanguageManager);
begin
  inherited Create(StringToGUID('{8E648161-59ED-4F3B-8279-7B45A1A9E269}'), ALanguageManager, car, shortest);
end;

function TPathDetalizeProviderCloudMadeShortestByCar.GetCaptionTranslated: string;
begin
  Result := _('On car (Shortest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByCar.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on car (Shortest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByCar.GetMenuItemNameTranslated: string;
begin
  Result := _('On car (Shortest)');
end;

{ TPathDetalizeProviderCloudMadeShortestByFoot }

constructor TPathDetalizeProviderCloudMadeShortestByFoot.Create(
  ALanguageManager: ILanguageManager);
begin
  inherited Create(StringToGUID('{05D95794-5A4C-4CD4-BE7D-6D17532A44B5}'), ALanguageManager, foot, shortest);
end;

function TPathDetalizeProviderCloudMadeShortestByFoot.GetCaptionTranslated: string;
begin
  Result := _('On foot (Shortest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByFoot.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on foot (Shortest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByFoot.GetMenuItemNameTranslated: string;
begin
  Result := _('On foot (Shortest)');
end;

{ TPathDetalizeProviderCloudMadeShortestByBicycle }

constructor TPathDetalizeProviderCloudMadeShortestByBicycle.Create(
  ALanguageManager: ILanguageManager);
begin
  inherited Create(StringToGUID('{63EAF551-FA9D-4119-910A-D9044CD5663D}'), ALanguageManager, bicycle, shortest);
end;

function TPathDetalizeProviderCloudMadeShortestByBicycle.GetCaptionTranslated: string;
begin
  Result := _('On bicycle (Shortest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByBicycle.GetDescriptionTranslated: string;
begin
  Result := _('Detalize route on bicycle (Shortest) by cloudmade.com');
end;

function TPathDetalizeProviderCloudMadeShortestByBicycle.GetMenuItemNameTranslated: string;
begin
  Result := _('On bicycle (Shortest)');
end;

end.
